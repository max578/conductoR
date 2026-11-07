# trace.R -- Every number in a report traced to the return it came from.

.REF_RE <- "\\{\\{\\s*([^{}]+?)\\s*\\}\\}"
.NUM_RE <- paste0(
  "(?<![A-Za-z0-9_.])-?(?:\\d{1,3}(?:,\\d{3})+|\\d+)(?:\\.\\d+)?",
  "(?:[eE][-+]?\\d+)?(?![A-Za-z0-9_])")

#' Trace every number in a report to the release it came from
#'
#' Checks a report before it is released. A composer that never saw a
#' number writes each one as a reference to a value of the release,
#' `{{<node>:<field>}}`, and `trace_report()` enters the value. Any number
#' written out in the text is matched against the release's values at the
#' digits shown. A reference to a value the release does not hold, or a
#' number that matches no value and was not declared in `allow`, blocks the
#' release: the result then carries no text, only the trace saying why.
#'
#' @param report The report: its text, as one string or a vector of lines,
#'   or the path of a text file.
#' @param ret The release the report draws on: a return from [run_return()]
#'   or the path of one written by [write_return()].
#' @param allow Numbers the report may show that are not results of the
#'   run, such as the confidence level of an interval, a year or a section
#'   number. Each is listed in the trace as `declared`. A numeric or
#'   character vector, or `NULL`.
#' @param run The run the release was built from, or `NULL`. When given,
#'   the version of the tool behind each traced value is added to the trace.
#'
#' @returns An object of class `conductoR_trace`: a list with `text` (the
#'   report with every reference replaced by its value, or `NA` when the
#'   release is blocked), `trace` (one row per reference and per number
#'   written out, see below), `released` (`TRUE` when every number traced),
#'   `plan_hash` and `input_hash` (those of the release).
#'
#' @section References:
#' A reference names a row of the release's `values`: `{{fit:slope}}` for
#' the value of a metric, `{{fit:params.estimate:3}}` for the third row of a
#' `params` field. The index may be left out only when the field returned a
#' single value. The value is entered as the release holds it, at the
#' significant figures the plan declared; a number the report needs in
#' another form, such as a difference or a percentage, is added to the plan
#' as a node, so that it too is a value of the release.
#'
#' @section The trace:
#' Columns `item`, `line` (of the report), `kind` (`reference` or
#' `literal`), `shown` (the text in the report), `status`, `reason`,
#' `node`, `field`, `index`, `value` (the text entered or matched),
#' `n_matches` (values a literal number matched), `emitter` and
#' `emitter_version`.
#'
#' `status` is `filled` (a reference resolved), `matched` (a number written
#' out equals at least one value of the release at the digits shown; the
#' first is named), `declared` (the number is in `allow`), `unresolved` (a
#' reference the release does not hold; `reason` is the reason the value
#' was refused, `not_in_return` or `index_needed`) or `untraced` (a number
#' that matches no value; `reason` is `no_match`). Either of the last two
#' blocks the release.
#'
#' @section What counts as a number:
#' A run of digits, with an optional sign, decimal part, exponent and
#' thousands separators, that is not part of a word: `H1`, `v0.3` and
#' `round_2` are not numbers, while the `95` of `95%` and both years of
#' `2008-2017` are. A number equals a value at the digits shown when the
#' value, rounded to the number of decimals written, is that number.
#'
#' @seealso [run_return()], [write_return()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "demo", "demo:lens",
#'                    metrics = list(slope = structure(0.3127, n_units = 24L)))
#' }
#' run <- conduct(oap_plan(id = "traced", nodes = list(
#'   node("lens", lens, emits = "treatment_effects"))), verbose = FALSE)
#' ret <- run_return(run, dose = 1, requests = list(lens = c(slope = 1)))
#'
#' # a reference is filled in; 95 is declared, 0.31 matches the slope
#' tr <- trace_report(c("The slope was {{lens:slope}} t/ha per 10 units",
#'                      "(0.31 rounded, 95% interval in the appendix)."),
#'                    ret, allow = c(10, 95))
#' tr$text
#' tr$trace[, c("shown", "status", "node", "field")]
#'
#' # a number that matches nothing blocks the release
#' trace_report("The slope was 0.42.", ret)$released
#'
#' @export
trace_report <- function(report, ret, allow = NULL, run = NULL) {
  txt <- .report_text(report)
  rel <- .release_tables(ret)
  allow_num <- if (is.null(allow)) numeric(0)
               else suppressWarnings(as.numeric(gsub(",", "", allow)))
  if (anyNA(allow_num)) {
    stop("allow must hold numbers only", call. = FALSE)
  }
  vals <- rel$values
  vals$num <- suppressWarnings(as.numeric(vals$value))
  emitter <- function(node) {
    hit <- rel$record$emitter[rel$record$kind == "node" &
                                rel$record$id == node]
    if (length(hit)) hit[1] else NA_character_
  }
  version <- function(node) {
    m <- if (is.null(run)) NULL else run$bus[[node]]
    if (is.null(m)) return(NA_character_)
    v <- tryCatch(m@emitter_version, error = function(e) NULL)
    if (is.character(v) && length(v) == 1L) v else NA_character_
  }
  line_of <- function(pos) {
    breaks <- gregexpr("\n", txt, fixed = TRUE)[[1]]
    1L + sum(breaks > 0L & breaks < pos)
  }
  rows <- list()
  add <- function(...) rows[[length(rows) + 1L]] <<- .trace_row(...)

  # References: each resolved from the release ------------------------------
  refs <- gregexpr(.REF_RE, txt, perl = TRUE)[[1]]
  fills <- character(0)
  if (refs[1] > 0L) {
    shown <- regmatches(txt, list(refs))[[1]]
    for (i in seq_along(shown)) {
      key <- .parse_ref(sub(.REF_RE, "\\1", shown[i], perl = TRUE))
      hit <- vals[vals$node == key$node & vals$field == key$field, ,
                  drop = FALSE]
      reason <- NA_character_
      if (!nrow(hit)) {
        ref <- rel$refused$reason[rel$refused$node == key$node &
                                    rel$refused$field == key$field]
        reason <- if (length(ref)) ref[1] else "not_in_return"
      } else if (is.na(key$index)) {
        if (nrow(hit) > 1L) reason <- "index_needed"
        else key$index <- as.integer(hit$index)
      } else {
        hit <- hit[as.integer(hit$index) == key$index, , drop = FALSE]
        if (!nrow(hit)) reason <- "not_in_return"
      }
      ok <- is.na(reason)
      fills[i] <- if (ok) hit$value[1] else shown[i]
      add(line = line_of(refs[i]), kind = "reference", shown = shown[i],
          status = if (ok) "filled" else "unresolved", reason = reason,
          node = key$node, field = key$field, index = key$index,
          value = if (ok) hit$value[1] else NA_character_,
          n_matches = if (ok) 1L else 0L,
          emitter = emitter(key$node),
          emitter_version = if (ok) version(key$node) else NA_character_)
    }
  }

  # Numbers written out: matched at the digits shown, or declared ------------
  bare <- txt
  if (refs[1] > 0L) {
    regmatches(bare, list(refs)) <- list(vapply(
      attr(refs, "match.length"), function(n) strrep(" ", n), character(1)))
  }
  nums <- gregexpr(.NUM_RE, bare, perl = TRUE)[[1]]
  if (nums[1] > 0L) {
    shown <- regmatches(bare, list(nums))[[1]]
    for (i in seq_along(shown)) {
      x <- as.numeric(gsub(",", "", shown[i]))
      d <- .decimals_shown(shown[i])
      tol <- 1e-9 * max(1, abs(x))
      if (any(abs(allow_num - x) <= tol)) {
        add(line = line_of(nums[i]), kind = "literal", shown = shown[i],
            status = "declared")
        next
      }
      at <- !is.na(vals$num) & abs(.round_shown(vals$num, d) - x) <= tol
      j <- which(at)
      ok <- length(j) > 0L
      add(line = line_of(nums[i]), kind = "literal", shown = shown[i],
          status = if (ok) "matched" else "untraced",
          reason = if (ok) NA_character_ else "no_match",
          node = if (ok) vals$node[j[1]] else NA_character_,
          field = if (ok) vals$field[j[1]] else NA_character_,
          index = if (ok) as.integer(vals$index[j[1]]) else NA_integer_,
          value = if (ok) vals$value[j[1]] else NA_character_,
          n_matches = length(j),
          emitter = if (ok) emitter(vals$node[j[1]]) else NA_character_,
          emitter_version = if (ok) version(vals$node[j[1]])
                            else NA_character_)
    }
  }

  trace <- if (length(rows)) do.call(rbind, rows) else .trace_row()[0, ]
  if (nrow(trace)) {
    trace <- trace[order(trace$line, match(trace$shown, trace$shown)), ,
                   drop = FALSE]
  }
  trace$item <- seq_len(nrow(trace))
  trace <- trace[, c("item", setdiff(names(trace), "item"))]
  rownames(trace) <- NULL
  released <- !any(trace$status %in% c("unresolved", "untraced"))
  text <- NA_character_
  if (released) {
    text <- txt
    if (refs[1] > 0L) regmatches(text, list(refs)) <- list(fills)
  }
  structure(list(text = text, trace = trace, released = released,
                 plan_hash = rel$plan_hash, input_hash = rel$input_hash),
            class = "conductoR_trace")
}

.trace_row <- function(line = NA_integer_, kind = NA_character_,
                       shown = NA_character_, status = NA_character_,
                       reason = NA_character_, node = NA_character_,
                       field = NA_character_, index = NA_integer_,
                       value = NA_character_, n_matches = NA_integer_,
                       emitter = NA_character_,
                       emitter_version = NA_character_) {
  data.frame(item = NA_integer_, line = as.integer(line), kind = kind,
             shown = shown, status = status, reason = reason, node = node,
             field = field, index = as.integer(index), value = value,
             n_matches = as.integer(n_matches), emitter = emitter,
             emitter_version = emitter_version, stringsAsFactors = FALSE)
}

# The report as one string, from its text or its file.
.report_text <- function(report) {
  if (!is.character(report) || !length(report) || anyNA(report)) {
    stop("report must be text or the path of a text file", call. = FALSE)
  }
  if (length(report) == 1L && !grepl("\n", report, fixed = TRUE) &&
      file.exists(report)) {
    report <- readLines(report, warn = FALSE)
  }
  paste(report, collapse = "\n")
}

# `node:field` or `node:field:index`; the field may hold a row filter.
.parse_ref <- function(s) {
  parts <- strsplit(s, ":", fixed = TRUE)[[1]]
  parts <- trimws(parts)
  idx <- NA_integer_
  if (length(parts) >= 3L && grepl("^[0-9]+$", parts[length(parts)])) {
    idx <- as.integer(parts[length(parts)])
    parts <- parts[-length(parts)]
  }
  if (length(parts) < 2L || !nzchar(parts[1])) {
    return(list(node = s, field = NA_character_, index = idx))
  }
  list(node = parts[1], field = paste(parts[-1], collapse = ":"),
       index = idx)
}

# The decimals written in a number; for one in exponent form, the decimals
# of its mantissa less the exponent.
.decimals_shown <- function(s) {
  s <- gsub(",", "", s)
  mant <- sub("[eE].*$", "", s)
  d <- if (grepl(".", mant, fixed = TRUE)) nchar(sub("^.*\\.", "", mant))
       else 0L
  if (grepl("[eE]", s)) d <- d - as.integer(sub("^.*[eE]", "", s))
  as.integer(d)
}

.round_shown <- function(x, d) {
  if (d >= 0L) round(x, d) else round(x / 10^(-d)) * 10^(-d)
}

# The values, refusals, record and hashes of a release, from the object or
# its written file.
.release_tables <- function(ret) {
  if (inherits(ret, "conductoR_return")) {
    return(list(values = ret$values, refused = ret$refused,
                record = ret$record, plan_hash = ret$plan_hash,
                input_hash = ret$input_hash))
  }
  if (!(is.character(ret) && length(ret) == 1L && file.exists(ret))) {
    stop("ret must be a return from run_return() or the path of one ",
         "written by write_return()", call. = FALSE)
  }
  txt <- readLines(ret, warn = FALSE)
  table_of <- function(name, cols) {
    sec <- .read_return_section(txt, name)
    if (length(sec) < 2L) {
      return(as.data.frame(stats::setNames(
        rep(list(character(0)), length(cols)), cols),
        stringsAsFactors = FALSE))
    }
    utils::read.csv(text = sec, colClasses = "character",
                    na.strings = character(0))
  }
  head <- txt[seq_len(grep("^\\[[a-z]+\\]$", txt)[1] - 1L)]
  field <- function(name) {
    hit <- grep(paste0("^", name, ": "), head, value = TRUE)
    if (length(hit)) sub(paste0("^", name, ": "), "", hit[1]) else NA_character_
  }
  list(values = table_of("values", c("node", "field", "dose", "index",
                                      "value")),
       refused = table_of("refused", c("node", "field", "dose", "reason",
                                       "n_rows")),
       record = table_of("record", .RECORD_COLS),
       plan_hash = field("plan_hash"), input_hash = field("input_hash"))
}

#' @export
format.conductoR_trace <- function(x, ...) {
  tr <- x$trace
  blocked <- tr[tr$status %in% c("unresolved", "untraced"), , drop = FALSE]
  c("conductoR trace",
    sprintf("plan_hash: %s", x$plan_hash),
    sprintf("input_hash: %s", x$input_hash),
    sprintf("released: %s", x$released),
    sprintf("numbers: %d (%s)", nrow(tr),
            paste(sprintf("%s %d", names(table(tr$status)),
                          as.integer(table(tr$status))), collapse = ", ")),
    if (nrow(blocked)) c("", "[blocked]", sprintf(
      "line %d: %s %s", blocked$line, blocked$shown, blocked$reason)),
    "", "[trace]", .csv_lines(tr))
}

#' @export
print.conductoR_trace <- function(x, ...) {
  writeLines(format(x))
  invisible(x)
}

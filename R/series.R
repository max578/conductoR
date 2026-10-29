# series.R -- The rounds of one analysis, read back from the returns written
# into a series folder.

# The non-empty lines of one section (`[message]`, `[values]`, ...) of a
# written return, given its text or its path.
.read_return_section <- function(txt, name) {
  if (length(txt) == 1L && file.exists(txt)) {
    txt <- readLines(txt, warn = FALSE)
  }
  sec <- grep("^\\[[a-z]+\\]$", txt)
  at <- match(paste0("[", name, "]"), txt)
  if (is.na(at)) return(character(0))
  end <- c(sec[sec > at], length(txt) + 1L)[1] - 1L
  out <- txt[seq_len(end - at) + at]
  out[nzchar(out)]
}

# The header fields and message lines of one written return.
.read_return <- function(path) {
  txt <- readLines(path, warn = FALSE)
  sec <- grep("^\\[[a-z]+\\]$", txt)
  head <- txt[seq_len(sec[1] - 1L)]
  field <- function(name) {
    hit <- grep(paste0("^", name, ": "), head, value = TRUE)
    if (length(hit)) sub(paste0("^", name, ": "), "", hit[1]) else NA_character_
  }
  vals <- .read_return_section(txt, "values")
  list(plan_hash = field("plan_hash"), series = field("series"),
       dose = as.integer(field("dose")),
       released = identical(field("released"), "TRUE"),
       time = field("time"), lines = .read_return_section(txt, "message"),
       n_values = max(length(vals) - 1L, 0L))
}

#' Report the rounds of a series
#'
#' Lists every round of one analysis from the returns written into its
#' series folder by [write_return()]: `<series>/round_<k>/return.txt`. A
#' round folder that holds no return is listed with its fields missing, so a
#' run that returned nothing still shows.
#'
#' @param dir The series folder.
#' @param budget The number of returns above dose 0 the data holder allows
#'   for the series, or `NULL` for no budget. The budget is the holder's
#'   number; the report only counts against it.
#'
#' @returns A data frame of class `conductoR_series` with one row per round:
#'   `round`, `plan_hash`, `dose`, `n_lines` and `lines` (the message, lines
#'   joined by ` | `), `n_values` (values returned), `released` and `time`.
#'   The attributes `n_above_dose_0`, `budget` and `within_budget` carry the
#'   count against the budget, which printing shows below the table.
#'
#' @seealso [write_return()], [run_return()], [same_plan()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "demo", "demo:lens",
#'                    metrics = list(slope = structure(0.31, n_units = 24L)))
#' }
#' run <- conduct(oap_plan(id = "s", nodes = list(
#'   node("lens", lens, emits = "treatment_effects"))), verbose = FALSE)
#' dir <- file.path(tempdir(), "demo_rounds")
#' write_return(run_return(run, series = "demo", round = 1), dir)
#' write_return(run_return(run, dose = 1, series = "demo", round = 2,
#'                         requests = list(lens = c(slope = 1))), dir)
#' series_report(dir, budget = 3)
#'
#' @export
series_report <- function(dir, budget = NULL) {
  stopifnot(is.character(dir), length(dir) == 1L, dir.exists(dir))
  if (!is.null(budget)) {
    stopifnot(is.numeric(budget), length(budget) == 1L, !is.na(budget),
              budget >= 0)
  }
  rounds <- list.files(dir, pattern = "^round_[0-9]+$")
  rounds <- rounds[dir.exists(file.path(dir, rounds))]
  k <- as.integer(sub("^round_", "", rounds))
  rounds <- rounds[order(k)]
  k <- sort(k)

  # One row per round folder -------------------------------------------------
  rows <- lapply(seq_along(rounds), function(i) {
    f <- file.path(dir, rounds[i], "return.txt")
    if (!file.exists(f)) {
      return(data.frame(round = k[i], plan_hash = NA_character_,
                        dose = NA_integer_, n_lines = 0L,
                        lines = NA_character_, n_values = 0L,
                        released = FALSE, time = NA_character_,
                        stringsAsFactors = FALSE))
    }
    r <- .read_return(f)
    data.frame(round = k[i], plan_hash = r$plan_hash, dose = r$dose,
               n_lines = length(r$lines),
               lines = paste(r$lines, collapse = " | "),
               n_values = r$n_values, released = r$released, time = r$time,
               stringsAsFactors = FALSE)
  })
  out <- if (length(rows)) do.call(rbind, rows)
         else data.frame(round = integer(0), plan_hash = character(0),
                         dose = integer(0), n_lines = integer(0),
                         lines = character(0), n_values = integer(0),
                         released = logical(0), time = character(0),
                         stringsAsFactors = FALSE)
  rownames(out) <- NULL

  # The count against the budget ---------------------------------------------
  n_above <- sum(out$dose > 0L, na.rm = TRUE)
  attr(out, "n_above_dose_0") <- n_above
  attr(out, "budget") <- budget
  attr(out, "within_budget") <- if (is.null(budget)) NA else n_above <= budget
  class(out) <- c("conductoR_series", "data.frame")
  out
}

#' @export
print.conductoR_series <- function(x, ...) {
  tab <- x
  class(tab) <- "data.frame"
  print(tab, ...)
  n <- attr(x, "n_above_dose_0")
  b <- attr(x, "budget")
  cat(if (is.null(b)) sprintf("Returns above dose 0: %d\n", n)
      else sprintf("Returns above dose 0: %d of a budget of %s%s\n", n,
                   format(b), if (n > b) " (over budget)" else ""))
  invisible(x)
}

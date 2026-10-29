# record.R -- What a run returns to the composer: the dose-0 record, the
# declared signals, the requested values and the check that guards them.

# The typed error words a return may carry, one per kind of failure.
.ERROR_TYPES <- c("typed_edge_violation", "missing_column", "type_mismatch",
                  "missing_block", "too_few_units", "model_not_converged",
                  "unidentifiable_parameter", "assumption_check_failed",
                  "tool_error", "licence_unavailable",
                  "credential_unavailable", "no_interior_minimum",
                  "no_interior_optimum", "no_response",
                  "postcondition_failed")

.TRIGGER_COLS <- c("trigger_id", "node", "class", "signal_type", "statistic",
                   "direction", "threshold", "action")
.TRIGGER_DIRECTIONS <- c(">", ">=", "<", "<=", "==")
.TRIGGER_ACTIONS <- c("branch", "return", "release")
.TRIGGER_CLASSES <- c("anomaly", "candidate")

.RECORD_COLS <- c("kind", "id", "emitter", "emits", "status", "reason",
                  "abstained", "checks_passed", "checks_failed")

# A word outside the alphabet is reported as a tool error, so a return never
# carries free text.
.as_error_type <- function(x) {
  ifelse(x %in% .ERROR_TYPES, x, "tool_error")
}

# The alphabet word for an error; the order is set out in try_conduct().
.error_type_of <- function(e) {
  hit <- intersect(class(e), .ERROR_TYPES)
  if (length(hit)) return(hit[1])
  msg <- conditionMessage(e)
  pre <- regmatches(msg, regexpr("^[a-z_]+(?=:)", msg, perl = TRUE))
  if (length(pre) && pre %in% .ERROR_TYPES) return(pre)
  edge_pat <- paste0("TYPED-EDGE VIOLATION|expected an orchestra_manifest|",
                     "did not return an orchestra_manifest|",
                     "emitted '.*', promised")
  if (grepl(edge_pat, msg)) return("typed_edge_violation")
  "tool_error"
}

#' Perform a plan and keep a record when it fails
#'
#' Calls [conduct()] with the same arguments. When the run completes, the
#' result is the run itself. When a node stops with an error, the result is
#' a failed run: the nodes performed before the error, with their manifests,
#' the node that failed and the typed error word for the failure. The
#' failed run is accepted by [run_record()] and [run_return()], so a run that
#' stops still returns a typed message to the composer.
#'
#' The error word is the condition's class when that class is one of the
#' words listed in [run_record()]; else the word before a colon that starts
#' the error message (`stop("missing_block: site S3 has no plots")`); else
#' `typed_edge_violation` for a contract failure at an edge; else
#' `tool_error`. The error message itself is kept in `failed$message` for
#' the data holder and never enters a record or a return.
#'
#' @param plan An [oap_plan].
#' @param data The data object every tool receives.
#' @param ... Further arguments to [conduct()].
#'
#' @returns The run returned by [conduct()], or a failed run: a list with
#'   `plan_id`, `plan_hash`, `input_hash`, `label`, `bus` and `provenance`
#'   (the nodes performed before the error), `not_run` (the ids of the
#'   nodes not reached) and `failed`, a list with `node` (the id of the node
#'   that failed, or `"plan"` when the error came before any node ran),
#'   `error_type`, `message` and `postconditions` (the `<input>.<id>` of
#'   each input postcondition that failed, when that stopped the run).
#'
#' @seealso [run_record()], [run_return()].
#'
#' @author Max Moldovan
#'
#' @examples
#' broken <- function(inputs, data, node) {
#'   stop("missing_column: the table has no column 'yield'")
#' }
#' p <- oap_plan(id = "fails", nodes = list(
#'   node("fit", broken, emits = "parameters")))
#' run <- try_conduct(p, verbose = FALSE)
#' run$failed$error_type
#' format_record(run_record(run))
#'
#' @export
try_conduct <- function(plan, data = list(), ...) {
  stopifnot(S7::S7_inherits(plan, oap_plan))
  state <- new.env(parent = emptyenv())
  state$node <- "plan"
  state$prov <- list()
  state$bus <- list()
  run <- tryCatch(
    withCallingHandlers(
      conduct(plan, data = data, ...),
      error = function(e) .note_failure(state, sys.frames())),
    error = function(e) e)
  if (!inherits(run, "error")) return(run)

  # The failed run ----------------------------------------------------------
  args <- list(...)
  done <- vapply(state$prov, function(p) p$node, character(1))
  order <- tryCatch(plan_order(plan), error = function(e) .node_ids(plan))
  list(plan_id = plan@id,
       plan_hash = tryCatch(.plan_hash(plan),
                            error = function(e) NA_character_),
       input_hash = .object_hash(data),
       label = args$label %||% NA_character_,
       bus = state$bus, provenance = state$prov,
       not_run = setdiff(order, c(done, state$node)),
       failed = list(node = state$node, error_type = .error_type_of(run),
                     message = conditionMessage(run),
                     postconditions = if (inherits(run, "postcondition_failed"))
                       run$failed))
}

# Called where the error is signalled: the innermost frame holding an
# `oap_node` names the node, and the walker's frame holds what ran before.
.note_failure <- function(state, frames) {
  node_id <- NA_character_
  for (fr in rev(frames)) {
    if (!exists("node", envir = fr, inherits = FALSE)) next
    nd <- tryCatch(get("node", envir = fr, inherits = FALSE),
                   error = function(e) NULL)
    if (S7::S7_inherits(nd, oap_node)) {
      node_id <- nd@id
      break
    }
  }
  for (fr in rev(frames)) {
    if (exists("by_id", envir = fr, inherits = FALSE) &&
        exists("prov", envir = fr, inherits = FALSE)) {
      state$prov <- get("prov", envir = fr, inherits = FALSE)
      state$bus <- get("bus", envir = fr, inherits = FALSE)
      if (is.na(node_id) && exists("id", envir = fr, inherits = FALSE)) {
        node_id <- get("id", envir = fr, inherits = FALSE)
      }
      break
    }
  }
  state$node <- if (is.na(node_id)) "plan" else node_id
  invisible()
}

# The nodes a run knows about, in execution order.
.run_node_ids <- function(run) {
  ids <- vapply(run$provenance, function(p) p$node, character(1))
  c(ids, run$failed$node[!identical(run$failed$node, "plan")],
    run$not_run %||% character(0))
}

#' Evaluate the declared signals of a run
#'
#' A signal is declared before the run, in a table with one row per
#' trigger. Each trigger reads one statistic of one node, the metric
#' `summary$metrics[[statistic]]` of the manifest the node emitted, and fires
#' when the value compares with the threshold as its direction says.
#'
#' @param run A run returned by [conduct()] or [try_conduct()].
#' @param triggers A data frame with columns `trigger_id`, `node`, `class`
#'   (`"anomaly"` or `"candidate"`), `signal_type` (a lower-case word,
#'   written with or without the prefix `signal:`),
#'   `statistic`, `direction` (`>`, `>=`, `<`, `<=` or `==`), `threshold`,
#'   `action` (`"return"`, `"branch"` or `"release"`) and, optionally,
#'   `rehearsal_plant` (what was planted in rehearsal to show the trigger
#'   fires). Columns may be character, as read from a CSV file.
#'
#' @returns The trigger table with `signal_type` carrying its `signal:`
#'   prefix, `threshold` as a number, and three added columns: `value` (the
#'   statistic, or `NA`), `fired` (`TRUE`, `FALSE`, or `NA` when the trigger
#'   could not be evaluated) and `status` (`"evaluated"`, `"node_not_run"`
#'   or `"statistic_missing"`).
#'
#' @section What stays with the data holder:
#' `value` is the statistic itself, a data value. It stays in the object
#' this function returns. [run_record()] and [run_return()] keep only
#' whether each trigger fired.
#'
#' @seealso [run_record()], [run_return()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "demo", "demo:spread",
#'                    metrics = list(spread = 1.6))
#' }
#' run <- conduct(oap_plan(id = "sig", nodes = list(
#'   node("lens", lens, emits = "treatment_effects"))), verbose = FALSE)
#' trig <- data.frame(trigger_id = "T_spread", node = "lens",
#'                    class = "anomaly", signal_type = "lens_spread",
#'                    statistic = "spread", direction = ">", threshold = 1.3,
#'                    action = "return")
#' evaluate_triggers(run, trig)[, c("trigger_id", "fired", "status")]
#'
#' @export
evaluate_triggers <- function(run, triggers) {
  stopifnot(is.list(run))
  tab <- .check_triggers(triggers, .run_node_ids(run))
  n <- nrow(tab)
  value <- rep(NA_real_, n)
  status <- character(n)

  # Read each statistic from its node's manifest ----------------------------
  for (i in seq_len(n)) {
    m <- run$bus[[tab$node[i]]]
    v <- if (is.null(m)) NULL else m@summary$metrics[[tab$statistic[i]]]
    ok <- (is.numeric(v) || is.logical(v)) && length(v) == 1L && !is.na(v)
    status[i] <- if (is.null(m)) "node_not_run"
                 else if (!ok) "statistic_missing" else "evaluated"
    if (ok) value[i] <- as.numeric(v)
  }

  # Compare with the declared thresholds ------------------------------------
  fired <- rep(NA, n)
  for (i in which(status == "evaluated")) {
    fired[i] <- switch(tab$direction[i],
      ">"  = value[i] >  tab$threshold[i],
      ">=" = value[i] >= tab$threshold[i],
      "<"  = value[i] <  tab$threshold[i],
      "<=" = value[i] <= tab$threshold[i],
      "==" = value[i] == tab$threshold[i])
  }
  tab$value <- value
  tab$fired <- fired
  tab$status <- status
  class(tab) <- c("conductoR_triggers", "data.frame")
  tab
}

# Refuses a trigger table with a missing column or a value outside the
# allowed set, naming every problem at once.
.check_triggers <- function(triggers, node_ids) {
  if (!is.data.frame(triggers)) {
    stop("triggers must be a data frame", call. = FALSE)
  }
  miss <- setdiff(.TRIGGER_COLS, names(triggers))
  if (length(miss)) {
    stop(sprintf("triggers lacks these columns: %s",
                 paste(miss, collapse = ", ")), call. = FALSE)
  }
  tab <- as.data.frame(triggers, stringsAsFactors = FALSE)
  class(tab) <- "data.frame"
  for (col in setdiff(.TRIGGER_COLS, "threshold")) {
    tab[[col]] <- as.character(tab[[col]])
  }
  if (!"rehearsal_plant" %in% names(tab)) tab$rehearsal_plant <- NA_character_
  sig <- tab$signal_type
  tab$signal_type <- ifelse(startsWith(sig, "signal:"), sig,
                            paste0("signal:", sig))
  tab$threshold <- suppressWarnings(as.numeric(as.character(tab$threshold)))

  probs <- character(0)
  bad <- function(ok, what) {
    if (all(ok)) return(invisible())
    rows <- paste(which(!ok), collapse = ", ")
    probs <<- c(probs, sprintf("%s (row %s)", what, rows))
  }
  bad(grepl("^[A-Za-z][A-Za-z0-9_.]*$", tab$trigger_id),
      "trigger_id must start with a letter and hold letters, digits, _ or .")
  bad(!duplicated(tab$trigger_id), "trigger_id must be unique")
  bad(tab$node %in% node_ids, "node is not a node of the run")
  bad(tab$class %in% .TRIGGER_CLASSES, "class must be anomaly or candidate")
  bad(grepl("^signal:[a-z][a-z0-9_]*$", tab$signal_type),
      "signal_type must be a lower-case word")
  bad(!is.na(tab$statistic) & nzchar(tab$statistic),
      "statistic must be named")
  bad(tab$direction %in% .TRIGGER_DIRECTIONS,
      "direction must be one of >, >=, <, <=, ==")
  bad(!is.na(tab$threshold), "threshold must be a number")
  bad(tab$action %in% .TRIGGER_ACTIONS,
      "action must be return, branch or release")
  if (length(probs)) {
    stop(paste(c("triggers are not valid:", paste0("  - ", probs)),
               collapse = "\n"), call. = FALSE)
  }
  tab
}

# The issue words of one manifest and of its `foreach` instances.
.manifest_issues <- function(m, nested = TRUE) {
  if (!orchestraManifest::is_orchestra_manifest(m)) return(character(0))
  s <- m@summary
  v <- as.character(unlist(s$metrics$issue))
  v <- v[!is.na(v) & nzchar(v)]
  out <- .as_error_type(v)
  r <- s$abstain_reason
  if (!length(out) && isTRUE(s$abstained) && length(r) == 1L &&
      !is.na(r) && r %in% .ERROR_TYPES) {
    out <- r
  }
  if (nested && is.list(m@outputs)) {
    for (mi in m@outputs) out <- c(out, .manifest_issues(mi, nested = FALSE))
  }
  unique(out)
}

# A declared check that is absent or not a single TRUE did not pass.
.node_checks <- function(m, declared) {
  metrics <- if (is.null(m)) list() else m@summary$metrics
  own <- metrics$checks
  vals <- list()
  if (length(own) && !is.null(names(own))) vals <- as.list(own)
  for (nm in declared) vals[[nm]] <- metrics[[nm]]
  if (!length(vals)) return(c(NA_character_, NA_character_))
  ok <- vapply(vals, function(v) is.logical(v) && length(v) == 1L &&
                 isTRUE(v), logical(1))
  for (nm in setdiff(declared, names(vals))) ok[nm] <- FALSE
  join <- function(x) if (length(x)) paste(x, collapse = ";") else NA_character_
  c(join(names(ok)[ok]), join(names(ok)[!ok]))
}

.record_row <- function(kind, id, emitter = NA_character_,
                        emits = NA_character_, status, reason = NA_character_,
                        abstained = NA, checks = c(NA_character_,
                                                   NA_character_)) {
  data.frame(kind = kind, id = id, emitter = emitter, emits = emits,
             status = status, reason = reason, abstained = abstained,
             checks_passed = checks[1], checks_failed = checks[2],
             stringsAsFactors = FALSE)
}

#' The dose-0 record of a run
#'
#' The record is what a run says about itself without giving away a data
#' value: one row per node with what it emitted and how it ended, and one
#' row per declared trigger with whether it fired. It is built from
#' [run_provenance()] and the manifests on the run's bus, and it keeps no
#' number, no headline and no free text from either.
#'
#' @param run A run returned by [conduct()] or [try_conduct()].
#' @param triggers A trigger table as described in [evaluate_triggers()],
#'   evaluated or not, or `NULL`.
#' @param checks A named list, keyed by node id, naming for each node the
#'   metrics that are its checks; each such metric holds one `TRUE` (passed)
#'   or `FALSE`. A node may also declare its own checks as a named logical
#'   vector in `summary$metrics$checks`. `NULL` for none.
#'
#' @returns A data frame with columns `kind` (`"node"`, `"trigger"`,
#'   `"input"` for an input postcondition that stopped the run, with `id`
#'   `<input>.<postcondition id>`, or `"plan"` when a run failed before any
#'   node ran for another reason), `id`, `emitter`,
#'   `emits` (for a trigger, its action), `status`, `reason`, `abstained`,
#'   `checks_passed` and `checks_failed` (check names joined by `;`).
#'
#'   A node's `status` is `completed`, `refused` (its result abstained),
#'   `skipped` (by the plan's own rules), `failed` (it stopped with an
#'   error) or `not_run` (the run stopped before it). Its `reason` holds the
#'   issue words it reported, joined by `;`, or `NA`.
#'
#'   A trigger's `status` is `fired`, `silent` or `not_evaluated`; its
#'   `emitter` is the node it reads and its `reason` the signal type.
#'
#' @section The error words:
#' A node's reason is one or more of `typed_edge_violation`,
#' `missing_column`, `type_mismatch`, `missing_block`, `too_few_units`,
#' `model_not_converged`, `unidentifiable_parameter`,
#' `assumption_check_failed` and `tool_error`; the refusal words
#' `licence_unavailable` and `credential_unavailable` (the environment
#' cannot reach a licence or read a credential the tool needs); and the
#' shape words `no_interior_minimum`, `no_interior_optimum` and
#' `no_response` (a fitted curve has no turning point inside the observed
#' range, or no response at all). An input row reports
#' `postcondition_failed`: the table did not meet a postcondition the plan
#' declared for it, so no node ran. A completed node reports an
#' issue through `summary$metrics$issue`; a word outside this list is
#' recorded as `tool_error`. A refused node with no issue reports its
#' abstention reason when that reason is one of the words, and nothing
#' otherwise: a refusal the plan provided for is not a failure. A node that
#' stopped with an error reports the word [try_conduct()] gave it. A node
#' whose declared trigger statistic is missing reports `tool_error`.
#'
#' @seealso [format_record()], [run_return()], [evaluate_triggers()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "demo", "demo:lens",
#'                    metrics = list(issue = "model_not_converged",
#'                                   converged = FALSE))
#' }
#' run <- conduct(oap_plan(id = "rec", nodes = list(
#'   node("lens", lens, emits = "treatment_effects"))), verbose = FALSE)
#' run_record(run, checks = list(lens = "converged"))
#'
#' @export
run_record <- function(run, triggers = NULL, checks = NULL) {
  stopifnot(is.list(run), is.list(run$provenance))
  if (!is.null(checks)) {
    stopifnot(is.list(checks), !is.null(names(checks)))
  }
  trig <- if (is.null(triggers)) NULL
          else if (inherits(triggers, "conductoR_triggers")) triggers
          else evaluate_triggers(run, triggers)
  missing_stat <- if (is.null(trig)) character(0)
                  else trig$node[trig$status == "statistic_missing"]

  # One row per node performed ----------------------------------------------
  rows <- lapply(run$provenance, function(p) {
    m <- run$bus[[p$node]]
    skipped <- identical(p$status, "skipped")
    abstained <- isTRUE(as.logical(p$abstained))
    issues <- if (skipped) character(0) else .manifest_issues(m)
    if (p$node %in% missing_stat) issues <- unique(c(issues, "tool_error"))
    .record_row(
      "node", p$node,
      emitter = if (skipped) NA_character_ else p$emitter,
      emits = if (skipped) NA_character_ else p$emits,
      status = if (skipped) "skipped" else if (abstained) "refused"
               else "completed",
      reason = if (length(issues)) paste(issues, collapse = ";")
               else NA_character_,
      abstained = if (skipped) NA else abstained,
      checks = if (skipped) c(NA_character_, NA_character_)
               else .node_checks(m, checks[[p$node]]))
  })

  # The failed node and the nodes not reached --------------------------------
  if (!is.null(run$failed)) {
    f <- run$failed
    rows <- c(rows, if (length(f$postconditions)) {
      lapply(f$postconditions, function(id) {
        .record_row("input", id, status = "failed",
                    reason = "postcondition_failed")
      })
    } else {
      list(.record_row(if (identical(f$node, "plan")) "plan" else "node",
                       f$node, status = "failed", reason = f$error_type))
    })
    rows <- c(rows, lapply(run$not_run %||% character(0), function(id) {
      .record_row("node", id, status = "not_run")
    }))
  }

  # One row per declared trigger ---------------------------------------------
  if (!is.null(trig) && nrow(trig)) {
    rows <- c(rows, lapply(seq_len(nrow(trig)), function(i) {
      .record_row("trigger", trig$trigger_id[i], emitter = trig$node[i],
                  emits = trig$action[i],
                  status = if (is.na(trig$fired[i])) "not_evaluated"
                           else if (trig$fired[i]) "fired" else "silent",
                  reason = trig$signal_type[i])
    }))
  }
  if (!length(rows)) return(.record_row("node", "x", status = "x")[0, ])
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' The typed message lines of a record
#'
#' Turns a record into the message a composer reads: `stop` when every node
#' completed, was refused or was skipped by the plan's own rules with no
#' issue and no return trigger fired; otherwise one line `<node id>
#' <error word>` per issue, then one line `<trigger id> signal:<type>` per
#' fired trigger whose action is `return`.
#'
#' @param record A record from [run_record()].
#'
#' @returns A character vector of message lines.
#'
#' @seealso [run_record()], [run_return()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "demo", "demo:lens")
#' }
#' run <- conduct(oap_plan(id = "ok", nodes = list(
#'   node("lens", lens, emits = "treatment_effects"))), verbose = FALSE)
#' format_record(run_record(run))
#'
#' @export
format_record <- function(record) {
  stopifnot(is.data.frame(record), all(.RECORD_COLS %in% names(record)))
  hit <- record[record$kind %in% c("node", "plan", "input") &
                  !is.na(record$reason), , drop = FALSE]
  issue_lines <- unlist(lapply(seq_len(nrow(hit)), function(i) {
    paste(hit$id[i], strsplit(hit$reason[i], ";", fixed = TRUE)[[1]])
  }))
  sig <- record[record$kind == "trigger" & record$emits == "return" &
                  record$status == "fired", , drop = FALSE]
  out <- c(issue_lines, paste(sig$id, sig$reason))
  if (length(out)) out else "stop"
}

#' A floor on what a returned statistic may rest on
#'
#' Builds the check [run_return()] applies to every value it would return
#' above dose 0. The check refuses a statistic computed on fewer than `k`
#' units, or one in which a single unit holds more than the `dominance`
#' share, when that information is present. A value that carries no unit
#' count is refused. Two scans follow: a value whose text holds one of the
#' `forbidden` strings is refused, and, when `coord_bands` is given, a
#' number that could be a coordinate is refused.
#'
#' The unit count is read from the value's `n_units` attribute or, for a
#' column of a node's `params` table, from that table's `n_units` column.
#' The largest share of one unit is read the same way from
#' `max_unit_share`. A column passes only when every row passes;
#' [run_return()] calls the check on each row of a `params` field, so a
#' row that fails is withheld alone.
#'
#' A number is coordinate-like when it lies inside one of `coord_bands` and
#' has more than two decimals. The scan applies to a value whose `unit`
#' attribute is missing, or names a coordinate (`coordinate`, `degrees`,
#' `decimal_degrees`, `latitude`, `longitude`, in any case). A value
#' declared in any other unit is not scanned: a rate in kg N/ha or a price
#' in $/ha can fall inside a band without being a place. [run_return()]
#' sets `unit` from the unit a request declares. There is no default band:
#' a band covering every latitude and longitude would refuse almost every
#' estimate, so the holder names the bands of the region the data come
#' from.
#'
#' This check is a floor, not a disclosure guarantee. It does not detect a
#' statistic that reveals a unit through its own value, or a combination of
#' returns that does; differencing between rounds is checked by
#' [run_return()] against the previous round. The data holder remains
#' responsible for what leaves.
#'
#' @param k The smallest number of units a returned statistic may rest on.
#' @param dominance The largest share of the statistic one unit may hold,
#'   between 0 and 1.
#' @param forbidden Strings no returned value may contain, matched as
#'   written: the names of places and trials in the holder's table.
#' @param coord_bands `NULL` for no coordinate scan, or a list of bands,
#'   each a numeric vector of a lower and an upper bound.
#'   `list(c(-45, -10), c(110, 155))` covers the latitudes and longitudes
#'   of south-eastern Australia.
#'
#' @returns A function of one argument, the value, that returns
#'   `NA_character_` when the value may be returned and otherwise the
#'   reason it may not: `"no_unit_count"`, `"too_few_units"`,
#'   `"dominant_unit"`, `"forbidden_string"` or `"coordinate_like"`.
#'
#' @seealso [run_return()].
#'
#' @author Max Moldovan
#'
#' @examples
#' check <- default_checker(k = 5)
#' check(structure(0.42, n_units = 12L))
#' check(structure(0.42, n_units = 3L))
#' check(0.42)
#'
#' scan <- default_checker(forbidden = "Roseworthy",
#'                         coord_bands = list(c(-45, -10), c(110, 155)))
#' scan(structure(138.6042, n_units = 12L))
#' scan(structure(138.6042, n_units = 12L, unit = "kg N/ha"))
#' scan(structure("near Roseworthy", n_units = 12L))
#'
#' @export
default_checker <- function(k = 5, dominance = 0.6,
                            forbidden = character(0), coord_bands = NULL) {
  stopifnot(is.numeric(k), length(k) == 1L, !is.na(k), k >= 1,
            is.numeric(dominance), length(dominance) == 1L,
            !is.na(dominance), dominance > 0, dominance <= 1,
            is.character(forbidden), !anyNA(forbidden),
            all(nzchar(forbidden)))
  if (!is.null(coord_bands)) {
    stopifnot(is.list(coord_bands), length(coord_bands) >= 1L,
              all(vapply(coord_bands, function(b) {
                is.numeric(b) && length(b) == 2L && !anyNA(b) && b[1] < b[2]
              }, logical(1))))
  }
  force(k)
  force(dominance)
  force(forbidden)
  force(coord_bands)
  function(value) {
    n <- attr(value, "n_units", exact = TRUE)
    share <- attr(value, "max_unit_share", exact = TRUE)
    if (is.data.frame(value)) {
      if (is.null(n)) n <- value[["n_units"]]
      if (is.null(share)) share <- value[["max_unit_share"]]
    }
    if (!is.numeric(n) || !length(n) || anyNA(n)) return("no_unit_count")
    if (any(n < k)) return("too_few_units")
    if (is.numeric(share) && any(share > dominance, na.rm = TRUE)) {
      return("dominant_unit")
    }
    if (length(forbidden)) {
      txt <- if (is.data.frame(value)) unlist(lapply(value, as.character))
             else as.character(value)
      txt <- txt[!is.na(txt)]
      if (any(vapply(forbidden, function(f) any(grepl(f, txt, fixed = TRUE)),
                     logical(1)))) {
        return("forbidden_string")
      }
    }
    unit <- attr(value, "unit", exact = TRUE)
    if (!is.null(coord_bands) && .coord_unit(unit)) {
      x <- if (is.data.frame(value)) value[[1]] else value
      if (is.numeric(x) && any(.coord_like(x, coord_bands))) {
        return("coordinate_like")
      }
    }
    NA_character_
  }
}

# A value is scanned for coordinates when its unit is undeclared or names a
# coordinate.
.coord_unit <- function(unit) {
  if (is.null(unit) || length(unit) != 1L || is.na(unit)) return(TRUE)
  tolower(unit) %in% c("coordinate", "degrees", "decimal_degrees",
                       "latitude", "longitude")
}

# Inside a band, with more than two decimals.
.coord_like <- function(x, bands) {
  x <- as.numeric(x)
  ok <- !is.na(x)
  inside <- Reduce(`|`, lapply(bands, function(b) x > b[1] & x < b[2]))
  ok & inside & abs(x - round(x, 2)) > 1e-9
}

# A requested field: `metrics.<name>`, `params.<column>`, a bare metric
# name, or `params.<column>[<key>=<value>,...]` for the rows of one kind.
# The unit count comes from `n_units_field` when one is declared, and only
# from there; the declared unit travels as the `unit` attribute.
.request_value <- function(m, field, unit = NA_character_,
                           n_units_field = NA_character_) {
  if (is.null(m)) return(NULL)
  pf <- .parse_request_field(field)
  has_nf <- !is.na(n_units_field)
  if (startsWith(pf$field, "params.")) {
    col <- sub("^params\\.", "", pf$field)
    tab <- as.data.frame(m@params, stringsAsFactors = FALSE)
    if (!col %in% names(tab)) return(NULL)
    if (!is.null(pf$key)) {
      if (!pf$key %in% names(tab)) return(NULL)
      tab <- tab[as.character(tab[[pf$key]]) %in% pf$values, , drop = FALSE]
      if (!nrow(tab)) return(NULL)
    }
    v <- tab[, col, drop = FALSE]
    if (has_nf) {
      if (n_units_field %in% names(tab)) v$n_units <- tab[[n_units_field]]
    } else if ("n_units" %in% names(tab)) {
      v$n_units <- tab$n_units
    }
    if ("max_unit_share" %in% names(tab)) {
      v$max_unit_share <- tab$max_unit_share
    }
    rownames(v) <- NULL
  } else {
    metrics <- m@summary$metrics
    v <- metrics[[sub("^metrics\\.", "", pf$field)]]
    if (is.null(v) || !is.atomic(v)) return(NULL)
    if (has_nf) {
      n <- metrics[[n_units_field]]
      attr(v, "n_units") <- if (is.numeric(n) && length(n) == 1L) n
                            else NA_real_
    }
  }
  if (!is.na(unit)) attr(v, "unit") <- unit
  v
}

# The text of a returned value: numbers to `digits` significant figures,
# and `|` escaped so the text can sit in a Markdown table.
.format_values <- function(v, digits = 7L) {
  x <- if (is.data.frame(v)) v[[1]] else v
  out <- vapply(x, function(e) {
    if (is.numeric(e)) format(signif(e, digits), digits = digits)
    else as.character(e)
  }, character(1), USE.NAMES = FALSE)
  gsub("|", "\\|", out, fixed = TRUE)
}

# The unit count of each row of a value, for the differencing rule.
.value_units <- function(v) {
  n <- attr(v, "n_units", exact = TRUE)
  if (is.null(n) && is.data.frame(v)) n <- v[["n_units"]]
  if (!is.numeric(n) || !length(n)) n <- NA_real_
  n
}

# The values of the previous round, from a return or its written file.
.previous_values <- function(previous) {
  if (is.null(previous)) return(NULL)
  if (inherits(previous, "conductoR_return")) return(previous$values)
  if (is.character(previous) && length(previous) == 1L &&
      file.exists(previous)) {
    vals <- .read_return_section(previous, "values")
    if (length(vals) < 2L) {
      return(data.frame(node = character(0), field = character(0),
                        index = integer(0), value = character(0),
                        stringsAsFactors = FALSE))
    }
    return(utils::read.csv(text = vals, colClasses = "character",
                           na.strings = character(0)))
  }
  stop("previous must be a return from run_return() or the path of one ",
       "written by write_return()", call. = FALSE)
}

# The checker's verdict on one value: NA_character_ or one reason.
.check_one <- function(checker, v) {
  why <- checker(v)
  if (!(is.character(why) && length(why) == 1L)) {
    stop("checker must return NA_character_ or one reason string",
         call. = FALSE)
  }
  why
}

# Missing in both rounds is unchanged; a written return keeps NA as an
# empty field, so "" and "NA" read back as missing.
.same_text <- function(a, b) {
  miss_a <- is.na(a) | a %in% c("", "NA")
  miss_b <- is.na(b) | b %in% c("", "NA")
  ifelse(miss_a | miss_b, miss_a & miss_b, a == b)
}

#' The message a run returns to the composer
#'
#' Builds what leaves the data holder after a run: the dose-0 record and its
#' typed message lines, and, at dose 1 or above, the values the plan's
#' requests name, each passed through `checker` first. A value the checker
#' refuses, or one requested at a dose above the run's, is listed with the
#' reason and never returned.
#'
#' @param run A run returned by [conduct()] or [try_conduct()].
#' @param triggers A trigger table, as described in [evaluate_triggers()],
#'   or `NULL`.
#' @param dose The dose of this return: 0 for the record alone, 1 or above
#'   for the record and the requested values up to that dose.
#' @param requests A named list keyed by node id. Each element is a named
#'   vector giving, for each requested field, the dose it needs, as in
#'   `list(quorum = c(width_ratio = 1, "params.estimate" = 2))`. A field is
#'   a metric of the node's `summary$metrics`, written bare or as
#'   `metrics.<name>`, or a column of its `params` table, written
#'   `params.<column>`. Or a data frame with the columns `node`, `field`
#'   and `dose`, and optionally `unit`, `n_units_field` and `digits`, as
#'   [plan_returns()] gives it.
#' @param checker A function of one value that returns `NA_character_` when
#'   the value may be returned, else the reason it may not. See
#'   [default_checker()].
#' @param series A short string naming the series of runs this return
#'   belongs to, or `NULL`.
#' @param round The round of the series, a whole number, or `NULL`.
#' @param checks Passed to [run_record()].
#' @param previous The previous round of the series, as a return from
#'   [run_return()] or the path of one written by [write_return()], or
#'   `NULL` for no differencing.
#' @param k_diff The smallest number of units a value changed since the
#'   previous round may rest on. Used only with `previous`.
#' @param digits The significant figures of returned numbers, unless a
#'   request declares its own.
#'
#' @returns An object of class `conductoR_return`: a list with `plan_id`,
#'   `plan_hash`, `input_hash`, `series`, `round`, `dose`, `time`, `lines`
#'   (the typed message), `record`, `values` (one row per returned value:
#'   `node`, `field`, `dose`, `index`, `value` as text), `refused` (one row
#'   per requested field not returned, or per reason rows of a `params`
#'   field were withheld: `node`, `field`, `dose`, `reason`, and `n_rows`,
#'   the number of rows withheld for that reason, `NA` for a field refused
#'   whole) and `released` (`TRUE` when a value was returned). It holds no trigger
#'   value, no error message and nothing else from the run. Printing it
#'   shows the text [write_return()] writes.
#'
#' @section Reasons a value is not returned:
#' `above_dose` (the request needs a higher dose than the return),
#' `not_available` (the node did not run, has no such field, or no row
#' passes the row filter), `differencing` (see below), and whatever reason
#' the checker gives.
#'
#' @section Rows of a params field:
#' A `params` field is checked row by row: the checker sees each row with
#' its own unit count, a row it refuses is withheld, and the rows that pass
#' are returned with their row number as `index`. `refused` counts the
#' withheld rows by reason in `n_rows` and names none of them, so the
#' composer learns how many rows are missing and why, and not which. A
#' metric is checked whole. Withholding a row does not stop it being
#' solved from a released total and its released siblings: pass the
#' release through [suppress_return()] with the partitions it carries.
#'
#' @section Declared fields:
#' A request in the data-frame form, as [plan_returns()] reads it from the
#' plan, may declare more than its dose. `unit` is set as the value's `unit`
#' attribute before the checker sees it, so [default_checker()] scans a
#' rate or a price for coordinates only when it is declared as one.
#' `n_units_field` names the metric, or the `params` column, that holds the
#' unit count; when it is declared the count is read from there and nowhere
#' else, and a missing field is no count. `digits` rounds the value's
#' numbers to that many significant figures, in place of `digits`.
#'
#' A `params` field may carry a row filter, `params.<column>[<key>=<value>]`,
#' with several values separated by commas. Only the rows whose `key` column
#' holds one of the values are returned, and the unit count is checked on
#' those rows alone, so rows of two kinds in one table are requested apart.
#'
#' @section Differencing across rounds:
#' Given the `previous` round of the series, a value that passes the checker
#' is compared with the value returned there under the same node and field.
#' When it changed, it is returned only if every row rests on at least
#' `k_diff` units; otherwise it is refused as `differencing`, since the
#' change between two rounds can reveal the few units behind it. A value
#' missing in both rounds is unchanged. A field the previous round did not
#' return is not compared. The rows of a `params` field are compared one by
#' one, by `index`: a changed row resting on fewer than `k_diff` units is
#' withheld as `differencing` and the other rows are returned, and a row
#' the previous round did not return is not compared.
#'
#' @seealso [run_record()], [write_return()], [series_report()],
#'   [plan_returns()], [default_checker()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "demo", "demo:lens",
#'                    metrics = list(slope = structure(0.31, n_units = 24L),
#'                                   spread = structure(1.1, n_units = 3L)))
#' }
#' run <- conduct(oap_plan(id = "doses", nodes = list(
#'   node("lens", lens, emits = "treatment_effects"))), verbose = FALSE)
#' ret <- run_return(run, dose = 1, series = "demo", round = 1,
#'                   requests = list(lens = c(slope = 1, spread = 1)))
#' ret$values
#' ret$refused
#'
#' @export
run_return <- function(run, triggers = NULL, dose = 0, requests = NULL,
                       checker = default_checker(k = 5), series = NULL,
                       round = NULL, checks = NULL, previous = NULL,
                       k_diff = 10, digits = 7) {
  stopifnot(is.list(run),
            is.numeric(dose), length(dose) == 1L, !is.na(dose), dose >= 0,
            dose == round(dose), is.function(checker),
            is.numeric(k_diff), length(k_diff) == 1L, !is.na(k_diff),
            .is_digits(digits))
  if (!is.null(series)) {
    stopifnot(is.character(series), length(series) == 1L, nzchar(series))
  }
  if (!is.null(round)) {
    stopifnot(is.numeric(round), length(round) == 1L, !is.na(round),
              round >= 1, round == round(round))
  }
  record <- run_record(run, triggers = triggers, checks = checks)
  reqs <- .check_requests(requests, .run_node_ids(run))
  prev <- .previous_values(previous)

  # Each requested field: withheld, refused or returned ----------------------
  values <- list()
  refused <- list()
  for (i in seq_len(nrow(reqs))) {
    r <- reqs[i, ]
    why <- NA_character_
    v <- NULL
    if (r$dose > dose) {
      why <- "above_dose"
    } else {
      v <- .request_value(run$bus[[r$node]], r$field, r$unit,
                          r$n_units_field)
      if (is.null(v) || (is.data.frame(v) && !nrow(v))) {
        why <- "not_available"
      } else if (!is.data.frame(v)) {
        why <- .check_one(checker, v)
      }
    }
    # A params column: each row is checked, and differenced, on its own
    if (is.na(why) && is.data.frame(v)) {
      fv <- .format_values(v, if (is.na(r$digits)) digits else r$digits)
      unit_attr <- attr(v, "unit", exact = TRUE)
      row_why <- vapply(seq_len(nrow(v)), function(j) {
        vj <- v[j, , drop = FALSE]
        attr(vj, "unit") <- unit_attr
        .check_one(checker, vj)
      }, character(1))
      before <- if (is.null(prev)) NULL
                else prev[prev$node == r$node & prev$field == r$field, ,
                          drop = FALSE]
      if (!is.null(before) && nrow(before)) {
        old <- before$value[match(seq_along(fv), as.integer(before$index))]
        n <- rep_len(.value_units(v), length(fv))
        thin <- is.na(row_why) & !is.na(old) & !.same_text(fv, old) &
          (is.na(n) | n < k_diff)
        row_why[thin] <- "differencing"
      }
      keep <- is.na(row_why)
      if (any(keep)) {
        values[[length(values) + 1L]] <- data.frame(
          node = r$node, field = r$field, dose = r$dose,
          index = which(keep), value = fv[keep], stringsAsFactors = FALSE)
      }
      if (any(!keep)) {
        n_why <- table(row_why[!keep])
        refused[[length(refused) + 1L]] <- data.frame(
          node = r$node, field = r$field, dose = r$dose,
          reason = names(n_why), n_rows = as.integer(n_why),
          stringsAsFactors = FALSE)
      }
      next
    }
    if (is.na(why)) {
      fv <- .format_values(v, if (is.na(r$digits)) digits else r$digits)
      before <- if (is.null(prev)) NULL
                else prev[prev$node == r$node & prev$field == r$field, ,
                          drop = FALSE]
      if (!is.null(before) && nrow(before)) {
        old <- before$value[match(seq_along(fv), as.integer(before$index))]
        changed <- length(fv) != nrow(before) | !.same_text(fv, old)
        n <- rep_len(.value_units(v), length(fv))
        if (any(changed & (is.na(n) | n < k_diff))) why <- "differencing"
      }
    }
    if (is.na(why)) {
      values[[length(values) + 1L]] <- data.frame(
        node = r$node, field = r$field, dose = r$dose,
        index = seq_along(fv), value = fv, stringsAsFactors = FALSE)
    } else {
      refused[[length(refused) + 1L]] <- data.frame(
        node = r$node, field = r$field, dose = r$dose, reason = why,
        n_rows = NA_integer_, stringsAsFactors = FALSE)
    }
  }
  values <- if (length(values)) do.call(rbind, values)
            else data.frame(node = character(0), field = character(0),
                            dose = numeric(0), index = integer(0),
                            value = character(0), stringsAsFactors = FALSE)
  refused <- if (length(refused)) do.call(rbind, refused)
             else data.frame(node = character(0), field = character(0),
                             dose = numeric(0), reason = character(0),
                             n_rows = integer(0), stringsAsFactors = FALSE)
  structure(list(
    plan_id = run$plan_id %||% NA_character_,
    plan_hash = run$plan_hash %||% NA_character_,
    input_hash = run$input_hash %||% NA_character_,
    series = series %||% NA_character_,
    round = if (is.null(round)) NA_integer_ else as.integer(round),
    dose = as.integer(dose),
    time = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    lines = format_record(record), record = record,
    values = values, refused = refused,
    released = dose > 0 && nrow(values) > 0L),
    class = "conductoR_return")
}

# The requests as a table: node, field, dose, unit, n_units_field, digits;
# refused when a node is not in the run, a dose is not a whole number of at
# least 1, a field is requested twice or a row filter is malformed.
.check_requests <- function(requests, node_ids) {
  cols <- c("node", "field", "dose", "unit", "n_units_field", "digits")
  empty <- data.frame(node = character(0), field = character(0),
                      dose = numeric(0), unit = character(0),
                      n_units_field = character(0), digits = integer(0),
                      stringsAsFactors = FALSE)
  if (is.null(requests) || !length(requests)) return(empty)
  if (is.data.frame(requests)) {
    out <- .check_request_table(requests, cols)
    if (!nrow(out)) return(empty)
  } else {
    if (!is.list(requests) || is.null(names(requests)) ||
        any(!nzchar(names(requests)))) {
      stop("requests must be a list named by node id", call. = FALSE)
    }
    out <- NULL
  }
  ids <- if (is.null(out)) names(requests) else unique(out$node)
  unknown <- setdiff(ids, node_ids)
  if (length(unknown)) {
    stop(sprintf("requests name nodes that are not in the run: %s",
                 paste(unknown, collapse = ", ")), call. = FALSE)
  }
  if (is.null(out)) {
    rows <- lapply(names(requests), function(id) {
      d <- requests[[id]]
      if (!is.numeric(d) || is.null(names(d)) || any(!nzchar(names(d))) ||
          anyNA(d) || any(d < 1) || any(d != round(d))) {
        stop(sprintf(paste0("requests for '%s' must be a named vector of ",
                            "doses, each a whole number of at least 1"), id),
             call. = FALSE)
      }
      data.frame(node = id, field = names(d), dose = as.numeric(d),
                 unit = NA_character_, n_units_field = NA_character_,
                 digits = NA_integer_, stringsAsFactors = FALSE)
    })
    out <- do.call(rbind, rows)
  }
  bad <- out$field[vapply(out$field, function(f) {
    is.null(.parse_request_field(f))
  }, logical(1))]
  if (length(bad)) {
    stop(sprintf(paste0("requested fields with a malformed row filter: %s; ",
                        "write a row filter on a params column as ",
                        "params.estimate[kind=state]"),
                 paste(bad, collapse = ", ")), call. = FALSE)
  }
  twice <- duplicated(out[, c("node", "field")])
  if (any(twice)) {
    stop(sprintf("fields requested twice: %s",
                 paste(out$node[twice], out$field[twice], sep = ".",
                       collapse = ", ")), call. = FALSE)
  }
  rownames(out) <- NULL
  out
}

# The data-frame form of requests, with every optional column filled in.
.check_request_table <- function(requests, cols) {
  if (!all(c("node", "field", "dose") %in% names(requests))) {
    stop("a requests table must have the columns node, field and dose",
         call. = FALSE)
  }
  out <- as.data.frame(requests, stringsAsFactors = FALSE)
  for (k in c("unit", "n_units_field")) {
    out[[k]] <- if (k %in% names(out)) as.character(out[[k]])
                else rep(NA_character_, nrow(out))
  }
  if (!"digits" %in% names(out)) out$digits <- rep(NA_integer_, nrow(out))
  d <- out$dose
  if (!is.numeric(d) || anyNA(d) || any(d < 1) || any(d != round(d))) {
    stop("requests: every dose must be a whole number of at least 1",
         call. = FALSE)
  }
  g <- out$digits
  if (!all(is.na(g) | vapply(g, .is_digits, logical(1)))) {
    stop("requests: digits must be a whole number from 1 to 15",
         call. = FALSE)
  }
  out$node <- as.character(out$node)
  out$field <- as.character(out$field)
  out$dose <- as.numeric(d)
  out$digits <- as.integer(g)
  out[, cols]
}

.csv_lines <- function(tab) {
  if (!nrow(tab)) return(paste(names(tab), collapse = ","))
  utils::capture.output(utils::write.csv(tab, row.names = FALSE, na = ""))
}

#' @export
format.conductoR_return <- function(x, ...) {
  c("conductoR return",
    sprintf("plan_id: %s", x$plan_id),
    sprintf("plan_hash: %s", x$plan_hash),
    sprintf("input_hash: %s", x$input_hash),
    sprintf("series: %s", x$series),
    sprintf("round: %s", x$round),
    sprintf("dose: %d", x$dose),
    sprintf("released: %s", x$released),
    sprintf("time: %s", x$time),
    "", "[message]", x$lines,
    "", "[record]", .csv_lines(x$record),
    "", "[values]", .csv_lines(x$values),
    "", "[refused]", .csv_lines(x$refused))
}

#' @export
print.conductoR_return <- function(x, ...) {
  writeLines(format(x))
  invisible(x)
}

#' Write a return to a file
#'
#' Writes the text of a return: a header with the plan id, `plan_hash`,
#' `input_hash`, series, round, dose, whether a value was released and the
#' time the return was built, then the sections `[message]`, `[record]`,
#' `[values]` and `[refused]`, the last three as CSV.
#'
#' @section A series of runs:
#' The rounds of one analysis share a folder, one sub-folder per round:
#' `<series>/round_<k>/return.txt`. Give `path` as the series folder and the
#' file goes to the sub-folder of the return's round; give a path ending in
#' `.txt` to write that file instead. [series_report()] reads a series
#' folder back. A return already written is never replaced: writing to an
#' existing file is an error.
#'
#' @param ret A return from [run_return()].
#' @param path A series folder, or a file path ending in `.txt`.
#'
#' @returns The path of the file written, invisibly.
#'
#' @seealso [run_return()], [series_report()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "demo", "demo:lens")
#' }
#' run <- conduct(oap_plan(id = "w", nodes = list(
#'   node("lens", lens, emits = "treatment_effects"))), verbose = FALSE)
#' dir <- file.path(tempdir(), "demo_series")
#' f <- write_return(run_return(run, series = "demo", round = 1), dir)
#' readLines(f)[1:9]
#' series_report(dir)
#'
#' @export
write_return <- function(ret, path) {
  stopifnot(inherits(ret, "conductoR_return"), is.character(path),
            length(path) == 1L, nzchar(path))
  if (!grepl("\\.txt$", path)) {
    if (is.na(ret$round)) {
      stop("a return written into a series folder needs a round",
           call. = FALSE)
    }
    path <- file.path(path, paste0("round_", ret$round), "return.txt")
  }
  if (file.exists(path)) {
    stop(sprintf("a return already exists at %s; it is not replaced", path),
         call. = FALSE)
  }
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  writeLines(format(ret), path)
  invisible(path)
}

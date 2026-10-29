# declared.R -- What a node declares before the run: the values it may
# return, the triggers it carries and the hypotheses it tests, each kept in
# canonical order so a plan hashes the same however it was built.

.RETURN_FIELDS <- c("field", "dose", "unit", "n_units_field", "digits")
# The trigger columns of record.R without `node`, which the node supplies.
.NODE_TRIGGER_FIELDS <- c("trigger_id", "class", "signal_type", "statistic",
                          "direction", "threshold", "action",
                          "rehearsal_plant")
.HYPOTHESIS_FIELDS <- c("hypothesis_id", "statistic", "direction",
                        "threshold", "firing_plant", "silent_plant")

# Records from a data frame (one per row, missing optional cells dropped),
# a named numeric vector of doses (returns only) or a list of mappings.
.as_records <- function(x, doses = FALSE) {
  if (is.null(x)) return(list())
  if (doses && is.numeric(x) && !is.null(names(x))) {
    return(Map(function(f, d) list(field = f, dose = d), names(x),
               unname(x), USE.NAMES = FALSE))
  }
  if (is.data.frame(x)) {
    return(lapply(seq_len(nrow(x)), function(i) {
      r <- lapply(as.list(x[i, , drop = FALSE]), function(v) {
        if (is.factor(v)) as.character(v) else v
      })
      Filter(function(v) !(length(v) == 1L && is.na(v)), r)
    }))
  }
  x
}

# One record in canonical field order with canonical types: whole numbers
# in `ints` as integers, numbers in `nums` as doubles.
.canon_record <- function(r, fields, ints = character(0),
                          nums = character(0)) {
  out <- list()
  for (f in fields) {
    v <- r[[f]]
    if (is.null(v)) next
    if (f %in% ints && is.numeric(v) && length(v) == 1L &&
        isTRUE(v == round(v))) v <- as.integer(v)
    if (f %in% nums && is.numeric(v)) v <- as.double(v)
    out[[f]] <- v
  }
  out
}

.canon_returns <- function(x) {
  lapply(x, .canon_record, fields = .RETURN_FIELDS,
         ints = c("dose", "digits"))
}

# A requested field split into the field itself and its row filter:
# `params.<column>[<key>=<value>,<value>]` keeps the rows whose `key` column
# holds one of the values. Returns `list(field, key, values)`, `key` NULL
# when there is no filter, or NULL when the filter is malformed.
.parse_request_field <- function(field) {
  if (!grepl("[][]", field)) {
    return(list(field = field, key = NULL, values = NULL))
  }
  re <- "^(params\\.[^][]+)\\[([A-Za-z][A-Za-z0-9_.]*)=([^][]+)\\]$"
  if (!grepl(re, field)) return(NULL)
  list(field = sub(re, "\\1", field), key = sub(re, "\\2", field),
       values = strsplit(sub(re, "\\3", field), ",", fixed = TRUE)[[1]])
}

.is_digits <- function(x) .is_whole(x) && x >= 1 && x <= 15

.canon_triggers <- function(x) {
  lapply(x, .canon_record, fields = .NODE_TRIGGER_FIELDS, nums = "threshold")
}

.canon_hypotheses <- function(x) {
  lapply(x, .canon_record, fields = .HYPOTHESIS_FIELDS, nums = "threshold")
}

# The 0.3 fields a node sets, in canonical form; empty fields left out.
.node_v03_fields <- function(n) {
  out <- list()
  if (length(n@returns)) out$returns <- .canon_returns(n@returns)
  if (length(n@triggers)) out$triggers <- .canon_triggers(n@triggers)
  if (length(n@hypotheses)) out$hypotheses <- .canon_hypotheses(n@hypotheses)
  out
}

.uses_v03 <- function(plan) {
  length(plan@inputs) > 0L || any(vapply(plan@nodes, function(n) {
    length(.node_v03_fields(n)) > 0L
  }, logical(1)))
}

# Checks the records of one declared field: a list of mappings, each with
# only the allowed fields and with every required field present.
.records_rules <- function(x, field, allowed, required, example) {
  out <- list()
  add <- function(f, rule, fix) out[[length(out) + 1L]] <<- .prob(f, rule, fix)
  if (!is.list(x) || !is.null(names(x))) {
    add(field, sprintf("must be a list of %s", field), example)
    return(list(problems = out, ok = list()))
  }
  ok <- list()
  for (i in seq_along(x)) {
    r <- x[[i]]
    f <- sprintf("%s[%d]", field, i)
    if (!.is_map(r) || length(r) == 0L) {
      add(f, "must be a mapping", example)
      next
    }
    for (k in setdiff(names(r), allowed)) {
      add(paste0(f, ".", k), sprintf("is not a field of %s", field),
          sprintf("the fields are %s", paste(allowed, collapse = ", ")))
    }
    for (k in setdiff(required, names(r))) {
      add(paste0(f, ".", k), "is required", example)
    }
    ok[[length(ok) + 1L]] <- list(i = i, r = r, f = f)
  }
  list(problems = out, ok = ok)
}

# Rules of a node's `returns`, `triggers` and `hypotheses`. Returns a list
# of problems.
.node_v03_rules <- function(returns, triggers, hypotheses) {
  out <- list()
  add <- function(f, rule, fix) out[[length(out) + 1L]] <<- .prob(f, rule, fix)
  word <- "^[A-Za-z][A-Za-z0-9_.]*$"

  rr <- .records_rules(returns %||% list(), "returns", .RETURN_FIELDS,
                       c("field", "dose"),
                       "write {field: \"slope\", dose: 1}")
  out <- c(out, rr$problems)
  seen <- character(0)
  for (e in rr$ok) {
    r <- e$r
    if (!.is_string(r$field) || !nzchar(r$field)) {
      add(paste0(e$f, ".field"), "must name a metric or params.<column>",
          "write field: \"slope\" or field: \"params.estimate\"")
    } else if (is.null(.parse_request_field(r$field))) {
      add(paste0(e$f, ".field"), "has a malformed row filter",
          "filter a params column: \"params.estimate[kind=state]\"")
    } else if (r$field %in% seen) {
      add(paste0(e$f, ".field"), sprintf("'%s' is requested twice", r$field),
          "request each field once")
    } else {
      seen <- c(seen, r$field)
    }
    if (!is.null(r$dose) && !(.is_whole(r$dose) && r$dose >= 1)) {
      add(paste0(e$f, ".dose"), "must be a whole number of at least 1",
          "write dose: 1")
    }
    for (k in c("unit", "n_units_field")) {
      if (!is.null(r[[k]]) && !(.is_string(r[[k]]) && nzchar(r[[k]]))) {
        add(paste0(e$f, ".", k), "must be a single string",
            sprintf("write %s as text in quotes", k))
      }
    }
    if (!is.null(r$digits) && !.is_digits(r$digits)) {
      add(paste0(e$f, ".digits"), "must be a whole number from 1 to 15",
          "write digits: 3")
    }
  }

  tr <- .records_rules(triggers %||% list(), "triggers", .NODE_TRIGGER_FIELDS,
                       setdiff(.NODE_TRIGGER_FIELDS, "rehearsal_plant"),
                       paste0("write {trigger_id:, class:, signal_type:, ",
                              "statistic:, direction:, threshold:, action:}"))
  out <- c(out, tr$problems)
  for (e in tr$ok) {
    r <- e$r
    chk <- function(k, ok, rule, fix) {
      if (!is.null(r[[k]]) && !ok) add(paste0(e$f, ".", k), rule, fix)
    }
    chk("trigger_id", .is_string(r$trigger_id) && grepl(word, r$trigger_id),
        "must start with a letter and hold letters, digits, _ or .",
        "write trigger_id: \"T_spread\"")
    chk("class", .is_string(r$class) && r$class %in% .TRIGGER_CLASSES,
        "must be anomaly or candidate", "write class: \"anomaly\"")
    chk("signal_type", .is_string(r$signal_type) &&
          grepl("^(signal:)?[a-z][a-z0-9_]*$", r$signal_type),
        "must be a lower-case word", "write signal_type: \"lens_spread\"")
    chk("statistic", .is_string(r$statistic) && nzchar(r$statistic),
        "must name a metric", "write statistic: \"spread\"")
    chk("direction", .is_string(r$direction) &&
          r$direction %in% .TRIGGER_DIRECTIONS,
        "must be one of >, >=, <, <=, ==", "write direction: \">\"")
    chk("threshold", is.numeric(r$threshold) && length(r$threshold) == 1L &&
          is.finite(r$threshold),
        "must be a number", "write threshold: 1.3")
    chk("action", .is_string(r$action) && r$action %in% .TRIGGER_ACTIONS,
        "must be return, branch or release", "write action: \"return\"")
    chk("rehearsal_plant", .is_string(r$rehearsal_plant),
        "must be a single string", "describe the plant in quotes")
  }

  hr <- .records_rules(hypotheses %||% list(), "hypotheses",
                       .HYPOTHESIS_FIELDS, .HYPOTHESIS_FIELDS,
                       paste0("write {hypothesis_id:, statistic:, direction:, ",
                              "threshold:, firing_plant:, silent_plant:}"))
  out <- c(out, hr$problems)
  for (e in hr$ok) {
    r <- e$r
    chk <- function(k, ok, rule, fix) {
      if (!is.null(r[[k]]) && !ok) add(paste0(e$f, ".", k), rule, fix)
    }
    chk("hypothesis_id", .is_string(r$hypothesis_id) &&
          grepl(word, r$hypothesis_id),
        "must start with a letter and hold letters, digits, _ or .",
        "write hypothesis_id: \"H_slope\"")
    chk("statistic", .is_string(r$statistic) && nzchar(r$statistic),
        "must name a metric", "write statistic: \"slope\"")
    chk("direction", .is_string(r$direction) &&
          r$direction %in% .TRIGGER_DIRECTIONS,
        "must be one of >, >=, <, <=, ==", "write direction: \"<\"")
    chk("threshold", is.numeric(r$threshold) && length(r$threshold) == 1L &&
          is.finite(r$threshold),
        "must be a number", "write threshold: 0.0")
    for (k in c("firing_plant", "silent_plant")) {
      chk(k, .is_string(r[[k]]) && nzchar(r[[k]]),
          "must describe the plant in rehearsal",
          sprintf("write %s as text in quotes", k))
    }
  }
  out
}

.node_v03_problems <- function(n) {
  vapply(
    .node_v03_rules(n@returns, n@triggers, n@hypotheses),
    function(p) {
      sprintf("node '%s', %s: %s. Fix: %s.", n@id, p$field, p$rule, p$fix)
    },
    character(1)
  )
}

# Trigger and hypothesis ids must be unique across the plan; `nodes` is a
# list of lists with `id`, `triggers` and `hypotheses`.
.declared_id_rules <- function(nodes) {
  out <- list()
  for (kind in c("trigger_id", "hypothesis_id")) {
    field <- if (kind == "trigger_id") "triggers" else "hypotheses"
    ids <- character(0)
    owner <- character(0)
    for (n in nodes) {
      for (r in n[[field]] %||% list()) {
        v <- r[[kind]]
        if (.is_string(v)) {
          ids <- c(ids, v)
          owner <- c(owner, n$id)
        }
      }
    }
    for (d in unique(ids[duplicated(ids)])) {
      out[[length(out) + 1L]] <- c(
        node = owner[ids == d][2],
        .prob(field, sprintf("%s '%s' is used twice in the plan", kind, d),
              sprintf("give each %s its own %s", sub("s$", "", field), kind)))
    }
  }
  out
}

# The plan-level 0.3 rules of a plan object: its inputs, and trigger and
# hypothesis ids unique across nodes.
.plan_v03_problems <- function(plan) {
  fmt <- function(p) {
    node <- p[["node"]] %||% ""
    sprintf("%s%s: %s. Fix: %s.",
            if (nzchar(node)) sprintf("node '%s', ", node) else "",
            p$field, p$rule, p$fix)
  }
  nodes <- lapply(plan@nodes, function(n) {
    list(id = n@id, triggers = n@triggers, hypotheses = n@hypotheses)
  })
  probs <- c(.inputs_rules(plan@inputs), .declared_id_rules(nodes))
  vapply(probs, fmt, character(1))
}

#' The values, triggers and hypotheses a plan declares
#'
#' A plan in language version 0.3 declares on each node what it may return
#' (`returns`), the signals it carries (`triggers`) and the hypotheses it
#' tests (`hypotheses`). These functions read the declarations into the
#' forms [run_return()] and [evaluate_triggers()] take, so a holder's runner
#' needs the plan file and nothing else.
#'
#' @param plan An [oap_plan].
#'
#' @returns `plan_requests()`: a list named by node id, each element a named
#'   vector of the dose each requested field needs, as `run_return(requests
#'   =)` takes it; an empty list when nothing is requested.
#'   `plan_returns()`: the same requests with what each declares besides
#'   its dose, a data frame with one row per requested field and the
#'   columns `node`, `field`, `dose`, `unit`, `n_units_field` and `digits`
#'   (missing where not declared), which `run_return(requests =)` also
#'   takes.
#'   `plan_triggers()`: a trigger table as [evaluate_triggers()] takes it,
#'   with the node column filled in. `plan_hypotheses()`: a data frame with
#'   one row per hypothesis and the columns `node`, `hypothesis_id`,
#'   `statistic`, `direction`, `threshold`, `firing_plant` and
#'   `silent_plant`.
#'
#' @seealso [node()], [run_return()], [evaluate_triggers()].
#'
#' @author Max Moldovan
#'
#' @examples
#' p <- oap_plan(id = "declared", nodes = list(
#'   node("lens", tool = "my_lens", emits = "treatment_effects",
#'        returns = c(slope = 1, "params.estimate" = 2),
#'        triggers = data.frame(trigger_id = "T_spread", class = "anomaly",
#'                              signal_type = "lens_spread",
#'                              statistic = "spread", direction = ">",
#'                              threshold = 1.3, action = "return"),
#'        hypotheses = data.frame(hypothesis_id = "H_slope",
#'                                statistic = "slope", direction = ">",
#'                                threshold = 0,
#'                                firing_plant = "slope of 0.3 planted",
#'                                silent_plant = "flat response planted"))))
#' plan_requests(p)
#' plan_returns(p)
#' plan_triggers(p)
#' plan_hypotheses(p)
#'
#' @name plan_declared
NULL

#' @rdname plan_declared
#' @export
plan_requests <- function(plan) {
  stopifnot(S7::S7_inherits(plan, oap_plan))
  out <- list()
  for (n in plan@nodes) {
    rs <- .canon_returns(n@returns)
    if (!length(rs)) next
    out[[n@id]] <- stats::setNames(
      vapply(rs, function(r) as.numeric(r$dose), numeric(1)),
      vapply(rs, function(r) r$field, character(1)))
  }
  out
}

#' @rdname plan_declared
#' @export
plan_returns <- function(plan) {
  stopifnot(S7::S7_inherits(plan, oap_plan))
  out <- .declared_table(plan, "returns", .canon_returns,
                         c("node", .RETURN_FIELDS))
  out$dose <- as.numeric(out$dose)
  for (k in c("unit", "n_units_field")) out[[k]] <- as.character(out[[k]])
  out$digits <- as.integer(out$digits)
  out
}

.declared_table <- function(plan, field, canon, cols) {
  rows <- list()
  for (n in plan@nodes) {
    for (r in canon(S7::prop(n, field))) {
      vals <- lapply(cols, function(k) {
        if (k == "node") n@id else r[[k]] %||% NA
      })
      rows[[length(rows) + 1L]] <- as.data.frame(
        stats::setNames(vals, cols), stringsAsFactors = FALSE)
    }
  }
  if (length(rows)) {
    out <- do.call(rbind, rows)
    rownames(out) <- NULL
    return(out)
  }
  as.data.frame(stats::setNames(
    lapply(cols, function(k) {
      if (k == "threshold") numeric(0) else character(0)
    }),
    cols), stringsAsFactors = FALSE)
}

#' @rdname plan_declared
#' @export
plan_triggers <- function(plan) {
  stopifnot(S7::S7_inherits(plan, oap_plan))
  .declared_table(plan, "triggers", .canon_triggers,
                  c(.TRIGGER_COLS, "rehearsal_plant"))
}

#' @rdname plan_declared
#' @export
plan_hypotheses <- function(plan) {
  stopifnot(S7::S7_inherits(plan, oap_plan))
  .declared_table(plan, "hypotheses", .canon_hypotheses,
                  c("node", .HYPOTHESIS_FIELDS))
}

# inputs.R -- The plan's declared inputs and the postconditions each must meet
# before the first node runs (plan language 0.3).

.INPUT_FIELDS <- c("name", "key", "postconditions")
.POSTCONDITION_FIELDS <- c("id", "condition")
.POST_SUMMARIES <- c("n_rows", "n_cols")
.POST_COLUMN_SUMMARIES <- c("n_missing", "n_distinct", "n_duplicated", "min",
                            "max", "levels")

# A table summary in a postcondition: .n_rows, .n_cols or
# .<summary>.<column>. NULL for any other name.
.post_ref <- function(name) {
  if (name %in% paste0(".", .POST_SUMMARIES)) {
    return(list(summary = substring(name, 2L), column = NA_character_))
  }
  pat <- sprintf("^\\.(%s)\\.(.+)$",
                 paste(.POST_COLUMN_SUMMARIES, collapse = "|"))
  m <- regmatches(name, regexec(pat, name))[[1]]
  if (length(m) == 0L) return(NULL)
  list(summary = m[2], column = m[3])
}

# An input as the plan stores it: name, key (character) and postconditions
# (a list of id and condition), from a list, a YAML mapping or plan_input().
.as_input <- function(x) {
  pc <- x$postconditions %||% list()
  if (is.character(pc)) {
    ids <- names(pc) %||% rep("", length(pc))
    ids[!nzchar(ids)] <- sprintf("P%d", which(!nzchar(ids)))
    pc <- Map(function(i, cnd) list(id = i, condition = cnd), ids,
              unname(pc), USE.NAMES = FALSE)
  }
  pc <- lapply(pc, function(p) list(id = p$id, condition = p$condition))
  out <- list(name = x$name)
  if (length(x$key)) out$key <- as.character(unlist(x$key))
  out$postconditions <- pc
  out
}

#' Declare an input of a plan
#'
#' An input is a table the plan's tools read, named as it is named in the
#' `data` passed to [conduct()]. Its postconditions are what the table must
#' satisfy before the first node runs; a table that fails one stops the run,
#' see the *Input postconditions* section of [conduct()].
#'
#' @param name The name of the table: an element of `data` when `data` is a
#'   list, or `"data"` for `data` itself when it is a data frame.
#' @param postconditions A character vector of conditions, named by their
#'   ids (an unnamed condition is given the id `P<n>`), written in the
#'   grammar described in [check_postconditions()].
#' @param key Columns whose combined values must be unique in every row and
#'   never missing, or `NULL`.
#'
#' @returns A list with `name`, `key` (when given) and `postconditions`, for
#'   the `inputs` of an [oap_plan].
#'
#' @seealso [check_postconditions()], [conduct()].
#'
#' @author Max Moldovan
#'
#' @examples
#' trials <- plan_input("trials", key = "trial_id",
#'                      postconditions = c(P_rows = ".n_rows == 48",
#'                                         P_days = "window_days <= 31"))
#' p <- oap_plan(id = "with_inputs", inputs = list(trials),
#'               nodes = list(node("fit", tool = "my_fit",
#'                                 emits = "parameters")))
#' cat(plan_to_yaml(p))
#'
#' @export
plan_input <- function(name, postconditions = character(0), key = NULL) {
  stopifnot(is.character(postconditions))
  .as_input(list(name = name, key = key, postconditions = postconditions))
}

# Rules of the plan's `inputs`. Returns a list of problems.
.inputs_rules <- function(inputs) {
  out <- list()
  add <- function(field, rule, fix) {
    out[[length(out) + 1L]] <<- .prob(field, rule, fix)
  }
  if (is.null(inputs)) return(out)
  if (!is.list(inputs) || !is.null(names(inputs))) {
    add("inputs", "must be a list of inputs",
        "write each input as {name:, key:, postconditions:}")
    return(out)
  }
  names_seen <- character(0)
  for (i in seq_along(inputs)) {
    x <- inputs[[i]]
    f <- sprintf("inputs[%d]", i)
    if (!.is_map(x) || length(x) == 0L) {
      add(f, "an input must be a mapping", "write name: and postconditions:")
      next
    }
    for (k in setdiff(names(x), .INPUT_FIELDS)) {
      add(paste0(f, ".", k), "is not an input field",
          sprintf("input fields are %s", paste(.INPUT_FIELDS, collapse = ", ")))
    }
    nm <- x$name
    if (!.is_string(nm) || !grepl("^[A-Za-z][A-Za-z0-9_.]*$", nm)) {
      add(paste0(f, ".name"), "the input needs a name",
          "write the name of the table in data: \"trials\"")
    } else if (nm %in% names_seen) {
      add(paste0(f, ".name"), sprintf("input '%s' is declared twice", nm),
          "declare each input once")
    } else {
      names_seen <- c(names_seen, nm)
    }
    key <- x$key
    key_ok <- is.null(key) ||
      (is.character(unlist(key)) && length(unlist(key)) > 0L &&
         all(nzchar(unlist(key))) && !anyNA(unlist(key)))
    if (!key_ok) {
      add(paste0(f, ".key"), "must name one or more columns",
          "write key: [\"trial_id\"]")
    }
    pc <- x$postconditions %||% list()
    if (!is.list(pc) || !is.null(names(pc))) {
      add(paste0(f, ".postconditions"), "must be a list of postconditions",
          "write each as {id: \"P_rows\", condition: \".n_rows == 48\"}")
      next
    }
    if (length(pc) == 0L && is.null(key)) {
      add(f, "an input declares nothing to check",
          "add a postcondition or a key")
    }
    ids_seen <- character(0)
    for (j in seq_along(pc)) {
      p <- pc[[j]]
      fj <- sprintf("%s.postconditions[%d]", f, j)
      if (!.is_map(p)) {
        add(fj, "a postcondition must be a mapping",
            "write {id: \"P_rows\", condition: \".n_rows == 48\"}")
        next
      }
      for (k in setdiff(names(p), .POSTCONDITION_FIELDS)) {
        add(paste0(fj, ".", k), "is not a postcondition field",
            "a postcondition has id and condition")
      }
      if (!.is_string(p$id) || !grepl("^[A-Za-z][A-Za-z0-9_]*$", p$id)) {
        add(paste0(fj, ".id"), "the postcondition needs an id",
            "start with a letter; use letters, digits and underscores")
      } else if (p$id == "key") {
        add(paste0(fj, ".id"), "`key` is the id of the input's key check",
            "choose another id")
      } else if (p$id %in% ids_seen) {
        add(paste0(fj, ".id"), sprintf("id '%s' is used twice", p$id),
            "give each postcondition of an input its own id")
      } else {
        ids_seen <- c(ids_seen, p$id)
      }
      chk <- .cond_check(p$condition, "post")
      for (pr in chk$problems) {
        add(paste0(fj, ".condition"), pr,
            .cond_fix(pr, "see ?check_postconditions for the grammar"))
      }
    }
  }
  out
}

# The table an input names in `data`, or NULL when there is none.
.input_table <- function(data, name) {
  if (is.data.frame(data)) return(if (identical(name, "data")) data else NULL)
  if (is.list(data)) return(data[[name]])
  NULL
}

# One postcondition on one table: whether it holds and, when it does not,
# why, in counts only. A summary or row check never returns a data value.
.post_eval <- function(tab, condition) {
  fail <- function(detail) list(holds = FALSE, detail = detail)
  chk <- .cond_check(condition, "post")
  if (length(chk$problems)) return(fail(paste(chk$problems, collapse = "; ")))
  absent <- character(0)
  lookup <- function(name) {
    ref <- .post_ref(name)
    col <- if (is.null(ref)) name else ref$column
    if (!is.na(col) && !col %in% names(tab)) {
      absent <<- c(absent, col)
      return(NA)
    }
    if (is.null(ref)) return(tab[[col]])
    x <- if (is.na(col)) NULL else tab[[col]]
    ok <- if (is.null(x)) logical(0) else x[!is.na(x)]
    switch(ref$summary,
      n_rows = nrow(tab),
      n_cols = ncol(tab),
      n_missing = sum(is.na(x)),
      n_distinct = length(unique(ok)),
      n_duplicated = sum(duplicated(ok)),
      min = if (length(ok)) min(ok) else NA,
      max = if (length(ok)) max(ok) else NA,
      levels = sort(unique(as.character(ok))))
  }
  val <- tryCatch(.cond_value(chk$expr, lookup), error = function(e) e)
  if (length(absent)) {
    return(fail(sprintf("the table has no column %s",
                        paste0("'", unique(absent), "'", collapse = ", "))))
  }
  if (inherits(val, "error")) {
    return(fail(sprintf("cannot be evaluated on this table (%s)",
                        conditionMessage(val))))
  }
  if (!is.logical(val) || length(val) == 0L) {
    return(fail("does not give TRUE or FALSE"))
  }
  n_na <- sum(is.na(val))
  n_false <- sum(!val, na.rm = TRUE)
  if (n_na == 0L && n_false == 0L) {
    return(list(holds = TRUE, detail = NA_character_))
  }
  if (length(val) == 1L) {
    return(fail(if (n_na) "gives NA" else "is FALSE"))
  }
  fail(sprintf("%d of %d rows fail%s", n_false + n_na, length(val),
               if (n_na) sprintf(" (%d of them NA)", n_na) else ""))
}

# The key check: the combined key columns are present, never missing and
# unique in every row.
.key_eval <- function(tab, key) {
  fail <- function(detail) list(holds = FALSE, detail = detail)
  absent <- setdiff(key, names(tab))
  if (length(absent)) {
    return(fail(sprintf("the table has no column %s",
                        paste0("'", absent, "'", collapse = ", "))))
  }
  k <- tab[key]
  n_na <- sum(!stats::complete.cases(k))
  if (n_na) return(fail(sprintf("%d rows have a missing key", n_na)))
  n_dup <- sum(duplicated(k))
  if (n_dup) return(fail(sprintf("%d rows repeat a key", n_dup)))
  list(holds = TRUE, detail = NA_character_)
}

# Every postcondition of every input checked against `data`, one row each:
# input, id, condition, holds, detail.
.post_results <- function(inputs, data) {
  rows <- list()
  add <- function(input, id, condition, res) {
    rows[[length(rows) + 1L]] <<- data.frame(
      input = input, id = id, condition = condition, holds = res$holds,
      detail = res$detail, stringsAsFactors = FALSE)
  }
  for (inp in inputs) {
    tab <- .input_table(data, inp$name)
    missing_tab <- if (is.null(tab)) "the input is not in data"
                   else if (!is.data.frame(tab)) "the input is not a table"
                   else NA_character_
    checks <- inp$postconditions
    if (length(inp$key)) {
      checks <- c(list(list(id = "key", condition = sprintf(
        "unique key (%s)", paste(inp$key, collapse = ", ")))), checks)
    }
    for (p in checks) {
      res <- if (!is.na(missing_tab)) list(holds = FALSE, detail = missing_tab)
             else if (p$id == "key") .key_eval(tab, inp$key)
             else .post_eval(tab, p$condition)
      add(inp$name, p$id, p$condition, res)
    }
  }
  if (length(rows)) return(do.call(rbind, rows))
  data.frame(input = character(0), id = character(0),
             condition = character(0), holds = logical(0),
             detail = character(0), stringsAsFactors = FALSE)
}

# Stops with a `postcondition_failed` condition when a postcondition does not
# hold; the condition carries `failed`, the `<input>.<id>` of each.
.stop_if_failed <- function(res) {
  bad <- res[!res$holds, , drop = FALSE]
  if (nrow(bad) == 0L) return(invisible(res))
  lines <- sprintf("  - %s.%s `%s`: %s", bad$input, bad$id, bad$condition,
                   bad$detail)
  msg <- paste(c(sprintf("postcondition_failed: %d postcondition%s not met:",
                         nrow(bad), if (nrow(bad) == 1L) " is" else "s are"),
                 lines), collapse = "\n")
  stop(structure(
    class = c("postcondition_failed", "error", "condition"),
    list(message = msg, call = NULL, failed = paste(bad$input, bad$id,
                                                    sep = "."))))
}

#' Check a table against its postconditions
#'
#' Checks a table against conditions declared before it was made, and stops
#' when one does not hold. [conduct()] runs the same check on every input a
#' plan declares, before the first node; a script that writes a table by
#' hand can call it on its own output before writing it.
#'
#' @section The grammar:
#' A postcondition is a condition in the grammar described in [conduct()],
#' with two kinds of name. A bare column name stands for the column, so a
#' comparison is made row by row and the postcondition holds when it is
#' `TRUE` in every row: `window_days <= 31`. A name that starts with a dot
#' is a summary of the table: `.n_rows`, `.n_cols`, and, for a column `x`,
#' `.n_missing.x`, `.n_distinct.x` and `.n_duplicated.x` (counted over the
#' values that are not missing), `.min.x` and `.max.x` (missing values
#' left out) and `.levels.x` (its distinct values as text, so that
#' `.levels.x %in% c("a", "b")` holds when no other value occurs). A
#' postcondition that gives `NA` in any row, or names a column the table
#' does not have, does not hold.
#'
#' @param x A data frame.
#' @param conditions A character vector of postconditions, named by their
#'   ids, or an [oap_plan], whose declared postconditions and key for the
#'   input `input` are checked.
#' @param input The name of the input in the plan, or a label for `x` in the
#'   messages when `conditions` is a vector.
#' @param key Columns whose combined values must be unique and never
#'   missing, or `NULL`. Ignored when `conditions` is a plan, which carries
#'   its own.
#'
#' @returns Invisibly, a data frame with one row per postcondition and the
#'   columns `input`, `id`, `condition`, `holds` and `detail`, when every
#'   postcondition holds. Otherwise an error of class `postcondition_failed`
#'   that lists each failure with a count of the rows that fail; the error
#'   names no value of the table.
#'
#' @seealso [plan_input()], [conduct()].
#'
#' @author Max Moldovan
#'
#' @examples
#' tab <- data.frame(trial_id = c("t1", "t2", "t3"),
#'                   window_days = c(30, 31, 62))
#' ok <- tryCatch(
#'   check_postconditions(tab, c(P_rows = ".n_rows == 3",
#'                               P_days = "window_days <= 31"),
#'                        input = "moisture", key = "trial_id"),
#'   postcondition_failed = function(e) conditionMessage(e))
#' cat(ok)
#'
#' @export
check_postconditions <- function(x, conditions, input = "input", key = NULL) {
  stopifnot(is.data.frame(x), .is_string(input))
  if (S7::S7_inherits(conditions, oap_plan)) {
    decl <- Filter(function(i) identical(i$name, input), conditions@inputs)
    if (length(decl) == 0L) {
      stop(sprintf("plan '%s' declares no input '%s'", conditions@id, input),
           call. = FALSE)
    }
    inp <- decl[[1]]
  } else {
    stopifnot(is.character(conditions))
    inp <- .as_input(list(name = input, key = key,
                          postconditions = conditions))
    probs <- .inputs_rules(list(inp))
    if (length(probs)) {
      stop(paste(c("the postconditions are not valid:",
                   vapply(probs, function(p) {
                     sprintf("  - %s: %s",
                             sub("^inputs\\[1\\]\\.?", "", p$field), p$rule)
                   }, character(1))), collapse = "\n"), call. = FALSE)
    }
  }
  data <- stats::setNames(list(x), input)
  .stop_if_failed(.post_results(list(inp), data))
}

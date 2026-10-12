# conditions.R -- The condition grammar of `when` and of `select` rules.
#
# A condition is parsed by R's parser and then walked against a whitelist of
# literals, references and operators. It is never evaluated as R code: the
# interpreter below applies each whitelisted operator itself.

.COND_COMPARE <- c("==", "!=", "<", "<=", ">", ">=")
.COND_LOGIC   <- c("&", "|", "&&", "||")
.COND_FIELDS  <- c("headline", "abstained", "n_rows")
.COND_ALLOWED <- "comparisons (== != < <= > >=), %in% with c(...), & | ! and brackets"

# A reference in a `when` condition: <node>.headline, <node>.abstained,
# <node>.n_rows or <node>.metrics.<name>.
.cond_ref <- function(name) {
  m <- regmatches(name, regexec(
    "^([A-Za-z][A-Za-z0-9_]*)\\.(headline|abstained|n_rows|metrics\\.(.+))$",
    name))[[1]]
  if (length(m) == 0L) return(NULL)
  list(node = m[2], field = if (nzchar(m[4])) "metrics" else m[3],
       metric = if (nzchar(m[4])) m[4] else NA_character_)
}

#' Check a condition against the grammar
#'
#' @param text The condition as written in the plan.
#' @param kind `"when"` for a node condition, `"rule"` for a `select` rule.
#'
#' @returns A list with `problems` (character, empty when the condition is
#'   valid), `expr` (the parsed expression or `NULL`) and `refs` (the node
#'   ids a `when` condition reads).
#'
#' @noRd
#' @keywords internal
.cond_check <- function(text, kind = c("when", "rule")) {
  kind <- match.arg(kind)
  out <- list(problems = character(0), expr = NULL, refs = character(0))
  if (!is.character(text) || length(text) != 1L || is.na(text) ||
      !nzchar(trimws(text))) {
    out$problems <- "a condition must be a single non-empty string"
    return(out)
  }
  if (grepl("`", text, fixed = TRUE)) {
    out$problems <- "backticks are not allowed in a condition"
    return(out)
  }
  pd <- tryCatch(
    utils::getParseData(parse(text = text, keep.source = TRUE)),
    error = function(e) NULL)
  expr <- tryCatch(str2lang(text), error = function(e) NULL)
  if (is.null(expr) || is.null(pd)) {
    out$problems <- sprintf("`%s` is not a single valid expression", text)
    return(out)
  }
  num <- pd$text[pd$token == "NUM_CONST"]
  num <- num[grepl("^[0-9.]", num)]
  odd <- num[!grepl("^([0-9]+\\.?[0-9]*|\\.[0-9]+)$", num)]
  if (length(odd)) {
    out$problems <- sprintf("`%s` is not written as a plain number", odd)
  }
  env <- new.env(parent = emptyenv())
  env$problems <- out$problems
  env$refs <- character(0)
  .cond_walk(expr, kind, env)
  out$problems <- unique(env$problems)
  out$refs <- unique(env$refs)
  if (length(out$problems) == 0L) out$expr <- expr
  out
}

.cond_literal <- function(x) {
  (is.numeric(x) && length(x) == 1L && is.finite(x)) ||
    (is.character(x) && length(x) == 1L && !is.na(x)) ||
    (is.logical(x) && length(x) == 1L && !is.na(x))
}

.cond_walk <- function(x, kind, env) {
  bad <- function(msg) env$problems <- c(env$problems, msg)
  if (.cond_literal(x)) return(invisible())
  if (is.symbol(x)) {
    name <- as.character(x)
    if (name %in% c("T", "F")) {
      bad("write TRUE or FALSE in full")
    } else if (kind == "when") {
      ref <- .cond_ref(name)
      if (is.null(ref)) {
        bad(sprintf(paste0("`%s` is not a reference; use <node>.headline, ",
                           "<node>.abstained, <node>.n_rows or ",
                           "<node>.metrics.<name>"), name))
      } else {
        env$refs <- c(env$refs, ref$node)
      }
    }
    return(invisible())
  }
  if (!is.call(x)) {
    bad("only literals, references and operators may appear in a condition")
    return(invisible())
  }
  head <- x[[1]]
  if (!is.symbol(head)) {
    inner <- if (is.call(head) && is.symbol(head[[1]])) as.character(head[[1]]) else ""
    bad(if (inner %in% c("::", ":::", "$", "@")) {
      sprintf("`%s` is not allowed in a condition", inner)
    } else {
      "function calls are not allowed in a condition"
    })
    return(invisible())
  }
  f <- as.character(head)
  args <- as.list(x)[-1]
  if (f %in% c(.COND_COMPARE, .COND_LOGIC) && length(args) == 2L) {
    for (a in args) .cond_walk(a, kind, env)
  } else if (f %in% c("!", "(") && length(args) == 1L) {
    .cond_walk(args[[1]], kind, env)
  } else if (f == "-" && length(args) == 1L && is.numeric(args[[1]])) {
    invisible()
  } else if (f == "%in%" && length(args) == 2L) {
    .cond_walk(args[[1]], kind, env)
    rhs <- args[[2]]
    if (is.call(rhs) && identical(rhs[[1]], as.symbol("c"))) {
      if (!all(vapply(as.list(rhs)[-1], .cond_literal, logical(1)))) {
        bad("the right side of %in% must be c() of literals")
      }
    } else if (!.cond_literal(rhs)) {
      bad("the right side of %in% must be c() of literals")
    }
  } else if (f %in% c("<-", "<<-", "=", "->", "->>")) {
    bad("assignment is not allowed in a condition")
  } else {
    bad(sprintf("`%s` is not allowed in a condition", f))
  }
  invisible()
}

# Applies the whitelisted operators to a checked expression. `lookup`
# returns the value of a reference (a `when`) or a column (a `select` rule).
.cond_value <- function(x, lookup) {
  if (is.symbol(x)) return(lookup(as.character(x)))
  if (!is.call(x)) return(x)
  f <- as.character(x[[1]])
  a <- as.list(x)[-1]
  v <- function(i) .cond_value(a[[i]], lookup)
  switch(f,
    "("  = v(1L),
    "!"  = !v(1L),
    "-"  = -a[[1]],
    "c"  = unlist(lapply(seq_along(a), v)),
    "%in%" = v(1L) %in% v(2L),
    "&"  = , "&&" = v(1L) & v(2L),
    "|"  = , "||" = v(1L) | v(2L),
    "==" = v(1L) == v(2L),
    "!=" = v(1L) != v(2L),
    "<"  = v(1L) < v(2L),
    "<=" = v(1L) <= v(2L),
    ">"  = v(1L) > v(2L),
    ">=" = v(1L) >= v(2L),
    stop(sprintf("`%s` is not allowed in a condition", f), call. = FALSE))
}

# The value of a node's `when` condition, read from the manifests already
# on the bus.
.when_value <- function(node, bus) {
  chk <- .cond_check(node@when, "when")
  lookup <- function(name) {
    ref <- .cond_ref(name)
    m <- bus[[ref$node]]
    if (is.null(m)) {
      stop(sprintf("node '%s': its when condition reads '%s', which has not run",
                   node@id, ref$node), call. = FALSE)
    }
    s <- m@summary
    val <- switch(ref$field,
      headline  = s$headline %||% NA_character_,
      abstained = isTRUE(s$abstained),
      n_rows    = nrow(m@params),
      metrics   = s$metrics[[ref$metric]])
    if (is.null(val) || !is.atomic(val) || length(val) != 1L) {
      stop(sprintf(paste0("node '%s': its when condition reads `%s`, but ",
                          "'%s' has no single value there"),
                   node@id, name, ref$node), call. = FALSE)
    }
    val
  }
  val <- .cond_value(chk$expr, lookup)
  if (!is.logical(val) || length(val) != 1L || is.na(val)) {
    stop(sprintf(paste0("node '%s': its when condition `%s` must give TRUE ",
                        "or FALSE, and gave %s"),
                 node@id, node@when, paste(format(val), collapse = ", ")),
         call. = FALSE)
  }
  val
}

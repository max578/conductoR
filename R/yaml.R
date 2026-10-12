# yaml.R -- A plan as a portable file.

.node_to_list <- function(n) {
  if (n@operator == "compute" && !nzchar(n@tool_ref)) {
    stop(sprintf(paste0("compute node '%s' carries an inline closure and no ",
                        "tool_ref; register the tool and reference it by ",
                        "name so the plan is portable"), n@id), call. = FALSE)
  }
  out <- list(id = n@id, role = n@role, operator = n@operator,
              emits = n@emits,
              consumes = lapply(n@consumes, function(e) {
                list(from = e$from, contract = e$contract)
              }),
              params = n@params)
  if (nzchar(n@tool_ref)) out$tool <- n@tool_ref
  c(out, .node_v02_fields(n))
}

# The v0.2 fields a node sets to something other than their defaults.
.node_v02_fields <- function(n) {
  out <- list()
  if (nzchar(n@when)) out$when <- n@when
  if (!is.null(n@foreach)) out$foreach <- n@foreach
  if (!identical(n@foreach_max, 50L)) out$foreach_max <- n@foreach_max
  if (!identical(n@on_refusal, "continue")) out$on_refusal <- n@on_refusal
  out
}

.uses_v02 <- function(plan) {
  any(vapply(plan@nodes, function(n) {
    length(.node_v02_fields(n)) > 0L || n@operator == "select"
  }, logical(1)))
}

.node_from_list <- function(x) {
  cons <- lapply(x$consumes %||% list(),
                 function(e) edge(e$from, e$contract %||% "any"))
  node(id = x$id, tool = x$tool %||% (function(...) NULL),
       consumes = cons, emits = x$emits %||% "any",
       operator = x$operator %||% "compute", role = x$role %||% "compute",
       params = .as_plain_list(x$params %||% list()),
       when = x$when %||% "", foreach = x$foreach,
       foreach_max = x$foreach_max %||% 50L,
       on_refusal = x$on_refusal %||% "continue")
}

# Every string is double-quoted and every number written plainly, so a plan
# reads back the same under any YAML 1.1 or 1.2 reader.
.yaml_scalar <- function(x) {
  if (is.null(x)) return("null")
  if (is.factor(x)) x <- as.character(x)
  if (is.na(x)) return("null")
  if (is.logical(x)) return(if (x) "true" else "false")
  if (is.integer(x)) return(as.character(x))
  if (is.double(x)) {
    if (is.infinite(x)) return(if (x > 0) ".inf" else "-.inf")
    s <- format(x, scientific = FALSE, digits = 15, trim = TRUE)
    if (!grepl(".", s, fixed = TRUE)) s <- paste0(s, ".0")
    return(s)
  }
  if (is.character(x)) {
    return(as.character(jsonlite::toJSON(x, auto_unbox = TRUE)))
  }
  stop(sprintf("a plan cannot hold a value of class '%s' in YAML",
               class(x)[1]), call. = FALSE)
}

.yaml_key <- function(k) {
  plain <- grepl("^[A-Za-z_][A-Za-z0-9_]*$", k) &&
    !tolower(k) %in% c("y", "n", "yes", "no", "on", "off", "true", "false",
                       "null")
  if (plain) k else .yaml_scalar(k)
}

.yaml_is_map <- function(x) is.list(x) && !is.null(names(x))

.yaml_is_scalar <- function(v) is.null(v) || (!is.list(v) && length(v) == 1L)

.yaml_empty <- function(v) if (.yaml_is_map(v)) "{}" else "[]"

.yaml_lines <- function(x, indent) {
  pad <- strrep(" ", indent)
  out <- character(0)
  if (.yaml_is_map(x)) {
    for (k in names(x)) {
      v <- x[[k]]
      key <- paste0(pad, .yaml_key(k), ":")
      if (.yaml_is_scalar(v)) {
        out <- c(out, paste(key, .yaml_scalar(v)))
      } else if (length(v) == 0L) {
        out <- c(out, paste(key, .yaml_empty(v)))
      } else if (.yaml_is_map(v)) {
        out <- c(out, key, .yaml_lines(v, indent + 2L))
      } else {
        out <- c(out, key, .yaml_lines(as.list(v), indent))
      }
    }
    return(out)
  }
  for (v in as.list(x)) {
    if (.yaml_is_scalar(v)) {
      out <- c(out, paste0(pad, "- ", .yaml_scalar(v)))
    } else if (length(v) == 0L) {
      out <- c(out, paste0(pad, "- ", .yaml_empty(v)))
    } else {
      sub <- .yaml_lines(if (.yaml_is_map(v)) v else as.list(v), indent + 2L)
      sub[1] <- paste0(pad, "- ", substring(sub[1], indent + 3L))
      out <- c(out, sub)
    }
  }
  out
}

#' Write a plan as YAML
#'
#' Every compute node must reference its tool by name (see [node()]), since
#' a closure cannot be written to a file. Operator nodes carry no tool.
#'
#' Every string is written in double quotes and every number in plain
#' decimal form, so the file reads back to the same plan. The line
#' `plan_version: "0.2"` is written only when the plan uses a field added in
#' version 0.2: `when`, `foreach`, `foreach_max`, `on_refusal` or a `select`
#' node.
#'
#' @param plan An [oap_plan].
#' @param path A file to write, or `NULL` to return the YAML text.
#'
#' @returns `path` invisibly when a file was written, otherwise the YAML
#'   text as a single string.
#'
#' @seealso [plan_from_yaml()], [register_tool()].
#'
#' @author Max Moldovan
#'
#' @examples
#' p <- oap_plan(id = "one_lens", nodes = list(
#'   node("lens", tool = "my_lens", emits = "treatment_effects"),
#'   node("gate", operator = "abstain_gate", emits = "decisions",
#'        consumes = list(edge("lens", "treatment_effects")),
#'        params = list(n_min = 25L))
#' ))
#' cat(plan_to_yaml(p))
#'
#' @export
plan_to_yaml <- function(plan, path = NULL) {
  stopifnot(S7::S7_inherits(plan, oap_plan))
  spec <- list(id = plan@id)
  if (.uses_v02(plan)) spec$plan_version <- "0.2"
  spec$meta <- plan@meta
  spec$nodes <- lapply(plan@nodes, .node_to_list)
  txt <- paste0(paste(.yaml_lines(spec, 0L), collapse = "\n"), "\n")
  if (is.null(path)) return(txt)
  writeLines(txt, path, sep = "")
  invisible(path)
}

#' Read a plan from YAML
#'
#' Tool references are kept as strings and resolved when the plan is
#' performed, so a plan loads without its tools present.
#'
#' The plan is checked by [plan_validate()] before it is built, and a plan
#' with problems is an error that lists all of them. Unquoted `yes`, `no`,
#' `on` and `off` are refused rather than read as logicals, and numbers in
#' scientific notation are refused with their plain form.
#'
#' @param path A file path, or a string of YAML text.
#'
#' @returns An [oap_plan].
#'
#' @seealso [plan_to_yaml()], [plan_validate()], [plan_from_json()].
#'
#' @author Max Moldovan
#'
#' @examples
#' f <- system.file("scores", "dual_lens_gate.oap.yaml", package = "conductoR")
#' p <- plan_from_yaml(f)
#' plan_order(p)
#'
#' @export
plan_from_yaml <- function(path) {
  stopifnot(is.character(path), length(path) == 1L)
  .spec_to_plan(.read_yaml_spec(path))
}

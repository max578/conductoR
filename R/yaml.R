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
  out
}

.node_from_list <- function(x) {
  cons <- lapply(x$consumes %||% list(),
                 function(e) edge(e$from, e$contract %||% "any"))
  node(id = x$id, tool = x$tool %||% (function(...) NULL),
       consumes = cons, emits = x$emits %||% "any",
       operator = x$operator %||% "compute", role = x$role %||% "compute",
       params = x$params %||% list())
}

#' Write a plan as YAML
#'
#' Every compute node must reference its tool by name (see [node()]), since
#' a closure cannot be written to a file. Operator nodes carry no tool.
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
  spec <- list(id = plan@id, meta = plan@meta,
               nodes = lapply(plan@nodes, .node_to_list))
  txt <- yaml::as.yaml(spec)
  if (is.null(path)) return(txt)
  writeLines(txt, path)
  invisible(path)
}

#' Read a plan from YAML
#'
#' Tool references are kept as strings and resolved when the plan is
#' performed, so a plan loads without its tools present.
#'
#' @param path A file path, or a string of YAML text.
#'
#' @returns An [oap_plan].
#'
#' @seealso [plan_to_yaml()].
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
  spec <- if (file.exists(path)) yaml::yaml.load_file(path) else yaml::yaml.load(path)
  oap_plan(id = spec$id, meta = spec$meta %||% list(),
           nodes = lapply(spec$nodes, .node_from_list))
}

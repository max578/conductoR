# targets.R -- Compile a plan to a targets pipeline.

.target_sym <- function(id) paste0("n_", id)

#' The target that holds a plan's terminal result
#'
#' @param plan An [oap_plan].
#'
#' @returns The name of the target, as a string, that
#'   [compile_to_targets()] maps the last node in topological order to.
#'
#' @seealso [compile_to_targets()].
#'
#' @author Max Moldovan
#'
#' @examples
#' p <- oap_plan(id = "chain", nodes = list(
#'   node("b", consumes = list(edge("a"))),
#'   node("a")
#' ))
#' terminal_target(p)
#'
#' @export
terminal_target <- function(plan) {
  ord <- plan_order(plan)
  .target_sym(ord[length(ord)])
}

#' Compile a plan to a targets script
#'
#' Writes a `_targets.R` in which each node is one `targets::tar_target()`
#' calling [conduct_step()], so the plan inherits caching and skipping of
#' up-to-date nodes from 'targets' while every edge stays type-checked. Target
#' names are the node ids prefixed with `n_`.
#'
#' A plan whose nodes use `when`, `foreach` or `on_refusal` is refused with
#' an error of class `conductoR_unsupported`, since those are applied by
#' [conduct()]. A `select` node compiles like any other node.
#'
#' @param plan An [oap_plan].
#' @param script_file The path to write.
#' @param preamble Lines of R placed at the top of the script. They must bind
#'   `.score` to the plan and `.data` to the data object, and make any
#'   registered tools available in the process that runs the pipeline.
#'
#' @returns `script_file`, invisibly.
#'
#' @seealso [terminal_target()], [conduct_step()].
#'
#' @author Max Moldovan
#'
#' @examples
#' p <- oap_plan(id = "chain", nodes = list(
#'   node("a", tool = "stats::median"),
#'   node("b", tool = "stats::median", consumes = list(edge("a")))
#' ))
#' f <- tempfile(fileext = ".R")
#' compile_to_targets(p, f, preamble = c("library(conductoR)",
#'                                       ".score <- readRDS('plan.rds')",
#'                                       ".data <- list()"))
#' cat(readLines(f), sep = "\n")
#'
#' @export
compile_to_targets <- function(plan, script_file, preamble = character(0)) {
  stopifnot(S7::S7_inherits(plan, oap_plan))
  for (n in plan@nodes) {
    used <- setdiff(names(.node_v02_fields(n)), "foreach_max")
    if (length(used)) {
      cnd <- structure(class = c("conductoR_unsupported", "error", "condition"),
        list(message = sprintf(paste0(
          "compile_to_targets() does not support %s (node '%s'); perform ",
          "this plan with conduct()"),
          paste0("`", used, "`", collapse = ", "), n@id), call = NULL))
      stop(cnd)
    }
  }
  tlines <- vapply(plan@nodes, function(n) {
    deps <- .edge_sources(n)
    inl <- if (length(deps)) {
      paste0("list(", paste(sprintf("%s = %s", deps, .target_sym(deps)),
                            collapse = ", "), ")")
    } else {
      "list()"
    }
    sprintf(paste0("  targets::tar_target(%s, conductoR::conduct_step(",
                   "conductoR::plan_node(.score, \"%s\"), %s, .data))"),
            .target_sym(n@id), n@id, inl)
  }, character(1))
  code <- c(preamble, "", "list(", paste(tlines, collapse = ",\n"), ")")
  writeLines(code, script_file)
  invisible(script_file)
}

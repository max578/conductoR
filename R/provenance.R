# provenance.R -- The record a run leaves behind.

#' A run's provenance as a table
#'
#' @param run The list returned by [conduct()].
#'
#' @returns A data frame with one row per node in execution order and columns
#'   `node`, `operator`, `role`, `consumes` (upstream ids joined with `+`),
#'   `emits`, `emitter`, `run_id`, `data_hash`, `abstained` and `seconds`.
#'
#' @section Files:
#' When [conduct()] is given `out_dir`, it writes three files there:
#' `run.rds` (the whole run object), `run_card.md` (this table and the
#' terminal verdict) and `provenance.md` (a front-matter record with the chain
#' of node ids and payload hashes in execution order).
#'
#' @seealso [conduct()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- node("lens", emits = "treatment_effects",
#'              tool = function(inputs, data, node) {
#'                lens_to_manifest("effect", TRUE, "example", "example:lens")
#'              })
#' run <- conduct(oap_plan(id = "one", nodes = list(lens)), verbose = FALSE)
#' run_provenance(run)[, c("node", "emits", "abstained")]
#'
#' @export
run_provenance <- function(run) {
  stopifnot(is.list(run), is.list(run$provenance))
  rows <- lapply(run$provenance, function(p) {
    data.frame(
      node = p$node, operator = p$operator, role = p$role,
      consumes = if (length(p$consumes)) paste(p$consumes, collapse = "+") else "",
      emits = p$emits, emitter = p$emitter, run_id = p$run_id,
      data_hash = p$data_hash, abstained = as.logical(p$abstained),
      seconds = as.numeric(p$seconds), stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

.write_provenance <- function(run, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(run, file.path(out_dir, "run.rds"))
  term <- run$terminal
  verdict <- term@summary$headline %||% term@inferential_target
  lines <- c(
    sprintf("# conductoR run: plan `%s`", run$plan_id),
    sprintf("Nodes: %d   Wall: %.2fs", run$n_nodes, run$seconds), "",
    "| node | operator | role | consumes | emits | run_id | abstained | s |",
    "|---|---|---|---|---|---|---|---|")
  for (p in run$provenance) {
    lines <- c(lines, sprintf("| %s | %s | %s | %s | %s | `%s` | %s | %.3f |",
      p$node, p$operator, p$role,
      if (length(p$consumes)) paste(p$consumes, collapse = "+") else "",
      p$emits, p$run_id, p$abstained, p$seconds))
  }
  lines <- c(lines, "",
             sprintf("**Terminal verdict** (`%s`): %s", term@run_id, verdict))
  writeLines(lines, file.path(out_dir, "run_card.md"))
  .write_provenance_record(run, out_dir)
  invisible(out_dir)
}

# The hash chain is the integrity spine of the run: one node id and the first
# 18 characters of its payload hash per line, in execution order.
.write_provenance_record <- function(run, out_dir) {
  term <- run$terminal
  verdict <- term@summary$headline %||% term@inferential_target
  chain <- vapply(run$provenance, function(p) {
    sprintf("%s:%s", p$node, substr(p$data_hash, 1L, 18L))
  }, character(1))
  n_lenses <- term@summary$metrics$n_lenses
  fm <- c(
    "---",
    "kind: run-provenance",
    sprintf("plan: %s", run$plan_id),
    sprintf("nodes: %d", run$n_nodes),
    sprintf("skipped: %d", run$n_skipped %||% 0L),
    sprintf("terminal_run_id: %s", term@run_id),
    sprintf("terminal_verdict: \"%s\"", verdict),
    sprintf("abstained: %s", isTRUE(term@summary$abstained)),
    "---", "")
  body <- c(
    sprintf("## Run provenance: plan `%s`", run$plan_id),
    "",
    sprintf("Performed %d nodes (%d skipped) in %.2fs. Terminal verdict: **%s**%s.",
            run$n_nodes, run$n_skipped %||% 0L, run$seconds, verdict,
            if (!is.null(n_lenses)) sprintf(" (%d lenses)", n_lenses) else ""),
    "",
    "### Integrity chain (node:data_hash, execution order)",
    "", "```", chain, "```", "",
    "### Run card",
    "",
    "See `run_card.md` in this directory for the per-node table.")
  writeLines(c(fm, body), file.path(out_dir, "provenance.md"))
  invisible(out_dir)
}

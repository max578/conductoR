# provenance.R -- The record a run leaves behind.

#' A run's provenance as a table
#'
#' @param run The list returned by [conduct()].
#'
#' @returns A data frame with one row per node in execution order and columns
#'   `node`, `operator`, `role`, `consumes` (upstream ids joined with `+`),
#'   `emits`, `emitter`, `run_id`, `data_hash`, `abstained`, `seconds`,
#'   `status` (`ran`, `refused` or `skipped`) and `reason` (why a node was
#'   skipped, otherwise empty).
#'
#' @section Files:
#' When [conduct()] is given `out_dir`, it writes three files there:
#' `run.rds` (the whole run object), `run_card.md` (this table and the
#' terminal verdict) and `provenance.md` (a front-matter record with the chain
#' of node ids and payload hashes in execution order). When the run skipped
#' nodes, evaluated `when` conditions or ran `foreach` items, both Markdown
#' files list them with their reasons, values and statuses.
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
      seconds = as.numeric(p$seconds), status = p$status %||% "ran",
      reason = p$reason %||% "", stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

.term_verdict <- function(term) {
  if (is.null(term)) return("(no node ran)")
  term@summary$headline %||% term@inferential_target
}

.term_id <- function(term) if (is.null(term)) NA_character_ else term@run_id

# Skipped nodes, `when` outcomes and `foreach` items, when the run has any.
.v02_sections <- function(run) {
  out <- character(0)
  skipped <- Filter(function(p) identical(p$status, "skipped"), run$provenance)
  if (length(skipped)) {
    out <- c(out, "", "### Skipped nodes", "", "| node | reason |", "|---|---|",
             vapply(skipped, function(p) sprintf("| %s | %s |", p$node, p$reason),
                    character(1)))
  }
  cd <- run$conditions
  if (!is.null(cd) && nrow(cd)) {
    out <- c(out, "", "### Conditions", "", "| node | when | value |",
             "|---|---|---|",
             sprintf("| %s | `%s` | %s |", cd$node, cd$condition, cd$value))
  }
  it <- run$items
  if (!is.null(it) && nrow(it)) {
    out <- c(out, "", "### Items", "", "| node | item | status | run_id |",
             "|---|---|---|---|",
             sprintf("| %s | %s | %s | `%s` |", it$node, it$item, it$status,
                     it$run_id))
  }
  out
}

.write_provenance <- function(run, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  saveRDS(run, file.path(out_dir, "run.rds"))
  term <- run$terminal
  verdict <- .term_verdict(term)
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
             sprintf("**Terminal verdict** (`%s`): %s", .term_id(term), verdict),
             .v02_sections(run))
  writeLines(lines, file.path(out_dir, "run_card.md"))
  .write_provenance_record(run, out_dir)
  invisible(out_dir)
}

# The hash chain is the integrity spine of the run: one node id and the first
# 18 characters of its payload hash per line, in execution order.
.write_provenance_record <- function(run, out_dir) {
  term <- run$terminal
  verdict <- .term_verdict(term)
  chain <- vapply(run$provenance, function(p) {
    sprintf("%s:%s", p$node, substr(p$data_hash, 1L, 18L))
  }, character(1))
  n_lenses <- if (is.null(term)) NULL else term@summary$metrics$n_lenses
  fm <- c(
    "---",
    "kind: run-provenance",
    sprintf("plan: %s", run$plan_id),
    sprintf("plan_hash: %s", run$plan_hash %||% NA_character_),
    sprintf("input_hash: %s", run$input_hash %||% NA_character_),
    sprintf("label: %s", run$label %||% NA_character_),
    sprintf("nodes: %d", run$n_nodes),
    sprintf("skipped: %d", run$n_skipped %||% 0L),
    sprintf("terminal_run_id: %s", .term_id(term)),
    sprintf("terminal_verdict: \"%s\"", verdict),
    sprintf("abstained: %s", !is.null(term) && isTRUE(term@summary$abstained)),
    if (!is.na(run$stopped_by %||% NA)) sprintf("stopped_by: %s", run$stopped_by),
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
    "See `run_card.md` in this directory for the per-node table.",
    .v02_sections(run))
  writeLines(c(fm, body), file.path(out_dir, "provenance.md"))
  invisible(out_dir)
}

#' Check that two runs used the same plan
#'
#' Compares the plan hashes and input hashes that [conduct()] records. The
#' plan hash covers the plan and the code of every tool it references, so
#' two runs with equal plan hashes ran the same analysis. Use it after a
#' blinded analysis: the blinded run and the unblinded run should share a
#' plan hash and differ in input hash.
#'
#' @param run_a,run_b Two runs returned by [conduct()].
#'
#' @returns A list with `same_plan` and `same_input` (logical), and the four
#'   hashes and two labels compared.
#'
#' @seealso [conduct()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- function(inputs, data, node) {
#'   lens_to_manifest(if (mean(data$d) > 0) "effect" else "no_effect",
#'                    mean(data$d) > 0, "demo", "demo:mean")
#' }
#' p <- oap_plan(id = "blind_demo", nodes = list(
#'   node("lens", lens, emits = "treatment_effects")))
#' d <- c(0.3, 0.1, 0.2, 0.4)
#' flip <- c(1, -1, -1, 1)
#' blinded <- conduct(p, data = list(d = d * flip), verbose = FALSE,
#'                    label = "blinded")
#' unblinded <- conduct(p, data = list(d = d), verbose = FALSE,
#'                      label = "unblinded")
#' same_plan(blinded, unblinded)$same_plan
#'
#' @export
same_plan <- function(run_a, run_b) {
  stopifnot(is.list(run_a), is.list(run_b))
  list(same_plan  = identical(run_a$plan_hash, run_b$plan_hash),
       same_input = identical(run_a$input_hash, run_b$input_hash),
       plan_hash  = c(run_a$plan_hash %||% NA_character_,
                      run_b$plan_hash %||% NA_character_),
       input_hash = c(run_a$input_hash %||% NA_character_,
                      run_b$input_hash %||% NA_character_),
       label      = c(run_a$label %||% NA_character_,
                      run_b$label %||% NA_character_))
}

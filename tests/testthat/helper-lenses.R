# helper-lenses.R -- Tools shared across the test files.
#
# Each returns a constant verdict so a plan's behaviour can be pinned down
# without any estimator running.

lens_tool <- function(headline = "effect", effect = TRUE, emitter = "lens",
                      metrics = list(n = 100L), run_id = NULL) {
  force(headline); force(effect); force(emitter); force(metrics); force(run_id)
  function(inputs, data, node) {
    lens_to_manifest(headline, effect, emitter, paste0(emitter, ":constant"),
                     metrics = metrics, run_id = run_id)
  }
}

effect_lens    <- lens_tool("effect", TRUE, "yes")
no_effect_lens <- lens_tool("no_effect", FALSE, "no")

# A gate node consuming `from` with the given params.
gate_node <- function(from = "lens", id = "gate", params = list()) {
  node(id, operator = "abstain_gate", emits = "decisions",
       consumes = list(edge(from, "treatment_effects")), params = params)
}

# A triangulate node consuming every id in `lenses` and, when given, `gate`.
consensus_node <- function(lenses, gate = NULL, id = "consensus") {
  cons <- lapply(lenses, edge, contract = "treatment_effects")
  if (!is.null(gate)) cons <- c(cons, list(edge(gate, "decisions")))
  node(id, operator = "triangulate", emits = "decisions", consumes = cons)
}

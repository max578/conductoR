# helper-return.R -- Tools for the return-path tests: planted results whose
# metrics, issues and unit counts are known in advance.

# A tool emitting a parameters manifest with the given metrics, params table,
# abstention and outputs.
planted_tool <- function(metrics = list(), tab = data.frame(),
                         target = "parameters", abstained = FALSE,
                         abstain_reason = NA_character_, outputs = NULL) {
  force(metrics); force(tab); force(target); force(abstained)
  force(abstain_reason); force(outputs)
  function(inputs, data, node) {
    summ <- orchestraManifest::manifest_summary(
      "planted_headline", abstained = abstained,
      abstain_reason = abstain_reason,
      metrics = metrics)
    orchestraManifest::orchestra_manifest(
      emitter_package = "planted", emitter_version = "0.0.0",
      inferential_target = target, run_id = paste0("planted-", node@id),
      method = "test:planted", params = tab, outputs = outputs,
      summary = summ, timestamp = Sys.time(),
      data_hash = orchestraManifest::manifest_data_hash(
        tab, outputs, NULL, NULL, NA_integer_, summ))
  }
}

failing_tool <- function(msg, class = NULL) {
  force(msg); force(class)
  function(inputs, data, node) {
    if (is.null(class)) stop(msg, call. = FALSE)
    stop(structure(class = c(class, "error", "condition"),
                   list(message = msg, call = NULL)))
  }
}

study_tab <- data.frame(
  unit = c("S1", "S2", "S3"),
  estimate = c(1.25, 0.43, 2.71),
  n_units = c(8L, 9L, 6L),
  max_unit_share = c(0.2, 0.3, 0.4),
  stringsAsFactors = FALSE)

# Fit -> screen -> quorum -> pooled, with an issue at quorum, the shape of
# the W2 round-1 record.
planted_plan <- function(quorum_issue = "assumption_check_failed") {
  q_metrics <- list(width_ratio = 1.6, agreement_pass = FALSE)
  if (!is.null(quorum_issue)) q_metrics$issue <- quorum_issue
  oap_plan(id = "planted", nodes = list(
    node("fit", planted_tool(
      list(slope = structure(0.318, n_units = 25L), dep_share = 0.42,
           n_fidelity_fail = 0L, converged = TRUE),
      tab = study_tab), emits = "parameters"),
    node("screen", planted_tool(list(het_p = 0.004), target = "structure"),
         emits = "structure", consumes = list(edge("fit", "parameters"))),
    node("quorum", planted_tool(q_metrics, target = "consensus"),
         emits = "consensus", consumes = list(edge("fit", "parameters"))),
    node("pooled", planted_tool(list(rise = 0.71), target = "decisions"),
         emits = "decisions",
         consumes = list(edge("quorum", "consensus")))))
}

planted_triggers <- data.frame(
  trigger_id = c("T_fidelity", "T_het", "T_spread", "T_dep"),
  node = c("fit", "screen", "quorum", "fit"),
  class = c("anomaly", "candidate", "anomaly", "anomaly"),
  signal_type = c("clone_fidelity", "heterogeneity", "signal:lens_spread",
                  "clone_fidelity"),
  statistic = c("n_fidelity_fail", "het_p", "width_ratio", "dep_share"),
  direction = c(">=", "<", ">", ">"),
  threshold = c("1", "0.01", "1.3", "0.4"),
  action = c("return", "release", "release", "return"),
  rehearsal_plant = "planted in rehearsal",
  stringsAsFactors = FALSE)

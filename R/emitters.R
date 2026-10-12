# emitters.R -- Build the manifests this package emits.
#
# Two exported emitters lift a component's verdict into the contract so any
# component can take part in a plan; two internal ones carry the operator
# outputs. Every manifest here has an empty `params` table and its content in
# the typed `summary` slot, which is inside the integrity hash.

.verdict_manifest <- function(summ, emitter, emitter_version, target, method,
                              run_id, consumed = list(), metadata = list()) {
  params <- data.frame()
  dh <- orchestraManifest::manifest_data_hash(params, NULL, NULL, NULL,
                                              NA_integer_, summ)
  metadata$derived <- length(consumed) > 0L
  orchestraManifest::orchestra_manifest(
    emitter_package = emitter, emitter_version = emitter_version,
    inferential_target = target, run_id = run_id, method = method,
    seed = NA_integer_, params = params, summary = summ,
    consumed_manifests = lapply(consumed, orchestraManifest::manifest_lineage),
    metadata = metadata, timestamp = Sys.time(), data_hash = dh)
}

#' Lift a binary verdict into a treatment-effects manifest
#'
#' Any method that answers "is there an effect?" on a shared question can take
#' part in a plan through this emitter: a kernel test, a classical regression
#' slope test, a rank correlation. The `effect_present` flag is what
#' triangulation reads; `n`, `effect_mean` and `noise_sd`, when supplied in
#' `metrics`, are what the abstention gate reads.
#'
#' @param headline The verdict label, such as `"effect"` or `"no_effect"`.
#' @param effect_present Whether the method found an effect.
#' @param emitter The name of the component or method family emitting the
#'   verdict.
#' @param method A single string naming the method, conventionally
#'   `"component:method"`.
#' @param metrics A named list of scalars behind the verdict.
#' @param emitter_version The emitting component's version string.
#' @param run_id A run identifier, or `NULL` to derive one from the verdict.
#'
#' @returns An [orchestraManifest::orchestra_manifest] with
#'   `inferential_target = "treatment_effects"`.
#'
#' @seealso [decision_to_manifest()], [conduct()].
#'
#' @author Max Moldovan
#'
#' @examples
#' m <- lens_to_manifest("effect", TRUE, "ols", "ols:slope_t",
#'                       metrics = list(p_value = 0.01, n = 136L))
#' m@summary$headline
#' orchestraManifest::verify_manifest(m)$ok
#'
#' @export
lens_to_manifest <- function(headline, effect_present, emitter, method,
                             metrics = list(), emitter_version = "0.0.0",
                             run_id = NULL) {
  summ <- orchestraManifest::manifest_summary(
    headline = headline, abstained = FALSE,
    metrics = c(list(effect_present = isTRUE(effect_present)), metrics))
  rid <- run_id %||% paste0(substr(emitter, 1L, 6L), "-",
                            .summary_digest(summ))
  .verdict_manifest(summ, emitter, emitter_version, "treatment_effects",
                    method, rid, metadata = list(source = emitter))
}

#' Lift a decision into a decisions manifest
#'
#' A decision node turns a consensus and its supporting metrics into an action
#' recommendation. The headline `"insufficient_evidence"` is recorded as an
#' abstention.
#'
#' @inheritParams lens_to_manifest
#' @param headline The recommended action, or `"insufficient_evidence"`.
#' @param consumed The upstream manifests the decision was derived from. Their
#'   lineage is recorded in the result.
#'
#' @returns An [orchestraManifest::orchestra_manifest] with
#'   `inferential_target = "decisions"`.
#'
#' @seealso [lens_to_manifest()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- lens_to_manifest("effect", TRUE, "ols", "ols:slope_t")
#' d <- decision_to_manifest("apply_nitrogen", "agronomy", "agronomy:rule",
#'                           metrics = list(rate_kg_ha = 90),
#'                           consumed = list(lens))
#' d@summary$headline
#' length(d@consumed_manifests)
#'
#' @export
decision_to_manifest <- function(headline, emitter, method, metrics = list(),
                                 emitter_version = "0.0.0", run_id = NULL,
                                 consumed = list()) {
  summ <- orchestraManifest::manifest_summary(
    headline = headline,
    abstained = identical(headline, "insufficient_evidence"),
    metrics = metrics)
  rid <- run_id %||% paste0(substr(emitter, 1L, 6L), "-",
                            .summary_digest(summ))
  .verdict_manifest(summ, emitter, emitter_version, "decisions", method, rid,
                    consumed = consumed, metadata = list(source = emitter))
}

gate_to_manifest <- function(gate_decision, reason, power_z, source_decision,
                             mode = "flag", gated_run_id = NA_character_,
                             consumed = list()) {
  abstained <- identical(gate_decision, "abstain")
  summ <- orchestraManifest::manifest_summary(
    headline = gate_decision, abstained = abstained,
    abstain_reason = if (abstained) reason else NA_character_,
    metrics = list(power_z = power_z, source_decision = source_decision,
                   mode = mode, reroute = abstained && identical(mode, "reroute"),
                   gated_run_id = gated_run_id))
  .verdict_manifest(summ, "conductoR", .conductor_version(), "decisions",
                    "abstain_gate:power_aware",
                    paste0("gate-", .summary_digest(summ)),
                    consumed = consumed,
                    metadata = list(source = "conductoR:abstain_gate",
                                    reason = reason))
}

triangulate_to_manifest <- function(consensus, agree, lenses, n_lenses, gate,
                                    rerouted, consumed = list()) {
  summ <- orchestraManifest::manifest_summary(
    headline = consensus, abstained = identical(consensus, "abstain"),
    metrics = list(agree = agree, n_lenses = n_lenses, gate = gate,
                   rerouted = rerouted, lenses = lenses))
  .verdict_manifest(summ, "conductoR", .conductor_version(), "decisions",
                    "triangulate:multi-lens",
                    paste0("consensus-", .summary_digest(summ)),
                    consumed = consumed,
                    metadata = list(source = "conductoR:triangulate"))
}

select_to_manifest <- function(kept, upstream, rule, n_in, n_na) {
  summ <- orchestraManifest::manifest_summary(
    headline = "selected", abstained = FALSE,
    metrics = list(rule = rule, n_in = as.integer(n_in),
                   n_kept = nrow(kept), n_na = as.integer(n_na)))
  dh <- orchestraManifest::manifest_data_hash(kept, NULL, NULL, NULL,
                                              NA_integer_, summ)
  orchestraManifest::orchestra_manifest(
    emitter_package = "conductoR", emitter_version = .conductor_version(),
    inferential_target = upstream@inferential_target,
    run_id = paste0("select-", .summary_digest(list(summ, kept))),
    method = "select:rule", seed = NA_integer_, params = kept, summary = summ,
    consumed_manifests = list(orchestraManifest::manifest_lineage(upstream)),
    metadata = list(derived = TRUE, source = "conductoR:select"),
    timestamp = Sys.time(), data_hash = dh)
}

# One manifest for a node run once per item: the instance manifests in
# `outputs`, named by instance id, and one row per item in `params`.
foreach_to_manifest <- function(node, items, target) {
  outs <- Filter(Negate(is.null), stats::setNames(
    lapply(items, function(it) it$manifest),
    vapply(items, function(it) it$instance, character(1))))
  if (length(outs) == 0L) outs <- NULL
  tab <- data.frame(
    item     = vapply(items, function(it) it$item, character(1)),
    instance = vapply(items, function(it) it$instance, character(1)),
    status   = vapply(items, function(it) it$status, character(1)),
    run_id   = vapply(items, function(it) it$run_id, character(1)),
    headline = vapply(items, function(it) it$headline, character(1)),
    stringsAsFactors = FALSE)
  status <- tab$status
  summ <- orchestraManifest::manifest_summary(
    headline = "foreach", abstained = FALSE,
    metrics = list(n_items = nrow(tab), n_ran = sum(status == "ran"),
                   n_refused = sum(status == "refused"),
                   n_skipped = sum(status == "skipped")))
  dh <- orchestraManifest::manifest_data_hash(tab, outs, NULL, NULL,
                                              NA_integer_, summ)
  orchestraManifest::orchestra_manifest(
    emitter_package = "conductoR", emitter_version = .conductor_version(),
    inferential_target = target,
    run_id = paste0("foreach-", .summary_digest(list(node@id, tab))),
    method = "foreach:items", seed = NA_integer_, params = tab,
    outputs = outs, summary = summ,
    consumed_manifests = lapply(unname(outs),
                                orchestraManifest::manifest_lineage),
    metadata = list(derived = !is.null(outs), source = "conductoR:foreach"),
    timestamp = Sys.time(), data_hash = dh)
}

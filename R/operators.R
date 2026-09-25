# operators.R -- The two epistemic operators the engine performs itself.
#
# Both read the typed `summary` slot of the manifests they consume and emit
# a `decisions` manifest through the internal emitters in emitters.R. The
# policies are documented on conduct().

.op_abstain_gate <- function(node, inputs) {
  if (length(node@consumes) != 1L) {
    stop(sprintf("gate '%s' must consume exactly one node", node@id),
         call. = FALSE)
  }
  src_id <- node@consumes[[1]]$from
  m  <- inputs[[src_id]]
  s  <- m@summary
  if (is.null(s)) {
    stop(sprintf("gate '%s' needs a manifest with a summary from '%s'",
                 node@id, src_id), call. = FALSE)
  }
  mx       <- s$metrics %||% list()
  decision <- s$headline %||% NA_character_
  z_floor  <- node@params$z_floor %||% 2.0
  n_min    <- node@params$n_min %||% 25L
  mode     <- node@params$mode %||% "flag"
  floors   <- node@params$floors %||% list()
  if (!mode %in% c("flag", "halt-branch", "reroute")) {
    stop(sprintf("gate '%s': mode must be flag, halt-branch or reroute",
                 node@id), call. = FALSE)
  }

  power_z <- NA_real_
  if (all(c("effect_mean", "noise_sd", "n") %in% names(mx)) &&
      is.finite(mx$noise_sd) && mx$noise_sd > 0) {
    power_z <- abs(mx$effect_mean) / (mx$noise_sd / sqrt(mx$n))
  }
  n_obs <- mx$n %||% NA_integer_
  small_n_null <- identical(decision, "no_effect") &&
    is.finite(n_obs) && n_obs < n_min
  underpowered <- identical(decision, "no_effect") &&
    is.finite(power_z) && power_z < z_floor

  floor_hits <- character(0)
  for (nm in names(floors)) {
    v <- mx[[nm]]
    if (!is.null(v) && is.finite(v) && v < floors[[nm]]) {
      floor_hits <- c(floor_hits,
                      sprintf("%s = %.3g < %.3g", nm, v, floors[[nm]]))
    }
  }

  abstain <- isTRUE(s$abstained) || small_n_null || underpowered ||
    length(floor_hits) > 0L
  reason <- if (!abstain) {
    "verdict passes the power-aware gate"
  } else if (length(floor_hits) > 0L) {
    sprintf("identification floor violated (%s); abstaining",
            paste(floor_hits, collapse = "; "))
  } else if (small_n_null) {
    sprintf(paste0("small-sample no_effect: n = %d < %d, so the design ",
                   "cannot rule out the model-implied effect; abstaining"),
            as.integer(n_obs), as.integer(n_min))
  } else if (underpowered) {
    sprintf(paste0("under-powered no_effect: power proxy z = %.2f < %.1f, ",
                   "so the design cannot resolve the model-implied effect; ",
                   "abstaining"), power_z, z_floor)
  } else {
    s$abstain_reason %||% "upstream emitter abstained"
  }

  gate_to_manifest(
    gate_decision = if (abstain) "abstain" else "pass",
    reason = reason, power_z = power_z, source_decision = decision,
    mode = mode, gated_run_id = m@run_id, consumed = list(m))
}

.op_triangulate <- function(node, inputs) {
  is_gate <- vapply(inputs, function(m) grepl("abstain_gate", m@method),
                    logical(1))
  if (sum(is_gate) > 1L) {
    stop(sprintf("triangulate '%s' consumes more than one gate", node@id),
         call. = FALSE)
  }
  gate    <- if (any(is_gate)) inputs[[which(is_gate)]] else NULL
  lens_ms <- inputs[!is_gate]

  gated_out <- !is.null(gate) && .abstained(gate)
  reroute   <- !is.null(gate) && isTRUE(gate@summary$metrics$reroute)
  gated_rid <- if (!is.null(gate)) gate@summary$metrics$gated_run_id else NULL

  if (gated_out && reroute && !is.null(gated_rid)) {
    keep <- vapply(lens_ms, function(m) !identical(m@run_id, gated_rid),
                   logical(1))
    lens_ms <- lens_ms[keep]
  }

  lens_effect <- vapply(lens_ms, function(m) {
    mx <- m@summary$metrics
    isTRUE(mx$effect_present) || isTRUE(mx$influence_present)
  }, logical(1))
  L <- length(lens_ms)
  n_eff <- sum(lens_effect)

  consensus <- if (gated_out && !reroute) {
    "abstain"
  } else if (L == 0L) {
    "abstain"
  } else if (n_eff == L && L >= 2L) {
    "causal_effect_corroborated"
  } else if (n_eff == L && L == 1L) {
    "single_lens_effect"
  } else if (n_eff == 0L && L >= 2L) {
    "no_effect_corroborated"
  } else if (n_eff == 0L && L == 1L) {
    "single_lens_no_effect"
  } else {
    "disagreement_flagged"
  }
  agree <- consensus %in% c("causal_effect_corroborated",
                            "no_effect_corroborated")

  lenses <- stats::setNames(
    lapply(lens_ms, function(m) m@summary$headline %||% NA_character_),
    names(lens_ms))

  triangulate_to_manifest(
    consensus = consensus, agree = agree, lenses = lenses, n_lenses = L,
    gate = if (!is.null(gate)) gate@summary$headline else NA_character_,
    rerouted = gated_out && reroute,
    consumed = Filter(Negate(is.null), c(unname(lens_ms), list(gate))))
}

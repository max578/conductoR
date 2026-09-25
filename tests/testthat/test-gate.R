gate_run <- function(lens, params = list()) {
  p <- oap_plan(id = "gate", nodes = list(
    node("lens", lens, emits = "treatment_effects"),
    gate_node("lens", params = params)
  ))
  conduct(p, verbose = FALSE)$bus$gate
}

test_that("a well-powered verdict passes", {
  g <- gate_run(lens_tool("no_effect", FALSE, metrics = list(
    n = 200L, effect_mean = 2, noise_sd = 1)))
  expect_identical(g@summary$headline, "pass")
  expect_false(g@summary$abstained)
  expect_identical(g@summary$metrics$source_decision, "no_effect")
  expect_equal(g@summary$metrics$power_z, 2 / (1 / sqrt(200)))
  expect_identical(g@inferential_target, "decisions")
  expect_identical(g@emitter_package, "conductoR")
  expect_identical(g@emitter_version, as.character(packageVersion("conductoR")))
  expect_length(g@consumed_manifests, 1L)
})

test_that("a small-sample no_effect abstains on the n_min floor", {
  g <- gate_run(lens_tool("no_effect", FALSE, metrics = list(n = 12L)))
  expect_identical(g@summary$headline, "abstain")
  expect_match(g@summary$abstain_reason, "n = 12 < 25")
  g2 <- gate_run(lens_tool("no_effect", FALSE, metrics = list(n = 12L)),
                 params = list(n_min = 10L))
  expect_identical(g2@summary$headline, "pass")
})

test_that("an under-powered no_effect abstains on the z_floor", {
  g <- gate_run(lens_tool("no_effect", FALSE, metrics = list(
    n = 100L, effect_mean = 0.1, noise_sd = 1)))
  expect_identical(g@summary$headline, "abstain")
  expect_match(g@summary$abstain_reason, "under-powered")
  g2 <- gate_run(lens_tool("no_effect", FALSE, metrics = list(
    n = 100L, effect_mean = 0.1, noise_sd = 1)), params = list(z_floor = 0.5))
  expect_identical(g2@summary$headline, "pass")
})

test_that("a small-sample effect verdict is not gated by the power floors", {
  g <- gate_run(lens_tool("effect", TRUE, metrics = list(n = 3L)))
  expect_identical(g@summary$headline, "pass")
})

test_that("a named floor gates any metric, whatever the verdict", {
  g <- gate_run(lens_tool("effect", TRUE, metrics = list(n = 100L, epv = 4)),
                params = list(floors = list(epv = 10)))
  expect_identical(g@summary$headline, "abstain")
  expect_match(g@summary$abstain_reason, "epv = 4 < 10")
  g2 <- gate_run(lens_tool("effect", TRUE, metrics = list(n = 100L, epv = 12)),
                 params = list(floors = list(epv = 10)))
  expect_identical(g2@summary$headline, "pass")
})

test_that("an upstream abstention is carried through with its reason", {
  abstaining <- function(inputs, data, node) {
    m <- lens_to_manifest("effect", TRUE, "x", "x:y")
    s <- orchestraManifest::manifest_summary("effect", abstained = TRUE,
                                             abstain_reason = "posterior inadequate",
                                             metrics = list(effect_present = TRUE))
    orchestraManifest::orchestra_manifest(
      emitter_package = "x", emitter_version = "1", inferential_target = "treatment_effects",
      run_id = "x-1", method = "x:y", summary = s, timestamp = Sys.time(),
      data_hash = orchestraManifest::manifest_data_hash(data.frame(), NULL, NULL, NULL,
                                                        NA_integer_, s))
  }
  g <- gate_run(abstaining)
  expect_identical(g@summary$headline, "abstain")
  expect_identical(g@summary$abstain_reason, "posterior inadequate")
})

test_that("flag mode records the abstention and lets the branch run", {
  p <- oap_plan(id = "flag", nodes = list(
    node("lens", no_effect_lens, emits = "treatment_effects"),
    gate_node("lens", params = list(mode = "flag", n_min = 200L)),
    node("after", effect_lens, consumes = list(edge("gate", "decisions")),
         emits = "treatment_effects")
  ))
  run <- conduct(p, verbose = FALSE)
  expect_identical(run$n_skipped, 0L)
  expect_true(run$bus$gate@summary$abstained)
  expect_false(run$bus$gate@summary$metrics$reroute)
  expect_identical(run$terminal@run_id, run$bus$after@run_id)
})

test_that("halt-branch mode skips every descendant and records it", {
  p <- oap_plan(id = "halt", nodes = list(
    node("lens", no_effect_lens, emits = "treatment_effects"),
    node("other", effect_lens, emits = "treatment_effects"),
    gate_node("lens", params = list(mode = "halt-branch", n_min = 200L)),
    node("after", effect_lens, consumes = list(edge("gate", "decisions")),
         emits = "treatment_effects"),
    node("later", effect_lens, consumes = list(edge("after", "treatment_effects")),
         emits = "treatment_effects")
  ))
  run <- conduct(p, verbose = FALSE)
  expect_identical(run$n_skipped, 2L)
  expect_null(run$bus$after)
  expect_null(run$bus$later)
  expect_false(is.null(run$bus$other))
  tab <- run_provenance(run)
  expect_identical(tab$operator[tab$node %in% c("after", "later")], c("skipped", "skipped"))
  expect_identical(run$terminal@run_id, run$bus$gate@run_id)
})

test_that("reroute mode sets the reroute flag for a downstream triangulation", {
  g <- gate_run(no_effect_lens, params = list(mode = "reroute", n_min = 200L))
  expect_true(g@summary$metrics$reroute)
  expect_identical(g@summary$metrics$mode, "reroute")
})

test_that("an unknown mode and a gate with two inputs are errors", {
  expect_error(gate_run(no_effect_lens, params = list(mode = "ignore")),
               "mode must be")
  p <- oap_plan(id = "two_in", nodes = list(
    node("a", effect_lens, emits = "treatment_effects"),
    node("b", effect_lens, emits = "treatment_effects"),
    node("gate", operator = "abstain_gate", emits = "decisions",
         consumes = list(edge("a", "treatment_effects"), edge("b", "treatment_effects")))
  ))
  expect_error(conduct(p, verbose = FALSE), "exactly one node")
})

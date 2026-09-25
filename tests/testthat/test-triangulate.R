tri_run <- function(lenses, gate_params = NULL, gate_on = NULL) {
  nodes <- lapply(names(lenses), function(id) {
    node(id, lenses[[id]], emits = "treatment_effects")
  })
  if (!is.null(gate_params)) {
    nodes <- c(nodes, list(gate_node(gate_on, params = gate_params)))
  }
  nodes <- c(nodes, list(consensus_node(names(lenses),
                                        gate = if (is.null(gate_params)) NULL else "gate")))
  conduct(oap_plan(id = "tri", nodes = nodes), verbose = FALSE)$bus$consensus
}

yes1 <- lens_tool("effect", TRUE, "yes1")
yes2 <- lens_tool("effect", TRUE, "yes2")
no1  <- lens_tool("no_effect", FALSE, "no1", metrics = list(n = 5L))
no2  <- lens_tool("no_effect", FALSE, "no2", metrics = list(n = 5L))

test_that("two agreeing effect lenses corroborate", {
  c1 <- tri_run(list(a = yes1, b = yes2))
  expect_identical(c1@summary$headline, "causal_effect_corroborated")
  expect_true(c1@summary$metrics$agree)
  expect_identical(c1@summary$metrics$n_lenses, 2L)
  expect_identical(c1@summary$metrics$lenses, list(a = "effect", b = "effect"))
  expect_true(is.na(c1@summary$metrics$gate))
  expect_length(c1@consumed_manifests, 2L)
})

test_that("two agreeing null lenses corroborate no effect", {
  c1 <- tri_run(list(a = no1, b = no2))
  expect_identical(c1@summary$headline, "no_effect_corroborated")
  expect_true(c1@summary$metrics$agree)
})

test_that("one lens gives a single-lens headline either way", {
  expect_identical(tri_run(list(a = yes1))@summary$headline, "single_lens_effect")
  expect_identical(tri_run(list(a = no1))@summary$headline, "single_lens_no_effect")
  expect_false(tri_run(list(a = yes1))@summary$metrics$agree)
})

test_that("mixed lenses flag a disagreement", {
  c1 <- tri_run(list(a = yes1, b = no1, c = yes2))
  expect_identical(c1@summary$headline, "disagreement_flagged")
  expect_false(c1@summary$metrics$agree)
})

test_that("a gate that abstains in flag mode makes the consensus abstain", {
  c1 <- tri_run(list(a = yes1, b = no1), gate_params = list(mode = "flag"), gate_on = "b")
  expect_identical(c1@summary$headline, "abstain")
  expect_true(c1@summary$abstained)
  expect_identical(c1@summary$metrics$gate, "abstain")
  expect_false(c1@summary$metrics$rerouted)
  expect_length(c1@consumed_manifests, 3L)
})

test_that("a gate that passes leaves the lenses to decide", {
  c1 <- tri_run(list(a = yes1, b = yes2), gate_params = list(mode = "flag"), gate_on = "a")
  expect_identical(c1@summary$headline, "causal_effect_corroborated")
  expect_identical(c1@summary$metrics$gate, "pass")
})

test_that("a reroute drops the gated lens and reconciles the survivors", {
  c1 <- tri_run(list(a = yes1, b = no1), gate_params = list(mode = "reroute"), gate_on = "b")
  expect_identical(c1@summary$headline, "single_lens_effect")
  expect_true(c1@summary$metrics$rerouted)
  expect_identical(c1@summary$metrics$n_lenses, 1L)
  expect_identical(names(c1@summary$metrics$lenses), "a")
})

test_that("a reroute that leaves no lens abstains", {
  c1 <- tri_run(list(a = no1), gate_params = list(mode = "reroute"), gate_on = "a")
  expect_identical(c1@summary$headline, "abstain")
  expect_identical(c1@summary$metrics$n_lenses, 0L)
})

test_that("influence_present counts as an effect", {
  dyn <- function(inputs, data, node) {
    m <- lens_to_manifest("influence", FALSE, "dyn", "dyn:series",
                          metrics = list(influence_present = TRUE))
    m
  }
  c1 <- tri_run(list(a = yes1, b = dyn))
  expect_identical(c1@summary$headline, "causal_effect_corroborated")
})

test_that("two gates on one triangulation are refused", {
  p <- oap_plan(id = "two_gates", nodes = list(
    node("a", yes1, emits = "treatment_effects"),
    gate_node("a", id = "g1"),
    gate_node("a", id = "g2"),
    node("consensus", operator = "triangulate", emits = "decisions",
         consumes = list(edge("a", "treatment_effects"), edge("g1", "decisions"),
                         edge("g2", "decisions")))
  ))
  expect_error(conduct(p, verbose = FALSE), "more than one gate")
})

# Version 0.3 record and provenance: the refusal reason kept by the holder,
# the refusal and shape words, the exported plan hash and plan_validate() on
# plan objects.

# Refusal reasons -------------------------------------------------------------

test_that("provenance keeps the abstention reason of a refused node", {
  p <- oap_plan(id = "why", nodes = list(
    node("free", planted_tool(abstained = TRUE,
                              abstain_reason = "too few sites"),
         emits = "parameters"),
    node("typed", planted_tool(abstained = TRUE,
                               abstain_reason = "credential_unavailable"),
         emits = "parameters"),
    node("ran", planted_tool(), emits = "parameters")))
  prov <- run_provenance(conduct(p, verbose = FALSE))
  expect_identical(prov$status, c("refused", "refused", "ran"))
  expect_identical(prov$reason,
                   c("too few sites", "credential_unavailable", ""))
})

test_that("a refusal without a reason leaves the provenance reason empty", {
  p <- oap_plan(id = "silent", nodes = list(
    node("a", planted_tool(abstained = TRUE), emits = "parameters")))
  prov <- run_provenance(conduct(p, verbose = FALSE))
  expect_identical(prov$reason, "")
})

test_that("refusal and shape words reach the record and its lines", {
  words <- c("licence_unavailable", "credential_unavailable",
             "no_interior_minimum", "no_interior_optimum", "no_response")
  nodes <- lapply(seq_along(words), function(i) {
    node(paste0("n", i), planted_tool(abstained = TRUE,
                                      abstain_reason = words[i]),
         emits = "parameters")
  })
  rec <- run_record(conduct(oap_plan(id = "words", nodes = nodes),
                            verbose = FALSE))
  expect_identical(rec$status, rep("refused", 5L))
  expect_identical(rec$reason, words)
  expect_identical(format_record(rec), paste(paste0("n", 1:5), words))
})

test_that("a free-text refusal never leaves the holder", {
  p <- oap_plan(id = "free", nodes = list(
    node("a", planted_tool(abstained = TRUE,
                           abstain_reason = "site S3 has 2 plots"),
         emits = "parameters")))
  run <- conduct(p, verbose = FALSE)
  rec <- run_record(run)
  expect_true(is.na(rec$reason))
  expect_identical(format_record(rec), "stop")
  ret <- run_return(run, dose = 0)
  expect_false(any(grepl("S3", format(ret), fixed = TRUE)))
})

# plan_hash() ----------------------------------------------------------------

test_that("plan_hash() is the hash conduct() records", {
  p <- oap_plan(id = "seal", nodes = list(
    node("lens", effect_lens, emits = "treatment_effects")))
  h <- plan_hash(p)
  expect_match(h, "^sha256:[0-9a-f]{64}$")
  expect_identical(conduct(p, verbose = FALSE)$plan_hash, h)
})

test_that("plan_hash() changes when a tool's code changes", {
  lens_a <- function(inputs, data, node) {
    lens_to_manifest("effect", TRUE, "demo", "demo:a")
  }
  lens_b <- function(inputs, data, node) {
    lens_to_manifest("effect", TRUE, "demo", "demo:b")
  }
  pa <- oap_plan(id = "seal", nodes = list(
    node("lens", lens_a, emits = "treatment_effects")))
  pb <- oap_plan(id = "seal", nodes = list(
    node("lens", lens_b, emits = "treatment_effects")))
  expect_false(identical(plan_hash(pa), plan_hash(pb)))
})

test_that("plan_hash() refuses anything but a plan", {
  expect_error(plan_hash("plan.yaml"))
})

# plan_validate() on plan objects -------------------------------------------

test_that("plan_validate() checks a plan object", {
  p <- oap_plan(id = "one_lens", nodes = list(
    node("lens", tool = "my_lens", emits = "treatment_effects"),
    node("gate", operator = "abstain_gate", emits = "decisions",
         consumes = list(edge("lens", "treatment_effects")),
         params = list(n_min = 25L))))
  expect_identical(nrow(plan_validate(p)), 0L)
  expect_identical(nrow(plan_validate(p)),
                   nrow(plan_validate(plan_to_yaml(p))))
})

test_that("plan_validate() accepts a plan whose tools are inline functions", {
  p <- oap_plan(id = "inline", nodes = list(
    node("lens", effect_lens, emits = "treatment_effects")))
  expect_identical(nrow(plan_validate(p)), 0L)
  expect_error(plan_to_yaml(p), "inline closure")
})

test_that("plan_validate() says what it takes", {
  expect_error(plan_validate(list(id = "x")),
               "takes an oap_plan, a plan file path or the plan's YAML")
  expect_error(plan_validate(NA_character_), "takes an oap_plan")
})

test_that("a run records plan and input hashes and its label", {
  p <- oap_plan(id = "h", nodes = list(
    node("a", effect_lens, emits = "treatment_effects")))
  r <- conduct(p, data = list(x = 1), verbose = FALSE, label = "blinded")
  expect_match(r$plan_hash, "^sha256:")
  expect_match(r$input_hash, "^sha256:")
  expect_identical(r$label, "blinded")
})

test_that("same_plan separates the plan from the data", {
  p <- oap_plan(id = "h", nodes = list(
    node("a", effect_lens, emits = "treatment_effects")))
  r1 <- conduct(p, data = list(d = c(1, -1)), verbose = FALSE)
  r2 <- conduct(p, data = list(d = c(1, 1)), verbose = FALSE)
  s <- same_plan(r1, r2)
  expect_true(s$same_plan)
  expect_false(s$same_input)
})

test_that("editing a tool changes the plan hash", {
  f <- tempfile(fileext = ".R")
  writeLines("lens <- function(inputs, data, node) conductoR::lens_to_manifest('effect', TRUE, 's', 's:1')", f)
  p <- oap_plan(id = "s", nodes = list(
    node("a", paste0("script:", f, "::lens"), emits = "treatment_effects")))
  h1 <- conduct(p, verbose = FALSE)$plan_hash
  writeLines("lens <- function(inputs, data, node) conductoR::lens_to_manifest('effect', TRUE, 's', 's:2')", f)
  h2 <- conduct(p, verbose = FALSE)$plan_hash
  expect_false(identical(h1, h2))
})

test_that("the provenance record carries the hashes", {
  p <- oap_plan(id = "h", nodes = list(
    node("a", effect_lens, emits = "treatment_effects")))
  d <- tempfile()
  r <- conduct(p, verbose = FALSE, out_dir = d, label = "unblinded")
  txt <- readLines(file.path(d, "provenance.md"))
  expect_true(any(grepl(r$plan_hash, txt, fixed = TRUE)))
  expect_true(any(txt == "label: unblinded"))
})

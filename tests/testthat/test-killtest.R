# The four properties the package exists for.

test_that("K1 a mistyped handoff is refused before the downstream node runs", {
  ran_b <- 0L
  tool_b <- function(inputs, data, node) {
    ran_b <<- ran_b + 1L
    lens_to_manifest("x", TRUE, "b", "b:x")
  }
  p <- oap_plan(id = "mistyped", nodes = list(
    node("A", effect_lens, emits = "treatment_effects"),
    node("B", tool_b, consumes = list(edge("A", "parameters")),
         emits = "treatment_effects")
  ))
  expect_error(conduct(p, verbose = FALSE), "TYPED-EDGE VIOLATION")
  expect_identical(ran_b, 0L)
})

test_that("K2 a verdict rides the contract, verifies and is consumed", {
  m <- lens_to_manifest("no_effect", FALSE, "kernel", "kernel:test",
                        metrics = list(p_value = 0.6, n = 12L))
  expect_true(orchestraManifest::is_orchestra_manifest(m))
  expect_identical(m@inferential_target, "treatment_effects")
  expect_s3_class(m@summary, "manifest_summary")
  expect_identical(m@summary$headline, "no_effect")
  expect_false(m@summary$metrics$effect_present)
  expect_identical(m@summary$metrics$n, 12L)
  expect_true(orchestraManifest::verify_manifest(m)$ok)
  expect_identical(orchestraManifest::consume_manifest(m), m)
})

test_that("K3 a tampered verdict is detected and refused on an edge", {
  m <- lens_to_manifest("no_effect", FALSE, "kernel", "kernel:test")
  s <- m@summary
  s$headline <- "effect"
  tampered <- m
  tampered@summary <- s
  expect_false(orchestraManifest::verify_manifest(tampered)$ok)
  expect_error(orchestraManifest::consume_manifest(tampered), "integrity")

  p <- oap_plan(id = "tampered", nodes = list(
    node("A", function(inputs, data, node) tampered,
         emits = "treatment_effects"),
    gate_node("A")
  ))
  expect_error(conduct(p, verbose = FALSE), "integrity")
})

test_that("K4 a script tool reference resolves and a missing function is reported", {
  tmp <- withr_tempfile(".R")
  writeLines("my_tool <- function(inputs, data, node) 42L", tmp)
  fn <- resolve_tool(paste0("script:", tmp, "::my_tool"))
  expect_true(is.function(fn))
  expect_identical(fn(list(), list(), NULL), 42L)
  expect_error(resolve_tool(paste0("script:", tmp, "::nope")), "no function")
  expect_error(resolve_tool("script:nowhere.R::f"), "not found")
  expect_error(resolve_tool("script:bad"), "script:<path>::<fn>")
})

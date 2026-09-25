# Components that emit manifests export their own verify_manifest() and
# orchestra_manifest(). Every contract call in this package is namespace
# qualified, so attaching such a component must not change a run.

test_that("a run is unchanged with PESTO attached", {
  skip_if_not_installed("PESTO")
  p <- oap_plan(id = "mask", nodes = list(
    node("a", effect_lens, emits = "treatment_effects"),
    node("b", no_effect_lens, emits = "treatment_effects"),
    gate_node("b"),
    consensus_node(c("a", "b"), gate = "gate")
  ))
  before <- conduct(p, verbose = FALSE)

  suppressPackageStartupMessages(library(PESTO))
  on.exit(detach("package:PESTO", character.only = TRUE), add = TRUE)
  expect_true("package:PESTO" %in% search())
  masking <- intersect(getNamespaceExports("PESTO"),
                       getNamespaceExports("orchestraManifest"))
  expect_true(length(masking) > 0L)

  after <- conduct(p, verbose = FALSE)
  expect_identical(after$terminal@run_id, before$terminal@run_id)
  expect_identical(after$terminal@summary$headline, before$terminal@summary$headline)
  expect_true(orchestraManifest::verify_manifest(after$terminal)$ok)
  expect_error(conduct(oap_plan(id = "bad", nodes = list(
    node("a", effect_lens, emits = "treatment_effects"),
    node("c", effect_lens, consumes = list(edge("a", "parameters")))
  )), verbose = FALSE), "TYPED-EDGE VIOLATION")
})

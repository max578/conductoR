test_that("a compiled script names one target per node with its dependencies", {
  p <- oap_plan(id = "chain", nodes = list(
    node("a", "stats::median"),
    node("b", "stats::median", consumes = list(edge("a")))
  ))
  f <- withr_tempfile(".R")
  compile_to_targets(p, f, preamble = "# head")
  lines <- readLines(f)
  expect_identical(lines[1], "# head")
  expect_true(any(grepl('tar_target\\(n_a, conductoR::conduct_step\\(conductoR::plan_node\\(.score, "a"\\), list\\(\\), .data\\)', lines)))
  expect_true(any(grepl("tar_target\\(n_b, .*list\\(a = n_a\\)", lines)))
  expect_identical(terminal_target(p), "n_b")
})

test_that("a compiled plan runs under targets to the same verdict as conduct()", {
  skip_if_not_installed("targets")
  p <- oap_plan(id = "tar", nodes = list(
    node("a", effect_lens, emits = "treatment_effects"),
    node("b", no_effect_lens, emits = "treatment_effects"),
    gate_node("b", params = list(mode = "reroute", n_min = 200L)),
    consensus_node(c("a", "b"), gate = "gate")
  ))
  direct <- conduct(p, verbose = FALSE)

  dir <- withr_tempfile()
  dir.create(dir)
  saveRDS(p, file.path(dir, "plan.rds"))
  script <- file.path(dir, "_targets.R")
  store  <- file.path(dir, "_targets")
  compile_to_targets(p, script, preamble = c(
    sprintf(".score <- readRDS(%s)", shQuote(file.path(dir, "plan.rds"))),
    ".data <- list()"))
  targets::tar_make(script = script, store = store, callr_function = NULL,
                    reporter = "silent")
  out <- targets::tar_read_raw(terminal_target(p), store = store)
  expect_identical(out@run_id, direct$terminal@run_id)
  expect_identical(out@summary$headline, "single_lens_effect")
  expect_true(orchestraManifest::verify_manifest(out)$ok)
})

two_lens_plan <- function() {
  oap_plan(id = "two", meta = list(question = "test"), nodes = list(
    node("a", effect_lens, emits = "treatment_effects"),
    node("b", no_effect_lens, emits = "treatment_effects"),
    gate_node("b"),
    consensus_node(c("a", "b"), gate = "gate")
  ))
}

test_that("conduct() and a hand-walked conduct_step() emit the same manifests", {
  p <- two_lens_plan()
  run <- conduct(p, verbose = FALSE)
  bus <- list()
  for (id in plan_order(p)) {
    n <- plan_node(p, id)
    inputs <- bus[vapply(n@consumes, function(e) e$from, character(1))]
    bus[[id]] <- conduct_step(n, inputs)
  }
  for (id in names(bus)) {
    expect_identical(bus[[id]]@run_id, run$bus[[id]]@run_id)
    expect_identical(bus[[id]]@data_hash, run$bus[[id]]@data_hash)
  }
  expect_identical(run$n_nodes, 4L)
  expect_identical(run$n_skipped, 0L)
  expect_identical(run$terminal@run_id, bus$consensus@run_id)
})

test_that("verbose printing names every node once", {
  out <- capture.output(conduct(two_lens_plan(), verbose = TRUE))
  expect_length(out, 4L)
  expect_true(any(grepl("^\\s*\\[abstain_gate\\s*\\] gate", out)))
})

test_that("a run writes its three provenance files", {
  dir <- withr_tempfile()
  run <- conduct(two_lens_plan(), out_dir = dir, verbose = FALSE)
  expect_setequal(list.files(dir), c("provenance.md", "run.rds", "run_card.md"))
  back <- readRDS(file.path(dir, "run.rds"))
  expect_identical(back$terminal@run_id, run$terminal@run_id)
  prov <- readLines(file.path(dir, "provenance.md"))
  expect_true(any(grepl("^kind: run-provenance$", prov)))
  expect_true(any(grepl(paste0("^consensus:", substr(run$terminal@data_hash, 1L, 18L)), prov)))
  card <- readLines(file.path(dir, "run_card.md"))
  expect_true(any(grepl("Terminal verdict", card)))
})

test_that("run_provenance() is one row per node in execution order", {
  run <- conduct(two_lens_plan(), verbose = FALSE)
  tab <- run_provenance(run)
  expect_s3_class(tab, "data.frame")
  expect_identical(tab$node, plan_order(two_lens_plan()))
  expect_identical(tab$consumes[tab$node == "consensus"], "a+b+gate")
  expect_true(is.logical(tab$abstained))
})

test_that("a tool that returns the wrong thing is refused after it runs", {
  p <- oap_plan(id = "bad", nodes = list(
    node("a", function(inputs, data, node) list(), emits = "any")
  ))
  expect_error(conduct(p, verbose = FALSE), "did not return an orchestra_manifest")
  q <- oap_plan(id = "wrong", nodes = list(
    node("a", effect_lens, emits = "decisions")
  ))
  expect_error(conduct(q, verbose = FALSE), "emitted 'treatment_effects', promised 'decisions'")
})

test_that("a node receives the data object and its own node", {
  seen <- NULL
  p <- oap_plan(id = "data", nodes = list(
    node("a", function(inputs, data, node) {
      seen <<- list(data = data, id = node@id, n_inputs = length(inputs))
      effect_lens(inputs, data, node)
    }, emits = "treatment_effects", params = list(k = 1))
  ))
  conduct(p, data = list(x = 1:3), verbose = FALSE)
  expect_identical(seen$data$x, 1:3)
  expect_identical(seen$id, "a")
  expect_identical(seen$n_inputs, 0L)
})

test_that("an edge typed 'any' accepts every target", {
  p <- oap_plan(id = "any", nodes = list(
    node("a", effect_lens, emits = "treatment_effects"),
    node("b", function(inputs, data, node) {
      decision_to_manifest("act", "d", "d:rule", consumed = list(inputs$a))
    }, consumes = list(edge("a")), emits = "decisions")
  ))
  run <- conduct(p, verbose = FALSE)
  expect_identical(run$terminal@consumed_manifests[[1]]$run_id, run$bus$a@run_id)
  expect_true(run$terminal@metadata$derived)
})

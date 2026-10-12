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

# The plan language 0.2 properties.

test_that("K5 a FALSE when keeps the tool from running", {
  calls <- new.env()
  p <- oap_plan(id = "k5", nodes = list(
    node("a", no_effect_lens, emits = "treatment_effects"),
    node("b", counting_tool(calls), emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects")),
         when = "a.metrics.effect_present")))
  r <- conduct(p, verbose = FALSE)
  expect_identical(calls$n, 0L)
  expect_identical(r$conditions$value, FALSE)
})

test_that("K6 a condition cannot run code by any route", {
  marker <- tempfile()
  touch <- sprintf("file.create('%s')", marker)
  routes <- c(touch, sprintf("isTRUE(%s)", touch),
              sprintf("a.headline == %s", touch),
              sprintf("a.headline %%in%% c(%s)", touch),
              sprintf("!(%s)", touch), sprintf("base::%s", touch))
  for (cond in routes) {
    expect_error(node("b", consumes = list(edge("a")), when = cond))
    expect_error(node("s", operator = "select", consumes = list(edge("a")),
                      params = list(rule = cond)))
  }
  expect_false(file.exists(marker))
})

test_that("K7 too many items stop the run before any item runs", {
  calls <- new.env()
  p <- oap_plan(id = "k7", nodes = list(
    node("t", table_tool(terms_tab), emits = "parameters"),
    node("v", item_tool(calls = calls), emits = "treatment_effects",
         consumes = list(edge("t", "parameters")),
         foreach = list(from = "t", column = "term"), foreach_max = 3L)))
  expect_error(conduct(p, verbose = FALSE), "more than foreach_max")
  expect_identical(calls$items, character(0))
})

test_that("K8 on_refusal stop keeps every later tool from running", {
  calls <- new.env()
  p <- oap_plan(id = "k8", nodes = list(
    node("a", refusing_lens, emits = "treatment_effects", on_refusal = "stop"),
    node("b", counting_tool(calls), emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects"))),
    node("c", counting_tool(calls), emits = "treatment_effects")))
  r <- conduct(p, verbose = FALSE)
  expect_identical(calls$n, 0L)
  expect_identical(r$stopped_by, "a")
})

test_that("K9 an unquoted yes or no never becomes a logical", {
  yaml_txt <- paste0("id: k9\nnodes:\n- id: a\n  tool: t\n",
                     "  params: {robust: no}\n")
  expect_error(plan_from_yaml(yaml_txt), "written without quotes")
  expect_identical(plan_validate(yaml_txt)$field, "params.robust")
})

test_that("K10 a tampered selection is refused downstream", {
  p <- oap_plan(id = "k10", nodes = list(
    node("t", table_tool(terms_tab), emits = "parameters"),
    node("s", operator = "select", emits = "parameters",
         consumes = list(edge("t", "parameters")),
         params = list(rule = "p_value < 0.05"))))
  m <- conduct(p, verbose = FALSE)$bus$s
  expect_identical(m@params$term, c("window", "window:site"))
  m@params <- terms_tab
  expect_false(orchestraManifest::verify_manifest(m)$ok)
  down <- node("d", effect_lens, emits = "treatment_effects",
               consumes = list(edge("s", "parameters")))
  expect_error(conduct_step(down, list(s = m)), "integrity")
})

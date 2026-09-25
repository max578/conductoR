test_that("a registered tool resolves by name and an unknown name is an error", {
  register_tool("test_yes", effect_lens)
  expect_identical(resolve_tool("test_yes"), effect_lens)
  expect_error(resolve_tool("never_registered"), "unknown tool ref")
  expect_error(register_tool("", effect_lens))
  expect_error(register_tool("x", "not a function"))
})

test_that("a pkg::fn reference resolves to the exported function", {
  expect_identical(resolve_tool("stats::median"), stats::median)
})

test_that("a plan with named tools round-trips through YAML to the same run", {
  register_tool("test_yes", effect_lens)
  register_tool("test_no", no_effect_lens)
  p <- oap_plan(id = "rt", meta = list(question = "round trip"), nodes = list(
    node("a", "test_yes", emits = "treatment_effects", role = "ols"),
    node("b", "test_no", emits = "treatment_effects"),
    gate_node("b", params = list(mode = "reroute", n_min = 200L)),
    consensus_node(c("a", "b"), gate = "gate")
  ))
  f <- withr_tempfile(".oap.yaml")
  expect_identical(plan_to_yaml(p, f), f)
  q <- plan_from_yaml(f)
  expect_identical(q@id, p@id)
  expect_identical(q@meta, p@meta)
  expect_identical(plan_order(q), plan_order(p))
  expect_identical(plan_node(q, "a")@role, "ols")
  expect_identical(plan_node(q, "gate")@params$mode, "reroute")
  expect_identical(plan_node(q, "gate")@params$n_min, 200L)
  expect_identical(plan_to_yaml(q), plan_to_yaml(p))

  r1 <- conduct(p, verbose = FALSE)
  r2 <- conduct(q, verbose = FALSE)
  expect_identical(r1$terminal@run_id, r2$terminal@run_id)
  expect_identical(r1$terminal@summary$headline, "single_lens_effect")
})

test_that("plan_from_yaml() also accepts YAML text", {
  txt <- plan_to_yaml(oap_plan(id = "txt", nodes = list(
    node("g", operator = "triangulate", emits = "decisions"))))
  q <- plan_from_yaml(txt)
  expect_identical(q@id, "txt")
  expect_identical(plan_node(q, "g")@operator, "triangulate")
})

test_that("a compute node with an inline closure cannot be serialised", {
  p <- oap_plan(id = "inline", nodes = list(node("a", effect_lens)))
  expect_error(plan_to_yaml(p), "inline closure")
})

test_that("the shipped plans parse, validate and order", {
  files <- list.files(system.file("scores", package = "conductoR"),
                      pattern = "\\.oap\\.yaml$", full.names = TRUE)
  expect_length(files, 2L)
  for (f in files) {
    p <- plan_from_yaml(f)
    expect_s3_class(p, "conductoR::oap_plan")
    expect_length(plan_order(p), 6L)
    expect_identical(plan_order(p)[6], plan_node(p, plan_order(p)[6])@id)
  }
  p <- plan_from_yaml(files[grepl("dual_lens_gate", files)])
  expect_identical(plan_node(p, "gate")@params$n_min, 25L)
  expect_identical(plan_node(p, "lens_kernel")@tool_ref, "tool_kernel_lens")
})

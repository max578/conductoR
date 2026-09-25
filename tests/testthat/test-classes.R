test_that("node() stores a string tool as a reference", {
  n <- node("a", tool = "my_tool", emits = "decisions", role = "decision")
  expect_identical(n@tool_ref, "my_tool")
  expect_true(is.function(n@tool))
  expect_identical(n@role, "decision")
  m <- node("b", tool = function(...) NULL)
  expect_identical(m@tool_ref, "")
})

test_that("edge() returns from and contract", {
  expect_identical(edge("a"), list(from = "a", contract = "any"))
  expect_identical(edge("a", "decisions"), list(from = "a", contract = "decisions"))
})

test_that("the node validator rejects bad ids, operators, targets and edges", {
  expect_error(node(""), "single non-empty string")
  expect_error(node("a", operator = "mix"), "operator")
  expect_error(node("a", emits = "verdicts"), "emits")
  expect_error(node("a", consumes = list(list(from = "b"))), "needs `from` and `contract`")
  expect_error(node("a", consumes = list(edge("b", "verdicts"))), "not a known target")
})

test_that("every inferential target of the contract is accepted", {
  for (t in orchestraManifest::inferential_targets()) {
    expect_identical(node("a", emits = t)@emits, t)
  }
})

test_that("the plan validator rejects duplicates, unknown sources and non-nodes", {
  expect_error(oap_plan(id = "p", nodes = list(node("a"), node("a"))), "unique")
  expect_error(oap_plan(id = "p", nodes = list(node("a", consumes = list(edge("z"))))),
               "unknown node 'z'")
  expect_error(oap_plan(id = "p", nodes = list("a")), "oap_node")
  expect_error(oap_plan(id = "", nodes = list()), "plan `id`")
})

test_that("plan_node() fetches by id and refuses an unknown id", {
  p <- oap_plan(id = "p", nodes = list(node("a"), node("b")))
  expect_identical(plan_node(p, "b")@id, "b")
  expect_error(plan_node(p, "c"), "no node 'c'")
})

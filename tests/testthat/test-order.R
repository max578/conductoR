test_that("a diamond orders sources before sinks whatever the input order", {
  p <- oap_plan(id = "diamond", nodes = list(
    node("d", consumes = list(edge("b"), edge("c"))),
    node("c", consumes = list(edge("a"))),
    node("b", consumes = list(edge("a"))),
    node("a")
  ))
  ord <- plan_order(p)
  expect_setequal(ord, c("a", "b", "c", "d"))
  expect_identical(ord[1], "a")
  expect_identical(ord[4], "d")
  expect_identical(terminal_target(p), "n_d")
})

test_that("a cycle is an error naming the unresolved nodes", {
  p <- oap_plan(id = "loop", nodes = list(
    node("a", consumes = list(edge("c"))),
    node("b", consumes = list(edge("a"))),
    node("c", consumes = list(edge("b"))),
    node("root")
  ))
  expect_error(plan_order(p), "cyclic.*a, b, c")
})

test_that("an empty plan orders to nothing", {
  expect_identical(plan_order(oap_plan(id = "empty")), character(0))
})

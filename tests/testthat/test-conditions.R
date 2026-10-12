# The condition grammar of `when` and `select` rules.

plan_with_when <- function(cond) {
  sprintf(paste0("id: p\nnodes:\n- id: a\n  tool: t\n  emits: treatment_effects\n",
                 "- id: b\n  tool: t\n  emits: treatment_effects\n",
                 "  consumes: [{from: a, contract: treatment_effects}]\n",
                 "  when: '%s'\n"), cond)
}

test_that("C3 conditions that could run code are refused when the plan loads", {
  marker <- tempfile()
  attacks <- c(
    sprintf("system('touch %s')", marker),
    "eval(parse(text = 'a'))",
    "get('a.headline') == 1",
    "base::system('ls') == 0",
    "a.headline == base::Sys.time()",
    "a$headline == 'effect'",
    "a.headline <- 'effect'",
    "a.headline = 'effect'",
    "'effect' -> a.headline",
    "do.call('system', list('ls'))",
    "(function() TRUE)()",
    "a.metrics.p[1] < 0.05",
    "a.headline == Sys.getenv('HOME')",
    "{a.n_rows > 0}",
    "a.n_rows > 0; system('ls')"
  )
  for (cond in attacks) {
    expect_error(node("b", when = cond), "condition|expression",
                 label = cond)
    expect_gt(nrow(plan_validate(plan_with_when(gsub("'", "\"", cond)))), 0L)
  }
  expect_false(file.exists(marker))
})

test_that("C3 backticks, :: and assignment are refused with a plain message", {
  expect_error(node("b", when = "`a.headline` == 'effect'"),
               "backticks are not allowed")
  expect_error(node("b", when = "`system`('ls')"), "backticks are not allowed")
  expect_error(node("b", when = "base::isTRUE(a.abstained)"),
               "`::` is not allowed")
  expect_error(node("b", when = "a.headline <- 'x'"),
               "assignment is not allowed")
  expect_error(node("b", when = "a$headline == 'x'"), "`\\$` is not allowed")
  expect_error(node("b", when = "nchar(a.headline) > 2"),
               "`nchar` is not allowed")
  expect_error(plan_from_yaml(plan_with_when("system(\"ls\")")),
               "node 'b', when: `system` is not allowed in a condition")
})

test_that("C3 a select rule follows the same grammar", {
  expect_error(node("s", operator = "select",
                    consumes = list(edge("a", "parameters")),
                    params = list(rule = "eval(p_value)")),
               "`eval` is not allowed")
  expect_error(node("s", operator = "select",
                    consumes = list(edge("a", "parameters")),
                    params = list(rule = "p_value < get('x')")),
               "`get` is not allowed")
})

test_that("the grammar accepts references, literals and the listed operators", {
  ok <- c("a.headline == 'effect'",
          "a.abstained",
          "!a.abstained & a.metrics.p_value < 0.05",
          "a.n_rows >= 1 && a.metrics.n > 20",
          "a.headline %in% c('effect', 'no_effect')",
          "(a.metrics.effect > -0.5) | (a.metrics.effect < 0.5)",
          "a.metrics.flag == TRUE",
          "a.metrics.flag != FALSE")
  for (cond in ok) {
    chk <- .cond_check(cond, "when")
    expect_identical(chk$problems, character(0), label = cond)
    expect_identical(chk$refs, "a")
  }
})

test_that("references and numbers must be written in the documented form", {
  expect_match(.cond_check("headline == 'x'", "when")$problems,
               "is not a reference")
  expect_match(.cond_check("a.summary == 'x'", "when")$problems,
               "is not a reference")
  expect_match(.cond_check("a.metrics.n > 1e3", "when")$problems,
               "`1e3` is not written as a plain number")
  expect_match(.cond_check("a.metrics.n > 10L", "when")$problems,
               "`10L` is not written as a plain number")
  expect_match(.cond_check("a.abstained == T", "when")$problems,
               "TRUE or FALSE in full")
  expect_match(.cond_check("a.headline %in% c(a.headline)", "when")$problems,
               "c\\(\\) of literals")
  expect_match(.cond_check("", "when")$problems, "non-empty")
  expect_match(.cond_check("a.n_rows >", "when")$problems,
               "not a single valid expression")
})

test_that("the interpreter applies each operator without evaluating code", {
  vals <- list(x = c(1, 5, 10), k = c("a", "b", "c"))
  look <- function(name) vals[[name]]
  run <- function(txt) .cond_value(.cond_check(txt, "rule")$expr, look)
  expect_identical(run("x > 4"), c(FALSE, TRUE, TRUE))
  expect_identical(run("x >= 5 & k != 'c'"), c(FALSE, TRUE, FALSE))
  expect_identical(run("x < 2 || k == 'c'"), c(TRUE, FALSE, TRUE))
  expect_identical(run("!(k %in% c('a', 'b'))"), c(FALSE, FALSE, TRUE))
  expect_identical(run("x == -1"), c(FALSE, FALSE, FALSE))
  expect_identical(run("x <= 1"), c(TRUE, FALSE, FALSE))
})

test_that("a when condition reads results from the bus", {
  m <- lens_to_manifest("effect", TRUE, "a", "a:x",
                        metrics = list(p_value = 0.01))
  n <- node("b", when = "a.headline == 'effect' & a.metrics.p_value < 0.05",
            consumes = list(edge("a")))
  expect_true(.when_value(n, list(a = m)))
  n2 <- node("b", when = "a.metrics.missing > 1", consumes = list(edge("a")))
  expect_error(.when_value(n2, list(a = m)), "has no single value there")
  n3 <- node("b", when = "a.headline", consumes = list(edge("a")))
  expect_error(.when_value(n3, list(a = m)), "must give TRUE or FALSE")
})

# The four changes ruled after the BT1 replay of 2026-10-29: a refused node
# returns only its refusal, a unit count declared on another node, differencing
# against every earlier round, and a warning for a return with no unit.

ruling_plan <- function(fit_metrics = list(), fit_abstained = FALSE,
                        fid_metrics = list(n_trials = 12L),
                        tab = data.frame(), returns = list()) {
  oap_plan(id = "rulings", nodes = list(
    node("fidelity", planted_tool(fid_metrics, target = "structure"),
         emits = "structure"),
    node("fit", planted_tool(fit_metrics, tab = tab,
                             abstained = fit_abstained,
                             abstain_reason = "too_few_units"),
         emits = "parameters", returns = returns)))
}

# R1: a refused node --------------------------------------------------------

test_that("a refused node returns its refusal and none of its fields", {
  m <- list(share = structure(0.62, n_units = 40L),
            value = structure(118.5, n_units = 40L))
  run <- conduct(ruling_plan(m, fit_abstained = TRUE), verbose = FALSE)
  ret <- run_return(run, dose = 1,
                    requests = list(fit = c(share = 1, value = 1)))
  expect_identical(nrow(ret$values), 0L)
  expect_identical(ret$refused$field, c("share", "value"))
  expect_identical(unique(ret$refused$reason), "node_refused")
  expect_true(all(is.na(ret$refused$n_rows)))
  # the refusal itself is in the record
  rec <- ret$record[ret$record$id == "fit", ]
  expect_identical(rec$status, "refused")
  expect_false(ret$released)
  # a request above the return's dose keeps its own reason
  ret2 <- run_return(run, dose = 1,
                     requests = list(fit = c(share = 2, value = 1)))
  ref <- stats::setNames(ret2$refused$reason, ret2$refused$field)
  expect_identical(ref[["share"]], "above_dose")
  expect_identical(ref[["value"]], "node_refused")
})

test_that("a completed node's fields are unaffected by the refusal rule", {
  m <- list(share = structure(0.62, n_units = 40L))
  ret <- run_return(conduct(ruling_plan(m), verbose = FALSE), dose = 1,
                    requests = list(fit = c(share = 1)))
  expect_identical(ret$values$value, "0.62")
  expect_identical(nrow(ret$refused), 0L)
})

# R2: a unit count from another node ----------------------------------------

test_that("a unit count declared as <node>.<metric> is read from that node", {
  m <- list(slope = 0.31, gap = 0.5, tilt = 0.2)
  tab <- data.frame(estimate = c(1.2, 3.4), stringsAsFactors = FALSE)
  reqs <- data.frame(
    node = "fit",
    field = c("slope", "gap", "tilt", "params.estimate"),
    dose = 1,
    n_units_field = c("fidelity.n_trials", "fidelity.absent",
                      "screen.n_trials", "fidelity.n_trials"),
    unit = "t/ha", stringsAsFactors = FALSE)
  run <- conduct(ruling_plan(m, tab = tab), verbose = FALSE)
  ret <- run_return(run, dose = 1, requests = reqs,
                    checker = default_checker(k = 5))
  # 12 trials on the fidelity node: slope and both rows return
  expect_identical(ret$values$field,
                   c("slope", "params.estimate", "params.estimate"))
  ref <- stats::setNames(ret$refused$reason, ret$refused$field)
  # a missing metric on the named node is no count
  expect_identical(ref[["gap"]], "no_unit_count")
  # `screen` is not a node of the run, so the field is read on the node
  # itself, where it is missing: no count
  expect_identical(ref[["tilt"]], "no_unit_count")
  # too few units on the named node refuses the value
  run_few <- conduct(ruling_plan(m, tab = tab,
                                 fid_metrics = list(n_trials = 3L)),
                     verbose = FALSE)
  ret_few <- run_return(run_few, dose = 1, requests = reqs[1, ],
                        checker = default_checker(k = 5))
  expect_identical(ret_few$refused$reason, "too_few_units")
})

test_that("a unit count named on a node that did not run is no count", {
  p <- oap_plan(id = "rulings_fail", nodes = list(
    node("fit", planted_tool(list(slope = 0.31)), emits = "parameters"),
    node("fidelity", failing_tool("tool_error: no clone"),
         emits = "structure",
         consumes = list(edge("fit", "parameters")))))
  run <- try_conduct(p, verbose = FALSE)
  reqs <- data.frame(node = "fit", field = "slope", dose = 1,
                     n_units_field = "fidelity.n_trials", unit = "t/ha",
                     stringsAsFactors = FALSE)
  ret <- run_return(run, dose = 1, requests = reqs)
  expect_identical(ret$refused$reason, "no_unit_count")
})

# R3: differencing against every earlier round ------------------------------

test_that("differencing compares with every earlier round", {
  req <- list(fit = c(slope = 1))
  ret_at <- function(v, round, previous = NULL) {
    run <- conduct(ruling_plan(list(slope = structure(v, n_units = 8L))),
                   verbose = FALSE)
    run_return(run, dose = 1, round = round, requests = req,
               previous = previous, k_diff = 10,
               checker = default_checker(k = 5))
  }
  r1 <- ret_at(0.31, 1)
  # round 2 changed on 8 units: withheld
  r2 <- ret_at(0.33, 2, previous = r1)
  expect_identical(r2$refused$reason, "differencing")
  # round 3 equals round 2's withheld value: compared with round 2 alone it
  # would pass, since round 2 returned nothing; compared with round 1 it
  # changed, so it stays withheld
  r3_last <- ret_at(0.33, 3, previous = r2)
  expect_identical(nrow(r3_last$refused), 0L)
  r3_all <- ret_at(0.33, 3, previous = list(r1, r2))
  expect_identical(r3_all$refused$reason, "differencing")
  # unchanged since every earlier round: returned
  r3_same <- ret_at(0.31, 3, previous = list(r1, r2))
  expect_identical(r3_same$values$value, "0.31")
})

test_that("earlier rounds may be given as written files", {
  dir <- tempfile("cumulative")
  on.exit(unlink(dir, recursive = TRUE))
  req <- list(fit = c(slope = 1))
  ret_at <- function(v, round, previous = NULL) {
    run <- conduct(ruling_plan(list(slope = structure(v, n_units = 8L))),
                   verbose = FALSE)
    run_return(run, dose = 1, series = "c", round = round, requests = req,
               previous = previous, k_diff = 10)
  }
  f1 <- write_return(ret_at(0.31, 1), dir)
  f2 <- write_return(ret_at(0.33, 2, previous = f1), dir)
  r3 <- ret_at(0.33, 3, previous = c(f1, f2))
  expect_identical(r3$refused$reason, "differencing")
  # no earlier rounds at all: nothing to compare with
  r0 <- ret_at(0.33, 1, previous = list())
  expect_identical(r0$values$value, "0.33")
  expect_error(ret_at(0.33, 3, previous = list(1)), "previous")
})

test_that("params rows are differenced against every earlier round", {
  tab_at <- function(e2) {
    data.frame(estimate = c(1.25, e2), n_units = c(8L, 8L),
               stringsAsFactors = FALSE)
  }
  req <- list(fit = c("params.estimate" = 1))
  ret_at <- function(e2, round, previous = NULL) {
    run <- conduct(ruling_plan(tab = tab_at(e2)), verbose = FALSE)
    run_return(run, dose = 1, round = round, requests = req,
               previous = previous, k_diff = 10)
  }
  r1 <- ret_at(0.43, 1)
  r2 <- ret_at(0.47, 2, previous = r1)
  expect_identical(r2$values$index, 1L)
  r3 <- ret_at(0.47, 3, previous = list(r1, r2))
  expect_identical(r3$values$index, 1L)
  expect_identical(r3$refused$n_rows, 1L)
})

# R5: a return with no unit -------------------------------------------------

test_that("plan_returns warns on a return with no declared unit", {
  p <- ruling_plan(returns = list(
    list(field = "slope", dose = 1, unit = "t/ha"),
    list(field = "gap", dose = 1)))
  expect_warning(r <- plan_returns(p), "no declared unit: fit.gap")
  expect_identical(r$unit, c("t/ha", NA))
  p_ok <- ruling_plan(returns = list(
    list(field = "slope", dose = 1, unit = "t/ha")))
  expect_no_warning(plan_returns(p_ok))
  expect_no_warning(plan_returns(ruling_plan()))
})

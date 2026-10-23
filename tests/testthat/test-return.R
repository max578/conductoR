# The return path: the dose-0 record, declared signals, checked values at a
# dose, written returns and the series report.

# run_record and format_record ------------------------------------------------

test_that("a clean run records every node as completed and says stop", {
  p <- oap_plan(id = "clean", nodes = list(
    node("a", effect_lens, emits = "treatment_effects"),
    node("b", effect_lens, emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects")))))
  rec <- run_record(conduct(p, verbose = FALSE))
  expect_identical(names(rec), c("kind", "id", "emitter", "emits", "status",
                                 "reason", "abstained", "checks_passed",
                                 "checks_failed"))
  expect_identical(rec$id, c("a", "b"))
  expect_identical(rec$status, c("completed", "completed"))
  expect_identical(format_record(rec), "stop")
})

test_that("the planted run gives the W2 round-1 record and message", {
  run <- conduct(planted_plan(), verbose = FALSE)
  rec <- run_record(run, planted_triggers)
  expect_identical(rec$kind, c(rep("node", 4), rep("trigger", 4)))
  expect_identical(rec$id, c("fit", "screen", "quorum", "pooled",
                             planted_triggers$trigger_id))
  expect_identical(rec$status[1:4], rep("completed", 4))
  expect_identical(rec$reason[1:4], c(NA, NA, "assumption_check_failed", NA))
  expect_identical(rec$status[5:8], c("silent", "fired", "fired", "fired"))
  expect_identical(rec$emits[5:8], planted_triggers$action)
  expect_identical(rec$reason[7], "signal:lens_spread")
  expect_identical(format_record(rec), c("quorum assumption_check_failed",
                                         "T_dep signal:clone_fidelity"))
})

test_that("the record carries no number, headline or free text", {
  run <- conduct(planted_plan(), verbose = FALSE)
  rec <- run_record(run, planted_triggers)
  expect_false(any(vapply(rec, is.numeric, logical(1))))
  txt <- paste(unlist(rec), collapse = " ")
  for (v in c("0.318", "0.42", "1.6", "0.004", "planted_headline", "S1",
              "rehearsal")) {
    expect_false(grepl(v, txt, fixed = TRUE), info = v)
  }
})

test_that("an issue outside the alphabet is recorded as tool_error", {
  p <- oap_plan(id = "odd", nodes = list(
    node("a", planted_tool(list(issue = "something odd")),
         emits = "parameters")))
  rec <- run_record(conduct(p, verbose = FALSE))
  expect_identical(rec$reason, "tool_error")
  expect_identical(format_record(rec), "a tool_error")
})

test_that("a refusal reports its reason only when it is an alphabet word", {
  p <- oap_plan(id = "refusals", nodes = list(
    node("typed", planted_tool(abstained = TRUE,
                               abstain_reason = "too_few_units"),
         emits = "parameters"),
    node("plain", planted_tool(abstained = TRUE,
                               abstain_reason = "too few sites"),
         emits = "parameters")))
  rec <- run_record(conduct(p, verbose = FALSE))
  expect_identical(rec$status, c("refused", "refused"))
  expect_identical(rec$abstained, c(TRUE, TRUE))
  expect_identical(rec$reason, c("too_few_units", NA))
  expect_identical(format_record(rec), "typed too_few_units")
})

test_that("skipped nodes are recorded without emitter or reason", {
  p <- oap_plan(id = "skip", nodes = list(
    node("a", no_effect_lens, emits = "treatment_effects"),
    node("b", effect_lens, emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects")),
         when = "a.headline == 'effect'")))
  rec <- run_record(conduct(p, verbose = FALSE))
  expect_identical(rec$status, c("completed", "skipped"))
  expect_true(is.na(rec$emitter[2]))
  expect_true(is.na(rec$reason[2]))
  expect_identical(format_record(rec), "stop")
})

test_that("issues of foreach instances belong to their node", {
  tool <- function(inputs, data, node) {
    it <- node@params$item
    planted_tool(if (it == "S2") list(issue = "model_not_converged")
                 else list())(inputs, data, node)
  }
  p <- oap_plan(id = "fe", nodes = list(
    node("fit", tool, emits = "parameters", foreach = c("S1", "S2"))))
  rec <- run_record(conduct(p, verbose = FALSE))
  expect_identical(rec$reason, "model_not_converged")
  expect_identical(format_record(rec), "fit model_not_converged")
})

test_that("declared checks are recorded as passed or failed", {
  own <- planted_tool(list(checks = c(levels_match = TRUE, no_gaps = FALSE),
                           converged = TRUE, mixing = "poor"))
  p <- oap_plan(id = "chk", nodes = list(node("a", own,
                                              emits = "parameters")))
  rec <- run_record(conduct(p, verbose = FALSE),
                    checks = list(a = c("converged", "mixing", "absent")))
  expect_identical(rec$checks_passed, "levels_match;converged")
  expect_identical(rec$checks_failed, "no_gaps;mixing;absent")
  expect_identical(format_record(rec), "stop")
})

test_that("an empty run gives an empty record and stop", {
  rec <- run_record(list(provenance = list(), bus = list()))
  expect_identical(nrow(rec), 0L)
  expect_identical(format_record(rec), "stop")
})

# try_conduct ---------------------------------------------------------------

test_that("try_conduct returns the run itself when nothing fails", {
  p <- planted_plan()
  a <- conduct(p, verbose = FALSE)
  b <- try_conduct(p, verbose = FALSE)
  expect_identical(names(b), names(a))
  expect_null(b$failed)
  expect_identical(b$plan_hash, a$plan_hash)
})

test_that("a node error becomes a failed run with a typed word", {
  p <- oap_plan(id = "fails", nodes = list(
    node("fit", planted_tool(list(n = 1L)), emits = "parameters"),
    node("shape", failing_tool("missing_block: site S3 has no plots"),
         emits = "structure", consumes = list(edge("fit", "parameters"))),
    node("pooled", effect_lens, emits = "treatment_effects",
         consumes = list(edge("shape", "structure")))))
  expect_error(conduct(p, verbose = FALSE), "missing_block")
  r <- try_conduct(p, verbose = FALSE, label = "real")
  expect_identical(r$failed$node, "shape")
  expect_identical(r$failed$error_type, "missing_block")
  expect_identical(r$not_run, "pooled")
  expect_identical(r$label, "real")
  expect_identical(r$plan_hash, conductoR:::.plan_hash(p))
  rec <- run_record(r)
  expect_identical(rec$status, c("completed", "failed", "not_run"))
  expect_identical(format_record(rec), "shape missing_block")
  ret <- run_return(r)
  expect_false(any(grepl("site S3", format(ret), fixed = TRUE)))
})

test_that("error words come from the class, the walker or tool_error", {
  by_class <- oap_plan(id = "c", nodes = list(
    node("a", failing_tool("no convergence", "model_not_converged"),
         emits = "parameters")))
  expect_identical(try_conduct(by_class, verbose = FALSE)$failed$error_type,
                   "model_not_converged")
  plain <- oap_plan(id = "p", nodes = list(
    node("a", failing_tool("subscript out of bounds"),
         emits = "parameters")))
  expect_identical(try_conduct(plain, verbose = FALSE)$failed$error_type,
                   "tool_error")
  edge_bad <- oap_plan(id = "e", nodes = list(
    node("a", effect_lens, emits = "treatment_effects"),
    node("b", effect_lens, emits = "treatment_effects",
         consumes = list(edge("a", "parameters")))))
  r <- try_conduct(edge_bad, verbose = FALSE)
  expect_identical(r$failed$node, "b")
  expect_identical(r$failed$error_type, "typed_edge_violation")
  expect_identical(format_record(run_record(r)), "b typed_edge_violation")
})

# evaluate_triggers ---------------------------------------------------------

test_that("each direction compares the statistic with its threshold", {
  p <- oap_plan(id = "d", nodes = list(
    node("a", planted_tool(list(x = 2)), emits = "parameters")))
  run <- conduct(p, verbose = FALSE)
  trig <- data.frame(trigger_id = paste0("T", 1:5), node = "a",
                     class = "anomaly", signal_type = "heterogeneity",
                     statistic = "x", direction = c(">", ">=", "<", "<=",
                                                    "=="),
                     threshold = 2, action = "return")
  ev <- evaluate_triggers(run, trig)
  expect_identical(ev$fired, c(FALSE, TRUE, FALSE, TRUE, TRUE))
  expect_identical(ev$value, rep(2, 5))
  expect_identical(ev$status, rep("evaluated", 5))
  expect_identical(ev$signal_type, rep("signal:heterogeneity", 5))
  expect_true(is.na(ev$rehearsal_plant[1]))
})

test_that("a trigger on a skipped node or a missing statistic is not fired", {
  p <- oap_plan(id = "m", nodes = list(
    node("a", no_effect_lens, emits = "treatment_effects"),
    node("b", planted_tool(list(x = 1)), emits = "parameters",
         consumes = list(edge("a", "treatment_effects")),
         when = "a.headline == 'effect'")))
  run <- conduct(p, verbose = FALSE)
  trig <- data.frame(trigger_id = c("T1", "T2"), node = c("b", "a"),
                     class = "anomaly", signal_type = "interaction",
                     statistic = c("x", "absent"), direction = ">",
                     threshold = 0, action = "return")
  ev <- evaluate_triggers(run, trig)
  expect_identical(ev$status, c("node_not_run", "statistic_missing"))
  expect_identical(ev$fired, c(NA, NA))
  rec <- run_record(run, ev)
  expect_identical(rec$status[3:4], c("not_evaluated", "not_evaluated"))
  # a promised statistic that is missing is an issue of its node
  expect_identical(format_record(rec), "a tool_error")
})

test_that("an invalid trigger table is refused with every problem named", {
  run <- conduct(planted_plan(), verbose = FALSE)
  expect_error(evaluate_triggers(run, planted_triggers[, -3]),
               "lacks these columns: class")
  bad <- planted_triggers
  bad$direction[1] <- "=>"
  bad$node[2] <- "nowhere"
  bad$action[3] <- "email"
  bad$threshold[4] <- "high"
  err <- tryCatch(evaluate_triggers(run, bad), error = conditionMessage)
  expect_match(err, "direction must be", fixed = TRUE)
  expect_match(err, "node is not a node of the run", fixed = TRUE)
  expect_match(err, "action must be", fixed = TRUE)
  expect_match(err, "threshold must be a number", fixed = TRUE)
  expect_error(evaluate_triggers(run, list(a = 1)), "must be a data frame")
})

# default_checker -----------------------------------------------------------

test_that("the default checker refuses few units, a dominant unit, no count", {
  chk <- default_checker(k = 5, dominance = 0.6)
  expect_identical(chk(structure(0.5, n_units = 5L)), NA_character_)
  expect_identical(chk(structure(0.5, n_units = 4L)), "too_few_units")
  expect_identical(chk(structure(0.5, n_units = 9L, max_unit_share = 0.7)),
                   "dominant_unit")
  expect_identical(chk(structure(0.5, n_units = 9L, max_unit_share = NA)),
                   NA_character_)
  expect_identical(chk(0.5), "no_unit_count")
  expect_identical(chk(structure(0.5, n_units = NA)), "no_unit_count")
  expect_identical(chk(study_tab[, c("estimate", "n_units")]), NA_character_)
  expect_identical(default_checker(k = 7)(study_tab), "too_few_units")
  expect_identical(default_checker(dominance = 0.35)(study_tab),
                   "dominant_unit")
  expect_error(default_checker(k = 0))
  expect_error(default_checker(dominance = 1.5))
})

# run_return ----------------------------------------------------------------

requests_w2 <- list(
  fit = c(slope = 1, dep_share = 1, "params.estimate" = 1,
          "params.unit" = 2),
  quorum = c("metrics.width_ratio" = 1, absent = 1))

test_that("dose 0 returns the record and withholds every request", {
  run <- conduct(planted_plan(), verbose = FALSE)
  ret <- run_return(run, planted_triggers, dose = 0, requests = requests_w2,
                    series = "W2", round = 1)
  expect_s3_class(ret, "conductoR_return")
  expect_identical(ret$lines, c("quorum assumption_check_failed",
                                "T_dep signal:clone_fidelity"))
  expect_identical(nrow(ret$values), 0L)
  expect_identical(ret$refused$reason, rep("above_dose", 6))
  expect_false(ret$released)
  expect_identical(ret$plan_hash, run$plan_hash)
  expect_identical(ret$input_hash, run$input_hash)
  txt <- paste(format(ret), collapse = "\n")
  for (v in c("0.318", "0.42", "1.6", "1.25")) {
    expect_false(grepl(v, txt, fixed = TRUE), info = v)
  }
})

test_that("dose 1 returns only checked values up to the dose", {
  run <- conduct(planted_plan(), verbose = FALSE)
  ret <- run_return(run, planted_triggers, dose = 1, requests = requests_w2)
  expect_true(ret$released)
  expect_identical(unique(ret$values$field), c("slope", "params.estimate"))
  expect_identical(ret$values$value, c("0.318", "1.25", "0.43", "2.71"))
  expect_identical(ret$values$index, c(1L, 1L, 2L, 3L))
  ref <- stats::setNames(ret$refused$reason, ret$refused$field)
  expect_identical(ref[["dep_share"]], "no_unit_count")
  expect_identical(ref[["params.unit"]], "above_dose")
  expect_identical(ref[["metrics.width_ratio"]], "no_unit_count")
  expect_identical(ref[["absent"]], "not_available")
  # the trigger value stays at the holder
  expect_false("value" %in% names(ret$record))
})

test_that("a custom checker decides, and a malformed one is an error", {
  run <- conduct(planted_plan(), verbose = FALSE)
  allow <- function(value) NA_character_
  ret <- run_return(run, dose = 1, requests = requests_w2, checker = allow)
  expect_true("dep_share" %in% ret$values$field)
  expect_error(run_return(run, dose = 1, requests = requests_w2,
                          checker = function(value) TRUE),
               "checker must return")
})

test_that("requests are validated against the run", {
  run <- conduct(planted_plan(), verbose = FALSE)
  expect_error(run_return(run, requests = list(nowhere = c(x = 1))),
               "not in the run: nowhere")
  expect_error(run_return(run, requests = list(fit = c(slope = 0))),
               "whole number of at least 1")
  expect_error(run_return(run, requests = list(fit = 1)),
               "named vector of doses")
  expect_error(run_return(run, dose = 1.5))
  expect_error(run_return(run, round = 0))
})

test_that("printing a return shows the text that is written", {
  run <- conduct(planted_plan(), verbose = FALSE)
  ret <- run_return(run, series = "W2", round = 1)
  out <- utils::capture.output(print(ret))
  expect_identical(out, format(ret))
  expect_identical(out[c(1, 5:7)], c("conductoR return", "series: W2",
                                     "round: 1", "dose: 0"))
})

# write_return and series_report --------------------------------------------

test_that("returns are written by round and never replaced", {
  run <- conduct(planted_plan(), verbose = FALSE)
  dir <- withr_tempfile()
  f <- write_return(run_return(run, series = "W2", round = 1), dir)
  expect_identical(f, file.path(dir, "round_1", "return.txt"))
  expect_identical(readLines(f)[c(1, 3)],
                   c("conductoR return",
                     paste0("plan_hash: ", run$plan_hash)))
  expect_error(write_return(run_return(run, round = 1), dir),
               "already exists")
  expect_error(write_return(run_return(run), dir), "needs a round")
  g <- file.path(dir, "loose.txt")
  expect_identical(write_return(run_return(run), g), g)
})

test_that("the series report lists every round and counts the budget", {
  run <- conduct(planted_plan(), verbose = FALSE)
  dir <- withr_tempfile()
  write_return(run_return(run, planted_triggers, series = "W2", round = 1),
               dir)
  write_return(run_return(run, dose = 1, requests = requests_w2,
                          series = "W2", round = 2), dir)
  write_return(run_return(run, dose = 1, requests = list(fit = c(rise = 1)),
                          series = "W2", round = 10), dir)
  dir.create(file.path(dir, "round_3"))
  rep <- series_report(dir, budget = 1)
  expect_s3_class(rep, "conductoR_series")
  expect_identical(rep$round, c(1L, 2L, 3L, 10L))
  expect_identical(rep$dose, c(0L, 1L, NA, 1L))
  expect_identical(rep$released, c(FALSE, TRUE, FALSE, FALSE))
  expect_identical(rep$n_values, c(0L, 4L, 0L, 0L))
  expect_identical(rep$lines[1], paste("quorum assumption_check_failed",
                                       "T_dep signal:clone_fidelity",
                                       sep = " | "))
  expect_identical(rep$plan_hash[c(1, 2, 4)], rep(run$plan_hash, 3))
  expect_identical(attr(rep, "n_above_dose_0"), 2L)
  expect_false(attr(rep, "within_budget"))
  expect_output(print(rep), "Returns above dose 0: 2 of a budget of 1")
  expect_output(print(series_report(dir)), "Returns above dose 0: 2$")
  expect_true(is.na(attr(series_report(dir), "within_budget")))
})

test_that("an empty series folder gives an empty report", {
  dir <- withr_tempfile()
  dir.create(dir)
  rep <- series_report(dir)
  expect_identical(nrow(rep), 0L)
  expect_identical(attr(rep, "n_above_dose_0"), 0L)
})

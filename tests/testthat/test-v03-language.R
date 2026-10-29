# Plan language 0.3 (Line D) and the postcondition gate (Line E) of the 0.3.0
# plan: `inputs:` with `postconditions:`, node `returns:`, `triggers:` and
# `hypotheses:`, schema v0.3, round trips and hash stability.

# A window table: 48 trials, each 30 days long; `n` of them doubled, as in
# the BT1 moisture step (need N36).
window_table <- function(n_doubled = 0L) {
  tab <- data.frame(trial_id = sprintf("t%02d", 1:48),
                    state = rep(c("north", "south"), 24),
                    window_days = rep(30, 48), stringsAsFactors = FALSE)
  tab$window_days[seq_len(n_doubled)] <- 60
  tab
}

window_input <- function() {
  plan_input("trials", key = "trial_id",
             postconditions = c(P_rows = ".n_rows == 48",
                                P_days = "window_days <= 31"))
}

# A plan whose one tool records that it ran.
ran_env <- new.env()
counted_plan <- function(inputs = list(window_input())) {
  ran_env$n <- 0L
  tool <- function(inputs, data, node) {
    ran_env$n <- ran_env$n + 1L
    lens_to_manifest("effect", TRUE, "demo", "demo:fit",
                     metrics = list(slope = structure(0.3, n_units = 48L),
                                    spread = 1.6))
  }
  oap_plan(id = "window", inputs = inputs,
           nodes = list(node("fit", tool, emits = "treatment_effects")))
}

test_that("N36 regression: a doubled window stops before node 1, typed", {
  p <- counted_plan()
  err <- tryCatch(conduct(p, list(trials = window_table(4L)), verbose = FALSE),
                  error = function(e) e)
  expect_s3_class(err, "postcondition_failed")
  expect_match(conditionMessage(err), "^postcondition_failed:")
  expect_match(conditionMessage(err),
               "trials.P_days `window_days <= 31`: 4 of 48 rows fail",
               fixed = TRUE)
  expect_identical(err$failed, "trials.P_days")
  expect_identical(ran_env$n, 0L)

  run <- try_conduct(p, list(trials = window_table(4L)), verbose = FALSE)
  expect_identical(run$failed$error_type, "postcondition_failed")
  expect_identical(run$not_run, "fit")
  rec <- run_record(run)
  expect_identical(rec$kind[rec$status == "failed"], "input")
  expect_identical(format_record(rec), "trials.P_days postcondition_failed")
  ret <- run_return(run, dose = 1, requests = list(fit = c(slope = 1)))
  expect_false(ret$released)
  expect_identical(ret$lines, "trials.P_days postcondition_failed")
})

test_that("a table that meets its postconditions runs, and the run says so", {
  p <- counted_plan()
  run <- conduct(p, list(trials = window_table()), verbose = FALSE)
  expect_identical(ran_env$n, 1L)
  expect_identical(run$postconditions$id, c("key", "P_rows", "P_days"))
  expect_true(all(run$postconditions$holds))
  expect_identical(format_record(run_record(run)), "stop")
})

test_that("a plan with no inputs runs as before and records none", {
  p <- counted_plan(inputs = list())
  run <- conduct(p, verbose = FALSE)
  expect_identical(nrow(run$postconditions), 0L)
  expect_identical(ran_env$n, 1L)
})

test_that("every failure is listed at once, and none names a data value", {
  tab <- window_table(4L)
  tab$trial_id[48] <- tab$trial_id[1]
  tab$window_days[10] <- 99999
  p <- counted_plan()
  err <- tryCatch(conduct(p, list(trials = tab), verbose = FALSE),
                  error = function(e) e)
  expect_setequal(err$failed, c("trials.key", "trials.P_days"))
  expect_match(conditionMessage(err), "2 postconditions are not met")
  expect_match(conditionMessage(err), "1 rows repeat a key", fixed = TRUE)
  expect_match(conditionMessage(err), "5 of 48 rows fail", fixed = TRUE)
  expect_false(grepl("99999", conditionMessage(err), fixed = TRUE))
  expect_false(grepl("t01", conditionMessage(err), fixed = TRUE))
  rec <- run_record(try_conduct(p, list(trials = tab), verbose = FALSE))
  expect_setequal(rec$id[rec$kind == "input"], c("trials.key", "trials.P_days"))
})

test_that("an absent input, a non-table input or an absent column fails", {
  p <- counted_plan()
  e1 <- tryCatch(conduct(p, list(other = window_table()), verbose = FALSE),
                 error = function(e) e)
  expect_match(conditionMessage(e1), "the input is not in data")
  e2 <- tryCatch(conduct(p, list(trials = 1:3), verbose = FALSE),
                 error = function(e) e)
  expect_match(conditionMessage(e2), "the input is not a table")
  e3 <- tryCatch(conduct(p, list(trials = window_table()[, 1:2]),
                         verbose = FALSE), error = function(e) e)
  expect_match(conditionMessage(e3), "no column 'window_days'", fixed = TRUE)
  expect_identical(ran_env$n, 0L)
})

test_that("a data frame passed as data is the input named data", {
  p <- counted_plan(inputs = list(plan_input("data", c(P = ".n_rows == 48"))))
  expect_no_error(conduct(p, window_table(), verbose = FALSE))
  p2 <- counted_plan()
  expect_error(conduct(p2, window_table(), verbose = FALSE),
               class = "postcondition_failed")
})

test_that("the table summaries and row checks give what the rules say", {
  tab <- data.frame(id = c("a", "b", "c", "d"), grp = c("x", "y", "x", NA),
                    v = c(1, 5, NA, 3), stringsAsFactors = FALSE)
  holds <- function(cond) {
    tryCatch(isTRUE(check_postconditions(tab, c(P = cond))$holds),
             postcondition_failed = function(e) FALSE)
  }
  expect_true(holds(".n_rows == 4 & .n_cols == 3"))
  expect_true(holds(".n_missing.grp == 1 & .n_missing.v == 1"))
  expect_true(holds(".n_distinct.grp == 2"))
  expect_true(holds(".n_duplicated.grp == 1"))
  expect_true(holds(".min.v == 1 & .max.v == 5"))
  expect_true(holds(".levels.grp %in% c(\"x\", \"y\")"))
  expect_false(holds(".levels.grp %in% c(\"x\")"))
  expect_false(holds("v >= 1"))
  expect_true(holds("id %in% c(\"a\", \"b\", \"c\", \"d\")"))
  expect_false(holds(".n_rows == 5"))
})

test_that("a row check that gives NA fails, and says how many", {
  tab <- data.frame(v = c(1, NA, 3, NA))
  err <- tryCatch(check_postconditions(tab, c(P_v = "v >= 0"), input = "t"),
                  error = function(e) e)
  expect_s3_class(err, "postcondition_failed")
  expect_match(conditionMessage(err), "2 of 4 rows fail (2 of them NA)",
               fixed = TRUE)
})

test_that("the key check refuses repeats and missing keys", {
  tab <- data.frame(a = c(1, 1, 2, NA), b = c("x", "x", "y", "z"))
  err <- tryCatch(check_postconditions(tab, character(0), key = c("a", "b")),
                  error = function(e) e)
  expect_match(conditionMessage(err), "1 rows have a missing key", fixed = TRUE)
  tab2 <- tab[1:3, ]
  err2 <- tryCatch(check_postconditions(tab2, character(0), key = c("a", "b")),
                   error = function(e) e)
  expect_match(conditionMessage(err2), "1 rows repeat a key", fixed = TRUE)
  res <- check_postconditions(tab2[2:3, ], character(0), key = c("a", "b"))
  expect_identical(res$id, "key")
  expect_true(res$holds)
})

test_that("check_postconditions() reads a plan's declared input", {
  p <- counted_plan()
  res <- check_postconditions(window_table(), p, input = "trials")
  expect_true(all(res$holds))
  expect_error(check_postconditions(window_table(2L), p, input = "trials"),
               class = "postcondition_failed")
  expect_error(check_postconditions(window_table(), p, input = "moisture"),
               "declares no input 'moisture'")
})

test_that("postconditions outside the grammar are refused when declared", {
  expect_error(check_postconditions(window_table(), c(P = "nrow(trials) > 0")),
               "`nrow` is not allowed in a condition", fixed = TRUE)
  expect_error(check_postconditions(window_table(),
                                    c(P = ".mean.window_days > 0")),
               "is not a table summary")
  expect_error(oap_plan(id = "x",
                        inputs = list(plan_input("t", c(key = ".n_rows > 0"))),
                        nodes = list(node("a", tool = "t"))),
               "`key` is the id of the input's key check")
  expect_error(oap_plan(id = "x", inputs = list(
    plan_input("t", c(P = ".n_rows > 0", P = ".n_cols > 0"))),
    nodes = list(node("a", tool = "t"))), "is used twice")
  expect_error(oap_plan(id = "x", inputs = list(plan_input("t")),
                        nodes = list(node("a", tool = "t"))),
               "declares nothing to check")
  probs <- plan_validate(paste0(
    "id: p\nplan_version: \"0.3\"\ninputs:\n- name: \"t\"\n",
    "  postconditions:\n  - id: \"P\"\n    condition: \"system(1) == 0\"\n",
    "nodes:\n- id: a\n  tool: t\n"))
  expect_identical(probs$field, "inputs[1].postconditions[1].condition")
})

test_that("returns, triggers and hypotheses are checked on the node", {
  expect_error(node("a", tool = "t", returns = c(slope = 0)),
               "must be a whole number of at least 1")
  expect_error(node("a", tool = "t", returns = list(list(field = "s", dose = 1,
                                                          shape = "x"))),
               "is not a field of returns")
  expect_error(node("a", tool = "t", triggers = list(list(trigger_id = "T"))),
               "is required")
  expect_error(node("a", tool = "t", hypotheses = data.frame(
    hypothesis_id = "H", statistic = "s", direction = ">", threshold = 0,
    firing_plant = "planted", silent_plant = "")),
    "must describe the plant in rehearsal")
  trig <- data.frame(trigger_id = "T_1", class = "anomaly", signal_type = "x",
                     statistic = "s", direction = ">", threshold = 1,
                     action = "return")
  expect_error(oap_plan(id = "x", nodes = list(
    node("a", tool = "t", triggers = trig),
    node("b", tool = "t", triggers = trig))),
    "trigger_id 'T_1' is used twice in the plan")
})

test_that("the declarations drive run_return() and evaluate_triggers()", {
  tool <- function(inputs, data, node) {
    lens_to_manifest("effect", TRUE, "demo", "demo:fit",
                     metrics = list(slope = structure(0.3, n_units = 48L),
                                    spread = 1.6))
  }
  p <- oap_plan(id = "declared", nodes = list(
    node("fit", tool, emits = "treatment_effects",
         returns = c(slope = 1, spread = 2),
         triggers = data.frame(trigger_id = "T_spread", class = "anomaly",
                               signal_type = "lens_spread",
                               statistic = "spread", direction = ">",
                               threshold = 1.3, action = "return"),
         hypotheses = data.frame(hypothesis_id = "H_slope", statistic = "slope",
                                 direction = ">", threshold = 0,
                                 firing_plant = "slope 0.3 planted",
                                 silent_plant = "flat planted"))))
  expect_identical(plan_requests(p), list(fit = c(slope = 1, spread = 2)))
  trig <- plan_triggers(p)
  expect_identical(names(trig), c(.TRIGGER_COLS, "rehearsal_plant"))
  expect_identical(trig$node, "fit")
  hyp <- plan_hypotheses(p)
  expect_identical(hyp$hypothesis_id, "H_slope")
  expect_identical(hyp$threshold, 0)
  run <- conduct(p, verbose = FALSE)
  expect_true(evaluate_triggers(run, trig)$fired)
  ret <- run_return(run, triggers = trig, dose = 1, requests = plan_requests(p))
  expect_identical(ret$lines, "T_spread signal:lens_spread")
  expect_identical(ret$values$field, "slope")
  expect_identical(ret$refused$reason, "above_dose")
})

test_that("a plan that declares nothing gives empty declarations", {
  p <- oap_plan(id = "none", nodes = list(node("a", tool = "t")))
  expect_identical(plan_requests(p), list())
  expect_identical(nrow(plan_triggers(p)), 0L)
  expect_identical(names(plan_hypotheses(p)), c("node", .HYPOTHESIS_FIELDS))
})

test_that("a 0.3 plan reads back from YAML and JSON with the same hash", {
  register_tool("t_a", effect_lens)
  register_tool("t_b", effect_lens)
  for (f in c("21_inputs_postconditions", "22_returns_triggers",
              "23_hypotheses")) {
    path <- test_path("fixtures", "corpus", "good", paste0(f, ".json"))
    p <- plan_from_json(path)
    txt <- plan_to_yaml(p)
    expect_match(txt, "plan_version: \"0.3\"", fixed = TRUE)
    q <- plan_from_yaml(txt)
    expect_identical(.plan_hash(q), .plan_hash(p), label = f)
    expect_identical(plan_to_yaml(q), txt, label = f)
  }
})

test_that("a plan built in R hashes as its file does", {
  register_tool("t_a", effect_lens)
  path <- test_path("fixtures", "corpus", "good", "22_returns_triggers.json")
  from_file <- plan_from_json(path)
  in_r <- oap_plan(id = "returns_triggers", nodes = list(
    node("a", tool = "t_a", emits = "treatment_effects",
         returns = list(list(n_units_field = "n_trials", field = "slope",
                             unit = "kg/ha per kg N", dose = 1),
                        list(field = "params.estimate", dose = 2L)),
         triggers = data.frame(trigger_id = "T_spread", class = "anomaly",
                               signal_type = "lens_spread",
                               statistic = "spread", direction = ">",
                               threshold = 1.3, action = "return",
                               rehearsal_plant =
                                 "spread of 1.6 planted in one state"))))
  expect_identical(.plan_hash(in_r), .plan_hash(from_file))
})

test_that("each 0.3 declaration enters the hash", {
  register_tool("t_a", effect_lens)
  base <- oap_plan(id = "h", nodes = list(node("a", tool = "t_a")))
  with_input <- oap_plan(id = "h", inputs = list(window_input()),
                         nodes = list(node("a", tool = "t_a")))
  changed <- plan_input("trials", key = "trial_id",
                        postconditions = c(P_rows = ".n_rows == 47",
                                           P_days = "window_days <= 31"))
  other_input <- oap_plan(id = "h", inputs = list(changed),
                          nodes = list(node("a", tool = "t_a")))
  with_return <- oap_plan(id = "h", nodes = list(
    node("a", tool = "t_a", returns = c(slope = 1))))
  hashes <- vapply(list(base, with_input, other_input, with_return),
                   .plan_hash, character(1))
  expect_identical(anyDuplicated(hashes), 0L)
  expect_false(grepl("plan_version", plan_to_yaml(base), fixed = TRUE))
})

test_that("every 0.2 plan in the package hashes as recorded before 0.3", {
  base <- utils::read.csv(test_path("fixtures", "plan_hashes_0.2.csv"),
                          stringsAsFactors = FALSE)
  stand_in <- as.function(alist(inputs = , data = , node = , NULL))
  for (i in seq_len(nrow(base))) {
    f <- system.file("scores", base$file[i], package = "conductoR")
    if (!nzchar(f)) f <- test_path("fixtures", base$file[i])
    p <- if (grepl("\\.json$", f)) plan_from_json(f) else plan_from_yaml(f)
    for (n in p@nodes) {
      if (nzchar(n@tool_ref)) register_tool(n@tool_ref, stand_in)
    }
    expect_identical(.plan_hash(p), base$plan_hash[i], label = base$file[i])
  }
})

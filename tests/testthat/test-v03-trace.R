# trace_report(): every number in a report traced to the release.

trace_run <- function() {
  tab <- data.frame(estimate = c(1.25, 0.43), n_units = c(8L, 9L),
                    stringsAsFactors = FALSE)
  conduct(oap_plan(id = "trace", nodes = list(
    node("fit", planted_tool(
      list(slope = structure(0.3127, n_units = 24L),
           thin = structure(0.9, n_units = 2L),
           total = structure(1234.5678, n_units = 24L)), tab = tab),
      emits = "parameters"))), verbose = FALSE)
}

trace_ret <- function(run = trace_run()) {
  run_return(run, dose = 1, requests = list(
    fit = c(slope = 1, thin = 1, total = 1, "params.estimate" = 1)))
}

test_that("references are filled from the release and traced", {
  run <- trace_run()
  tr <- trace_report(c("Slope {{fit:slope}};", "row two {{fit:params.estimate:2}}."),
                     trace_ret(run), run = run)
  expect_true(tr$released)
  expect_identical(tr$text, "Slope 0.3127;\nrow two 0.43.")
  expect_identical(tr$trace$status, c("filled", "filled"))
  expect_identical(tr$trace$line, c(1L, 2L))
  expect_identical(tr$trace$index, c(1L, 2L))
  expect_identical(unique(tr$trace$emitter), "planted")
  expect_identical(unique(tr$trace$emitter_version), "0.0.0")
})

test_that("a number written out is matched at the digits shown", {
  ret <- trace_ret()
  tr <- trace_report("Slope 0.31, total 1,234.6 and 1235.", ret)
  expect_true(tr$released)
  expect_identical(tr$trace$status, rep("matched", 3))
  expect_identical(tr$trace$field, c("slope", "total", "total"))
  # 0.32 is not the slope at two decimals
  tr2 <- trace_report("Slope 0.32.", ret)
  expect_false(tr2$released)
  expect_identical(tr2$trace$status, "untraced")
  expect_identical(tr2$trace$reason, "no_match")
  expect_true(is.na(tr2$text))
})

test_that("declared numbers pass and identifiers are not numbers", {
  ret <- trace_ret()
  tr <- trace_report("In H1 (v0.3, round_2) the 95% interval, 2008-2017.",
                     ret, allow = c(95, 2008, 2017))
  expect_true(tr$released)
  expect_identical(tr$trace$shown, c("95", "2008", "2017"))
  expect_identical(unique(tr$trace$status), "declared")
  expect_false(trace_report("the 95% interval", ret)$released)
  expect_error(trace_report("x", ret, allow = "ninety"), "allow")
})

test_that("a reference the release does not hold blocks it, with the reason", {
  ret <- trace_ret()
  tr <- trace_report(c("{{fit:thin}}", "{{fit:absent}}",
                       "{{fit:params.estimate}}", "{{fit:params.estimate:7}}"),
                     ret)
  expect_false(tr$released)
  expect_identical(tr$trace$reason,
                   c("too_few_units", "not_in_return", "index_needed",
                     "not_in_return"))
  expect_true(is.na(tr$text))
  expect_match(format(tr), "\\[blocked\\]", all = FALSE)
})

test_that("a dose-0 release holds no number to enter", {
  ret0 <- run_return(trace_run(), dose = 0,
                     requests = list(fit = c(slope = 1)))
  tr <- trace_report("Slope {{fit:slope}}.", ret0)
  expect_false(tr$released)
  expect_identical(tr$trace$reason, "above_dose")
  # a report with no number at all is released as it is
  expect_true(trace_report("No number here.", ret0)$released)
})

test_that("the release and the report may be read from files", {
  dir <- tempfile("trace_files")
  on.exit(unlink(dir, recursive = TRUE))
  ret <- trace_ret()
  f <- write_return(run_return(trace_run(), dose = 1, series = "t", round = 1,
                               requests = list(fit = c(slope = 1))), dir)
  rep_file <- file.path(dir, "report.md")
  writeLines(c("# Result", "Slope {{fit:slope}} (0.31)."), rep_file)
  tr <- trace_report(rep_file, f)
  expect_true(tr$released)
  expect_identical(tr$text, "# Result\nSlope 0.3127 (0.31).")
  expect_identical(tr$plan_hash, ret$plan_hash)
  expect_error(trace_report("x", 3), "ret must be")
  expect_error(trace_report(NA_character_, ret), "report must be")
})

# The return checks: declared units scope the coordinate scan, row filters,
# declared unit counts, differencing across rounds, forbidden strings, `|`
# escaped and declared significant figures.

# A band of latitudes and one of longitudes, as a holder in south-eastern
# Australia would declare them.
bands_au <- list(c(-45, -10), c(110, 155))

trial_tab <- data.frame(
  unit = c("north", "south", "east", "west"),
  kind = c("state", "state", "year", "year"),
  estimate = c(1.25, 0.43, 2.71, 0.98),
  n_units = c(8L, 9L, 6L, 12L),
  n_trials = c(4L, 3L, 7L, 9L),
  stringsAsFactors = FALSE)

checks_plan <- function(metrics, tab = trial_tab, returns = list()) {
  oap_plan(id = "checks", nodes = list(
    node("fit", planted_tool(metrics, tab = tab), emits = "parameters",
         returns = returns)))
}

# The coordinate scan --------------------------------------------------------

test_that("the coordinate scan refuses a value in a declared band", {
  chk <- default_checker(coord_bands = bands_au)
  expect_identical(chk(structure(138.6042, n_units = 10L)), "coordinate_like")
  expect_identical(chk(structure(-34.9285, n_units = 10L)), "coordinate_like")
  # two decimals or fewer, or outside every band, is not coordinate-like
  expect_identical(chk(structure(138.6, n_units = 10L)), NA_character_)
  expect_identical(chk(structure(-34.93, n_units = 10L)), NA_character_)
  expect_identical(chk(structure(60.1234, n_units = 10L)), NA_character_)
  # with no bands declared there is no scan
  expect_identical(default_checker()(structure(138.6042, n_units = 10L)),
                   NA_character_)
  expect_error(default_checker(coord_bands = list(c(1, 2, 3))))
  expect_error(default_checker(coord_bands = list(c(5, 1))))
})

test_that("a declared unit other than a coordinate is never scanned", {
  chk <- default_checker(coord_bands = bands_au)
  # a nitrogen rate in the longitude band and a negative price difference
  # in the latitude band (BT1 needs N18, N22, N26)
  expect_identical(chk(structure(120.375, n_units = 10L, unit = "kg N/ha")),
                   NA_character_)
  expect_identical(chk(structure(-31.125, n_units = 10L, unit = "$/ha")),
                   NA_character_)
  for (u in c("degrees", "Latitude", "longitude", "decimal_degrees",
              "coordinate")) {
    expect_identical(chk(structure(120.375, n_units = 10L, unit = u)),
                     "coordinate_like", info = u)
  }
})

test_that("a column is scanned row by row and refused on any hit", {
  chk <- default_checker(coord_bands = bands_au)
  tab <- trial_tab[, c("estimate", "n_units")]
  expect_identical(chk(tab), NA_character_)
  tab$estimate[3] <- 139.0517
  expect_identical(chk(tab), "coordinate_like")
})

# The forbidden-string scan --------------------------------------------------

test_that("a value carrying a forbidden string is refused", {
  chk <- default_checker(forbidden = c("Roseworthy", "T0417"))
  expect_identical(chk(structure("near Roseworthy", n_units = 10L)),
                   "forbidden_string")
  tab <- data.frame(label = c("ok", "T0417"), n_units = c(9L, 9L),
                    stringsAsFactors = FALSE)
  expect_identical(chk(tab), "forbidden_string")
  expect_identical(chk(structure("elsewhere", n_units = 10L)), NA_character_)
  expect_error(default_checker(forbidden = c("a", NA)))
  expect_error(default_checker(forbidden = ""))
})

# Declared units, unit counts, rows and digits through run_return ------------

test_that("plan_returns reads every declared field of a node's returns", {
  p <- checks_plan(list(), returns = list(
    list(field = "slope", dose = 1, unit = "kg N/ha", digits = 3),
    list(field = "params.estimate", dose = 2, n_units_field = "n_trials")))
  expect_warning(r <- plan_returns(p), "no declared unit: fit.params.estimate")
  expect_identical(names(r), c("node", "field", "dose", "unit",
                               "n_units_field", "digits"))
  expect_identical(r$field, c("slope", "params.estimate"))
  expect_identical(r$dose, c(1, 2))
  expect_identical(r$unit, c("kg N/ha", NA))
  expect_identical(r$n_units_field, c(NA, "n_trials"))
  expect_identical(r$digits, c(3L, NA))
  expect_identical(nrow(plan_returns(checks_plan(list()))), 0L)
})

test_that("the declared unit reaches the coordinate scan", {
  m <- list(eonr = structure(120.375, n_units = 10L),
            spot = structure(120.375, n_units = 10L))
  p <- checks_plan(m, returns = list(
    list(field = "eonr", dose = 1, unit = "kg N/ha"),
    list(field = "spot", dose = 1)))
  ret <- run_return(conduct(p, verbose = FALSE), dose = 1,
                    requests = suppressWarnings(plan_returns(p)),
                    checker = default_checker(coord_bands = bands_au))
  expect_identical(ret$values$field, "eonr")
  expect_identical(ret$refused$field, "spot")
  expect_identical(ret$refused$reason, "coordinate_like")
})

test_that("the unit count is read from the declared field and nowhere else", {
  m <- list(slope = structure(0.31, n_units = 2L), n_trials = 12L,
            gap = structure(0.5, n_units = 40L))
  p <- checks_plan(m, returns = list(
    list(field = "slope", dose = 1, n_units_field = "n_trials"),
    list(field = "gap", dose = 1, n_units_field = "absent"),
    list(field = "params.estimate", dose = 1, n_units_field = "n_trials")))
  ret <- run_return(conduct(p, verbose = FALSE), dose = 1,
                    requests = suppressWarnings(plan_returns(p)),
                    checker = default_checker(k = 5))
  # slope rests on 12 trials, not on its own attribute of 2
  expect_identical(ret$values$value[ret$values$field == "slope"], "0.31")
  ref <- stats::setNames(ret$refused$reason, ret$refused$field)
  # a declared count that is missing is no count, whatever else the value
  # carries
  expect_identical(ref[["gap"]], "no_unit_count")
  # n_trials 4, 3, 7, 9: two rows under 5 are withheld, the other two return
  expect_identical(ref[["params.estimate"]], "too_few_units")
  expect_identical(ret$refused$n_rows[ret$refused$field == "params.estimate"],
                   2L)
  expect_identical(ret$values$index[ret$values$field == "params.estimate"],
                   c(3L, 4L))
})

test_that("a params row that fails is withheld alone and counted", {
  tab <- trial_tab
  tab$estimate[1] <- 123.8493
  run <- conduct(checks_plan(list(), tab = tab), verbose = FALSE)
  ret <- run_return(run, dose = 1,
                    requests = list(fit = c("params.estimate" = 1)),
                    checker = default_checker(k = 5, coord_bands = bands_au))
  expect_identical(ret$values$index, 2:4)
  expect_identical(ret$values$value, c("0.43", "2.71", "0.98"))
  expect_identical(ret$refused$reason, "coordinate_like")
  expect_identical(ret$refused$n_rows, 1L)
  # two reasons in one column are counted apart
  tab$n_units[4] <- 2L
  run <- conduct(checks_plan(list(), tab = tab), verbose = FALSE)
  ret <- run_return(run, dose = 1,
                    requests = list(fit = c("params.estimate" = 1)),
                    checker = default_checker(k = 5, coord_bands = bands_au))
  expect_identical(ret$values$index, 2:3)
  expect_identical(stats::setNames(ret$refused$n_rows, ret$refused$reason),
                   c(coordinate_like = 1L, too_few_units = 1L))
  # every row withheld: nothing returned, every row counted
  ret <- run_return(run, dose = 1,
                    requests = list(fit = c("params.estimate" = 1)),
                    checker = default_checker(k = 50))
  expect_identical(nrow(ret$values), 0L)
  expect_identical(ret$refused$n_rows, 4L)
  expect_false(ret$released)
  # a field refused whole carries no row count
  ret <- run_return(run, dose = 1, requests = list(fit = c(absent = 1)))
  expect_identical(ret$refused$reason, "not_available")
  expect_identical(ret$refused$n_rows, NA_integer_)
})

test_that("a params table with no rows is not available", {
  run <- conduct(checks_plan(list(), tab = trial_tab[0, ]), verbose = FALSE)
  ret <- run_return(run, dose = 1,
                    requests = list(fit = c("params.estimate" = 1)))
  expect_identical(ret$refused$reason, "not_available")
  expect_identical(nrow(ret$values), 0L)
})

test_that("a row filter returns only the rows of one kind", {
  p <- checks_plan(list())
  run <- conduct(p, verbose = FALSE)
  ret <- run_return(run, dose = 1, requests = list(
    fit = c("params.estimate[kind=year]" = 1,
            "params.estimate[kind=state]" = 1,
            "params.estimate[unit=north,east]" = 1,
            "params.estimate[kind=decade]" = 1,
            "params.estimate[colour=red]" = 1)))
  got <- split(ret$values$value, ret$values$field)
  expect_identical(got[["params.estimate[kind=year]"]], c("2.71", "0.98"))
  expect_identical(got[["params.estimate[kind=state]"]], c("1.25", "0.43"))
  expect_identical(got[["params.estimate[unit=north,east]"]],
                   c("1.25", "2.71"))
  ref <- stats::setNames(ret$refused$reason, ret$refused$field)
  expect_identical(ref[["params.estimate[kind=decade]"]], "not_available")
  expect_identical(ref[["params.estimate[colour=red]"]], "not_available")
  expect_error(run_return(run, requests = list(
    fit = c("params.estimate[kind]" = 1))), "row filter")
  expect_error(run_return(run, requests = list(
    fit = c("slope[kind=year]" = 1))), "row filter")
})

test_that("the count of a filtered request is checked on its rows only", {
  tab <- trial_tab
  tab$n_units <- c(2L, 3L, 6L, 12L)
  run <- conduct(checks_plan(list(), tab = tab), verbose = FALSE)
  ret <- run_return(run, dose = 1, requests = list(
    fit = c("params.estimate[kind=year]" = 1,
            "params.estimate[kind=state]" = 1)))
  expect_identical(unique(ret$values$field), "params.estimate[kind=year]")
  expect_identical(ret$refused$reason, "too_few_units")
})

test_that("numbers are rounded to the declared significant figures", {
  m <- list(a = structure(0.318456, n_units = 10L),
            b = structure(1234.5678, n_units = 10L))
  p <- checks_plan(m, returns = list(
    list(field = "a", dose = 1, digits = 3),
    list(field = "b", dose = 1)))
  run <- conduct(p, verbose = FALSE)
  ret <- run_return(run, dose = 1, requests = suppressWarnings(plan_returns(p)), digits = 5)
  v <- stats::setNames(ret$values$value, ret$values$field)
  expect_identical(v[["a"]], "0.318")
  expect_identical(v[["b"]], "1234.6")
  expect_error(run_return(run, digits = 0))
  expect_error(run_return(run, digits = 2.5))
})

test_that("a | inside a returned value is escaped", {
  p <- checks_plan(list(tag = structure("low|high", n_units = 10L)))
  ret <- run_return(conduct(p, verbose = FALSE), dose = 1,
                    requests = list(fit = c(tag = 1)))
  expect_identical(ret$values$value, "low\\|high")
})

test_that("the data-frame form of requests is validated", {
  run <- conduct(checks_plan(list()), verbose = FALSE)
  expect_error(run_return(run, requests = data.frame(node = "fit",
                                                     field = "x")),
               "node, field and dose")
  expect_error(run_return(run, requests = data.frame(
    node = "fit", field = "x", dose = 1, digits = 0)), "digits")
  expect_error(run_return(run, requests = data.frame(
    node = "fit", field = c("x", "x"), dose = 1)), "requested twice")
})

# Differencing across the rounds of a series ---------------------------------

test_that("a value changed on few units since the last round is refused", {
  r1 <- run_return(conduct(checks_plan(
    list(slope = structure(0.31, n_units = 8L),
         rise = structure(0.70, n_units = 12L),
         flat = structure(0.50, n_units = 8L))), verbose = FALSE),
    dose = 1, round = 1, requests = list(fit = c(slope = 1, rise = 1,
                                                 flat = 1)))
  run2 <- conduct(checks_plan(
    list(slope = structure(0.33, n_units = 8L),
         rise = structure(0.74, n_units = 12L),
         flat = structure(0.50, n_units = 8L))), verbose = FALSE)
  r2 <- run_return(run2, dose = 1, round = 2, previous = r1, k_diff = 10,
                   requests = list(fit = c(slope = 1, rise = 1, flat = 1)))
  # slope changed on 8 units, under 10: refused; rise changed on 12 and
  # flat did not change: returned
  expect_identical(r2$refused$field, "slope")
  expect_identical(r2$refused$reason, "differencing")
  expect_identical(r2$values$field, c("rise", "flat"))
  # without a previous round there is nothing to difference against
  r2b <- run_return(run2, dose = 1, k_diff = 10,
                    requests = list(fit = c(slope = 1, rise = 1, flat = 1)))
  expect_identical(nrow(r2b$refused), 0L)
})

test_that("a value missing in both rounds is unchanged", {
  m <- list(gap = structure(NA_real_, n_units = 8L))
  r1 <- run_return(conduct(checks_plan(m), verbose = FALSE), dose = 1,
                   round = 1, requests = list(fit = c(gap = 1)))
  r2 <- run_return(conduct(checks_plan(m), verbose = FALSE), dose = 1,
                   round = 2, previous = r1, k_diff = 10,
                   requests = list(fit = c(gap = 1)))
  expect_identical(r2$values$field, "gap")
  expect_identical(nrow(r2$refused), 0L)
  # a missing text value is written as an empty field and reads back as ""
  dir <- tempfile("na_series")
  on.exit(unlink(dir, recursive = TRUE))
  mt <- list(tag = structure(NA_character_, n_units = 8L))
  f1 <- write_return(run_return(conduct(checks_plan(mt), verbose = FALSE),
                                dose = 1, series = "n", round = 1,
                                requests = list(fit = c(tag = 1))), dir)
  r3 <- run_return(conduct(checks_plan(mt), verbose = FALSE), dose = 1,
                   round = 2, previous = f1, k_diff = 10,
                   requests = list(fit = c(tag = 1)))
  expect_identical(r3$values$field, "tag")
  expect_identical(nrow(r3$refused), 0L)
})

test_that("differencing reads the previous round from its written file", {
  dir <- file.path(tempfile("diff_series"))
  on.exit(unlink(dir, recursive = TRUE))
  m1 <- list(slope = structure(0.31, n_units = 8L))
  m2 <- list(slope = structure(0.33, n_units = 8L))
  r1 <- run_return(conduct(checks_plan(m1), verbose = FALSE), dose = 1,
                   series = "d", round = 1, requests = list(fit = c(slope = 1)))
  f1 <- write_return(r1, dir)
  r2 <- run_return(conduct(checks_plan(m2), verbose = FALSE), dose = 1,
                   round = 2, previous = f1, k_diff = 10,
                   requests = list(fit = c(slope = 1)))
  expect_identical(r2$refused$reason, "differencing")
  # an unchanged row of a column passes; a changed row on few units is
  # withheld alone
  tab2 <- trial_tab
  tab2$estimate[2] <- 0.47
  ra <- run_return(conduct(checks_plan(list()), verbose = FALSE), dose = 1,
                   round = 1, requests = list(fit = c("params.estimate" = 1)))
  rb <- run_return(conduct(checks_plan(list(), tab = tab2), verbose = FALSE),
                   dose = 1, round = 2, previous = ra, k_diff = 10,
                   requests = list(fit = c("params.estimate" = 1)))
  expect_identical(rb$refused$reason, "differencing")
  expect_identical(rb$refused$n_rows, 1L)
  expect_identical(rb$values$index, c(1L, 3L, 4L))
  rc <- run_return(conduct(checks_plan(list(), tab = tab2), verbose = FALSE),
                   dose = 1, round = 2, previous = ra, k_diff = 9,
                   requests = list(fit = c("params.estimate" = 1)))
  expect_identical(nrow(rc$refused), 0L)
  expect_error(run_return(conduct(checks_plan(m2), verbose = FALSE),
                          previous = 3), "previous")
})

# The validator ----------------------------------------------------------------

test_that("digits and row filters in returns are checked", {
  expect_error(node("a", tool = "t", returns = list(
    list(field = "s", dose = 1, digits = 0))), "digits")
  expect_error(node("a", tool = "t", returns = list(
    list(field = "s", dose = 1, digits = 16))), "digits")
  expect_error(node("a", tool = "t", returns = list(
    list(field = "params.x[kind]", dose = 1))), "row filter")
  expect_error(node("a", tool = "t", returns = list(
    list(field = "slope[kind=a]", dose = 1))), "row filter")
  n <- node("a", tool = "t", returns = list(
    list(field = "params.x[kind=a]", dose = 1, digits = 3),
    list(field = "params.x[kind=b]", dose = 1)))
  expect_length(n@returns, 2L)
})

# Secondary suppression at release: complementary withholding over declared
# partitions, derivation closure over declared links, identical cells,
# zero-width intervals, by-design duplicates and the route search. Every
# table here is synthetic.

# One cell per row; counts repeat on every field of a unit.
sup_cells <- function(node = "fit", field = "estimate", unit, value,
                      status = "released", n_units = NA_real_,
                      n_obs = NA_real_, max_unit_share = NA_real_,
                      payload = "params") {
  data.frame(node = node, payload = payload, field = field, unit = unit,
             value = value, status = status, n_units = n_units,
             n_obs = n_obs, max_unit_share = max_unit_share,
             stringsAsFactors = FALSE)
}

sup_part <- function(parent, child, partition = parent,
                     units_additive = TRUE, obs_additive = TRUE) {
  data.frame(partition = partition, parent = parent, child = child,
             units_additive = units_additive, obs_additive = obs_additive,
             stringsAsFactors = FALSE)
}

# The status, rule and detail of one cell after suppression.
sup_status <- function(res, unit, field = "estimate", node = "fit") {
  c <- res$cells
  c$status[c$node == node & c$field == field & c$unit == unit]
}
sup_rule <- function(res, unit, field = "estimate", node = "fit") {
  c <- res$cells
  c$rule[c$node == node & c$field == field & c$unit == unit]
}

# A parent of 20 units and three sub-cells.
abc_cells <- function(n = c(20, 8, 7, 5), status = rep("released", 4L),
                      value = c(1.11, 1.37, 0.92, 1.04)) {
  sup_cells(unit = c("P", "a", "b", "c"), value = value, status = status,
            n_units = n)
}
abc_part <- sup_part("P", c("a", "b", "c"))

# Input checks ----------------------------------------------------------------

test_that("malformed cells and declarations are refused", {
  ok <- abc_cells()
  expect_error(suppress_return(ok[, c("node", "unit")]), "lack the columns")
  bad <- ok
  bad$status[1] <- "maybe"
  expect_error(suppress_return(bad), "released or withheld")
  bad <- ok
  bad$value <- as.character(bad$value)
  expect_error(suppress_return(bad), "numeric")
  expect_error(suppress_return(rbind(ok, ok[1, ])), "twice")
  bad <- ok
  bad$n_units[2] <- NA
  bad <- rbind(bad, transform(ok[2, ], field = "slope", n_units = 9))
  expect_error(suppress_return(bad), "counts")
  expect_error(suppress_return(ok, partitions = data.frame(parent = "P")),
               "partitions")
  expect_error(suppress_return(ok, partitions = sup_part("P", "P")),
               "own sub-cell")
  expect_error(suppress_return(ok, links = data.frame(
    link = "L", type = "product", node = "fit", field = "estimate",
    unit = c("a", "b"))), "type")
  expect_error(suppress_return(ok, links = data.frame(
    link = "L", type = "identity", node = "fit", field = "estimate",
    unit = c("a", "b", "c"))), "identity")
  expect_error(suppress_return(ok, route_limit = 0))
  expect_error(suppress_return(ok, dominance = 1.5))
})

test_that("with nothing declared and nothing to find, nothing changes", {
  res <- suppress_return(abc_cells())
  expect_identical(res$cells$status, rep("released", 4L))
  expect_true(all(is.na(res$cells$rule)))
  expect_identical(nrow(res$changes), 0L)
  expect_identical(nrow(res$unbreakable), 0L)
  expect_identical(nrow(res$limit), 0L)
  expect_named(res, c("cells", "changes", "unbreakable", "limit"))
})

# Complementary withholding -----------------------------------------------

test_that("a complement equal to exactly one sub-cell withholds another", {
  # P (20) = a (8) + b (7) + c (5); c withheld: P - a - b is c
  res <- suppress_return(abc_cells(status = c("released", "released",
                                              "released", "withheld")),
                         partitions = abc_part)
  # the sub-cell with the fewest units goes first
  expect_identical(sup_status(res, "b"), "withheld")
  expect_identical(sup_rule(res, "b"), "complement")
  expect_identical(sup_status(res, "a"), "released")
  expect_identical(sup_status(res, "P"), "released")
  expect_identical(res$changes$unit, "b")
})

test_that("a complement that is not one sub-cell and has k units is silent", {
  # P holds 10 units outside its sub-cells: P - a - b is c plus 10 units
  res <- suppress_return(abc_cells(n = c(30, 8, 7, 5),
                                   status = c("released", "released",
                                              "released", "withheld")),
                         partitions = abc_part)
  expect_identical(nrow(res$changes), 0L)
  # nothing withheld: no complement at all
  res <- suppress_return(abc_cells(), partitions = abc_part)
  expect_identical(nrow(res$changes), 0L)
})

test_that("a complement under k units withholds a sub-cell", {
  cells <- sup_cells(unit = c("P", "a", "b", "c", "d"),
                     value = c(1.11, 1.37, 0.92, 1.04, 1.21),
                     status = c("released", "released", "released",
                                "withheld", "withheld"),
                     n_units = c(20, 9, 7, 2, 2))
  part <- sup_part("P", c("a", "b", "c", "d"))
  res <- suppress_return(cells, partitions = part, k = 5)
  # complement c + d rests on 4 units; withholding b makes it 11
  expect_identical(sup_status(res, "b"), "withheld")
  expect_identical(sup_status(res, "a"), "released")
  # 3 + 3 = 6 units is silent
  cells$n_units <- c(22, 9, 7, 3, 3)
  res <- suppress_return(cells, partitions = part, k = 5)
  expect_identical(nrow(res$changes), 0L)
})

test_that("observation counts and a dominant unit breach a complement", {
  cells <- sup_cells(unit = c("P", "a", "b", "c"),
                     value = c(1.11, 1.37, 0.92, 1.04),
                     status = c("released", "released", "withheld",
                                "withheld"),
                     n_units = c(30, 10, 10, 10),
                     n_obs = c(100, 60, 25, 15),
                     max_unit_share = c(0.3, 0.2, 0.2, 0.2))
  # the complement b + c has 40 observations; the parent's largest unit
  # holds 30, possibly 75% of the complement
  res <- suppress_return(cells, partitions = abc_part)
  expect_identical(sup_status(res, "a"), "withheld")
  # a largest unit of 20 observations is at most 50%: silent
  cells$max_unit_share[1] <- 0.2
  res <- suppress_return(cells, partitions = abc_part)
  expect_identical(nrow(res$changes), 0L)
  # fewer than k_obs observations in the complement
  res <- suppress_return(cells, partitions = abc_part, k_obs = 50)
  expect_identical(sup_status(res, "a"), "withheld")
})

test_that("a partition with units that do not add uses observations", {
  # rate cells hold the same trials at different rates: the trial counts do
  # not add, the plot counts do
  cells <- sup_cells(unit = c("pooled", "r60", "r120"),
                     value = c(1.5, 1.3, 1.7),
                     status = c("released", "released", "withheld"),
                     n_units = c(10, 10, 10), n_obs = c(80, 40, 40))
  part <- sup_part("pooled", c("r60", "r120"), units_additive = FALSE)
  res <- suppress_return(cells, partitions = part)
  expect_identical(sup_status(res, "r60"), "withheld")
  # with a third rate withheld beside it, the complement is two rates
  cells <- sup_cells(unit = c("pooled", "r60", "r120", "r180"),
                     value = c(1.5, 1.3, 1.7, 1.6),
                     status = c("released", "released", "withheld",
                                "withheld"),
                     n_units = c(10, 10, 10, 10), n_obs = c(120, 40, 40, 40))
  part <- sup_part("pooled", c("r60", "r120", "r180"),
                   units_additive = FALSE)
  res <- suppress_return(cells, partitions = part)
  expect_identical(nrow(res$changes), 0L)
})

test_that("a missing count fails closed", {
  cells <- abc_cells(status = c("released", "released", "released",
                                "withheld"),
                     n = c(30, NA, 7, 5))
  res <- suppress_return(cells, partitions = abc_part)
  expect_gt(sum(res$cells$status == "withheld"), 1L)
})

test_that("a parent with only one sub-cell, withheld, is withheld", {
  cells <- sup_cells(unit = c("P", "a"), value = c(1.2, 1.2),
                     status = c("released", "withheld"), n_units = c(9, 9))
  # with additive units the identical-cell rule reaches it first
  res <- suppress_return(cells, partitions = sup_part("P", "a"))
  expect_identical(sup_status(res, "P"), "withheld")
  # a rate-like partition is not an identical-cell family
  res <- suppress_return(cells, partitions = sup_part(
    "P", "a", units_additive = FALSE))
  expect_identical(sup_status(res, "P"), "withheld")
  expect_identical(sup_rule(res, "P"), "complement")
})

# Derivation closure ------------------------------------------------------

sum_link <- data.frame(link = "total", type = "sum", node = "fit",
                       field = c("grain", "straw", "biomass"),
                       unit = "<u>", stringsAsFactors = FALSE)

test_that("a three-member sum link with one member withheld withholds all", {
  cells <- rbind(
    sup_cells(field = "grain", unit = c("u1", "u2"), value = c(2.13, 2.41),
              status = c("withheld", "released")),
    sup_cells(field = "straw", unit = c("u1", "u2"), value = c(3.07, 3.32)),
    sup_cells(field = "biomass", unit = c("u1", "u2"),
              value = c(5.2, 5.73)))
  res <- suppress_return(cells, links = sum_link)
  # at u1 grain = biomass - straw: both released members go
  expect_identical(sup_status(res, "u1", "straw"), "withheld")
  expect_identical(sup_status(res, "u1", "biomass"), "withheld")
  expect_identical(sup_rule(res, "u1", "straw"), "derivation")
  # u2 has nothing withheld
  expect_identical(sup_status(res, "u2", "straw"), "released")
  expect_identical(sup_status(res, "u2", "biomass"), "released")
})

test_that("a link with two members withheld is silent", {
  cells <- rbind(
    sup_cells(field = "grain", unit = "u1", value = 2.13,
              status = "withheld"),
    sup_cells(field = "straw", unit = "u1", value = 3.07,
              status = "withheld"),
    sup_cells(field = "biomass", unit = "u1", value = 5.2))
  res <- suppress_return(cells, links = sum_link)
  expect_identical(nrow(res$changes), 0L)
})

test_that("a link with fixed units spans nodes", {
  cells <- rbind(sup_cells(node = "a", unit = "pooled", value = 0.734,
                           status = "withheld"),
                 sup_cells(node = "b", unit = "pooled", value = 0.6152))
  link <- data.frame(link = "same", type = "identity", node = c("a", "b"),
                     field = "estimate", unit = "pooled")
  res <- suppress_return(cells, links = link)
  expect_identical(sup_status(res, "pooled", node = "b"), "withheld")
  expect_identical(res$changes$detail, "link same")
})

# Identical cells -------------------------------------------------------------

test_that("a parent with one populated sub-cell is made consistent with it", {
  cells <- rbind(
    sup_cells(field = "estimate", unit = c("A", "A_2020", "A_2021"),
              value = c(1.41, 1.41, NA),
              status = c("withheld", "released", "withheld"),
              n_units = c(6, 6, 0)),
    sup_cells(field = "slope", unit = c("A", "A_2020", "A_2021"),
              value = c(0.27, 0.27, NA),
              status = c("released", "withheld", "withheld"),
              n_units = c(6, 6, 0)))
  part <- sup_part("A", c("A_2020", "A_2021"))
  res <- suppress_return(cells, partitions = part)
  expect_identical(sup_status(res, "A_2020"), "withheld")
  expect_identical(sup_rule(res, "A_2020"), "identical_cell")
  expect_identical(sup_status(res, "A", "slope"), "withheld")
  expect_identical(sup_rule(res, "A", "slope"), "identical_cell")
})

test_that("two populated sub-cells are not identical to the parent", {
  cells <- sup_cells(unit = c("A", "A_2020", "A_2021"),
                     value = c(1.41, 1.38, 1.47),
                     status = c("withheld", "released", "released"),
                     n_units = c(12, 6, 6))
  part <- sup_part("A", c("A_2020", "A_2021"))
  res <- suppress_return(cells, partitions = part, k = 5)
  expect_false(any(res$changes$rule == "identical_cell"))
  # a parent and its one populated sub-cell already consistent: silent
  cells <- sup_cells(unit = c("A", "A_2020", "A_2021"),
                     value = c(1.41, 1.41, NA),
                     status = c("released", "released", "withheld"),
                     n_units = c(6, 6, 0))
  res <- suppress_return(cells, partitions = part, k = 5)
  expect_identical(nrow(res$changes), 0L)
})

# Zero-width intervals ----------------------------------------------------

zw_cells <- function(lo = 0.42, hi = 0.42, point = 0.42,
                     names = c("estimate", "estimate_lo", "estimate_hi")) {
  sup_cells(field = names, unit = "u1", value = c(point, lo, hi),
            status = c("withheld", "released", "released"))
}

test_that("bounds equal to each other and to a withheld point are withheld", {
  res <- suppress_return(zw_cells())
  expect_identical(sup_status(res, "u1", "estimate_lo"), "withheld")
  expect_identical(sup_status(res, "u1", "estimate_hi"), "withheld")
  expect_true("zero_width" %in% res$changes$rule)
  # bounds declared under other names
  res <- suppress_return(zw_cells(names = c("estimate", "lo", "hi")),
                         bounds = data.frame(field = "estimate", lo = "lo",
                                             hi = "hi"))
  expect_identical(sup_status(res, "u1", "lo"), "withheld")
  expect_identical(sup_status(res, "u1", "hi"), "withheld")
})

test_that("an interval of positive width is silent", {
  res <- suppress_return(zw_cells(lo = 0.31, hi = 0.53))
  expect_identical(nrow(res$changes), 0L)
})

# By-design duplicates ----------------------------------------------------

dup_cells <- function(status = c("withheld", "released")) {
  sup_cells(node = c("q_a", "q_b"), field = "price_ratio", unit = "pooled",
            value = 7.25, status = status)
}
dup_group <- data.frame(group = "price", node = c("q_a", "q_b"),
                        field = "price_ratio", unit = "pooled")

test_that("a duplicate withheld on one row is withheld on every row", {
  res <- suppress_return(dup_cells(), groups = dup_group)
  expect_identical(res$cells$status, c("withheld", "withheld"))
  expect_identical(sup_rule(res, "pooled", "price_ratio", "q_b"), "duplicate")
})

test_that("a duplicate already returned is released on every row", {
  seen <- data.frame(node = "q_b", field = "price_ratio", unit = "pooled")
  res <- suppress_return(dup_cells(), groups = dup_group, seen = seen)
  expect_identical(res$cells$status, c("released", "released"))
  expect_identical(sup_rule(res, "pooled", "price_ratio", "q_a"), "duplicate")
})

test_that("a consistent duplicate is silent", {
  res <- suppress_return(dup_cells(c("released", "released")),
                         groups = dup_group)
  expect_identical(nrow(res$changes), 0L)
  res <- suppress_return(dup_cells(c("withheld", "withheld")),
                         groups = dup_group)
  expect_identical(nrow(res$changes), 0L)
})

# Route search -----------------------------------------------------------------

test_that("a value equal to a released one at its unit is a route", {
  cells <- rbind(sup_cells(node = "a", unit = "u1", value = 0.4321,
                           status = "withheld"),
                 sup_cells(node = "b", unit = "u1", value = 0.4321))
  res <- suppress_return(cells)
  expect_identical(sup_status(res, "u1", node = "b"), "withheld")
  expect_identical(sup_rule(res, "u1", node = "b"), "route")
  expect_match(res$changes$detail, "a.params.estimate\\[u1\\]")
  # a different unit is not a candidate
  cells$unit[2] <- "u2"
  res <- suppress_return(cells)
  expect_identical(nrow(res$changes), 0L)
})

test_that("a ratio route across two nodes is broken by its cheaper term", {
  cells <- rbind(
    sup_cells(node = "A", field = "ratio", unit = "u1", value = 0.75,
              status = "withheld"),
    sup_cells(node = "A", field = "num", unit = "u1", value = 3.3),
    sup_cells(node = "B", field = "den", unit = "u1", value = 4.4))
  res <- suppress_return(cells)
  expect_identical(sum(res$cells$status == "withheld"), 2L)
  expect_identical(res$changes$rule, "route")
  # with A's numerator declared primary, B's denominator goes
  res <- suppress_return(cells, primary = data.frame(node = "A",
                                                     field = "num"))
  expect_identical(sup_status(res, "u1", "den", "B"), "withheld")
  expect_identical(sup_status(res, "u1", "num", "A"), "released")
})

test_that("unrelated values and whole-number targets of a sum are silent", {
  cells <- rbind(
    sup_cells(node = "A", field = "ratio", unit = "u1", value = 0.75,
              status = "withheld"),
    sup_cells(node = "A", field = "num", unit = "u1", value = 3.31),
    sup_cells(node = "B", field = "den", unit = "u1", value = 4.4))
  expect_identical(nrow(suppress_return(cells)$changes), 0L)
  # a whole number is not searched for two-term routes
  cells <- rbind(
    sup_cells(field = "count", unit = "u1", value = 3, status = "withheld"),
    sup_cells(field = "x", unit = "u1", value = 1),
    sup_cells(field = "y", unit = "u1", value = 2))
  expect_identical(nrow(suppress_return(cells)$changes), 0L)
})

test_that("a withheld parent equal to the weighted mean of its sub-cells", {
  cells <- sup_cells(unit = c("P", "a", "b"), value = c(2.5, 2.2, 2.95),
                     status = c("withheld", "released", "released"),
                     n_units = c(10, 6, 4))
  # (6 * 2.2 + 4 * 2.95) / 10 = 2.5
  part <- sup_part("P", c("a", "b"))
  res <- suppress_return(cells, partitions = part, k = 3)
  expect_identical(sum(res$changes$rule == "route"), 1L)
  # a sub-cell solved from the parent and its sibling
  cells$status <- c("released", "withheld", "released")
  res <- suppress_return(cells, partitions = part, k = 3)
  expect_true(any(res$changes$rule %in% c("route", "complement")))
  # different weights: no route
  cells$status <- c("withheld", "released", "released")
  cells$n_units <- c(10, 5, 5)
  res <- suppress_return(cells, partitions = part, k = 3)
  expect_identical(nrow(res$changes), 0L)
})

test_that("a route search that hits its limit withholds and logs", {
  cells <- rbind(sup_cells(node = "a", unit = "u1", value = 0.123,
                           status = "withheld"),
                 sup_cells(node = "b", unit = "u1", value = 0.811),
                 sup_cells(node = "c", unit = "u1", value = 0.457),
                 sup_cells(node = "d", unit = "u1", value = 0.902))
  res <- suppress_return(cells, route_limit = 2)
  expect_identical(nrow(res$limit), 1L)
  expect_identical(res$limit$node, "a")
  expect_identical(res$limit$n_candidates, 3L)
  expect_identical(sum(res$cells$status == "withheld"), 4L)
  expect_true(all(res$changes$rule == "route_limit"))
  # within the limit nothing is found and nothing withheld
  res <- suppress_return(cells, route_limit = 3)
  expect_identical(nrow(res$limit), 0L)
  expect_identical(nrow(res$changes), 0L)
})

test_that("a pinned duplicate makes a route unbreakable, listed", {
  cells <- rbind(
    sup_cells(node = "q_a", field = "price_ratio", unit = "pooled",
              value = 7.25),
    sup_cells(node = "q_b", field = "price_ratio", unit = "pooled",
              value = 7.25),
    sup_cells(node = "q_a", field = "breakeven", unit = "pooled",
              value = 7.25, status = "withheld"))
  seen <- data.frame(node = "q_a", field = "price_ratio", unit = "pooled")
  res <- suppress_return(cells, groups = dup_group, seen = seen)
  # the duplicates were returned before: they stay released
  expect_identical(sup_status(res, "pooled", "price_ratio", "q_a"),
                   "released")
  expect_identical(sup_status(res, "pooled", "price_ratio", "q_b"),
                   "released")
  # one row per route: equal to either duplicate
  expect_identical(nrow(res$unbreakable), 2L)
  expect_identical(unique(res$unbreakable$field), "breakeven")
  expect_identical(sup_status(res, "pooled", "breakeven", "q_a"), "withheld")
  # without the earlier return, the route is broken instead
  res <- suppress_return(cells, groups = dup_group)
  expect_identical(nrow(res$unbreakable), 0L)
  expect_true(all(res$cells$status == "withheld"))
})

test_that("a link whose released members are all pinned is listed", {
  cells <- rbind(
    sup_cells(field = "grain", unit = "u1", value = 2.13,
              status = "withheld"),
    sup_cells(field = "straw", unit = "u1", value = 3.07),
    sup_cells(field = "biomass", unit = "u1", value = 5.2))
  groups <- data.frame(group = c("s", "b"), node = "fit",
                       field = c("straw", "biomass"), unit = "u1")
  seen <- groups[, c("node", "field", "unit")]
  res <- suppress_return(cells, links = sum_link, groups = groups,
                         seen = seen)
  expect_identical(sup_status(res, "u1", "straw"), "released")
  expect_true(nrow(res$unbreakable) >= 1L)
  expect_true(all(res$unbreakable$field == "grain"))
})

test_that("every rule reported is one of the listed words", {
  cells <- rbind(sup_cells(node = "a", unit = "u1", value = 0.4321,
                           status = "withheld"),
                 sup_cells(node = "b", unit = "u1", value = 0.4321),
                 sup_cells(node = "c", unit = "u1", value = 0.811))
  res <- suppress_return(cells, route_limit = 1)
  expect_true(all(res$changes$rule %in% c(
    "complement", "derivation", "identical_cell", "zero_width",
    "duplicate", "route", "route_limit")))
})

# A trial-series table, from synthetic labels ----------------------------

test_that("the declared structure covers a trial-series table", {
  units <- c("pooled", "state_A", "state_B", "year_2020", "year_2021",
             "state_year_A_2020", "state_year_A_2021",
             "state_year_B_2020", "state_year_B_2021", "rate_60", "rate_120")
  n_t <- c(24, 12, 12, 13, 11, 6, 6, 7, 5, 24, 24)
  n_p <- c(192, 96, 96, 104, 88, 48, 48, 56, 40, 96, 96)
  est <- c(1.512, 1.433, 1.591, 1.478, 1.553, 1.402, 1.465, 1.555, 1.642,
           1.384, 1.640)
  status <- rep("released", length(units))
  # the B-by-2021 cell rests on too few trials and was withheld
  status[units == "state_year_B_2021"] <- "withheld"
  cells <- sup_cells(node = "q1", unit = units, value = est, status = status,
                     n_units = n_t, n_obs = n_p,
                     max_unit_share = rep(0.15, length(units)))
  # the partitions, read from the labels by the holder
  st <- grep("^state_[A-Z]$", units, value = TRUE)
  yr <- grep("^year_[0-9]{4}$", units, value = TRUE)
  sy <- grep("^state_year_", units, value = TRUE)
  rt <- grep("^rate_", units, value = TRUE)
  sy_s <- sub("^state_year_(.*)_([0-9]{4})$", "\\1", sy)
  sy_y <- sub("^state_year_(.*)_([0-9]{4})$", "\\2", sy)
  part <- rbind(
    sup_part("pooled", st, "states"), sup_part("pooled", yr, "years"),
    sup_part("pooled", rt, "rates", units_additive = FALSE),
    do.call(rbind, lapply(st, function(s) {
      sup_part(s, sy[sy_s == sub("^state_", "", s)])
    })),
    do.call(rbind, lapply(yr, function(y) {
      sup_part(y, sy[sy_y == sub("^year_", "", y)])
    })))
  res <- suppress_return(cells, partitions = part, k = 3)
  # state_B - state_year_B_2020 = state_year_B_2021: a state-by-year cell
  # goes first, so state_year_B_2020 is withheld; the year partitions then
  # withhold state_year_A_2020 and state_year_A_2021; the state and year
  # cells, the rate cells and the pooled row stay released
  expect_identical(sup_status(res, "state_year_B_2020", node = "q1"),
                   "withheld")
  expect_identical(sup_rule(res, "state_year_B_2020", node = "q1"),
                   "complement")
  expect_identical(sup_status(res, "pooled", node = "q1"), "released")
  expect_identical(sup_status(res, "state_B", node = "q1"), "released")
  expect_identical(sup_status(res, "rate_60", node = "q1"), "released")
  expect_true(all(res$changes$rule == "complement"))
  w <- res$cells$unit[res$cells$status == "withheld"]
  expect_setequal(w, sy)
})

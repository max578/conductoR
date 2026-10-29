# suppress.R -- Secondary suppression at release: the rules that look across
# cells, over the partitions and links the holder declares, after each cell
# has passed its own checks in run_return().

# The rules a change may name; no other word is returned.
.SUPPRESSION_RULES <- c("complement", "derivation", "identical_cell",
                        "zero_width", "duplicate", "route", "route_limit")
.LINK_TYPES <- c("sum", "difference", "ratio", "identity")
# The separator of a cell key: node, payload, field, unit. A control
# character, so no label can hold it.
.SEP <- "\x1f"

# paste() recycles a zero-length part to "": no part, no key.
.key <- function(node, payload, field, unit) {
  if (!length(node) || !length(payload) || !length(field) || !length(unit)) {
    return(character(0))
  }
  paste(node, payload, field, unit, sep = .SEP)
}

# A cell as a reader sees it in a log line: node.payload.field[unit].
.cell_label <- function(cl, keys) {
  i <- match(keys, cl$key)
  sprintf("%s.%s.%s[%s]", cl$node[i], cl$payload[i], cl$field[i], cl$unit[i])
}

.tol_eq <- function(x, v) abs(x - v) <= 1e-9 * max(abs(v), 1e-12)

#' Secondary suppression of a release
#'
#' Applies the rules that look across cells to the cells a release would
#' return, after each cell has passed its own checks in [run_return()]: a
#' withheld value must not follow from the released ones. Six rules run
#' until none changes a cell: by-design duplicates, identical cells,
#' complementary withholding over declared partitions, zero-width
#' intervals, derivation closure over declared links, and a route search
#' over identities, two-term arithmetic and count-weighted means. Every
#' rule fails closed: it withholds on doubt, and what it cannot break it
#' lists.
#'
#' This is a floor, not a disclosure guarantee. It finds the routes it is
#' told to look for, from the structure the holder declares; a route
#' through a link nobody declared, or through values outside `cells`, is
#' not found. The data holder remains responsible for what leaves.
#'
#' @section Cells:
#' `cells` holds one row per value of the release, with the columns `node`,
#' `field`, `unit` (the row of the node's table the value sits in; `""` for
#' a metric), `value` (numeric; a missing value is never a route) and
#' `status` (`"released"` or `"withheld"`, as the per-cell checks left it).
#' Optional columns: `payload` (`"params"` when absent, `"metrics"` for a
#' metric), and the counts of the unit's row, `n_units` (the units, such as
#' trials, the value rests on), `n_obs` (the observations, such as plots)
#' and `max_unit_share` (the largest share of the observations one unit
#' holds). Counts are the same on every row of one node and unit, and are
#' treated as public.
#'
#' @section Partitions:
#' `partitions` declares which rows are sub-cells of which: one row per
#' sub-cell, with the columns `parent` and `child`, and optionally
#' `partition` (an id; one parent may have several partitions, such as
#' states and years; default the parent), `units_additive` and
#' `obs_additive` (whether the sub-cells' unit and observation counts add
#' up to a part of the parent's; default `TRUE`) and `node` (the node it
#' applies to; missing for every node). Sub-cells of the rates of one set
#' of trials hold the same trials, so their units do not add while their
#' observations do.
#'
#' For a released parent field, the complement is the parent minus its
#' released sub-cells. It breaches when it is exactly one withheld sub-cell,
#' rests on fewer than `k` units, or, when the node carries observation
#' counts, fewer than `k_obs` observations or a possible share above
#' `dominance` for the parent's largest unit. When units do not add, the
#' complement's units are bounded below by its largest sub-cell. A breach
#' withholds one more released sub-cell: a sub-cell that is no parent
#' first, then the fewest units, then by name. With none left to withhold,
#' the parent is withheld. A missing count is a breach.
#'
#' A parent whose additive partition has exactly one sub-cell with units is
#' the same data as that sub-cell: a field withheld in either is withheld in
#' both (`identical_cell`).
#'
#' @section Links:
#' `links` declares arithmetic between fields: one row per member, with the
#' columns `link` (an id), `type` (`"sum"`, `"difference"`, `"ratio"` or
#' `"identity"`), `node`, `field` and `unit`, and optionally `payload`. A
#' unit written `"<u>"` is free: the link holds at every unit of the cells.
#' An identity has two members, a difference and a ratio three, a sum at
#' least three. When every member of a link but one is released and that
#' one is withheld, every released member is withheld (`derivation`).
#'
#' @section Bounds, duplicates and the route search:
#' A withheld value whose released bounds at the same node and unit equal
#' each other and the value has both bounds withheld (`zero_width`). The
#' bounds of a field `f` are `f_lo` and `f_hi`, unless `bounds` names
#' others (columns `field`, `lo`, `hi`).
#'
#' `groups` declares values that are one value by design (columns `group`,
#' `node`, `field`, `unit`, optionally `payload`). A group with a member in
#' `seen`, the cells an earlier return released, and a member released now
#' is released on every row and pinned: it is never withheld, since it has
#' already left. Any other group withheld on some rows is withheld on every
#' row (`duplicate`).
#'
#' The route search looks, for every withheld value, among the released
#' values at its unit, at the units whose row is identical to its row in
#' its node, at the top parents of the partitions and among the metrics: a
#' value equal to one of them, or, for a value that is not a whole number,
#' to a sum, difference, product or ratio of two; and a withheld parent
#' equal to the unit- or observation-weighted mean of its released
#' sub-cells, or a withheld sub-cell solved from its released parent and
#' siblings (relative tolerance `1e-9`). A smallest set of released values
#' that breaks every route is withheld, chosen greedily (most routes per
#' cost; cost 100 for a `primary` field, else 1; ties to a sub-cell that is
#' no parent, then by name), and the search repeats, since a newly
#' withheld value can have routes of its own (`route`). A withheld value
#' with more than `route_limit` candidates is not searched: every candidate
#' is withheld (`route_limit`) and the value is logged in `limit`. A route
#' made only of pinned values cannot be broken: it is listed in
#' `unbreakable`, and so is a complement or link that only a pinned value
#' could break.
#'
#' @param cells The cells of the release, as described below.
#' @param partitions `NULL`, or the declared partitions, as described below.
#' @param links `NULL`, or the declared links, as described below.
#' @param groups `NULL`, or the by-design duplicates.
#' @param seen `NULL`, or the cells an earlier return released (columns
#'   `node`, `field`, `unit`, optionally `payload`).
#' @param bounds `NULL`, or a table of the bounds of fields whose bounds are
#'   not named `<field>_lo` and `<field>_hi`.
#' @param primary `NULL`, or the fields kept longest by the route search
#'   (columns `node`, `field`).
#' @param k The fewest units a complement may rest on.
#' @param k_obs The fewest observations a complement may rest on, when the
#'   node carries observation counts.
#' @param dominance The largest share of a complement's observations one
#'   unit may hold, between 0 and 1.
#' @param route_limit The most candidates the route search examines for one
#'   withheld value, a whole number of at least 1.
#'
#' @returns A list: `cells` (the input with `status` as the rules leave it,
#'   and `rule` and `detail` for each cell a rule changed), `changes` (the
#'   changed cells: `node`, `payload`, `field`, `unit`, `status`, `rule`,
#'   `detail`), `unbreakable` (`node`, `payload`, `field`, `unit`, `route`:
#'   withheld values that still follow from pinned values) and `limit`
#'   (`node`, `payload`, `field`, `unit`, `n_candidates`: withheld values
#'   the route search did not search). `rule` is one of `complement`,
#'   `derivation`, `identical_cell`, `zero_width`, `duplicate`, `route` and
#'   `route_limit`.
#'
#' @seealso [run_return()], [default_checker()].
#'
#' @author Max Moldovan
#'
#' @examples
#' # A parent of 20 units over three sub-cells; c rests on too few units
#' cells <- data.frame(node = "fit", field = "estimate",
#'                     unit = c("all", "a", "b", "c"),
#'                     value = c(1.11, 1.37, 0.92, 1.04),
#'                     status = c("released", "released", "released",
#'                                "withheld"),
#'                     n_units = c(20, 8, 7, 5))
#' parts <- data.frame(parent = "all", child = c("a", "b", "c"))
#' res <- suppress_return(cells, partitions = parts)
#' res$changes
#'
#' # yield = biomass - straw at every unit
#' cells <- data.frame(node = "fit", field = c("yield", "straw", "biomass"),
#'                     unit = "site_1", value = c(2.13, 3.07, 5.2),
#'                     status = c("withheld", "released", "released"))
#' links <- data.frame(link = "biomass", type = "sum", node = "fit",
#'                     field = c("yield", "straw", "biomass"), unit = "<u>")
#' suppress_return(cells, links = links)$changes
#'
#' @export
suppress_return <- function(cells, partitions = NULL, links = NULL,
                            groups = NULL, seen = NULL, bounds = NULL,
                            primary = NULL, k = 5, k_obs = 5,
                            dominance = 0.6, route_limit = 200) {
  stopifnot(is.numeric(k), length(k) == 1L, !is.na(k), k >= 1,
            is.numeric(k_obs), length(k_obs) == 1L, !is.na(k_obs),
            k_obs >= 1,
            is.numeric(dominance), length(dominance) == 1L,
            !is.na(dominance), dominance > 0, dominance <= 1,
            .is_whole(route_limit), route_limit >= 1)
  cl <- .check_cells(cells)
  parts <- .check_partitions(partitions)
  lk <- .check_links(links)
  gr <- .check_keyed(groups, "groups", "group")
  sn <- .check_keyed(seen, "seen")
  bd <- .check_bounds(bounds)
  if (!is.null(primary)) {
    if (!is.data.frame(primary) ||
        !all(c("node", "field") %in% names(primary))) {
      stop("primary must be a data frame with the columns node and field",
           call. = FALSE)
    }
    primary <- paste(primary$node, primary$field, sep = .SEP)
  }

  status <- stats::setNames(cl$status, cl$key)
  rule <- stats::setNames(rep(NA_character_, nrow(cl)), cl$key)
  detail <- rule
  unb <- list()
  lim <- list()

  # Duplicates already returned are pinned, and released on every row -------
  pinned <- character(0)
  if (nrow(gr)) {
    sn_keys <- if (nrow(sn)) sn$key else character(0)
    for (g in split(gr$key, gr$group)) {
      g <- g[g %in% cl$key]
      if (any(g %in% sn_keys) && any(status[g] == "released")) {
        back <- g[status[g] == "withheld"]
        status[back] <- "released"
        rule[back] <- "duplicate"
        detail[back] <- "returned before"
        pinned <- c(pinned, g)
      }
    }
  }

  withhold <- function(res, r) {
    w <- res$withhold
    w <- w[!w$key %in% pinned & status[w$key] == "released", , drop = FALSE]
    w <- w[!duplicated(w$key), , drop = FALSE]
    status[w$key] <<- "withheld"
    rule[w$key] <<- r
    detail[w$key] <<- w$detail
    if (!is.null(res$unbreakable) && nrow(res$unbreakable)) {
      unb[[length(unb) + 1L]] <<- res$unbreakable
    }
    nrow(w)
  }

  # Every rule until none changes a cell -------------------------------------
  repeat {
    n <- 0L
    cur <- function() {
      cl$status <- unname(status[cl$key])
      cl
    }
    n <- n + withhold(.rule_duplicates(cur(), gr, pinned), "duplicate")
    n <- n + withhold(.rule_identical(cur(), parts), "identical_cell")
    n <- n + withhold(.rule_complement(cur(), parts, pinned, k, k_obs,
                                       dominance), "complement")
    n <- n + withhold(.rule_zero_width(cur(), bd), "zero_width")
    n <- n + withhold(.rule_closure(cur(), lk, pinned), "derivation")
    rs <- .rule_routes(cur(), parts, pinned, primary, route_limit)
    if (nrow(rs$limit)) lim[[length(lim) + 1L]] <- rs$limit
    n <- n + withhold(list(withhold = rs$forced), "route_limit")
    n <- n + withhold(rs, "route")
    if (n == 0L) break
  }

  stopifnot(!any(status[pinned] == "withheld"),
            all(is.na(rule) | rule %in% .SUPPRESSION_RULES))
  cl$status <- unname(status[cl$key])
  cl$rule <- unname(rule[cl$key])
  cl$detail <- unname(detail[cl$key])
  cols <- c("node", "payload", "field", "unit")
  chg <- cl[!is.na(cl$rule), c(cols, "status", "rule", "detail"),
            drop = FALSE]
  rownames(chg) <- NULL
  unb <- if (length(unb)) unique(do.call(rbind, unb))
         else data.frame(key = character(0), route = character(0))
  unb <- cbind(cl[match(unb$key, cl$key), cols, drop = FALSE],
               route = unb$route, stringsAsFactors = FALSE)
  rownames(unb) <- NULL
  lim <- if (length(lim)) unique(do.call(rbind, lim))
         else data.frame(key = character(0), n_candidates = integer(0))
  lim <- cbind(cl[match(lim$key, cl$key), cols, drop = FALSE],
               n_candidates = lim$n_candidates)
  rownames(lim) <- NULL
  cl$key <- NULL
  list(cells = cl, changes = chg, unbreakable = unb, limit = lim)
}

# Input checks -----------------------------------------------------------------

.check_cells <- function(cells) {
  if (!is.data.frame(cells)) {
    stop("cells must be a data frame", call. = FALSE)
  }
  miss <- setdiff(c("node", "field", "unit", "value", "status"), names(cells))
  if (length(miss)) {
    stop(sprintf("cells lack the columns: %s", paste(miss, collapse = ", ")),
         call. = FALSE)
  }
  cl <- as.data.frame(cells, stringsAsFactors = FALSE)
  if (!"payload" %in% names(cl)) cl$payload <- rep("params", nrow(cl))
  id <- c("node", "payload", "field", "unit", "status")
  for (j in id) cl[[j]] <- as.character(cl[[j]])
  if (anyNA(cl[, id])) {
    stop("cells: node, payload, field, unit and status may not be missing",
         call. = FALSE)
  }
  if (!all(cl$status %in% c("released", "withheld"))) {
    stop("cells: status must be released or withheld", call. = FALSE)
  }
  if (!is.numeric(cl$value)) {
    stop("cells: value must be numeric", call. = FALSE)
  }
  for (j in c("n_units", "n_obs", "max_unit_share")) {
    cl[[j]] <- if (j %in% names(cl)) as.numeric(cl[[j]])
               else rep(NA_real_, nrow(cl))
  }
  cl$key <- .key(cl$node, cl$payload, cl$field, cl$unit)
  if (anyDuplicated(cl$key)) {
    stop("cells: a node, payload, field and unit appear twice", call. = FALSE)
  }
  row_id <- paste(cl$node, cl$payload, cl$unit, sep = .SEP)
  for (j in c("n_units", "n_obs", "max_unit_share")) {
    v <- paste(row_id, format(cl[[j]], digits = 15))
    if (any(tapply(v, row_id, function(x) length(unique(x))) > 1L)) {
      stop(sprintf(paste0("cells: the counts in %s differ between the ",
                          "fields of one node and unit"), j), call. = FALSE)
    }
  }
  cl
}

.check_partitions <- function(partitions) {
  empty <- data.frame(partition = character(0), parent = character(0),
                      child = character(0), units_additive = logical(0),
                      obs_additive = logical(0), node = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(partitions)) return(empty)
  if (!is.data.frame(partitions) ||
      !all(c("parent", "child") %in% names(partitions))) {
    stop("partitions must be a data frame with the columns parent and child",
         call. = FALSE)
  }
  p <- as.data.frame(partitions, stringsAsFactors = FALSE)
  p$parent <- as.character(p$parent)
  p$child <- as.character(p$child)
  p$partition <- if ("partition" %in% names(p)) as.character(p$partition)
                 else p$parent
  p$node <- if ("node" %in% names(p)) as.character(p$node)
            else rep(NA_character_, nrow(p))
  for (j in c("units_additive", "obs_additive")) {
    p[[j]] <- if (j %in% names(p)) p[[j]] else rep(TRUE, nrow(p))
    if (!is.logical(p[[j]]) || anyNA(p[[j]])) {
      stop(sprintf("partitions: %s must be TRUE or FALSE", j), call. = FALSE)
    }
  }
  if (anyNA(p[, c("parent", "child", "partition")])) {
    stop("partitions: parent, child and partition may not be missing",
         call. = FALSE)
  }
  if (any(p$parent == p$child)) {
    stop("partitions: a unit cannot be its own sub-cell", call. = FALSE)
  }
  grp <- paste(p$node, p$partition, sep = .SEP)
  for (j in c("parent", "units_additive", "obs_additive")) {
    if (any(tapply(p[[j]], grp, function(x) length(unique(x))) > 1L)) {
      stop(sprintf("partitions: one partition has more than one %s", j),
           call. = FALSE)
    }
  }
  p[, names(empty)]
}

.check_links <- function(links) {
  empty <- data.frame(link = character(0), type = character(0),
                      node = character(0), payload = character(0),
                      field = character(0), unit = character(0),
                      stringsAsFactors = FALSE)
  if (is.null(links)) return(empty)
  need <- c("link", "type", "node", "field", "unit")
  if (!is.data.frame(links) || !all(need %in% names(links))) {
    stop("links must be a data frame with the columns link, type, node, ",
         "field and unit", call. = FALSE)
  }
  l <- as.data.frame(links, stringsAsFactors = FALSE)
  if (!"payload" %in% names(l)) l$payload <- rep("params", nrow(l))
  for (j in names(empty)) l[[j]] <- as.character(l[[j]])
  if (anyNA(l[, names(empty)])) {
    stop("links: no column may be missing", call. = FALSE)
  }
  if (!all(l$type %in% .LINK_TYPES)) {
    stop(sprintf("links: type must be one of %s",
                 paste(.LINK_TYPES, collapse = ", ")), call. = FALSE)
  }
  for (g in split(l, l$link)) {
    m <- nrow(g)
    ty <- unique(g$type)
    if (length(ty) != 1L) {
      stop(sprintf("links: link '%s' has more than one type", g$link[1]),
           call. = FALSE)
    }
    ok <- switch(ty, identity = m == 2L, difference = m == 3L,
                 ratio = m == 3L, sum = m >= 3L)
    if (!ok) {
      stop(sprintf(paste0("links: link '%s' has %d members; an identity ",
                          "has 2, a difference and a ratio 3, a sum at ",
                          "least 3"), g$link[1], m), call. = FALSE)
    }
  }
  l[, names(empty)]
}

# groups and seen: node, field, unit, optionally payload, and `extra`.
.check_keyed <- function(x, what, extra = character(0)) {
  if (is.null(x)) {
    return(data.frame(key = character(0), group = character(0)))
  }
  need <- c(extra, "node", "field", "unit")
  if (!is.data.frame(x) || !all(need %in% names(x))) {
    stop(sprintf("%s must be a data frame with the columns %s", what,
                 paste(need, collapse = ", ")), call. = FALSE)
  }
  x <- as.data.frame(x, stringsAsFactors = FALSE)
  pl <- if ("payload" %in% names(x)) as.character(x$payload)
        else rep("params", nrow(x))
  data.frame(key = .key(x$node, pl, x$field, x$unit),
             group = if (length(extra)) as.character(x[[extra]])
                     else rep(NA_character_, nrow(x)),
             stringsAsFactors = FALSE)
}

.check_bounds <- function(bounds) {
  if (is.null(bounds)) {
    return(data.frame(field = character(0), lo = character(0),
                      hi = character(0)))
  }
  if (!is.data.frame(bounds) ||
      !all(c("field", "lo", "hi") %in% names(bounds))) {
    stop("bounds must be a data frame with the columns field, lo and hi",
         call. = FALSE)
  }
  data.frame(field = as.character(bounds$field),
             lo = as.character(bounds$lo), hi = as.character(bounds$hi),
             stringsAsFactors = FALSE)
}

# Declared structure at one node -------------------------------------------

# A count of each node and unit, from the params cells.
.counts <- function(cl, j) {
  pr <- cl[cl$payload == "params", , drop = FALSE]
  id <- paste(pr$node, pr$unit, sep = .SEP)
  first <- !duplicated(id)
  stats::setNames(pr[[j]][first], id[first])
}

# The partitions at node `nd` whose parent and at least one sub-cell are
# among its `units`.
.node_parts <- function(parts, nd, units) {
  p <- parts[is.na(parts$node) | parts$node == nd, , drop = FALSE]
  out <- list()
  for (q in split(p, paste(p$node, p$partition, sep = .SEP))) {
    ch <- q$child[q$child %in% units]
    if (!q$parent[1] %in% units || !length(ch)) next
    out[[length(out) + 1L]] <- list(
      parent = q$parent[1], children = unique(ch),
      units_additive = q$units_additive[1],
      obs_additive = q$obs_additive[1])
  }
  out
}

.empty_withhold <- function() {
  data.frame(key = character(0), detail = character(0),
             stringsAsFactors = FALSE)
}
.empty_unbreakable <- function() {
  data.frame(key = character(0), route = character(0),
             stringsAsFactors = FALSE)
}

# The rules --------------------------------------------------------------------

# A group withheld on some rows and released on others, none returned
# before (those are pinned), is withheld on every row.
.rule_duplicates <- function(cl, gr, pinned) {
  out <- .empty_withhold()
  st <- stats::setNames(cl$status, cl$key)
  for (g in split(gr$key, gr$group)) {
    g <- g[g %in% cl$key]
    if (any(g %in% pinned)) next
    if (any(st[g] == "withheld") && any(st[g] == "released")) {
      r <- g[st[g] == "released"]
      out <- rbind(out, data.frame(key = r, detail = "withheld on another row",
                                   stringsAsFactors = FALSE))
    }
  }
  list(withhold = out)
}

# A parent whose additive partition has one sub-cell with units: a field
# withheld in one of the two is withheld in the other. (BT1's option B,
# withholding the whole sub-cell row, is never smaller and on a tie gives
# the same cells, so it is not separate here.)
.rule_identical <- function(cl, parts) {
  out <- .empty_withhold()
  pr <- cl[cl$payload == "params", , drop = FALSE]
  nu <- .counts(cl, "n_units")
  for (nd in unique(pr$node)) {
    sub <- pr[pr$node == nd, , drop = FALSE]
    st <- stats::setNames(sub$status, paste(sub$unit, sub$field, sep = .SEP))
    for (p in .node_parts(parts, nd, unique(sub$unit))) {
      if (!p$units_additive) next
      n_ch <- nu[paste(nd, p$children, sep = .SEP)]
      ch <- p$children[!is.na(n_ch) & n_ch > 0]
      if (length(ch) != 1L) next
      fs <- unique(sub$field[sub$unit %in% c(p$parent, ch)])
      s_p <- st[paste(p$parent, fs, sep = .SEP)]
      s_c <- st[paste(ch, fs, sep = .SEP)]
      a_c <- fs[s_p %in% "withheld" & s_c %in% "released"]
      a_p <- fs[s_c %in% "withheld" & s_p %in% "released"]
      if (length(a_c)) {
        out <- rbind(out, data.frame(
          key = .key(nd, "params", a_c, ch),
          detail = sprintf("the only populated sub-cell of %s", p$parent),
          stringsAsFactors = FALSE))
      }
      if (length(a_p)) {
        out <- rbind(out, data.frame(
          key = .key(nd, "params", a_p, p$parent),
          detail = sprintf("same data as its only populated sub-cell %s",
                           ch), stringsAsFactors = FALSE))
      }
    }
  }
  list(withhold = out)
}

# Complementary withholding, field by field over every partition.
.rule_complement <- function(cl, parts, pinned, k, k_obs, dominance) {
  out <- .empty_withhold()
  unb <- .empty_unbreakable()
  pr <- cl[cl$payload == "params", , drop = FALSE]
  rel <- pr$key[pr$status == "released"]
  nu <- .counts(cl, "n_units")
  no <- .counts(cl, "n_obs")
  sh <- .counts(cl, "max_unit_share")
  parents <- unique(parts$parent)
  for (nd in unique(pr$node)) {
    sub <- pr[pr$node == nd, , drop = FALSE]
    g <- function(v, u) {
      if (!length(u)) return(numeric(0))
      unname(v[paste(nd, u, sep = .SEP)])
    }
    for (p in .node_parts(parts, nd, unique(sub$unit))) {
      for (f in unique(sub$field)) {
        kp <- .key(nd, "params", f, p$parent)
        repeat {
          if (!kp %in% rel) break
          is_rel <- .key(nd, "params", f, p$children) %in% rel
          rel_ch <- p$children[is_rel]
          unrel <- p$children[!is_rel]
          o_par <- g(no, p$parent)
          has_obs <- !is.na(o_par)
          o_c <- if (p$obs_additive) o_par - sum(g(no, rel_ch))
                 else if (length(unrel)) max(g(no, unrel))
                 else NA_real_
          t_c <- if (p$units_additive) g(nu, p$parent) - sum(g(nu, rel_ch))
                 else if (length(unrel)) max(g(nu, unrel))
                 else if (!is.na(o_c) && o_c > 0) 1 else 0
          has_rest <- is.na(t_c) || t_c > 0 || (!is.na(o_c) && o_c > 0)
          single <- length(unrel) == 1L && !isFALSE(
            if (p$units_additive) t_c == g(nu, unrel)
            else o_c == g(no, unrel))
          big <- g(sh, p$parent) * o_par
          obs_bad <- has_obs && (is.na(o_c) || o_c < k_obs ||
                                   isTRUE(big / o_c > dominance))
          breach <- has_rest && (single || !isFALSE(t_c < k) || obs_bad)
          if (!breach) break
          why <- sprintf("complement of %s in %s", p$parent, f)
          k_rel <- .key(nd, "params", f, rel_ch)
          can <- rel_ch[!k_rel %in% pinned]
          if (!length(can)) {
            if (single || length(rel_ch)) {
              if (kp %in% pinned) {
                left <- if (length(unrel)) .key(nd, "params", f, unrel)
                        else kp
                unb <- rbind(unb, data.frame(key = left, route = why))
              } else {
                out <- rbind(out, data.frame(key = kp, detail = why))
                rel <- setdiff(rel, kp)
              }
            }
            break
          }
          ord <- order(can %in% parents, g(nu, can), can, na.last = TRUE)
          pick <- .key(nd, "params", f, can[ord[1]])
          out <- rbind(out, data.frame(key = pick, detail = why))
          rel <- setdiff(rel, pick)
        }
      }
    }
  }
  list(withhold = out, unbreakable = unb)
}

# Both bounds of a withheld value, when they are released, equal to each
# other and to the value.
.rule_zero_width <- function(cl, bd) {
  out <- .empty_withhold()
  unb <- .empty_unbreakable()
  w <- cl[cl$status == "withheld" & is.finite(cl$value), , drop = FALSE]
  for (i in seq_len(nrow(w))) {
    f <- w$field[i]
    j <- match(f, bd$field)
    b_f <- if (is.na(j)) paste0(f, c("_lo", "_hi")) else c(bd$lo[j], bd$hi[j])
    k_b <- .key(w$node[i], w$payload[i], b_f, w$unit[i])
    m <- match(k_b, cl$key)
    if (anyNA(m) || !all(cl$status[m] == "released")) next
    v <- cl$value[m]
    if (all(is.finite(v)) && .tol_eq(v[1], v[2]) && .tol_eq(v[1], w$value[i])) {
      out <- rbind(out, data.frame(
        key = k_b, detail = sprintf("zero-width interval of %s",
                                    .cell_label(cl, w$key[i]))))
    }
  }
  list(withhold = out, unbreakable = unb)
}

# Derivation closure over the declared links.
.rule_closure <- function(cl, lk, pinned) {
  out <- .empty_withhold()
  unb <- .empty_unbreakable()
  if (!nrow(lk)) return(list(withhold = out, unbreakable = unb))
  rel <- cl$key[cl$status == "released"]
  wh <- cl$key[cl$status == "withheld"]
  units <- unique(cl$unit[cl$payload == "params"])
  inst <- list()
  for (l in split(lk, lk$link)) {
    if (!any(l$unit == "<u>")) {
      inst[[length(inst) + 1L]] <- l
      next
    }
    for (u in units) {
      l2 <- l
      l2$unit[l2$unit == "<u>"] <- u
      inst[[length(inst) + 1L]] <- l2
    }
  }
  changed <- TRUE
  while (changed) {
    changed <- FALSE
    for (g in inst) {
      kg <- .key(g$node, g$payload, g$field, g$unit)
      is_rel <- kg %in% rel
      if (sum(!is_rel) != 1L || !kg[!is_rel] %in% wh || !any(is_rel)) next
      take <- kg[is_rel & !kg %in% pinned]
      why <- sprintf("link %s", g$link[1])
      if (!length(take)) {
        unb <- rbind(unb, data.frame(key = kg[!is_rel], route = why))
        next
      }
      out <- rbind(out, data.frame(key = take, detail = why))
      rel <- setdiff(rel, take)
      wh <- c(wh, take)
      changed <- TRUE
    }
  }
  list(withhold = out, unbreakable = unique(unb))
}

# The units of a node whose rows hold the same fields with the same values.
.aliases <- function(cl) {
  pr <- cl[cl$payload == "params", , drop = FALSE]
  out <- list()
  for (nd in unique(pr$node)) {
    sub <- pr[pr$node == nd, , drop = FALSE]
    sub <- sub[order(sub$unit, sub$field), , drop = FALSE]
    sig <- tapply(paste(sub$field, format(sub$value, digits = 15)), sub$unit,
                  paste, collapse = .SEP)
    for (s in unique(sig[duplicated(sig)])) {
      us <- names(sig)[sig == s]
      for (u in us) out[[paste(nd, u, sep = .SEP)]] <- us
    }
  }
  out
}

# The route search with a greedy smallest hitting set.
.rule_routes <- function(cl, parts, pinned, primary, route_limit) {
  out_w <- .empty_withhold()
  out_u <- .empty_unbreakable()
  forced <- .empty_withhold()
  lim <- data.frame(key = character(0), n_candidates = integer(0))
  fin <- cl[is.finite(cl$value), , drop = FALSE]
  rel <- fin$key[fin$status == "released"]
  wh <- fin$key[fin$status == "withheld"]
  parents <- unique(parts$parent)
  roots <- setdiff(parents, parts$child)
  alias <- .aliases(cl)
  nu <- .counts(cl, "n_units")
  no <- .counts(cl, "n_obs")
  val <- stats::setNames(cl$value, cl$key)
  cost_of <- function(k) {
    i <- match(k, cl$key)
    ifelse(paste(cl$node[i], cl$field[i], sep = .SEP) %in% primary, 100, 1)
  }
  is_parent <- function(k) cl$unit[match(k, cl$key)] %in% parents
  done <- character(0)
  repeat {
    R <- fin[fin$key %in% rel, , drop = FALSE]
    W <- fin[fin$key %in% setdiff(wh, done), , drop = FALSE]
    routes <- list()
    tgt <- character(0)
    add <- function(r, t) {
      routes[[length(routes) + 1L]] <<- r
      tgt <<- c(tgt, t)
    }
    for (r1 in seq_len(nrow(W))) {
      w <- W[r1, ]
      us <- unique(c(w$unit, alias[[paste(w$node, w$unit, sep = .SEP)]]))
      C <- R[R$unit %in% c(us, roots, "") &
               (R$node == w$node | R$unit %in% us), , drop = FALSE]
      if (nrow(C) > route_limit) {
        lim <- rbind(lim, data.frame(key = w$key, n_candidates = nrow(C)))
        take <- setdiff(C$key, c(pinned, forced$key))
        if (length(take)) {
          forced <- rbind(forced, data.frame(
            key = take, detail = sprintf("route search limit at %s",
                                         .cell_label(cl, w$key))))
        }
        done <- c(done, w$key)
        next
      }
      if (nrow(C)) {
        for (i1 in which(.tol_eq(C$value, w$value))) add(C$key[i1], w$key)
        if (w$value != 0 && abs(w$value - round(w$value)) > 1e-9) {
          for (op in c("+", "-", "*", "/")) {
            M <- outer(C$value, C$value, op)
            ij <- which(abs(M - w$value) <= 1e-9 * abs(w$value),
                        arr.ind = TRUE)
            ij <- ij[ij[, 1] != ij[, 2], , drop = FALSE]
            for (k1 in seq_len(nrow(ij))) add(sort(C$key[ij[k1, ]]), w$key)
          }
        }
      }
      # count-weighted means of a family
      if (w$payload == "params") {
        units_nd <- unique(cl$unit[cl$node == w$node &
                                     cl$payload == "params"])
        for (p in .node_parts(parts, w$node, units_nd)) {
          if (!w$unit %in% c(p$parent, p$children)) next
          k_par <- .key(w$node, "params", w$field, p$parent)
          k_ch <- .key(w$node, "params", w$field, p$children)
          x_ch <- unname(val[k_ch])
          x_par <- unname(val[k_par])
          id_ch <- paste(w$node, p$children, sep = .SEP)
          for (wt in list(unname(nu[id_ch]), unname(no[id_ch]))) {
            if (any(!is.finite(wt)) || sum(wt) <= 0) next
            if (w$unit == p$parent && all(k_ch %in% rel) &&
                all(is.finite(x_ch)) &&
                .tol_eq(sum(x_ch * wt) / sum(wt), w$value)) {
              add(k_ch, w$key)
            }
            i_c <- match(w$unit, p$children)
            if (!is.na(i_c) && k_par %in% rel && all(k_ch[-i_c] %in% rel) &&
                is.finite(x_par) && all(is.finite(x_ch))) {
              v_s <- (x_par * sum(wt) - sum(x_ch[-i_c] * wt[-i_c])) / wt[i_c]
              if (.tol_eq(v_s, w$value)) add(c(k_par, k_ch[-i_c]), w$key)
            }
          }
        }
      }
    }
    # a forced term breaks every route through it
    if (nrow(forced)) {
      rel <- setdiff(rel, forced$key)
      wh <- union(wh, forced$key)
      keep <- !vapply(routes, function(r) any(r %in% forced$key), logical(1))
      routes <- routes[keep]
      tgt <- tgt[keep]
    }
    if (!length(routes)) break
    # a route of pinned values only cannot be broken: list its target and
    # leave that target's other routes alone
    unb <- vapply(routes, function(r) all(r %in% pinned), logical(1))
    if (any(unb)) {
      out_u <- rbind(out_u, data.frame(
        key = tgt[unb],
        route = vapply(routes[unb], function(r) {
          paste(.cell_label(cl, r), collapse = " & ")
        }, character(1))))
      keep <- !tgt %in% tgt[unb]
      done <- union(done, tgt[unb])
      routes <- routes[keep]
      tgt <- tgt[keep]
    }
    if (!length(routes)) next
    routes <- lapply(routes, setdiff, pinned)
    picked <- character(0)
    open_r <- rep(TRUE, length(routes))
    while (any(open_r)) {
      n_r <- table(unlist(routes[open_r]))
      nm <- names(n_r)
      sc <- as.numeric(n_r) / cost_of(nm)
      k_pick <- nm[order(-sc, is_parent(nm), nm)[1]]
      picked <- c(picked, k_pick)
      open_r <- open_r & !vapply(routes, function(r) k_pick %in% r,
                                 logical(1))
    }
    why <- vapply(picked, function(k) {
      hit <- unique(tgt[vapply(routes, function(r) k %in% r, logical(1))])
      sprintf("route to %s", paste(.cell_label(cl, hit), collapse = "; "))
    }, character(1), USE.NAMES = FALSE)
    out_w <- rbind(out_w, data.frame(key = picked, detail = why))
    rel <- setdiff(rel, picked)
    wh <- c(wh, picked)
  }
  list(withhold = out_w, unbreakable = unique(out_u), forced = forced,
       limit = lim)
}

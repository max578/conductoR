# conduct.R -- The walker: type-check every edge, dispatch each node, record
# provenance.

# Refuses an input that is not a manifest, fails the contract's own consumer
# guard, or declares a different inferential target from the one the edge
# was typed with.
.check_edge <- function(upstream, contract, to_id, from_id) {
  if (!orchestraManifest::is_orchestra_manifest(upstream)) {
    stop(sprintf("node '%s' expected an orchestra_manifest from '%s'",
                 to_id, from_id), call. = FALSE)
  }
  orchestraManifest::consume_manifest(upstream)
  if (contract != "any" && upstream@inferential_target != contract) {
    stop(sprintf(paste0("TYPED-EDGE VIOLATION -- node '%s' consumes '%s' as ",
                        "'%s', but '%s' emits inferential_target '%s'. ",
                        "The step is not run."),
                 to_id, from_id, contract, from_id,
                 upstream@inferential_target), call. = FALSE)
  }
  invisible(upstream)
}

.abstained <- function(m) isTRUE(m@summary$abstained)

# Map from each node id to the ids of the nodes that consume it.
.child_map <- function(plan) {
  ids <- .node_ids(plan)
  ch  <- stats::setNames(vector("list", length(ids)), ids)
  for (n in plan@nodes) for (e in n@consumes) {
    ch[[e$from]] <- c(ch[[e$from]], n@id)
  }
  ch
}

.descendants <- function(children, id) {
  out <- character(0)
  frontier <- children[[id]] %||% character(0)
  while (length(frontier)) {
    nxt <- setdiff(frontier, out)
    out <- union(out, nxt)
    frontier <- unique(unlist(children[nxt]))
  }
  out
}

#' Perform one node
#'
#' Type-checks every input the node consumes, dispatches on the node's
#' operator and checks that the output declares the inferential target the
#' node promised. [conduct()] calls this once per node, and a pipeline
#' written by [compile_to_targets()] calls it once per target.
#'
#' @param node An [oap_node].
#' @param inputs A named list of upstream manifests, one per edge in
#'   `node@consumes`, named by the upstream node id.
#' @param data The data object every tool receives as its second argument.
#'
#' @returns The [orchestraManifest::orchestra_manifest] the node emitted.
#'
#' @section Errors:
#' An input that does not declare the contract its edge was typed with stops
#' the step with a message containing `TYPED-EDGE VIOLATION`. An input that
#' fails [orchestraManifest::consume_manifest()] stops with that guard's own
#' message. A tool that returns something other than a manifest, or a
#' manifest with a different target from `node@emits`, stops after the tool
#' has run.
#'
#' `when`, `foreach` and `on_refusal` are applied by [conduct()];
#' `conduct_step()` performs the node once, as given.
#'
#' @seealso [conduct()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- node("lens", emits = "treatment_effects",
#'              tool = function(inputs, data, node) {
#'                lens_to_manifest("effect", TRUE, "example", "example:lens")
#'              })
#' m <- conduct_step(lens)
#' m@inferential_target
#'
#' @export
conduct_step <- function(node, inputs = list(), data = list()) {
  stopifnot(S7::S7_inherits(node, oap_node))
  for (e in node@consumes) {
    .check_edge(inputs[[e$from]], e$contract, to_id = node@id, from_id = e$from)
  }
  out <- switch(node@operator,
    compute      = (if (nzchar(node@tool_ref)) resolve_tool(node@tool_ref)
                    else node@tool)(inputs, data, node),
    abstain_gate = .op_abstain_gate(node, inputs),
    triangulate  = .op_triangulate(node, inputs),
    select       = .op_select(node, inputs))
  if (!orchestraManifest::is_orchestra_manifest(out)) {
    stop(sprintf("node '%s' did not return an orchestra_manifest", node@id),
         call. = FALSE)
  }
  if (node@emits != "any" && out@inferential_target != node@emits) {
    stop(sprintf("node '%s' emitted '%s', promised '%s'",
                 node@id, out@inferential_target, node@emits), call. = FALSE)
  }
  out
}

#' Perform a plan
#'
#' Walks the plan in topological order and performs each node with
#' [conduct_step()]. Every in-edge is type-checked before its node runs, so a
#' mismatched handoff stops the run before the downstream tool executes. An
#' abstaining gate in `halt-branch` mode skips every node downstream of it;
#' the skipped nodes are recorded in the provenance, not dropped.
#'
#' @section The abstention gate:
#' An `abstain_gate` node reads the one verdict it consumes and abstains
#' when any of these holds: the upstream emitter abstained; the verdict is
#' `"no_effect"` and the metric `n` is below `params$n_min` (default 25); the
#' verdict is `"no_effect"` and the power proxy
#' `|effect_mean| / (noise_sd / sqrt(n))` is below `params$z_floor` (default
#' 2); or any metric named in `params$floors` (a named list of minima) is
#' below its floor. `params$mode` sets what an abstention does downstream:
#' `"flag"` (default) records it and continues, `"halt-branch"` skips the
#' descendants, `"reroute"` tells a downstream triangulation to drop the
#' gated lens and reconcile the survivors.
#'
#' @section Triangulation:
#' A `triangulate` node reads every lens it consumes, each a manifest whose
#' summary metrics carry `effect_present` or `influence_present`, and at most
#' one gate. It emits one of seven consensus headlines:
#' `causal_effect_corroborated` (two or more lenses, all with an effect),
#' `no_effect_corroborated` (two or more, none with an effect),
#' `single_lens_effect` and `single_lens_no_effect` (one lens),
#' `disagreement_flagged` (mixed), and `abstain` (the gate abstained in flag
#' mode, or no lens survived a reroute).
#'
#' @section Conditions:
#' A node with a `when` condition runs only when the condition is `TRUE`.
#' When it is `FALSE`, the node and every node downstream of it are skipped,
#' and the run records the condition, its value and the reason. A condition
#' reads results of nodes upstream: `<node>.headline`, `<node>.abstained`,
#' `<node>.n_rows` (rows of its `params` table) and `<node>.metrics.<name>`.
#' It may use numbers written plainly, quoted strings, `TRUE`, `FALSE`, the
#' comparisons `==`, `!=`, `<`, `<=`, `>`, `>=`, `%in%` with `c(...)`, the
#' logical operators `&`, `|`, `!` (`&&` and `||` read the same) and
#' brackets. A function call, `$`, `::`, backticks, an assignment and
#' anything else not listed here are refused when the plan is built. A
#' condition is never run as R code.
#'
#' A `select` node keeps the rows of its input's `params` table for which
#' `params$rule` is `TRUE`. The rule follows the same grammar, with column
#' names in place of references. Rows where the rule gives `NA` are dropped
#' and counted in the metric `n_na`.
#'
#' @section Running a node once per item:
#' A compute node with `foreach` runs once for each item: a fixed list, or
#' each distinct value of a column of the `params` table of a node it
#' consumes (`list(from = <node>, column = <name>)`). Each instance receives
#' its item as `node@params$item` and is recorded as `<id>[<item>]`. More
#' items than `foreach_max` (default 50) stop the run with an error. The node
#' emits one manifest whose `params` table has one row per item, with its
#' status (`ran`, `refused` or `skipped`), and whose `outputs` holds the
#' instance manifests. With no items, nothing runs and downstream nodes
#' receive that record, with `n_items = 0`.
#'
#' @section Refusals:
#' A refusal is a result that abstains: its summary says `abstained`. A
#' node's `on_refusal` sets what a refusal does: `"continue"` (default)
#' passes it on, `"halt-branch"` skips every node downstream of it, `"stop"`
#' ends the run; the nodes not yet performed are recorded as skipped. In a
#' `foreach`, `"halt-branch"` and `"stop"` also skip the items not yet run.
#' A tool that fails with an error stops the run in every case: an error is
#' not a refusal.
#'
#' @param plan An [oap_plan].
#' @param data The data object every tool receives.
#' @param out_dir A directory to write the provenance record into, or `NULL`
#'   to write nothing. See [run_provenance()] for the files.
#' @param verbose Print one line per node as it runs.
#' @param label A short string stored with the run, such as `"blinded"` or
#'   `"unblinded"`, or `NULL`.
#'
#' @section Blinded runs:
#' Every run records `input_hash`, a SHA-256 of `data`, and `plan_hash`, a
#' SHA-256 of the plan together with the code of every tool it references
#' (see [same_plan()]). To blind an analysis, run the plan on data whose
#' treatment labels are hidden, freeze the plan, then run it once on the
#' true data. [same_plan()] then shows that both runs used one plan on
#' different data.
#'
#' @returns A list with elements `plan_id`, `plan_hash`, `input_hash`,
#'   `label`, `bus` (the manifest each node emitted, named by node id),
#'   `provenance` (one record per node), `conditions` (one row per `when`
#'   evaluated: node, condition, value), `items` (one row per `foreach` item:
#'   node, item, instance, status, run_id), `stopped_by` (the node whose
#'   refusal ended the run, or `NA`), `n_nodes`, `n_skipped`, `seconds` and
#'   `terminal` (the manifest of the last node that ran). Returned invisibly.
#'
#' @seealso [conduct_step()], [run_provenance()], [plan_to_yaml()],
#'   [same_plan()].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens_a <- function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "lens_a", "lens_a:demo")
#' }
#' lens_b <- function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "lens_b", "lens_b:demo")
#' }
#' p <- oap_plan(id = "two_lenses", nodes = list(
#'   node("a", lens_a, emits = "treatment_effects"),
#'   node("b", lens_b, emits = "treatment_effects"),
#'   node("consensus", operator = "triangulate", emits = "decisions",
#'        consumes = list(edge("a", "treatment_effects"),
#'                        edge("b", "treatment_effects")))
#' ))
#' run <- conduct(p, verbose = FALSE)
#' run$terminal@summary$headline
#'
#' # Run a check once per site, but only when lens a found an effect.
#' check <- function(inputs, data, node) {
#'   lens_to_manifest("effect", TRUE, "check",
#'                    paste0("check:", node@params$item))
#' }
#' p2 <- oap_plan(id = "per_site", nodes = list(
#'   node("a", lens_a, emits = "treatment_effects"),
#'   node("check", check, emits = "treatment_effects",
#'        consumes = list(edge("a", "treatment_effects")),
#'        when = "a.headline == \"effect\"",
#'        foreach = c("site_1", "site_2"))
#' ))
#' run2 <- conduct(p2, verbose = FALSE)
#' run2$items
#'
#' @export
conduct <- function(plan, data = list(), out_dir = NULL, verbose = TRUE,
                    label = NULL) {
  stopifnot(S7::S7_inherits(plan, oap_plan))
  if (!is.null(label)) stopifnot(is.character(label), length(label) == 1L)
  plan_hash  <- .plan_hash(plan)
  input_hash <- .object_hash(data)
  order    <- plan_order(plan)
  by_id    <- stats::setNames(plan@nodes, .node_ids(plan))
  children <- .child_map(plan)
  bus <- list()
  prov <- list()
  skip_reason <- list()
  conds <- list()
  items <- list()
  stopped_by <- NA_character_
  last_produced <- NA_character_
  t0 <- Sys.time()

  record <- function(id, operator, consumes, emits, emitter, run_id,
                     data_hash, abstained, seconds, tag = "",
                     status = "ran", reason = "") {
    prov[[length(prov) + 1L]] <<- list(
      node = id, role = by_id[[id]]@role, operator = operator,
      consumes = consumes, emits = emits, emitter = emitter, run_id = run_id,
      data_hash = data_hash, abstained = abstained, seconds = round(seconds, 3),
      status = status, reason = reason)
    if (verbose) {
      cat(sprintf("  [%-12s] %-14s %-20s -> %-18s %s\n", operator, id,
                  if (length(consumes)) paste(consumes, collapse = "+") else "",
                  emits, tag))
    }
  }
  skip_below <- function(id, why) {
    for (d in .descendants(children, id)) {
      if (is.null(skip_reason[[d]])) skip_reason[[d]] <<- why
    }
  }

  for (id in order) {
    n  <- by_id[[id]]
    cn <- .edge_sources(n)

    # Skipped and conditional nodes ---------------------------------------
    if (!is.null(skip_reason[[id]])) {
      record(id, "skipped", cn, "(skipped)", "--", "--", "--", NA, 0,
             tag = "[skipped]", status = "skipped",
             reason = skip_reason[[id]])
      next
    }
    if (nzchar(n@when)) {
      ok <- .when_value(n, bus)
      conds[[length(conds) + 1L]] <- data.frame(
        node = id, condition = n@when, value = ok, stringsAsFactors = FALSE)
      if (!ok) {
        why <- sprintf("when `%s` is FALSE", n@when)
        record(id, "skipped", cn, "(skipped)", "--", "--", "--", NA, 0,
               tag = "[when FALSE]", status = "skipped", reason = why)
        skip_below(id, sprintf("upstream node '%s' was skipped (%s)", id, why))
        next
      }
    }

    # Perform the node ------------------------------------------------------
    inputs <- stats::setNames(lapply(cn, function(from) bus[[from]]), cn)
    ts  <- Sys.time()
    if (is.null(n@foreach)) {
      out <- conduct_step(n, inputs, data)
      refused <- .abstained(out)
    } else {
      fe <- .run_foreach(n, inputs, data)
      out <- fe$manifest
      items <- c(items, list(fe$table))
      refused <- fe$refused
    }
    dt  <- as.numeric(difftime(Sys.time(), ts, units = "secs"))
    bus[[id]] <- out
    last_produced <- id

    if (n@operator == "abstain_gate" && .abstained(out) &&
        identical(out@summary$metrics$mode, "halt-branch")) {
      skip_below(id, sprintf("gate '%s' abstained in halt-branch mode", id))
    }
    if (refused && n@on_refusal == "halt-branch") {
      skip_below(id, sprintf("'%s' refused (on_refusal: halt-branch)", id))
    }
    record(id, n@operator, cn, out@inferential_target, out@emitter_package,
           out@run_id, out@data_hash, .abstained(out), dt,
           tag = if (.abstained(out)) "[ABSTAIN]" else "",
           status = if (refused && is.null(n@foreach)) "refused" else "ran")
    if (refused && n@on_refusal == "stop") {
      stopped_by <- id
      why <- sprintf("the run stopped: '%s' refused (on_refusal: stop)", id)
      for (rest in order[seq_along(order) > match(id, order)]) {
        if (is.null(skip_reason[[rest]])) skip_reason[[rest]] <- why
        record(rest, "skipped", .edge_sources(by_id[[rest]]), "(skipped)",
               "--", "--", "--", NA, 0, tag = "[stopped]",
               status = "skipped", reason = skip_reason[[rest]])
      }
      break
    }
  }

  total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  terminal <- if (!is.na(last_produced)) bus[[last_produced]] else NULL
  n_skipped <- sum(vapply(prov, function(p) p$status == "skipped", logical(1)))
  run <- list(plan_id = plan@id, plan_hash = plan_hash,
              input_hash = input_hash, label = label %||% NA_character_,
              bus = bus, provenance = prov,
              conditions = .bind_rows(conds, c("node", "condition", "value")),
              items = .bind_rows(items, c("node", "item", "instance",
                                          "status", "run_id")),
              stopped_by = stopped_by,
              n_nodes = length(order), n_skipped = n_skipped,
              seconds = round(total, 3), terminal = terminal)
  if (!is.null(out_dir)) .write_provenance(run, out_dir)
  invisible(run)
}

.bind_rows <- function(rows, cols) {
  if (length(rows)) return(do.call(rbind, rows))
  out <- as.data.frame(stats::setNames(
    replicate(length(cols), character(0), simplify = FALSE), cols),
    stringsAsFactors = FALSE)
  if ("value" %in% cols) out$value <- logical(0)
  out
}

# The items of a foreach node: its fixed list, or the distinct values of a
# column of an upstream params table.
.foreach_items <- function(n, inputs) {
  fe <- n@foreach
  if (!is.null(names(fe))) {
    tab <- inputs[[fe$from]]@params
    if (!fe$column %in% names(tab)) {
      stop(sprintf(paste0("node '%s' takes its items from column '%s' of ",
                          "'%s', which has no such column; its columns ",
                          "are: %s"), n@id, fe$column, fe$from,
                   if (ncol(tab)) paste(names(tab), collapse = ", ") else "(none)"),
           call. = FALSE)
    }
    vals <- tab[[fe$column]]
    if (anyNA(vals)) {
      stop(sprintf("node '%s': column '%s' of '%s' has missing values",
                   n@id, fe$column, fe$from), call. = FALSE)
    }
    fe <- as.list(unique(as.vector(vals)))
  }
  if (length(fe) > n@foreach_max) {
    stop(sprintf(paste0("node '%s' has %d items, more than foreach_max = %d; ",
                        "raise foreach_max or narrow the selection"),
                 n@id, length(fe), n@foreach_max), call. = FALSE)
  }
  fe
}

.run_foreach <- function(n, inputs, data) {
  its <- .foreach_items(n, inputs)
  rows <- list()
  refused <- FALSE
  halted <- FALSE
  for (it in its) {
    inst <- sprintf("%s[%s]", n@id, as.character(it))
    if (halted) {
      rows[[length(rows) + 1L]] <- list(item = as.character(it), instance = inst,
                                        status = "skipped", run_id = NA_character_,
                                        headline = NA_character_, manifest = NULL)
      next
    }
    ni <- n
    ni@foreach <- NULL
    ni@params$item <- it
    m <- conduct_step(ni, inputs, data)
    ab <- .abstained(m)
    rows[[length(rows) + 1L]] <- list(
      item = as.character(it), instance = inst,
      status = if (ab) "refused" else "ran", run_id = m@run_id,
      headline = as.character(m@summary$headline %||% NA_character_),
      manifest = m)
    if (ab) {
      refused <- TRUE
      halted <- n@on_refusal %in% c("halt-branch", "stop")
    }
  }
  man <- foreach_to_manifest(n, rows, n@emits)
  tab <- man@params
  list(manifest = man, refused = refused,
       table = data.frame(node = rep(n@id, nrow(tab)), tab[, c("item", "instance",
                                                              "status", "run_id")],
                          stringsAsFactors = FALSE))
}

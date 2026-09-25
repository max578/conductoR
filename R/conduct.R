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
                        "Refusing to run the step rather than coerce across ",
                        "a contract boundary."),
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
    triangulate  = .op_triangulate(node, inputs))
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
#' @param plan An [oap_plan].
#' @param data The data object every tool receives.
#' @param out_dir A directory to write the provenance record into, or `NULL`
#'   to write nothing. See [run_provenance()] for the files.
#' @param verbose Print one line per node as it runs.
#'
#' @returns A list with elements `plan_id`, `bus` (the manifest each node
#'   emitted, named by node id), `provenance` (one record per node),
#'   `n_nodes`, `n_skipped`, `seconds` and `terminal` (the manifest of the
#'   last node that ran). Returned invisibly.
#'
#' @seealso [conduct_step()], [run_provenance()], [plan_to_yaml()].
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
#' @export
conduct <- function(plan, data = list(), out_dir = NULL, verbose = TRUE) {
  stopifnot(S7::S7_inherits(plan, oap_plan))
  order    <- plan_order(plan)
  by_id    <- stats::setNames(plan@nodes, .node_ids(plan))
  children <- .child_map(plan)
  bus <- list()
  prov <- list()
  skipped <- character(0)
  last_produced <- NA_character_
  t0 <- Sys.time()

  record <- function(id, operator, consumes, emits, emitter, run_id,
                     data_hash, abstained, seconds, tag = "") {
    prov[[length(prov) + 1L]] <<- list(
      node = id, role = by_id[[id]]@role, operator = operator,
      consumes = consumes, emits = emits, emitter = emitter, run_id = run_id,
      data_hash = data_hash, abstained = abstained, seconds = round(seconds, 3))
    if (verbose) {
      cat(sprintf("  [%-12s] %-14s %-20s -> %-18s %s\n", operator, id,
                  if (length(consumes)) paste(consumes, collapse = "+") else "",
                  emits, tag))
    }
  }

  for (id in order) {
    n  <- by_id[[id]]
    cn <- .edge_sources(n)
    if (id %in% skipped) {
      record(id, "skipped", cn, "(skipped)", "--", "--", "--", NA, 0,
             tag = "[halt-branch]")
      next
    }
    inputs <- stats::setNames(lapply(cn, function(from) bus[[from]]), cn)
    ts  <- Sys.time()
    out <- conduct_step(n, inputs, data)
    dt  <- as.numeric(difftime(Sys.time(), ts, units = "secs"))
    bus[[id]] <- out
    last_produced <- id

    if (n@operator == "abstain_gate" && .abstained(out) &&
        identical(out@summary$metrics$mode, "halt-branch")) {
      skipped <- union(skipped, .descendants(children, id))
    }

    record(id, n@operator, cn, out@inferential_target, out@emitter_package,
           out@run_id, out@data_hash, .abstained(out), dt,
           tag = if (.abstained(out)) "[ABSTAIN]" else "")
  }

  total <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  terminal <- if (!is.na(last_produced)) bus[[last_produced]] else NULL
  run <- list(plan_id = plan@id, bus = bus, provenance = prov,
              n_nodes = length(order), n_skipped = length(skipped),
              seconds = round(total, 3), terminal = terminal)
  if (!is.null(out_dir)) .write_provenance(run, out_dir)
  invisible(run)
}

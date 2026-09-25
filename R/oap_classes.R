# oap_classes.R -- The plan classes and their constructors.
#
# A plan is a directed acyclic graph of nodes. A node names one analytical
# step: the tool that performs it, the typed in-edges it consumes and the
# inferential target its output manifest must declare.

#' A node of an analytical plan
#'
#' One step of a plan, bound to a tool with the uniform signature
#' `tool(inputs, data, node)` that returns an
#' [orchestraManifest::orchestra_manifest]. The node declares which upstream
#' nodes it consumes, each with the contract it expects that input to carry,
#' and the inferential target its own output must declare.
#'
#' @details
#' `operator` distinguishes ordinary computation from the two epistemic node
#' types the engine performs itself. A `"compute"` node calls its tool. An
#' `"abstain_gate"` node reads the one verdict it consumes and passes or
#' abstains, see [conduct()] for the policy. A `"triangulate"` node reconciles
#' every lens it consumes into one consensus.
#'
#' A tool may be an in-session closure or a string reference that
#' [resolve_tool()] resolves when the node runs. The reference form is what
#' makes a plan portable, see [plan_to_yaml()].
#'
#' @param id A single non-empty string, unique within the plan.
#' @param role A free-text label for the component the node stands for.
#' @param tool The closure that performs the node, or an unused stand-in when
#'   `tool_ref` is set.
#' @param tool_ref A tool reference in one of the forms [resolve_tool()]
#'   accepts, or `""` for an in-session closure.
#' @param consumes A list of edges, each as returned by [edge()].
#' @param emits The inferential target the node's output must declare, one of
#'   [orchestraManifest::inferential_targets()] or `"any"`.
#' @param operator One of `"compute"`, `"abstain_gate"` or `"triangulate"`.
#' @param params A named list of operator parameters.
#'
#' @returns An `oap_node` object.
#'
#' @seealso [node()] for the declarative constructor, [oap_plan] for the
#'   plan.
#'
#' @author Max Moldovan
#'
#' @examples
#' n <- oap_node(id = "lens", emits = "treatment_effects")
#' n@id
#'
#' @export
oap_node <- S7::new_class(
  "oap_node",
  properties = list(
    id       = S7::class_character,
    role     = S7::new_property(S7::class_character, default = "compute"),
    tool     = S7::new_property(S7::class_function,
                                default = quote(function(...) NULL)),
    tool_ref = S7::new_property(S7::class_character, default = ""),
    consumes = S7::new_property(S7::class_list, default = quote(list())),
    emits    = S7::new_property(S7::class_character, default = "any"),
    operator = S7::new_property(S7::class_character, default = "compute"),
    params   = S7::new_property(S7::class_list, default = quote(list()))
  ),
  validator = function(self) {
    errs <- character(0)
    if (length(self@id) != 1L || !nzchar(self@id)) {
      errs <- c(errs, "node `id` must be a single non-empty string")
    }
    if (length(self@operator) != 1L || !self@operator %in% .OAP_OPERATORS) {
      errs <- c(errs, sprintf("`operator` must be one of {%s}",
                              paste(.OAP_OPERATORS, collapse = ", ")))
    }
    if (length(self@emits) != 1L || !self@emits %in% .OAP_CONTRACTS) {
      errs <- c(errs, sprintf("`emits` must be one of {%s}",
                              paste(.OAP_CONTRACTS, collapse = ", ")))
    }
    for (e in self@consumes) {
      if (is.null(e$from) || is.null(e$contract)) {
        errs <- c(errs, "each `consumes` edge needs `from` and `contract`")
      } else if (!e$contract %in% .OAP_CONTRACTS) {
        errs <- c(errs, sprintf("edge contract '%s' is not a known target",
                                e$contract))
      }
    }
    if (length(errs) == 0L) NULL else paste(errs, collapse = "; ")
  }
)

#' An analytical plan
#'
#' A plan is a list of [oap_node] objects with unique ids whose edges all
#' point at nodes within the plan. Acyclicity is checked when the plan is
#' ordered, see [plan_order()].
#'
#' @param id A single string naming the plan.
#' @param nodes A list of [oap_node] objects.
#' @param meta A named list of free-form metadata, such as the question the
#'   plan answers. Serialised with the plan.
#'
#' @returns An `oap_plan` object.
#'
#' @seealso [conduct()] to perform a plan, [plan_to_yaml()] to save one.
#'
#' @author Max Moldovan
#'
#' @examples
#' p <- oap_plan(id = "one_lens", nodes = list(
#'   node("lens", emits = "treatment_effects")
#' ))
#' plan_order(p)
#'
#' @export
oap_plan <- S7::new_class(
  "oap_plan",
  properties = list(
    id    = S7::class_character,
    nodes = S7::new_property(S7::class_list, default = quote(list())),
    meta  = S7::new_property(S7::class_list, default = quote(list()))
  ),
  validator = function(self) {
    if (length(self@id) != 1L || !nzchar(self@id)) {
      return("plan `id` must be a single non-empty string")
    }
    for (n in self@nodes) {
      if (!S7::S7_inherits(n, oap_node)) return("every node must be an oap_node")
    }
    ids <- .node_ids(self)
    if (anyDuplicated(ids)) return("node ids must be unique within a plan")
    for (n in self@nodes) for (e in n@consumes) {
      if (!is.null(e$from) && !e$from %in% ids) {
        return(sprintf("node '%s' consumes unknown node '%s'", n@id, e$from))
      }
    }
    NULL
  }
)

#' Declare a node
#'
#' Builds an [oap_node] so a plan reads as a declaration. `tool` is either a
#' closure or a string reference; a string is stored as `tool_ref` and
#' resolved by [resolve_tool()] when the node runs.
#'
#' @inheritParams oap_node
#' @param tool A closure `function(inputs, data, node)` returning an
#'   [orchestraManifest::orchestra_manifest], or a string reference in one of
#'   the forms [resolve_tool()] accepts.
#'
#' @returns An [oap_node].
#'
#' @seealso [edge()], [oap_plan].
#'
#' @author Max Moldovan
#'
#' @examples
#' lens <- node("lens", tool = "my_lens", emits = "treatment_effects")
#' lens@tool_ref
#' gate <- node("gate", consumes = list(edge("lens", "treatment_effects")),
#'              emits = "decisions", operator = "abstain_gate",
#'              params = list(n_min = 25L))
#' gate@operator
#'
#' @export
node <- function(id, tool = function(...) NULL, consumes = list(),
                 emits = "any", operator = "compute", role = "compute",
                 params = list()) {
  tref <- ""
  if (is.character(tool)) {
    tref <- tool
    tool <- function(...) NULL
  }
  oap_node(id = id, tool = tool, tool_ref = tref, consumes = consumes,
           emits = emits, operator = operator, role = role, params = params)
}

#' Declare a typed edge
#'
#' @param from The id of the upstream node.
#' @param contract The inferential target the upstream manifest must declare,
#'   or `"any"` to accept every target.
#'
#' @returns A list with elements `from` and `contract`.
#'
#' @seealso [node()].
#'
#' @author Max Moldovan
#'
#' @examples
#' edge("lens", "treatment_effects")
#'
#' @export
edge <- function(from, contract = "any") {
  list(from = from, contract = contract)
}

#' Topological order of a plan
#'
#' Orders the nodes so that every node follows the nodes it consumes. A plan
#' with a cycle is an error.
#'
#' @param plan An [oap_plan].
#'
#' @returns A character vector of node ids in execution order.
#'
#' @author Max Moldovan
#'
#' @examples
#' p <- oap_plan(id = "chain", nodes = list(
#'   node("b", consumes = list(edge("a"))),
#'   node("a")
#' ))
#' plan_order(p)
#'
#' @export
plan_order <- function(plan) {
  stopifnot(S7::S7_inherits(plan, oap_plan))
  ids  <- .node_ids(plan)
  deps <- stats::setNames(lapply(plan@nodes, function(n) unique(.edge_sources(n))),
                          ids)
  ordered   <- character(0)
  remaining <- ids
  repeat {
    ready <- remaining[vapply(remaining, function(i) all(deps[[i]] %in% ordered),
                              logical(1))]
    if (length(ready) == 0L) break
    ordered   <- c(ordered, ready)
    remaining <- setdiff(remaining, ready)
  }
  if (length(remaining) > 0L) {
    stop(sprintf("plan '%s' is cyclic; unresolved nodes: %s",
                 plan@id, paste(remaining, collapse = ", ")), call. = FALSE)
  }
  ordered
}

#' Fetch a node by id
#'
#' @param plan An [oap_plan].
#' @param id The id of the node.
#'
#' @returns The [oap_node] with that id. An unknown id is an error.
#'
#' @author Max Moldovan
#'
#' @examples
#' p <- oap_plan(id = "one", nodes = list(node("a")))
#' plan_node(p, "a")@id
#'
#' @export
plan_node <- function(plan, id) {
  for (n in plan@nodes) if (n@id == id) return(n)
  stop(sprintf("no node '%s' in plan '%s'", id, plan@id), call. = FALSE)
}

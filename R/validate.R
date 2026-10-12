# validate.R -- Check a plan file against the plan language and report every
# problem at once. Mirrors inst/schema/; references between nodes, cycles and
# conditions are checked only here.

.PLAN_FIELDS <- c("id", "plan_version", "meta", "nodes")
.NODE_FIELDS <- c("id", "role", "tool", "operator", "emits", "consumes",
                  "params", "when", "foreach", "foreach_max", "on_refusal")
.ON_REFUSAL  <- c("continue", "halt-branch", "stop")
.GATE_MODES  <- c("flag", "halt-branch", "reroute")
.YAML_MARK   <- "\001"
.SCI_PATTERN <- "^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)[eE][-+]?[0-9]+$"

.prob <- function(field, rule, fix) list(field = field, rule = rule, fix = fix)

.is_string <- function(x) is.character(x) && length(x) == 1L && !is.na(x)

.is_map <- function(x) {
  is.list(x) && (length(x) == 0L || (!is.null(names(x)) && all(nzchar(names(x)))))
}

.is_whole <- function(x) {
  is.numeric(x) && length(x) == 1L && is.finite(x) && x == round(x)
}

# ---------------------------------------------------------------
# Reading
# ---------------------------------------------------------------

# Plain scalars that YAML 1.1 would turn into a logical or an unusual number
# are kept as marked text, so the validator can name them.
.yaml_handlers <- function() {
  word <- function(x) paste0(.YAML_MARK, "word:", x)
  num  <- function(x) paste0(.YAML_MARK, "number:", x)
  list("bool#yes" = word, "bool#no" = word, "float#exp" = num,
       "int#hex" = num, "int#oct" = num, "int#base60" = num,
       "float#base60" = num)
}

.read_yaml_spec <- function(x) {
  is_file <- length(x) == 1L && !grepl("\n", x, fixed = TRUE) && file.exists(x)
  tryCatch(
    if (is_file) {
      yaml::read_yaml(x, handlers = .yaml_handlers())
    } else {
      yaml::yaml.load(x, handlers = .yaml_handlers())
    },
    error = function(e) {
      stop(sprintf("the plan is not valid YAML: %s", conditionMessage(e)),
           call. = FALSE)
    })
}

# JSON arrays of scalars of one type become vectors, as YAML sequences do,
# so one plan reads the same from either spelling.
.simplify_json <- function(x) {
  if (!is.list(x)) return(x)
  x <- lapply(x, .simplify_json)
  if (length(x) > 0L && is.null(names(x)) &&
      all(vapply(x, function(e) is.atomic(e) && length(e) == 1L, logical(1)))) {
    cls <- unique(vapply(x, function(e) class(e)[1], character(1)))
    if (length(cls) == 1L) return(unlist(x))
  }
  x
}

.read_json_spec <- function(x) {
  is_file <- length(x) == 1L && !grepl("\n", x, fixed = TRUE) &&
    !startsWith(trimws(x), "{") && file.exists(x)
  txt <- if (is_file) paste(readLines(x, warn = FALSE), collapse = "\n") else x
  spec <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                   error = function(e) {
                     stop(sprintf("the plan is not valid JSON: %s",
                                  conditionMessage(e)), call. = FALSE)
                   })
  .simplify_json(spec)
}

.looks_json <- function(x) {
  if (length(x) == 1L && !grepl("\n", x, fixed = TRUE) && file.exists(x)) {
    return(grepl("\\.json$", x, ignore.case = TRUE))
  }
  startsWith(trimws(x), "{")
}

# ---------------------------------------------------------------
# Marked words and numbers
# ---------------------------------------------------------------

# Turns unquoted true/false into logicals and reports the other marked
# scalars and any number written in scientific notation.
.resolve_marks <- function(x, path, env) {
  if (is.list(x)) {
    nm <- names(x)
    for (i in seq_along(x)) {
      sub <- if (!is.null(nm) && nzchar(nm[i])) {
        if (nzchar(path)) paste0(path, ".", nm[i]) else nm[i]
      } else {
        sprintf("%s[%d]", path, i)
      }
      v <- .resolve_marks(x[[i]], sub, env)
      if (!identical(v, x[[i]])) {
        if (is.null(v)) x[i] <- list(NULL) else x[[i]] <- v
      }
    }
    return(x)
  }
  if (!is.character(x) || length(x) == 0L) return(x)
  marked <- startsWith(x, .YAML_MARK)
  sci <- !marked & grepl(.SCI_PATTERN, x)
  if (!any(marked | sci)) return(x)
  out <- as.list(x)
  for (i in seq_along(x)) {
    p <- if (length(x) > 1L) sprintf("%s[%d]", path, i) else path
    if (sci[i]) {
      env$add(p, sprintf(paste0("`%s` is a number in scientific notation, ",
                                "which is read as text"), x[i]),
              sprintf("write the number plainly: %s", .plain_number(x[i])))
    } else if (marked[i]) {
      body <- sub(paste0("^", .YAML_MARK), "", x[i])
      kind <- sub(":.*$", "", body)
      val  <- sub("^[a-z]+:", "", body)
      out[[i]] <- val
      if (kind == "word" && tolower(val) %in% c("true", "false")) {
        out[[i]] <- tolower(val) == "true"
      } else if (kind == "word") {
        env$add(p, sprintf(paste0("`%s` is written without quotes, and YAML ",
                                  "reads it as a logical"), val),
                sprintf("write true or false for a logical, or \"%s\" in quotes for text", val))
      } else if (grepl("[eE]", val)) {
        env$add(p, sprintf("`%s` is in scientific notation", val),
                sprintf("write the number plainly: %s", .plain_number(val)))
      } else {
        env$add(p, sprintf("`%s` is not a plain decimal number", val),
                "write the number in plain decimal digits, or quote it for text")
      }
    }
  }
  if (all(vapply(out, is.logical, logical(1)))) return(unlist(out))
  if (all(vapply(out, is.character, logical(1)))) return(unlist(out))
  out
}

.plain_number <- function(x) {
  v <- suppressWarnings(as.numeric(x))
  if (is.na(v)) return("digits only")
  format(v, scientific = FALSE, trim = TRUE, digits = 15)
}

# ---------------------------------------------------------------
# Rules shared by files and in-session plans
# ---------------------------------------------------------------

.cond_fix <- function(problem, general) {
  if (grepl("plain number", problem, fixed = TRUE)) {
    "write numbers in plain decimal form: 0.05, 1000"
  } else {
    general
  }
}

# Rules of the v0.2 fields of one node. Returns a list of problems.
.node_rules <- function(id, operator, emits, consumes, params, when, foreach,
                        foreach_max, on_refusal) {
  out <- list()
  froms <- vapply(consumes, function(e) as.character(e$from %||% ""),
                  character(1))
  if (identical(operator, "select")) {
    if (length(consumes) != 1L) {
      out <- c(out, list(.prob("consumes", "a select node consumes exactly one node",
                               "list the one node whose rows it selects")))
    }
    if (!.is_string(params$rule)) {
      out <- c(out, list(.prob("params.rule", "a select node needs a rule",
                               "write the rule as a quoted string: \"p_value < 0.05\"")))
    } else {
      for (m in .cond_check(params$rule, "rule")$problems) {
        out <- c(out, list(.prob("params.rule", m, .cond_fix(m, sprintf(
          "a rule may use column names and %s", .COND_ALLOWED)))))
      }
    }
    contract <- if (length(consumes) == 1L) consumes[[1]]$contract %||% "any" else "any"
    if (!emits %in% c("any", contract) && contract != "any") {
      out <- c(out, list(.prob("emits", sprintf(
        "a select node emits what it consumes ('%s'), not '%s'", contract, emits),
        sprintf("set emits to \"%s\"", contract))))
    }
  }
  if (nzchar(when)) {
    for (m in .cond_check(when, "when")$problems) {
      out <- c(out, list(.prob("when", m, .cond_fix(m, sprintf(
        "a condition may use <node>.headline, <node>.abstained, <node>.n_rows, <node>.metrics.<name>, literals and %s",
        .COND_ALLOWED)))))
    }
  }
  if (!on_refusal %in% .ON_REFUSAL) {
    out <- c(out, list(.prob("on_refusal", sprintf("`%s` is not a refusal policy", on_refusal),
                             "use continue, halt-branch or stop")))
  }
  if (!.is_whole(foreach_max) || foreach_max < 1) {
    out <- c(out, list(.prob("foreach_max", "must be a whole number of at least 1",
                             "write a plain whole number; the default is 50")))
  }
  if (!is.null(foreach)) {
    if (!identical(operator, "compute")) {
      out <- c(out, list(.prob("foreach", "only a compute node can run once per item",
                               "move foreach to the compute node that does the work")))
    }
    if (identical(emits, "any")) {
      out <- c(out, list(.prob("emits", "a foreach node must declare what it emits",
                               "set emits to the inferential target its tool returns")))
    }
    if (!is.null(params$item)) {
      out <- c(out, list(.prob("params.item", "foreach sets params.item for each item",
                               "remove item from params")))
    }
    if (!is.null(names(foreach))) {
      if (!setequal(names(foreach), c("from", "column")) ||
          !.is_string(foreach$from) || !.is_string(foreach$column)) {
        out <- c(out, list(.prob("foreach", "a foreach from a result needs exactly `from` and `column`",
                                 "write foreach: {from: <node id>, column: <column name>}")))
      } else if (!foreach$from %in% froms) {
        out <- c(out, list(.prob("foreach.from", sprintf(
          "'%s' is not a node this node consumes", foreach$from),
          sprintf("add an edge from '%s' to consumes", foreach$from))))
      }
    } else {
      ok <- vapply(foreach, function(e) {
        (is.character(e) || is.numeric(e)) && length(e) == 1L && !is.na(e)
      }, logical(1))
      if (!all(ok)) {
        out <- c(out, list(.prob("foreach", "every item must be a single string or number",
                                 "write the items as a list of plain values")))
      } else if (anyDuplicated(vapply(foreach, as.character, character(1)))) {
        out <- c(out, list(.prob("foreach", "items must be distinct",
                                 "remove the repeated items")))
      }
      if (.is_whole(foreach_max) && length(foreach) > foreach_max) {
        out <- c(out, list(.prob("foreach", sprintf(
          "%d items is more than foreach_max (%d)", length(foreach), as.integer(foreach_max)),
          "raise foreach_max or shorten the list")))
      }
    }
  }
  out
}

.node_v02_problems <- function(n) {
  probs <- .node_rules(n@id, n@operator, n@emits, n@consumes, n@params,
                       n@when, n@foreach, n@foreach_max, n@on_refusal)
  vapply(probs, function(p) {
    sprintf("node '%s', %s: %s. Fix: %s.", n@id, p$field, p$rule, p$fix)
  }, character(1))
}

# Nodes upstream of `id`, following consumes edges.
.ancestors <- function(deps, id) {
  out <- character(0)
  frontier <- deps[[id]] %||% character(0)
  while (length(frontier)) {
    nxt <- setdiff(frontier, out)
    out <- union(out, nxt)
    frontier <- unique(unlist(deps[nxt]))
  }
  out
}

# A `when` may read only nodes upstream of its own node.
.graph_rules <- function(ids, deps, whens) {
  out <- list()
  for (id in ids) {
    w <- whens[[id]]
    if (is.null(w) || !nzchar(w)) next
    up <- .ancestors(deps, id)
    for (r in .cond_check(w, "when")$refs) {
      if (!r %in% up) {
        out[[length(out) + 1L]] <- c(node = id, .prob("when", sprintf(
          "reads '%s', which is not upstream of '%s'", r, id),
          sprintf("add an edge from '%s' to consumes, or read a node that is upstream", r)))
      }
    }
  }
  out
}

.plan_v02_problems <- function(nodes) {
  ids <- vapply(nodes, function(n) n@id, character(1))
  deps <- stats::setNames(lapply(nodes, .edge_sources), ids)
  whens <- stats::setNames(lapply(nodes, function(n) n@when), ids)
  vapply(.graph_rules(ids, deps, whens), function(p) {
    sprintf("node '%s', %s: %s. Fix: %s.", p$node, p$field, p$rule, p$fix)
  }, character(1))
}

# ---------------------------------------------------------------
# Checking a plan read from a file
# ---------------------------------------------------------------

.validate_spec <- function(spec) {
  env <- new.env(parent = emptyenv())
  env$rows <- list()
  env$seen <- character(0)
  env$ids <- character(0)
  add_row <- function(node, field, rule, fix) {
    key <- paste(node, field, sep = "\r")
    if (key %in% env$seen) return(invisible())
    env$seen <- c(env$seen, key)
    env$rows[[length(env$rows) + 1L]] <- data.frame(
      field = field, node = node, rule = rule, fix = fix,
      stringsAsFactors = FALSE)
    invisible()
  }
  env$add <- function(path, rule, fix) {
    m <- regmatches(path, regexec("^nodes\\[([0-9]+)\\]\\.?(.*)$", path))[[1]]
    if (length(m)) {
      i <- as.integer(m[2])
      add_row(env$ids[i] %||% sprintf("#%d", i),
              if (nzchar(m[3])) m[3] else "(node)", rule, fix)
    } else {
      add_row("", path, rule, fix)
    }
  }
  if (.is_map(spec) && is.list(spec$nodes)) {
    env$ids <- vapply(seq_along(spec$nodes), function(i) {
      x <- spec$nodes[[i]]
      if (is.list(x) && .is_string(x$id)) x$id else sprintf("#%d", i)
    }, character(1))
  }
  spec <- .resolve_marks(spec, "", env)
  result <- function() {
    probs <- if (length(env$rows)) do.call(rbind, env$rows) else
      data.frame(field = character(0), node = character(0),
                 rule = character(0), fix = character(0),
                 stringsAsFactors = FALSE)
    list(spec = spec, problems = probs)
  }

  # Plan fields -------------------------------------------------------------
  if (!.is_map(spec) || length(spec) == 0L) {
    add_row("", "(plan)", "a plan must be a mapping of fields",
            "start from the example in plan_schema()")
    return(result())
  }
  for (k in setdiff(names(spec), .PLAN_FIELDS)) {
    add_row("", k, "is not a plan field",
            sprintf("plan fields are %s", paste(.PLAN_FIELDS, collapse = ", ")))
  }
  if (!.is_string(spec$id) || !nzchar(spec$id)) {
    add_row("", "id", "the plan needs an id", "write id: \"my_plan\"")
  }
  pv <- spec$plan_version
  if (!is.null(pv) && !(.is_string(pv) && pv %in% c("0.1", "0.2"))) {
    add_row("", "plan_version", sprintf("`%s` is not a plan version", format(pv)),
            "write plan_version: \"0.2\" in quotes")
  }
  if (!is.null(spec$meta) && !.is_map(spec$meta)) {
    add_row("", "meta", "meta must be a mapping", "write meta as key: value pairs")
  }
  nodes <- spec$nodes
  if (!is.list(nodes) || length(nodes) == 0L || !is.null(names(nodes))) {
    add_row("", "nodes", "a plan needs a list of one or more nodes",
            "write nodes: followed by one entry per node")
    return(result())
  }

  # Node fields -------------------------------------------------------------
  ok_nodes <- logical(length(nodes))
  for (i in seq_along(nodes)) {
    x <- nodes[[i]]
    nid <- env$ids[i]
    bad <- function(field, rule, fix) {
      add_row(nid, field, rule, fix)
      ok_nodes[i] <<- FALSE
    }
    ok_nodes[i] <- TRUE
    if (!.is_map(x) || length(x) == 0L) {
      bad("(node)", "a node must be a mapping of fields", "write id:, tool: and emits: at least")
      next
    }
    for (k in setdiff(names(x), .NODE_FIELDS)) {
      add_row(nid, k, "is not a node field",
              sprintf("node fields are %s", paste(.NODE_FIELDS, collapse = ", ")))
    }
    if (!.is_string(x$id) || !nzchar(x$id)) {
      bad("id", "the node needs an id", "write id: \"a_name\"")
    } else if (grepl("[.[:space:]]|\\[|\\]", x$id)) {
      bad("id", "a node id may not contain dots, square brackets or white space",
          "use letters, digits and underscores")
    }
    for (f in c("role", "tool", "when")) {
      if (!is.null(x[[f]]) && !.is_string(x[[f]])) {
        bad(f, "must be a single string", sprintf("write %s as text in quotes", f))
      }
    }
    op <- x$operator %||% "compute"
    if (!(.is_string(op) && op %in% .OAP_OPERATORS)) {
      bad("operator", sprintf("`%s` is not an operator", format(op)),
          sprintf("use one of %s", paste(.OAP_OPERATORS, collapse = ", ")))
      op <- ""
    }
    emits <- x$emits %||% "any"
    if (!(.is_string(emits) && emits %in% .OAP_CONTRACTS)) {
      bad("emits", sprintf("`%s` is not an inferential target", format(emits)),
          sprintf("use one of %s", paste(.OAP_CONTRACTS, collapse = ", ")))
    }
    if (op == "compute" && !(.is_string(x$tool) && nzchar(x$tool))) {
      bad("tool", "a compute node needs a tool",
          "name a registered tool, a pkg::fn or a script:<path>::<fn>")
    }
    cons <- x$consumes %||% list()
    if (!is.list(cons) || !is.null(names(cons))) {
      bad("consumes", "must be a list of edges", "write each edge as {from: <id>, contract: <target>}")
      cons <- list()
    }
    for (j in seq_along(cons)) {
      e <- cons[[j]]
      fj <- sprintf("consumes[%d]", j)
      if (!.is_map(e) || !.is_string(e$from)) {
        bad(fj, "an edge needs `from`", "write the edge as {from: <id>, contract: <target>}")
        next
      }
      for (k in setdiff(names(e), c("from", "contract"))) {
        bad(paste0(fj, ".", k), "is not an edge field", "an edge has from and contract")
      }
      if (!is.null(e$contract) &&
          !(.is_string(e$contract) && e$contract %in% .OAP_CONTRACTS)) {
        bad(paste0(fj, ".contract"), sprintf("`%s` is not an inferential target", format(e$contract)),
            sprintf("use one of %s", paste(.OAP_CONTRACTS, collapse = ", ")))
      }
    }
    params <- x$params %||% list()
    if (!.is_map(params)) {
      bad("params", "must be a mapping", "write params as key: value pairs")
      params <- list()
    }
    if (op == "abstain_gate") {
      if (length(cons) != 1L) {
        bad("consumes", "a gate consumes exactly one node", "list the one verdict the gate reads")
      }
      if (!is.null(params$mode) && !(.is_string(params$mode) && params$mode %in% .GATE_MODES)) {
        bad("params.mode", sprintf("`%s` is not a gate mode", format(params$mode)),
            "use flag, halt-branch or reroute, in quotes")
      }
      if (!is.null(params$n_min) && !(.is_whole(params$n_min) && params$n_min >= 0)) {
        bad("params.n_min", "must be a whole number", "write a plain whole number; the default is 25")
      }
      if (!is.null(params$z_floor) && !(is.numeric(params$z_floor) && length(params$z_floor) == 1L)) {
        bad("params.z_floor", "must be a number", "write a plain number; the default is 2.0")
      }
      fl <- params$floors
      if (!is.null(fl) && !(.is_map(fl) && all(vapply(fl, function(v) is.numeric(v) && length(v) == 1L, logical(1))))) {
        bad("params.floors", "must map metric names to numbers", "write floors: {metric: 10.0}")
      }
    }
    fe <- x$foreach
    if (!is.null(fe) && !is.atomic(fe) && !is.list(fe)) {
      bad("foreach", "must be a list of items or {from, column}", "see the foreach section of ?conduct")
      fe <- NULL
    }
    fmax <- x$foreach_max %||% 50L
    if (!(.is_whole(fmax) && fmax >= 1)) {
      bad("foreach_max", "must be a whole number of at least 1", "write a plain whole number; the default is 50")
      fmax <- 50L
    }
    onr <- x$on_refusal %||% "continue"
    if (!.is_string(onr)) {
      bad("on_refusal", "must be a single string", "use continue, halt-branch or stop")
      onr <- "continue"
    }
    if (!ok_nodes[i]) next
    if (!is.null(fe) && is.atomic(fe)) fe <- as.list(fe)
    edges <- lapply(cons, function(e) list(from = e$from, contract = e$contract %||% "any"))
    for (p in .node_rules(x$id, op, emits, edges, params, x$when %||% "", fe,
                          fmax, onr)) {
      bad(p$field, p$rule, p$fix)
    }
  }

  # Graph -------------------------------------------------------------------
  if (!all(ok_nodes)) return(result())
  ids <- vapply(nodes, function(x) x$id, character(1))
  for (d in unique(ids[duplicated(ids)])) {
    add_row(d, "id", "node ids must be unique", "rename one of the nodes")
  }
  deps <- stats::setNames(lapply(nodes, function(x) {
    vapply(x$consumes %||% list(), function(e) e$from, character(1))
  }), ids)
  for (id in unique(ids)) {
    for (f in setdiff(deps[[id]], ids)) {
      add_row(id, "consumes", sprintf("'%s' is not a node of this plan", f),
              "consume a node that the plan defines")
    }
  }
  if (anyDuplicated(ids) || !all(unlist(deps) %in% ids)) return(result())
  remaining <- ids
  repeat {
    ready <- remaining[vapply(remaining, function(i) {
      all(deps[[i]] %in% setdiff(ids, remaining))
    }, logical(1))]
    if (length(ready) == 0L) break
    remaining <- setdiff(remaining, ready)
  }
  if (length(remaining)) {
    add_row("", "nodes", sprintf("the plan has a cycle through %s",
                                 paste(remaining, collapse = ", ")),
            "remove an edge so that no node depends on itself")
    return(result())
  }
  whens <- stats::setNames(lapply(nodes, function(x) x$when %||% ""), ids)
  for (p in .graph_rules(ids, deps, whens)) add_row(p$node, p$field, p$rule, p$fix)
  result()
}

.format_problems <- function(probs) {
  lines <- sprintf("* %s%s: %s. Fix: %s.",
                   ifelse(nzchar(probs$node), sprintf("node '%s', ", probs$node), ""),
                   probs$field, probs$rule, probs$fix)
  paste0(sprintf("The plan has %d problem%s:\n", nrow(probs),
                 if (nrow(probs) == 1L) "" else "s"),
         paste(lines, collapse = "\n"))
}

# An empty mapping reads as a named empty list from JSON; plans store list().
.as_plain_list <- function(x) if (is.list(x) && length(x) == 0L) list() else x

.spec_to_plan <- function(spec) {
  v <- .validate_spec(spec)
  if (nrow(v$problems)) stop(.format_problems(v$problems), call. = FALSE)
  spec <- v$spec
  oap_plan(id = spec$id, meta = .as_plain_list(spec$meta %||% list()),
           nodes = lapply(spec$nodes, .node_from_list))
}

# ---------------------------------------------------------------
# Exported entry points
# ---------------------------------------------------------------

#' Check a plan file and list every problem
#'
#' Reads a plan written in YAML or JSON and checks it against the plan
#' language: the fields and their types, the operators and inferential
#' targets, the condition grammar, and the references between nodes. Every
#' problem is reported, not only the first, each with the field, the node,
#' the rule it breaks and how to fix it. A person or an AI model can repair a
#' plan from this table and check it again.
#'
#' @details
#' In a YAML plan, an unquoted `yes`, `no`, `on` or `off` is refused: write
#' `true` or `false` for a logical, or quote the word for text. A number in
#' scientific notation (`1e3`) is refused with its plain form (`1000`). The
#' structure checks follow the JSON Schema returned by [plan_schema()].
#'
#' @param x The path of a `.yaml`, `.yml` or `.json` plan file, or the plan's
#'   text. Text that starts with an opening brace is read as JSON.
#'
#' @returns A data frame with one row per problem and the columns `field`,
#'   `node` (empty for a plan-level field), `rule` and `fix`. A valid plan
#'   gives zero rows.
#'
#' @seealso [plan_schema()], [plan_from_yaml()], [plan_from_json()].
#'
#' @author Max Moldovan
#'
#' @examples
#' f <- system.file("scores", "dual_lens_gate.oap.yaml", package = "conductoR")
#' nrow(plan_validate(f))
#'
#' plan_validate('
#' id: broken
#' nodes:
#' - id: gate
#'   operator: abstain_gate
#'   consumes: [{from: lens, contract: treatment_effects}]
#'   params: {mode: off, n_min: 1e3}
#' ')
#'
#' @export
plan_validate <- function(x) {
  stopifnot(is.character(x), length(x) == 1L)
  spec <- tryCatch(
    if (.looks_json(x)) .read_json_spec(x) else .read_yaml_spec(x),
    error = function(e) e)
  if (inherits(spec, "error")) {
    return(data.frame(field = "(file)", node = "", rule = conditionMessage(spec),
                      fix = "correct the syntax at the place the message names",
                      stringsAsFactors = FALSE))
  }
  .validate_spec(spec)$problems
}

#' Read a plan from JSON
#'
#' The JSON spelling of a plan holds the same fields as the YAML spelling and
#' follows the schema returned by [plan_schema()]. A plan written as JSON,
#' for example as structured output from an AI model, can be read here and
#' saved as YAML with [plan_to_yaml()].
#'
#' @param path A file path, or a string of JSON text.
#'
#' @returns An [oap_plan]. A plan with problems is an error that lists all of
#'   them; see [plan_validate()].
#'
#' @seealso [plan_validate()], [plan_from_yaml()].
#'
#' @author Max Moldovan
#'
#' @examples
#' p <- plan_from_json('{
#'   "id": "one_lens",
#'   "nodes": [
#'     {"id": "lens", "tool": "my_lens", "emits": "treatment_effects"}
#'   ]
#' }')
#' plan_order(p)
#'
#' @export
plan_from_json <- function(path) {
  stopifnot(is.character(path), length(path) == 1L)
  .spec_to_plan(.read_json_spec(path))
}

#' The JSON Schema of the plan language
#'
#' The schema (JSON Schema 2020-12) describes every field a plan may hold,
#' its type and its allowed values. Editors can use it to check a plan as it
#' is written, and an AI model can use it to write a plan as structured
#' output. [plan_validate()] makes the same checks and adds those a schema
#' cannot express: references between nodes, cycles and the condition
#' grammar.
#'
#' @returns The path of the schema file, `oap_plan_v0.2.schema.json`.
#'
#' @seealso [plan_validate()].
#'
#' @author Max Moldovan
#'
#' @examples
#' s <- jsonlite::fromJSON(plan_schema(), simplifyVector = FALSE)
#' names(s$properties)
#'
#' @export
plan_schema <- function() {
  system.file("schema", "oap_plan_v0.2.schema.json", package = "conductoR",
              mustWork = TRUE)
}

# utils.R -- Small helpers shared across the package.

`%||%` <- function(x, y) if (is.null(x)) y else x

# A short hex digest of a manifest summary, used to build stable run ids.
# The digest is the contract's own payload hash over an empty payload plus
# the summary, so two emissions of the same verdict get the same id.
.summary_digest <- function(summ, n = 10L) {
  h <- orchestraManifest::manifest_data_hash(data.frame(), NULL, NULL, NULL,
                                             NA_integer_, summ)
  substr(sub("^sha256:", "", h), 1L, n)
}

# The version string this package writes into the manifests it emits.
.conductor_version <- function() {
  as.character(utils::packageVersion("conductoR"))
}

.node_ids <- function(plan) {
  vapply(plan@nodes, function(n) n@id, character(1))
}

.edge_sources <- function(node) {
  vapply(node@consumes, function(e) e$from, character(1))
}

# SHA-256 of any R object, computed by the contract's own payload hash with
# the object in the outputs slot.
.object_hash <- function(x) {
  orchestraManifest::manifest_data_hash(data.frame(), list(x), NULL, NULL,
                                        NA_integer_)
}

# The code a tool reference stands for: a script's text, a package
# function's body with the package version, or a closure's deparsed body.
.tool_code <- function(n) {
  ref <- n@tool_ref
  if (!nzchar(ref)) return(deparse(n@tool))
  if (startsWith(ref, "script:")) {
    path <- path.expand(strsplit(sub("^script:", "", ref), "::",
                                 fixed = TRUE)[[1]][1])
    return(if (file.exists(path)) readLines(path, warn = FALSE) else NA)
  }
  if (grepl("::", ref, fixed = TRUE)) {
    pkg <- strsplit(ref, "::", fixed = TRUE)[[1]][1]
    return(c(pkg, as.character(utils::packageVersion(pkg)),
             deparse(resolve_tool(ref))))
  }
  deparse(resolve_tool(ref))
}

# SHA-256 of a plan: its nodes, edges, parameters and the code each node
# runs, so editing a tool changes the hash as surely as editing the plan.
# A v0.2 field enters the hash only when it differs from its default.
.plan_hash <- function(plan) {
  spec <- lapply(plan@nodes, function(n) {
    c(list(id = n@id, role = n@role, operator = n@operator, emits = n@emits,
           consumes = n@consumes, params = n@params, tool_ref = n@tool_ref,
           code = if (n@operator == "compute") .tool_code(n) else NULL),
      .node_v02_fields(n))
  })
  .object_hash(list(id = plan@id, nodes = spec))
}

# Column names of a table for an error message.
.col_names <- function(tab) {
  if (ncol(tab)) paste(names(tab), collapse = ", ") else "(none)"
}

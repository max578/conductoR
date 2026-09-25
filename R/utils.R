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

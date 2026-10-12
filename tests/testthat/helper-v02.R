# helper-v02.R -- Tools for the plan language 0.2 tests.

# A tool that returns `tab` as the params table of a parameters manifest.
table_tool <- function(tab, headline = "flagged") {
  force(tab)
  function(inputs, data, node) {
    summ <- orchestraManifest::manifest_summary(headline,
                                                metrics = list(n = nrow(tab)))
    orchestraManifest::orchestra_manifest(
      emitter_package = "test", emitter_version = "0.0.0",
      inferential_target = "parameters", run_id = "table-1",
      method = "test:table", params = tab, summary = summ,
      timestamp = Sys.time(),
      data_hash = orchestraManifest::manifest_data_hash(tab, NULL, NULL, NULL,
                                                        NA_integer_, summ))
  }
}

terms_tab <- data.frame(
  term    = c("window", "window:site", "window:year", "site:year"),
  order   = c(1L, 2L, 2L, 2L),
  p_value = c(0.001, 0.01, 0.30, NA),
  stringsAsFactors = FALSE)

# A treatment-effects verdict that abstains.
refusing_lens <- function(inputs, data, node) {
  summ <- orchestraManifest::manifest_summary(
    "abstain", abstained = TRUE, abstain_reason = "too few sites",
    metrics = list(effect_present = FALSE))
  orchestraManifest::orchestra_manifest(
    emitter_package = "test", emitter_version = "0.0.0",
    inferential_target = "treatment_effects", run_id = "refuse-1",
    method = "test:refuse", summary = summ, timestamp = Sys.time(),
    data_hash = orchestraManifest::manifest_data_hash(data.frame(), NULL, NULL,
                                                      NULL, NA_integer_, summ))
}

# A per-item tool that refuses the items listed in `refuse` and records the
# items it was called with in `calls`.
item_tool <- function(refuse = character(0), calls = new.env()) {
  calls$items <- character(0)
  function(inputs, data, node) {
    it <- as.character(node@params$item)
    calls$items <- c(calls$items, it)
    if (it %in% refuse) return(refusing_lens(inputs, data, node))
    lens_to_manifest("effect", TRUE, "item", paste0("item:", it))
  }
}

counting_tool <- function(calls = new.env()) {
  calls$n <- 0L
  function(inputs, data, node) {
    calls$n <- calls$n + 1L
    lens_to_manifest("effect", TRUE, "count", "count:x")
  }
}

prov_of <- function(run, id) {
  pr <- run_provenance(run)
  pr[pr$node == id, , drop = FALSE]
}

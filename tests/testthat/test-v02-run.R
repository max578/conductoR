# when, foreach, on_refusal and select, each through pass, fail, empty and
# refusal (criterion C2).

# when ------------------------------------------------------------------------

test_that("when TRUE runs the node and records the condition", {
  p <- oap_plan(id = "w", nodes = list(
    node("a", effect_lens, emits = "treatment_effects"),
    node("b", effect_lens, emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects")),
         when = "a.headline == 'effect'")))
  r <- conduct(p, verbose = FALSE)
  expect_identical(prov_of(r, "b")$status, "ran")
  expect_identical(r$conditions$value, TRUE)
  expect_identical(r$conditions$condition, "a.headline == 'effect'")
})

test_that("when FALSE skips the node and its descendants, with the reason", {
  calls <- new.env()
  p <- oap_plan(id = "w", nodes = list(
    node("a", no_effect_lens, emits = "treatment_effects"),
    node("b", counting_tool(calls), emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects")),
         when = "a.headline == 'effect'"),
    node("c", counting_tool(calls), emits = "treatment_effects",
         consumes = list(edge("b", "treatment_effects"))),
    node("d", counting_tool(calls), emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects")))))
  r <- conduct(p, verbose = FALSE)
  expect_identical(calls$n, 1L)
  expect_identical(prov_of(r, "b")$status, "skipped")
  expect_match(prov_of(r, "b")$reason, "when `a.headline == 'effect'` is FALSE",
               fixed = TRUE)
  expect_match(prov_of(r, "c")$reason, "upstream node 'b' was skipped")
  expect_identical(prov_of(r, "d")$status, "ran")
  expect_identical(r$conditions$value, FALSE)
  expect_identical(r$n_skipped, 2L)
})

test_that("when on an empty selection is FALSE", {
  p <- oap_plan(id = "w", nodes = list(
    node("t", table_tool(terms_tab[0, ]), emits = "parameters"),
    node("b", effect_lens, emits = "treatment_effects",
         consumes = list(edge("t", "parameters")), when = "t.n_rows > 0")))
  r <- conduct(p, verbose = FALSE)
  expect_identical(prov_of(r, "b")$status, "skipped")
})

test_that("when can read a refusal upstream", {
  p <- oap_plan(id = "w", nodes = list(
    node("a", refusing_lens, emits = "treatment_effects"),
    node("b", effect_lens, emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects")),
         when = "!a.abstained")))
  r <- conduct(p, verbose = FALSE)
  expect_identical(prov_of(r, "a")$status, "refused")
  expect_identical(prov_of(r, "b")$status, "skipped")
})

test_that("a when that reads a node not upstream is refused when the plan is built", {
  expect_error(oap_plan(id = "w", nodes = list(
    node("a", effect_lens, emits = "treatment_effects"),
    node("b", effect_lens, emits = "treatment_effects",
         when = "a.headline == 'effect'"))),
    "reads 'a', which is not upstream of 'b'")
})

# foreach ---------------------------------------------------------------------

test_that("foreach over a fixed list runs once per item, in order", {
  calls <- new.env()
  p <- oap_plan(id = "f", nodes = list(
    node("v", item_tool(calls = calls), emits = "treatment_effects",
         foreach = c("north", "south"))))
  r <- conduct(p, verbose = FALSE)
  expect_identical(calls$items, c("north", "south"))
  expect_identical(r$items$instance, c("v[north]", "v[south]"))
  expect_identical(r$items$status, c("ran", "ran"))
  m <- r$bus$v
  expect_identical(m@inferential_target, "treatment_effects")
  expect_identical(names(m@outputs), c("v[north]", "v[south]"))
  expect_identical(m@summary$metrics$n_items, 2L)
  expect_true(orchestraManifest::verify_manifest(m)$ok)
})

test_that("foreach from a result takes each distinct value of the column", {
  calls <- new.env()
  tab <- data.frame(term = c("x", "y", "x"), stringsAsFactors = FALSE)
  p <- oap_plan(id = "f", nodes = list(
    node("t", table_tool(tab), emits = "parameters"),
    node("v", item_tool(calls = calls), emits = "treatment_effects",
         consumes = list(edge("t", "parameters")),
         foreach = list(from = "t", column = "term"))))
  r <- conduct(p, verbose = FALSE)
  expect_identical(calls$items, c("x", "y"))
})

test_that("foreach fails loudly past foreach_max, on a missing column and on a tool error", {
  p <- oap_plan(id = "f", nodes = list(
    node("v", item_tool(), emits = "treatment_effects",
         foreach = list(from = "t", column = "term"), foreach_max = 2L,
         consumes = list(edge("t", "parameters"))),
    node("t", table_tool(terms_tab), emits = "parameters")))
  expect_error(conduct(p, verbose = FALSE),
               "has 4 items, more than foreach_max = 2")
  p2 <- oap_plan(id = "f", nodes = list(
    node("t", table_tool(terms_tab), emits = "parameters"),
    node("v", item_tool(), emits = "treatment_effects",
         consumes = list(edge("t", "parameters")),
         foreach = list(from = "t", column = "nope"))))
  expect_error(conduct(p2, verbose = FALSE),
               "takes its items from column 'nope'")
  boom <- function(inputs, data, node) {
    if (node@params$item == "b") stop("tool failed on b")
    lens_to_manifest("effect", TRUE, "x", "x:y")
  }
  p3 <- oap_plan(id = "f", nodes = list(
    node("v", boom, emits = "treatment_effects", foreach = c("a", "b"),
         on_refusal = "continue")))
  expect_error(conduct(p3, verbose = FALSE), "tool failed on b")
  expect_error(node("v", emits = "treatment_effects", foreach = 1:3,
                    foreach_max = 2L), "3 items is more than foreach_max")
})

test_that("foreach over no items runs nothing and passes the record on", {
  calls <- new.env()
  seen <- new.env()
  reader <- function(inputs, data, node) {
    seen$n <- inputs$v@summary$metrics$n_items
    decision_to_manifest("none", "reader", "reader:x")
  }
  p <- oap_plan(id = "f", nodes = list(
    node("v", item_tool(calls = calls), emits = "treatment_effects",
         foreach = list()),
    node("d", reader, emits = "decisions",
         consumes = list(edge("v", "treatment_effects")))))
  r <- conduct(p, verbose = FALSE)
  expect_identical(calls$items, character(0))
  expect_identical(seen$n, 0L)
  expect_identical(nrow(r$items), 0L)
  expect_identical(prov_of(r, "d")$status, "ran")
})

test_that("a refused item is recorded and the policy decides what follows", {
  calls <- new.env()
  mk <- function(policy) oap_plan(id = "f", nodes = list(
    node("v", item_tool("b", calls), emits = "treatment_effects",
         foreach = c("a", "b", "c"), on_refusal = policy),
    node("d", effect_lens, emits = "treatment_effects",
         consumes = list(edge("v", "treatment_effects")))))
  r <- conduct(mk("continue"), verbose = FALSE)
  expect_identical(r$items$status, c("ran", "refused", "ran"))
  expect_identical(prov_of(r, "d")$status, "ran")

  r <- conduct(mk("halt-branch"), verbose = FALSE)
  expect_identical(r$items$status, c("ran", "refused", "skipped"))
  expect_identical(calls$items, c("a", "b"))
  expect_identical(prov_of(r, "d")$status, "skipped")
  expect_match(prov_of(r, "d")$reason, "'v' refused \\(on_refusal: halt-branch\\)")

  r <- conduct(mk("stop"), verbose = FALSE)
  expect_identical(r$stopped_by, "v")
  expect_match(prov_of(r, "d")$reason, "the run stopped")
})

# on_refusal ------------------------------------------------------------------

test_that("on_refusal continue passes a refusal on, as in 0.1.0", {
  p <- oap_plan(id = "r", nodes = list(
    node("a", refusing_lens, emits = "treatment_effects"),
    node("b", effect_lens, emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects")))))
  r <- conduct(p, verbose = FALSE)
  expect_identical(run_provenance(r)$status, c("refused", "ran"))
  expect_true(is.na(r$stopped_by))
})

test_that("on_refusal halt-branch skips only the descendants", {
  p <- oap_plan(id = "r", nodes = list(
    node("a", refusing_lens, emits = "treatment_effects",
         on_refusal = "halt-branch"),
    node("b", effect_lens, emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects"))),
    node("c", effect_lens, emits = "treatment_effects")))
  r <- conduct(p, verbose = FALSE)
  expect_identical(run_provenance(r)$status[match(c("a", "b", "c"),
                                                  run_provenance(r)$node)],
                   c("refused", "skipped", "ran"))
})

test_that("on_refusal stop ends the run and records every node left", {
  calls <- new.env()
  p <- oap_plan(id = "r", nodes = list(
    node("a", refusing_lens, emits = "treatment_effects", on_refusal = "stop"),
    node("b", counting_tool(calls), emits = "treatment_effects"),
    node("c", counting_tool(calls), emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects")))))
  d <- tempfile()
  r <- conduct(p, verbose = FALSE, out_dir = d)
  expect_identical(calls$n, 0L)
  expect_identical(r$stopped_by, "a")
  expect_identical(nrow(run_provenance(r)), 3L)
  expect_identical(r$n_skipped, 2L)
  txt <- readLines(file.path(d, "provenance.md"))
  expect_true("stopped_by: a" %in% txt)
  expect_true(any(grepl("### Skipped nodes", txt, fixed = TRUE)))
})

test_that("on_refusal does nothing when the node does not refuse", {
  p <- oap_plan(id = "r", nodes = list(
    node("a", effect_lens, emits = "treatment_effects", on_refusal = "stop"),
    node("b", effect_lens, emits = "treatment_effects")))
  r <- conduct(p, verbose = FALSE)
  expect_identical(r$n_skipped, 0L)
})

test_that("a tool error stops the run whatever on_refusal says", {
  boom <- function(inputs, data, node) stop("estimator failed")
  p <- oap_plan(id = "r", nodes = list(
    node("a", boom, emits = "treatment_effects", on_refusal = "continue")))
  expect_error(conduct(p, verbose = FALSE), "estimator failed")
})

# select ----------------------------------------------------------------------

select_plan <- function(rule, tab = terms_tab) {
  oap_plan(id = "s", nodes = list(
    node("t", table_tool(tab), emits = "parameters"),
    node("s", operator = "select", emits = "parameters",
         consumes = list(edge("t", "parameters")),
         params = list(rule = rule))))
}

test_that("select keeps the rows that meet the rule and counts NA rows", {
  r <- conduct(select_plan("order > 1 & p_value < 0.05"), verbose = FALSE)
  m <- r$bus$s
  expect_identical(m@params$term, "window:site")
  expect_identical(m@inferential_target, "parameters")
  expect_identical(m@summary$metrics$n_in, 4L)
  expect_identical(m@summary$metrics$n_na, 1L)
  expect_true(orchestraManifest::verify_manifest(m)$ok)
  expect_length(m@consumed_manifests, 1L)
})

test_that("select fails loudly on a column the table does not have", {
  expect_error(conduct(select_plan("q_value < 0.05"), verbose = FALSE),
               "reads column 'q_value'.*term, order, p_value")
})

test_that("select with no row meeting the rule emits an empty table", {
  r <- conduct(select_plan("p_value < 0"), verbose = FALSE)
  expect_identical(nrow(r$bus$s@params), 0L)
  expect_identical(names(r$bus$s@params), names(terms_tab))
  r <- conduct(select_plan("p_value < 0.05", terms_tab[0, ]), verbose = FALSE)
  expect_identical(nrow(r$bus$s@params), 0L)
})

test_that("select on a refused input still selects and does not refuse", {
  refusing_table <- function(inputs, data, node) {
    summ <- orchestraManifest::manifest_summary("abstain", abstained = TRUE)
    orchestraManifest::orchestra_manifest(
      emitter_package = "test", emitter_version = "0",
      inferential_target = "parameters", run_id = "rt",
      method = "test:rt", params = terms_tab, summary = summ,
      timestamp = Sys.time(),
      data_hash = orchestraManifest::manifest_data_hash(terms_tab, NULL, NULL,
                                                        NULL, NA_integer_, summ))
  }
  p <- oap_plan(id = "s", nodes = list(
    node("t", refusing_table, emits = "parameters"),
    node("s", operator = "select", emits = "parameters",
         consumes = list(edge("t", "parameters")),
         params = list(rule = "order == 1"))))
  r <- conduct(p, verbose = FALSE)
  expect_identical(run_provenance(r)$status, c("refused", "ran"))
  expect_identical(r$bus$s@params$term, "window")
})

test_that("a select node needs one input and a rule", {
  expect_error(node("s", operator = "select", params = list(rule = "x > 1")),
               "consumes exactly one node")
  expect_error(node("s", operator = "select",
                    consumes = list(edge("t", "parameters"))),
               "needs a rule")
  expect_error(node("s", operator = "select", emits = "decisions",
                    consumes = list(edge("t", "parameters")),
                    params = list(rule = "x > 1")),
               "emits what it consumes")
})

# The pattern the vignette shows ----------------------------------------------

test_that("select then foreach from the result runs a check per flagged term", {
  calls <- new.env()
  p <- oap_plan(id = "flag_then_check", nodes = list(
    node("terms", table_tool(terms_tab), emits = "parameters"),
    node("flagged", operator = "select", emits = "parameters",
         consumes = list(edge("terms", "parameters")),
         params = list(rule = "order > 1 & p_value < 0.05")),
    node("check", item_tool("window:site", calls), emits = "treatment_effects",
         consumes = list(edge("flagged", "parameters")),
         when = "flagged.n_rows > 0",
         foreach = list(from = "flagged", column = "term"))))
  d <- tempfile()
  r <- conduct(p, verbose = FALSE, out_dir = d)
  expect_identical(r$items$item, "window:site")
  expect_identical(r$items$status, "refused")
  card <- readLines(file.path(d, "run_card.md"))
  expect_true("### Items" %in% card)
  expect_true("### Conditions" %in% card)
})

test_that("compile_to_targets() refuses v0.2 fields with a typed error", {
  p <- oap_plan(id = "t", nodes = list(
    node("a", "stats::median", emits = "treatment_effects", foreach = c(1, 2))))
  err <- tryCatch(compile_to_targets(p, tempfile()), error = function(e) e)
  expect_s3_class(err, "conductoR_unsupported")
  expect_match(conditionMessage(err), "`foreach` \\(node 'a'\\)")
})

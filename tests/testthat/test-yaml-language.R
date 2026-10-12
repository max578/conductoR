# Reading and writing plans: the YAML traps fail loudly (criterion C4).

gate_yaml <- function(params) {
  paste0("id: p\nnodes:\n",
         "- id: lens\n  tool: t\n  emits: treatment_effects\n",
         "- id: gate\n  operator: abstain_gate\n  emits: decisions\n",
         "  consumes: [{from: lens, contract: treatment_effects}]\n",
         "  params: ", params, "\n")
}

test_that("C4 mode: off is refused with the quoting fix", {
  expect_error(plan_from_yaml(gate_yaml("{mode: off}")),
               paste0("node 'gate', params.mode: `off` is written without ",
                      "quotes, and YAML reads it as a logical. Fix: write ",
                      "true or false for a logical, or \"off\" in quotes for ",
                      "text."), fixed = TRUE)
  expect_error(plan_from_yaml(gate_yaml("{mode: \"off\"}")),
               "`off` is not a gate mode")
})

test_that("C4 n_min: 1e3 is refused with the plain form", {
  expect_error(plan_from_yaml(gate_yaml("{n_min: 1e3}")),
               paste0("node 'gate', params.n_min: `1e3` is a number in ",
                      "scientific notation, which is read as text. Fix: ",
                      "write the number plainly: 1000."), fixed = TRUE)
  expect_error(plan_from_yaml(gate_yaml("{z_floor: 1.0e+3}")),
               "`1.0e+3` is in scientific notation. Fix: write the number plainly: 1000.",
               fixed = TRUE)
  expect_error(plan_from_yaml(gate_yaml("{n_min: 2.5e-1}")),
               "write the number plainly: 0.25", fixed = TRUE)
})

test_that("C4 an unquoted yes or no anywhere is refused, a quoted one is text", {
  expect_error(plan_from_yaml(gate_yaml("{robust: yes}")),
               "params.robust: `yes` is written without quotes")
  expect_error(plan_from_yaml(gate_yaml("{levels: [low, no]}")),
               "params.levels\\[2\\]: `no` is written without quotes")
  expect_error(plan_from_yaml(paste0(gate_yaml("[]"), "meta: {sealed: on}\n")),
               "meta.sealed: `on` is written without quotes")
  p <- plan_from_yaml(gate_yaml("{label: \"no\", robust: true, strict: False}"))
  expect_identical(plan_node(p, "gate")@params,
                   list(label = "no", robust = TRUE, strict = FALSE))
})

test_that("unusual YAML 1.1 numbers are refused", {
  expect_error(plan_from_yaml(gate_yaml("{n_min: 0x1A}")),
               "`0x1A` is not a plain decimal number")
  expect_error(plan_from_yaml(gate_yaml("{n_min: 1:30}")),
               "`1:30` is not a plain decimal number")
})

test_that("every problem is reported at once", {
  probs <- plan_validate(gate_yaml("{mode: off, n_min: 1e3, robust: no}"))
  expect_identical(nrow(probs), 3L)
  expect_identical(probs$field, c("params.mode", "params.n_min", "params.robust"))
  expect_identical(unique(probs$node), "gate")
  expect_identical(names(probs), c("field", "node", "rule", "fix"))
  err <- tryCatch(plan_from_yaml(gate_yaml("{mode: off, n_min: 1e3}")),
                  error = conditionMessage)
  expect_match(err, "^The plan has 2 problems:")
})

test_that("plan-level problems name the field and leave the node empty", {
  probs <- plan_validate("id: p\nplan_version: 0.2\nversion: 2\nnodes:\n- id: a\n  tool: t\n")
  expect_setequal(probs$field, c("plan_version", "version"))
  expect_identical(unique(probs$node), "")
  expect_match(probs$fix[probs$field == "plan_version"], "\"0.2\" in quotes")
})

test_that("graph problems are found: unknown inputs, cycles, foreach sources", {
  probs <- plan_validate(paste0(
    "id: p\nnodes:\n- id: a\n  tool: t\n  consumes: [{from: b}]\n",
    "- id: b\n  tool: t\n  consumes: [{from: a}]\n"))
  expect_match(probs$rule, "cycle through a, b")
  probs <- plan_validate("id: p\nnodes:\n- id: a\n  tool: t\n  consumes: [{from: zz}]\n")
  expect_match(probs$rule, "'zz' is not a node of this plan")
  probs <- plan_validate(paste0(
    "id: p\nnodes:\n- id: a\n  tool: t\n  emits: parameters\n",
    "- id: b\n  tool: t\n  emits: decisions\n",
    "  foreach: {from: a, column: term}\n"))
  expect_match(probs$rule, "'a' is not a node this node consumes")
  probs <- plan_validate("id: p\nnodes:\n- id: a\n  tool: t\n- id: a\n  tool: t\n")
  expect_match(probs$rule, "node ids must be unique")
})

test_that("a file that is not YAML or JSON is reported as one problem", {
  probs <- plan_validate("id: [unclosed\n")
  expect_identical(probs$field, "(file)")
  expect_match(probs$rule, "not valid YAML")
  probs <- plan_validate("{\"id\": }")
  expect_match(probs$rule, "not valid JSON")
})

test_that("plan_to_yaml() quotes strings and writes numbers plainly", {
  p <- oap_plan(id = "q", meta = list(answer = "no", n = 1e6), nodes = list(
    node("a", "t", emits = "treatment_effects",
         params = list(mode = "off", alpha = 0.05, reps = 1000L, big = 1e6,
                       tiny = 1e-7, keep = TRUE, code = "1e3"))))
  txt <- plan_to_yaml(p)
  expect_match(txt, "mode: \"off\"", fixed = TRUE)
  expect_match(txt, "answer: \"no\"", fixed = TRUE)
  expect_match(txt, "big: 1000000.0", fixed = TRUE)
  expect_match(txt, "tiny: 0.0000001", fixed = TRUE)
  expect_match(txt, "keep: true", fixed = TRUE)
  expect_false(grepl("plan_version", txt, fixed = TRUE))
})

test_that("a plan using v0.2 fields says so and round-trips with its hash", {
  register_tool("rt_lens", effect_lens)
  p <- oap_plan(id = "rt2", nodes = list(
    node("a", "rt_lens", emits = "treatment_effects", on_refusal = "stop"),
    node("b", "rt_lens", emits = "treatment_effects",
         consumes = list(edge("a", "treatment_effects")),
         when = "a.metrics.n >= 10 & a.headline %in% c(\"effect\")",
         foreach = c("x", "y"), foreach_max = 5L,
         params = list(alpha = 0.05, flags = c(TRUE, FALSE)))))
  txt <- plan_to_yaml(p)
  expect_match(txt, "plan_version: \"0.2\"", fixed = TRUE)
  q <- plan_from_yaml(txt)
  expect_identical(.plan_hash(q), .plan_hash(p))
  expect_identical(plan_to_yaml(q), txt)
  expect_identical(plan_node(q, "b")@foreach, list("x", "y"))
})

test_that("a v0.2 field enters the hash only when it is set", {
  register_tool("rt_lens", effect_lens)
  base <- oap_plan(id = "h", nodes = list(node("a", "rt_lens")))
  same <- oap_plan(id = "h", nodes = list(
    node("a", "rt_lens", when = "", foreach = NULL, foreach_max = 50L,
         on_refusal = "continue")))
  expect_identical(.plan_hash(same), .plan_hash(base))
  for (changed in list(
    node("a", "rt_lens", on_refusal = "stop"),
    node("a", "rt_lens", emits = "decisions", foreach = list()),
    node("a", "rt_lens", foreach_max = 10L))) {
    p <- oap_plan(id = "h", nodes = list(changed))
    expect_false(identical(.plan_hash(p), .plan_hash(base)))
  }
})

test_that("an unknown node field is refused rather than ignored", {
  probs <- plan_validate("id: p\nnodes:\n- id: a\n  tool: t\n  on_refuse: stop\n")
  expect_identical(probs$field, "on_refuse")
  expect_match(probs$fix, "node fields are id, role")
})

test_that("an unknown field does not hide the other problems of its node", {
  probs <- plan_validate(paste0(
    "id: p\nnodes:\n- id: a\n  tool: t\n  emits: parameters\n",
    "- id: b\n  tool: t\n  consumes: [{from: a}]\n",
    "  foreach: {from: a, column: term}\n  on_refuse: continue\n"))
  expect_setequal(probs$field, c("on_refuse", "emits"))
})

# conductoR (development version)

* The package has a logo.
* README carries status badges and lists related packages.
* CI runs lintr (report-only) and test coverage.

# conductoR 0.2.0

## New features

* Plan language 0.2. Four optional node fields let a plan act on its own
  results; a plan that uses none of them runs and hashes as before.
  * `when` runs a node only if a condition on upstream results is true, and
    otherwise skips it and its descendants with the reason recorded.
  * `foreach` runs a compute node once per item, from a fixed list or from a
    column of an upstream `params` table; `foreach_max` (default 50) caps the
    number of items with an error, never a silent cut.
  * `on_refusal` sets what an abstaining result does: `"continue"` (default),
    `"halt-branch"` or `"stop"`. A tool error still stops the run.
  * The `select` operator keeps the rows of an upstream table that meet a
    rule and emits them under the same inferential target.
* Conditions and `select` rules are parsed by R's parser and read against a
  fixed grammar of references, plain literals and comparison and logical
  operators. They are never evaluated: a function call, `$`, `::`,
  backticks or an assignment is refused when the plan is built.
* `plan_validate()` checks a YAML or JSON plan and lists every problem at
  once, each with the field, the node, the rule broken and the fix.
* `plan_schema()` returns the JSON Schema (draft 2020-12) of the language,
  and `plan_from_json()` reads a plan written against it.
* `conduct()` records each skipped node with its reason, each `when`
  condition with its value, each `foreach` item with its status, and the
  node whose refusal stopped the run. `run_provenance()` gains `status` and
  `reason` columns, and the run card and provenance record list skipped
  nodes, conditions and items.
* `conduct()` records `plan_hash` (the plan and the code of every tool it
  references), `input_hash` (the data) and an optional `label` with each run,
  and writes them into `provenance.md`. `same_plan()` compares two runs, so a
  blinded run and the unblinded run that follows it can be shown to share
  one plan.

## Breaking changes

* `plan_from_yaml()` now refuses an unquoted `yes`, `no`, `on` or `off`,
  which YAML 1.1 reads as a logical, and a number in scientific notation,
  which it reads as text. Write `true` or `false`, quote the word, or write
  the number plainly. It also refuses unknown fields instead of ignoring
  them.

  ```yaml
  # 0.1.0 read this as mode = FALSE and n_min = "1e3"
  params: {mode: off, n_min: 1e3}
  # 0.2.0
  params: {mode: "flag", n_min: 1000}
  ```

* `plan_to_yaml()` writes every string in double quotes and every number in
  plain decimal form, and adds `plan_version: "0.2"` only when a 0.2 field is
  used.
* Node ids may not contain dots, square brackets or white space.

## Minor improvements and fixes

* `compile_to_targets()` refuses a plan that uses `when`, `foreach` or
  `on_refusal` with an error of class `conductoR_unsupported`.
* Windows builds no longer fail the `targets` test: the test writes the plan
  path with `deparse()`, so `C:\Users` is not read as a `\U` escape.

# conductoR 0.1.0

First release.

* `oap_plan`, `oap_node`, `node()` and `edge()` declare a plan whose edges are
  typed by the inferential targets of `orchestraManifest`.
* `conduct()` performs a plan in topological order and refuses a mistyped or
  tampered handoff before the downstream node runs; `conduct_step()` performs
  one node.
* The `abstain_gate` operator holds back a null verdict the design could not
  have resolved, with `flag`, `halt-branch` and `reroute` modes and named
  metric floors; the `triangulate` operator reconciles any number of lenses.
* `lens_to_manifest()` and `decision_to_manifest()` lift a verdict or a
  recommendation into the contract.
* `register_tool()`, `resolve_tool()`, `plan_to_yaml()` and `plan_from_yaml()`
  make a plan a portable file.
* `compile_to_targets()` and `terminal_target()` write a `targets` pipeline.
* `run_provenance()` and the files written under `out_dir` record every run.

# conductoR (development version)

* The package has a logo.
* README carries status badges and lists related packages.
* CI runs lintr (report-only) and test coverage.

## Refusals, plan hash and plan checks

* A node that refuses now leaves its abstention reason in the provenance
  (`run_provenance()$reason`), which stays with the data holder. A record or
  a return still carries the reason only when it is one of the error words.
* The error words gain `licence_unavailable` and `credential_unavailable`,
  for a tool that cannot reach a licence or read a credential, and
  `no_interior_minimum`, `no_interior_optimum` and `no_response`, for a
  fitted curve with no turning point inside the observed range or no
  response at all.
* `plan_hash()` gives the hash `conduct()` records, so a plan can be sealed
  before it runs.
* `plan_validate()` also takes a plan object, and says what it takes when
  given anything else.

## What returns to the composer

* `run_record()` gives the dose-0 record of a run: one row per node with its
  emitter, target, status (`completed`, `refused`, `skipped`, `failed`,
  `not_run`), the typed error words it reported and the declared checks it
  passed or failed, and one row per declared trigger with whether it fired.
  It holds no data value. `format_record()` turns it into the typed message
  lines: `stop`, `<node> <error word>` and `<trigger> signal:<type>`.
* `evaluate_triggers()` reads each declared trigger's statistic from its
  node's manifest and compares it with the threshold. The value stays in the
  object it returns; the record keeps only whether the trigger fired.
* `try_conduct()` runs a plan like `conduct()` and, when a node stops with an
  error, returns a failed run naming the node and the typed error word, so a
  run that fails still returns a message. `conduct()` itself is unchanged.
* `run_return()` builds the message to the composer at a dose: the record
  and its lines at dose 0, and at dose 1 or above the requested summary
  metrics and `params` columns, each passed through a checker. Values not
  returned are listed with the reason. `default_checker()` refuses a value
  resting on fewer than `k` units, one where a single unit holds more than a
  set share, and one with no unit count. It is a floor, not a disclosure
  guarantee.
* `write_return()` writes a return as a text file carrying the plan hash,
  input hash, series, round and dose, under `<series>/round_<k>/`, and never
  replaces one already written. `series_report()` lists the rounds of a
  series and counts the returns above dose 0 against a budget.

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

# conductoR 0.3.0

* New vignette, "Returning results from a run", follows one run on open
  maize nitrogen data from the input postconditions to a traced report.
* The package has a logo.
* README carries status badges and lists related packages.
* CI runs lintr (report-only) and test coverage.

## Tracing the numbers of a report

* `trace_report()` checks a report before release. A reference written
  `{{<node>:<field>}}` or `{{<node>:<field>:<index>}}` is filled in from the
  release's values. A number written out in the text is matched against
  those values at the digits shown, unless it is declared in `allow` (a
  confidence level, a year). A reference the release does not hold, or a
  number that matches nothing, blocks the release: the result then carries
  no text and a trace naming each failure with its reason. With the run
  given, the trace also names the version of the tool behind each value.

## Returns

* A node that refused returns only its refusal, which is in the record.
  Every field requested from it is refused as `node_refused`.
* A unit count may be declared on another node, as
  `n_units_field = "<node>.<metric>"` (e.g. `fidelity.n_trials`). A node
  that did not run, or has no such metric, gives no count.
* `run_return(previous =)` takes every earlier round of the series, as a
  list of returns or a vector of written files, and refuses a value that
  changed on few units since any of them, not only the last.
* `plan_returns()` warns when a declared return has no unit, since an
  undeclared value is scanned as a possible coordinate. A unit will be
  required from 0.4.0.

## Plan language 0.3 and input postconditions

* A plan may declare the tables it reads, in `inputs`, with the
  postconditions each must meet and an optional key (`plan_input()`).
  `conduct()` checks every one before the first node runs. When one does
  not hold, no node runs and the run stops with an error of class
  `postcondition_failed` that lists every failure with a count of the rows
  that fail and names no data value. `try_conduct()` records one line
  `<input>.<id> postcondition_failed` per failure, and the error words gain
  `postcondition_failed`.
* A postcondition is a condition in the plan grammar. A bare column name is
  checked row by row (`window_days <= 31`); `.n_rows`, `.n_cols`,
  `.n_missing.<column>`, `.n_distinct.<column>`, `.n_duplicated.<column>`,
  `.min.<column>`, `.max.<column>` and `.levels.<column>` summarise the
  table. `check_postconditions()` makes the same check on any table, so a
  script that writes a table by hand can check its output before writing
  it, against conditions of its own or those a plan declares.
* A node may declare what it may return (`returns`: field, dose, and
  optionally unit and the field holding its unit count), the triggers it
  carries (`triggers`) and the hypotheses it tests (`hypotheses`:
  statistic, direction, threshold, and what rehearsal planted to show the
  rule fires and stays silent). `plan_requests()`, `plan_triggers()` and
  `plan_hypotheses()` read them into the forms `run_return()` and
  `evaluate_triggers()` take, so a holder's runner needs only the plan file.
* `plan_to_yaml()` writes `plan_version: "0.3"` when a plan uses any of
  these fields. `plan_schema()` now returns the version 0.3 schema, and the
  version 0.2 schema stays installed. Every new field is optional: a 0.1 or
  0.2 plan reads, runs and hashes exactly as before.

## Return checks

* A request may declare the unit of its field. `default_checker()` gains a
  coordinate scan (`coord_bands`): a number inside a declared band with
  more than two decimals is refused as `coordinate_like`, unless the field
  is declared in a unit that is not a coordinate, so a rate in kg N/ha or a
  negative price difference is no longer withheld as a place.
  `default_checker()` also gains `forbidden`, strings no returned value may
  contain (`forbidden_string`).
* A `params` field may carry a row filter,
  `params.estimate[kind=state]`, so rows of two kinds in one table are
  requested apart and each is checked on its own unit counts.
* A declared `n_units_field` is now the only source of a value's unit
  count; a declared field that is missing is no count.
* `run_return()` gains `previous` and `k_diff`: a value that changed since
  the previous round of the series, given as a return or the path of a
  written one, is returned only when every row rests on at least `k_diff`
  units (`differencing`); a value missing in both rounds is unchanged.
* `run_return()` checks a `params` field row by row: a row the checker
  refuses, or a changed row resting on fewer than `k_diff` units, is
  withheld alone and the other rows are returned. `refused` gains `n_rows`,
  the number of rows withheld for each reason, without naming them; it is
  `NA` for a field refused whole. Before, one failing row withheld the
  whole column. A metric is still checked whole.
* Returned numbers are rounded to `digits` significant figures, or to the
  `digits` a request declares, and a `|` inside a returned value is
  escaped. `plan_returns()` reads every declared field of a plan's returns
  into the table `run_return(requests =)` takes.

## Secondary suppression

* `suppress_return()` applies the rules that look across cells to the cells
  of a release, after each has passed its own checks in `run_return()`:
  complementary withholding over declared partitions (parent to sub-cells,
  with whether unit and observation counts add), derivation closure over
  declared links (sum, difference, ratio, identity; a unit `"<u>"` holds at
  every unit), identical cells, zero-width intervals, by-design duplicates
  (pinned once an earlier return released them), and a route search over
  identities, two-term arithmetic and count-weighted means, broken by a
  greedy smallest hitting set. Each change names its rule. A withheld value
  with more than `route_limit` candidates has every candidate withheld and
  is logged; a route only pinned values make is listed as unbreakable. The
  rules fail closed and are a floor, not a disclosure guarantee.
  `run_return()` is unchanged.

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

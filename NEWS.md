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

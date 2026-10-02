# conductoR

Perform an analytical plan as a directed acyclic graph of nodes, each bound to a tool that returns an `orchestraManifest::orchestra_manifest`. Every edge is typed by the inferential target the upstream node declares, and a mismatched handoff stops the run before the downstream node executes.

Two operators are built in. An abstention gate refuses to pass a `no_effect` verdict from a design too small or too weak to have resolved the implied effect, and can flag, halt the branch or reroute. A triangulation reconciles any number of independent lenses on one question into one of seven consensus headlines.

A plan writes to YAML and reads back to the same run, compiles to a `targets` pipeline, and every run leaves a provenance record with the chain of payload hashes. Writing a plan from a question and a tool roster automatically is a future prospect.

```r
lens <- node("lens", tool = "ols_lens", emits = "treatment_effects")
gate <- node("gate", operator = "abstain_gate", emits = "decisions",
             consumes = list(edge("lens", "treatment_effects")), params = list(n_min = 25L))
run  <- conduct(oap_plan(id = "one", nodes = list(lens, gate)), data = my_data)
run$terminal@summary$headline
```

Install from GitHub with `remotes::install_github("max578/conductoR")`, which also installs `orchestraManifest`. See `vignette("conductoR")` for a worked example on `agridat::hernandez.nitrogen`. MIT licence.

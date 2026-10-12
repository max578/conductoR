# Plan hashes recorded with conductoR 0.1.0 (commit 68ef403, R 4.6.1), each
# tool registered as the stand-in `function(inputs, data, node) NULL`.

test_that("C1 every v0.1 plan loads under 0.2.0 with its 0.1.0 hash", {
  base <- utils::read.csv(test_path("fixtures", "plan_hashes_0.1.0.csv"),
                          stringsAsFactors = FALSE)
  stand_in <- as.function(alist(inputs = , data = , node = , NULL))
  for (i in seq_len(nrow(base))) {
    f <- system.file("scores", base$file[i], package = "conductoR")
    if (!nzchar(f)) f <- test_path("fixtures", base$file[i])
    p <- plan_from_yaml(f)
    for (n in p@nodes) {
      if (nzchar(n@tool_ref)) register_tool(n@tool_ref, stand_in)
    }
    expect_identical(p@id, base$plan_id[i])
    expect_identical(.plan_hash(p), base$plan_hash[i], label = base$file[i])
  }
})

test_that("C1 a v0.1 plan written by 0.2.0 still says nothing about v0.2", {
  f <- test_path("fixtures", "field_study_v0.1.oap.yaml")
  txt <- plan_to_yaml(plan_from_yaml(f))
  expect_false(grepl("plan_version", txt, fixed = TRUE))
})

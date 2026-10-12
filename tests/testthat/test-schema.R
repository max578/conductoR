# The JSON Schema and plan_validate() agree (criterion C5).

corpus <- function(kind) {
  list.files(test_path("fixtures", "corpus", kind), pattern = "\\.json$",
             full.names = TRUE)
}

read_text <- function(f) paste(readLines(f), collapse = "\n")

test_that("the schema is installed and lists the contract's targets", {
  s <- jsonlite::fromJSON(plan_schema(), simplifyVector = FALSE)
  expect_identical(s[["$schema"]], "https://json-schema.org/draft/2020-12/schema")
  expect_setequal(unlist(s[["$defs"]]$target$enum), .OAP_CONTRACTS)
  expect_setequal(unlist(s[["$defs"]]$node$properties$operator$enum),
                  .OAP_OPERATORS)
  expect_setequal(names(s[["$defs"]]$node$properties), .NODE_FIELDS)
  expect_setequal(names(s$properties), .PLAN_FIELDS)
  expect_setequal(unlist(s[["$defs"]]$node$properties$on_refusal$enum),
                  .ON_REFUSAL)
})

test_that("C5 the corpus holds 20 good and 20 bad plans", {
  expect_length(corpus("good"), 20L)
  expect_length(corpus("bad"), 20L)
})

test_that("C5 jsonvalidate and plan_validate() agree on every corpus plan", {
  skip_if_not_installed("jsonvalidate")
  v <- jsonvalidate::json_validator(plan_schema(), engine = "ajv")
  for (f in corpus("good")) {
    expect_true(v(read_text(f)), label = paste("schema:", basename(f)))
    expect_identical(nrow(plan_validate(f)), 0L,
                     label = paste("plan_validate:", basename(f)))
  }
  for (f in corpus("bad")) {
    expect_false(v(read_text(f)), label = paste("schema:", basename(f)))
    expect_gt(nrow(plan_validate(f)), 0L)
  }
})

test_that("every good plan builds, and reads the same from JSON and from YAML", {
  for (f in corpus("good")) {
    p <- plan_from_json(f)
    for (n in p@nodes) {
      if (nzchar(n@tool_ref)) register_tool(n@tool_ref, effect_lens)
    }
    q <- plan_from_yaml(plan_to_yaml(p))
    expect_identical(.plan_hash(q), .plan_hash(p), label = basename(f))
    expect_identical(plan_from_json(read_text(f))@id, p@id)
  }
})

test_that("every bad plan is refused when read, with all its problems", {
  for (f in corpus("bad")) {
    expect_error(plan_from_json(f), "The plan has [0-9]+ problems?:",
                 label = basename(f))
  }
})

test_that("checks a schema cannot make are made by plan_validate()", {
  base <- jsonlite::fromJSON(corpus("good")[grepl("06_when", corpus("good"))],
                             simplifyVector = FALSE)
  semantic <- list(
    function(x) { x$nodes[[2]]$when <- "system(\"ls\") == 0"; x },
    function(x) { x$nodes[[2]]$when <- "zz.headline == \"effect\""; x },
    function(x) { x$nodes[[2]]$consumes[[1]]$from <- "zz"; x },
    function(x) { x$nodes[[1]]$consumes <- list(list(from = "b")); x })
  skip_if_not_installed("jsonvalidate")
  v <- jsonvalidate::json_validator(plan_schema(), engine = "ajv")
  for (edit in semantic) {
    txt <- as.character(jsonlite::toJSON(edit(base), auto_unbox = TRUE))
    expect_true(v(txt))
    expect_gt(nrow(plan_validate(txt)), 0L)
  }
})

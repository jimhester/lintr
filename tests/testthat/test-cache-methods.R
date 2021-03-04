# Fixtures

fixtures <- list()

fixtures$retrieve_lint <- function() {
  file_name <- "R/test.R"
  lines <- c("foobar1 = 1", "foobar2 = 2", "foobar3 = 3")
  lints <- list(
    Lint(file_name, 1, line = "foobar1 = 1"),
    Lint(file_name, 2, line = "foobar2 = 2"),
    Lint(file_name, 3, line = "foobar3 = 3")
  )
  list(
    filename = file_name,
    lines = lines,
    expr = list(content = paste(collapse = "\n", lines)),
    linters = list(assignment_linter = assignment_linter()),
    lints = lints
  )
}

# `retrieve_lint`

test_that("retrieve_lint returns NULL when no cache is in use", {
  test_data <- fixtures$retrieve_lint()

  cache_object <- Cache(
    cache = FALSE,
    filename = test_data[["filename"]],
    linters = test_data[["linters"]]
  )

  cache_lint(
    cache = cache_object,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lints = test_data[["lints"]]
  )

  cached_lints <- retrieve_lint(
    cache = cache_object,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lines = test_data[["lines"]]
  )

  expect_null(cached_lints)
})

test_that("retrieve_lint returns the same lints if nothing has changed", {
  test_data <- fixtures$retrieve_lint()

  e1 <- new.env(parent = emptyenv())

  cache_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lints = test_data[["lints"]]
  )

  t1 <- retrieve_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lines = test_data[["lines"]]
  )

  expect_equal(t1, test_data[["lints"]])
})

test_that(
  p("retrieve_lint returns the same lints with fixed line numbers if lines",
    " added above"), {
  test_data <- fixtures$retrieve_lint()

  e1 <- new.env(parent = emptyenv())

  lines1 <- test_data[["lines"]]
  lines2 <- c("", lines1)
  lints <- test_data[["lints"]]

  cache_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lints = lints
  )

  t1 <- retrieve_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lines = lines2
  )

  expect_equal(t1[[1]]$line_number, lints[[1]]$line_number + 1)
  expect_equal(t1[[2]]$line_number, lints[[2]]$line_number + 1)
  expect_equal(t1[[3]]$line_number, lints[[3]]$line_number + 1)
})

test_that("retrieve_lint returns the same lints with lines added below", {
  test_data <- fixtures$retrieve_lint()

  e1 <- new.env(parent = emptyenv())

  lines1 <- test_data[["lines"]]
  lines2 <- c(lines1, "")

  cache_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lints = test_data[["lints"]]
  )

  t1 <- retrieve_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lines = lines2
  )

  expect_equal(t1, test_data[["lints"]])
})

test_that(
  p("retrieve_lint returns the same lints with fixed line numbers if lines",
    " added between"), {
  test_data <- fixtures$retrieve_lint()

  e1 <- new.env(parent = emptyenv())

  lines1 <- test_data[["lines"]]
  lines2 <- c(lines1[1], "", lines1[2:3], "")

  lints1 <- test_data[["lints"]]

  cache_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lints = lints1
  )

  t1 <- retrieve_lint(
    cache = e1,
    expr = test_data[["expr"]],
    linter = test_data[["linters"]],
    lines = lines2
  )

  expect_equal(t1[[1]]$line_number, lints1[[1]]$line_number)
  expect_equal(t1[[2]]$line_number, lints1[[2]]$line_number + 1)
  expect_equal(t1[[3]]$line_number, lints1[[3]]$line_number + 1)
})

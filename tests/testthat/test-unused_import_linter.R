test_that("test unused_import_linter", {
  expect_lint(
    "library(dplyr)\ntibble(a = 1)",
    NULL,
    unused_import_linter()
  )

  expect_lint(
    "library(tidyverse)\ntibble(a = 1)",
    NULL,
    unused_import_linter()
  )

  expect_lint(
    "library(dplyr)\n1 + 1",
    "package dplyr is never used",
    unused_import_linter()
  )
})

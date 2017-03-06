context("trailing_whitespace_linter")
test_that("returns the correct linting", {
  expect_lint("blah",
    NULL,
    trailing_whitespace_linter())

  expect_lint("blah <- 1  ",
    c(message = rex("Trailing whitespace is superfluous."),
      column_number = 10),
    trailing_whitespace_linter())

  expect_lint("blah <- 1  \n'hi'",
    rex("Trailing whitespace is superfluous."),
    trailing_whitespace_linter())

  expect_lint("blah <- 1\n'hi'\na <- 2  ",
    list(
      c(
        message = rex("Trailing whitespace is superfluous."),
        line_number = 3
      )
    ),
    trailing_whitespace_linter())
})

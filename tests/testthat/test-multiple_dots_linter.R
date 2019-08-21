context("multiple_dot_args_linter")

test_that("returns the correct linting", {
  msg <- "functions should not forward ... multiple times."

  expect_lint("function(...) x", NULL, multiple_dot_args_linter)
  expect_lint("function(...) list(...)", NULL, multiple_dot_args_linter)
  expect_lint("function(...) plot(...)", NULL, multiple_dot_args_linter)

  # ignore list(...)
  dots_list <- "function(...){plot(...); x <- list(...)}"
  expect_lint(dots_list, NULL, multiple_dot_args_linter)

  # ignore repeated calls to identical functions
  dots_repeated <- "function(x, ...){plot(...); plot(x, ...)}"
  expect_lint(dots_repeated, NULL, multiple_dot_args_linter)

  # ignore function definitions
  dots_formal <- "function(...){plot(...); f <- function(...){}}"
  expect_lint(dots_formal, NULL, multiple_dot_args_linter)

  # dangerous
  dots_double <- "function(...){plot(...); x <- lm(...)}"
  expect_lint(dots_double,
              list(list(message = msg), list(message = msg)),
              multiple_dot_args_linter)
})

context("commas_before_linter")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, commas_before_linter)

  expect_lint("fun(1, 1)", NULL, commas_before_linter)

  expect_lint("fun(1,\n  1)", NULL, commas_before_linter)

  expect_lint("fun(1,\n1)", NULL, commas_before_linter)

  expect_lint("fun(1\n,\n1)", NULL, commas_before_linter)

  expect_lint("fun(1\n  ,\n1)", NULL, commas_before_linter)

  expect_lint("fun(1 ,1)",
    list(
      rex("Commas should never have a space before.")
      ),
    commas_before_linter)

  expect_lint("\"fun(1 ,1)\"",
    NULL,
    commas_before_linter)

  expect_lint("a[1, , 2]",
    NULL,
    commas_before_linter)

  expect_lint("a[, 2]",
              NULL,
              commas_before_linter)
  
  expect_lint("a[ , 2]",
              list(
                rex("Commas should never have a space before.")
              ),
              commas_before_linter)
  
  expect_lint("a[1, , 2, , 3]",
    NULL,
    commas_before_linter)
})

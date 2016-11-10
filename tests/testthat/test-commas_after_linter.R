context("commas_after_linter")
test_that("returns the correct linting", {

  expect_lint("blah", NULL, commas_after_linter)

  expect_lint("fun(1, 1)", NULL, commas_after_linter)

  expect_lint("fun(1,\n  1)", NULL, commas_after_linter)

  expect_lint("fun(1,\n1)", NULL, commas_after_linter)

  expect_lint("fun(1\n,\n1)", NULL, commas_after_linter)

  expect_lint("fun(1\n  ,\n1)", NULL, commas_after_linter)

  expect_lint("fun(1\n,1)",
    rex("Commas should always have a space after."),
    commas_after_linter)

  expect_lint("fun(1,1)",
    rex("Commas should always have a space after."),
    commas_after_linter)

  expect_lint("\nfun(1,1)",
    rex("Commas should always have a space after."),
    commas_after_linter)

  expect_lint("fun(1 ,1)",
    list(
      rex("Commas should always have a space after.")
      ),
    commas_after_linter)

  expect_lint("\"fun(1 ,1)\"",
    NULL,
    commas_after_linter)

  expect_lint("a[1, , 2]",
    NULL,
    commas_after_linter)

  expect_lint("a[1, , 2, , 3]",
    NULL,
    commas_after_linter)
})

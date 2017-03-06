context("indentation_linter")

test_that("no_tab_linter returns the correct linting", {

  expect_lint("blah", NULL, no_tab_linter)

  expect_lint("  blah", NULL, no_tab_linter)

  expect_lint("#\tblah", NULL, no_tab_linter)

  expect_lint("\tblah", rex("Use 2 spaces to indent."), no_tab_linter)

  expect_lint("\t\tblah", rex("Use 2 spaces to indent."), no_tab_linter)

})

test_that("indentation_linter works with tabs", {

  expect_lint("blah", NULL, indentation_linter(0L))

  expect_lint("\tblah", NULL, indentation_linter(0L))

  expect_lint("\t\tblah", NULL, indentation_linter(0L))

  expect_lint("  blah", rex("Use tabs to indent."), indentation_linter(0L))

  expect_lint("#\tblah", NULL, indentation_linter(0L))

})


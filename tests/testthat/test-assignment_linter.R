context("assignment_linter")
options(encoding = "UTF-8")
Sys.setenv(LANGUAGE = "en")  # make sure no translated messages are matched
test_that("returns the correct linting", {
  expect_lint("blah", NULL, assignment_linter)

  expect_lint("blah <- 1", NULL, assignment_linter)

  expect_lint("blah<-1", NULL, assignment_linter)

  expect_lint("fun(blah=1)", NULL, assignment_linter)

  expect_lint("blah=1",
    rex("Use <-, not =, for assignment."),
      assignment_linter)

    expect_lint("blah = 1",
      rex("Use <-, not =, for assignment."),
        assignment_linter)

  expect_lint("blah = fun(1)",
    rex("Use <-, not =, for assignment.")
    , assignment_linter)

  expect_lint("blah = fun(1) {",
    list(
      rex("Use <-, not =, for assignment."),
      c(type = "error", "unexpected")
      ),
      assignment_linter)

  expect_lint("fun((blah = fun(1)))",
    rex("Use <-, not =, for assignment."),
    assignment_linter)

})

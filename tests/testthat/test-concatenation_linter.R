context("concatenation_linter")

msg_e <- rex::escape(
  "Unneded concatenation without arguments. Replace the \"c\" call by NULL.")
msg_n <- rex::escape(
  "Unneded concatenation of NULL. Remove NULL.")
msg_s <- rex::escape(
  "Unneded concatenation of a single constant. Remove the \"c\" call.")
msg_c <- rex::escape(
  "Coercive concatenation of heterogeneous atomic constants. Replace the \"c\" call by list().")


test_that("returns the correct linting with default parameters", {
  linter <- concatenation_linter()

  expect_lint("c(x)", NULL, linter)
  expect_lint("c(1, 2)", NULL, linter)
  expect_lint("c(x, recursive=TRUE)", NULL, linter)
  expect_lint("lapply(1, c)", NULL, linter)

  expect_lint("c()", msg_e, linter)
  expect_lint("c(1)", msg_s, linter)
  expect_lint("c(1, NULL)", msg_n, linter)
  expect_lint("c(\"TRUE\", TRUE)", msg_c, linter)
  expect_lint("c(1, recursive=TRUE)", msg_s, linter)
})

test_that("returns the correct linting for unneeded concatenations", {
  linter <- concatenation_linter("unneeded")
  expect_lint("c(1, 2)", NULL, linter)
  expect_lint("c(a=1)", NULL, linter)
  expect_lint("c()", list(message=msg_e, line_number=1L, column_number=1L), linter)
  expect_lint("c(NULL)", list(message=msg_n, line_number=1L, column_number=1L), linter)
  expect_lint("c(1, NULL)", list(message=msg_n, line_number=1L, column_number=1L), linter)
  expect_lint("c(1)", list(message=msg_s, line_number=1L, column_number=1L), linter)
  expect_lint("c (\n'a' )", list(message=msg_s, line_number=1L, column_number=1L), linter)
  expect_lint("c(y, c('c('),\nc())",
              list(
                list(message=msg_s, line_number=1L, column_number=6L),
                list(message=msg_e, line_number=2L, column_number=1L)
              ),
              linter)
})

test_that("returns the correct linting for coercive concatenations", {
  linter <- concatenation_linter("coercive")
  expect_lint("c(1L, b=2L, list(3L))", NULL, linter) # parser reports them as NUM_CONST
  expect_lint("c(1.0, 1L, TRUE)", NULL, linter) # parser reports them as NUM_CONST
  expect_lint("c(\"1\", 1)", list(message=msg_c, line_number=1L, column_number=1L), linter)
  expect_lint("c(\"TRUE\", TRUE)", list(message=msg_c, line_number=1L, column_number=1L), linter)
})

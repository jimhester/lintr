context("nonlocal_variable_linter")

msg <- rex("Do not use non-local variables.")

global_1 <- "function(){
  # anonymous toplevel function
  CONSTANT_A
}
"

global_2 <- "function(){
  # toplevel function with right-assign
  CONSTANT_B
} -> some_func_right
"

global_3 <- "some_func <- function(x) {
  # regular toplevel function
  y <- x + log(a)
  return(y)
}
"

global_4 <- "function() {const <<- 123.456}"

global_5 <- "l <- lapply(c(intKeys, strKeys), function(k) {valTransform(dictRefExistent(d, k))})"

nonlocal_1 <- "some_func <- function() {
  inner_func <- function(x) {
    dummy <- \"test\"
    if (runif(1) > 0.5) {
      y <- x + 1L
    } else {
      y <- a + b - 1L
    }; function(){
      dummy
    }
    y
  }
  inner_func(1.23)
}"

nonlocal_2 <- "function() {
  count <- 0
  lapply(
    1:3,
    function(x) {count <<- x + 1L; count}
  )
  count
}
"

test_that("returns the correct linting", {
  linter <- nonlocal_variable_linter

  expect_lint("", NULL, linter)

  expect_lint("function(a) {a*2L}", NULL, linter)

  # properly handle @ attributes
  expect_lint("function(u, v) {mget(u, v@.xData)}", NULL, linter)

  # codetools cannot distinguish a non-local access from a local assignment to the same name
  expect_lint("function() {a <- a+1}", NULL, linter)

  expect_lint(
    global_1,
    list(message = msg, type = "warning", linter = "nonlocal_variable_linter",
        line_number = 3L, column_number = 3L),
    linter
  )

  expect_lint(
    global_2,
    list(message = msg, line_number = 3L, column_number = 3L),
    linter
  )

  expect_lint(
    global_3,
    list(message = msg, line_number = 3L, column_number = 16L),
    linter
  )

  expect_lint(
    global_4,
    list(message = msg, line_number = 1L, column_number = 13L),
    linter
  )

  expect_lint(
    global_5,
    list(message = msg, line_number = 1L, column_number = 76L),
    linter
  )

  expect_lint(
    nonlocal_1,
    list(
      list(message = msg, line_number = 7L, column_number = 12L),
      list(message = msg, line_number = 7L, column_number = 16L),
      list(message = msg, line_number = 9L, column_number = 7L)
    ),
    linter
  )

  expect_lint(
    nonlocal_2,
    list(message = msg, line_number = 5L, column_number = 18L),
    linter
  )
})

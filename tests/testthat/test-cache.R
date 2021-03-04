# Helper functions

fhash <- function(filename) {
  digest::digest(filename, algo = "sha1")
}

# Tests

# `clear_cache`

test_that("clear_cache deletes the file if a file is given", {
  mockery::stub(clear_cache, "read_settings", function(...) invisible(...))
  mockery::stub(clear_cache, "unlink", function(...) list(...))

  e1 <- new.env(parent = emptyenv())
  d1 <- tempfile(pattern = "lintr_cache_")
  f1 <- "R/test.R"
  save_cache(cache = e1, file = f1, path = d1)

  want <- list(file.path(d1, fhash("R/test.R")), recursive = TRUE)
  expect_equal(clear_cache(f1, d1), want)
  expect_equal(clear_cache(file = f1, path = d1), want)
})

test_that("clear_cache deletes the directory if no file is given", {
  mockery::stub(clear_cache, "read_settings", function(...) invisible(...))
  mockery::stub(clear_cache, "unlink", function(...) list(...))

  expect_equal(clear_cache(file = NULL, path = "."), list(".", recursive = TRUE))
})

# `load_cache`

test_that("load_cache loads the saved file in a new empty environment", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    e1 <- new.env(parent = emptyenv()),
    assign("x", "foobar", envir = e1),
    d1 <- tempfile(pattern = "lintr_cache_"),
    f1 <- "R/test.R",
    save_cache(cache = e1, file = f1, path = d1),
    e2 <- load_cache_from_file(file = f1, path = d1),

    expect_equal(ls(e2), "x"),
    expect_equal(e2[["x"]], "foobar")
  )
})

test_that("load_cache returns an empty environment if no cache file exists", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    e1 <- new.env(parent = emptyenv()),
    d1 <- tempfile(pattern = "lintr_cache_"),
    f1 <- "R/test.R",
    f2 <- "test.R",

    save_cache(cache = e1, file = f1, path = d1),
    e2 <- load_cache_from_file(file = f2, path = d1),

    expect_equal(ls(e2), character(0))
  )
})

# `save_cache`

test_that("save_cache creates a directory if needed", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    e1 <- new.env(parent = emptyenv()),
    d1 <- tempfile(pattern = "lintr_cache_"),
    f1 <- "R/test.R",

    expect_false(file.exists(d1)),
    expect_false(file.exists(file.path(d1, fhash(f1)))),

    save_cache(cache = e1, file = f1, path = d1),

    expect_true(file.exists(d1)),
    expect_true(file.info(d1)$isdir),
    expect_true(file.exists(file.path(d1, fhash(f1))))
  )
})

test_that("save_cache uses unambiguous cache file names", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    e1 <- new.env(parent = emptyenv()),
    d1 <- tempfile(pattern = "lintr_cache_"),
    f1 <- "R/test.R",
    f2 <- "test.R",

    expect_false(file.exists(file.path(d1, fhash(f1)))),
    expect_false(file.exists(file.path(d1, fhash(f2)))),

    save_cache(cache = e1, file = f1, path = d1),
    save_cache(cache = e1, file = f2, path = d1),

    expect_true(fhash(f1) != fhash(f2)),
    expect_true(file.exists(file.path(d1, fhash(f1)))),
    expect_true(file.exists(file.path(d1, fhash(f2))))
  )
})

test_that("save_cache saves all non-hidden objects from the environment", {
  with_mock(
    `lintr::read_settings` = function(...) invisible(...),

    e1 <- new.env(parent = emptyenv()),
    e1$t1 <- 1,
    e1$t2 <- 2,

    d1 <- tempfile(pattern = "lintr_cache_"),
    f1 <- "R/test.R",

    save_cache(cache = e1, file = f1, path = d1),

    e2 <- new.env(parent = emptyenv()),
    load(file = file.path(d1, fhash(f1)), envir = e2),

    expect_equal(e1, e2)
  )
})

# `cache_file`

test_that("cache_file generates the same cache with different lints", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("foobar", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list())
  cache_file(e1, f1, list(), list(1))

  expect_equal(length(ls(e1)), 1)
})

test_that("cache_file generates different caches for different linters", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("foobar", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list())
  cache_file(e1, f1, list(1), list())

  expect_equal(length(ls(e1)), 2)
})

# `retrieve_file`

test_that("retrieve_file returns NULL if there is no cached result", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("foobar", f1)
  on.exit(unlink(f1))

  expect_equal(retrieve_file(e1, f1, list()), NULL)
})

test_that("retrieve_file returns the cached result if found", {
  e1 <- new.env(parent = emptyenv())

  f1 <- tempfile()
  writeLines("foobar", f1)
  on.exit(unlink(f1))

  cache_file(e1, f1, list(), list("foobar"))
  expect_equal(retrieve_file(e1, f1, list()), list("foobar"))
})

# `cache_lint`

test_that("cache_lint generates the same cache with different lints", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test")
  cache_lint(e1, t1, list(), list())
  cache_lint(e1, t1, list(), list(1))

  expect_equal(length(ls(e1)), 1)
})

test_that("cache_lint generates different caches for different linters", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "test")

  cache_lint(e1, t1, list(), list())
  cache_lint(e1, t1, list(1), list())

  expect_equal(length(ls(e1)), 2)
})

# `has_lint`

test_that("has_lint returns FALSE if there is no cached result", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "foobar")
  expect_false(has_lint(e1, t1, list()))
})

test_that("has_lint returns TRUE if there is a cached result", {
  e1 <- new.env(parent = emptyenv())

  t1 <- list(content = "foobar")
  cache_lint(e1, t1, list(), list())
  expect_true(has_lint(e1, t1, list()))
})

test_that("has_lint distinguishes global expressions from line expression with same content", {
  e1 <- new.env(parent = emptyenv())

  same_content <- "foobar"

  line_expr   <- list(content = same_content, parsed_content = data.frame())
  cache_lint(e1, line_expr, list(), list())

  global_expr <- list(content = same_content, file_lines = character())
  expect_false(has_lint(e1, global_expr, list()))
})

# `find_new_line`

test_that("find_new_line returns the same if the line is the same", {
  t1 <- c("foobar1",
          "foobar2",
          "foobar3")
  expect_equal(find_new_line(1, "foobar1", t1), 1)

  expect_equal(find_new_line(2, "foobar2", t1), 2)

  expect_equal(find_new_line(3, "foobar3", t1), 3)
})

test_that("find_new_line returns the correct line if it is before the current line", {
  t1 <- c("foobar1",
          "foobar2",
          "foobar3")
  expect_equal(find_new_line(1, "foobar1", t1), 1)

  expect_equal(find_new_line(2, "foobar1", t1), 1)

  expect_equal(find_new_line(3, "foobar1", t1), 1)
})

test_that("find_new_line returns the correct line if it is after the current line", {
  t1 <- c("foobar1",
          "foobar2",
          "foobar3")
  expect_equal(find_new_line(1, "foobar3", t1), 3)

  expect_equal(find_new_line(2, "foobar3", t1), 3)

  expect_equal(find_new_line(3, "foobar3", t1), 3)
})

#

test_that("lint with cache uses the provided relative cache directory", {
  path <- "./my_cache_dir"
  expect_false(dir.exists(path))
  linter <- assignment_linter()

  # create the cache
  expect_lint("a <- 1", NULL, linter, cache = path)
  expect_true(dir.exists(path))
  expect_length(list.files(file.path(path)), 1)

  # read the cache
  expect_lint("a <- 1", NULL, linter, cache = path)
  expect_true(dir.exists(path))

  unlink(path, recursive = TRUE)
})

test_that("it works outside of a package", {
  linter <- assignment_linter()

  with_mock(
    `lintr::find_package` = function(...) NULL,
    `lintr::pkg_name` = function(...) NULL,

    path <- tempfile(pattern = "my_cache_dir_"),
    expect_false(dir.exists(path)),
    expect_lint("a <- 1", NULL, linter, cache = path),
    expect_true(dir.exists(path)),
    expect_length(list.files(path), 1),
    expect_lint("a <- 1", NULL, linter, cache = path),
    expect_true(dir.exists(path))
  )
})

test_that("cache = TRUE workflow works", {
  # Need a test structure with a safe to load .lintr
  pkg <- "dummy_packages/package"
  files <- normalizePath(list.files(pkg, recursive = TRUE, full.names = TRUE))

  # Manually clear cache (that function is exported)
  for (f in files) {
    clear_cache(file = f)
  }
  l1 <- lint_package(pkg, cache = TRUE)
  l2 <- lint_package(pkg, cache = TRUE)
  expect_identical(l1, l2)
})

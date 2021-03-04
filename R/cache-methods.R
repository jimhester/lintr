# cache-wide stuff

#' @include cache.R

save_cache <- function(cache, ...) {
  UseMethod("save_cache", cache)
}

save_cache.environment <- function(cache, file, path = NULL) {
  if (is.null(path)) {
    # Only retrieve settings if `path` isn't specified.
    # Otherwise, other settings may inadvertently be loaded, such as exclusions.
    read_settings(file)
    path <- settings$cache_directory
  }

  if (!file.exists(path)) {
    dir.create(path, recursive = TRUE)
  }

  save(
    file = get_cache_file_path(file, path),
    envir = cache,
    list = ls(envir = cache)
  )
}

save_cache.Cache <- function(cache) {
  if (isTRUE(cache$should_cache)) {
    save_cache(cache$lint_cache, cache$filename, cache$cache_path)
  }
}

load_cache <- function(cache, ...) {
  UseMethod("load_cache", cache)
}

load_cache.Cache <- function(cache, ...) {
  load_cache_from_file(cache$filename, cache$cache_path)
}

# file-specific stuff

retrieve_file <- function(cache, ...) {
  UseMethod("retrieve_file", cache)
}

retrieve_file.environment <- function(cache, filename, linters) {
  mget(
    envir = cache,
    x = digest_content(linters, filename),
    mode = "list",
    inherits = FALSE,
    ifnotfound = list(NULL)
  )[[1L]]
}

retrieve_file.Cache <- function(cache) {
  retrieve_file(cache$lint_cache, cache$filename, cache$linters)
}

cache_file <- function(cache, ...) {
  UseMethod("cache_file", cache)
}

cache_file.environment <- function(cache, filename, linters, lints) {
  assign(
    envir = cache,
    x = digest_content(linters, filename),
    value = lints,
    inherits = FALSE
  )
}

cache_file.Cache <- function(cache, lints) {
  if (isTRUE(cache$should_cache)) {
    cache_file(cache$lint_cache, cache$filename, cache$linters, lints)
  }
}

# lint-specific stuff

retrieve_lint <- function(cache, ...) {
  UseMethod("retrieve_lint", cache)
}

retrieve_lint.environment <- function(cache, expr, linter, lines) {
  lints <- get(
    envir = cache,
    x = digest_content(linter, expr),
    mode = "list",
    inherits = FALSE
  )
  lints[] <- lapply(lints, function(lint) {
    lint$line_number <- find_new_line(
      lint$line_number, unname(lint$line), lines
    )
    lint
  })
  cache_lint(cache, expr, linter, lints)
  lints
}

retrieve_lint.Cache <- function(cache, expr, linter, lines) {
  if (!"lint_cache" %in% names(cache)) {
    return(NULL)
  }
  retrieve_lint(
    cache$lint_cache, expr, linter, lines
  )
}

cache_lint <- function(cache, ...) {
  UseMethod("cache_lint", cache)
}

cache_lint.environment <- function(cache, expr, linter, lints) {
  assign(
    envir = cache,
    x = digest_content(linter, expr),
    value = lints,
    inherits = FALSE)
}

cache_lint.Cache <- function(cache, expr, linter, lints) {
  if (isTRUE(cache$should_cache)) {
    cache_lint(cache$lint_cache, expr, linter, lints)
  }
}

has_lint <- function(cache, ...) {
  UseMethod("has_lint", cache)
}

has_lint.environment <- function(cache, expr, linter) {
  exists(
    envir = cache,
    x = digest_content(linter, expr),
    mode = "list",
    inherits = FALSE
  )
}

has_lint.Cache <- function(cache, expr, linter) {
  isTRUE(cache$should_cache) && has_lint(cache$lint_cache, expr, linter)
}

# helper functions

load_cache_from_file <- function(file, path = NULL) {
  if (is.null(path)) {
    # Only retrieve settings if `path` isn't specified.
    # Otherwise, other settings may inadvertently be loaded, such as exclusions.
    read_settings(file)
    path <- settings$cache_directory
  }

  env <- new.env(parent = emptyenv())

  file <- get_cache_file_path(file, path)
  if (file.exists(file)) {
    load(file = file, envir = env)
  } # else nothing to do for source file that has no cache

  env
}

digest_content <- function(linters, obj) {
  content <- if (is.list(obj)) {
    # assume an expression (global expression if obj$parsed_content is lacking)
    list(linters, obj$content, is.null(obj$parsed_content))
  } else {
    # assume a filename
    list(linters, readLines(obj))
  }
  digest::digest(content, algo = "sha1")
}

find_new_line <- function(line_number, line, lines) {

  if (lines[line_number] %==% line) {
    return(line_number)
  }

  width <- 1L

  while (width <= length(lines)) {
    low <- line_number - width
    if (low > 0L) {
      if (lines[low] %==% line) {
        return(low)
      }
    }
    high <- line_number + width
    if (high <= length(lines)) {
      if (lines[high] %==% line) {
        return(high)
      }
    }
    width <- width + 1L
  }
  NA
}

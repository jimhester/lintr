# nolint start: object_name_linter.
Cache <- function(cache = FALSE, filename, linters) {
  # nolint end
  cache_path <- define_cache_path(cache)
  instance <- list(
    cache_path = cache_path,
    filename = filename,
    linters = linters
  )
  class(instance) <- "Cache"

  if (length(cache_path)) {
    instance$lint_cache <- load_cache(instance)
    instance$cached_lints <- retrieve_file(instance)
    instance$should_cache <- TRUE
  } else {
    instance$should_cache <- FALSE
  }
  instance
}

define_cache_path <- function(cache) {
  if (isTRUE(cache)) {
    settings$cache_directory
  } else if (is.character(cache)) {
    cache
  } else {
    character(0)
  }
}

#' Clear the lintr cache
#'
#' @param file filename whose cache to clear.  If you pass \code{NULL}, it will
#' delete all of the caches.
#' @param path directory to store caches.  Reads option 'lintr.cache_directory'
#' as the default.
#' @return 0 for success, 1 for failure, invisibly.
#' @export
clear_cache <- function(file = NULL, path = NULL) {
  if (is.null(path)) {
    # Only retrieve settings if `path` isn't specified.
    # Otherwise, other settings may inadvertently be loaded, such as exclusions.
    read_settings(file)
    path <- settings$cache_directory
  }

  if (!is.null(file)) {
    path <- get_cache_file_path(file, path)
  }

  unlink(path, recursive = TRUE)
}

get_cache_file_path <- function(file, path) {
  # this assumes that a normalized absolute file path was given
  file.path(path, digest::digest(file, algo = "sha1"))
}

#' Lintr
#'
#' Checks adherence to a given style, syntax errors and possible semantic
#' issues.  Supports on the fly checking of R code edited with Emacs, Vim and
#' Sublime Text.
#' @name lintr
#' @seealso \code{\link{lint}}, \code{\link{lint_package}}, \code{\link{lint_dir}}, \code{\link{linters}}
#' @importFrom stats na.omit
#' @importFrom utils capture.output getParseData relist
NULL

#' Lint a file
#'
#' Apply one or more linters to a file and return the lints found.
#'
#' @name lint_file
#'
#' @param filename either the filename for a file to lint, or a character
#' string of inline R code for linting. The latter [inline data] applies
#' whenever \code{filename} has a newline character (\\n).
#' @param linters a named list of linter functions to apply see
#' \code{\link{linters}} for a full list of default and available linters.
#' @param cache given a logical, toggle caching of lint results. If passed a
#' character string, store the cache in this directory.
#' @param ... additional arguments passed to \code{\link{exclude}}.
#' @param parse_settings whether to try and parse the settings.
#' @param text Optional argument for supplying a string or lines directly,
#'   e.g. if the file is already in memory or linting is being done ad hoc.
#'
#' @return A list of lint objects.
#'
#' @examples
#' \dontrun{
#'   lint("some/file-name.R") # linting a file
#'   lint("a = 123\n")        # linting inline-code
#'   lint(text = "a = 123")   # linting inline-code
#' }
#'
#' @export
lint <- function(filename, linters = NULL, cache = FALSE, ..., parse_settings = TRUE, text = NULL) {

  if (is.null(text)) {
    inline_data <- rex::re_matches(filename, rex::rex(newline))
    if (inline_data) {
      text <- gsub("\n$", "", filename)
      filename <- NULL
    }
  } else {
    inline_data <- TRUE
    if (length(text) > 1) {
      text <- paste(text, sep = "", collapse = "\n")
    }
  }

  no_filename <- missing(filename) || is.null(filename)

  if (inline_data && no_filename) {
    filename <- tempfile()
    con <- file(filename, open = "w", encoding = settings$encoding)
    on.exit(unlink(filename), add = TRUE)
    writeLines(text = text, con = con, sep = "\n")
    close(con)
  }

  lines <- if (is.null(text)) {
    read_lines(filename)
  } else {
    strsplit(text, "\n", fixed = TRUE)[[1]]
  }

  filename <- normalizePath(filename, mustWork = !inline_data)  # to ensure a unique file in cache
  source_expressions <- get_source_expressions(filename, lines)

  if (isTRUE(parse_settings)) {
    read_settings(filename)
    on.exit(clear_settings, add = TRUE)
  }

  linters <- define_linters(linters)
  linters <- Map(validate_linter_object, linters, names(linters))

  lints <- list()
  itr <- 0

  cache_path <- if (isTRUE(cache)) {
    settings$cache_directory
  } else if (is.character(cache)) {
    cache
  } else {
    character(0)
  }

  if (length(cache_path)) {
    lint_cache <- load_cache(filename, cache_path)
    lint_obj <- if (is.null(text)) filename else list(content = get_content(lines), TRUE)
    lints <- retrieve_file(lint_cache, lint_obj, linters)
    if (!is.null(lints)) {
      return(exclude(lints, lines = lines, ...))
    }
    cache <- TRUE
  } else {
    cache <- FALSE
  }

  for (expr in source_expressions$expressions) {
    for (linter in names(linters)) {
      expr_lints <- NULL
      if (isTRUE(cache) && has_lint(lint_cache, expr, linter)) {
        # retrieve_lint() might return NULL if missing line number is encountered.
        # It could be caused by nolint comments.
        expr_lints <- retrieve_lint(lint_cache, expr, linter, source_expressions$lines)
      }

      if (is.null(expr_lints)) {
        expr_lints <- flatten_lints(linters[[linter]](expr))

        for (i in seq_along(expr_lints)) {
          expr_lints[[i]]$linter <- linter
        }

        if (isTRUE(cache)) {
          cache_lint(lint_cache, expr, linter, expr_lints)
        }
      }
      lints[[itr <- itr + 1L]] <- expr_lints
    }
  }

  if (inherits(source_expressions$error, "lint")) {
    lints[[itr <- itr + 1L]] <- source_expressions$error

    if (isTRUE(cache)) {
      cache_lint(lint_cache, list(filename = filename, content = ""), "error", source_expressions$error)
    }
  }

  lints <- structure(reorder_lints(flatten_lints(lints)), class = "lints")

  if (isTRUE(cache)) {
    cache_file(lint_cache, filename, linters, lints)
    save_cache(lint_cache, filename, cache_path)
  }

  res <- exclude(lints, lines = lines, ...)

  # simplify filename if inline
  if (no_filename) {
    for (i in seq_along(res)) {
      res[[i]][["filename"]] <- "<text>"
    }
  }
  res
}

#' Lint a directory
#'
#' Apply one or more linters to all of the R files in a directory
#' @param path the path to the base directory, by default,
#' it will be searched in the parent directories of the current directory.
#' @param relative_path if \code{TRUE}, file paths are printed using their path
#' relative to the base directory.  If \code{FALSE}, use the full
#' absolute path.
#' @param ... additional arguments passed to \code{\link{lint}}, e.g.
#' \code{cache} or \code{linters}.
#' @param exclusions exclusions for \code{\link{exclude}}, relative to the
#' package path.
#' @param pattern pattern for files, by default it will take files with any of
#' the extensions .R, .Rmd, .Rnw, .Rhtml, .Rrst, .Rtex, .Rtxt allowing for
#' lowercase r (.r, ...)
#' @inherit lint_file return
#' @inheritParams lint_file
#' @examples
#' \dontrun{
#'   lint_dir()
#'   lint_dir(
#'     linters = list(semicolon_terminator_linter())
#'     cache = TRUE,
#'     exclusions = list("inst/doc/creating_linters.R" = 1, "inst/example/bad.R", "renv")
#'   )
#' }
#' @export
lint_dir <- function(path = ".", relative_path = TRUE, ..., exclusions = list("renv", "packrat"),
                     pattern = rex::rex(
                       ".", one_of("Rr"),
                       or("", "html", "md", "nw", "rst", "tex", "txt"),
                       end
                     ),
                     parse_settings = TRUE) {

  if (isTRUE(parse_settings)) {
    read_settings(path)
    on.exit(clear_settings, add = TRUE)

    exclusions <- c(exclusions, settings$exclusions)
  }

  exclusions <- normalize_exclusions(
    exclusions,
    root = path,
    pattern = pattern
  )

  # normalizePath ensures names(exclusions) and files have the same names for the same files.
  # Otherwise on windows, files might incorrectly not be excluded in to_exclude
  files <- normalizePath(dir(
    path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE
  ))

  # Remove fully ignored files to avoid reading & parsing
  to_exclude <- vapply(
    seq_len(length(files)),
    function(i) {
      file <- files[i]
      file %in% names(exclusions) && is_excluded_file(exclusions[[file]])
    },
    logical(1)
  )
  files <- files[!to_exclude]

  lints <- flatten_lints(lapply(
    files,
    function(file) {
      if (interactive()) {
        message(".", appendLF = FALSE) # nocov
      }
      lint(file, ..., parse_settings = FALSE, exclusions = exclusions)
    }
  ))

  if (interactive()) {
    message() # nocov. for a newline
  }

  lints <- reorder_lints(lints)

  if (relative_path == TRUE) {
    path <- normalizePath(path, mustWork = FALSE)
    lints[] <- lapply(
      lints,
      function(x) {
        x$filename <- re_substitutes(x$filename, rex(path, one_of("/", "\\")), "")
        x
      }
    )
    attr(lints, "path") <- path
  }

  class(lints) <- "lints"

  lints
}


#' Lint a package
#'
#' Apply one or more linters to all of the R files in a package.
#' @param path the path to the base directory of the package, if \code{NULL},
#' it will be searched in the parent directories of the current directory.
#' @inherit lint_file return
#' @inheritParams lint_dir
#' @examples
#' \dontrun{
#'   lint_package()
#'
#'   lint_package(
#'     linters = with_defaults(semicolon_linter = semicolon_terminator_linter())
#'     cache = TRUE,
#'     exclusions = list("inst/doc/creating_linters.R" = 1, "inst/example/bad.R")
#'   )
#' }
#' @export
lint_package <- function(path = ".", relative_path = TRUE, ...,
                         exclusions = list("R/RcppExports.R"), parse_settings = TRUE) {
  pkg_path <- find_package(path)

  if (is.null(pkg_path)) {
    warning("Didn't find any R package searching upwards from '", path, "'.")
    return(NULL)
  }

  if (parse_settings) {
    read_settings(pkg_path)
    on.exit(clear_settings, add = TRUE)
  }

  exclusions <- normalize_exclusions(
    c(exclusions, settings$exclusions),
    root = pkg_path
  )

  lints <- lint_dir(file.path(pkg_path, c("R", "tests", "inst", "vignettes", "data-raw", "demo")),
                    relative_path = FALSE, exclusions = exclusions, parse_settings = FALSE, ...)

  if (isTRUE(relative_path)) {
    path <- normalizePath(pkg_path, mustWork = FALSE)
    lints[] <- lapply(
      lints,
      function(x) {
        x$filename <- re_substitutes(x$filename, rex(path, one_of("/", "\\")), "")
        x
      }
    )
    attr(lints, "path") <- path
  }

  lints
}

define_linters <- function(linters = NULL) {
  if (is.null(linters)) {
    linters <- settings$linters
    names(linters) <- auto_names(linters)
  } else if (inherits(linters, "linter")) {
    linters <- list(linters)
    names(linters) <- attr(linters[[1L]], "name", exact = TRUE)
  } else if (!is.list(linters)) {
    name <- deparse(substitute(linters))
    linters <- list(linters)
    names(linters) <- name
  } else {
    names(linters) <- auto_names(linters)
  }
  linters
}

validate_linter_object <- function(linter, name) {
  if (inherits(linter, "linter")) {
  } else if (is.function(linter)) {
    if (is.null(formals(linter))) {
      old <- "Passing linters as variables"
      new <- "a call to the linters (see ?linters)"
      lintr_deprecated(old = old, new = new, version = "3.0.0",
                       type = "")
      linter <- linter()
    } else {
      old <- "The use of linters of class 'function'"
      new <- "linters classed as 'linter' (see ?Linter)"
      lintr_deprecated(old = old, new = new, version = "3.0.0",
                       type = "")
      linter <- Linter(linter, name = name)
    }
  } else {
    stop(gettextf("Expected '%s' to be of class 'linter', not '%s'",
                  name, class(linter)[[1L]]))
  }
  linter
}

reorder_lints <- function(lints) {
  files <- vapply(lints, `[[`, character(1), "filename")
  lines <- vapply(lints, `[[`, integer(1), "line_number")
  columns <- vapply(lints, `[[`, integer(1), "column_number")
  lints[order(
    files,
    lines,
    columns
  )]
}

has_description <- function(path) {
  desc_info <- file.info(file.path(path, "DESCRIPTION"))
  !is.na(desc_info$size) && desc_info$size > 0 && !desc_info$isdir
}

find_package <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)

  while (!has_description(path)) {
    path <- dirname(path)
    if (is_root(path)) {
      return(NULL)
    }
  }

  path
}

find_rproj_at <- function(path) {
  head(list.files(path = path, pattern = "\\.Rproj$", full.names = TRUE), 1L)
}

find_rproj <- function(path) {
  path <- normalizePath(path, mustWork = FALSE)

  while (length(res <- find_rproj_at(path)) == 0L) {
    path <- dirname(path)
    if (is_root(path)) {
      return(NULL)
    }
  }

  res
}

is_root <- function(path) {
  identical(path, dirname(path))
}

has_config <- function(path, config) {
  file.exists(file.path(path, config))
}

find_config2 <- function(path) {
  config <- basename(getOption("lintr.linter_file"))
  path <- normalizePath(path, mustWork = FALSE)

  while (!has_config(path, config)) {
    path <- dirname(path)
    if (is_root(path)) {
      return(character())
    }
  }
  return(file.path(path, config))
}

pkg_name <- function(path = find_package()) {
  if (is.null(path)) {
    return(NULL)
  } else {
    read.dcf(file.path(path, "DESCRIPTION"), fields = "Package")[1]
  }
}

#' Create a \code{lint} object
#' @param filename path to the source file that was linted.
#' @param line_number line number where the lint occurred.
#' @param column_number column number where the lint occurred.
#' @param type type of lint.
#' @param message message used to describe the lint error
#' @param line code source where the lint occurred
#' @param ranges a list of ranges on the line that should be emphasized.
#' @param linter deprecated. No longer used.
#' @return an object of class 'lint'.
#' @export
Lint <- function(filename, line_number = 1L, column_number = 1L, # nolint: object_name_linter.
                 type = c("style", "warning", "error"),
                 message = "", line = "", ranges = NULL, linter = "") {
  if (!missing(linter)) {
    lintr_deprecated(
      old = "Using the `linter` argument of `Lint()`",
      version = "3.0.0",
      type = ""
    )
  }

  type <- match.arg(type)

  structure(
    list(
      filename = filename,
      line_number = as.integer(line_number),
      column_number = as.integer(column_number),
      type = type,
      message = message,
      line = line,
      ranges = ranges,
      linter = NA_character_
    ),
    class = "lint")
}

rstudio_source_markers <- function(lints) {

  # package path will be NULL unless it is a relative path
  package_path <- attr(lints, "path")

  # generate the markers
  markers <- lapply(lints, function(x) {
    filename <- if (!is.null(package_path)) {
      file.path(package_path, x$filename)
    } else {
      x$filename
    }

    marker <- list()
    marker$type <- x$type
    marker$file <- filename
    marker$line <- x$line_number
    marker$column <- x$column_number
    marker$message <- x$message
    marker
  })

  # request source markers
  out <- rstudioapi::callFun(
    "sourceMarkers",
    name = "lintr",
    markers = markers,
    basePath = package_path,
    autoSelect = "first"
  )

  # workaround to avoid focusing an empty Markers pane
  # when possible, better solution is to delete the "lintr" source marker list
  # https://github.com/rstudio/rstudioapi/issues/209
  if (length(lints) == 0) {
    Sys.sleep(0.1)
    rstudioapi::executeCommand("activateConsole")
  }

  out
}

#' Checkstyle Report for lint results
#'
#' Generate a report of the linting results using the
#' \href{http://checkstyle.sourceforge.net/}{Checkstyle} XML format.
#' @param lints the linting results.
#' @param filename the name of the output report
#' @export
checkstyle_output <- function(lints, filename = "lintr_results.xml") {

  # package path will be NULL unless it is a relative path
  package_path <- attr(lints, "path")

  # setup file
  d <- xml2::xml_new_document()
  n <- xml2::xml_add_child(d, "checkstyle", version = paste0("lintr-", utils::packageVersion("lintr")))

  # output the style markers to the file
  lapply(split(lints, names(lints)), function(lints_per_file) {
    filename <- if (!is.null(package_path)) {
      file.path(package_path, lints_per_file[[1]]$filename)
    } else {
      lints_per_file[[1]]$filename
    }
    f <- xml2::xml_add_child(n, "file", name = filename)

    lapply(lints_per_file, function(x) {
      xml2::xml_add_child(
        f, "error",
        line = as.character(x$line_number),
        column = as.character(x$column_number),
        severity = switch(
          x$type,
          style = "info",
          x$type
        ),
        message = x$message)
    })
  })

  xml2::write_xml(d, filename)
}

highlight_string <- function(message, column_number = NULL, ranges = NULL) {

  maximum <- max(column_number, unlist(ranges))

  line <- fill_with(" ", maximum)

  lapply(ranges, function(range) {
    substr(line, range[1], range[2]) <<-
      fill_with("~", range[2] - range[1] + 1L)
  })

  substr(line, column_number, column_number + 1L) <- "^"

  line
}

fill_with <- function(character = " ", length = 1L) {
  paste0(collapse = "", rep.int(character, length))
}

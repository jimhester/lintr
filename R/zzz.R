#' Available linters
#'
#' @name linters
#' @title linters
#' @param source_file returned by \code{\link{get_source_expressions}}
#' @param length the length cutoff to use for the given linter.
NULL

named_list <- function(...) {
  nms <- re_substitutes(as.character(eval(substitute(alist(...)))),
    rex("(", anything), "")
  vals <- list(...)
  names(vals) <- nms
  vals[!vapply(vals, is.null, logical(1))]
}

#' Modify lintr defaults
#'
#' Make a new list based on \pkg{lintr}'s default linters, undesirable operators or functions.
#'
#' @param ... arguments of elements to change. If unnamed, the argument is named. If the named
#' argument already exists in "default", it is replaced by the new element. If it does not exist,
#' it is added. If the value is \code{NULL}, the element is removed.
#' @param default list of elements to modify.
#' @return A modified list of elements.
#' @examples
#' # the default linter list with a different line length cutoff
#' my_linters <- with_defaults(line_length_linter = line_length_linter(120))
#'
#' # omit the argument name if you are just using different arguments
#' my_linters <- with_defaults(default = my_linters,
#'                             object_name_linter("lowerCamelCase"))
#'
#' # remove assignment checks (with NULL), add absolute path checks
#' my_linters <- with_defaults(default = my_linters,
#'                             assignment_linter = NULL,
#'                             absolute_path_linter)
#'
#' # custom list of undesirable functions:
#' #    remove sapply (using NULL)
#' #    add cat (with a accompanying message),
#' #    add print (unnamed, i.e. with no accompanying message)
#' #    add return (as taken from all_undesirable_functions)
#' my_undesirable_functions <- with_defaults(default = default_undesirable_functions,
#'   sapply=NULL, "cat"="No cat allowed", "print", all_undesirable_functions[["return"]])
#' @export
with_defaults <- function(..., default = default_linters) {
  vals <- list(...)
  nms <- names2(vals)
  missing <- nms == ""
  if (any(missing)) {
    args <- as.character(eval(substitute(alist(...)[missing])))
    # foo_linter(x=1) => "foo"
    # var[["foo"]]    => "foo"
    nms[missing] <- re_substitutes(
      re_substitutes(
        re_substitutes(args, rex("(", anything), ""),
        rex(start, anything, "[\""),
        ""),
      rex("\"]", anything, end),
      "")
  }

  vals[nms == vals] <- NA
  default[nms] <- vals

  res <- default[!vapply(default, is.null, logical(1))]

  res[] <- lapply(res, function(x) {
    prev_class <- class(x)
    if (inherits(x, "function")) {
      class(x) <- c(prev_class, "lintr_function")
    }
    x
  })
}

# this is just to make the auto documentation cleaner
str.lintr_function <- function(x, ...) {
  cat("\n")
}

#' Default linters
#'
#' List of default linters for \code{\link{lint}}. Use \code{\link{with_defaults}} to customize it.
#' @export
default_linters <- with_defaults(default = list(),
  assignment_linter,
  closed_curly_linter(),
  commas_linter,
  commented_code_linter,
  equals_na_linter,
  function_left_parentheses_linter,
  infix_spaces_linter,
  line_length_linter(80),
  multiple_dot_args_linter,
  no_tab_linter,
  object_length_linter(),
  object_name_linter("snake_case"),
  object_usage_linter,
  open_curly_linter(),
  pipe_continuation_linter,
  single_quotes_linter,
  spaces_inside_linter,
  spaces_left_parentheses_linter,
  trailing_blank_lines_linter,
  trailing_whitespace_linter)

#' Default undesirable functions and operators
#'
#' Lists of function names and operators for \code{\link{undesirable_function_linter}} and
#' \code{\link{undesirable_operator_linter}}. There is a list for the default elements and another
#' that contains all available elements. Use \code{\link{with_defaults}} to produce a custom list.
#' @format A named list of character strings.
#' @rdname default_undesirable_functions
#' @export
all_undesirable_functions <- with_defaults(default = list(),
  "attach" = "use roxygen2's @importFrom statement in packages, or `::` in scripts",
  "detach" = "use roxygen2's @importFrom statement in packages, or `::` in scripts",
  "ifelse" = "use an if () {} else {} block",
  ".libPaths" = "use withr::with_libpaths()",
  "library" = "use roxygen2's @importFrom statement in packages, or `::` in scripts",
  "loadNamespace" = "use `::` or requireNamespace()",
  "mapply" = "use Map()",
  "options" = "use withr::with_options()",
  "par" = "use withr::with_par()",
  "require" = "use roxygen2's @importFrom statement in packages, or `::` in scripts",
  "return" = "let the last value of a function automatically be returned",
  "sapply" = "use vapply() or lapply()",
  "setwd" = "use withr::with_dir()",
  "sink" = "use withr::with_sink()",
  "source" = NA,
  "substring" = "use substr()",
  "Sys.setenv" = "use withr::with_envvar()",
  "Sys.setlocale" = "use withr::with_locale()"
)

#' @rdname default_undesirable_functions
#' @export
default_undesirable_functions <- do.call(with_defaults, c(list(default=list()),
  all_undesirable_functions[c(
    "attach",
    "detach",
    ".libPaths",
    "library",
    "mapply",
    "options",
    "par",
    "require",
    "sapply",
    "setwd",
    "sink",
    "source",
    "Sys.setenv",
    "Sys.setlocale"
  )]
))

#' @rdname default_undesirable_functions
#' @export
all_undesirable_operators <- with_defaults(default = list(),
  ":::" = NA,
  "<<-" = NA,
  "->>" = NA
)

#' @rdname default_undesirable_functions
#' @export
default_undesirable_operators <- do.call(with_defaults, c(list(default=list()),
  all_undesirable_operators[c(
    ":::",
    "<<-",
    "->>"
  )]
))


#' Default lintr settings
#' @seealso \code{\link{read_settings}}, \code{\link{default_linters}}
#' @export
default_settings <- NULL

settings <- NULL

.onLoad <- function(libname, pkgname) { # nolint
  op <- options()
  op.lintr <- list(
    lintr.linter_file = ".lintr"
  )
  toset <- !(names(op.lintr) %in% names(op))
  if (any(toset)) options(op.lintr[toset])

  default_settings <<- list(
    linters = default_linters,
    exclude = rex::rex("#", any_spaces, "nolint"),
    exclude_start = rex::rex("#", any_spaces, "nolint start"),
    exclude_end = rex::rex("#", any_spaces, "nolint end"),
    exclusions = list(),
    cache_directory = "~/.R/lintr_cache", # nolint
    comment_token = Sys.getenv("GITHUB_TOKEN", unset = NA) %||% rot(
      paste0(
        "0n12nn72507",
        "r6273qnnp34",
        "43qno7q42n1",
        "n71nn28")
      , 54 - 13),
    comment_bot = logical_env("LINTR_COMMENT_BOT") %||% TRUE,
    error_on_lint = logical_env("LINTR_ERROR_ON_LINT") %||% FALSE
  )

  settings <<- list2env(default_settings, parent = emptyenv())
  invisible()
}

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

#' Modify the list of default linters
#'
#' @param ... named arguments of linters to change.  If the named linter already
#' exists it is replaced by the new linter, if it does not exist it is added.
#' If the value is \code{NULL} the linter is removed.
#' @param default default linters to change
#' @export
#' @examples
#' # change the default line length cutoff
#' with_defaults(line_length_linter = line_length_linter(120))
#'
#' # you can also omit the argument name if you are just using different
#' #   arguments.
#' with_defaults(line_length_linter(120))
#'
#' # enforce camelCase rather than snake_case
#' with_defaults(camel_case_linter = NULL,
#'               snake_case_linter)
with_defaults <- function(..., default = default_linters) {
  vals <- list(...)
  nms <- names2(vals)
  missing <- nms == ""
  if (any(missing)) {
    nms[missing] <- re_substitutes(as.character(eval(substitute(alist(...)[missing]))),
      rex("(", anything), "")
  }
  default[nms] <- vals

  res <- default[!vapply(default, is.null, logical(1))]

  res[] <- lapply(res, function(x) {
    prev_class <- class(x)
    class(x) <- c(prev_class, "lintr_function")
    x
  })
}

# this is just to make the auto documentation cleaner
str.lintr_function <- function(x, ...) {
  cat("\n")
}

#' Default linters to use
#' @export
default_linters <- with_defaults(default = list(),

  assignment_linter,
  single_quotes_linter,
  no_tab_linter,
  line_length_linter(80),
  commas_after_linter,
  commas_before_linter,
  infix_spaces_linter,
  spaces_left_parentheses_linter,
  spaces_inside_linter,
  open_curly_linter(),
  closed_curly_linter(),
  camel_case_linter,
  multiple_dots_linter,
  object_length_linter(30),
  object_usage_linter,
  trailing_whitespace_linter,
  trailing_blank_lines_linter,
  commented_code_linter,

  NULL
)

#' Default lintr settings
#' @seealso \code{\link{read_settings}}, \code{\link{default_linters}}
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
    comment_token = rot(
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

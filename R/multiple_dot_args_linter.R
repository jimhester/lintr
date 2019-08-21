#' @describeIn linters Check that functions with \code{...}-arguments hand them
#'   over to sub-functions at most once.
#' @export
multiple_dot_args_linter <- function(source_file) {
  if (!is.null(source_file[["file_lines"]])) {
    # abort if source_file is entire file, not a top level expression.
    return(NULL)
  }
  parsed <- source_file$parsed_content
  has_dot_args <- any(parsed$text == "..." & parsed$token == "SYMBOL_FORMALS")
  if (!has_dot_args) {
    return(NULL)
  }
  dot_call_locs <- get_dot_call_locs(parsed)
  if (sum(!duplicated(parsed[dot_call_locs, "text"])) < 2) {
    return(NULL)
  }
  lapply(dot_call_locs, function(loc) {
    lint <- parsed[loc + which(parsed$text[-(1:loc)] == "...")[1],]
    Lint(
      filename = source_file[["filename"]],
      line_number = lint[["line1"]],
      column_number = lint[["col1"]],
      ranges = list(c(lint[["col1"]], lint[["col2"]])),
      type = "style",
      message = "functions should not forward ... multiple times.",
      line = source_file$lines[as.character(lint[["line1"]])],
      linter = "multiple_dots_linter"
    )
  })
}

get_dot_call_locs <- function(parsed) {
  # parsed[parsed$line1 %in% c(21, 22),]
  dot_locs <- which((parsed$text == "...") & (parsed$token == "SYMBOL"))
  # find the expr that contains the arguments of the function calls to
  # which the dots belong
  dot_parents <-
    subset(parsed, parent %in%
             subset(parsed, id %in% parsed[dot_locs, "parent"])$parent)$id
  # find lines of function calls for these argument list expressions
  # ignore list(...)
  with(parsed,
       which(parent %in% dot_parents &
               token == "SYMBOL_FUNCTION_CALL" &
               text != "list"))
}

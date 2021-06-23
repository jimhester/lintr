#' @describeIn linters  Check that \code{<-} is always used for assignment.
#' @export
assignment_linter <- function() {
  Linter(function(source_file) {
    lapply(
      ids_with_token(source_file, "EQ_ASSIGN"),
      function(id) {
        parsed <- with_id(source_file, id)
        Lint(
          filename = source_file$filename,
          line_number = parsed$line1,
          column_number = parsed$col1,
          type = "style",
          message = "Use <-, not =, for assignment.",
          line = source_file$lines[as.character(parsed$line1)],
          fixes = "<-"
        )
      })
  })
}

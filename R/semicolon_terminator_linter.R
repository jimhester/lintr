#' @describeIn linters  Check that no semicolons terminate statements.
#' @param semicolon A character vector defining which semicolons to report:\describe{
#'   \item{compound}{Semicolons that separate two statements on the same line.}
#'   \item{trailing}{Semicolons following the last statement on the line.}
#' }
#' @export
semicolon_terminator_linter <- function(semicolon = c("compound", "trailing")) {
  semicolon <- match.arg(semicolon, several.ok = TRUE)
  function(source_file) {
    tokens <- with_id(source_file, ids_with_token(source_file, "';'"))
    is_trailing <- is_trailing_sc(tokens, source_file)

    to_keep <- ( is_trailing & "trailing" %in% semicolon) |
               (!is_trailing & "compound" %in% semicolon)

    tokens <- tokens[to_keep, ]
    are_trailing <- is_trailing[to_keep]

    Map(
      function(token, is_trailing) {
        msg <- if (is_trailing) {
          "Trailing semicolons are not needed."
        } else  {
          "Compound semicolons are not needed. Replace them by a newline."
        }
        Lint(
          filename = source_file[["filename"]],
          line_number = token[["line1"]],
          column_number = token[["col1"]],
          type = "style",
          message = msg,
          line = source_file[["lines"]][[as.character(token[["line1"]])]],
          ranges = list(c(token[["col1"]], token[["col2"]])),
          linter = "semicolon_linter"
        )
      },
      split(tokens, seq_len(nrow(tokens))),
      are_trailing
    )
  }
}


is_trailing_sc <- function(sc_tokens, source_file) {
  line_str <- source_file[["lines"]][as.character(sc_tokens[["line1"]])]
  tail_str <- substr(line_str, sc_tokens[["col1"]] + 1L, nchar(line_str))
  grepl("^\\s*(#|}|\\z)", tail_str, perl = TRUE)
}


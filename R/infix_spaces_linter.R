# names: xml tags; values: getParseData tokens
# these can be used as unary operators; treat separately
unary_infix_tokens <- c(
  "OP-PLUS" = "'+'",               # +        : unary plus
  "OP-MINUS" = "'-'",              # -        : unary minus
  NULL
)
binary_infix_tokens <- c(

  "GT" = "GT",                     # >        : greater than
  "GE" = "GE",                     # <=       : greater than or equal to
  "LT" = "LT",                     # <        : less than
  "LE" = "LE",                     # <=       : less than or equal to
  "EQ" = "EQ",                     # ==       : vector equality
  "NE" = "NE",                     # !=       : not equal
  "AND" = "AND",                   # &        : vector boolean and
  "OR" = "OR",                     # |        : vector boolean or
  "AND2" = "AND2",                 # &&       : scalar boolean and
  "OR2" = "OR2",                   # ||       : scalar boolean or
  "LEFT_ASSIGN" = "LEFT_ASSIGN",   # <- or := : left assignment
  "RIGHT_ASSIGN" = "RIGHT_ASSIGN", # ->       : right assignment
  "EQ_ASSIGN" = "EQ_ASSIGN",       # =        : equal assignment
  "EQ_SUB" = "EQ_SUB",             # =        : keyword assignment
  "SPECIAL" = "SPECIAL",           # %[^%]*%  : infix operators
  "OP-SLASH" = "'/'",              # /        : unary division
  "OP-STAR" = "'*'",               # *        : unary multiplication

  NULL
)
infix_tokens <- c(unary_infix_tokens, binary_infix_tokens)

#' @describeIn linters  Check that infix operators are surrounded by spaces.
#' @export
infix_spaces_linter <- function() {
  Linter(function(source_file) {
    lapply(
      ids_with_token(source_file, infix_tokens, fun = `%in%`),
      function(id) {
        parsed <- with_id(source_file, id)

        line <- source_file$lines[as.character(parsed$line1)]

        around_operator <- substr(line, parsed$col1 - 1L, parsed$col2 + 1L)
        non_space_before <- re_matches(around_operator, rex(start, non_space))
        newline_after <- unname(nchar(line)) %==% parsed$col2
        non_space_after <- re_matches(around_operator, rex(non_space, end))

        if (non_space_before || (!newline_after && non_space_after)) {

          # we only should check spacing if the operator is infix,
          # which only happens if there is more than one sibling
          is_infix <- length(siblings(source_file$parsed_content, parsed$id, 1)) > 1L

          start <- end <- parsed$col1

          if (is_infix) {
            fix <- substr(line, parsed$col1, parsed$col2)
            if (non_space_before) {
              start <- parsed$col1 - 1L
              fix <- paste0(substr(line, start, start), " ", fix)
            }
            if (non_space_after) {
              end <- parsed$col2 + 1L
              fix <- paste0(fix, " ", substr(line, end, end))
            }

            Lint(
              filename = source_file$filename,
              line_number = parsed$line1,
              column_number = parsed$col1,
              type = "style",
              message = "Put spaces around all infix operators.",
              line = line,
              ranges = list(c(start, end)),
              fixes = fix
            )
          }
        }
      })
  })
}

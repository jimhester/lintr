#' @include aaa.R
#'
#' @describeIn linters  Avoid the symbols \code{T} and \code{F} (for \code{TRUE} and \code{FALSE}).
#' @export
T_and_F_symbol_linter <- function() { # nolint: object_name_linter.
  Linter(function(source_file) {
    if (is.null(source_file$xml_parsed_content)) return(list())

    xpath <- paste0(
      "//SYMBOL[",
      "(text() = 'T' or text() = 'F')", # T or F symbol
      " and not(preceding-sibling::OP-DOLLAR)", # not part of a $-subset expression
      " and not(parent::expr[",
      "  following-sibling::LEFT_ASSIGN", # not target of left assignment
      "  or preceding-sibling::RIGHT_ASSIGN", # not target of right assignment
      "  or following-sibling::EQ_ASSIGN", # not target of equals assignment
      "])",
      "]"
    )

    xpath_assignment <- paste0(
      "//SYMBOL[",
      "(text() = 'T' or text() = 'F')", # T or F symbol
      " and not(preceding-sibling::OP-DOLLAR)", # not part of a $-subset expression
      " and parent::expr[", #, but ...
      "  following-sibling::LEFT_ASSIGN", # target of left assignment
      "  or preceding-sibling::RIGHT_ASSIGN", # target of right assignment
      "  or following-sibling::EQ_ASSIGN", # target of equals assignment
      " ]",
      "]"
    )

    bad_exprs <- xml2::xml_find_all(source_file$xml_parsed_content, xpath)
    bad_assigns <- xml2::xml_find_all(source_file$xml_parsed_content, xpath_assignment)

    make_lint <- function(expr, fmt) {
      symbol <- xml2::xml_text(expr)
      replacement <- switch(symbol, "T" = "TRUE", "F" = "FALSE")
      message <- sprintf(fmt, replacement, symbol)
      xml_nodes_to_lint(
        xml = expr,
        source_file = source_file,
        message = message,
        type = "style",
        offset = 1L,
        fix = replacement
      )
    }

    c(
      lapply(
        bad_exprs, make_lint,
        fmt = "Use %s instead of the symbol %s."
      ),
      lapply(
        bad_assigns, make_lint,
        fmt = "Don't use %2$s as a variable name, as it can break code relying on %2$s being %1$s."
      )
    )
  })
}

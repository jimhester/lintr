#' @describeIn linters  Check that the \code{c} function is not used needlessly or coercively.
#' @param concatenation A character vector for the types of concatenations to report:\describe{
#'   \item{unneeded}{Unneeded concatenation with no argument or a single constant.}
#'   \item{coercive}{Coercive concatenation of heterogeneous atomic constants.}
#' }
#' @export
concatenation_linter <- function(concatenation = c("unneeded", "coercive")) {
  concatenation <- match.arg(concatenation, several.ok = TRUE)
  function(source_file) {
    tokens <- source_file[["parsed_content"]] <-
      filter_out_token_type(source_file[["parsed_content"]], "expr")
    msg_empty <- "Unneded concatenation without arguments. Replace the \"c\" call by NULL."
    msg_null <- "Unneded concatenation of NULL. Remove NULL."
    msg_single <- "Unneded concatenation of a single constant. Remove the \"c\" call."
    msg_coercive <- "Coercive concatenation of heterogeneous atomic constants. Replace the \"c\" call by list()."
    check_unneeded <- "unneeded" %in% concatenation
    check_coercive <- "coercive" %in% concatenation
    lapply(
      ids_with_token(source_file, "SYMBOL_FUNCTION_CALL"),
      function(token_num) {
        msg <- switch(detect_concat_type(token_num, tokens),
          "empty"         = if (check_unneeded) {msg_empty   },
          "null"          = if (check_unneeded) {msg_null    },
          "single"        = if (check_unneeded) {msg_single  },
          "heterogeneous" = if (check_coercive) {msg_coercive}
        )
        if (!is.null(msg)) {
          token <- with_id(source_file, token_num)
          start_col_num <- token[["col1"]]
          end_col_num <- token[["col2"]]
          line_num <- token[["line1"]]
          line <- source_file[["lines"]][[as.character(line_num)]]
          Lint(
            filename = source_file[["filename"]],
            line_number = line_num,
            column_number = start_col_num,
            type = "warn",
            message = msg,
            line = line,
            linter = "unneeded_concatenation_linter",
            ranges = list(c(start_col_num, end_col_num))
          )
        }
      }
    )
  }
}

detect_concat_type <- function(token_num, tokens) {
  # Detect the type of concatenation "empty", "null", "single", "heterogeneous" or "".
  open_paren_num <- token_num + 1L
  r <- ""
  if (tokens[token_num, "text"] == "c" &&
      tokens[open_paren_num, "token"] == "'('") {
    token_args <- get_tokens_in_parentheses(open_paren_num, tokens)
    print(token_args)
    recursive_tokens <- token_args[token_args[, "text"] == "recursive", "token"]
    plus_args <- if (length(recursive_tokens) && length(recursive_tokens == "SYMBOL_SUB")) {
      -1L # recursive argument was given
    } else {
      0L
    }
    token_args <- token_args[["token"]]
    num_args <- if (length(token_args)) {
      sum(token_args == "','") + 1L + plus_args
    } else {
      0L
    }
    if (!num_args) {
      r <- "empty"
    } else if (num_args == 1L) {
      if (token_args[[1L]] == "NULL_CONST") {
        r <- "null"
      } else if (token_args[[1L]] %in% c("STR_CONST", "NUM_CONST")) {
        r <- "single"
      }
    } else if (num_args > 1L) {
      if (any(token_args == "NULL_CONST")) {
        r <- "null"
      } else if ( any(token_args == "STR_CONST") &&   # double, integer, logical
                  any(token_args == "NUM_CONST") ) {  # character
        r <- "heterogeneous"
      }
    }
  }
  r
}

get_tokens_in_parentheses <- function(open_paren_line_num, tokens) {
  # Return the tokens enclosed by the opening parenthesis/bracket at the given line, or NA.
  open_paren_token <- tokens[open_paren_line_num, ]
  open_paren_text <- open_paren_token[["text"]]
  close_paren_token <- tail(get_sibling_tokens(open_paren_token, tokens), 1L)
  close_paren_text <- close_paren_token[["text"]]
  close_paren_line_num <- which(rownames(tokens) == rownames(close_paren_token))
  if ( (open_paren_text == "("  && close_paren_text ==  ")") ||
       (open_paren_text == "{"  && close_paren_text ==  "}") ||
       (open_paren_text == "["  && close_paren_text ==  "]") ||
       (open_paren_text == "[[" && close_paren_text == "]]") ) {
    range <- if (open_paren_line_num + 1L == close_paren_line_num) {
      integer()
    } else {
      (open_paren_line_num + 1L):(close_paren_line_num - 1L)
    }
    tokens[range, ]
  } else {
    NA
  }
}


get_sibling_tokens <- function(child, tokens) {
  # Get all siblings of the given child token (i.e. that have the same parent id)
  tokens[tokens[, "parent"] == child[["parent"]], ]
}


filter_out_token_type <- function(tokens, types) {
  tokens[!tokens[["token"]] %in% types, ]
}

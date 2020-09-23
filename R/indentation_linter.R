#'
#' @param indent The number of characters to indent each expression.
#' @param outermost_only Whether all offending lines should be reported as lint,
#'   or only the outermost offending expression. Default is \code{TRUE}, only
#'   the outermost expressions will be flagged.
#' @param func_header_to_open_paren Whether function header parameters should be
#'   wrapped to the indentation level of the first parameter, immediately
#'   following an opening parenthesis or should otherwise be treated as a
#'   regular expression, indenting to one indentation level. The default is
#'   \code{TRUE}, to follow the tidyverse style guide, indenting to the first
#'   parameter.
#' @param keyword_to_open_paren Whether keyword parenthetical expressions (such
#'   as the parentheses following `for`, `while`, `if` or `else`) should be
#'   indented so that they are aligned with the opening parenthesis. The default
#'   is \code{TRUE}.
#' @param func_call_closing_paren Whether closing parenthesis of function calls
#'   should
#' @param expr_closing_curyl Whether all curly braces that wrap multi-line
#'   expressions should return to the parent indentation level.
#'
#' @describeIn linters Indent nested expressions and return to the parent
#'   indentation for closing multi-line expressions with curly-braces.
#'
#' @export
indentation_linter <- function(indent = 2L, outermost_only = TRUE,
                               func_header_to_open_paren = TRUE,
                               keyword_to_open_paren = FALSE,
                               func_call_closing_paren = TRUE,
                               expr_closing_curly = TRUE) {

  function(source_file) {
    # short-circuit on global expression as to not double-lint indentations
    if (is.null(source_file$parsed_content)) return(list())
    pc <- add_indentation_data(source_file$parsed_content)

    # associate parent information for nested expressions
    pc <- merge(
      pc,
      pc,
      by.x = "parent",
      by.y = "id",
      suffixes = c("", ".par"),
      all.x = TRUE)

    # reorder to ensure outermost expressions are identified first
    pc <- pc[order(pc$line1, pc$col1, -pc$line2, -pc$col2, pc$parent),]

    # calculate linty indenting
    linty <- with(pc, list(
      closing_curly =
        token == "'}'" &
        line_indent != base_indent.par,

      closing_paren =
        token == "')'" &
        !is_kw_expr &   # ignore parens from FUNCTION, IF, WHILE
        !token.par %in% "forcond" &  # ignore parens from FOR forcond
        line_indent != line_indent.par,

      generalized_kw_paren_exprs =
        line1 != line1.par &
        is_kw_paren_line &
        !is.na(kw.par) & kw.par != "FUNCTION" &
        line_indent != kw_expr_line_indent.par + indent,

      hanging_kw_paren_exprs =
        line1 != line1.par &
        is_kw_paren_line &
        !is.na(kw.par) & kw.par != "FUNCTION" &
        line_indent != kw_open_paren_indent.par + 1L,

      generalized_func_header =
        line1 != line1.par &
        is_kw_paren_expr &
        !is.na(kw) & kw == "FUNCTION" &
        line_indent != kw_expr_line_indent + indent,

      hanging_func_header =
        line1 != line1.par &
        is_kw_paren_expr &
        !is.na(kw) & kw == "FUNCTION" &
        line_indent != kw_open_paren_indent + 1L,

      ifelse_indent =
        is_kw_expr & kw == "IF" &
        !is_kw_paren_line &
        line1 != line1.par &
        line_indent != base_indent.par,

      indent =
        line1 != line1.par &
        !is_kw_paren_line &
        !token %in% c("')'", "'}'", "','", "SYMBOL_FORMALS", "COMMENT") &
        !(is_kw_expr & kw == "IF") &
        line_indent != base_indent.par + indent))

    if (keyword_to_open_paren) linty$generalized_kw_paren_exprs <- FALSE
    else linty$hanging_kw_paren_exprs <- FALSE

    if (func_header_to_open_paren) linty$generalized_func_header <- FALSE
    else linty$hanging_func_header <- FALSE

    # filter out NAs from linty results, caused by missing parent
    linty <- lapply(linty, vapply, isTRUE, logical(1L))

    # filter out secondary indentation lints on the same line
    any_lint <- Reduce(`|`, linty)
    duplicated_line <- duplicated(ifelse(any_lint, pc$line1, NA), NA)
    linty <- lapply(linty, `&`, !duplicated_line)

    # filter out nested linty expressions to avoid cascading indentation lints
    if (outermost_only && any(linty$indent)) {
      nested_linty_lines <- unique(unlist(apply(
        pc[linty$indent,],
        1L,
        function(row) tail(row["line1"]:row["line2"], -1L))))
      linty$indent <- linty$indent & !pc$line1 %in% nested_linty_lines
    }

    hanging_kw_paren_exprs_lints <- mapply(
      Lint,
      line_number = pc[linty$hanging_kw_paren_exprs, "line1"],
      column_number = pc[linty$hanging_kw_paren_exprs, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = paste0(
          "Keyword parentheticals that wrap to a new line should be aligned ",
          "with the keyword's opening parenthesis."),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    generalized_kw_paren_exprs_lints <- mapply(
      Lint,
      line_number = pc[linty$generalized_kw_paren_exprs, "line1"],
      column_number = pc[linty$generalized_kw_paren_exprs, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = sprintf(
          paste0(
            "Keyword parentheticals that wrap to a new line should be ",
            "indented by %d characters."),
          indent),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    hanging_func_header_lints <- mapply(
      Lint,
      line_number = pc[linty$hanging_func_header, "line1"],
      column_number = pc[linty$hanging_func_header, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = paste0(
          "Function arguments that wrap to a new line should be aligned with ",
          "the function header's opening parenthesis."),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    generalized_func_header_lints <- mapply(
      Lint,
      line_number = pc[linty$generalized_func_header, "line1"],
      column_number = pc[linty$generalized_func_header, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = sprintf(
          paste0(
            "Function arguments that wrap to a new line should be indented by ",
            "%d characters."),
          indent),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    closing_curly_indent_lints <- mapply(
      Lint,
      line_number = pc[linty$closing_curly, "line1"],
      column_number = pc[linty$closing_curly, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = paste0(
          "Closing curly-braces should be at the same indentation level as ",
          "the parent expression."),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    closing_paren_indent_lints <- mapply(
      Lint,
      line_number = pc[linty$closing_paren, "line1"],
      column_number = pc[linty$closing_paren, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = paste0(
          "Closing parenthesis of multi-line function calls should be at the ",
          "same indentation level as the parent expression."),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    ifelse_indent_lints <- mapply(
      Lint,
      line_number = pc[linty$ifelse_indent, "line1"],
      column_number = pc[linty$ifelse_indent, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = paste0(
          "All if-else clauses should begin at the same indentation level ",
          "as the first if keyword."),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    expr_indent_lints <- mapply(
      Lint,
      line_number = pc[linty$indent, "line1"],
      column_number = pc[linty$indent, "col1"],
      MoreArgs = list(
        filename = source_file$filename,
        type = "style",
        message = sprintf(
          paste0(
            "Expressions that wrap to a new line should be indented by %d ",
            "characters."),
          indent),
        line = "",
        ranges = NULL,
        linter = "indentation_linter"),
      SIMPLIFY = FALSE)

    header_indent_lints <- if (func_header_to_open_paren) {
      hanging_func_header_lints
    } else {
      generalized_func_header_lints
    }

    kw_paren_expr_lints <- if (keyword_to_open_paren) {
      hanging_kw_paren_exprs_lints
    } else {
      generalized_kw_paren_exprs_lints
    }

    flatten_lints(list(
      header_indent_lints,
      kw_paren_expr_lints,
      if (func_call_closing_paren) closing_paren_indent_lints,
      if (expr_closing_curly) closing_curly_indent_lints,
      ifelse_indent_lints,
      expr_indent_lints))
  }
}


#' Add indentation data to the parsed content
#'
#' @param pc parsed_content as produced by \code{get_source_file}
#' @return a modified parsed_content data.frame with additional fields
#'   relating to line indentation, function header indentation and indentation
#'   relative to a parent expression's line.
#'
add_indentation_data <- function(pc) {
  # construct new parsed content df with columns for indentation data
  pc <- cbind(pc,
    kw = NA_character_,
    is_kw_expr = NA,
    is_kw_paren_expr = NA,
    is_kw_paren_line = NA,
    line_indent = NA_real_,
    kw_open_paren_indent = NA_real_,
    kw_expr_line_indent = NA_real_,
    base_indent = NA_real_)

  pc_new_col_labels <- c(
    kw = "keyword token of the next closest parent keyword expression",
    is_kw_expr = "whether the token's parent expression is a keyword syntax block",
    is_kw_paren_expr = "whether the token is part of a keyword parenthetical expression",
    is_kw_paren_line = "whether the token is on a line containing a keyword parenthetical expression",
    line_indent = "indentation of the token's line",
    kw_open_paren_indent = "indentation of a keyword block's opening parenthesis",
    kw_expr_line_indent = "indentation of a keyword block",
    base_indent = "line, or keyword syntax block indent for keyword expressions")

  pc <- pc[order(pc$line1, pc$col1, pc$parent),]
  pc$line_indent <- as.numeric(unlist(lapply(
    split(pc, pc$line1, drop = TRUE),
    function(i) rep(min(i$col1), nrow(i)))))

  # calculate indentation at the start of each keyword's opening
  # parenthesis (e.g. `if (_`, `for (_`)
  kw_tokens <- pc[pc$token %in% c("FUNCTION", "IF", "FOR", "forcond", "WHILE"),]
  kw_ids <- ifelse(kw_tokens$token == "forcond", kw_tokens$id, kw_tokens$parent)
  pc$is_kw_expr <- pc$parent %in% unique(kw_ids)

  # for each keyword syntax block, find opening paren and indentation
  pc <- pc[order(pc$parent, pc$line1, pc$col1),]
  kw <- do.call(rbind, lapply(
    split(pc[pc$is_kw_expr,], pc[pc$is_kw_expr, "parent"]),
    function(i) {
      data.frame(
        id = i$id[[1]],
        token = i$token[[1]],
        open_paren_indent = i[i$token == "'('", "col2"] %||% NA_real_,
        expr_line_indent = min(i$line_indent),
        open_paren = which(i$token == "'('") %||% NA_real_,
        closing_paren = which(i$token == "')'") %||% NA_real_,
        ntokens = nrow(i))
    }))

  # build index to replicate across all expressions of each keyword syntax block
  kw_idx <- rep(seq_along(kw$ntokens), times = kw$ntokens)

  # derive some vectors for finding expressions between open & close parens
  kw_token_i <- sequence(kw$ntokens)
  kw_after_open_paren <- kw_token_i == kw$open_paren[kw_idx]
  kw_before_close_paren <- kw_token_i == kw$closing_paren[kw_idx]

  # derive a few variables from the keyword expression data
  pc[pc$is_kw_expr, "kw"] <- kw$token[kw_idx]
  pc[pc$is_kw_expr, "kw_open_paren_indent"] <- kw$open_paren_indent[kw_idx]
  pc[pc$is_kw_expr, "kw_expr_line_indent"] <- kw$expr_line_indent[kw_idx]
  pc[pc$is_kw_expr, "is_kw_paren_expr"] <- kw_after_open_paren - kw_before_close_paren
  pc[is.na(pc$is_kw_paren_expr), "is_kw_paren_expr"] <- 0L

  # derive kw as the +/- first token id of each keyword block
  pc$kw <- numeric(nrow(pc))
  pc$kw[pc$is_kw_expr] <- kw$id[kw_idx] * ((kw_token_i == 1L) - (kw_token_i == kw$ntokens[kw_idx]))

  # reorder by start position in text
  pc <- pc[order(pc$line1, pc$col1, pc$parent),]

  # derive whether an expression is within bookended parentheses markers
  pc$is_kw_paren_expr <- (cumsum(pc$is_kw_paren_expr) > 0L)
  is_kw_paren_end <- which(diff(pc$is_kw_paren_expr) == -1L) + 1L
  pc$is_kw_paren_expr[is_kw_paren_end] <- pc$is_kw_paren_expr[is_kw_paren_end - 1L]
  pc$is_kw_paren_line <- as.logical(unlist(lapply(
    split(pc, pc$line1, drop = TRUE),
    function(i) rep(any(i$is_kw_paren_expr), nrow(i)))))

  # associate keyword by first token id
  kw_row <- match(stack_to_segments(pc$kw), pc$id)
  pc$kw_open_paren_indent <- pc$kw_open_paren_indent[kw_row]
  pc$kw <- pc$token[kw_row]
  pc$kw[pc$kw == "'('"] <- "forcond"  # fix irregular forcond exprs

  # derive base indent based on keyword block or line indent
  pc$base_indent <- ifelse(pc$is_kw_expr, pc$kw_expr_line_indent, pc$line_indent)

  for (col in names(pc_new_col_labels))
    attr(pc[[col]], "label") <- pc_new_col_labels[[col]]

  pc
}



#' Convert a stack of bookends into independent segments
#'
#' Given a stack, provided as a vector of positive and negative bookend
#' identifiers, convert the stack into a vector of identifiers highest on the
#' stack at each position.
#'
#' @examples
#' # Stack:                                 |--- 7 ---|
#' #                                |----------- 4 -------|
#' #            |--- 1 ---|     |--------------- 9 ------------| |----- 3 -----|
#' x <- c(0, 0,  1,  0, -1,  0,  9,  4,  0,  7,  0, -7, -4,  -9,  3,  0,  0, -3)
#' stack_to_segments(x)
#' #> [1] 0  0   1   1   1   0   9   4   4   7   7   7   4    9   3   3   3   3
#' # Segments:  |--- 1 ---|     |9| |- 4 -| |--- 7 ---| |4|  |9| |----- 3 -----|
#'
#' x <- c(0, 1, 2, 0, 0, -2, -1,  0)
#' stack_to_segments(x)
#' #> [1] 0 1 2 2 2 2 1 0
#'
#'
stack_to_segments <- function(x) {
  is_nz <- x != 0L
  if (!sum(is_nz)) return(x)

  x[length(x)] <- 0
  x[is.na(x)] <- 0L
  x_in <- x

  # use diffs for sequential rises
  i <- which(sign(x[is_nz]) + lag(sign(x[is_nz])) == 2L)
  x[is_nz][i] <- x[is_nz][i] - x[is_nz][i - 1L]

  # lag falls and add into following value
  which_lt_z <- which(x_in < 0)
  x[which_lt_z + 1L] <- x[which_lt_z + 1L] + x[which_lt_z]
  x[which_lt_z] <- 0

  # use diffs for sequential falls
  i <- which(sign(x_in[is_nz]) + lag(sign(x_in[is_nz])) == -2L)
  x[is_nz][i] <- x_in[is_nz][i - 1L] - x_in[is_nz][i]

  cumsum(x)
}

#' @include utils.R
#' @describeIn linters check that a certain kind of indentation is used
#' @export
indentation_linter <- function(spaces, linter_name = "indentation_linter") {
  fail_msg <- paste("'spaces' needs to be FALSE, '\\t' or numeric, not", class(spaces))

  if (length(spaces) != 1L) stop(fail_msg)

  if (!spaces || spaces == '\t') {
    allowed <- "\t"
    message <- "Use tabs to indent."
  } else if (is.numeric(spaces)) {
    nspc <- spaces  # else rex' internal 'spaces' is used!
    allowed <- rex(n_times(" ", nspc))
    message <- sprintf("Use %s spaces to indent.", spaces)
  } else {
    stop(fail_msg)
  }

  function(source_file) {
    res <- re_matches(source_file$lines,
      rex(start, zero_or_more(allowed, type = "possessive"), some_of(" ", "\t")),
      locations = TRUE, global = TRUE)

    lapply(seq_along(source_file$lines), function(line_number) {
      lint_indent <- function(start, end) {
        if (is.na(start)) {
          return()
        }

        Lint(
          filename = source_file$filename,
          line_number = line_number,
          column_number = start,
          type = "style",
          message = message,
          line = source_file$lines[line_number],

          # R outputs tabs with 8 spaces
          # TODO: this is incorrect for embedded tabs, I am not going to fix it.
          ranges = list(c(start, end)),
          linter = linter_name
        )
      }

      hits <- res[[line_number]]
      mapply(lint_indent, hits$start, hits$end, SIMPLIFY = FALSE)
    })
  }
}

#' @describeIn linters check that only spaces are used, never tabs.
#' @export
no_tab_linter <- indentation_linter(2L, "no_tab_linter")

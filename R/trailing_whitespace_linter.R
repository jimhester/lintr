#' @describeIn linters check there are no trailing whitespace characters.
#' @export
trailing_whitespace_linter <- function(check_empty_lines = TRUE) {
  # for backwards compatibility, check if this was called on the
  # check_empty_lines option or directly on a source file
  if (is.logical(check_empty_lines)) {
    function(source_file) trailws_linter(source_file, check_empty_lines)
  } else {  # call with old default
    source_file <- check_empty_lines
    trailws_linter(source_file, TRUE)
  }
}

trailws_linter <- function(source_file, check_empty_lines) {
  res <- re_matches(source_file$lines,
    rex(capture(name = "space", some_of(" ", regex("\\t"))), or(newline, end)),
    global = TRUE,
    locations = TRUE)

  if (any(lapply(res, nrow) != 1L)) {
    stop("invalid data: text after '\\n' found")
  }

  lapply(seq_along(source_file$lines), function(idx) {
    start <- res[[idx]]$space.start
    end   <- res[[idx]]$space.end

    skip_empty_line <- !check_empty_lines && start == 1L
    if (is.na(start) || skip_empty_line) {
      return()
    }

    Lint(
      filename = source_file$filename,
      line_number = names(source_file$lines)[[idx]],
      column_number = start,
      type = "style",
      message = "Trailing whitespace is superfluous.",
      line = source_file$lines[[idx]],
      ranges = list(c(start, end)),
      linter = "trailing_whitespace_linter"
    )
  })
}

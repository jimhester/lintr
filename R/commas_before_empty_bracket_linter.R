#' @describeIn linters check that all commas do not have spaces before them 
#' unless after a [.
#' @export
commas_before_empty_bracket_linter <- function(source_file) {
  
  re <- rex(list(one_or_more(" "), ",") %or% list(",", non_space))

  res <- re_matches(source_file$lines, re, global = TRUE, locations = TRUE)

  lapply(seq_along(res), function(id) {
    line_number <- names(source_file$lines)[id]

    mapply(
        FUN = function(start, end) {
          if (is.na(start)) {
            return()
          }

          lints <- list()

          line <- unname(source_file$lines[[id]])

          comma_loc <- start + re_matches(substr(line, start, end), rex(","),
                                          locations = TRUE)$start - 1L

          space_before <- substr(line, comma_loc - 1L, comma_loc - 1L) %==% " "

          if (space_before) {

            has_token <- any(source_file$parsed_content$line1 == line_number &
              source_file$parsed_content$col1 == comma_loc &
              source_file$parsed_content$token == "','")

            start_of_line <- re_matches(line, rex(start, spaces, ","))

            empty_comma <- 
              substr(line, comma_loc - 2L, comma_loc - 1L) %==% ", "
            empty_bracket <-
              substr(line, comma_loc - 2L, comma_loc - 2L) %==% "["
            if (has_token && !start_of_line && !empty_comma && !empty_bracket) {
              lints[[length(lints) + 1L]] <-
                Lint(
                  filename = source_file$filename,
                  line_number = line_number,
                  column_number = comma_loc,
                  type = "style",
                  message = "Commas should never have a space before.",
                  line = line,
                  ranges = list(c(start, end)),
                  "commas_linter"
                  )
            }
          }
          lints
        },
        start = res[[id]]$start,
        end = res[[id]]$end,
        SIMPLIFY = FALSE
        )
})
}

#' @describeIn linters  Check that functions avoid the use of non-local variables. Note that this
#' uses \code{\link[base]{eval}}, so do not use with untrusted code.
#' @export
nonlocal_variable_linter <- function(source_file) {
  x <- global_xml_parsed_content(source_file)
  if (is.null(x)) {
    return()
  }
  Map(
    function(xml_func, source_file) {
      lapply(
        nonlocal_var_locations(xml_func, source_file),
        function(var_loc) {
          Lint(
            filename = source_file$filename,
            line_number = var_loc[["line1"]],
            column_number = var_loc[["col1"]],
            type = "warning",
            message = "Do not use non-local variables.",
            line = source_file[["file_lines"]][[var_loc[["line1"]]]],
            ranges = list(c(var_loc[["col1"]], var_loc[["col2"]])),
            linter = "nonlocal_variable_linter"
          )
        }
      )
    },
    c(toplevel_functions_xml(x), inner_functions_xml(x)),
    MoreArgs = list(source_file = source_file)
  )
}

toplevel_functions_xml <- function(x) {
  xpath <- paste0(
    # top-level left-assigned function
    "/exprlist/expr[expr/SYMBOL][LEFT_ASSIGN|EQ_ASSIGN][expr[FUNCTION]]/expr[FUNCTION]",
    "|",
    # or top-level anonymous or right-assigned function
    "/exprlist/expr[FUNCTION]"
  )
  xml2::xml_find_all(x, xpath)
}

inner_functions_xml <- function(x) {
  xpath <- paste0(
    "/exprlist/*//expr[expr/SYMBOL][LEFT_ASSIGN|EQ_ASSIGN][expr[FUNCTION]]/expr[FUNCTION]",
    "|",
    "/exprlist/*//*[not(self::LEFT_ASSIGN) and not(self::EQ_ASSIGN)]/expr[FUNCTION]"
  )
  xml2::xml_find_all(x, xpath)
}

function_source <- function(x, source_file) {
  pos <- xml_location(x)
  lines <- source_file[["file_lines"]][seq.int(pos[["line1"]], pos[["line2"]])]
  lines[[1L]] <- substr(lines[[1L]], pos[["col1"]], nchar(lines[[1L]]))
  last_line <- length(lines)
  last_char <- if (last_line == 1L) {
    pos[["col2"]] - pos[["col1"]] + 1L
  } else {
    pos[["col2"]]
  }
  lines[[last_line]] <- substr(lines[[last_line]], 1L, last_char)
  paste0(lines, collapse = "\n")
}

xml_location <- function(x) {
  attrs <- xml2::xml_attrs(x)
  pos <- as.integer(attrs)
  names(pos) <- names(attrs)
  pos
}

nonlocal_var_locations <- function(xml_func, source_file) {
  env <- new.env()
  func <- try_silently(eval(
    parse(text = function_source(xml_func, source_file), keep.source = TRUE),
    envir = env
  ))
  vars <- codetools::findGlobals(func, merge = FALSE)[["variables"]]
  names(vars) <- vars
  is_nonlocal_var <- !vapply(vars, exists, logical(1), envir = env)
  vars <- vars[is_nonlocal_var]
  res <- variable_locations(xml_func, vars)
  res
}


variable_locations <- function(x, var_names) {
  # return the variable locations, drop unfound variables because of false positives e.g. "obj@attr"
  locs <- lapply(
    var_names,
    function(var_name, xml) {
      xml_location(xml2::xml_find_first(xml, paste0(".//SYMBOL[text() = '", var_name,"']")))
    },
    x
  )
  locs <- locs[!is.na(locs)]
}

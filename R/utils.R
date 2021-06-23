`%||%` <- function(x, y) {
  if (is.null(x) || length(x) <= 0 || is.na(x[[1L]])) {
    y
  } else {
    x
  }
}

`%==%` <- function(x, y) {
  identical(x, y)
}

`%!=%` <- function(x, y) {
  !identical(x, y)
}

"%:::%" <- function(p, f) {
  get(f, envir = asNamespace(p))
}

flatten_lints <- function(x) {
  structure(
    flatten_list(x, class = "lint"),
    class = "lints"
  )
}

# any function using unlist or c was dropping the classnames,
# so need to brute force copy the objects
flatten_list <- function(x, class) {

  res <- list()
  itr <- 1L
  assign_item <- function(x) {
    if (inherits(x, class)) {
      res[[itr]] <<- x
      itr <<- itr + 1L
    }
    else if (is.list(x)) {
      lapply(x, assign_item)
    }
  }
  assign_item(x)
  res

}

fix_names <- function(x, default) {
  nms <- names(x)

  if (is.null(nms)) {
    nms <- default
  }
  else {
    nms[nms == ""] <- default
  }
  names(x) <- nms
  x
}

names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

linter_auto_name <- function(which = -3L) {
  call <- sys.call(which = which)
  nm <- paste(deparse(call, 500L), collapse = " ")
  regex <- rex(start, one_or_more(alnum %or% "." %or% "_"))
  if (re_matches(nm, regex)) {
    match <- re_matches(nm, regex, locations = TRUE)
    nm <- substr(nm, start = 1L, stop = match[1L, "end"])
  }
  nm
}

auto_names <- function(x) {
  nms <- names2(x)
  missing <- nms == ""
  if (all(!missing)) return(nms)

  default_name <- function(x) {
    if (inherits(x, "linter")) {
      attr(x, "name", exact = TRUE)
    } else {
      paste(deparse(x, 500L), collapse = " ")
    }
  }
  defaults <- vapply(x[missing], default_name, character(1), USE.NAMES = FALSE)

  nms[missing] <- defaults
  nms
}

# The following functions is from dplyr
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

safe_parse_to_xml <- function(parsed_content) {
  if (is.null(parsed_content)) return(NULL)
  tryCatch(xml2::read_xml(xmlparsedata::xml_parse_data(parsed_content)), error = function(e) NULL)
}

get_content <- function(lines, info) {
  lines[is.na(lines)] <- ""

  if (!missing(info)) {
    lines[length(lines)] <- substr(lines[length(lines)], 1L, info$col2)
    lines[1] <- substr(lines[1], info$col1, nchar(lines[1]))
  }
  paste0(collapse = "\n", lines)
}

logical_env <- function(x) {
  res <- as.logical(Sys.getenv(x))
  if (is.na(res)) {
    return(NULL)
  }
  res
}

# from ?chartr
rot <- function(ch, k = 13) {
  p0 <- function(...) paste(c(...), collapse = "")
  alphabet <- c(letters, LETTERS, " '")
  idx <- seq_len(k)
  chartr(p0(alphabet), p0(c(alphabet[-idx], alphabet[idx])), ch)
}

trim_ws <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
}

`@` <- function(x, y) {
  name <- as.character(substitute(y))
  attr(x, name, exact = TRUE)
}

global_xml_parsed_content <- function(source_file) {
  if (exists("file_lines", source_file)) {
    source_file$full_xml_parsed_content
  }
}

get_file_line <- function(source_file, line) {
  unname(source_file$file_lines[[as.numeric(line)]])
}

p <- function(...) paste0(...)

lengths <- function(x) vapply(x, length, integer(1L))

try_silently <- function(expr) {
  suppressWarnings(
    suppressMessages(
      try(expr, silent = TRUE)
    )
  )
}

viapply <- function(x, ...) vapply(x, ..., FUN.VALUE = integer(1))

# imitate sQuote(x, q) [requires R>=3.6]
quote_wrap <- function(x, q) paste0(q, x, q)

unquote <- function(str, q="`") {
  # Remove surrounding quotes (select either single, double or backtick) from given character vector
  # and unescape special characters.
  str <- re_substitutes(str, rex(start, q, capture(anything), q, end), "\\1")
  unescape(str, q)
}

escape_chars <- c(
  "\\\\" = "\\",  # backslash
  "\\n"  = "\n",  # newline
  "\\r"  = "\r",  # carriage return
  "\\t"  = "\t",  # tab
  "\\b"  = "\b",  # backspace
  "\\a"  = "\a",  # alert (bell)
  "\\f"  = "\f",  # form feed
  "\\v"  = "\v"   # vertical tab
  # dynamically-added:
  #"\\'"  --> "'",  # ASCII apostrophe
  #"\\\"" --> "\"", # ASCII quotation mark
  #"\\`"  --> "`"   # ASCII grave accent (backtick)
)

unescape <- function(str, q="`") {
  names(q) <- paste0("\\", q)
  my_escape_chars <- c(escape_chars, q)
  res <- gregexpr(text = str, pattern = rex(or(names(my_escape_chars))))
  all_matches <- regmatches(str, res)
  regmatches(str, res) <- lapply(
    all_matches,
    function(string_matches) {
      my_escape_chars[string_matches]
    }
  )
  str
}

# convert an XML match into a Lint
xml_nodes_to_lint <- function(xml, source_file, message,
                              type = c("style", "warning", "error"),
                              offset = 0L, fix = NULL, global = FALSE) {
  type <- match.arg(type, c("style", "warning", "error"))
  line1 <- xml2::xml_attr(xml, "line1")[1]
  col1 <- as.integer(xml2::xml_attr(xml, "col1")) + offset

  line_elt <- if (global) "file_lines" else "lines"

  if (xml2::xml_attr(xml, "line2") == line1) {
    col2 <- as.integer(xml2::xml_attr(xml, "col2")) + offset
  } else {
    col2 <- unname(nchar(source_file[[line_elt]][line1]))
  }
  return(Lint(
    filename = source_file$filename,
    line_number = as.integer(line1),
    column_number = as.integer(col1),
    type = type,
    message = message,
    line = source_file[[line_elt]][line1],
    ranges = list(c(col1 - offset, col2)),
    fixes = as.character(fix)
  ))
}

# interface to work like options() or setwd() -- returns the old value for convenience
set_lang <- function(new_lang) {
  old_lang <- Sys.getenv("LANGUAGE", unset = NA)
  Sys.setenv(LANGUAGE = new_lang)
  old_lang
}
# handle the logic of either unsetting if it was previously unset, or resetting
reset_lang <- function(old_lang) {
  if (is.na(old_lang)) Sys.unsetenv("LANGUAGE") else Sys.setenv(LANGUAGE = old_lang)
}

#' Create a \code{linter} closure
#' @param fun A function that takes a source file and returns \code{lint} objects.
#' @param name Default name of the Linter.
#' Lints produced by the linter will be labelled with \code{name} by default.
#' @return The same function with its class set to 'linter'.
#' @export
Linter <- function(fun, name = linter_auto_name()) { # nolint: object_name_linter.
  if (!is.function(fun) || length(formals(args(fun))) != 1L) {
    stop("`fun` must be a function taking exactly one argument.", call. = FALSE)
  }
  force(name)
  structure(fun, class = "linter", name = name)
}

read_lines <- function(file, ...) {
  terminal_newline <- TRUE
  lines <- withCallingHandlers({
    readLines(file, warn = TRUE, ...)
  },
  warning = function(w) {
    if (grepl("incomplete final line found on", w$message, fixed = TRUE)) {
      terminal_newline <<- FALSE
      invokeRestart("muffleWarning")
    }
  })
  attr(lines, "terminal_newline") <- terminal_newline
  lines
}

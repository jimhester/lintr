`%||%` <- function(x, y) {
  if (is.null(x) || length(x) <= 0) {
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


# sort lints and remove duplicates
tidy_lints <- function(lints) {
  files <- vapply(lints, `[[`, character(1L), "filename")
  lines <- vapply(lints, `[[`, integer(1L), "line_number")
  columns <- vapply(lints, `[[`, integer(1L), "column_number")
  messages <- vapply(lints, `[[`, character(1), "message")

  dups <- which(duplicated(files) & duplicated(lines) & duplicated(columns) & duplicated(messages))
  order <- order(files, lines, columns)

  lints[ order[!order %in% dups] ]
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

auto_names <- function(x) {
  nms <- names2(x)
  missing <- nms == ""
  if (all(!missing)) return(nms)

  deparse2 <- function(x) paste(deparse(x, 500L), collapse = "")
  defaults <- vapply(x[missing], deparse2, character(1), USE.NAMES = FALSE)

  nms[missing] <- defaults
  nms
}

blank_text <- function(s, re, shift_start = 0, shift_end = 0) {
  m <- gregexpr(re, s, perl = TRUE)
  regmatches(s, m) <- lapply(regmatches(s, m),
    quoted_blanks,
    shift_start = shift_start,
    shift_end = shift_end)

  s
}

quoted_blanks <- function(matches, shift_start = 0, shift_end = 0) {
  lengths <- nchar(matches)
  blanks <- vapply(Map(rep.int,
      rep.int(" ", length(lengths - (shift_start + shift_end))),
      lengths - (shift_start + shift_end), USE.NAMES = FALSE),
    paste, "", collapse = "")

  substr(matches, shift_start + 1L, nchar(matches) - shift_end) <- blanks
  matches
}

ids_with_token <- function(source_file, value, fun = `==`) {
  if (source_file$parsed_content$col1 %==% integer(0)) {
    return(integer(0))
  }
  loc <- which(fun(source_file$parsed_content$token, value))
  if (loc %==% integer(0)) {
    return(integer(0))
  }
  loc
}

# The following functions is from dplyr
names2 <- function(x) {
  names(x) %||% rep("", length(x))
}

recursive_ls <- function(env) {
  if (parent.env(env) %!=% emptyenv()) {
    c(ls(envir = env), recursive_ls(parent.env(env)))
  }
  else {
    ls(envir = env)
  }
}

with_id <- function(source_file, id) {
  if (is.null(source_file$parsed_content)) {
    return(data.frame())
  }
  source_file$parsed_content[id, ]
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
  A <- c(letters, LETTERS, " '")
  I <- seq_len(k); chartr(p0(A), p0(c(A[-I], A[I])), ch)
}

trim_ws <- function(x) {
  sub("^\\s+", "", sub("\\s+$", "", x))
}

`@` <- function(x, y) {
  name <- as.character(substitute(y))
  attr(x, name, exact = TRUE)
}

global_parsed_content <- function(source_file) {
  if (exists("file_lines", source_file)) {
    source_file$parsed_content
  }
}

global_xml_parsed_content <- function(source_file) {
  if (exists("file_lines", source_file)) {
    source_file$xml_parsed_content
  }
}

get_file_line <- function(source_file, line) {
  unname(source_file$file_lines[[as.numeric(line)]])
}

p <- function(...) paste0(...)

try_silently <- function(expr) {
  suppressWarnings(
    suppressMessages(
      try(expr, silent = TRUE)
    )
  )
}

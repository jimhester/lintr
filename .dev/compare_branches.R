#!/usr/local/bin/Rscript

# compare the lints obtained before/after a given PR/branch vs current master

library(optparse)
library(dplyr)
library(purrr)
library(tibble)
library(usethis)
library(gert)
library(devtools)

if (!file.exists("lintr.Rproj")) {
  "compare_branches.R should be run inside the lintr-package directory"
}

param_list <- list(
  optparse::make_option(
    "--linters",
    default = "object_usage_linter",
    help = "Run the comparison for these linter(s) (comma-separated) [default %default]"
  ),
  optparse::make_option(
    "--branch",
    default = if (interactive()) {
      readline("Name a branch to compare to master (or skip to enter a PR#): ")
    },
    help = "Run the comparison for master vs. this branch"
  ),
  optparse::make_option(
    "--pr",
    default = if (interactive()) {
      as.integer(readline("Name a PR # to compare to master (skip if you've entered a branch): "))
    },
    type = "integer",
    help = "Run the comparison for master vs. this PR"
  ),
  optparse::make_option(
    "--packages",
    default = if (interactive()) {
      readline("Provide a comma-separated list of packages (skip to provide a directory): ")
    },
    help = "Run the comparison using these packages (comma-separated)"
  ),
  optparse::make_option(
    "--pkg_dir",
    default = if (interactive()) {
      readline("Provide a directory where to select packages (skip if already provided as a list): ")
    },
    help = "Run the comparison using all packages in this directory"
  ),
  optparse::make_option(
    "--sample_size",
    type = "integer",
    default = if (interactive()) {
      as.integer(readline("Enter the number of packages to include (skip to include all): "))
    },
    help = "Select a sample of this number of packages from 'packages' or 'pkg_dir'"
  ),
  optparse::make_option(
    "--outfile",
    default = file.path("~", sprintf("lintr_compare_branches_%d.csv", as.integer(Sys.time()))),
    help = "Destination file to which to write the output"
  )
)

params <- optparse::parse_args(optparse::OptionParser(option_list = param_list))
# treat any skipped arguments from the prompt as missing
if (interactive()) {
  for (opt in c("branch", "pr", "packages", "pkg_dir", "sample_size")) {
    if (params[[opt]] == "") params[[opt]] <- NULL
  }
}

linter_names <- strsplit(params$linters, ",", fixed = TRUE)[[1L]]

# prioritize "branch"
is_branch <- FALSE
if (!is.null(params$branch)) {
  branch <- params$branch
  is_branch <- TRUE
} else if (!is.null(params$pr)) {
  pr <- params$pr
} else {
  stop("Please supply a branch (--branch) or a PR number (--pr)")
}

# prioritize packages
if (!is.null(params$packages)) {
  packages <- strsplit(params$packages, ",", fixed = TRUE)[[1L]]
} else if (!is.null(params$pkg_dir)) {
  packages <- list.files(normalizePath(params$pkg_dir), full.names = TRUE)
} else {
  stop("Please supply a comma-separated list of packages (--packages) or a directory of packages (--pkg_dir)")
}
# filter to (1) package directories or (2) package tar.gz files
packages <- packages[
  file.exists(packages) &
    (
      file.exists(file.path(packages, "DESCRIPTION")) |
        grepl("^[a-zA-Z0-9.]+_[0-9.-]+\\.tar\\.gz", basename(packages))
    )
]

if (!is.null(params$sample_size)) {
  packages <- sample(packages, min(length(packages), params$sample_size))
}

# test if nchar(., "chars") works as intended
#   for all files in dir (see #541)
test_encoding <- function(dir) {
  tryCatch({
    lapply(
      list.files(dir, pattern = "(?i)\\.r(?:md)?$", recursive = TRUE, full.names = TRUE),
      function(x) nchar(readLines(x, warn = FALSE))
    )
    FALSE
  }, error = function(x) TRUE)
}

# read Depends from DESCRIPTION
get_deps <- function(pkg) {
  deps <- read.dcf(file.path(pkg, "DESCRIPTION"), c("Imports", "Depends"))
  deps <- toString(deps[!is.na(deps)])
  if (deps == "") return(character())
  deps <- strsplit(deps, ",", fixed = TRUE)[[1L]]
  deps <- trimws(gsub("\\([^)]*\\)", "", deps))
  deps <- deps[deps != "R"]
  deps
}

lint_all_packages <- function(pkgs, linter, check_depends) {
  pkg_is_dir <- file.info(pkgs)$isdir
  pkg_names <- dplyr::if_else(
    pkg_is_dir,
    basename(pkgs),
    gsub("_.*", "", basename(pkgs))
  )

  map(
    seq_along(pkgs),
    function(ii) {
      if (!pkg_is_dir[ii]) {
        tmp <- file.path(tempdir(), pkg_names[ii])
        on.exit(unlink(tmp, recursive = TRUE))
        # --strip-components makes sure the output structure is
        # /path/to/tmp/pkg/ instead of /path/to/tmp/pkg/pkg
        utils::untar(pkgs[ii], exdir = tmp, extras="--strip-components=1")
        pkg <- tmp
      }
      if (test_encoding(pkg)) {
        warning(sprintf(
          "Package %s has some files with unknown encoding; skipping",
          pkg_names[ii]
        ))
        return(NULL)
      }
      # object_usage_linter requires running package code, which may
      #   not work if the package has unavailable Depends;
      # object_name_linter also tries to run loadNamespace on Imports
      #   found in the target package's NAMESPACE file
      if (check_depends) {
        pkg_deps <- get_deps(pkg)
        if ("tcltk" %in% pkg_deps && !capabilities("tcltk")) {
          warning(sprintf(
            "Package %s depends on tcltk, which is not available (via capabilities()); skipping",
            pkg_names[ii]
          ))
          return(NULL)
        }
        try_deps <- tryCatch(
          find.package(pkg_deps),
          error = identity, warning = identity
        )
        if (inherits(try_deps, c("warning", "error"))) {
          warning(sprintf(
            "Some package Dependencies for %s were unavailable: %s; skipping",
            pkg_names[ii],
            gsub("there (?:are no packages|is no package) called ", "", try_deps$message)
          ))
          return(NULL)
        }
      }
      lint_dir(pkg, linters = linter, parse_settings = FALSE)
    }
  ) %>%
    set_names(pkg_names)
}

format_lints <- function(x) {
  x %>%
    purrr::map(tibble::as_tibble) %>%
    dplyr::bind_rows(.id = "package")
}

run_lints <- function(pkgs, linter, check_depends) {
  format_lints(lint_all_packages(pkgs, linter, check_depends))
}

run_on <- function(what, pkgs, linter_name, ...) {
  switch(
    what,
    master = {
      gert::git_branch_checkout("master")
    },
    pr = {
      usethis::pr_fetch(...)
    },
    branch = {
      gert::git_branch_checkout(...)
    }
  )
  devtools::load_all()

  linter <- get(linter_name)()

  check_depends <- linter_name %in% c("object_usage_linter", "object_name_linter")

  run_lints(pkgs, linter, check_depends = check_depends)
}

run_pr_workflow <- function(linter_name, pkgs, pr) {
  old_branch <- gert::git_branch()
  on.exit(gert::git_branch_checkout(old_branch))

  dplyr::bind_rows(
    main = run_on("master", pkgs, linter_name),
    pr = run_on("pr", pkgs, linter_name, number = pr),
    .id = "source"
  )
}

run_branch_workflow <- function(linter_name, pkgs, branch) {
  old_branch <- gert::git_branch()
  on.exit(gert::git_branch_checkout(old_branch))

  dplyr::bind_rows(
    main = run_on("master", pkgs, linter_name),
    branch = run_on("branch", pkgs, linter_name, branch = branch),
    .id = "source"
  )
}

###############################################################################
# TODO: handle the case when working directory is not the lintr directory
###############################################################################

message("Comparing the output of the following linters: ", toString(linter_names))
if (is_branch) {
  message("Comparing branch ", branch, " to master")
} else {
  message("Comparing PR#", pr, " to master")
}
message("Comparing output of lint_dir run for the following packages: ", toString(basename(packages)))

if (is_branch) {
  lints <- purrr::map_df(linter_names, run_branch_workflow, packages, branch)
} else {
  lints <- purrr::map_df(linter_names, run_pr_workflow, packages, pr)
}

write.csv(lints, params$outfile, row.names = FALSE)
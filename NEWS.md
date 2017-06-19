# lintr 1.0.0.9001 #
* Export expect_lint() (#178, #210)
* Added proper handling of tab characters (fixes #44, @fangly)
* Fix line number sometimes wrongly reported by no_tab_linter() (#134, @fangly)
* Fix line and column number sometimes wrongly reported by spaces_inside_linter()
  (#203, @fangly)
* Add `pipe_continuation_linter()` (#216).
* Deprecated camel_case_linter(), snake_case_linter() and multiple_dots_linter()
  in favor of object_name_linter() which enforce the given style: snake_case,
  dotted.case, lowerCamelCalse, UpperCamelCase, alllowercase or ALLUPPERCASE
  (#59, @fangly).
* Deprecated absolute_paths_linter() in favor of the new absolute_path_linter(),
  with a lax mode for fewer false positive lints (#199, fangly).
* New semicolon_terminator_linter() reports semicolons at the end a line (#147,
  @gaborcsardi) and between expressions (#181, @fangly).
* New nonportable_path_linter() identifies paths constructed without file.path()
  (@fangly).
* New concatenation_linter() lints unneeded and (some) coercive uses of c() (@fangly).
* New T_and_F_symbol_linter() warns when using T and F instead of TRUE and FALSE
  (@fangly).
* New todo_comment_linter() lints TODOs (@fangly).
* New implicit_integer_linter() detects round numbers not declared as integers,
  i.e. 1 instead of 1L (@fangly).
* New extraction_operator_linter() checks that the `[[` operator is used when
  extracting a single element from an object, not `[` (subsetting) nor `$`
  (interactive use) (@fangly).
* Undesirable_operator_linter() and undesirable_function_linter() lint uses of
  user-specified functions and operators (#48, #149, @fangly).
* Relaxed the commented_code_linter(): do not lint comments within roxygen blocks
  and do not consider "-" an R operator to avoid too many false positives.
* Fixed object linters to only lint objects declared in the current file
  (#76, #108, #136, #191, #194, #201, @fangly).
* Fixed expect_lint() issues (#180, #211, @fangly): markers were displayed when
  check was NULL, some error messages were malformed.
* Fixed Lint() / as.data.frame() error (#179, @fangly).
* Do not error with inline \\Sexpr (#127).
* Do not error with '<% %>' constructs (#185).
* Allow closing parenthesis or comma after closing curly brace (#167, @Enchufa2)
* Support checkstyle XML output (#156, @joshkgold)
* seq_linter, finds `1:length(...)` (and similar) expressions (#155, @gaborcsardi)
* linters can use the XML parse tree as well now, via the
  https://github.com/MangoTheCat/xmlparsedata package (#154, @gaborcsardi)
* lintr does not need the igraph package any more (#152, @gaborcsardi)
* Fixed lint_package bug where cache was not caching (#146, @schloerke)
* Fixed cache not saved in a directory other than requested (#213, @fangly)
* Commas linter handles missing arguments calls properly (#145)
* Add `function_left_parentheses_linter` to check that there is no space between
  a function name and its left parentheses (#204, @jrnold).

# lintr 1.0.0 #
* bugfix to work with testthat 1.0.0

# lintr 0.3.3 #
* infix_spaces_linter now properly checks `=` in named arguments. (#130, @saurfang).
* commas_linter now properly recognizes lints when preceded by a blank line and
  points to the missing space rather than the comma (#111, #129, @saurfang).
* Make spaces_left_parentheses_linter more robust when determining `(` type (#128, @saurfang)
* commented_code_linter (#83, @jackwasey)
* Now trims long comments (#55, reported by @paulstaab)
* Automatic commenting of Github commits and pull requests when linting on Travis-CI
* expect_lint_free expectation can be added to testthat unit tests.
* Robust configuration system and exclusion logic
* Emacs and Sublime Text 3 plugins now available from their respective package repositories.
* add `names.lints`, `split.lints` (#49, @ttriche)
* Fixed bug that caused vim syntatic plugin not to work properly in windows (#46, @abossenbroek)
* allow lintr customization per project using `.lintr` config files.
* use `globalenv()` instead of `baseenv()` for default parent environment so
  that `methods` will be included.
* do not check object usage if eval fails.  Fixes (#24, reported by @fabian-s)
* `trailing_whitespace_linter` was reporting the incorrect line number
* Use RStudio source marker API to display lints (#37, @jjallaire)
* Permit single quotes if they quote literal double quotes (#28, @jackwasey)
* # nolint comments are respected with caching (#68, @krlmlr)
* Properly handle all knitr document formats

# lintr 0.2.0 #

* Initial release

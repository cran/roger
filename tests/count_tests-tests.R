### roger: Automated grading of R scripts
###
### Tests for the validity of function count_tests.
###
### AUTHORS: Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later.

library(roger)

## Tests are run by R CMD check in a non interactive session where
## parse data is thrown out by default.
if (!interactive())
    options(keep.source = TRUE)

TESTS_FILE <- tempfile(fileext = ".R")
cat(
"stopifnot(1 == 1)",
"tools::assertError(stop(\"error message\"))",
"tools::assertWarning(warning(\"warning message\"))",
"stopifnot(2 == 2, 3 == 3)",
"stopifnot(exprs = {",
"    4 == 4",
"    5 == 5",
"    6 == 6",
"})",
"## 'stopifnot' in a function is ignored",
"f <- function() stopifnot(6 == 6)",
file = TESTS_FILE, sep = "\n")

stopifnot(count_tests(TESTS_FILE) == 8L)

## NOT RUN: check the actual number of tests in interface-tests.R.
#stopifnot(count_tests("interface-tests.R") == 30)
## END NOT RUN

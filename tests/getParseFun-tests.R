### roger: Automated grading of R scripts
###
### Tests for getParseFun and friends.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later.

library(roger)

## Tests are run by R CMD check in a non interactive session where
## parse data is thrown out by default.
if (!interactive())
    options(keep.source = TRUE)

FILE <- tempfile(fileext = ".R")
cat("
foo <- function(x, y) x

bar <- function(x, z)
{
    baz <- function(x) x^2
    x^2 + baz(z)
}
",
file = FILE)
parseData <- getSourceData(FILE)$parseData

## Expected results
FUN <- c("foo", "bar")
FORMALS <- list(c("x", "y"), c("x", "z"))

## Tests
stopifnot(exprs = {
    identical(FUN,
              roger:::getParseFun(parseData))
    identical(FORMALS,
              roger:::getParseFormals(parseData))
})

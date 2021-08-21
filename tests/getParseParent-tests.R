### roger: Automated grading of R scripts
###
### Tests for getParseParent and friends.
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

## Sample arguments
SINGLE_ID <- 3
MULTIPLE_ID <- c(3, 27, 39)
PARENT_ID <- 52
OP_ID <- 47

## Expected results
PARENT_SINGLE_ID <- 5L
PARENT_MULTIPLE_ID <- c(5L, 84L, 55L)
GRANDPARENT_SINGLE_ID <- 19L
GRANDPARENT_MULTIPLE_ID <- c(19L, 85L, 81L)
GREATGRANDPARENT_SINGLE_ID <- 0L
GREATGRANDPARENT_MULTIPLE_ID <- c(0L, 0L, 84L)
CHILDREN_ID <- c(48L, 47L, 50L)
SIBLINGS_ID <- c(48L, 50L)

## Tests
stopifnot(exprs = {
    identical(PARENT_SINGLE_ID,
              roger:::getParseParent(parseData, SINGLE_ID))
    identical(PARENT_MULTIPLE_ID,
              roger:::getParseParent(parseData, MULTIPLE_ID))
    identical(GRANDPARENT_SINGLE_ID,
              roger:::getParseGParent(parseData, SINGLE_ID))
    identical(GRANDPARENT_MULTIPLE_ID,
              roger:::getParseGParent(parseData, MULTIPLE_ID))
    identical(GREATGRANDPARENT_SINGLE_ID,
              roger:::getParseGGParent(parseData, SINGLE_ID))
    identical(GREATGRANDPARENT_MULTIPLE_ID,
              roger:::getParseGGParent(parseData, MULTIPLE_ID))
    identical(CHILDREN_ID,
              roger:::getParseChildren(parseData, PARENT_ID))
    identical(SIBLINGS_ID,
              roger:::getParseSiblings(parseData, OP_ID))
})

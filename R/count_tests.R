### roger: Automated grading of R scripts
###
### Count the number of regression tests in a file.
###
### AUTHOR: Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

## Tests are identified as calls to 'stopifnot' at the top level of
## the parse tree. Therefore, calls in functions are ignored. Calls in
## a group are also currently omitted.
##
## The shape of the parse tree for usage of 'stopifnot' as
##
##   stopifnot(...)
##
## is as follows:
##
##   expr
##    |----\----\--------\--------\
##   expr  (   expr  [, expr...]  )
##    |
##   SYMBOL_FUNCTION_CALL
##    ("stopifnot")
##
## For usage as
##
##   stopifnot(exprs = { ... })
##
## the parse tree is as follows:
##
##   expr
##    |-----\--------\------------\--------\----\
##   expr   (    SYMBOL_SUB     EQ_SUB    expr  )
##    |           ("exprs")     ("=")      |----\--------\--------\
##   SYMBOL_FUNCTION_CALL                  {   expr    [expr...]  }
##    ("stopifnot")
##
## Usage as stopifnot(exprObject = ...) is not supported (because
## quite harder to parse reliably).

count_tests <- function(file, encoding = "UTF-8",
                        keep.source = getOption("keep.source"))
{
    ## Get parse information data frame from file
    x <- getParseData(parse(file, encoding = encoding,
                            keep.source = keep.source))

    ## Auxiliary function to count the number of "expr" tokens in an
    ## expression.
    countExprInExpr <- function(id)
        sum(x$parent == id & x$token == "expr")

    ## Initialize the number of tests.
    n <- 0L

    ## Get the ids of the expr containing the 'stopifnot' calls, and
    ## of their parents.
    testId <- x[x$token == "SYMBOL_FUNCTION_CALL" &
                x$text %in% c("stopifnot", "assertError", "assertWarning"),
                "parent"]
    testParent <- getParseParent(x, testId)

    ## Keep only calls that are at the top level.
    keep <- getParseParent(x, testParent) == 0
    testId <- testId[keep]
    testParent <- testParent[keep]

    ## Get the ids of 'exprs' symbols, if any, and of their parents.
    exprsId <- x[x$token == "SYMBOL_SUB" &
                 x$text == "exprs", "id"]
    exprsParent <- getParseParent(x, exprsId)

    ## Count the number of expressions in the 'stopifnot(...)' case.
    ## We need to subtract one to remove the expression containing the
    ## 'stopifnot' call itself.
    if (any(a <- match(testParent, exprsParent, 0L) == 0L))
        n <- n + sum(sapply(testParent[a], countExprInExpr) - 1L)

    ## Count the number of expressions in the 'stopifnot(exprs = ...)'
    ## case.
    if (any(a <- exprsParent %in% testParent))
    {
        ## For each test, find the sibling containing the expressions.
        exprId <- sapply(exprsParent[a],
                         function(id)
                             max(x[x$parent == id &
                                   x$token == "expr", "id"]))

        ## Count the number of expressions in 'exprs'.
        n <- n + sum(sapply(exprId, countExprInExpr))
    }

    n
}

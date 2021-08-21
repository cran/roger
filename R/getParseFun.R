### roger: Automated grading of R scripts
###
### Unexported auxiliary functions to extract function names and
### formal arguments defined in a parse information data frame.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

getParseFun <- function(parseData)
{
    ## The last three columns of a parse information data frame are
    ## the following for a function definition:
    ##
    ##        token terminal     text
    ##       SYMBOL     TRUE    <name>
    ##         expr    FALSE
    ##  LEFT_ASSIGN     TRUE       <-
    ##         expr    FALSE
    ##     FUNCTION     TRUE function
    ##
    ## Therefore, the name of a function is 4 rows higher than the
    ## 'FUNCTION' token in the data frame.
    UPROWS <- 4L

    ## Locate tokens corresponding to function definitions.
    w <- which(parseData$token == "FUNCTION")

    ## Keep only top-level functions, that is, those with a
    ## great-grand-parent of 0.
    w <- w[getParseGGParent(parseData, parseData$id[w]) == 0L]

    ## Extract function names.
    parseData$text[w - UPROWS]
}

getParseFormals <- function(parseData)
{
    ## Locate tokens corresponding to formal arguments.
    w <- which(parseData$token == "SYMBOL_FORMALS")

    ## Keep only top-level functions, that is, those with a
    ## great-grand-parent of 0.
    w <- w[getParseGGParent(parseData, parseData$id[w]) == 0L]

    ## Formal arguments of a function all share the same parent id.
    ## Therefore, we simply need to split the text strings by parent
    ## id to build the list of formals.
    x <- split(parseData$text[w], parseData$parent[w])
    names(x) <- NULL                    # drop names
    x
}

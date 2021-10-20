### roger: Automated grading of R scripts
###
### Validation of usage of c() with zero or one argument.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

unneeded_concatenation_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate calls to c().
    w <- which(parseData$token == "SYMBOL_FUNCTION_CALL" & parseData$text == "c")

    ## If c() is not used, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Get the number of expressions inside c() by counting the number
    ## of expressions in the parse information that share the same
    ## parent expression as the call to c(). Subtract 1 to exclude the
    ## call to c() itself.
    expr <- parseData$id[w - 1L]
    nexpr <- sapply(expr, function(id)
        sum(parseData$parent == id & parseData$token == "expr") - 1L)

    ## Check that c() is called with more than one argument.
    valid <- nexpr > 1L
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- parseData$line1[w][!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("do not use 'c()' with 0 or 1 argument"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg)
    }

    res
}

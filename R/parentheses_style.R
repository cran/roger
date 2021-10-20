### roger: Automated grading of R scripts
###
### Validation of spacing around parentheses.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

###
### Closing parenthesis: no space immediately before
###
close_parenthesis_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to a closing parenthesis.
    w <- which(parseData$token == "')'")

    ## If there are no closing parentheses, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Check that closing parentheses are not preceded by a space.
    valid <- parseData$col1[w] - parseData$col2[w - 1L] == 1L
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- parseData$line1[w][!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("do not use spaces before closing parentheses"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

###
### Opening parenthesis: no space immediately after
###
open_parenthesis_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to an opening parenthesis.
    w <- which(parseData$token == "'('")

    ## If there are no opening parentheses, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Check that opening parentheses are not followed by a space.
    valid <- parseData$col1[w + 1L] - parseData$col1[w] == 1L
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- parseData$line1[w][!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("do not use spaces after opening parentheses"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

###
### Left parenthesis: space immediately before a left parenthesis
### except in function calls or at the start of a sub-expression (e.g.
### two consecutive left parentheses)
###
left_parenthesis_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to a closing parenthesis.
    w <- which(parseData$token == "'('")

    ## If there are no left parentheses, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Determine if the parentheses are used in the definition of a
    ## function or in a function call. The tokens are one and two
    ## rows above in the data frame, respectively.
    funcall <-
        (parseData$token[w - 1L] == "FUNCTION") |
        (parseData$token[w - 2L] == "SYMBOL_FUNCTION_CALL")

    ## Check that there is no space before the parentheses in function
    ## calls.
    valid <- logical(length(w))
    wfc <- w[funcall]
    valid[funcall] <-
        (parseData$col1[wfc] - parseData$col2[wfc - 1L]) == 1L

    ## For all other contexts, first determine the number of rows to
    ## go up in the data frame to retrieve the value of 'col2'. It is
    ## one for all standard cases, but two for a sub-expression and
    ## for a "for" control structure. Adding 1 to the result of a
    ## logical operation yields the correct number of rows (1 or 2).
    wnfc <- w[!funcall]
    nrowup <-
        (parseData$token[wnfc - 1L] %in% c("expr", "forcond")) + 1L

    ## Now check that there is a space before those parentheses.
    ## Exceptions where no space is required: at the start of
    ## sub-expressions; after operators '^' and '/'; after a (most
    ## likely optional) left parenthesis '('.
    wup <- wnfc - nrowup
    valid[!funcall] <-
        (parseData$col1[wnfc] - parseData$col2[wup] > 1L) |
        (parseData$token[wup] %in% c("expr", "'^'", "'/'", "'('"))

    ## Wrap up.
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        i <- funcall[!valid]
        lines <- parseData$line1[w][!valid]
        msg <- character(length(lines))

        ## First, the messages for the errors in function calls.
        msg[i] <- sapply(lines[i], function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("do not use spaces before opening parentheses in function calls"),
                         appendLF = TRUE))

        ## Second, the messages for the errors in other contexts.
        i <- !i
        msg[i] <- sapply(lines[i], function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("use spaces before opening parentheses (except in function calls)"),
                         appendLF = TRUE))

        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

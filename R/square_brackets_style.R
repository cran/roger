### roger: Automated grading of R scripts
###
### Validation of spaces inside square brackets.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

###
### Closing bracket: no spaces immediately before
###
close_bracket_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to a closing bracket.
    w <- which(parseData$token == "']'")

    ## If there are no closing brackets, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Get the position (line, column) of every closing bracket.
    col <- parseData$col1[w]
    lines <- parseData$line1[w]

    ## Check that closing brackets are not preceded by a space, unless
    ## that space is after a comma
    pcol <- parseData$col2[w - 1L]
    pchar <- parseData$token[w - 1L]
    valid <- ((col - pcol) == 1L & pchar != "','") |
        ((col - pcol) > 1L & pchar == "','")
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- lines[!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("do not use spaces before closing brackets (except after a comma)"),
                     appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

###
### Opening bracket: no spaces immediately after
###
open_bracket_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to an opening bracket.
    w <- which(parseData$token == "'['")

    ## If there are no opening brackets, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Get the position (line, column) of every opening bracket.
    col <- parseData$col1[w]
    lines <- parseData$line1[w]

    # Check that opening brackets are not followed by a space
    valid <- 1L == (parseData$col1[w + 1L] - col)
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- lines[!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("do not use spaces after opening brackets"),
                     appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

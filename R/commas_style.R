### roger: Automated grading of R scripts
###
### Validation of spaces around commas: never one before and always
### one after, unless the comma ends the line.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

commas_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to a comma.
    w <- which(parseData$token == "','")

    ## If there are no commas, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Get the position (line and column) of every comma.
    lines <- parseData$line1[w]
    cols <- parseData$col1[w]

    ## First, check that there is no space before the comma.
    valid_before <- 1L == (cols - parseData$col2[w - 1L])

    ## Then, check that there is a space after the comma or that the
    ## comma ends the line.
    valid_after <- (parseData$col1[w + 1L] - cols) > 1L |
        (parseData$line1[w + 1L] - lines) >= 1L

    ## Combine results.
    valid <- valid_before & valid_after
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- lines[!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("never use a space before a comma, and always use one after\n"),
                         gettext("  (unless the comma ends its line)"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

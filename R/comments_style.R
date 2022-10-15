### roger: Automated grading of R scripts
###
### Validation of spaces after comment delimiters if the comment
### contains text.
###
### AUTHORS: Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

comments_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate comments in the parse tree.
    w <- which(parseData$token == "COMMENT")

    ## If there are no comments, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Valid comments start with one or more # symbol, possibly
    ## followed by any combination of punctuation signs (see ?regex);
    ## if the comment contains text (characters other than
    ## whitespace), it must be preceded by a space.
    valid <- grepl(r"(#+[[:punct:]]* *($| [^[:blank:]]))", parseData$text[w])
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- parseData$line1[w[!valid]]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("put a space after the comment delimiters"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

### roger: Automated grading of R scripts
###
### Validation of the use of '<-' instead of '=' for assignment.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

assignment_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Check that the token "EQ_ASSIGN" (representing '=' for
    ## assignment in the parse tree) is not used in the code.
    valid <- parseData$token != "EQ_ASSIGN"
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- parseData$line1[!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("use '<-' instead of '=' for assignment"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

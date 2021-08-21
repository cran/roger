### roger: Automated grading of R scripts
###
### Validation of line length for code and comments.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

line_length_style <- function(srcData, nchar_max = 80L)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Check line length in number of characters (or width).
    valid <- parseData$col2 <= nchar_max
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- parseData$line1[!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("limit line length to"), " ", nchar_max, " ",
                         gettext("characters."),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

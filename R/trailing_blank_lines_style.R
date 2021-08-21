### roger: Automated grading of R scripts
###
### Validation of superfluous blank lines at the end of script files.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

trailing_blank_lines_style <- function(srcData)
{
    ## Get source code from argument.
    Lines <- srcData$Lines

    ## Get the last non-blank line number
    last_non_blank_line <- tail(which(nzchar(Lines)), 1)

    ## Check that there are blank lines at the end of the source code.
    last_line <- length(Lines)
    res <- last_line == last_non_blank_line

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- (last_non_blank_line + 1L):last_line
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("tidy up and delete this superfluous blank line"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

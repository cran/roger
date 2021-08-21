### roger: Automated grading of R scripts
###
### Validation of unnecessary trailing whitespace.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

trailing_whitespace_style <- function(srcData)
{
    ## Get source code from argument.
    Lines <- srcData$Lines

    ## Check that lines do not contain trailing whitespace.
    valid <- !grepl(r"([[:blank:]]$)", Lines)
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- which(!valid)
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("tidy up and delete unnecessary trailing whitespace"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

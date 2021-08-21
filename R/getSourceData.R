### roger: Automated grading of R scripts
###
### Creation of a list containing the parse information data frame and
### the source code of an R script file.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

getSourceData <- function(file, encoding = "UTF-8",
                          keep.source = getOption("keep.source"))
{
    d <- getParseData(parse(file, encoding = encoding,
                            keep.source = keep.source))

    list(parseData = d, Lines = attr(d, "srcfile")$lines)
}


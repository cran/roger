\name{getSourceData}
\alias{getSourceData}
\title{Get Parse Information and Source Code}
\description{
  Get parse information and source code from an R script file.
}
\usage{
getSourceData(file, encoding, keep.source = getOption("keep.source"))
}
\arguments{
  \item{file}{a connection object or a character.}
  \item{encoding}{encoding to be assumed for input strings.}
  \item{keep.source}{a logical value; if \code{TRUE}, keep source
    reference information.}
}
\details{
  The parse information of the script file is obtained using
  \code{\link[utils]{getParseData}}. The source code is read in using
  \code{\link{readLines}}. Arguments \code{file}, \code{encoding} and
  \code{keep.source} should be compatible with these functions.

  Linters using results of this function may not work properly if the
  encoding used does not match the one of the script file.
}
\value{
  A list of two elements:
  \item{parseData}{parse information data frame;}
  \item{Lines}{source code character vector of length the number of
    lines in the file.}
}
\examples{
## Keep parse data in non interactive sessions.
if (!interactive())
    op <- options(keep.source = TRUE)

fil <- tempfile(fileext = ".R")
cat("## A simple function",
    "foo <- function(x, y)",
    "{",
    "    z <- x + y",
    "    if (z > 3)",
    "        (x * y)^2",
    "    else",
    "        sqrt(x * y)",
    "}",
    file = fil, sep = "\n")
getSourceData(fil)
}

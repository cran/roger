\name{line_length_style}
\alias{line_length_style}
\title{Validation of Length of Code and Comments Lines}
\description{
  Check that the length of code and comment lines does not  exceed a
  given limit in number of characters.
}
\usage{
line_length_style(srcData, nchar_max = 80L)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
  \item{nchar_max}{maximum number of characters per line.}
}
\details{
  Good coding practice dictates to limit line length to a value around
  80 characters for readability purposes.
  
  The line length is determined from the column number ending each line
  of code in the parse information. This may give unintended results if
  the encoding used in \code{\link{getParseData}} does not match the
  encoding of the script file and the latter contains non ASCII
  characters.
}
\value{
  Boolean. When \code{FALSE}, a \link{message} indicates the nature of
  the error and the faulty lines, and the returned value has the
  following \link{attributes}:
  \item{lines}{faulty line numbers;}
  \item{message}{text of the error message.}
}
\examples{
## Keep parse data in non interactive sessions.
if (!interactive())
    op <- options(keep.source = TRUE)

## Valid line length
fil <- tempfile(fileext = ".R")
cat("## innocuous comment",
    "x <- 2",
    "y <- 3",
    "x + y",
    file = fil, sep = "\n")
line_length_style(getSourceData(fil))

## Limit valid line length to only 20 characters
fil <- tempfile(fileext = ".R")
cat("## no longer innocuous comment",
    "x <- 2",
    "y <- 3",
    "x + y",
    file = fil, sep = "\n")
line_length_style(getSourceData(fil), 20)
}


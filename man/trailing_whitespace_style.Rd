\name{trailing_whitespace_style}
\alias{trailing_whitespace_style}
\title{Validation of Trailing Whitespace}
\description{
  Check that a script file does not contain unnecessary whitespace at
  the end of lines.
}
\usage{
trailing_whitespace_style(srcData)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
}
\details{
  Good coding practices dictate that a script file should contain
  unnecessary whitespace (space or tabulation) at the end of lines.
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

## Correct script without trailing whitespace
fil <- tempfile(fileext = ".R")
cat("## A simple function",
    "foo <- function(x, y)",
    "{",
    "    x + y",
    "}",
    file = fil, sep = "\n")
trailing_whitespace_style(getSourceData(fil))

## Incorrect script with trailing whitespace
fil <- tempfile(fileext = ".R")
cat("## A simple function",
    "foo <- function(x, y)",
    "{   ",
    "    x + y",
    "}\t",
    file = fil, sep = "\n")
trailing_whitespace_style(getSourceData(fil))
}

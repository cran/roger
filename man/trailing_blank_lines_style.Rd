\name{trailing_blank_lines_style}
\alias{trailing_blank_lines_style}
\title{Validation of Trailing Blank Lines}
\description{
  Check that a script file does not contain superfluous trailing blank
  lines.
}
\usage{
trailing_blank_lines_style(srcData)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
}
\details{
  Good coding practices dictate that a script file should not end with
  blank lines.
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

## Correct script without trailing blank lines
fil <- tempfile(fileext = ".R")
cat("## A simple function",
    "foo <- function(x, y)",
    "{",
    "    x + y",
    "}",
    file = fil, sep = "\n")
trailing_blank_lines_style(getSourceData(fil))

## Incorrect script with trailing blank lines
fil <- tempfile(fileext = ".R")
cat("## A simple function",
    "foo <- function(x, y)",
    "{",
    "    x + y",
    "}",
    "",
    "",
    file = fil, sep = "\n")
trailing_blank_lines_style(getSourceData(fil))
}

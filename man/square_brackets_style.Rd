\name{square_brackets_style}
\alias{square_brackets_style}
\alias{open_bracket_style}
\alias{close_bracket_style}
\title{Validation of Spacing Around Square Brackets}
\description{
  Check that spacing around square brackets is valid.
}
\usage{
open_bracket_style(srcData)
close_bracket_style(srcData)
}

\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
}
\details{
  Good coding practices dictate the correct spacing around square
  brackets: opening brackets should not be immediately followed by a
  space; closing brackets should not be immediately preceded by a
  space, unless that space is after a comma.
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

## Correct use of spacing around square brackets
fil <- tempfile(fileext = ".R")
cat("x <- c(1, 2, 3, 5, 7, 9)",
    "x[x > 3 & x < 7]",
    "dim(x) <- c(2, 3)",
    "x[1, ]",
    file = fil, sep = "\n")
srcData <- getSourceData(fil)
open_bracket_style(srcData)
close_bracket_style(srcData)

## Incorrect use of spacing around square brackets
fil <- tempfile(fileext = ".R")
cat("x <- c(1, 2, 3, 5, 7, 9)",
    "x[ x > 3 & x < 7 ]",
    "dim(x) <- c(2, 3)",
    "x[1,]",
    "x[1,  ]",
    file = fil, sep = "\n")
srcData <- getSourceData(fil)
open_bracket_style(srcData)
close_bracket_style(srcData)
}

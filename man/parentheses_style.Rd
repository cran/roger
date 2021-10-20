\name{parentheses_style}
\alias{parentheses_style}
\alias{open_parenthesis_style}
\alias{close_parenthesis_style}
\alias{left_parenthesis_style}
\title{Validation of Spacing Around Parentheses}
\description{
  Check that spacing around parentheses is valid.
}
\usage{
open_parenthesis_style(srcData)
close_parenthesis_style(srcData)
left_parenthesis_style(srcData)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
}
\details{
  Good coding practices dictate the correct spacing around parentheses.
  First, opening parentheses should not be immediately followed by a
  space. Second, closing parentheses should not be immediately preceded by a
  space. Third, left (or opening) parentheses should always be preceded
  by a space, except in function calls, at the start of
  sub-expressions, after operators \code{/} and \code{^}, or after an
  optional left parenthesis.
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

## Correct use of spacing around parentheses
fil <- tempfile(fileext = ".R")
cat("x <- c(2, 3, 5)",
    "if (any((2 * x) > 4))",
    "    sum(x)",
    "1/(x + 1)",
    "2^(x - 1)",
    "2^((x - 1))",
    file = fil, sep = "\n")
srcData <- getSourceData(fil)
open_parenthesis_style(srcData)
close_parenthesis_style(srcData)
left_parenthesis_style(srcData)

## Incorrect use of spacing around parentheses
fil <- tempfile(fileext = ".R")
cat("x <- c(2, 3, 5 )",
    "if(any(x > 4))",
    "    sum( x )",
    file = fil, sep = "\n")
srcData <- getSourceData(fil)
open_parenthesis_style(srcData)
close_parenthesis_style(srcData)
left_parenthesis_style(srcData)
}


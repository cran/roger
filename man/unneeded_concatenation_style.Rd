\name{unneeded_concatenation_style}
\alias{unneeded_concatenation_style}
\title{Validation of Concatenation Usage}
\description{
  Check that function \code{c} is used with more than one argument.
}
\usage{
unneeded_concatenation_style(srcData)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
}
\details{
  Function \code{\link{c}} is used to combine its arguments. Therefore,
  good coding practice dictates that the function should never be used
  with zero or one argument.

  Usage with zero argument to create an empty vector should be replaced
  by calls to object creation functions like \code{\link{numeric}} or
  \code{\link{character}}.

  Usage with one argument is a superfluous call to \code{\link{c}} that
  should just be replaced by the argument.
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

## Correct use of the 'c()' function
fil <- tempfile(fileext = ".R")
cat("x <- c(1, 2, 3, 4)", file = fil)
unneeded_concatenation_style(getSourceData(fil))

## Incorrect uses of the 'c()' function
fil <- tempfile(fileext = ".R")
cat("x <- c()",
    "x <- c(42)",
    file = fil, sep = "\n")
unneeded_concatenation_style(getSourceData(fil))
}

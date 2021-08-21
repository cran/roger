\name{nomagic_style}
\alias{nomagic_style}
\title{Validation of Magic Numbers}
\description{
  Check the absence of \emph{magic numbers} in code.
}
\usage{
nomagic_style(srcData, ignore = c(-1, 0, 1, 2, 100),
              ignore.also = NULL)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
  \item{ignore}{vector of numbers not considered as magic.}
  \item{ignore.also}{vector of numbers not considered as magic in
    addition to those in \code{ignore}.}
}
\details{

  Good coding practices dictate to avoid using \dQuote{magic numbers}
  (unnamed or insufficiently documented numerical constants) in code.
  For example, in the expression \code{y <- x + 42}, \code{42} is a
  magic number.

  When scanning the code for magic numbers, the following numerical
  constants are ignored:
  \itemize{
    \item{numbers in vectors \code{ignore} and \code{ignore.also}, with or
      without a suffix \code{L} denoting integer values;}
    \item{numbers used as the only expression in indexing;}
    \item{numbers in \dQuote{simple} assignments to variables with all
      uppercase names.}
  }
  
  \dQuote{Simple} assignments serve to assign magic numbers to objects.
  Such expressions should contain at most three levels of
  sub-expressions and hold on a single line of code. The following
  expressions contain one, two and three levels of sub-expressions,
  respectively: \code{MAX <- 4294967295}, \code{MAX <- 4294967296 - 1},
  \code{MAX <- 2^32 - 1}.
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

fil <- tempfile(fileext = ".R")
cat("MAX <- 2^6 - 1",
    "size <- 42",
    "x <- rnorm(MAX)",
    "runif(123)",
    "x[1]",
    "x[1] * 7 + 2",
    "x[33]",
    "x * 100",
    "x <- numeric(0)",
    "y <- logical(5)",
    file = fil, sep = "\n")

## Default list of ignored magic numbers
nomagic_style(getSourceData(fil))

## Additional exceptions
nomagic_style(getSourceData(fil), ignore.also = c(5, 42))
}


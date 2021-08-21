\name{commas_style}
\alias{commas_style}
\title{Validation of Spacing Around Commas}
\description{
  Check that commas are never preceded by a space and always followed by
  one, unless the comma ends the line.
}
\usage{
commas_style(srcData)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
}
\details{
  Good coding practices dictate to follow commas by a space, unless it
  ends its line, and to never put a space before.
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

## Correct use of spacing around a comma
fil <- tempfile(fileext = ".R")
cat("x <- c(2, 3, 5)", "crossprod(2,", "x)",
    file = fil, sep = "\n")
commas_style(getSourceData(fil))

## Incorrect use of spacing around a comma
fil <- tempfile(fileext = ".R")
cat("x <- c(2,3, 5)", "crossprod(2 ,", "x)",
    file = fil, sep = "\n")
commas_style(getSourceData(fil))
}


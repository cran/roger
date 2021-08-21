\name{assignment_style}
\alias{assignment_style}
\title{Validation of the Assignment Operator}
\description{
  Check that the left assign symbol (\code{<-}) is used to assign values
  to objects instead of the equation assign symbol (\code{=}).
}
\usage{
assignment_style(srcData)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
}
\details{
  Although \code{\link[=assignOps]{=}} is a proper assignment symbol at
  the top-level, its use is discouraged in R. Good coding practices
  dictate to rather use \code{<-} for (left) assignment to objects.
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

## Correct coding practice
fil <- tempfile(fileext = ".R")
cat("x <- 2", file = fil, sep = "\n")
assignment_style(getSourceData(fil))

## Incorrect coding practice
cat("x = 2", file = fil, sep = "\n")
assignment_style(getSourceData(fil))
}

\name{comments_style}
\alias{comments_style}
\title{Validation of Comments}
\description{
  Check that comment delimiters and the text of comments, when there is
  any, are separated by at least one space.
}
\usage{
comments_style(srcData)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
}
\details{
  Good coding practices dictate to follow the comment delimiters by at
  least one space when a comment contains text.
  
  The accepted delimiters are: one or more \code{\#} symbols possibly
  followed immediately by any combination of punctuation characters of the
  \code{[:punct:]} class (see \code{\link{regex}}).  
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

## Correct comments
fil <- tempfile(fileext = ".R")
cat("### foo",
    "##-- bar",
    "##  ",
    "#",
    "#***",
    file = fil, sep = "\n")
comments_style(getSourceData(fil))

## Incorrect comments
fil <- tempfile(fileext = ".R")
cat("###foo",
    "##++bar",
    file = fil, sep = "\n")
comments_style(getSourceData(fil))
}


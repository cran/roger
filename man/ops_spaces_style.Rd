\name{ops_spaces_style}
\alias{ops_spaces_style}
\title{Validation of Spacing Around Operators}
\description{
  Check that spacing around infix and unary operators is valid.
}
\usage{
ops_spaces_style(srcData, ops = ops_list)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
  \item{ops}{vector of R parser tokens corresponding to infix operators to
    check for correct spacing; see details.}
}
\details{
  Good coding practices dictate to surround infix operators with spaces
  (including line breaks) and to follow unary operators immediately by
  their argument.

  The default value for argument \code{ops} is an internal object
  containing the following R parser tokens: \code{'+'}, \code{'-'},
  \code{'*'}, \code{GT}, \code{GE}, \code{LT}, \code{LE},
  \code{EQ}, \code{NE}, \code{AND}, \code{OR}, \code{AND2},
  \code{OR2}, \code{'!'}, \code{LEFT_ASSIGN}, \code{RIGHT_ASSIGN},
  \code{EQ_ASSIGN}, \code{EQ_SUB}, \code{SPECIAL}.
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

## Correct use of spacing around '+' and '-' operators
fil <- tempfile(fileext = ".R")
cat("2 + 3",
    "-2",
    "(4\n + 2)",
    "4 +\n2",
    file = fil, sep = "\n")
ops_spaces_style(getSourceData(fil), c("'+'", "'-'"))

## Incorrect use of spacing around '>' and '!' operators
fil <- tempfile(fileext = ".R")
cat("2> 3",
    "4 >2",
    "6>3",
    "! FALSE",
    "!\nFALSE",
    file = fil, sep = "\n")
ops_spaces_style(getSourceData(fil), c("GT", "'!'"))
}


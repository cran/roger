\name{count_tests}
\alias{count_tests}
\title{Number of Regression Tests in Source Code}
\description{
  Count the number of calls to \code{stopifnot} at the top level of an
  R script file.
}
\usage{
count_tests(file, encoding, keep.source = getOption("keep.source"))
}
\arguments{
  \item{file}{a connection object or a character.}
  \item{encoding}{encoding to be assumed for input strings.}
  \item{keep.source}{a logical value; if \code{TRUE}, keep source
    reference information.}
}
\details{
  A test is an R expression included in either of these forms of
  \code{\link{stopifnot}} usage: \code{stopifnot(A, B)}, or
  \code{stopifnot(exprs= {A ; B})}. Usage as \code{stopifnot(exprObject
    = Obj)} is not supported.

  Calls to \code{stopifnot} in functions are ignored. Calls inside a
  group are also currently omitted.
}
\value{
  A number of tests.
}
\examples{
## Keep parse data in non interactive sessions.
if (!interactive())
    op <- options(keep.source = TRUE)

fil <- tempfile(fileext = ".R")
cat("stopifnot(1 == 1)",
    "stopifnot(2 == 2, 3 == 3)",
    "stopifnot(exprs = {",
    "    4 == 4",
    "    5 == 5",
    "    6 == 6",
    "})",
    "## 'stopifnot' in a function is ignored",
    "f <- function() stopifnot(6 == 6)",
    file = fil, sep = "\n")
count_tests(fil)
}

\name{documentation_linters}
\alias{documentation_linters}
\alias{any_doc}
\alias{signature_doc}
\alias{section_doc}
\alias{description_section_doc}
\alias{arguments_section_doc}
\alias{value_section_doc}
\alias{examples_section_doc}
\alias{formals_doc}
\title{Validation of Documentation}
\description{
  Check for proper documentation of a function in the comments of a
  script file, and if certain mandatory sections are present.

  The expected documentation format is not unlike R help pages; see
  details.
}
\usage{
any_doc(srcData, ...)

signature_doc(srcData, ...)

section_doc(srcData, pattern, ...)
description_section_doc(srcData, ...)
arguments_section_doc(srcData, ...)
value_section_doc(srcData, ...)
examples_section_doc(srcData, ...)

formals_doc(srcData, ...)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
  \item{pattern}{character string containing a regular expression
    describing a keyword to match in the documentation.}
  \item{\dots}{further arguments passed to \code{\link[base]{grepl}}.}
}
\details{
  These functions check the documentation provided with function
  definitions in a script file. Lines starting with at least one comment
  character \code{#} (in column 1) are considered documentation.

  \code{any_doc} checks that the file contains at least some
  documentation.

  \code{signature_doc} checks that the signature (or usage information)
  of every function is present in the documentation.

  \code{section_doc} checks that the documentation contains a section
  title corresponding to \code{pattern} for every (or as many) function
  definition. Functions \code{description_section_doc},
  \code{arguments_section_doc}, \code{value_section_doc} and
  \code{examples_section_doc} are wrappers for common patterns
  \code{Description}, \code{Arguments?}, \code{Value} and
  \code{Examples?}, respectively.

  \code{formals_doc} checks that the description of every formal
  argument is present in the documentation.
}
\value{
  Boolean. When \code{FALSE}, a \link{message} indicates the nature of
  the error, and the returned value has the following \link{attributes}:
  \item{nlines}{number of lines checked for documentation
    (\code{any_doc} only);}
  \item{fun}{names of functions without a signature in the
    documentation (\code{signature_doc} only);}
  \item{sections}{number of sections missing from in the documentation
    (\code{section_doc} and wrapper functions only);}
  \item{formals}{formal arguments missing from the documentation
    (\code{formals_doc} only);}
  \item{message}{text of the error message.}

}
\references{
  Goulet, V.,
  \href{https://vigou3.gitlab.io/programmer-avec-r/}{Programmer avec  R}
  for the standard documentation format these functions were
  developed for.
}
\examples{
## Keep parse data in non interactive sessions.
if (!interactive())
    op <- options(keep.source = TRUE)

## Define script file with embedded documentation.
fil <- tempfile(fileext = ".R")
cat(file = fil, "
###
### foo(x, y = 2)
###
##  Adding two vectors
##
##  Arguments
##
##  x: a vector
##  y: another vector
##
##  Value
##
##  Sum of the two vectors.
##
##  Examples
##
##  foo(1:5)
##
foo <- function(x, y = 2)
    x + y
")
fooData <- getSourceData(fil)

## Elements present in the documentation
any_doc(fooData)
arguments_section_doc(fooData)
value_section_doc(fooData)
examples_section_doc(fooData)
formals_doc(fooData)

## Missing section title
description_section_doc(fooData)
}
\keyword{programming}
\keyword{documentation}

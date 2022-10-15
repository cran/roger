\name{documentation_linters}
\alias{documentation_linters}
\alias{any_comments}
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
  Check for comments and proper documentation of a function in a script
  file.

  The expected documentation format is not unlike R help pages; see
  details.
}
\usage{
any_comments(srcData, ...)

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
  \code{any_comments} checks that a script file contains at least one
  non empty comment, that is, a string with at least one character other
  than whitespace after the comment delimiters.

  The other functions check the documentation provided with function
  definitions in a script file. Comments starting in column 1 are
  considered documentation.

  \code{any_doc} checks that the file contains some documentation.

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
  \item{nlines}{number of lines checked for comments or documentation
    (\code{any_comments} and \code{any_doc} only);}
  \item{fun}{names of functions without a signature in the
    documentation (\code{signature_doc} only);}
  \item{sections}{number of sections missing from documentation
    (\code{section_doc} and wrapper functions only);}
  \item{formals}{formal arguments missing from documentation
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

## Script with non empty comments.
fil <- tempfile(fileext = ".R")
cat(file = fil, "
#-
## here is foo
#-
foo <- function(x, y = 2)
    x + y  #addition
")
any_comments(getSourceData(fil))

## Script with embedded documentation.
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
any_comments(fooData)
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

\name{curly_braces_style}
\alias{curly_braces_style}
\alias{close_brace_style}
\alias{open_brace_style}
\alias{open_brace_unique_style}
\title{Validation of the Positioning of Braces}
\description{
  Check that the opening and closing braces are positioned according to
  standard bracing styles rules.
}
\usage{
close_brace_style(srcData)
open_brace_style(srcData, style = c("R", "1TBS"))
open_brace_unique_style(srcData)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
  \item{style}{character string of a supported bracing style.}
}
\details{
  Good coding practices dictate to use one bracing style uniformly in a
  script.

  The "R" bracing style has both opening and closing braces on their own
  lines, left aligned with their corresponding statement:
  \preformatted{
    if (x > 0)
    {
        ...
    }
  }

  The "1TBS" bracing style, also known at "K&R" style, has the opening
  brace immediately follow its corresponding statement, separated by a
  space. The closing brace lies on its own line, left aligned with the
  statement:
  \preformatted{
    if (x > 0) {
        ...
    }
  }

  \code{open_brace_style} validates that the coding style in argument
  is used for opening braces in a script file.
  \code{open_brace_unique_style} validates that only one style is used
  throughout the script.

  These functions use \code{\link{getParseText}} and, therefore, require
  that the \code{"keep.source"} option is \code{TRUE}.
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

## Correct positioning of braces in R bracing style
fil <- tempfile(fileext = ".R")
cat("x <- 2",
    "if (x <= 2)",
    "{",
    "    y <- 3",
    "    x + y",
    "}",
    file = fil, sep = "\n")
srcData <- getSourceData(fil)
open_brace_style(srcData, style = "R")
close_brace_style(srcData)

## Above code in invalid in 1TBS bracing style
open_brace_style(srcData, style = "1TBS")

## Incorrect positioning of the opening brace and
## misalignment of the closing brace
fil <- tempfile(fileext = ".R")
cat("f <- function(x) {",
    "    x^2",
    "  }",
    file = fil, sep = "\n")
srcData <- getSourceData(fil)
open_brace_style(srcData, style = "R")
close_brace_style(srcData)

## Incorrect simultaneous use of two bracing styles
fil <- tempfile(fileext = ".R")
cat("x <- 2",
    "if (x <= 2)",
    "{",
    "    y <- 3",
    "    x + y",
    "}",
    "for (i in 1:5) {",
    "    x + i",
    "}",
    file = fil, sep = "\n")
open_brace_unique_style(getSourceData(fil))
}

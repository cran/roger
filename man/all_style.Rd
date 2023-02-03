\name{all_style}
\alias{all_style}
\title{Validation Using All Style Linters}
\description{
  Check the style of code using all available linters.
}
\usage{
all_style(srcData, include, exclude, \dots)
}
\arguments{
  \item{srcData}{a list as returned by \code{\link{getSourceData}}.}
  \item{include, exclude}{character vector of linters to use or to omit,
    respectively. If both arguments are unspecified or \code{NULL}, all
    available linters are used.}
  \item{\dots}{further arguments passed to linters when appropriately
    named.}
}
\details{
  The complete list of available style linters is as follows:
  \itemize{
    \item{assignment_style}
    \item{close_brace_style}
    \item{close_bracket_style}
    \item{close_parenthesis_style}
    \item{commas_style}
    \item{comments_style}
    \item{left_parenthesis_style}
    \item{line_length_style}
    \item{nomagic_style}
    \item{open_brace_style}
    \item{open_brace_unique_style}
    \item{open_bracket_style}
    \item{open_parenthesis_style}
    \item{ops_spaces_style}
    \item{trailing_blank_lines_style}
    \item{trailing_whitespace_style}
    \item{unneeded_concatenation_style}
  }
}
\value{
  A named list with the results of the linters.
}
\examples{
## Keep parse data in non interactive sessions.
if (!interactive())
    op <- options(keep.source = TRUE)

fil <- tempfile(fileext = ".R")
cat("x <- c(2, 3, 5)", "crossprod(2,", "x)",
    "a <- array(1:24, 2:4)", "a[1, , ]",
    file = fil, sep = "\n")

all_style(getSourceData(fil))
all_style(getSourceData(fil), include = c("commas", "nomagic"))
all_style(getSourceData(fil), include = c("commas", "nomagic"),
          ignore.also = 5)
all_style(getSourceData(fil), exclude = "nomagic")
}


### roger: Automated grading of R scripts
###
### Tests for the the validity of the style linters.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later.

library(roger)

## Tests are run by R CMD check in a non interactive session where
## parse data is thrown out by default.
if (!interactive())
    options(keep.source = TRUE)

###
### Tests for code free of style errors
###

## General expressions
VALID_STYLE_FILE <- tempfile(fileext = ".R")
cat(
"## Correct use of left assign token",
"x <- 2",
"y <- 3",
"z <- c(42, 43)",
"",
"## Correct use of spacing",
## Operators
"x + y",
"x - y",
"x > y",
"x >= y",
"x < y",
"x <= y",
"x == y",
"x != y",
"x & y",
"x | y",
"x && y",
"x || y",
"c(42, 43) -> z",
"x %% y",
"x %*% y",
"x * y",
"x/y", # this one does not need surrounding spaces
"x^y", # this one does not need surrounding spaces
"x +",
"y",
"z[1",
"< 2]",
"x <- -2",
"2 + !TRUE",
"",
## Parentheses
"sum(x)",
"1 + (x + y)",
"1 + ((x + y) * z)",
"1 + (z/(x + y))",
"1 + (z^(x + y))",
"1 + (sum(1:10))",
"1 + ((sum(1:10)))",
"if (x > 0) x + y",
"for (i in seq_along(z)) x[i] <- x[i] + 2",
"while (x < 2) x + 2",
"switch(2, 2 + 2, c(sum(x), diff(z)))",
"foo <- function(x) x^2",
"",
## Square brackets
"z[1]",
"U <- matrix(1:4, ncol = 2)",
"U[2, ]",
"",
## Commas
"paste('Hello',",
"    ' World')",
"## There should be no whitespaces at the end of a line",
"",
"## There should be no trailing blank lines in a file",
file = VALID_STYLE_FILE, sep = "\n")
VALID_STYLE <- getSourceData(VALID_STYLE_FILE)

## Magic numbers
VALID_MAGIC_FILE <- tempfile(fileext = ".R")
cat(
"## Correct assignment of magic numbers",
"SIZE <- 42",
"BAR <- 2^32",
"BAZ <- 2^32 - 1",
"",
"## Allowed use of some numbers",
"x <- rnorm(SIZE)",
"x[1]",
"x[1] * 2",
"x[33]",
"x[-1] - x[-SIZE] == -1L",
"x * 100",
"for (i in 1:SIZE) x[1]",
"x <- numeric(0)",
file = VALID_MAGIC_FILE, sep = "\n")
VALID_MAGIC <- getSourceData(VALID_MAGIC_FILE)

## Magic numbers with additional exceptions
VALID_MAGIC_IGNORE_FILE <- tempfile(fileext = ".R")
file.copy(VALID_MAGIC_FILE, VALID_MAGIC_IGNORE_FILE)
cat(
"x <- numeric(32)",
"length(x) > 5L",
file = VALID_MAGIC_IGNORE_FILE, sep = "\n")
VALID_MAGIC_IGNORE <- getSourceData(VALID_MAGIC_IGNORE_FILE)

## R bracing style
VALID_BRACE_R_FILE <- tempfile(fileext = ".R")
cat(
"## Correct use of R style for braces",
"foo <- function(x, y)",
"{",
"    if (x > 2)",
"    {",
"        x^2 + y^3",
"    }",
"    else",
"    {",
"        z <- 3",
"        x^2 + y^3 + z^4",
"    }",
"",
"    {",
"        2 + 3",
"    }",
"}",
"",
"function(x, y)",
"{",
"    2 + 3",
"}",
"",
"{",
"    2 + 3",
"}",
file = VALID_BRACE_R_FILE, sep = "\n")
VALID_BRACE_R <- getSourceData(VALID_BRACE_R_FILE)

## 1TBS bracing style
VALID_BRACE_1TBS_FILE <- tempfile(fileext = ".R")
cat(
"## Correct use of 1TBS style for braces",
"bar <- function(x, y = 42) {",
"    if (x < y) {",
"        x + y",
"    } else {",
"        x - y",
"    }",
"}",
"",
"function(x, y) {",
"    2 + 3",
"}",
file = VALID_BRACE_1TBS_FILE, sep = "\n")
VALID_BRACE_1TBS <- getSourceData(VALID_BRACE_1TBS_FILE)

## Tests for valid style
stopifnot(exprs = {
    assignment_style(VALID_STYLE)
    commas_style(VALID_STYLE)
    close_brace_style(VALID_BRACE_R)
    close_brace_style(VALID_BRACE_1TBS)
    open_brace_style(VALID_BRACE_R, "R")
    open_brace_style(VALID_BRACE_1TBS, "1TBS")
    open_brace_unique_style(VALID_BRACE_R)
    open_brace_unique_style(VALID_BRACE_1TBS)
    line_length_style(VALID_STYLE)
    nomagic_style(VALID_MAGIC)
    nomagic_style(VALID_MAGIC_IGNORE, ignore.also = c(32, 5L))
    ops_spaces_style(VALID_STYLE)
    open_parenthesis_style(VALID_STYLE)
    close_parenthesis_style(VALID_STYLE)
    left_parenthesis_style(VALID_STYLE)
    open_bracket_style(VALID_STYLE)
    close_bracket_style(VALID_STYLE)
    trailing_blank_lines_style(VALID_STYLE)
    trailing_whitespace_style(VALID_STYLE)
    unneeded_concatenation_style(VALID_STYLE)
})


###
### Tests for expressions containing style errors
###

## General expressions
INVALID_STYLE_FILE <- tempfile(fileext = ".R")
cat(
"## Invalid assignment",
"x = 2",
"y = 3",
"z = c(42, 43)",
"",
"## Invalid use of spacing (no spacing)",
## Operators
"x+y",
"x-y",
"x>y",
"x>=y",
"x<y",
"x<=y",
"x==y",
"x!=y",
"x&y",
"x|y",
"x&&y",
"x||y",
"c(42, 43)->z",
"x%%y",
"x%*%y",
"x*y",
"x<- -2",
"2+!TRUE",
## Parentheses
"sum( x)",
"sum(x )",
"sum (x)",
"1 +(x + y)",
"if(x < 2) x",
"for(i in seq_along(z)) x[i] <- x[i] + 2",
"while(x < 2) x + 2",
"switch (2, 2 + 2, 2 - 2)",
"foo <- function (x) x",
## Square brackets
"z[ 1]",
"z[1 ]",
"U <- matrix(1:4, ncol = 2)",
"U[2,]",
## Commas
"z <- c(42,43)",
"z <- c(42 ,43)",
"",
## Line length
"## Lines should be less than 80 characters ########################################",
"",
## Trailing whitespaces
"## Here is an invalid line with trailing whitespaces   ",
"## And one ending with a tabulation\t",
"",
## Unneeded concatenation
"a <- c(42)",
"b <- c(sum(z))",
"d <- c()",
"",
## Trailing blank lines
"",
"",
file = INVALID_STYLE_FILE, sep = "\n")
INVALID_STYLE <- getSourceData(INVALID_STYLE_FILE)

## Magic numbers
INVALID_MAGIC_FILE <- tempfile(fileext = ".R")
cat(
"## Incorrect assignment of magic numbers",
"size <- 42",
"fooBar <- 2^32",
"BAZ <- 4 * (2^32 - 1)",
"",
"## Magic numbers used",
"runif(123)",
"x <- rnorm(size)",
"x[3] * 7 + 2",                         # only 7 is magic
"x <- 3 * runif(1) - 12",               # 3 and 12 are magic
"x <- numeric(5)",
file = INVALID_MAGIC_FILE, sep = "\n")
INVALID_MAGIC <- getSourceData(INVALID_MAGIC_FILE)

## R bracing style
INVALID_BRACE_R_FILE <- tempfile(fileext = ".R")
cat(
"## Wrong use of R style for braces",
"foo <- function(x, y){",
"    if(x > 2)",
"        x^2 + y^3",
"    else",
"  {",
"        z <- 3",
"        x^2 + y^3 + z^4}",
"  }",
"",
"bar <- function(x)",
"{    x^2",
"}",
"",
"function(x, y)",
"  {",
"    2 + 3 }",
"",
"{",
"    2 + 3",
"  }",
file = INVALID_BRACE_R_FILE, sep = "\n")
INVALID_BRACE_R <- getSourceData(INVALID_BRACE_R_FILE)

## 1TBS bracing style
INVALID_BRACE_1TBS_FILE <- tempfile(fileext = ".R")
cat(
"## Wrong use of 1TBS style for braces",
"bar <- function(x, y = 42){",
"",
"    if (x < y)",
"    {",
"        x + y",
"        }",
"    else {",
"        x - y",
"    }}",
file = INVALID_BRACE_1TBS_FILE, sep = "\n")
INVALID_BRACE_1TBS <- getSourceData(INVALID_BRACE_1TBS_FILE)

## Target line numbers of errors
INVALID_EQ_ASSIGN_LINES <- c(2L, 3L, 4L)
INVALID_COMMAS_LINES <- c(37L, 38L, 39L)
INVALID_CLOSE_BRACE_R_LINES <- c(8L, 9L, 17L, 21L)
INVALID_OPEN_BRACE_R_LINES <- c(2L, 6L, 12L, 16L)
INVALID_CLOSE_BRACE_1TBS_LINES <- c(7L, 10L, 10L)
INVALID_OPEN_BRACE_1TBS_LINES <- c(2L, 5L)
INVALID_OPEN_BRACE_UNIQUE_LINES <- list(R = c(2L, 6L, 12L, 16L, 19L),
                                        ONETBS = c(2L, 5L, 8L))
INVALID_LINE_LENGTH_LINES <- 41L
INVALID_MAGIC_LINES <- c(2L, 3L, 4L, 7L, 9L, 10L, 10L, 11L)
INVALID_MAGIC_IGNORE_LINES <- c(3L, 4L, 7L, 9L, 10L, 10L)
INVALID_OPS_SPACING_LINES <- c(7:24, 28L)
INVALID_OPEN_PARENTHESIS_LINES <- 25L
INVALID_CLOSE_PARENTHESIS_LINES <- 26L
INVALID_LEFT_PARENTHESIS_LINES <- 27:33
INVALID_OPEN_BRACKET_LINES <- 34L
INVALID_CLOSE_BRACKET_LINES <- c(35L, 37L)
INVALID_TRAILING_BLANK_LINES_LINES <- c(49L, 50L, 51L)
INVALID_TRAILING_WHITESPACE_LINES <- c(43L, 44L)
INVALID_USE_OF_CONCATENATION_LINES <- c(46L, 47L, 48L)

## Results for invalid style
res.assignment_style <- suppressMessages(assignment_style(INVALID_STYLE))
res.commas_style <- suppressMessages(commas_style(INVALID_STYLE))
res.close_brace_style_R <- suppressMessages(close_brace_style(INVALID_BRACE_R))
res.open_brace_style_R <- suppressMessages(open_brace_style(INVALID_BRACE_R, "R"))
res.close_brace_style_1TBS <- suppressMessages(close_brace_style(INVALID_BRACE_1TBS))
res.open_brace_style_1TBS <- suppressMessages(open_brace_style(INVALID_BRACE_1TBS, "1TBS"))
res.open_brace_unique_style <- list(R = suppressMessages(open_brace_unique_style(INVALID_BRACE_R)),
                                    ONETBS = suppressMessages(open_brace_unique_style(INVALID_BRACE_1TBS)))
res.line_length_style <- suppressMessages(line_length_style(INVALID_STYLE))
res.nomagic_style <- suppressMessages(nomagic_style(INVALID_MAGIC))
res.nomagic_ignore_style <- suppressMessages(nomagic_style(INVALID_MAGIC, ignore.also = c(42, 5)))
res.ops_spaces_style <- suppressMessages(ops_spaces_style(INVALID_STYLE))
res.open_parenthesis_style <- suppressMessages(open_parenthesis_style(INVALID_STYLE))
res.close_parenthesis_style <- suppressMessages(close_parenthesis_style(INVALID_STYLE))
res.left_parenthesis_style <- suppressMessages(
    left_parenthesis_style(INVALID_STYLE)
)
res.open_bracket_style <- suppressMessages(open_bracket_style(INVALID_STYLE))
res.close_bracket_style <- suppressMessages(close_bracket_style(INVALID_STYLE))
res.trailing_blank_lines_style <- suppressMessages(trailing_blank_lines_style(INVALID_STYLE))
res.trailing_whitespace_style <- suppressMessages(trailing_whitespace_style(INVALID_STYLE))
res.unneeded_concatanation_style <- suppressMessages(unneeded_concatenation_style(INVALID_STYLE))

## Tests for invalid style
stopifnot(exprs = {
    isFALSE(res.assignment_style)
    isFALSE(res.commas_style)
    isFALSE(res.close_brace_style_R)
    isFALSE(res.open_brace_style_R)
    isFALSE(res.close_brace_style_1TBS)
    isFALSE(res.open_brace_style_1TBS)
    isFALSE(res.open_brace_unique_style$R)
    isFALSE(res.open_brace_unique_style$ONETBS)
    isFALSE(res.line_length_style)
    isFALSE(res.nomagic_style)
    isFALSE(res.nomagic_ignore_style)
    isFALSE(res.ops_spaces_style)
    isFALSE(res.open_parenthesis_style)
    isFALSE(res.close_parenthesis_style)
    isFALSE(res.left_parenthesis_style)
    isFALSE(res.open_bracket_style)
    isFALSE(res.close_bracket_style)
    isFALSE(res.trailing_blank_lines_style)
    isFALSE(res.trailing_whitespace_style)
    isFALSE(res.unneeded_concatanation_style)
})

## Tests for the line numbers of errors
stopifnot(exprs = {
    identical(INVALID_EQ_ASSIGN_LINES,
              attr(res.assignment_style, "line"))
    identical(INVALID_COMMAS_LINES,
              attr(res.commas_style, "lines"))
    identical(INVALID_CLOSE_BRACE_R_LINES,
              attr(res.close_brace_style_R, "lines"))
    identical(INVALID_OPEN_BRACE_R_LINES,
              attr(res.open_brace_style_R, "lines"))
    identical(INVALID_CLOSE_BRACE_1TBS_LINES,
              attr(res.close_brace_style_1TBS, "lines"))
    identical(INVALID_OPEN_BRACE_1TBS_LINES,
              attr(res.open_brace_style_1TBS, "lines"))
    identical(INVALID_OPEN_BRACE_UNIQUE_LINES$R,
              attr(res.open_brace_unique_style$R, "lines"))
    identical(INVALID_OPEN_BRACE_UNIQUE_LINES$ONETBS,
              attr(res.open_brace_unique_style$ONETBS, "lines"))
    identical(INVALID_LINE_LENGTH_LINES,
              attr(res.line_length_style, "lines"))
    identical(INVALID_MAGIC_LINES,
              attr(res.nomagic_style, "lines"))
    identical(INVALID_MAGIC_IGNORE_LINES,
              attr(res.nomagic_ignore_style, "lines"))
    identical(INVALID_OPS_SPACING_LINES,
              attr(res.ops_spaces_style, "lines"))
    identical(INVALID_OPEN_PARENTHESIS_LINES,
              attr(res.open_parenthesis_style, "lines"))
    identical(INVALID_CLOSE_PARENTHESIS_LINES,
              attr(res.close_parenthesis_style, "lines"))
    identical(INVALID_LEFT_PARENTHESIS_LINES,
              attr(res.left_parenthesis_style, "lines"))
    identical(INVALID_OPEN_BRACKET_LINES,
              attr(res.open_bracket_style, "lines"))
    identical(INVALID_CLOSE_BRACKET_LINES,
              attr(res.close_bracket_style, "lines"))
    identical(INVALID_TRAILING_BLANK_LINES_LINES,
              attr(res.trailing_blank_lines_style, "lines"))
    identical(INVALID_TRAILING_WHITESPACE_LINES,
              attr(res.trailing_whitespace_style, "lines"))
    identical(INVALID_USE_OF_CONCATENATION_LINES,
              attr(res.unneeded_concatanation_style, "lines"))
})

###
### Tests for "empty" files (no code to analyse)
###
EMPTY_FILE <- tempfile(fileext = ".R")
cat("## Hello World", file = EMPTY_FILE)
EMPTY <- getSourceData(EMPTY_FILE)

stopifnot(exprs = {
    assignment_style(EMPTY)
    commas_style(EMPTY)
    close_brace_style(EMPTY)
    open_brace_style(EMPTY, "R")
    open_brace_style(EMPTY, "1TBS")
    open_brace_unique_style(EMPTY)
    line_length_style(EMPTY)
    ops_spaces_style(EMPTY)
    open_parenthesis_style(EMPTY)
    close_parenthesis_style(EMPTY)
    left_parenthesis_style(EMPTY)
    open_bracket_style(EMPTY)
    close_bracket_style(EMPTY)
    trailing_blank_lines_style(EMPTY)
    trailing_whitespace_style(EMPTY)
    unneeded_concatenation_style(EMPTY)
})

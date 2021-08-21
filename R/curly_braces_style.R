### roger: Automated grading of R scripts
###
### Validation of the positioning of braces. Two styles are allowed
### for opening braces: R/C++ style with the brace on its own line;
### 1TBS (a.k.a. K&R) with the brace at end of the statement. In
### either style, the closing brace should on its own line.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

###
### Unexported auxiliary functions
###

## Get the id of the statement corresponding to an expression block.
##
## This is usually the id of the parent expression.
##
## However, for an expression block without any corresponding
## statement, this is only the id of the block itself.
getParseStatementId <- function(parseData, id)
{
    ## Get the id of the parent expression and the siblings.
    parent_id <- getParseParent(parseData, id)
    siblings_id <- getParseChildren(parseData, parent_id)

    ## Determine if the expression block is attached to a statement.
    ## It is if, and only if, some tokens of the siblings are
    ## different from "'{'", "'}'" or "expr".
    statement <- any(is.na(match(parseData[as.character(siblings_id), "token"],
                                 c("'{'", "'}'", "expr"))))

    ## Return the parent or expression block id, as appropriate.
    if (statement)
        parent_id
    else
        id
}

## Get the starting column of a statement.
##
## For control statements and anonymous function definitions, this is
## just the "col1" element of the parse information data frame.
##
## However, for named function definitions, this is rather the "col1"
## element of the parent expression that contains the assignment.
getParseColumn <- function(parseData, id)
{
    ## Trivial case.
    if (id == 0)
        return(1)

    ## List of assignment tokens.
    assign_tokens <- c("LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN")

    ## Get the id of the parent expression, the children and the
    ## siblings.
    parent_id <- getParseParent(parseData, id)
    children_id <- getParseChildren(parseData, id)
    siblings_id <- getParseChildren(parseData, parent_id)

    ## Determine if the statement is a named function definition. It
    ## is if the token "FUNCTION" is among the children of the
    ## expression and the parent expression is an assignment.
    named_fun_def <-
        any(parseData[as.character(children_id), "token"] %in%
            "FUNCTION") &&
        any(parseData[as.character(siblings_id), "token"] %in%
            assign_tokens)

    ## Return the column number of the parent assignment expression or
    ## the current statement, as appropriate.
    if (named_fun_def)
        parseData[as.character(parent_id), "col1"]
    else
        parseData[as.character(id), "col1"]
}

###
### Closing braces
###
close_brace_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Locate tokens corresponding to a closing brace.
    w <- which(parseData$token == "'}'")

    ## If there are no closing braces, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Get the expressions (actually: source lines) containing closing
    ## braces without leading or trailing whitespace.
    lines <- parseData$line1[w]
    expr <- trimws(srcData$Lines[lines])

    ## Check that the closing brace is either: alone on its own line;
    ## on its line and immediately followed by an 'else' statement.
    valid_line <- grepl(r"(^\}$)", expr) | grepl(r"(^\} else( \{)?$)", expr)

    ## Check that the closing brace is in the same column as the start
    ## of its corresponding statement.
    brace_id <- parseData$id[w]
    statement_id <- sapply(getParseParent(parseData, brace_id),
                           getParseStatementId,
                           parseData = parseData)
    valid_col <- parseData[as.character(brace_id), "col1"] ==
        sapply(statement_id, getParseColumn, parseData = parseData)

    ## The positioning is valid if both criteria are met.
    valid <- valid_line & valid_col
    res <- all(valid)

    if (!res)
    {
        lines <- lines[!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("put closing braces on their own line, aligned with their statement"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

###
### Opening braces
###
open_brace_style <- function(srcData, style = c("R", "1TBS"))
{
    ## Set bracing style to check.
    style <- match.arg(style)
    pat <- switch(style,
                  "R" = r"(^\{$)",
                  "1TBS" = r"( \{$)")
    msg <- switch(style,
                  "R" = gettext("put opening braces on their own line, aligned with their statement"),
                  "1TBS" = gettext("put opening braces after their statement, separated by a space"))

    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Locate tokens corresponding to an opening brace.
    w <- which(parseData$token == "'{'")

    ## If there are no opening braces, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Get the expressions (source lines) containing opening braces
    ## without leading or trailing whitespace.
    lines <- parseData$line1[w]
    expr <- trimws(srcData$Lines[lines])

    ## Check that the opening brace is on its own line.
    valid_line <- grepl(pat, expr)

    ## For the "R" bracing style: check that the opening brace is
    ## in the same column as the start of its corresponding statement.
    ## The latter expression is the grand-parent of the brace in the
    ## parse tree (the parent being the expression within braces
    ## itself).
    ##
    ## For the "1TBS" bracing style, the correct positioning was
    ## checked previously.
    valid_col <-
        if (style == "R")
        {
            brace_id <- parseData$id[w]
            statement_id <- sapply(getParseParent(parseData, brace_id),
                                   getParseStatementId,
                                   parseData = parseData)
            parseData[as.character(brace_id), "col1"] ==
                sapply(statement_id, getParseColumn, parseData = parseData)
        }
        else
            TRUE

    ## The positioning is valid if both criteria are met.
    valid <- valid_line & valid_col
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- lines[!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ", msg, appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

###
### Unique bracing style in one file
###
open_brace_unique_style <- function(srcData)
{
    ## First check whether there are opening braces in the code. If
    ## there are none, return TRUE; job done.
    w <- which(srcData$parseData$token == "'{'")
    if (!length(w))
        return(TRUE)

    ## Otherwise, check supported styles; the linters should not both
    ## be TRUE.
    l1 <- suppressMessages(open_brace_style(srcData, style = "R"))
    l2 <- suppressMessages(open_brace_style(srcData, style = "1TBS"))
    res <- xor(l1, l2)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- sort(unique(c(attr(l1, "line"), attr(l2, "line"))))
        msg <- .makeMessage(gettext("Different bracing styles used in the same file; pick one\n"),
                            gettext("  see lines"), ": ", paste0(lines, collapse = ", "),
                            appendLF = TRUE)
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}


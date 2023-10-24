### roger: Automated grading of R scripts
###
### Validation of spacing around parentheses.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

###
### Unexported auxiliary functions
###

## Determine if a left parenthesis belongs to a function definition,
## that is, if the token "FUNCTION" is a sibling of the parenthesis.
is.fundef <- function(parseData, id)
{
    ## Get the id of the siblings.
    sibling_id <- getParseSiblings(parseData, id)

    ## Check if any sibling is "FUNCTION".
    any(parseData[as.character(sibling_id),
                  "token"] == "FUNCTION")
}

## Determine if a left parenthesis belongs to a function call, that
## is, if a sibling 'expr' has a child with a token
## "SYMBOL_FUNCTION_CALL".
is.funcall <- function(parseData, id)
{
    ## Get the id of the siblings.
    sibling_id <- getParseSiblings(parseData, id)

    ## Keep only the siblings that are expressions.
    sibling_id <- sibling_id[parseData[as.character(sibling_id),
                                       "token"] == "expr"]

    ## Get the id of all the children.
    child_id <- unlist(lapply(sibling_id, getParseChildren,
                              parseData = parseData))

    ## Check if any child is "SYMBOL_FUNCTION_CALL".
    any(parseData[as.character(child_id),
                  "token"] == "SYMBOL_FUNCTION_CALL")
}

## Determine if a there is no space before a left parenthesis in
## function definitions and function calls.
left_parenthesis_funcall <- function(parseData, w)
{
    ## The previous token is one line above in the parse information.
    parseData$col1[w] - parseData$col2[w - 1L] == 1L
}

## Determine if a space is present before the left parenthesis in an
## expression involving operators and control structures. This needs
## to be done recursively up the parse tree.
##
## Exception where no space is needed: the operator is on a previous
## line; after unary operators; after "special" operators: '^', '/', ':',
## '(', '[', 'LBB' ('[[').
left_parenthesis_iter <- function(parseData, w)
{
    ## Get the id of the token, its parent, and its "uncles", the
    ## siblings of the parent.
    id <- parseData$id[w]
    parent_id <- getParseParent(parseData, id)
    uncle_id <- getParseSiblings(parseData, parent_id)

    ## Keep only the uncles that are either expressions or the keyword
    ## 'in' in 'for' loops.
    uncle_id <- uncle_id[parseData[as.character(uncle_id),
                                   "token"] %in% c("expr", "IN")]

    ## Stop if the top of the parse tree is reached.
    if (parent_id == 0)
        return(TRUE)

    ## If there are uncles and the parent is the leftmost expression,
    ## move up the parse tree; otherwise check the spacing.
    if (length(uncle_id) && all(parent_id < uncle_id))
        Recall(parseData, which(parseData$id == parent_id))
    else
    {
        ## When the left sibling is an expression or the condition of
        ## a for loop, the previous token is two lines above in the
        ## parse information. Otherwise, the left sibling is a control
        ## structure and the previous token is one line above.
        nup <- 1L + parseData$token[w - 1L] %in% c("expr", "forcond")

        ## Check all conditions for proper spacing.
        (parseData$col1[w] - parseData$col2[w - nup] > 1L) ||
            (parseData$line1[w] != parseData$line2[w - nup]) ||
            (!length(uncle_id)) ||
            any(parseData$token[w - 2L] %in%
                c("'^'", "'/'", "':'", "'('", "'['", "LBB"))
    }
}

###
### Closing parenthesis: no space immediately before
###
close_parenthesis_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to a closing parenthesis.
    w <- which(parseData$token == "')'")

    ## If there are no closing parentheses, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Check that closing parentheses are not preceded by a space, or
    ## that the previous expression is on a different line.
    valid <- (parseData$col1[w] - parseData$col2[w - 1L] == 1L) |
        (parseData$line1[w] != parseData$line2[w - 1L])

    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- parseData$line1[w][!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("do not use spaces before closing parentheses"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

###
### Opening parenthesis: no space immediately after
###
open_parenthesis_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to an opening parenthesis.
    w <- which(parseData$token == "'('")

    ## If there are no opening parentheses, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Check that opening parentheses are not followed by a space, or
    ## that the following expression is on a different line.
    valid <- (parseData$col1[w + 1L] - parseData$col1[w] == 1L) |
        (parseData$line1[w + 1L] != parseData$line1[w])
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        lines <- parseData$line1[w][!valid]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("do not use spaces after opening parentheses"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

###
### Left parenthesis: space immediately before a left parenthesis
### except in function calls, or after unary and some special
### operators (including the left parenthesis).
###
left_parenthesis_style <- function(srcData)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to an opening parenthesis.
    w <- which(parseData$token == "'('")

    ## If there are no left parentheses, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Initialize a boolean vector for the results of the code
    ## analysis.
    valid <- logical(length(w))

    ## There should be no space after the keyword 'function' of
    ## function definitions, and in function calls. These cases are
    ## easy to analyse. First, identify the parentheses that belong to
    ## either.
    id <- parseData$id[w]
    fc <- sapply(id, is.fundef, parseData = parseData) |
        sapply(id, is.funcall, parseData = parseData)

    ## Check that there is no space before the left parenthesis in
    ## function calls.
    wfc <- w[fc]
    valid[fc] <- left_parenthesis_funcall(parseData, wfc)

    ## Now check that there is a space before the left parenthesis for
    ## the remaining cases, if any.
    expr <- !fc
    if (any(expr))
    {
        wexpr <- w[expr]
        valid[expr] <- sapply(wexpr, left_parenthesis_iter,
                              parseData = parseData)
    }

    ## Wrap up.
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        i <- fc[!valid]
        lines <- parseData$line1[w][!valid]
        msg <- character(length(lines))

        ## First, the messages for the errors in function calls.
        msg[i] <- sapply(lines[i], function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("do not use spaces before opening parentheses in function calls"),
                         appendLF = TRUE))

        ## Second, the messages for the errors in other contexts.
        i <- !i
        msg[i] <- sapply(lines[i], function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("use spaces before opening parentheses (except in function calls)"),
                         appendLF = TRUE))

        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

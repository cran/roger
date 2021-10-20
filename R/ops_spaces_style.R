### roger: Automated grading of R scripts
###
### Validation of spaces around infix operators.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

## Operators for which spacing is tested.
ops_list <- c("'+'", "'-'", "'*'", "GT", "GE", "LT", "LE", "EQ", "NE",
              "AND", "OR", "AND2", "OR2", "'!'",
              "LEFT_ASSIGN", "RIGHT_ASSIGN", "EQ_ASSIGN",
              "EQ_SUB", "SPECIAL")

ops_spaces_style <- function(srcData, ops = ops_list)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to operators in the list.
    w <- which(parseData$token %in% ops)

    ## If there are no operators, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## Get the position (line, column) of every operator.
    lines <- parseData$line1[w]
    col1 <- parseData$col1[w]
    col2 <- parseData$col2[w]

    ## Standard unary and binary operators have one and two siblings
    ## (or arguments) in the parse information, respectively.
    ## However, the argument assignment operator "EQ_SUB" ('=') easily
    ## has five or more.
    ##
    ## Without loss of generality, the siblings of interest to check
    ## for adequate spacing around operators are the "adjacent" ones,
    ## that is, the siblings immediately before (if applicable) and
    ## after an operator in the parse information.
    ##
    ## Get the id of the adjacent siblings for every operator.
    sib <- lapply(parseData$id[w], getParseAdjSiblings,
                  parseData = parseData)

    ## Determine if an operator is infix. An infix operator has more
    ## than one sibling.
    infix <- sapply(sib, function(s) length(s) > 1L)

    ## Check validity of spacing for infix operators. Infix operators
    ## must have a space on both sides, or their first or second
    ## sibling on the previous or next line, respectively.
    valid <- logical(length(w))
    sib.1 <- as.character(sapply(sib[infix], "[", 1L))
    sib.2 <- as.character(sapply(sib[infix], "[", 2L))
    valid_before <-
        (col1[infix] - parseData[sib.1, "col2"] > 1L) |
        (lines[infix] - parseData[sib.1, "line2"] >= 1L)
    valid_after <-
        (parseData[sib.2, "col1"] - col2[infix] > 1L) |
        (parseData[sib.2, "line1"] - lines[infix] >= 1L)
    valid[infix] <- valid_before & valid_after

    ## Check validity of spacing for unary (prefix) operators. Unary
    ## operators must be contiguous to their argument.
    unary <- !infix
    sib.1 <- as.character(sapply(sib[unary], "[", 1L))
    valid[unary] <-
        (parseData[sib.1, "col1"] - col2[unary] == 1L) &
        (parseData[sib.1, "line1"] - lines[unary] == 0L)

    ## Wrap up.
    res <- all(valid)

    ## Return an error message for lines that are not valid.
    if (!res)
    {
        i <- infix[!valid]
        lines <- lines[!valid]
        msg <- character(length(lines))

        ## First, the messages for infix operators.
        msg[i] <- sapply(lines[i], function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("use spaces on both sides of infix operators"),
                         appendLF = TRUE))

        ## Second, the messages for unary operators.
        i <- !i
        msg[i] <- sapply(lines[i], function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("do not use spaces after unary operators"),
                         appendLF = TRUE))

        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

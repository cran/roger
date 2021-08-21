### roger: Automated grading of R scripts
###
### Validation of the absence of magic numbers in code.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

nomagic_style <- function(srcData, ignore = c(-1, 0, 1, 2, 100),
                          ignore.also = NULL)
{
    ## Get parse information from argument.
    parseData <- srcData$parseData

    ## Locate tokens corresponding to a numeric constant.
    w <- which(parseData$token == "NUM_CONST")

    ## If there are no numeric constants, return TRUE; job done.
    if (!length(w))
        return(TRUE)

    ## IGNORED NUMBERS
    ##
    ## Some numbers are considered "not magic": -1, 0, 1, 2 and 100 by
    ## default, plus those provided in 'ignore.also' (with or without
    ## the suffix "L" denoting strict integers).
    ignore <- c(ignore, ignore.also)
    pat <- paste0("^(", paste0(ignore, collapse = "|"), ")L?$")
    valid <- grepl(pat, parseData$text[w])

    ## Remove positions of ignored numbers from the list of potential
    ## magic numbers.
    w <- w[!valid]

    ## NUMBERS IN INDEXING
    ##
    ## Numbers used directly (as the only expression) in indexing are
    ## considered "not magic". The parent expression of these numbers
    ## is a sibling of tokens '[' or "LBB" (the '[[' operator).
    id <- getParseParent(parseData, parseData$id[w])
    sib <- lapply(id, getParseSiblings, parseData = parseData)
    valid <- sapply(sib, function(s)
        any(c("'['", "LBB") %in% parseData[as.character(s), "token"]))

    ## Remove positions of numbers in indexing from the list of
    ## potential magic numbers.
    w <- w[!valid]

    ## NUMBERS IN ASSIGNMENT EXPRESSIONS
    ##
    ## Numbers that appear in "simple" assignments to variable with
    ## all uppercase names are considered "not magic". Such
    ## expressions should have few children in the parse tree and hold
    ## on a single line of source code.
    ##
    ## "Simplicity" of an assignment is determined by the number of
    ## levels of sub-expressions in the call to '<-':
    ##
    ##   M <- 32          1 level ('32')
    ##   M <- 32 - 1      2 levels ('32', '32 - 1')
    ##   M <- 2^32 - 1    3 levels ('2', '2^32', '2^32 - 1')
    ##   ...
    ##
    ## Set the maximum number of levels (this is a valid magic number!).
    MAXLEVELS <- 3L

    ## Determine a regex pattern to identify valid assignment
    ## expressions with '<-', '=' or '->' to names in uppercase
    ## letters only (and the other symbols accepted in names).
    name <- "[A-Z.][A-Z1-9_.]*"
    pat <- paste0("(^", name, " *(<-|=))", "|", "(-> *", name, "$)")

    ## Starting from NUM_CONST, move up MAXLEVELS in the call stack to
    ## identify simple assignments of magic numbers to variables.
    id <- parseData$id[w]
    for (i in seq_len(MAXLEVELS))
    {
        ## Move one level up the call stack.
        id <- getParseParent(parseData, id)

        ## Get the text of the parent expression.
        expr <- getParseText(parseData, getParseParent(parseData, id))

        ## Determine if it is a valid assignment of a magic number.
        valid <- grepl(pat, expr)

        ## Remove id and position of numbers in simple assignments
        ## from the list of potential magic numbers.
        id <- id[!valid]
        w <- w[!valid]
    }

    ## Check that there are no remaining magic numbers.
    res <- length(w) == 0L

    ## Return an error message for lines containing magic numbers.
    if (!res)
    {
        lines <- parseData$line1[w]
        msg <- sapply(lines, function(l)
            .makeMessage(gettext("Line"), " ", l, ": ",
                         gettext("potential magic number; assign to a variable"),
                         appendLF = TRUE))
        attributes(res) <- list(lines = lines, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

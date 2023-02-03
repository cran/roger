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

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Locate tokens corresponding to a numeric constant (other than
    ## logical values 'TRUE' and 'FALSE', and the special values
    ## 'Inf', 'NaN' and 'NA').
    w <- which(parseData$token == "NUM_CONST"
               & !parseData$text %in% c("TRUE", "FALSE", "Inf", "NaN", "NA"))

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
    ## The shape of the parse tree for simple assignment expressions
    ## is as follows:
    ##
    ##             expr
    ##    /---------|--------\
    ## expr    LEFT_ASSIGN  expr (*)
    ##  |                    |
    ## SYMBOL               NUM_CONST
    ##
    ## Expressions involving NUM_CONST on the right hand side of
    ## assignments have more 'expr' levels below the 'expr' marked
    ## (*), above.
    ##
    ## Numbers in "simple" assignments to variables with all uppercase
    ## names are considered "not magic". Simplicity is determined by
    ## the number of levels of sub-expressions in the call to '<-':
    ##
    ##   M <- 32          1 level ('32')
    ##   M <- 32 - 1      2 levels ('32', '32 - 1')
    ##   M <- 2^32 - 1    3 levels ('2', '2^32', '2^32 - 1')
    ##   ...
    ##
    ## Set the maximum number of levels (this is a valid magic number!).
    MAXLEVELS <- 3L

    ## Function to get the text of the SYMBOL token corresponding to
    ## the id of the first child of 'expr (*)' above, be it a
    ## NUM_CONST or an 'expr' containing a NUM_CONST.
    getSymbolName <- function(parseData, id)
    {
        ## Get the id of the 'expr' parent of 'id'.
        id.parent <- getParseParent(parseData, id)

        ## Get the parse information of all the siblings of 'expr'
        ## (this excludes 'expr' itself).
        p <- parseData[as.character(getParseSiblings(parseData, id.parent)), ]

        ## We look after the SYMBOL text if one of the siblings is an
        ## assignment. The expression on the left hand side should
        ## only be a symbol.
        if (any(p$token %in% c("LEFT_ASSIGN", "RIGHT_ASSIGN")))
        {
            id.expr <- p$id[which(p$token == "expr")]
            w <- which(parseData$token == "SYMBOL" & parseData$parent == id.expr)
            if (length(w) > 0L)
                return(parseData$text[w])
        }

        ## Return NULL in all other cases.
        NULL
    }

    ## Function to check that the text of a symbol is a valid
    ## identifier for a magic number, namely: containing only
    ## uppercase letters, digits, the period and the underscore.
    is.validName <- function(name)
        !(is.null(name) || nzchar(gsub("[[:upper:][:digit:]_.]", "", name)))

    ## Starting from NUM_CONST, move up MAXLEVELS in the call stack to
    ## identify simple assignments of magic numbers to variables.
    id <- parseData$id[w]
    for (i in seq_len(MAXLEVELS))
    {
        ## Determine if the NUM_CONST are valid assignments of magic
        ## numbers.
        valid <- sapply(id,
                        function(id) is.validName(getSymbolName(parseData, id)))

        ## Remove id and position of valid assignments from the list
        ## of potential magic numbers.
        id <- id[!valid]
        w <- w[!valid]

        ## Move one level up the call stack.
        id <- getParseParent(parseData, id)
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

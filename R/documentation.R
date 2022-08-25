### roger: Automated grading of R scripts
###
### Simple validation of the documentation of a function provided in a
### script file. Only checks the presence of documentation elements,
### not their actual content.
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

any_doc <- function(srcData, ...)
{
    ## Get source code from argument.
    Lines <- srcData$Lines

    ## Guard against null source code.
    if (is.null(Lines))
        stop("no source code; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Check that some documentation is present in the source code.
    res <- any(grepl(r"(^#+ +[[:alnum:][:punct:]]+)", Lines, ...))

    if (!res)
    {
        msg <- .makeMessage(gettext("No documentation found"),
                            appendLF = TRUE)
        attributes(res) <- list(nlines = length(Lines), message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

signature_doc <- function(srcData, ...)
{
    ## Get parse information and source code from argument.
    parseData <- srcData$parseData
    Lines <- srcData$Lines

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Get the names and formal arguments of the top-level functions
    ## defined in the data frame.
    FUN <- getParseFun(parseData)
    FORMALS <- getParseFormals(parseData)

    ## Build patterns for function calls containing the formals.
    pat <- paste0(r"(^#+ +)", FUN, r"(\()",
                  sapply(FORMALS, paste0, ".*", collapse = ", "),
                  r"(\))")

    ## Check that the patterns appear in the documentation.
    valid <- sapply(pat, function(p) any(grepl(p, Lines, ...)))
    res <- all(valid)

    if (!res)
    {
        FUN <- FUN[!valid]
        msg <- sapply(FUN, function(f)
            .makeMessage(gettext("No signature found for function"), " '", f, "'",
                         appendLF = TRUE))
        attributes(res) <- list(fun = FUN, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

section_doc <- function(srcData, pattern, ...)
{
    ## Get source code from argument.
    Lines <- srcData$Lines

    ## Guard against null source code.
    if (is.null(Lines))
        stop("no source code; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Get the number of top-level functions defined in the data
    ## frame.
    nfun <- length(getParseFun(srcData$parseData))

    ## Complete pattern to match: comment symbol followed by one or
    ## many spaces; the pattern provided in argument; nothing else
    ## than spaces until line end.
    pat <- paste0(r"(^#+ *()", pattern, r"() *$)")

    ## Check if the pattern appears in the documentation as many times
    ## as there are top-level function definitions.
    nsection <- sum(grepl(pat, Lines, ...))
    res <- nfun == nsection

    if (!res)
    {
        msg <- if (nsection == 0L)
                   .makeMessage(gettext("No section matching"), " '", pat, "' ",
                                gettext("found"), appendLF = TRUE)
               else
                   .makeMessage(sprintf(ngettext(nfun,
                                                 "%d top-level function definition found, ",
                                                 "%d top-level function definitions found, "),
                                        nfun),
                                sprintf(ngettext(nsection,
                                                 "but %d section matching",
                                                 "but %d sections matching"),
                                        nsection),
                                " '", pat, "'", appendLF = TRUE)
        attributes(res) <- list(sections = nsection, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

description_section_doc <- function(srcData, ...)
    section_doc(srcData, "Description?", ...)

arguments_section_doc <- function(srcData, ...)
    section_doc(srcData, "Arguments?", ...)

value_section_doc <- function(srcData, ...)
    section_doc(srcData, "Value", ...)

examples_section_doc <- function(srcData, ...)
    section_doc(srcData, "Examples?", ...)

formals_doc <- function(srcData, ...)
{
    ## Get parse information and source code from argument.
    parseData <- srcData$parseData
    Lines <- srcData$Lines

    ## Guard against null parse data.
    if (is.null(parseData))
        stop("no parse data; ",
             "use 'getSourceData' with 'keep.source = TRUE'")

    ## Get the formal arguments of the top-level functions defined in
    ## the data frame, losing to which function they belong. Keep one
    ## copy of each.
    FORMALS <- unique(unlist(getParseFormals(parseData)))

    ## Create search patterns.
    pat <- paste0("^#+ +", FORMALS)

    ## Check that all formals appear in the documentation.
    valid <- sapply(pat, function(p) any(grepl(p, Lines, ...)))
    res <- all(valid)

    if (!res)
    {
        FORMALS <- FORMALS[!valid]
        msg <- sapply(FORMALS, function(f)
            .makeMessage(gettext("Description of argument"), " '", f, "' ",
                         gettext("missing from the documentation"),
                         appendLF = TRUE))
        attributes(res) <- list(formals = FORMALS, message = msg)
        message(msg, appendLF = FALSE)
    }

    res
}

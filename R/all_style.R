### roger: Automated grading of R scripts
###
### Utility function to validate code using all available style
### linters.
###
### AUTHOR: Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

all_style <- function(srcData, include, exclude, ...)
{
    ## List of available linters.
    linters <- c("assignment_style",
                 "close_brace_style",
                 "close_bracket_style",
                 "close_parenthesis_style",
                 "commas_style",
                 "comments_style",
                 "left_parenthesis_style",
                 "line_length_style",
                 "nomagic_style",
                 "open_brace_style",
                 "open_brace_unique_style",
                 "open_bracket_style",
                 "open_parenthesis_style",
                 "ops_spaces_style",
                 "trailing_blank_lines_style",
                 "trailing_whitespace_style",
                 "unneeded_concatenation_style")

    ## Keep the call without arguments 'include' and 'exclude'. We
    ## will replace the function name by each linter in turn below.
    Call <- match.call(expand.dots = TRUE)
    Call$include <- Call$exclude <- NULL

    ## Set the list of linters to use. Part of the code in the 'if'
    ## statements below is adapted from 'match.arg'.
    if (!(missing(include) || is.null(include)))
    {
        i <- pmatch(include, linters, nomatch = 0L, duplicates.ok = TRUE)
        if (all(i == 0L))
            stop(sprintf("%s does not match any of the available linters: %s",
                         sQuote("include"),
                         paste(dQuote(linters), collapse = ", ")))
        i <- i[i > 0L]
        linters <- linters[i]
    }
    if (!(missing(exclude) || is.null(exclude)))
    {
        i <- pmatch(exclude, linters, nomatch = 0L, duplicates.ok = TRUE)
        if (any(w <- i == 0L))
            warning(sprintf(ngettext(sum(w),
                                     "%s in %s does not match any linter",
                                     "%s in %s do not match any linter"),
                            paste(sQuote(exclude[w]), collapse = ", "),
                            sQuote("exclude")))
        i <- i[!w]
        if (length(i))
            linters <- linters[-i]
    }

    ## Evaluate every linter in turn using a loop to avoid scoping
    ## issues that may arise with an application function.
    res <- as.list(numeric(length(linters)))
    names(res) <- linters
    for (s in linters)
    {
        f <- formals(s)
        m <- match(names(f), names(Call), nomatch = 0L)
        cl <- Call[c(1, m)]
        cl[[1L]] <- str2lang(s)
        res[[s]] <- eval.parent(cl)
    }
    res
}

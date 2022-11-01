### roger: Automated grading of R scripts
###
### R interface for the roger command line tool.
###
### AUTHORS: Samuel Fréchette, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later

###
### Unexported auxiliary functions
###

## Extract roger tool name and CLI arguments from call.
get_cli_opts <- function(call)
{
    ## Convert call in argument to a list.
    call <- as.list(call)

    ## Remove argument ".debugOnly" if present (never an argument to a
    ## roger tool)
    call$.debugOnly <- NULL

    ## Extract tool name and (evaluated) arguments.
    list("tool" = sub("^roger_", "", as.character(call[[1L]])),
         "opts" = lapply(call[-1L], eval))
}

## Format command line options: path expand, quote and prepend with
## flag.
format_opts <- function(option, flag)
{
    ## If string 'y' is not NULL, path expand and shell quote string
    ## 'y', then prepend string 'x' to it.
    f <- function(x, y)
        if (is.null(y))
            y
        else
            paste0(x, shQuote(path.expand(as.character(y))))

    ## flag[i] is inserted in front of option[i] if, and only if,
    ## option[i] is not NULL.
    mapply(f, flag, option, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

###
### R to CLI interface
###
roger_checkreq <- function(file = "./requirements.txt", ...,
                           .debugOnly = FALSE)
{
    ## Extract tool name and arguments; keep options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts

    ## Quote CLI options and prepend with corresponding flag.
    flags <- c(file = "--file=")
    Call$opts <- format_opts(opts[names(flags)], flags)

    ## Print command as string in debugging mode.
    if (.debugOnly)
        return(paste(c("roger", unlist(Call, use.names = FALSE)),
                     collapse = " "))

    ## Call CLI roger with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}

roger_clone <- function(pattern, project, rogerrc_file = NULL,
                        machine = NULL, curl_options = NULL,
                        api = NULL, quiet = FALSE, ...,
                        .debugOnly = FALSE)
{
    ## Extract tool name and arguments; keep options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts[-c(1L, 2L)]

    ## Quote non-boolean CLI options and prepend with corresponding
    ## flags. Leave 'curl_options' untouched, if any.
    flags <- c(rogerrc_file = "--rogerrc-file=",
               machine = "--machine=",
               api = "--api=")
    opts[names(flags)] <- format_opts(opts[names(flags)], flags)

    ## Boolean argument 'quiet' translates into an option without a
    ## value.
    opts$quiet <- if (quiet) "--quiet"

    ## Rewrite complete list of CLI arguments in suitable order.
    Call$opts <- c(opts, "--", shQuote(c(pattern, project)))

    ## Print command as string in debugging mode.
    if (.debugOnly)
        return(paste(c("roger", unlist(Call, use.names = FALSE)),
                     collapse = " "))

    ## Call CLI roger with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}

roger_grade <- function(dir = getwd(), config_file = NULL,
                        time_limit = NULL, detached_head = FALSE,
                        output_file = NULL, quiet = FALSE, ...,
                        .debugOnly = FALSE)
{
    ## Extract tool name and arguments; keep argument with a default
    ## value and options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts

    ## Quote non-boolean CLI options and prepend with corresponding
    ## flags. This also drops the argument with a default value from
    ## the options list.
    flags <- c(config_file = "--config-file=",
               time_limit = "--time-limit=",
               output_file = "--output-file=")
    opts <- format_opts(opts[names(flags)], flags)

    ## Boolean argument 'detached_head' and 'quiet' translate into an
    ## option without a value.
    opts$detached_head <- if (detached_head) "--detached-head"
    opts$quiet <- if (quiet) "--quiet"

    ## Rewrite complete list of CLI arguments in suitable order.
    Call$opts <- c(opts, shQuote(dir))

    ## Print command as string in debugging mode.
    if (.debugOnly)
        return(paste(c("roger", unlist(Call, use.names = FALSE)),
                     collapse = " "))

    ## Call roger CLI with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}

roger_push <- function(branch, repos = getwd(), create = FALSE,
                       file = NULL, add_file = NULL, message = NULL,
                       quiet = FALSE, ..., .debugOnly = FALSE)
{
    ## Extract tool name and arguments; keep argument with a default
    ## value and options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts[-1L]

    ## Argument 'add_file' can be a vector of file names. Each file
    ## name requires a flag in front to generate multiple options in
    ## the CLI call.
    if (!is.null(add_file))
        opts$add_file <- format_opts(opts$add_file, "--add-file=")

    ## Idem for argument 'message'.
    if (!is.null(message))
        opts$message <- format_opts(opts$message, "--message=")

    ## Quote remaining non-boolean CLI options and prepend with
    ## corresponding flag.
    flags <- c(file = "--file=")
    opts[names(flags)] <- format_opts(opts[names(flags)], flags)

    ## Drop the argument with a default value from the options list.
    opts$repos <- NULL

    ## Boolean arguments 'create' and 'quiet' translate into options
    ## without a value.
    opts$create <- if (create) "--create"
    opts$quiet <- if (quiet) "--quiet"

    ## Rewrite complete list of CLI arguments in suitable order.
    Call$opts <- c(opts, shQuote(c(branch, repos)))

    ## Print command as string in debugging mode.
    if (.debugOnly)
        return(paste(c("roger", unlist(Call, use.names = FALSE)),
                     collapse = " "))

    ## Call roger CLI with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}

roger_switch <- function(branch, repos = getwd(), quiet = FALSE,
                         ..., .debugOnly = FALSE)
{
    ## Extract tool name and arguments; keep argument with a default
    ## value and options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts[-1L]

    ## Drop the argument with a default value from the options list.
    opts$repos <- NULL

    ## Boolean argument 'quiet' translates into an option without a
    ## value.
    opts$quiet <- if (quiet) "--quiet"

    ## Rewrite complete list of CLI arguments in suitable order.
    Call$opts <- c(opts, shQuote(c(branch, repos)))

    ## Print command as string in debugging mode.
    if (.debugOnly)
        return(paste(c("roger", unlist(Call, use.names = FALSE)),
                     collapse = " "))

    ## Call roger CLI with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}

roger_validate <- function(dir = getwd(), config_file = NULL,
                           check_local_repos = TRUE, ...,
                           .debugOnly = FALSE)
{
    ## Extract tool name and arguments; keep options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts

    ## Quote non-boolean CLI options and prepend with corresponding
    ## flags. This also drops the argument with a default value from
    ## the options list.
    flags <- c(config_file = "--config-file=")
    opts <- format_opts(opts[names(flags)], flags)

    ## Boolean argument 'check_local_repos' translates into an option
    ## without a value (with an inverted logic).
    opts$check_local_repos <-
        if (!check_local_repos) "--no-check-local-repos"

    ## Keep only first directory name.
    Call$opts$dir <- Call$opts$dir[1L]

    ## Rewrite complete list of CLI arguments in suitable order.
    Call$opts <- c(opts, shQuote(dir))

    ## Print command as string in debugging mode.
    if (.debugOnly)
        return(paste(c("roger", unlist(Call, use.names = FALSE)),
                     collapse = " "))

    ## Call roger CLI with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}


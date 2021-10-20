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
    ## Convert call in argument to list.
    call <- as.list(call)

    ## Extract tool name and (evaluated) arguments.
    list("tool" = as.character(call[[1L]]),
         "opts" = lapply(call[-1L], eval))
}

## Format command line options: path expand, quote and prepend with
## flag.
format_opts <- function(option, flag)
{
    ## If string 'y' is not NULL, path expand and shell quote string
    ## 'y', then prepend string 'x' to it.
    f <- function(x, y)
        if (is.null(y)) y else paste0(x, shQuote(path.expand(y)))

    ## flag[i] is inserted in front of option[i] if, and only if,
    ## option[i] is not NULL.
    mapply(f, flag, option, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

###
### R to CLI interface
###
checkreq <- function(file = "./requirements.txt", ...)
{
    ## Extract tool name and arguments; keep options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts

    ## Quote CLI options and prepend with corresponding flag.
    flags <- c(file = "--file=")
    Call$opts <- format_opts(opts[names(flags)], flags)

    ## Call CLI roger with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}

clone <- function(project, pattern, page_limit = NULL, machine = NULL,
                  curl_options = NULL, api = "bitbucket", ...)
{
    ## Extract tool name and arguments; keep options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts[-c(1L, 2L)]

    ## Match the API given in argument to the corresponding CLI
    ## option.
    api <- match.arg(api)
    opts$api <- switch(api,
                       "bitbucket" = "--bitbucket-api",
                       stop("unsupported API"))

    ## Append "--" to curl_options arguments as a safety precaution.
    opts$curl_options <- paste0(opts$curl_options, " --")

    ## Quote remaining CLI options and prepend with corresponding
    ## flag.
    flags <- c(page_limit = "--page-limit=",
               machine = "--machine")
    opts[names(flags)] <- format_opts(opts[names(flags)], flags)

    ## Rewrite complete list of CLI arguments in suitable order.
    Call$opts <- c(opts, Call$opts[c("pattern", "project")])

    ## Call CLI roger with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}

grade <- function(dir, config_file = NULL, time_limit = NULL,
                  output_file = NULL, ...)
{
    ## Extract tool name and arguments; keep options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts[-1L]

    ## Quote CLI options and prepend with corresponding flag.
    flags <- c(config_file = "--config-file=",
               time_limit = "--time-limit=",
               output_file = "--output-file=")
    opts <- format_opts(opts[names(flags)], flags)

    ## Rewrite complete list of CLI arguments in suitable order.
    Call$opts <- c(opts, Call$opts["dir"])

    ## Call roger CLI with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}

push <- function(repos, branch, add_file = NULL, create_branch = FALSE,
                 file = NULL, message = NULL, ...)
{
    ## Extract tool name and arguments; keep options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts[-c(1L, 2L)]

    ## Boolean argument 'create_branch' translates into an option
    ## without a value.
    opts$create_branch <-
        if (create_branch) "--create-branch" else NULL

    ## Argument 'add_file' can be a vector of file names. Each file
    ## name requires a flag in front to generate multiple options in
    ## the CLI call.
    if (!is.null(add_file))
        opts$add_file <- format_opts(opts$add_file, "--add-file=")

    ## Idem for argument 'message'.
    if (!is.null(message))
        opts$message <- format_opts(opts$message, "--message=")

    ## Quote remaining CLI options and prepend with corresponding
    ## flag.
    flags <- c(file = "--file=")
    opts[names(flags)] <- format_opts(opts[names(flags)], flags)

    ## Rewrite complete list of CLI arguments in suitable order.
    Call$opts <- c(opts, Call$opts[c("branch", "repos")])

    ## Call roger CLI with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}

validate <- function(dir, config_file = NULL,
                     check_local_repos = TRUE, ...)
{
    ## Extract tool name and arguments; keep options.
    Call <- get_cli_opts(match.call())
    opts <- Call$opts[-1L]

    ## Boolean argument 'check_local_repos' translates into an option
    ## without a value (with an inverted logic).
    opts$check_local_repos <-
        if (check_local_repos) NULL else "--no-check-local-repos"

    ## Quote remaining CLI options and prepend with corresponding
    ## flag.
    flags <- c(config_file = "--config-file=")
    opts[names(flags)] <- format_opts(opts[names(flags)], flags)

    ## Keep only first directory name.
    Call$opts$dir <- Call$opts$dir[1L]

    ## Rewrite complete list of CLI arguments in suitable order.
    Call$opts <- c(opts, Call$opts["dir"])

    ## Call roger CLI with the tool and arguments.
    system2("roger", args = unlist(Call, use.names = FALSE), ...)
}


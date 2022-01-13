\name{roger-interface}
\alias{roger-interface}
\alias{roger_checkreq}
\alias{roger_clone}
\alias{roger_grade}
\alias{roger_push}
\alias{roger_switch}
\alias{roger_validate}
\title{R Interface for Roger Command Line Tools}
\description{
  R interfaces to the Roger base system command line tools \code{roger
  checkreq}, \code{roger clone}, \code{roger grade}, \code{roger push},
  \code{roger switch} and \code{roger validate}.
}
\usage{
roger_checkreq(file = "./requirements.txt", \dots,
               .debugOnly = FALSE)

roger_clone(project, pattern, page_limit = NULL, machine = NULL,
            curl_options = NULL, api = "bitbucket", \dots,
            .debugOnly = FALSE)

roger_grade(dir, config_file = NULL, time_limit = NULL,
            detached_head = FALSE, output_file = NULL, \dots,
            .debugOnly = FALSE)

roger_push(repos, branch, create = FALSE, file = NULL,
           add_file = NULL, message = NULL, quiet = FALSE, \dots,
           .debugOnly = FALSE)

roger_switch(repos, branch, quiet = FALSE, \dots,
             .debugOnly = FALSE)

roger_validate(dir, config_file = NULL, check_local_repos = TRUE,
               \dots, .debugOnly = FALSE)
}
\arguments{
  \item{project}{name of a Git project containing repositories.}
  \item{pattern}{regular expression pattern.}
  \item{dir}{character vector of directory names containing projects to
    grade or validate; only the first one is used by \code{validate}.}
  \item{repos}{character vector of Git repository names to publish
    grading results into.}
  \item{branch}{name of the branch in which to publish the grading
    results (identical for every repository).}
  \item{add_file}{character vector of file names to publish along with
    the grading results.}
  \item{api}{character string; name of the REST API used to retrieve
    the urls of the repositories. Currently only the BitBucket API is
    supported.}
  \item{check_local_repos}{boolean; check the status of the local
    repository?}
  \item{config_file}{name of grading or validation configuration file;
    overrides the defaults \code{gradeconf} and \code{valideconf}.}
  \item{create}{boolean; is \code{branch} a new branch to create
    in the repositories?}
  \item{curl_options}{character vector of command line options to pass
    to \code{curl}.}
  \item{detached_head}{boolean; leave the repositories in a detached
    head state for further manual grading?}
  \item{file}{requirements file name for \code{checkreq}; name of the
    grading results file for \code{push} (overriding the default, locale
    dependent, value).}
  \item{machine}{URI and context of the Git server.}
  \item{message}{character vector of commit messages pasted together to
    form a single paragraph.}
  \item{output_file}{grading results file name; if \code{NULL} or
    \code{-}, results are written to standard output.}
  \item{page_limit}{integer; value of the REST API parameter
    \code{limit} indicating the number of results to return per page.}
  \item{quiet}{boolean; suppress output?}
  \item{time_limit}{date and time in ISO 8601 format (YYYY-MM-DD
    HH:MM:SS) by which to grade a project in a Git repository.}
  \item{\dots}{further arguments passed to \code{\link{system2}}.}
  \item{.debugOnly}{boolean; print the system call only?}
}
\details{
  These functions build calls to the Roger base system command line
  tools and execute them using \code{\link{system2}}.

  Command line option values are always quoted with
  \code{\link{shQuote}}.

  Refer to the command line tools documentation for detailed information
  on the arguments and options.
}
\value{
  Character vector containing the standard output and standard error of
  the command line tools.
}
\note{
  The interface functions require that the Roger base system is
  installed on your machine and in your system path. See the Roger
  Project download page \url{https://roger-project.gitlab.io/download/}.
}
\examples{
## Sample usage for students
\dontrun{## Validate the project in the current directory using the
## configuration file 'validateconf-prototype'.
roger_validate(".", config_file = "validateconf-prototype")}

## Sample usage for graders.
\dontrun{## First check the availability of the grading tools.
roger_checkreq()

## Clone all repositories in the BitBucket project 'a2020-12345'
## matching the pattern '[0-9]{9}_prototype'.
roger_clone("a2042-12345", "[0-9]{9}_prototype")

## Grade all directories (repositories) starting with '[0-9]*'
## as of 2020-04-30 23:59:59 using the configuration in file
## 'gradeconf-prototype'; write results in file 'GRADING.txt'
## of each directory; leave repositories in detached head state
## for manual grading of files.
roger_grade("[0-9]*/", config_file = "gradeconf-prototype",
            time_limit = "2020-04-30 23:59:59", detached_head = TRUE,
            output_file = "GRADING.txt")

## Publish results in every repository in a new branch
## 'grading-results'. No need to specify the grading results file
## name since 'GRADING.txt' is the default in an English locale.
roger_push("[0-9]*/", "grading-results", create = TRUE,
           message = c("Here are your grading results"))

## Return all repositories to main branch.
roger_switch("[0-9]*/", "main")}
}

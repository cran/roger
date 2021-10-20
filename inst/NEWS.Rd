\name{NEWS}
\title{\pkg{roger} News}
\encoding{UTF-8}

\section{Changes in \pkg{roger} version 0.99-1 (2021-10-19)}{
  \subsection{New features}{
    \itemize{
      \item{The style and documentation linters now stop with an
	informative error message when the parse data or source code in
	argument is \code{NULL}.}
      \item{The \code{any_doc} documentation linter now accepts 
	punctuation characters after the comment character.}
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item{The \code{left_parenthesis_style} linter no longer requires
	a space before a left parenthesis after the division and
	exponent operators, and after an optional opening parenthesis.}
      \item{Script options in interface functions are now path expanded
	on the R side.}
    }
  }
}

\section{Changes in \pkg{roger} version 0.99-0 (2021-08-20)}{
  \itemize{
    \item{Initial public release.}
    \item{The package contains the following style linters:
      \code{assignment_style},
      \code{comma_style},
      \code{close_brace_style},
      \code{close_bracket_style},
      \code{close_parenthesis_style},
      \code{left_parenthesis_style},
      \code{line_length_style},
      \code{nomagic_style},
      \code{open_brace_style},
      \code{open_brace_unique_style},
      \code{open_bracket_style},
      \code{open_parenthesis_style},
      \code{ops_spaces_style},
      \code{trailing_blank_style},
      \code{trailing_whitespace_style},
      \code{unneeded_concatenations_style}.}
    \item{The available documentation linters are:
      \code{any_doc},
      \code{signature_doc},
      \code{section_doc},
      \code{formals_doc}.}
    \item{The package also contains interface functions to the shell
      scripts of the Roger base system:
      \code{checkreq},
      \code{clone},
      \code{grade},
      \code{push},
      \code{validate}.}
  }
}

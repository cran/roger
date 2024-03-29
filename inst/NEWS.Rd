\name{NEWS}
\title{\pkg{roger} News}
\encoding{UTF-8}

\section{Changes in \pkg{roger} version 1.5-1}{
  \subsection{Bug fixes}{
    \itemize{
      \item{\code{left_parenthesis_style} failed on composite
	expressions involving more than one operator, e.g. \code{(x + y)
	  * 3 + 1}.}
    }
  }
}

\section{Changes in \pkg{roger} version 1.5-0}{
  \subsection{New features}{
    \itemize{
      \item{\code{close_bracket_style} and \code{open_bracket_style} now
	accept that the preceding (resp. following) expression is on a
	different line than the bracket.}
      \item{\code{close_parenthesis_style} and
	\code{open_parenthesis_style} now accept that the preceding
	(resp. following) expression is on a different line than the
	parenthesis.}
      \item{\code{ops_spaces_style} now also checks spacing around the
	keyword \code{in} used in \code{for} loops.}
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item{\code{left_parenthesis_style} now works correctly with the
	unary operator \code{!} and with parentheses used to echo the
	result of an expression. The linter is also more robust and
	reliable (although slower) for many other constructions.}
      \item{Translations of messages from \code{all_linters}
	completed.}
    }
  }
}
  
\section{Changes in \pkg{roger} version 1.4-0}{
  \subsection{New features}{
    \itemize{
      \item{\code{all_style} utility function to validate code using all
	available style linters at once.}
      \item{\code{nomagic_style} now allows non-ASCII uppercase letters
	in symbol names.}
    }
  }
  \subsection{Changes}{
    \itemize{
      \item{\code{nomagic_style} uses a more robust procedure to identify
	valid assignments of magic numbers to variables.}	
      \item{\code{line_length_style} now issues only one message per
	line exceeding the length threshold.}
    }
  }
}

\section{Changes in \pkg{roger} version 1.3-1}{
  \subsection{Bug fixes}{
    \itemize{
      \item{\code{commas_style} now allows spaces before a comma if the
	preceeding symbol is also a comma. This exception to the general
	rule is needed for consecutive commas commonly used in the
	indexing of arrays.}
    }
  }
}

\section{Changes in \pkg{roger} version 1.3-0}{
  \subsection{New features}{
    \itemize{
      \item{\code{roger_grade} gains a \code{quiet} argument that was
	missing in earlier releases.}
    }
  }
  \subsection{User visible changes}{
    \itemize{
      \item{Following changes in the base system, the interface
	functions \code{roger_grade}, \code{roger_push},
	\code{roger_switch} and \code{roger_validate} now use a default
	value of \code{getwd()} for the \code{dir} or \code{repos}
	argument.}
      \item{\code{roger_push} and \code{roger_switch}: to put the
	mandatory argument ahead of optional ones, \code{branch} is now
	the first argument of these functions.}
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item{\code{roger_clone} now correctly matches the arguments of
	the corresponding base system tool: argument \code{page_limit}
	is removed; argument \code{rogerrc_file} is added; argument
	\code{api} is now \code{NULL} by default.} 
    }
  }
}

\section{Changes in \pkg{roger} version 1.2-0}{
  \subsection{New feature}{
    \itemize{
      \item{\code{comments_style} linter to check that comment
	delimiters and the text of comments, when there is 
	any, are separated by at least one space.}
    }
  }
  \subsection{User visible changes}{
    \itemize{
      \item{\code{any_comments} now accepts comments without spaces
	between the comment delimiters and the text. Use
	\code{comments_style} to test the actual styling of comments.}
    }
  }
  \subsection{Bug fix}{
    \itemize{
      \item{Add missing French translation for a message from
	\code{any_comments}.} 
    }
  }
}

\section{Changes in \pkg{roger} version 1.1-0}{
  \subsection{New feature}{
    \itemize{
      \item{Function \code{any_comments} to check if a script file
	contains at least one non empty comment.}
    }
  }
}

\section{Changes in \pkg{roger} version 1.0-0}{
  \subsection{Changes}{
    \itemize{
      \item{To reflect an interface change in \code{roger clone} of the
	base system, argument \code{api} of \code{roger_clone} no longer
	has a default value.}
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item{\code{nomagic_style} now omits the logical values
        \code{TRUE} and \code{FALSE}.} 
      \item{\code{section_doc} now works correctly with user
	submitted regex patterns that include alternation operators.}
      \item{\code{section_doc} now requires that only spaces can
	possibly follow the section title on its line. This is to avoid
	false positives caused by a description that starts with the
	same expression as the section title. For example, a section
	"Value" that starts with "Value of\dots".}
    }
  }
}

\section{Changes in \pkg{roger} version 0.99-2 (2022-01-12)}{
  \subsection{User visible changes}{
    \itemize{
      \item{The nomenclature of the interface functions changes from
	\code{foo} to \code{roger_foo}.}
    }
  }
  \subsection{New features}{
    \itemize{
      \item{The set of interface functions matches the new features
	introduced in version 0.99-8 of the Roger base system:
	\code{roger_grade} gains a \code{detached_head} argument;
	\code{roger_push} gains a \code{quiet} argument; new interface
	function \code{roger_switch}.}
      \item{Interface functions gain a \code{.debugOnly} argument to
	print the command that would be sent to roger-base without
	actually executing it.}
    }
  }
  \subsection{Bug fixes}{
    \itemize{
      \item{\code{nomagic_style} now allows the digit \code{0} in
	variable names.}
      \item{\code{nomagic_style} now omits the special numerical
	constants \code{Inf}, \code{NaN} and \code{NA}.}
      \item{The \code{clone} interface now works correctly with multiple
	additional curl arguments.}
      \item{The \code{clone} interface now correctly passes the
	\code{--machine} argument.}
      \item{Required arguments are now shell quoted in interface
	functions.}	
    }
  }
}

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

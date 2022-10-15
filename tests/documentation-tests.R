### roger: Automated grading of R scripts
###
### Tests for the the validity of the documentation linters
###
### AUTHORS: Jean-Christophe Langlois, Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later.

library(roger)

## Tests are run by R CMD check in a non interactive session where
## parse data is thrown out by default.
if (!interactive())
    options(keep.source = TRUE)

###
### Tests for valid documentation
###

## Documentation in English
VALID_DOC_EN_FILE <- tempfile(fileext = ".R")
cat(file = VALID_DOC_EN_FILE, "
###
### foo(x, y = 2)
###
##  Adding two vectors.
##
##  Arguments
##
##  x: a vector
##  y: another vector
##
##  Value
##
##  Sum of the two vectors.
##
##  Examples
##
##  foo(2, 3)
##  foo(1:5)
##
foo <- function(x, y = 2)
    x + y

###
### bar(z)
###
##  Squaring a vector.
##
##  Arguments
##
##  z: a vector
##
##  Value
##
##  Square of the elements of the vector.
##
##  Examples
##
##  bar(1:5)
##
bar <- function(z) z^2
")
VALID_DOC_EN <- getSourceData(VALID_DOC_EN_FILE)

## Tests for valid English documentation
stopifnot(exprs = {
    any_comments(VALID_DOC_EN)
    any_doc(VALID_DOC_EN)
    signature_doc(VALID_DOC_EN)
    section_doc(VALID_DOC_EN, "ARGUMENTS", ignore.case = TRUE)
    arguments_section_doc(VALID_DOC_EN)
    section_doc(VALID_DOC_EN, "value", ignore.case = TRUE)
    value_section_doc(VALID_DOC_EN)
    section_doc(VALID_DOC_EN, "examples", ignore.case = TRUE)
    examples_section_doc(VALID_DOC_EN)
    formals_doc(VALID_DOC_EN)
})

## Documentation in French
VALID_DOC_FR_FILE <- tempfile(fileext = ".R")
cat(file = VALID_DOC_FR_FILE, "
###
### foo(x, y = 2)
###
##  Additionner deux vecteurs.
##
##  Arguments
##
##  x: un vecteur
##  y: un autre vecteur
##
##  Valeur
##
##  Somme des deux vecteurs
##
##  Exemples
##
##  foo(2, 3)
##  foo(1:5)
##
foo <- function(x, y = 2)
    x + y
")
VALID_DOC_FR <- getSourceData(VALID_DOC_FR_FILE)

## Tests for valid French documentation
stopifnot(exprs = {
    any_comments(VALID_DOC_FR)
    any_doc(VALID_DOC_FR)
    signature_doc(VALID_DOC_FR)
    section_doc(VALID_DOC_FR, "Arguments")
    section_doc(VALID_DOC_FR, "ARGUMENTS", ignore.case = TRUE)
    section_doc(VALID_DOC_FR, "Valeur")
    section_doc(VALID_DOC_FR, "valeur", ignore.case = TRUE)
    section_doc(VALID_DOC_FR, "Exemples")
    section_doc(VALID_DOC_FR, "exemples", ignore.case = TRUE)
    formals_doc(VALID_DOC_FR)
})

###
### Specific test for the presence of valid comments
###
VALID_COMMENTS_FILE <- tempfile(fileext = ".R")
cat(file = VALID_COMMENTS_FILE, "
# comment
#comment
2 + 3 # comment
42    #*42
a     ###            a
")
VALID_COMMENTS <- getSourceData(VALID_COMMENTS_FILE)

stopifnot(exprs = {
    any_comments(VALID_COMMENTS)
})

###
### Tests for invalid documentation
###

## Documentation in English
INVALID_DOC_EN_FILE <- tempfile(fileext = ".R")
cat(file = INVALID_DOC_EN_FILE, "
###
### <signature missing>
###
##
##  Args <wrong title>
##
##  x: a vector
##  <second argument missing>
##
##  Vallue <wrong title>
##
##  Sum of the two vectors.
##
##  <Missing section title>
##
##  foo(2, 3)
##  foo(1:5)
##
foo <- function(x, y = 2)
    x + y

## <no documentation for 'bar'>
bar <- function(z) z^2
")
INVALID_DOC_EN <- getSourceData(INVALID_DOC_EN_FILE)

## Target attributes of errors
INVALID_SIGNATURE_EN <- c("foo", "bar")
INVALID_ARGUMENTS_SECTION_EN <- 0L
INVALID_VALUE_SECTION_EN <- 0L
INVALID_EXAMPLES_SECTION_EN <- 0L
INVALID_FORMALS_EN <- c("y", "z")

## Results for invalid English documentation
res.signature_doc_EN <- suppressMessages(signature_doc(INVALID_DOC_EN))
res.arguments_section_doc_EN <- suppressMessages(arguments_section_doc(INVALID_DOC_EN))
res.section_doc_arguments_EN <- suppressMessages(section_doc(INVALID_DOC_EN, "ARGUMENTS", ignore.case = TRUE))
res.value_section_doc_EN <- suppressMessages(value_section_doc(INVALID_DOC_EN))
res.section_doc_value_EN <- suppressMessages(section_doc(INVALID_DOC_EN, "value", ignore.case = TRUE))
res.examples_section_doc_EN <- suppressMessages(examples_section_doc(INVALID_DOC_EN))
res.section_doc_examples_EN <- suppressMessages(section_doc(INVALID_DOC_EN, "examples", ignore.case = TRUE))
res.formals_doc_EN <- suppressMessages(formals_doc(INVALID_DOC_EN))

## Tests for invalid English documentation
stopifnot(exprs = {
    isFALSE(res.signature_doc_EN)
    isFALSE(res.arguments_section_doc_EN)
    isFALSE(res.section_doc_arguments_EN)
    isFALSE(res.value_section_doc_EN)
    isFALSE(res.section_doc_value_EN)
    isFALSE(res.examples_section_doc_EN)
    isFALSE(res.section_doc_examples_EN)
    isFALSE(res.formals_doc_EN)
})

## Tests for the attributes of errors
stopifnot(exprs = {
    identical(INVALID_SIGNATURE_EN,
              attr(res.signature_doc_EN, "fun"))
    identical(INVALID_ARGUMENTS_SECTION_EN,
              attr(res.arguments_section_doc_EN, "sections"))
    identical(INVALID_ARGUMENTS_SECTION_EN,
              attr(res.section_doc_arguments_EN, "sections"))
    identical(INVALID_VALUE_SECTION_EN,
              attr(res.value_section_doc_EN, "sections"))
    identical(INVALID_VALUE_SECTION_EN,
              attr(res.section_doc_value_EN, "sections"))
    identical(INVALID_EXAMPLES_SECTION_EN,
              attr(res.examples_section_doc_EN, "sections"))
    identical(INVALID_EXAMPLES_SECTION_EN,
              attr(res.section_doc_examples_EN, "sections"))
    identical(INVALID_FORMALS_EN,
              attr(res.formals_doc_EN, "formals"))
})

## Documentation in French
INVALID_DOC_FR_FILE <- tempfile(fileext = ".R")
cat(file = INVALID_DOC_FR_FILE, "
###
### <signature manquante>
###
##
##  Args <titre erroné>
##
##  x: un vecteur
##  <second argument manquant>
##
##  Valleur <titre erroné>
##
##  Somme de deux vecteurs.
##
##  <Titre de section manquant>
##
##  foo(2, 3)
##  foo(1:5)
##
foo <- function(x, y = 2)
    x + y
")
INVALID_DOC_FR <- getSourceData(INVALID_DOC_FR_FILE)

## Target attribute of errors
INVALID_SIGNATURE_FR <- c("foo")
INVALID_ARGUMENTS_SECTION_FR <- 0L
INVALID_VALUE_SECTION_FR <- 0L
INVALID_EXAMPLES_SECTION_FR <- 0L
INVALID_FORMALS_FR <- "y"

## Results for invalid French documentation
res.signature_doc_FR <- suppressMessages(signature_doc(INVALID_DOC_FR))
res.arguments_section_doc_FR <- suppressMessages(arguments_section_doc(INVALID_DOC_FR))
res.section_doc_arguments_FR <- suppressMessages(section_doc(INVALID_DOC_FR, "ARGUMENTS", ignore.case = TRUE))
res.value_section_doc_FR <- suppressMessages(value_section_doc(INVALID_DOC_FR))
res.section_doc_value_FR <- suppressMessages(section_doc(INVALID_DOC_FR, "value", ignore.case = TRUE))
res.examples_section_doc_FR <- suppressMessages(examples_section_doc(INVALID_DOC_FR))
res.section_doc_examples_FR <- suppressMessages(section_doc(INVALID_DOC_FR, "examples", ignore.case = TRUE))
res.formals_doc_FR <- suppressMessages(formals_doc(INVALID_DOC_FR))

## Tests for invalid French documentation
stopifnot(exprs = {
    isFALSE(res.signature_doc_FR)
    isFALSE(res.arguments_section_doc_FR)
    isFALSE(res.section_doc_arguments_FR)
    isFALSE(res.value_section_doc_FR)
    isFALSE(res.section_doc_value_FR)
    isFALSE(res.examples_section_doc_FR)
    isFALSE(res.section_doc_examples_FR)
    isFALSE(res.formals_doc_FR)
})

## Tests for the attribute of errors
stopifnot(exprs = {
    identical(INVALID_SIGNATURE_FR,
              attr(res.signature_doc_FR, "fun"))
    identical(INVALID_ARGUMENTS_SECTION_FR,
              attr(res.arguments_section_doc_FR, "sections"))
    identical(INVALID_ARGUMENTS_SECTION_FR,
              attr(res.section_doc_arguments_FR, "sections"))
    identical(INVALID_VALUE_SECTION_FR,
              attr(res.value_section_doc_FR, "sections"))
    identical(INVALID_VALUE_SECTION_FR,
              attr(res.section_doc_value_FR, "sections"))
    identical(INVALID_EXAMPLES_SECTION_FR,
              attr(res.examples_section_doc_FR, "sections"))
    identical(INVALID_EXAMPLES_SECTION_FR,
              attr(res.section_doc_examples_FR, "sections"))
    identical(INVALID_FORMALS_FR,
              attr(res.formals_doc_FR, "formals"))
})

###
### Test for no proper comments
### (there are spaces at the end of lines 2 and 3)
###
NO_COMMENTS_FILE_1 <- tempfile(fileext = ".R")
cat(file = NO_COMMENTS_FILE_1, "
##
foo <- function(x, y = 2) ##  
    x + y #   
")
NO_COMMENTS_1 <- getSourceData(NO_COMMENTS_FILE_1)

NO_COMMENTS_FILE_2 <- tempfile(fileext = ".R")
cat(file = NO_COMMENTS_FILE_2, "
foo <- function(x, y = 2)
    x + y
")
NO_COMMENTS_2 <- getSourceData(NO_COMMENTS_FILE_2)

## Target attribute of error
INVALID_ANY_COMMENTS_1 <- 4L
INVALID_ANY_COMMENTS_2 <- 3L

## Results for invalid comments
res.any_comments.1 <- suppressMessages(
    any_comments(NO_COMMENTS_1)
                                       )
res.any_comments.2 <- suppressMessages(any_comments(NO_COMMENTS_2))

## Tests for the invalid comments
stopifnot(exprs = {
    isFALSE(res.any_comments.1)
    isFALSE(res.any_comments.2)
})

## Tests for the attribute of errors
stopifnot(exprs = {
    identical(INVALID_ANY_COMMENTS_1,
              attr(res.any_comments.1, "nlines"))
    identical(INVALID_ANY_COMMENTS_2,
              attr(res.any_comments.2, "nlines"))
})

## Local Variables:
## ess-nuke-trailing-whitespace-p: nil
## End:

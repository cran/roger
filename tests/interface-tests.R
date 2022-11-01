### roger: Automated grading of R scripts
###
### Tests for the validity of the interface functions.
###
### AUTHOR: Vincent Goulet <vincent.goulet@act.ulaval.ca>
### LICENSE: GPL 2 or later.

library(roger)

###
### Tests for roger_checkreq
###
stopifnot(exprs = {
    identical(roger_checkreq(.debugOnly = TRUE),
              r"(roger checkreq)")
    identical(roger_checkreq("~/foobar", .debugOnly = TRUE),
              paste0("roger checkreq --file=",
                     shQuote(paste0(path.expand("~"), "/foobar"))))
    identical(roger_checkreq("foo bar", .debugOnly = TRUE),
              paste0("roger checkreq --file=",
                     shQuote("foo bar")))
})

###
### Tests for roger_clone
###
stopifnot(exprs = {
    identical(roger_clone("[0-9]{9}_[Ll]ab", "project", .debugOnly = TRUE),
              paste0("roger clone ",
                     "-- ", shQuote("[0-9]{9}_[Ll]ab"), " ",
                     shQuote("project")))
    identical(roger_clone(pat = "[0-9]{9}_[Ll]ab", "project", quiet = TRUE,
                          .debugOnly = TRUE),
              paste0("roger clone --quiet ",
                     "-- ", shQuote("[0-9]{9}_[Ll]ab"), " ",
                     shQuote("project")))
    identical(roger_clone("project", pattern = "[0-9]{9} [Ll]ab",
                          api = "gitlab", .debugOnly = TRUE),
              paste0("roger clone --api=",
                     shQuote("gitlab"), " ",
                     "-- ", shQuote("[0-9]{9} [Ll]ab"), " ",
                     shQuote("project")))
    identical(roger_clone(pattern = ".*", machine = "x.y.z", pro = "project",
                          .debugOnly = TRUE),
              paste0("roger clone --machine=",
                     shQuote("x.y.z"), " ",
                     "-- ", shQuote(".*"), " ",
                     shQuote("project")))
    identical(roger_clone(pattern = ".*", machine = "x.y.z", pro = "project",
                    rogerrc_file = "~/.foo", .debugOnly = TRUE),
              paste0("roger clone --rogerrc-file=",
                     shQuote(path.expand("~/.foo")), " --machine=",
                     shQuote("x.y.z"), " ",
                     "-- ", shQuote(".*"), " ",
                     shQuote("project")))
    identical(roger_clone(pro = "project", "[0-9]{9}_[Ll]ab",
                          curl_options = c("-a", "-b"), .debugOnly = TRUE),
              paste0("roger clone -a -b ",
                     "-- ", shQuote("[0-9]{9}_[Ll]ab"), " ",
                     shQuote("project")))
})

###
### Tests for roger_grade
###
stopifnot(exprs = {
    identical(roger_grade(.debugOnly = TRUE),
              paste0("roger grade ",
                     shQuote(getwd())))
    identical(roger_grade(quiet = TRUE, .debugOnly = TRUE),
              paste0("roger grade --quiet ",
                     shQuote(getwd())))
    identical(roger_grade("[0-9]*/", conf = "gradeconf-lab", .debugOnly = TRUE),
              paste0("roger grade --config-file=",
                     shQuote("gradeconf-lab"), " ",
                     shQuote("[0-9]*/")))
    identical(roger_grade("[0-9]*/", time_limit = "2001-05-08 18:00:00",
                          output = "grading.txt", .debugOnly = TRUE),
              paste0("roger grade --time-limit=",
                     shQuote("2001-05-08 18:00:00"), " --output-file=",
                     shQuote("grading.txt"), " ",
                     shQuote("[0-9]*/")))
    identical(roger_grade(detached_head = TRUE, .debugOnly = TRUE),
              paste0("roger grade --detached-head ",
                     shQuote(getwd())))
    identical(roger_grade("[0-9]*/", detached_head = TRUE,
                          time_limit = "2042-10-11 23:59:59",
                          output_file = "CORRECTION.txt",
                          .debugOnly = TRUE),
              paste0("roger grade --time-limit=",
                     shQuote("2042-10-11 23:59:59"), " --output-file=",
                     shQuote("CORRECTION.txt"), " --detached-head ",
                     shQuote("[0-9]*/")))
})

###
### Tests for roger_push
###
stopifnot(exprs = {
    identical(roger_push("grading",
                   .debugOnly = TRUE),
              paste0("roger push ",
                     shQuote("grading"), " ",
                     shQuote(getwd())))
    identical(roger_push("[0-9]*/", branch = "grading",
                   .debugOnly = TRUE),
              paste0("roger push ",
                     shQuote("grading"), " ",
                     shQuote("[0-9]*/")))
    identical(roger_push("[0-9]*/", branch = "grading", create = TRUE,
                         .debugOnly = TRUE),
              paste0("roger push --create ",
                     shQuote("grading"), " ",
                     shQuote("[0-9]*/")))
    identical(roger_push("[0-9]*/", branch = "grading", "GRADING.txt",
                         create = TRUE, .debugOnly = TRUE),
              paste0("roger push --create --file=",
                     shQuote("GRADING.txt"), " ",
                     shQuote("grading"), " ",
                     shQuote("[0-9]*/")))
    identical(roger_push("[0-9]*/", branch = "grading", file = "GRADING.txt",
                         create = TRUE, add_file = "foo.txt",
                         .debugOnly = TRUE),
              paste0("roger push --create --file=",
                     shQuote("GRADING.txt"), " --add-file=",
                     shQuote("foo.txt"), " ",
                     shQuote("grading"), " ",
                     shQuote("[0-9]*/")))
    identical(roger_push("[0-9]*/", branch = "grading", file = "GRADING.txt",
                         create = TRUE, add_file = c("foo", "bar"),
                         .debugOnly = TRUE),
              paste0("roger push --create --file=",
                     shQuote("GRADING.txt"), " --add-file=",
                     shQuote("foo"), " --add-file=",
                     shQuote("bar"), " ",
                     shQuote("grading"), " ",
                     shQuote("[0-9]*/")))
    identical(roger_push("[0-9]*/", branch = "grading",
                         message = "foo", .debugOnly = TRUE),
              paste0("roger push --message=",
                     shQuote("foo"), " ",
                     shQuote("grading"), " ",
                     shQuote("[0-9]*/")))
    identical(roger_push("[0-9]*/", branch = "grading",
                         message = c("foo", "bar"), .debugOnly = TRUE),
              paste0("roger push --message=",
                     shQuote("foo"), " --message=",
                     shQuote("bar"), " ",
                     shQuote("grading"), " ",
                     shQuote("[0-9]*/")))
})

###
### Tests for roger_switch
###
stopifnot(exprs = {
    identical(roger_switch("main", .debugOnly = TRUE),
              paste0("roger switch ",
                     shQuote("main"), " ",
                     shQuote(getwd())))
    identical(roger_switch("main", "foobar", .debugOnly = TRUE),
              paste0("roger switch ",
                     shQuote("main"), " ",
                     shQuote("foobar")))
    identical(roger_switch(repos = "foobar", "main", TRUE,
                     .debugOnly = TRUE),
              paste0("roger switch --quiet ",
                     shQuote("main"), " ",
                     shQuote("foobar")))
    identical(roger_switch("main", quiet = TRUE, "foobar",
                           .debugOnly = TRUE),
              paste0("roger switch --quiet ",
                     shQuote("main"), " ",
                     shQuote("foobar")))
})

###
### Tests for roger_validate
###
stopifnot(exprs = {
    identical(roger_validate(.debugOnly = TRUE),
              paste0("roger validate ",
                     shQuote(getwd())))
    identical(roger_validate(config_file = "validateconf-foo",
                             .debugOnly = TRUE),
              paste0("roger validate --config-file=",
                     shQuote("validateconf-foo"), " ",
                     shQuote(getwd())))
    identical(roger_validate("foo", check = FALSE,
                             .debugOnly = TRUE),
              paste0("roger validate --no-check-local-repos ",
                     shQuote("foo")))
})

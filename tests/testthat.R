library(testthat)
library(soundscapeR)

Sys.setenv("TESTTHAT_MAX_FAILS" = 10)

my_reporter <- testthat::ProgressReporter$new(max_failures = 10)
testthat::set_reporter(reporter = my_reporter)
testthat::test_package(package = "soundscapeR", reporter = my_reporter)

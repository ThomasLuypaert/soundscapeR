library(testthat)
library(soundscapeR)

# 1. Load the merged '.csv' data frame files,
# binarize the data frame, aggregate the data frame, and make
#wrong data frame types for testing purposes

fpath_CVR_case_study <- system.file("extdata/case_study/merged_CVR_256_case_study.ssc",
                         package="soundscapeR")

merged_soundscape_CVR_case_study <- qs::qread(file = fpath_CVR_case_study)

for (i in 1:length(merged_soundscape_CVR_case_study)){

  merged_soundscape_CVR_case_study[[i]]@fileloc <- substr(fpath_CVR_case_study, 0, nchar(fpath_CVR_case_study)-26)

}

binarized_soundscape_CVR_case_study <- lapply(merged_soundscape_CVR_case_study,
                                              function(x) ss_binarize(merged_soundscape = x,
                                                   method = "IsoData",
                                                   value = NULL))

aggregated_soundscape_CVR_case_study <- lapply(binarized_soundscape_CVR_case_study,
                                               function(x) ss_aggregate(binarized_soundscape = x,
                                                     output = "incidence_freq"))

aggregated_soundscape_CVR_A <- aggregated_soundscape_CVR_case_study[[1]]
aggregated_soundscape_CVR_B <- aggregated_soundscape_CVR_case_study[[2]]



# 2. Start testing the ss_diversity_plot function

# 2.0. If required arguments are missing

  # aggregated_soundscape_A & aggregated_soundscape_B

testthat::test_that("the ss_compare function provides the correct error when the aggregated_soundscape_A argument is missing", {

  testthat::expect_error(
    object = soundscapeR::ss_compare(aggregated_soundscape_B = aggregated_soundscape_CVR_B),
    regexp = "aggregated_soundscape_A argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

testthat::test_that("the ss_compare function provides the correct error when the aggregated_soundscape_B argument is missing", {

  testthat::expect_error(
    object = soundscapeR::ss_compare(aggregated_soundscape_A = aggregated_soundscape_CVR_A),
    regexp = "aggregated_soundscape_B argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

# 2.1. When the correct arguments are supplied

  # type = 'regular'

testthat::test_that("The ss_compare function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create ss_compare",
    fig = ss_compare(aggregated_soundscape_A = aggregated_soundscape_CVR_A,
                     aggregated_soundscape_B = aggregated_soundscape_CVR_B),
  )
})

  # type = 'polar'

testthat::test_that("The ss_compare function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create ss_compare with type = 'polar'",
    fig = ss_compare(aggregated_soundscape_A = aggregated_soundscape_CVR_A,
                     aggregated_soundscape_B = aggregated_soundscape_CVR_B,
                     type = "polar",
                     mintime = "06:00:00",
                     maxtime = "06:05:00",
                     minfreq = 0,
                     maxfreq = 500),
  )
})

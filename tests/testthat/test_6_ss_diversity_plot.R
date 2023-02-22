library(testthat)
library(soundscapeR)

# 1. Load the merged '.csv' data frame files,
# binarize the data frame, aggregate the data frame, and make
#wrong data frame types for testing purposes

fpath_CVR <- system.file("extdata/ss_binarize_test/merged_soundscape_CVR.ssc",
                         package="soundscapeR")

fpath_test <- system.file("extdata/test_data",
                          package="soundscapeR")

merged_soundscape_CVR <- qs::qread(file = fpath_CVR)
merged_soundscape_CVR@fileloc <- substr(fpath_CVR, 0, nchar(fpath_CVR)-26)

binarized_soundscape_CVR <- ss_binarize(merged_soundscape = merged_soundscape_CVR,
                                        method = "IsoData",
                                        value = NULL)

aggregated_soundscape_CVR <- ss_aggregate(binarized_soundscape = binarized_soundscape_CVR,
                                          output = "incidence_freq")

# 2. Start testing the ss_diversity_plot function

  # 2.0. If required arguments are missing

testthat::test_that("the ss_diversity_plot function provides the correct error when the aggregated_soundscape argument is missing", {

  testthat::expect_error(
    object = ss_diversity_plot(qvalue = 1),
    regexp = "aggregated_soundscape argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

testthat::test_that("the ss_diversity_plot function provides the correct error when the qvalue argument is missing", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR),
    regexp = "qvalue argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

# 2.1. When the correct arguments are supplied

  # graphtype = "total"

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create ss_diversity_plot with graphtype = 'total'",
    fig = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR,
                            qvalue = 0,
                            graphtype = "total"),
  )
})

  # graphtype = "frequency"

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create ss_diversity_plot with graphtype = 'frequency'",
    fig = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR,
                            qvalue = 0,
                            graphtype = "frequency"),
  )
})

  # graphtype = "normfreq"

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create ss_diversity_plot with graphtype = 'normfreq'",
    fig = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR,
                            qvalue = 0,
                            graphtype = "normfreq"),
  )
})

  # graphtype = "linefreq"

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create ss_diversity_plot with graphtype = 'linefreq'",
    fig = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR,
                            qvalue = 0,
                            graphtype = "linefreq"),
  )
})


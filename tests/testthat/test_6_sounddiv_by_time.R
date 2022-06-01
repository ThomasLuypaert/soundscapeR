library(testthat)
library(soundscapeR)

# 1. Load the merged '.csv' data frame files,
# binarize the data frame, aggregate the data frame, and make
#wrong data frame types for testing purposes

fpath_CVR <- system.file("/extdata/merged_soundscape/merged_soundscape_CVR.ssc",
                         package="soundscapeR")

fpath_test <- system.file("/extdata/test_data/sounddiv_test_tod.csv",
                          package="soundscapeR")

merged_soundscape_CVR <- qs::qread(file = fpath_CVR)
merged_soundscape_CVR@fileloc <- substr(fpath_CVR, 0, nchar(fpath_CVR)-26)

binarized_soundscape_CVR <- binarize_df(merged_soundscape = merged_soundscape_CVR,
                                        method = "Otsu",
                                        value = NULL)

aggregated_soundscape_CVR <- aggregate_df(binarized_soundscape = binarized_soundscape_CVR,
                                          output = "incidence_freq")

# 2. Start testing the sounddiv_by_time function

  # 2.0. If required arguments are missing

testthat::test_that("the sounddiv function provides the correct error when the aggregated_soundscpae argument is missing", {

  testthat::expect_error(
    object = sounddiv_by_time(qvalue = 1),
    regexp = "aggregated_soundscape argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

testthat::test_that("the sounddiv function provides the correct error when the qvalue argument is missing", {

  testthat::expect_error(
    object = sounddiv_by_time(aggregated_soundscape = aggregated_soundscape_CVR),
    regexp = "qvalue argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

  # 2.1. If the correct arguments are supplied

    # 2.1.1. graphtype = total, smooth = TRUE and no frequency subsetting

testthat::test_that("the sounddiv_by_time function works as expected when the correct arguments are supplied", {

  plot <- sounddiv_by_time(aggregated_soundscape = aggregated_soundscape_CVR,
                           qvalue = 0,
                           graphtype = "total",
                           smooth = TRUE)

  vdiffr::expect_doppelganger("sounddiv_by_time_total_smooth_nosub", plot)

})

    # 2.1.2. graphtype = total, smooth = FALSE and no frequency subsetting

testthat::test_that("the sounddiv_by_time function works as expected when the correct arguments are supplied", {

  plot <- sounddiv_by_time(aggregated_soundscape = aggregated_soundscape_CVR,
                           qvalue = 0,
                           graphtype = "total",
                           smooth = FALSE)

  vdiffr::expect_doppelganger("sounddiv_by_time_total_nosmooth_nosub", plot)

})

    # 2.1.3. graphtype = 'frequency', smooth = TRUE and no frequency subsetting

testthat::test_that("the sounddiv_by_time function works as expected when the correct arguments are supplied", {

  plot <- sounddiv_by_time(aggregated_soundscape = aggregated_soundscape_CVR,
                           qvalue = 0,
                           graphtype = "frequency",
                           smooth = TRUE)

  vdiffr::expect_doppelganger("sounddiv_by_time_freq_smooth_nosub", plot)

})

    # 2.1.4. graphtype = 'frequency', smooth = FALSE and no frequency subsetting

testthat::test_that("the sounddiv_by_time function works as expected when the correct arguments are supplied", {

  plot <- sounddiv_by_time(aggregated_soundscape = aggregated_soundscape_CVR,
                           qvalue = 0,
                           graphtype = "frequency",
                           smooth = FALSE)

  vdiffr::expect_doppelganger("sounddiv_by_time_freq_nosmooth_nosub", plot)

})

    # 2.1.5. graphtype = 'normfreq', smooth = TRUE and no frequency subsetting

testthat::test_that("the sounddiv_by_time function works as expected when the correct arguments are supplied", {

  plot <- sounddiv_by_time(aggregated_soundscape = aggregated_soundscape_CVR,
                           qvalue = 0,
                           graphtype = "normfreq",
                           smooth = TRUE)

  vdiffr::expect_doppelganger("sounddiv_by_time_normfreq_smooth_nosub", plot)

})

    # 2.1.6. graphtype = 'normfreq', smooth = FALSE and no frequency subsetting

testthat::test_that("the sounddiv_by_time function works as expected when the correct arguments are supplied", {

  plot <- sounddiv_by_time(aggregated_soundscape = aggregated_soundscape_CVR,
                           qvalue = 0,
                           graphtype = "normfreq",
                           smooth = FALSE)

  vdiffr::expect_doppelganger("sounddiv_by_time_normfreq_nosmooth_nosub", plot)

})

    # 2.1.7. graphtype = 'linefreq', smooth = TRUE and no frequency subsetting

testthat::test_that("the sounddiv_by_time function works as expected when the correct arguments are supplied", {

  plot <- sounddiv_by_time(aggregated_soundscape = aggregated_soundscape_CVR,
                           qvalue = 0,
                           graphtype = "linefreq",
                           smooth = TRUE)

  vdiffr::expect_doppelganger("sounddiv_by_time_linefreq_smooth_nosub", plot)

})

    # 2.1.8. graphtype = 'linefreq', smooth = FALSE and no frequency subsetting

testthat::test_that("the sounddiv_by_time function works as expected when the correct arguments are supplied", {

  plot <- sounddiv_by_time(aggregated_soundscape = aggregated_soundscape_CVR,
                           qvalue = 0,
                           graphtype = "linefreq",
                           smooth = FALSE)

  vdiffr::expect_doppelganger("sounddiv_by_time_linefreq_nosmooth_nosub", plot)

})

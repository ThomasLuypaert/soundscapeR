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

  # 2.2. Correct error message when some of the supplied arguments are incorrect

    # 2.2.1. The aggregated_soundscape argument is not an S4 object of the type 'soundscape'

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape argument is not an S4-object of the type 'soundscape' ", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR@merged_df,
                               qvalue = 0),
    regexp = "aggregated_soundscape is not an S4-object of the type 'soundscape'. Please supply the aggregated_soundscape object produced by the ss_aggregate() or ss_create() functions. Consult the package documentation for further information.",
    fixed=TRUE
  )

})

  # 2.2.2. Some of the aggregated_soundscape elements are incorrect

    # Wrong lat and long coordinates

aggregated_soundscape_CVR_coord_1 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_coord_2 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_coord_3 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_coord_4 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_coord_5 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_coord_6 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_coord_1@lat <- 91
aggregated_soundscape_CVR_coord_2@lat <- -91
aggregated_soundscape_CVR_coord_3@lon <- 181
aggregated_soundscape_CVR_coord_4@lon <- -181
aggregated_soundscape_CVR_coord_5@lat <- 91
aggregated_soundscape_CVR_coord_5@lon <- 181
aggregated_soundscape_CVR_coord_6@lat <- -91
aggregated_soundscape_CVR_coord_6@lon <- -181

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_coord_1,
                               qvalue = 0),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_coord_2,
                               qvalue = 0),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_coord_3,
                               qvalue = 0),
    regexp = "aggregated_soundscape@lon is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_coord_4,
                               qvalue = 0),
    regexp = "aggregated_soundscape@lon is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_coord_5,
                               qvalue = 0),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_coord_6,
                               qvalue = 0),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

# Wrong time zone argument

aggregated_soundscape_CVR_tz <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_tz@tz <- "Emarica/Manaus"

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape tz argument is wrong", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_tz,
                               qvalue = 0),
    regexp = "aggregated_soundscape@tz is not a recognized timezone. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed=TRUE
  )

})

# Wrong index argument

aggregated_soundscape_CVR_index <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_index@index <- "I'm not an option!"

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape index argument is wrong", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_index,
                               qvalue = 0),
    regexp = "aggregated_soundscape@index is not a character string of one of the available index options. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed=TRUE
  )

})

# Wrong samplerate and window arguments

aggregated_soundscape_CVR_index_samplerate1 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_index_samplerate2 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_index_samplerate1@samplerate <- -44100
aggregated_soundscape_CVR_index_samplerate2@samplerate <- c(44100, 44200)

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape samplerate argument is wrong", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_index_samplerate1,
                               qvalue = 0),
    regexp = "aggregated_soundscape@samplerate is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape samplerate argument is wrong", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_index_samplerate2,
                               qvalue = 0),
    regexp = "aggregated_soundscape@samplerate is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

aggregated_soundscape_CVR_index_window1 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_index_window2 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_index_window1@window <- -256
aggregated_soundscape_CVR_index_window2@window <- c(256, 512)

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape window argument is wrong", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_index_window1,
                               qvalue = 0),
    regexp = "aggregated_soundscape@window is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape window argument is wrong", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_index_window2,
                               qvalue = 0),
    regexp = "aggregated_soundscape@window is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

# Wrong binarization method

aggregated_soundscape_CVR_binmeth <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_binmeth@binarization_method <- "I'm not an option!"

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape binarization method argument is wrong", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_binmeth,
                               qvalue = 0),
    regexp = "aggregated_soundscape@binarization_method is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

# Wrong threshold

aggregated_soundscape_CVR_thresh <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_thresh@threshold <- c(1.5, 1.6)

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape threshold argument is wrong", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_thresh,
                               qvalue =0),
    regexp = "aggregated_soundscape@threshold is not a single numeric value. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed=TRUE
  )

})


# Wrong output

aggregated_soundscape_CVR_output <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_output@output <- "I'm not an option"

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape output argument is wrong", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_output,
                               qvalue = 0),
    regexp = "aggregated_soundscape@output is not a character string describing one of the available output options. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed=TRUE
  )

})


# Wrong merged_df

aggregated_soundscape_CVR_df_1 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_df_2 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_df_3 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_df_4 <- aggregated_soundscape_CVR

aggregated_soundscape_CVR_df_1@merged_df <- aggregated_soundscape_CVR_df_1@merged_df[FALSE,]
aggregated_soundscape_CVR_df_2@merged_df[1,1] <- NA
aggregated_soundscape_CVR_df_3@merged_df[1,1] <- "I'm not numeric"
rownames(aggregated_soundscape_CVR_df_4@merged_df) <-
  seq(1,nrow(aggregated_soundscape_CVR_df_4@merged_df), 1)

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregate_soundscape merged_df argument is empty", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_df_1,
                               qvalue = 0),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregate_soundscape merged_df argument contains NA values", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_df_2,
                               qvalue = 0),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregate_soundscape merged_df argument contains non-numeric values", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_df_3,
                               qvalue = 0),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregate_soundscape merged_df argument has incorrect row names", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_df_4,
                               qvalue = 0),
    regexp = "aggregated_soundscape@merged_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed=TRUE
  )

})

# Wrong binarized_df

aggregated_soundscape_CVR_bindf1 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_bindf2 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_bindf3 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_bindf4 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_bindf5 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_bindf6 <- aggregated_soundscape_CVR

aggregated_soundscape_CVR_bindf1@binarized_df <- aggregated_soundscape_CVR_bindf1@binarized_df[FALSE,]
aggregated_soundscape_CVR_bindf2@binarized_df[1,1] <- NA
aggregated_soundscape_CVR_bindf3@binarized_df[1,1] <- "I'm not numeric"
rownames(aggregated_soundscape_CVR_bindf4@binarized_df) <-
  seq(1,nrow(aggregated_soundscape_CVR_bindf4@binarized_df), 1)
aggregated_soundscape_CVR_bindf5@binarized_df[1,1] <- 25

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape binarized_df argument is empty", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_bindf1,
                               qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape binarized_df argument contains NA values", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_bindf2,
                               qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape binarized_df argument contains non-numeric values", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_bindf3,
                               qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape binarized_df argument has incorrect row names", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_bindf4,
                               qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape binarized_df argument is non-binary", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_bindf5,
                               qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

# Wrong aggregated_df

aggregated_soundscape_CVR_aggdf1 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_aggdf2 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_aggdf3 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_aggdf4 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_aggdf5 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_aggdf6 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_aggdf7 <- aggregated_soundscape_CVR

aggregated_soundscape_CVR_aggdf1@aggregated_df <- aggregated_soundscape_CVR_aggdf1@aggregated_df[FALSE,]
aggregated_soundscape_CVR_aggdf2@aggregated_df[1,1] <- NA
aggregated_soundscape_CVR_aggdf3@aggregated_df[1,1] <- "I'm not numeric"
rownames(aggregated_soundscape_CVR_aggdf4@aggregated_df) <-
  seq(1,nrow(aggregated_soundscape_CVR_aggdf4@aggregated_df), 1)

aggregated_soundscape_CVR_aggdf6@aggregated_df[1,1] <- 25
aggregated_soundscape_CVR_aggdf7@aggregated_df[1,1] <- 500
aggregated_soundscape_CVR_aggdf7@output <- "raw"

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape aggregated_df argument is empty", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_aggdf1,
                               qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape aggregated_df argument contains NA values", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_aggdf2,
                               qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape aggregated_df argument contains non-numeric values", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_aggdf3,
                               qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape aggregated_df argument has incorrect row names", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_aggdf4,
                               qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape output argument is 'incidence_freq' but the data frame contains values larger than one.", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_aggdf6,
                               qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the aggregated_soundscape argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregated_soundscape output argument is 'raw' but the data frame contains values larger than the number of sampling days", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape = aggregated_soundscape_CVR_aggdf7,
                               qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the aggregated_soundscape argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed=TRUE
  )

})


# 2.4. The qvalues argument is wrong

testthat::test_that("the ss_diversity_plot function produces the correct error message when the qvalue argument is a character string", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = "0"),
    regexp = "qvalue is a character string of length 1. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the qvalue argument is a list", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = as.list(c(1, 2, 3))),
    regexp = "qvalue is a list. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the qvalue argument is a list", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = as.factor(1)),
    regexp = "qvalue is not an numeric/integer value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the qvalue argument is a list", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = -1),
    regexp = "qvalue is not a positive value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE
  )

})

# 2.5. The minfreq and maxfreq arguments are wrong

testthat::test_that("the ss_diversity_plot function produces the correct error message when the minfreq argument is a negative number", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          minfreq = -1),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the minfreq argument is larger than the upper frequency bound", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          minfreq = 50000),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the minfreq argument is not numeric", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          minfreq = "1"),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the maxfreq argument is a negative number", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          maxfreq = -1),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the maxfreq argument is larger than the upper frequency bound", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          maxfreq = 50000),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the maxfreq argument is not numeric", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          maxfreq = "1"),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the maxfreq argument is zero", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          maxfreq = 0),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the minfreq and maxfreq argument are not in the correct format", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          minfreq = -1,
                          maxfreq = 0),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})


# 2.9. The nbins argument is wrong

testthat::test_that("the ss_diversity_plot function produces the correct error message when the nbins argument is a single positive integer", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          nbins = -1),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the nbins argument is a character vector", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          nbins = "20"),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the nbins argument is zero", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          nbins = 0),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the nbins argument is larger than the number of rows in the data frame", {

  testthat::expect_error(
    object = ss_diversity_plot(aggregated_soundscape= aggregated_soundscape_CVR,
                          qvalue = 0,
                          nbins = 5000),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE
  )

})

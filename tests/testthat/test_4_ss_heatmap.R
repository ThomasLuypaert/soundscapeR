library(testthat)
library(soundscapeR)

# 1. Load the merged '.csv' data frame files,
# binarize the data frame and make wrong data frame types
# for testing purposes

fpath_CVR <- system.file("/extdata/ss_binarize_test/merged_soundscape_CVR.ssc",
                         package="soundscapeR")

merged_soundscape_CVR <- qs::qread(file = fpath_CVR)
merged_soundscape_CVR@fileloc <- substr(fpath_CVR, 0, nchar(fpath_CVR)-26)

binarized_soundscape_CVR <- ss_binarize(merged_soundscape = merged_soundscape_CVR,
                                        method = "IsoData",
                                        value = NULL)

aggregated_soundscape_CVR <- ss_aggregate(binarized_soundscape = binarized_soundscape_CVR,
                                          output = "incidence_freq")

# 2. Start testing the ss_heatmap() function

  # 2.0. When some of the required arguments are missing

testthat::test_that("the ss_heatmap function provides the correct error when the aggregated_soundscape argument is missing", {

  testthat::expect_error(
    object = ss_heatmap(),
    regexp = "aggregated_soundscape argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

  # 2.1. When the correct arguments are supplied

    # Regular heatmap and annotate == TRUE

testthat::test_that("The ss_heatmap function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create regular heatmap plot with annotation",
    fig = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "regular",
                     annotate = TRUE),
  )
})

    # Regular heatmap and annotate == FALSE

testthat::test_that("The ss_heatmap function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create regular heatmap plot without annotation",
    fig = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "regular",
                     annotate = FALSE),
  )
})

    # Polar heatmap and annotate == TRUE

testthat::test_that("The ss_heatmap function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create polar heatmap plot with annotation",
    fig = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "polar",
                     annotate = TRUE,
                     minfreq = 0,
                     maxfreq = 500),
  )
})

    # Polar heatmap and annotate == FALSE

testthat::test_that("The ss_heatmap function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create polar heatmap plot without annotation",
    fig = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "polar",
                     annotate = FALSE,
                     minfreq = 0,
                     maxfreq = 500),
  )
})

    # Subsetting in time and frequency domain

testthat::test_that("The ss_heatmap function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create regular heatmap plot with temporal and frequency subsetting",
    fig = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "regular",
                     annotate = FALSE,
                     mintime = "06:00:00",
                     maxtime = "18:00:00",
                     minfreq = 2000,
                     maxfreq = 20000),
  )
})

  # Modify the time and frequency axis labels

testthat::test_that("The ss_heatmap function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create regular heatmap plot with modified time-frequency axes",
    fig = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "regular",
                     annotate = FALSE,
                     timeinterval = "4 hours",
                     freqinterval = 5000),
  )
})

    # With zero.black == TRUE

testthat::test_that("The ss_heatmap function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create regular heatmap plot with a black background",
    fig = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "regular",
                     annotate = FALSE,
                     zero.black = TRUE),
  )
})

    # maxfreq > 22000

testthat::test_that("The ss_heatmap function works as expected when the correct arguments are supplied", {

  vdiffr::expect_doppelganger(
    title = "Create regular heatmap plot with maxfreq > 22000",
    fig = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "regular",
                     annotate = FALSE,
                     maxfreq = 22001),
  )
})


  # 2.2. Correct error message when some of the supplied arguments are incorrect

    #2.2.1. The aggregated_soundscape argument is not an S4 object of the type 'soundscape'

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape argument is not an S4-object of the type 'soundscape' ", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR@merged_df),
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

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_coord_1),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_coord_2),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_coord_3),
    regexp = "aggregated_soundscape@lon is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_coord_4),
    regexp = "aggregated_soundscape@lon is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_coord_5),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_coord_6),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

    # Wrong time zone argument

aggregated_soundscape_CVR_tz <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_tz@tz <- "Emarica/Manaus"

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape tz argument is wrong", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_tz),
    regexp = "aggregated_soundscape@tz is not a recognized timezone. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed=TRUE
  )

})

    # Wrong index argument

aggregated_soundscape_CVR_index <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_index@index <- "I'm not an option!"

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape index argument is wrong", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_index),
    regexp = "aggregated_soundscape@index is not a character string of one of the available index options. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed=TRUE
  )

})

    # Wrong samplerate and window arguments

aggregated_soundscape_CVR_index_samplerate1 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_index_samplerate2 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_index_samplerate1@samplerate <- -44100
aggregated_soundscape_CVR_index_samplerate2@samplerate <- c(44100, 44200)

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape samplerate argument is wrong", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_index_samplerate1),
    regexp = "aggregated_soundscape@samplerate is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape samplerate argument is wrong", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_index_samplerate2),
    regexp = "aggregated_soundscape@samplerate is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

aggregated_soundscape_CVR_index_window1 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_index_window2 <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_index_window1@window <- -256
aggregated_soundscape_CVR_index_window2@window <- c(256, 512)

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape window argument is wrong", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_index_window1),
    regexp = "aggregated_soundscape@window is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape window argument is wrong", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_index_window2),
    regexp = "aggregated_soundscape@window is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

    # Wrong binarization method

aggregated_soundscape_CVR_binmeth <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_binmeth@binarization_method <- "I'm not an option!"

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape binarization method argument is wrong", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_binmeth),
    regexp = "aggregated_soundscape@binarization_method is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

    # Wrong threshold

aggregated_soundscape_CVR_thresh <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_thresh@threshold <- c(1.5, 1.6)

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape threshold argument is wrong", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_thresh),
    regexp = "aggregated_soundscape@threshold is not a single numeric value. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed=TRUE
  )

})


    # Wrong output

aggregated_soundscape_CVR_output <- aggregated_soundscape_CVR
aggregated_soundscape_CVR_output@output <- "I'm not an option"

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape output argument is wrong", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_output),
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

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregate_soundscape merged_df argument is empty", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_df_1),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregate_soundscape merged_df argument contains NA values", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_df_2),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregate_soundscape merged_df argument contains non-numeric values", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_df_3),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregate_soundscape merged_df argument has incorrect row names", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_df_4),
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

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape binarized_df argument is empty", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_bindf1),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape binarized_df argument contains NA values", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_bindf2),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape binarized_df argument contains non-numeric values", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_bindf3),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape binarized_df argument has incorrect row names", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_bindf4),
    regexp = "aggregated_soundscape@binarized_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape binarized_df argument is non-binary", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_bindf5),
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

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape aggregated_df argument is empty", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_aggdf1),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape aggregated_df argument contains NA values", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_aggdf2),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape aggregated_df argument contains non-numeric values", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_aggdf3),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape aggregated_df argument has incorrect row names", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_aggdf4),
    regexp = "aggregated_soundscape@aggregated_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape output argument is 'incidence_freq' but the data frame contains values larger than one.", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_aggdf6),
    regexp = "aggregated_soundscape@aggregated_df contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the aggregated_soundscape argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the aggregated_soundscape output argument is 'raw' but the data frame contains values larger than the number of sampling days", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR_aggdf7),
    regexp = "aggregated_soundscape@aggregated_df contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the aggregated_soundscape argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed=TRUE
  )

})


  # 2.2.3. The type argument

testthat::test_that("the ss_heatmap function produces the correct error message when the type argument is not a character string", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        type = 25),
    regexp = "type is not a character string. Please supply the heatmap type as a character string. Consult package documentation for available type argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the type argument is not one of the available options", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        type = "I'm not an option"),
    regexp = "type is not one of the available heatmap type options. Please consult package documentation for available type argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

  # 2.2.4. The annotate argument

testthat::test_that("the ss_heatmap function produces the correct error message when the type argument is not a boolean flag", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        annotate = "I'm not a boolean flag"),
    regexp = "annotate is not a Boolean flag (TRUE or FALSE). Please set annotate argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed=TRUE
  )

})


  # 2.2.5. The timeinterval argument

testthat::test_that("the ss_heatmap function produces the correct error message when the timeinterval argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        timeinterval = "five seconds"),
    regexp = "timeinterval is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.",
    fixed=TRUE
  )

})

  # 2.2.6. The mintime and maxtime arguments

testthat::test_that("the ss_heatmap function produces the correct error message when the mintime or maxtime arguments are not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        mintime = "30:0:0"),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the mintime or maxtime arguments are not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        maxtime = "30:0:0"),
    regexp = "maxtime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE
  )

})


  # 2.2.7. The freqinterval argument

testthat::test_that("the ss_heatmap function produces the correct error message when the freqinterval argument is not in the correct format or exceeds the frequency limits", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        freqinterval = "I'm not an option"),
    regexp = "freqinterval is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the freqinterval argument is not in the correct format or exceeds the frequency limits", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        freqinterval = 30000),
    regexp = "freqinterval is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).",
    fixed=TRUE
  )

})

  # 2.2.8. The minfreq and maxfreq arguments

testthat::test_that("the ss_heatmap function produces the correct error message when the minfreq or maxfreq argument is not in the correct format or exceeds the frequency limits", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        minfreq = "I'm not an option"),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the minfreq or maxfreq argument is not in the correct format or exceeds the frequency limits", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        minfreq = 30000),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the minfreq or maxfreq argument is not in the correct format or exceeds the frequency limits", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        maxfreq = "I'm not an option"),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the minfreq or maxfreq argument is not in the correct format or exceeds the frequency limits", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        maxfreq = 30000),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})


  # 2.2.10. The labelsize argument

testthat::test_that("the ss_heatmap function produces the correct error message when the labelsize_time argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_time = "I'm not an option"),
    regexp = "labelsize_time is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the labelsize_time argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_time = -1),
    regexp = "labelsize_time is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the labelsize_time argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_time = c(1,1)),
    regexp = "labelsize_time is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the labelsize_frequency argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_frequency = "I'm not an option"),
    regexp = "labelsize_frequency is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the labelsize_frequency argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_frequency = -1),
    regexp = "labelsize_frequency is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the labelsize_frequency argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_frequency = c(1,1)),
    regexp = "labelsize_frequency is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the labelsize_polar argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_polar = "I'm not an option"),
    regexp = "labelsize_polar is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the labelsize_polar argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_polar = -1),
    regexp = "labelsize_polar is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the labelsize_polar argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_polar = c(1,1)),
    regexp = "labelsize_polar is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE
  )

})



  # 2.2.11. The palette and direction arguments

testthat::test_that("the ss_heatmap function produces the correct error message when the palette argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        palette = 2),
    regexp = "palette is not a valid palette argument. Palette needs to be supplied as a character string of either: 'A', 'B', 'C', 'D', 'E', 'magma', 'inferno', 'plasma', 'viridis' or 'cividis'. Please supply a valid palette argument. Consult the soundscapeR of viridis package documentation for more information.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the palette argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        palette = "I'm not an option"),
    regexp = "palette is not a valid palette argument. Palette needs to be supplied as a character string of either: 'A', 'B', 'C', 'D', 'E', 'magma', 'inferno', 'plasma', 'viridis' or 'cividis'. Please supply a valid palette argument. Consult the soundscapeR of viridis package documentation for more information.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the direction argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        direction = "I'm not an option"),
    regexp = "direction is not a valid direction argument. The direction argument needs to be supplied as a single integer of either 1 or -1. Please supply a valid direction argument. Consult the soundscapeR of viridis package documentation for more information.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the direction argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        direction = 5),
    regexp = "direction is not a valid direction argument. The direction argument needs to be supplied as a single integer of either 1 or -1. Please supply a valid direction argument. Consult the soundscapeR of viridis package documentation for more information.",
    fixed=TRUE
  )

})


  # 2.2.12. The boolean flag arguments (zero.black, interactive, save)

testthat::test_that("the ss_heatmap function produces the correct error message when the zero.black argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        zero.black = "FALSE"),
    regexp = "zero.black is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the interactive argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        interactive = "FALSE"),
    regexp = "interactive is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the save argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        save = "FALSE"),
    regexp = "save is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE
  )

})


testthat::test_that("the ss_heatmap function produces the correct error message when the zero.black argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        zero.black = 2),
    regexp = "zero.black is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the interactive argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        interactive = 2),
    regexp = "interactive is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the save argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        save = 2),
    regexp = "save is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE
  )

})

  # 2.2.14. The dir, filename and device arguments

testthat::test_that("the ss_heatmap function produces the correct error message when the dir argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        dir = 2),
    regexp = "dir is not a character string. The dir arguments needs to be a character string of either 'default' - or a valid pathname to an existing directory on your device. If you're working on a Windows operating system, pay attention to backslash and forwardslash.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the filename argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        filename = 27),
    regexp = "filename is not a valid filename argument. The filename argument needs to be a character string.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the filename argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        filename = "file.plg"),
    regexp = "filename is not a valid filename argument. Please make the filename argument you provide a character string without the extension.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the device argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        filename = "file.png",
                        device = 2),
    regexp = "device is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the device argument is not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        filename = "file.png",
                        device = "plg"),
    regexp = "device is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.",
    fixed=TRUE
  )

})


  # 2.2.15. The height and width arguments

testthat::test_that("the ss_heatmap function produces the correct error message when the height and width arguments are not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        filename = "file.png",
                        device = "png",
                        height = "three"),
    regexp = "height is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_heatmap function produces the correct error message when the height and width arguments are not in the correct format", {

  testthat::expect_error(
    object = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
                        filename = "file.png",
                        device = "png",
                        width = "three"),
    regexp = "width is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
    fixed=TRUE
  )

})






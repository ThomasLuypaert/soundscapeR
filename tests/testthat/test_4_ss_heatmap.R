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

# testthat::test_that("The ss_heatmap function works as expected when the correct arguments are supplied", {
#
#   vdiffr::expect_doppelganger(
#     title = "Create polar heatmap plot with annotation",
#     fig = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
#                      type = "polar",
#                      annotate = TRUE),
#   )
# })

    # Polar heatmap and annotate == FALSE

# testthat::test_that("The ss_heatmap function works as expected when the correct arguments are supplied", {
#
#   vdiffr::expect_doppelganger(
#     title = "Create polar heatmap plot without annotation",
#     fig = ss_heatmap(aggregated_soundscape = aggregated_soundscape_CVR,
#                      type = "polar",
#                      annotate = FALSE),
#   )
# })

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

    # Wrong binarized_df

    # Wrong aggregated_df


  # 2.2.3. The type argument

  # 2.2.4. The annotate argument

  # 2.2.5. The timeinterval argument

  # 2.2.6. The mintime and maxtime arguments

  # 2.2.7. The freqinterval argument

  # 2.2.8. The minfreq and maxfreq arguments

  # 2.2.9. The nbins argument

  # 2.2.10. The labelsize argument

  # 2.2.11. The palette and direction arguments

  # 2.2.12. The boolean flag arguments (zero.black, marginplot, interactive, save)

  # 2.2.13. The n_time and n_freq arguments

  # 2.2.14. The dir, filename and device arguments

  # 2.2.15. The height and width arguments






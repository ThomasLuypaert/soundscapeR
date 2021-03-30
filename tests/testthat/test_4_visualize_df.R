library(testthat)
library(soundscapeR)

# 1. Load the merged '.csv' data frame files,
# binarize the data frame, aggregate the data frame, and make
#wrong data frame types for testing purposes

fpath_CVR <- system.file("/extdata/merged_soundscape/merged_soundscape_CVR.ssc",
                         package="soundscapeR")

merged_soundscape_CVR <- qs::qread(file = fpath_CVR)
merged_soundscape_CVR@fileloc <- substr(fpath_CVR, 0, nchar(fpath_CVR)-26)

binarized_soundscape_CVR <- binarize_df(merged_soundscape = merged_soundscape_CVR,
                                        method = "Otsu",
                                        value = NULL)

aggregated_soundscape_CVR <- aggregate_df(binarized_soundscape = binarized_soundscape_CVR,
                                  output = "incidence_freq")

# 2. Start testing the heatmapper function

  # 2.0. If required argument is missing

testthat::test_that("the heatmapper function provides the correct error when the aggregated_soundscape argument is missing", {

  testthat::expect_error(
    object = heatmapper(),
    regexp = "aggregated_soundscape argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

  # 2.1. When all the correct arguments are supplied

    # 2.1.1. type = 'regular' and annotate = FALSE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "regular",
                     annotate = FALSE)

  vdiffr::expect_doppelganger("heatmapper_regular_no_anno", plot)

})

    # 2.1.2. type = 'regular' and annotate = TRUE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
               type = "regular",
               annotate = TRUE)

  vdiffr::expect_doppelganger("heatmapper_regular_anno", plot)

})

    # 2.1.3. type = 'polar' and annotate = FALSE

 # testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
 #
 #   plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
 #                type = "polar",
 #                annotate = FALSE)
 #
 #   vdiffr::expect_doppelganger("heatmapper_polar_no_anno", plot)
 #
 # })

    # 2.1.4. type = 'polar' and annotate = TRUE

 # testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
 #
 #   plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
 #                type = "polar",
 #                annotate = TRUE)
 #
 #   vdiffr::expect_doppelganger("heatmapper_polar_anno", plot)
 #
 # })

  # 2.1.5. type = 'regular', annotate = FALSE & subsetting

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "regular",
                     annotate = FALSE,
                     minfreq = 2000,
                     maxfreq = 15000,
                     mintime = "04:00:00",
                     maxtime = "20:00:00")

  vdiffr::expect_doppelganger("heatmapper_regular_no_anno_subset", plot)

})


testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "regular",
                     annotate = TRUE,
                     minfreq = 2000,
                     maxfreq = 15000,
                     mintime = "04:00:00",
                     maxtime = "20:00:00")

  vdiffr::expect_doppelganger("heatmapper_regular_anno_subset", plot)

})

    # 2.1.4. If type = 'regular' and zero.black = TRUE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                     type = "regular",
                     zero.black = TRUE)

  vdiffr::expect_doppelganger("heatmapper_regular_zero.black", plot)

})

  # 2.1.5. If type = 'polar' and zero.black = TRUE
#
# testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
#
#   plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
#                type = "polar",
#                zero.black = TRUE)
#
#   vdiffr::expect_doppelganger("heatmapper_polar_zero.black", plot)
#
# })

    # 2.1.6. If type = 'regular' and maxfreq > 22000 and direction = 1

# testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
#
#   plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
#                type = "regular",
#                maxfreq = 22001)
#
#   vdiffr::expect_doppelganger("heatmapper_regular_maxfreq_22001_dir1", plot)
#
# })

    # 2.1.7. If type = 'regular' and maxfreq > 22000 and direction = -1

# testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
#
#   plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
#                type = "regular",
#                maxfreq = 22001,
#                direction = -1)
#
#   vdiffr::expect_doppelganger("heatmapper_regular_maxfreq_22001_dir-1", plot)
#
# })

    # 2.1.8. If type = 'regular' and direction = -1

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
               type = "regular",
               direction = -1)

  vdiffr::expect_doppelganger("heatmapper_regular_dir1", plot)

})

    # 2.1.9. If type = 'polar' and direction = -1

# testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
#
#   plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
#                type = "polar",
#                direction = -1)
#
#   vdiffr::expect_doppelganger("heatmapper_polar_dir-1", plot)
#
# })

    # 2.1.10. If marginplot = FALSE, interactive = FALSE, save = TRUE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
               type = "regular",
               marginplot = FALSE,
               interactive = FALSE,
               save = TRUE,
               dir = getwd(),
               filename = "file",
               device = "png")

  vdiffr::expect_doppelganger("heatmapper_regular_nomarg_noint_save", plot)

})

    # 2.1.11. If marginplot = FALSE, interactive = TRUE, save = FALSE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
               type = "regular",
               marginplot = FALSE,
               interactive = TRUE,
               save = FALSE)

  vdiffr::expect_doppelganger("heatmapper_regular_nomarg_int_nosave", plot)

})

    # 2.1.12. If marginplot = FALSE, interactive = TRUE, save = TRUE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
               type = "regular",
               marginplot = FALSE,
               interactive = TRUE,
               save = FALSE)

  vdiffr::expect_doppelganger("heatmapper_regular_nomarg_int_save", plot)

})

    # 2.1.13. If marginplot = TRUE, interactive = FALSE, save = FALSE

# testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
#
#   plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
#                      type = "regular",
#                      marginplot = TRUE,
#                      interactive = FALSE,
#                      save = FALSE)
#
#   vdiffr::expect_doppelganger("heatmapper_regular_marg_noint_nosave", plot)
#
# })

    # 2.1.14. If marginplot = TRUE, interactive = FALSE, save = TRUE

# testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
#
#   plot <- heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
#                      type = "regular",
#                      marginplot = TRUE,
#                      interactive = FALSE,
#                      save = TRUE)
#
#   vdiffr::expect_doppelganger("heatmapper_regular_marg_noint_save", plot)
#
# })

  # 2.2. When the supplied aggregated_soundscape argument is wrong

    # 2.2.1. The aggregated_soundscape argument is not an S4 object of the type
    # 'soundscape'

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape argument is not an S4-object of the type 'soundscape' ", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR@merged_df),
    regexp = "aggregated_soundscape is not an S4-object of the type 'soundscape'. Please supply the aggregated_soundscape object produced by the aggregate_df() function. Consult the package documentation for further information.",
    fixed=TRUE
  )

})

  # 2.3. When the aggregated_soundscape elements are wrong

    # 2.3.1. The lat and lon arguments are wrong

aggregated_soundscape_coord_1 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_2 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_3 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_4 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_5 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_6 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_1@lat <- 91
aggregated_soundscape_coord_2@lat <- -91
aggregated_soundscape_coord_3@lon <- 181
aggregated_soundscape_coord_4@lon <- -181
aggregated_soundscape_coord_5@lat <- 91
aggregated_soundscape_coord_5@lon <- 181
aggregated_soundscape_coord_6@lat <- -91
aggregated_soundscape_coord_6@lon <- -181

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_coord_1),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_coord_2),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_coord_3),
    regexp = "aggregated_soundscape@lon is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_coord_4),
    regexp = "aggregated_soundscape@lon is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_coord_5),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_coord_6),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

    # 2.3.2. When the tz argument is wrong

aggregated_soundscape_tz <- aggregated_soundscape_CVR
aggregated_soundscape_tz@tz <- "Emarica/Manaus"

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape tz argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_tz),
    regexp = "aggregated_soundscape@tz is not a recognized timezone. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed=TRUE
  )

})

    # 2.3.3. When the fileloc argument is wrong

aggregated_soundscape_fileloc <- aggregated_soundscape_CVR
aggregated_soundscape_fileloc@fileloc <- paste0(getwd(), "/IDontExist")

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape fileloc argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_fileloc),
    regexp = paste0("Path ",
                    paste0("'", getwd(), "/IDontExist", "'"),
                    " does not exist"),
    fixed=TRUE
  )

})

    # 2.3.4. When the index argument is wrong

aggregated_soundscape_index <- aggregated_soundscape_CVR
aggregated_soundscape_index@index <- "I'm not an option!"

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape index argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_index),
    regexp = "aggregated_soundscape@index is not a character string of one of the available index options. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed=TRUE
  )

})

    # 2.3.5. When the samplerate argument is wrong

aggregated_soundscape_samplerate1 <- aggregated_soundscape_CVR
aggregated_soundscape_samplerate2 <- aggregated_soundscape_CVR
aggregated_soundscape_samplerate1@samplerate <- -44100
aggregated_soundscape_samplerate2@samplerate <- c(44100, 44200)

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape samplerate argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_samplerate1),
    regexp = "aggregated_soundscape@samplerate is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape samplerate argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_samplerate2),
    regexp = "aggregated_soundscape@samplerate is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

    # 2.3.6. When the window argument is wrong

aggregated_soundscape_window1 <- aggregated_soundscape_CVR
aggregated_soundscape_window2 <- aggregated_soundscape_CVR
aggregated_soundscape_window1@window <- -256
aggregated_soundscape_window2@window <- c(256, 512)

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape window argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_window1),
    regexp = "aggregated_soundscape@window is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape window argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_window2),
    regexp = "aggregated_soundscape@window is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

    # 2.3.7. When the binarization_method argument is wrong

aggregated_soundscape_binmeth1 <- aggregated_soundscape_CVR
aggregated_soundscape_binmeth1@binarization_method <- "I'm not an option!"

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape window argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_binmeth1),
    regexp = "aggregated_soundscape@binarization_method is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

    # 2.3.8. When the threshold argument is wrong

aggregated_soundscape_thresh1 <- aggregated_soundscape_CVR
aggregated_soundscape_thresh1@threshold <- c(1.5, 1.6)

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape threshold argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_thresh1),
    regexp = "aggregated_soundscape@threshold is not a single numeric value. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed=TRUE
  )

})

    # 2.3.9. When the output argument is wrong

aggregated_soundscape_output1 <- aggregated_soundscape_CVR
aggregated_soundscape_output2 <- aggregated_soundscape_CVR
aggregated_soundscape_output1@output <- "I'm not  an option"
aggregated_soundscape_output2@output <- c("raw", "incidence_freq")

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape output argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_output1),
    regexp = "aggregated_soundscape@output is not a character string describing one of the available output options. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape output argument is wrong", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_output2),
    regexp = "aggregated_soundscape@output is not a character string describing one of the available output options. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed=TRUE
  )

})

    # 2.3.10. The merged_df argument is wrong

aggregated_soundscape_merged_df1 <- aggregated_soundscape_CVR
aggregated_soundscape_merged_df2 <- aggregated_soundscape_CVR
aggregated_soundscape_merged_df3 <- aggregated_soundscape_CVR
aggregated_soundscape_merged_df4 <- aggregated_soundscape_CVR
aggregated_soundscape_merged_df5 <- aggregated_soundscape_CVR
aggregated_soundscape_merged_df1@merged_df <- aggregated_soundscape_merged_df1@merged_df[FALSE,]
aggregated_soundscape_merged_df2@merged_df[1,1] <- NA
aggregated_soundscape_merged_df3@merged_df[1,1] <- "I'm not numeric"
rownames(aggregated_soundscape_merged_df4@merged_df) <-
  seq(1,nrow(aggregated_soundscape_merged_df4@merged_df), 1)
colnames(aggregated_soundscape_merged_df5@merged_df) <-
  seq(1,ncol(aggregated_soundscape_merged_df5@merged_df), 1)

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape merged_df argument is empty", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_merged_df1),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape merged_df argument contains NA values", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_merged_df2),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape merged_df argument contains non-numeric values", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_merged_df3),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape merged_df argument has incorrect row names", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_merged_df4),
    regexp = "aggregated_soundscape@merged_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape merged_df argument has incorrect column names", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_merged_df5),
    regexp = "aggregated_soundscape@merged_df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})

    # 2.3.11. The binarized_df argument is wrong

aggregated_soundscape_bindf1 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf2 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf3 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf4 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf5 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf6 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf1@binarized_df <- aggregated_soundscape_bindf1@binarized_df[FALSE,]
aggregated_soundscape_bindf2@binarized_df[1,1] <- NA
aggregated_soundscape_bindf3@binarized_df[1,1] <- "I'm not numeric"
rownames(aggregated_soundscape_bindf4@binarized_df) <-
  seq(1,nrow(aggregated_soundscape_bindf4@binarized_df), 1)
colnames(aggregated_soundscape_bindf5@binarized_df) <-
  seq(1,ncol(aggregated_soundscape_bindf5@binarized_df), 1)
aggregated_soundscape_bindf6@binarized_df[1,1] <- 25

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape binarized_df argument is empty", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_bindf1),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape binarized_df argument contains NA values", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_bindf2),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape binarized_df argument contains non-numeric values", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_bindf3),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape binarized_df argument has incorrect row names", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_bindf4),
    regexp = "aggregated_soundscape@binarized_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape binarized_df argument has incorrect column names", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_bindf5),
    regexp = "aggregated_soundscape@binarized_df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape binarized_df argument is non-binary", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_bindf6),
    regexp = "aggregated_soundscape@binarized_df has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the aggregate_df() function.",
    fixed=TRUE
  )

})

    # 2.3.12. The aggregated_df argument is wrong

aggregated_soundscape_aggdf1 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf2 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf3 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf4 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf5 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf6 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf1@aggregated_df <- aggregated_soundscape_aggdf1@aggregated_df[FALSE,]
aggregated_soundscape_aggdf2@aggregated_df[1,1] <- NA
aggregated_soundscape_aggdf3@aggregated_df[1,1] <- "I'm not numeric"
rownames(aggregated_soundscape_aggdf4@aggregated_df) <-
  seq(1,nrow(aggregated_soundscape_aggdf4@aggregated_df), 1)
colnames(aggregated_soundscape_aggdf5@aggregated_df) <-
  seq(1,ncol(aggregated_soundscape_aggdf5@aggregated_df), 1)
aggregated_soundscape_aggdf6@aggregated_df[1,1] <- 25

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape aggregated_df argument is empty", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_aggdf1),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape aggregated_df argument contains NA values", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_aggdf2),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape aggregated_df argument contains non-numeric values", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_aggdf3),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape aggregated_df argument has incorrect row names", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_aggdf4),
    regexp = "aggregated_soundscape@aggregated_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape aggregated_df argument has incorrect column names", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_aggdf5),
    regexp = "aggregated_soundscape@aggregated_df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})


testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape aggregated_df argument contains values outside of the expected range", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape =aggregated_soundscape_aggdf6),
    regexp = "aggregated_soundscape@aggregated_df contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument.",
    fixed=TRUE
  )

})

    # 2.3.14. The aggregated_df_per_time argument is wrong

aggregated_soundscape_aggtime1 <- aggregated_soundscape_CVR
aggregated_soundscape_aggtime2 <- aggregated_soundscape_CVR
aggregated_soundscape_aggtime3 <- aggregated_soundscape_CVR
aggregated_soundscape_aggtime4 <- aggregated_soundscape_CVR

aggregated_soundscape_aggtime1@aggregated_df_per_time <-
  as.list(seq(1, length(aggregated_soundscape_CVR@aggregated_df_per_time), 1))

aggregated_soundscape_aggtime2@aggregated_df_per_time <-
  lapply(aggregated_soundscape_CVR@aggregated_df_per_time, function(x) x[,1])

names(aggregated_soundscape_aggtime3@aggregated_df_per_time) <-
  as.character(seq(1, length(aggregated_soundscape_CVR@aggregated_df_per_time)))

aggregated_soundscape_aggtime4@aggregated_df_per_time <-
  aggregated_soundscape_CVR@aggregated_df_per_time[[1]]

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape aggregated_df_per_time argument does not have the expected values", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_aggtime1),
    regexp = "aggregated_soundscape@aggregated_df_per_time does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape aggregated_df_per_time argument does not have the expected dimensions", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_aggtime2),
    regexp = "aggregated_soundscape@aggregated_df_per_time does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape aggregated_df_per_time argument does not have the expected names", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_aggtime3),
    regexp = "aggregated_soundscape@aggregated_df_per_time does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape aggregated_df_per_time argument does not have the expected length", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_aggtime4),
    regexp = "aggregated_soundscape@aggregated_df_per_time does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

    # 2.3.15. The effort_per_time argument is wrong

aggregated_soundscape_efftime <- aggregated_soundscape_CVR

aggregated_soundscape_efftime@effort_per_time <-
  as.list(seq(1, length(aggregated_soundscape_CVR@effort_per_time), 1))

testthat::test_that("the heatmapper function produces the correct error message when the aggregated_soundscape effort_per_time argument does not have the expected format", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape= aggregated_soundscape_efftime),
    regexp = "aggregated_soundscape@effort_per_time does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})


  # 2.4. If the wrong type argument is supplied

testthat::test_that("the heatmapper function provides the correct error message when the supplied type argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        type = as.factor("regular")),
    regexp = "type is not a character string. Please supply the heatmap type as a character string. Consult package documentation for available type argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied type argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        type = "I'm not an option!"),
    regexp = "type is not one of the available heatmap type options. Please consult package documentation for available type argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE)

})

  # 2.5. The wrong annotate argument is supplied

testthat::test_that("the heatmapper function provides the correct error message when the supplied annotate argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        annotate = "I'm not a boolean flag!"),
    regexp = "annotate is not a Boolean flag (TRUE or FALSE). Please set annotate argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied annotate argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        annotate = "TRUE"),
    regexp = "annotate is not a Boolean flag (TRUE or FALSE). Please set annotate argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed=TRUE)

})

  # 2.6. The wrong timeinterval argument is supplied

testthat::test_that("the heatmapper function provides the correct error message when the supplied timeinterval argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        timeinterval = "half an hour"),
    regexp = "timeinterval is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied timeinterval argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        timeinterval = "thirty minutes"),
    regexp = "timeinterval is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.",
    fixed=TRUE)

})

  # 2.7. The wrong mintime and maxtime arguments are supplied

testthat::test_that("the heatmapper function provides the correct error message when the supplied mintime argument does not fit the expected format", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        mintime = "09-00-00"),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied maxtime argument does not fit the expected format", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        maxtime = "18-00-00"),
    regexp = "maxtime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied mintime and maxtime arguments don't not fit the expected format", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        mintime = "09-00-00",
                        maxtime = "18-00-00"),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE)

})

  # 2.8. The wrong freqinterval argument is supplied

testthat::test_that("the heatmapper function provides the correct error message when the supplied freqinterval argument is not a single positive integer", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        freqinterval = 1.000001),
    regexp = "freqinterval is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied freqinterval argument is larger than the upper frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        freqinterval = 25000),
    regexp = "freqinterval is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied freqinterval argument is lower than the lower frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        freqinterval = 1),
    regexp = "freqinterval is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).",
    fixed=TRUE)

})

  # 2.9. The wrong minfreq and maxfreq arguments are supplied

testthat::test_that("the heatmapper function provides the correct error message when the supplied minfreq argument is not a single positive integer", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        minfreq = 1.0001),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied maxfreq argument is not a single positive integer", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        maxfreq = 1.0001),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied minfreq and maxfreq arguments are not a single positive integers", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        minfreq = 1.0001,
                        maxfreq = 1.0001),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied minfreq argument is lower than the lower frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        minfreq = 1),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied minfreq argument is higher than the upper frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        minfreq = 25000),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied maxfreq argument is lower than the lower frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        maxfreq = 1),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied maxfreq argument is higher than the upper frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        maxfreq = 25000),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied maxfreq argument is zero", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        maxfreq = 0),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

  # 2.10. The wrong nbins argument is supplied

testthat::test_that("the heatmapper function provides the correct error message when the supplied nbins argument is not a single positive integer", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        nbins = -1),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the marginplot argument. If you wish to display a marginal plot, please set marginplot = TRUE.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied nbins argument is zero", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        nbins = -1),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the marginplot argument. If you wish to display a marginal plot, please set marginplot = TRUE.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied nbins argument is higher than the number of bins (rows) in the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        nbins = 100000),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the marginplot argument. If you wish to display a marginal plot, please set marginplot = TRUE.",
    fixed=TRUE)

})

  # 2.11. The wrong twilight argument is supplied

testthat::test_that("the heatmapper function provides the correct error message when the supplied twilight argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        twilight = as.factor("sunlight")),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied twilight argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        twilight = "I'm not an option!"),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied twilight argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        twilight = as.vector(2.15, mode="integer")),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE)

})

  # 2.12. The wrong labelsize arguments are provided

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_time argument is zero", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_time = 0),
    regexp = "labelsize_time is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_frequency argument is zero", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_frequency = 0),
    regexp = "labelsize_frequency is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_polar argument is zero", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_polar = 0),
    regexp = "labelsize_polar is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_time argument is not a single positive number", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_time = -2),
    regexp = "labelsize_time is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_frequency argument is not a single positive number", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_frequency = -2),
    regexp = "labelsize_frequency is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_polar argument is not a single positive number", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        labelsize_polar = -2),
    regexp = "labelsize_polar is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

  # 2.13. The wrong palette argument is provided

testthat::test_that("the heatmapper function provides the correct error message when the supplied palette argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        palette = as.factor("magma")),
    regexp = "palette is not a valid palette argument. Palette needs to be supplied as a character string of either: 'A', 'B', 'C', 'D', 'E', 'magma', 'inferno', 'plasma', 'viridis' or 'cividis'. Please supply a valid palette argument. Consult the soundscapeR of viridis package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied palette argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        palette = "I'm not an option!"),
    regexp = "palette is not a valid palette argument. Palette needs to be supplied as a character string of either: 'A', 'B', 'C', 'D', 'E', 'magma', 'inferno', 'plasma', 'viridis' or 'cividis'. Please supply a valid palette argument. Consult the soundscapeR of viridis package documentation for more information.",
    fixed=TRUE)

})

  # 2.14. The wrong direction argument is provided

testthat::test_that("the heatmapper function provides the correct error message when the supplied direction argument is one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        direction = 2),
    regexp = "direction is not a valid direction argument. The direction argument needs to be supplied as a single integer of either 1 or -1. Please supply a valid direction argument. Consult the soundscapeR of viridis package documentation for more information.",
    fixed=TRUE)

})

  # 2.15. Check if the boolean arguments follow the expected format

testthat::test_that("the heatmapper function provides the correct error message when the supplied zero.black argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        zero.black = "Not a boolean flag!"),
    regexp = "zero.black is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied marginplot argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = "Not a boolean flag!"),
    regexp = "marginplot is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied interactive argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        interactive = "Not a boolean flag!"),
    regexp = "interactive is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied save argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        save = "Not a boolean flag!"),
    regexp = "save is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE)

})

  # 2.16. Check if, when the marginplot argument is set to TRUE,
  # correct additional argument are supplied, and arguments are not in
  # conflict

testthat::test_that("the heatmapper function provides the correct error message when the marginplot argument is set to true, but the type argument is set to 'polar'", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        type = "polar",
                        interactive = FALSE,
                        nbins = 10,
                        n_time = 5,
                        n_freq = 5
                        ),
    regexp = "marginplot is used with other arguments which are not accepted. The marginplot=TRUE argument can not be used in synergy with type='polar' or interactive = TRUE.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the marginplot argument is set to true, but the type argument is set to 'polar'", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        type = "regular",
                        interactive = TRUE,
                        nbins = 10,
                        n_time = 5,
                        n_freq = 5
    ),
    regexp = "marginplot is used with other arguments which are not accepted. The marginplot=TRUE argument can not be used in synergy with type='polar' or interactive = TRUE.",
    fixed=TRUE)

})

 # 2.17. The n_time and n_freq arguments don't follow the expected format

testthat::test_that("the heatmapper function provides the correct error message when the n_time argument is not an integer'", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = 3.25,
                        n_freq = 5
    ),
    regexp = "n_time does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the n_time argument is not an integer'", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = "5",
                        n_freq = 5
    ),
    regexp = "n_time does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the n_time argument is not positive'", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = -5,
                        n_freq = 5
    ),
    regexp = "n_time does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the n_freq argument is not an integer'", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = 5,
                        n_freq = 3.25
    ),
    regexp = "n_freq does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the n_freq argument is not an integer'", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = 5,
                        n_freq = "5"
    ),
    regexp = "n_freq does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the n_freq argument is not positive'", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = 5,
                        n_freq = -5
    ),
    regexp = "n_freq does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

  # 2.18. Check if the dir, filename and device argument follow
  # the expected format

testthat::test_that("the heatmapper function provides the correct error message when the dir argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        save = TRUE,
                        dir = as.factor(getwd()),
                        filename = "test" ,
                        device = "png"
    ),
    regexp = "dir is not a character string. The dir arguments needs to be a character string of either 'default' - or a valid pathname to an existing directory on your device. If you're working on a Windows operating system, pay attention to backslash and forwardslash.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the dir argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        save = TRUE,
                        dir = paste0(getwd(), "/IDontExist"),
                        filename = "test" ,
                        device = "png"
    ),
    regexp = paste0("Path ", paste0("'", getwd(), "/IDontExist", "'"), " does not exist"),
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the filename argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        save = TRUE,
                        dir = getwd(),
                        filename = as.factor("test"),
                        device = "png"
    ),
    regexp = "filename is not a valid filename argument. The filename argument needs to be a character string.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the filename argument ends with an extension", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        save = TRUE,
                        dir = getwd(),
                        filename = "filename.png",
                        device = "png"
    ),
    regexp = "filename is not a valid filename argument. Please make the filename argument you provide a character string without the extension.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the device argument is not a string", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        save = TRUE,
                        dir = getwd(),
                        filename = "filename",
                        device = as.factor("png")
    ),
    regexp = "device is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the device argument is one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        save = TRUE,
                        dir = getwd(),
                        filename = "filename",
                        device = "I'm not an option!"
    ),
    regexp = "device is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.",
    fixed=TRUE)

})

  # 2.19. The width and height arguments don't follow the expected format

testthat::test_that("the heatmapper function provides the correct error message when the width argument is a negative integer", {

  testthat::expect_error(
    object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                        save = TRUE,
                        dir = getwd(),
                        filename = "filename",
                        device = "png",
                        width = -1,
                        height = 200
    ),
    regexp = "width is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
    fixed=TRUE)
})

  testthat::test_that("the heatmapper function provides the correct error message when the width argument is a string", {

    testthat::expect_error(
      object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                          save = TRUE,
                          dir = getwd(),
                          filename = "filename",
                          device = "png",
                          width = "200",
                          height = 200
      ),
      regexp = "width is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
      fixed=TRUE)

})

  testthat::test_that("the heatmapper function provides the correct error message when the width argument is not an integer", {

    testthat::expect_error(
      object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                          save = TRUE,
                          dir = getwd(),
                          filename = "filename",
                          device = "png",
                          width = 21.50,
                          height = 200
      ),
      regexp = "width is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
      fixed=TRUE)

  })

  testthat::test_that("the heatmapper function provides the correct error message when the length argument is a negative integer", {

    testthat::expect_error(
      object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                          save = TRUE,
                          dir = getwd(),
                          filename = "filename",
                          device = "png",
                          width = 200,
                          height = -1
      ),
      regexp = "height is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
      fixed=TRUE)
  })

  testthat::test_that("the heatmapper function provides the correct error message when the height argument is a string", {

    testthat::expect_error(
      object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                          save = TRUE,
                          dir = getwd(),
                          filename = "filename",
                          device = "png",
                          width = 200,
                          height = "200"
      ),
      regexp = "height is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
      fixed=TRUE)

  })

  testthat::test_that("the heatmapper function provides the correct error message when the height argument is not an integer", {

    testthat::expect_error(
      object = heatmapper(aggregated_soundscape = aggregated_soundscape_CVR,
                          save = TRUE,
                          dir = getwd(),
                          filename = "filename",
                          device = "png",
                          width = 200,
                          height = 21.50
      ),
      regexp = "height is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
      fixed=TRUE)

  })

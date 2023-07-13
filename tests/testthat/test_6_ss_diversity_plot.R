library(testthat)
library(soundscapeR)

# 1. Load the merged '.csv' data frame files,
# binarize the data frame, aggregate the data frame, and make
# wrong data frame types for testing purposes

fpath_CVR <- system.file("extdata/ss_binarize_test/merged_soundscape_CVR.ssc",
  package = "soundscapeR"
)

fpath_test <- system.file("extdata/test_data",
  package = "soundscapeR"
)

merged_soundscape_CVR <- qs::qread(file = fpath_CVR)
merged_soundscape_CVR@fileloc <- substr(fpath_CVR, 0, nchar(fpath_CVR) - 26)

binarized_soundscape_CVR <- ss_binarize(
  merged_soundscape = merged_soundscape_CVR,
  method = "IsoData",
  value = NULL
)

soundscape_obj_CVR <- ss_aggregate(
  binarized_soundscape = binarized_soundscape_CVR,
  output = "incidence_freq"
)

# 2. Start testing the ss_diversity_plot function

# 2.0. If required arguments are missing

testthat::test_that("the ss_diversity_plot function provides the correct error when the soundscape_obj argument is missing", {
  testthat::expect_error(
    object = ss_diversity_plot(qvalue = 1),
    regexp = "soundscape_obj argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function provides the correct error when the qvalue argument is missing", {
  testthat::expect_error(
    object = ss_diversity_plot(soundscape_obj = soundscape_obj_CVR),
    regexp = "qvalue argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

# 2.1. When the correct arguments are supplied

# graphtype = "total"

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_1",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "total"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_2",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "total",
      smooth = FALSE
    ),
  )
})


testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_3",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 1,
      graphtype = "total",
      output = "percentage"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_4",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 1,
      graphtype = "total",
      output = "raw"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_5",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "total",
      output = "raw"
    ),
  )
})

# graphtype = "frequency"

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_6",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "frequency"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_7",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "frequency",
      smooth = FALSE
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_8",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 1,
      graphtype = "frequency",
      output = "percentage"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_9",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 1,
      graphtype = "frequency",
      output = "raw"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_10",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "frequency",
      output = "raw"
    ),
  )
})

# graphtype = "normfreq"

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_11",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "normfreq"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_12",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "normfreq",
      smooth = FALSE
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_13",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 1,
      graphtype = "normfreq",
      output = "percentage"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_14",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 1,
      graphtype = "normfreq",
      output = "raw"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_15",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "normfreq",
      output = "raw"
    ),
  )
})

# graphtype = "linefreq"

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_16",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "linefreq"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_17",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "linefreq",
      smooth = FALSE
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_18",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 1,
      graphtype = "linefreq",
      output = "percentage"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_19",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 1,
      graphtype = "linefreq",
      output = "raw"
    ),
  )
})

testthat::test_that("The ss_diversity_plot function works as expected when the correct arguments are supplied", {
  vdiffr::expect_doppelganger(
    title = "ss_divplot_20",
    fig = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      graphtype = "linefreq",
      output = "raw"
    ),
  )
})



# 2.2. Correct error message when some of the supplied arguments are incorrect

# 2.2.1. The soundscape_obj argument is not an S4 object of the type 'soundscape'

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj argument is not an S4-object of the type 'soundscape' ", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR@merged_df,
      qvalue = 0
    ),
    regexp = "soundscape_obj is not an S4-object of the type 'soundscape'. Please supply the soundscape_obj object produced by the ss_aggregate() or ss_create() functions. Consult the package documentation for further information.",
    fixed = TRUE
  )
})

# 2.2.2. Some of the soundscape_obj elements are incorrect

# Wrong lat and long coordinates

soundscape_obj_CVR_coord_1 <- soundscape_obj_CVR
soundscape_obj_CVR_coord_2 <- soundscape_obj_CVR
soundscape_obj_CVR_coord_3 <- soundscape_obj_CVR
soundscape_obj_CVR_coord_4 <- soundscape_obj_CVR
soundscape_obj_CVR_coord_5 <- soundscape_obj_CVR
soundscape_obj_CVR_coord_6 <- soundscape_obj_CVR
soundscape_obj_CVR_coord_1@lat <- 91
soundscape_obj_CVR_coord_2@lat <- -91
soundscape_obj_CVR_coord_3@lon <- 181
soundscape_obj_CVR_coord_4@lon <- -181
soundscape_obj_CVR_coord_5@lat <- 91
soundscape_obj_CVR_coord_5@lon <- 181
soundscape_obj_CVR_coord_6@lat <- -91
soundscape_obj_CVR_coord_6@lon <- -181

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_coord_1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_coord_2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_coord_3,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lon is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_coord_4,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lon is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_coord_5,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_coord_6,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

# Wrong time zone argument

soundscape_obj_CVR_tz <- soundscape_obj_CVR
soundscape_obj_CVR_tz@tz <- "Emarica/Manaus"

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj tz argument is wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_tz,
      qvalue = 0
    ),
    regexp = "soundscape_obj@tz is not a recognized timezone. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed = TRUE
  )
})

# Wrong index argument

soundscape_obj_CVR_index <- soundscape_obj_CVR
soundscape_obj_CVR_index@index <- "I'm not an option!"

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj index argument is wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_index,
      qvalue = 0
    ),
    regexp = "soundscape_obj@index is not a character string of one of the available index options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed = TRUE
  )
})

# Wrong samplerate and window arguments

soundscape_obj_CVR_index_samplerate1 <- soundscape_obj_CVR
soundscape_obj_CVR_index_samplerate2 <- soundscape_obj_CVR
soundscape_obj_CVR_index_samplerate1@samplerate <- -44100
soundscape_obj_CVR_index_samplerate2@samplerate <- c(44100, 44200)

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj samplerate argument is wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_index_samplerate1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@samplerate is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj samplerate argument is wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_index_samplerate2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@samplerate is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

soundscape_obj_CVR_index_window1 <- soundscape_obj_CVR
soundscape_obj_CVR_index_window2 <- soundscape_obj_CVR
soundscape_obj_CVR_index_window1@window <- -256
soundscape_obj_CVR_index_window2@window <- c(256, 512)

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj window argument is wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_index_window1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@window is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj window argument is wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_index_window2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@window is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

# Wrong binarization method

soundscape_obj_CVR_binmeth <- soundscape_obj_CVR
soundscape_obj_CVR_binmeth@binarization_method <- "I'm not an option!"

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj binarization method argument is wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_binmeth,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarization_method is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

# Wrong threshold

soundscape_obj_CVR_thresh <- soundscape_obj_CVR
soundscape_obj_CVR_thresh@threshold <- c(1.5, 1.6)

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj threshold argument is wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_thresh,
      qvalue = 0
    ),
    regexp = "soundscape_obj@threshold is not a single numeric value. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed = TRUE
  )
})


# Wrong output

soundscape_obj_CVR_output <- soundscape_obj_CVR
soundscape_obj_CVR_output@output <- "I'm not an option"

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj output argument is wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_output,
      qvalue = 0
    ),
    regexp = "soundscape_obj@output is not a character string describing one of the available output options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed = TRUE
  )
})


# Wrong merged_df

soundscape_obj_CVR_df_1 <- soundscape_obj_CVR
soundscape_obj_CVR_df_2 <- soundscape_obj_CVR
soundscape_obj_CVR_df_3 <- soundscape_obj_CVR
soundscape_obj_CVR_df_4 <- soundscape_obj_CVR

soundscape_obj_CVR_df_1@merged_df <- soundscape_obj_CVR_df_1@merged_df[FALSE, ]
soundscape_obj_CVR_df_2@merged_df[1, 1] <- NA
soundscape_obj_CVR_df_3@merged_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_df_4@merged_df) <-
  seq(1, nrow(soundscape_obj_CVR_df_4@merged_df), 1)

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregate_soundscape merged_df argument is empty", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_df_1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregate_soundscape merged_df argument contains NA values", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_df_2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregate_soundscape merged_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_df_3,
      qvalue = 0
    ),
    regexp = "soundscape_obj@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the aggregate_soundscape merged_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_df_4,
      qvalue = 0
    ),
    regexp = "soundscape_obj@merged_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})

# Wrong binarized_df

soundscape_obj_CVR_bindf1 <- soundscape_obj_CVR
soundscape_obj_CVR_bindf2 <- soundscape_obj_CVR
soundscape_obj_CVR_bindf3 <- soundscape_obj_CVR
soundscape_obj_CVR_bindf4 <- soundscape_obj_CVR
soundscape_obj_CVR_bindf5 <- soundscape_obj_CVR
soundscape_obj_CVR_bindf6 <- soundscape_obj_CVR

soundscape_obj_CVR_bindf1@binarized_df <- soundscape_obj_CVR_bindf1@binarized_df[FALSE, ]
soundscape_obj_CVR_bindf2@binarized_df[1, 1] <- NA
soundscape_obj_CVR_bindf3@binarized_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_bindf4@binarized_df) <-
  seq(1, nrow(soundscape_obj_CVR_bindf4@binarized_df), 1)
soundscape_obj_CVR_bindf5@binarized_df[1, 1] <- 25

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj binarized_df argument is empty", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_bindf1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj binarized_df argument contains NA values", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_bindf2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj binarized_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_bindf3,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj binarized_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_bindf4,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarized_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj binarized_df argument is non-binary", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_bindf5,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarized_df has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

# Wrong aggregated_df

soundscape_obj_CVR_aggdf1 <- soundscape_obj_CVR
soundscape_obj_CVR_aggdf2 <- soundscape_obj_CVR
soundscape_obj_CVR_aggdf3 <- soundscape_obj_CVR
soundscape_obj_CVR_aggdf4 <- soundscape_obj_CVR
soundscape_obj_CVR_aggdf5 <- soundscape_obj_CVR
soundscape_obj_CVR_aggdf6 <- soundscape_obj_CVR
soundscape_obj_CVR_aggdf7 <- soundscape_obj_CVR

soundscape_obj_CVR_aggdf1@aggregated_df <- soundscape_obj_CVR_aggdf1@aggregated_df[FALSE, ]
soundscape_obj_CVR_aggdf2@aggregated_df[1, 1] <- NA
soundscape_obj_CVR_aggdf3@aggregated_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_aggdf4@aggregated_df) <-
  seq(1, nrow(soundscape_obj_CVR_aggdf4@aggregated_df), 1)

soundscape_obj_CVR_aggdf6@aggregated_df[1, 1] <- 25
soundscape_obj_CVR_aggdf7@aggregated_df[1, 1] <- 500
soundscape_obj_CVR_aggdf7@output <- "raw"

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj aggregated_df argument is empty", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_aggdf1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj aggregated_df argument contains NA values", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_aggdf2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj aggregated_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_aggdf3,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj aggregated_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_aggdf4,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj output argument is 'incidence_freq' but the data frame contains values larger than one.", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_aggdf6,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the soundscape_obj output argument is 'raw' but the data frame contains values larger than the number of sampling days", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR_aggdf7,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed = TRUE
  )
})


# 2.4. The qvalues argument is wrong

testthat::test_that("the ss_diversity_plot function produces the correct error message when the qvalue argument is a character string", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = "0"
    ),
    regexp = "qvalue is a character string of length 1. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the qvalue argument is a list", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = as.list(c(1, 2, 3))
    ),
    regexp = "qvalue is a list. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the qvalue argument is a factor", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = as.factor(1)
    ),
    regexp = "qvalue is not an numeric/integer value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the qvalue argument is negative", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = -1
    ),
    regexp = "qvalue is not a positive value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

# 2.5. The graphtype argument is wrong

testthat::test_that("the ss_diversity_plot function produces the correct error message when the graphtype argument is a wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 1,
      graphtype = 3
    ),
    regexp = "graphtype is not a character string. Please supply the ss_diversity_plot graphtype argument as a character string. Consult package documentation for available graphtype argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the graphtype argument is a wrong", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 1,
      graphtype = "I'm not an option!"
    ),
    regexp = "graphtype is not one of the available ss_diversity_plot graphtype options. Please consult package documentation for available graphtype argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})


# 2.6. The minfreq and maxfreq arguments are wrong

testthat::test_that("the ss_diversity_plot function produces the correct error message when the minfreq argument is a negative number", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      minfreq = -1
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the minfreq argument is larger than the upper frequency bound", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      minfreq = 50000
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the minfreq argument is not numeric", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      minfreq = "1"
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the maxfreq argument is a negative number", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      maxfreq = -1
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the maxfreq argument is larger than the upper frequency bound", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      maxfreq = 50000
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the maxfreq argument is not numeric", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      maxfreq = "1"
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the maxfreq argument is zero", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      maxfreq = 0
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the minfreq and maxfreq argument are not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      minfreq = -1,
      maxfreq = 0
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})


# 2.9. The nbins argument is wrong

testthat::test_that("the ss_diversity_plot function produces the correct error message when the nbins argument is a single positive integer", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      nbins = -1
    ),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the nbins argument is a character vector", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      nbins = "20"
    ),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the nbins argument is zero", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      nbins = 0
    ),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the nbins argument is larger than the number of rows in the data frame", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      nbins = 5000
    ),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed = TRUE
  )
})


# 2.10. The timeinterval argument is wrong

testthat::test_that("the ss_heatmap function produces the correct error message when the timeinterval argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      timeinterval = "five seconds"
    ),
    regexp = "timeinterval is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.",
    fixed = TRUE
  )
})


# 2.11. The boolean flag arguments are wrong

# smooth

testthat::test_that("the ss_diversity_plot function produces the correct error message when the smooth argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      smooth = "FALSE"
    ),
    regexp = "smooth is not a Boolean flag (TRUE or FALSE). Please set argument argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the smooth argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      smooth = 1
    ),
    regexp = "smooth is not a Boolean flag (TRUE or FALSE). Please set argument argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed = TRUE
  )
})

# interactive

testthat::test_that("the ss_diversity_plot function produces the correct error message when the 'interactive' argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      interactive = "FALSE"
    ),
    regexp = "interactive is not a Boolean flag (TRUE or FALSE). Please set argument argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the 'interactive' argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      interactive = 1
    ),
    regexp = "interactive is not a Boolean flag (TRUE or FALSE). Please set argument argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed = TRUE
  )
})

# save

testthat::test_that("the ss_diversity_plot function produces the correct error message when the 'save' argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      save = "FALSE"
    ),
    regexp = "save is not a Boolean flag (TRUE or FALSE). Please set argument argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the 'save' argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      save = 1
    ),
    regexp = "save is not a Boolean flag (TRUE or FALSE). Please set argument argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed = TRUE
  )
})

# 2.12. The movavg argument is not in the correct format

testthat::test_that("the ss_diversity_plot function produces the correct error message when the 'movavg' argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      movavg = "I'm not an option!"
    ),
    regexp = "movavg is not a valid movavg argument. Please supply the movavg argument as a single positive integer with a value larger than zero and smaller than the number of unique times in the recording period.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the 'movavg' argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      movavg = -1
    ),
    regexp = "movavg is not a valid movavg argument. Please supply the movavg argument as a single positive integer with a value larger than zero and smaller than the number of unique times in the recording period.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the 'movavg' argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      movavg = 5000
    ),
    regexp = "movavg is not a valid movavg argument. Please supply the movavg argument as a single positive integer with a value larger than zero and smaller than the number of unique times in the recording period.",
    fixed = TRUE
  )
})

# 2.13. The dir, filename and device arguments

testthat::test_that("the ss_diversity_plot function produces the correct error message when the dir argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      dir = 2
    ),
    regexp = "dir is not a character string. The dir arguments needs to be a character string of either 'default' - or a valid pathname to an existing directory on your device. If you're working on a Windows operating system, pay attention to backslash and forwardslash.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the filename argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      filename = 27
    ),
    regexp = "filename is not a valid filename argument. The filename argument needs to be a character string.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the filename argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      filename = "file.plg"
    ),
    regexp = "filename is not a valid filename argument. Please make the filename argument you provide a character string without the extension.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the device argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      filename = "file.png",
      device = 2
    ),
    regexp = "device is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the device argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      filename = "file.png",
      device = "plg"
    ),
    regexp = "device is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.",
    fixed = TRUE
  )
})

# 2.14. The output argument


testthat::test_that("the ss_diversity_plot function produces the correct error message when the output argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      output = 2
    ),
    regexp = "output is not a character string. Please supply the output argument as a character string. Consult package documentation for available output argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the output argument is not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      output = "I'm not an option!"
    ),
    regexp = "output is not one of the available ss_diversity_plot output options. Please consult package documentation for available output argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

# 2.15. The height and width arguments

testthat::test_that("the ss_diversity_plot function produces the correct error message when the height and width arguments are not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      filename = "file.png",
      device = "png",
      height = "three"
    ),
    regexp = "height is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity_plot function produces the correct error message when the height and width arguments are not in the correct format", {
  testthat::expect_error(
    object = ss_diversity_plot(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      filename = "file.png",
      device = "png",
      width = "three"
    ),
    regexp = "width is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
    fixed = TRUE
  )
})

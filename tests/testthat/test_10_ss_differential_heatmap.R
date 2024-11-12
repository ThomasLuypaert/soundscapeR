library(testthat)
library(soundscapeR)

# Set locale before running any tests
Sys.setlocale("LC_ALL", "en_US.UTF-8")


# 1. Load the merged '.csv' data frame files,
# binarize the data frame, aggregate the data frame, and make
# wrong data frame types for testing purposes

fpath_CVR_case_study <- system.file("extdata/case_study/merged_CVR_256_case_study.ssc",
  package = "soundscapeR"
)

merged_soundscape_CVR_case_study <- qs::qread(file = fpath_CVR_case_study)

for (i in 1:length(merged_soundscape_CVR_case_study)) {
  merged_soundscape_CVR_case_study[[i]]@fileloc <- substr(fpath_CVR_case_study, 0, nchar(fpath_CVR_case_study) - 26)
}

binarized_soundscape_CVR_case_study <- lapply(
  merged_soundscape_CVR_case_study,
  function(x) {
    ss_binarize(
      merged_soundscape = x,
      method = "IsoData",
      value = NULL
    )
  }
)

soundscape_obj_CVR_case_study <- lapply(
  binarized_soundscape_CVR_case_study,
  function(x) {
    ss_aggregate(
      binarized_soundscape = x,
      output = "incidence_freq"
    )
  }
)

soundscape_obj_CVR_A <- soundscape_obj_CVR_case_study[[1]]
soundscape_obj_CVR_B <- soundscape_obj_CVR_case_study[[2]]



# 2. Start testing the ss_compare function

# 2.0. If required arguments are missing

# soundscape_obj_A & soundscape_obj_B

testthat::test_that("the ss_compare function provides the correct error when the soundscape_obj_A argument is missing", {
  testthat::expect_error(
    object = soundscapeR::ss_compare(soundscape_obj_B = soundscape_obj_CVR_B),
    regexp = "soundscape_obj_A argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function provides the correct error when the soundscape_obj_B argument is missing", {
  testthat::expect_error(
    object = soundscapeR::ss_compare(soundscape_obj_A = soundscape_obj_CVR_A),
    regexp = "soundscape_obj_B argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

# 2.1. When the correct arguments are supplied

# type = 'regular'

# testthat::test_that("The ss_compare function works as expected when the correct arguments are supplied", {
#   vdiffr::expect_doppelganger(
#     title = "ss_compare_regular",
#     fig = ss_compare(
#       soundscape_obj_A = soundscape_obj_CVR_A,
#       soundscape_obj_B = soundscape_obj_CVR_B
#     ),
#   )
# })

# type = 'polar'

# testthat::test_that("The ss_compare function works as expected when the correct arguments are supplied", {
#
#   vdiffr::expect_doppelganger(
#     title = "Create ss_compare with type = 'polar'",
#     fig = ss_compare(soundscape_obj_A = soundscape_obj_CVR_A,
#                      soundscape_obj_B = soundscape_obj_CVR_B,
#                      type = "polar",
#                      mintime = "06:00:00",
#                      maxtime = "06:05:00",
#                      minfreq = 0,
#                      maxfreq = 500),
#   )
# })

# 2.2. Some of the soundscape_obj elements are incorrect

# Wrong lat and long coordinates

# For soundscape_obj_A

soundscape_obj_CVR_coord_1_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_coord_2_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_coord_3_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_coord_4_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_coord_5_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_coord_6_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_coord_1_A@lat <- 91
soundscape_obj_CVR_coord_2_A@lat <- -91
soundscape_obj_CVR_coord_3_A@lon <- 181
soundscape_obj_CVR_coord_4_A@lon <- -181
soundscape_obj_CVR_coord_5_A@lat <- 91
soundscape_obj_CVR_coord_5_A@lon <- 181
soundscape_obj_CVR_coord_6_A@lat <- -91
soundscape_obj_CVR_coord_6_A@lon <- -181

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_coord_1_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj = soundscape_obj_CVR_coord_2_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj = soundscape_obj_CVR_coord_3_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@lon is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj = soundscape_obj_CVR_coord_4_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@lon is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj = soundscape_obj_CVR_coord_5_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj = soundscape_obj_CVR_coord_6_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

# For soundscape_obj_B

soundscape_obj_CVR_coord_1_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_coord_2_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_coord_3_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_coord_4_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_coord_5_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_coord_6_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_coord_1_B@lat <- 91
soundscape_obj_CVR_coord_2_B@lat <- -91
soundscape_obj_CVR_coord_3_B@lon <- 181
soundscape_obj_CVR_coord_4_B@lon <- -181
soundscape_obj_CVR_coord_5_B@lat <- 91
soundscape_obj_CVR_coord_5_B@lon <- 181
soundscape_obj_CVR_coord_6_B@lat <- -91
soundscape_obj_CVR_coord_6_B@lon <- -181

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_B,
      soundscape_obj_B = soundscape_obj_CVR_coord_1_B
    ),
    regexp = "soundscape_obj_B@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_B,
      soundscape_obj_B = soundscape_obj_CVR_coord_2_B
    ),
    regexp = "soundscape_obj_B@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_B,
      soundscape_obj_B = soundscape_obj_CVR_coord_3_B
    ),
    regexp = "soundscape_obj_B@lon is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_B,
      soundscape_obj_B = soundscape_obj_CVR_coord_4_B
    ),
    regexp = "soundscape_obj_B@lon is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_B,
      soundscape_obj_B = soundscape_obj_CVR_coord_5_B
    ),
    regexp = "soundscape_obj_B@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_B,
      soundscape_obj_B = soundscape_obj_CVR_coord_6_B
    ),
    regexp = "soundscape_obj_B@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})



# Wrong time zone argument

# For soundscape_obj_A

soundscape_obj_CVR_tz_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_tz_A@tz <- "Emarica/Manaus"

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A tz argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_tz_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@tz is not a recognized timezone. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed = TRUE
  )
})

# For soundscape_obj_B

soundscape_obj_CVR_tz_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_tz_B@tz <- "Emarica/Manaus"

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B tz argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_tz_B
    ),
    regexp = "soundscape_obj_B@tz is not a recognized timezone. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed = TRUE
  )
})


# Wrong index argument

# For soundscape_obj_A

soundscape_obj_CVR_index_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_index_A@index <- "I'm not an option!"

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A index argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_index_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@index is not a character string of one of the available index options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed = TRUE
  )
})

# For soundscape_obj_B

soundscape_obj_CVR_index_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_index_B@index <- "I'm not an option!"

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B index argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_index_B
    ),
    regexp = "soundscape_obj_B@index is not a character string of one of the available index options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed = TRUE
  )
})

# Wrong samplerate and window arguments

# For soundscape_obj_A

soundscape_obj_CVR_index_samplerate1_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_index_samplerate2_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_index_samplerate1_A@samplerate <- -44100
soundscape_obj_CVR_index_samplerate2_A@samplerate <- c(44100, 44200)

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A samplerate argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_index_samplerate1_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@samplerate is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A samplerate argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_index_samplerate2_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@samplerate is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

soundscape_obj_CVR_index_window1_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_index_window2_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_index_window1_A@window <- -256
soundscape_obj_CVR_index_window2_A@window <- c(256, 512)

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A window argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_index_window1_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@window is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A window argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_index_window2_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@window is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

# For soundscape_obj_B

soundscape_obj_CVR_index_samplerate1_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_index_samplerate2_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_index_samplerate1_B@samplerate <- -44100
soundscape_obj_CVR_index_samplerate2_B@samplerate <- c(44100, 44200)

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B samplerate argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_index_samplerate1_B
    ),
    regexp = "soundscape_obj_B@samplerate is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B samplerate argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_index_samplerate2_B
    ),
    regexp = "soundscape_obj_B@samplerate is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

soundscape_obj_CVR_index_window1_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_index_window2_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_index_window1_B@window <- -256
soundscape_obj_CVR_index_window2_B@window <- c(256, 512)

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B window argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_index_window1_B
    ),
    regexp = "soundscape_obj_B@window is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B window argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_index_window2_B
    ),
    regexp = "soundscape_obj_B@window is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

# Wrong binarization method

# For soundscape_obj_A

soundscape_obj_CVR_binmeth_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_binmeth_A@binarization_method <- "I'm not an option!"

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A binarization method argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_binmeth_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@binarization_method is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

# For soundscape_obj_B

soundscape_obj_CVR_binmeth_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_binmeth_B@binarization_method <- "I'm not an option!"

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B binarization method argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_binmeth_B
    ),
    regexp = "soundscape_obj_B@binarization_method is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

# Wrong threshold

# For soundscape_obj_A

soundscape_obj_CVR_thresh_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_thresh_A@threshold <- c(1.5, 1.6)

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A threshold argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_thresh_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@threshold is not a single numeric value. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed = TRUE
  )
})

# For soundscape_obj_B

soundscape_obj_CVR_thresh_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_thresh_B@threshold <- c(1.5, 1.6)

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B threshold argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_thresh_B
    ),
    regexp = "soundscape_obj_B@threshold is not a single numeric value. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed = TRUE
  )
})


# Wrong output

# For soundscape_obj_A

soundscape_obj_CVR_output_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_output_A@output <- "I'm not an option"

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A output argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_output_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@output is not a character string describing one of the available output options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed = TRUE
  )
})

# For soundscape_obj_B

soundscape_obj_CVR_output_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_output_B@output <- "I'm not an option"

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B output argument is wrong", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_output_B
    ),
    regexp = "soundscape_obj_B@output is not a character string describing one of the available output options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed = TRUE
  )
})


# Wrong merged_df

# For soundscape_obj_A

soundscape_obj_CVR_df_1_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_df_2_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_df_3_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_df_4_A <- soundscape_obj_CVR_A

soundscape_obj_CVR_df_1_A@merged_df <- soundscape_obj_CVR_df_1_A@merged_df[FALSE, ]
soundscape_obj_CVR_df_2_A@merged_df[1, 1] <- NA
soundscape_obj_CVR_df_3_A@merged_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_df_4_A@merged_df) <-
  seq(1, nrow(soundscape_obj_CVR_df_4_A@merged_df), 1)

testthat::test_that("the ss_compare function produces the correct error message when the aggregate_soundscape_A merged_df argument is empty", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_df_1_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the aggregate_soundscape_A merged_df argument contains NA values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_df_2_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the aggregate_soundscape_A merged_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_df_3_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the aggregate_soundscape_A merged_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_df_4_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@merged_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})

# For soundscape_obj_B

soundscape_obj_CVR_df_1_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_df_2_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_df_3_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_df_4_B <- soundscape_obj_CVR_B

soundscape_obj_CVR_df_1_B@merged_df <- soundscape_obj_CVR_df_1_B@merged_df[FALSE, ]
soundscape_obj_CVR_df_2_B@merged_df[1, 1] <- NA
soundscape_obj_CVR_df_3_B@merged_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_df_4_B@merged_df) <-
  seq(1, nrow(soundscape_obj_CVR_df_4_B@merged_df), 1)

testthat::test_that("the ss_compare function produces the correct error message when the aggregate_soundscape_B merged_df argument is empty", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_df_1_B
    ),
    regexp = "soundscape_obj_B@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the aggregate_soundscape_B merged_df argument contains NA values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_df_2_B
    ),
    regexp = "soundscape_obj_B@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the aggregate_soundscape_B merged_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_df_3_B
    ),
    regexp = "soundscape_obj_B@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the aggregate_soundscape_B merged_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_df_4_B
    ),
    regexp = "soundscape_obj_B@merged_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})

# Wrong binarized_df

# For soundscape_obj_A

soundscape_obj_CVR_bindf1_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_bindf2_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_bindf3_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_bindf4_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_bindf5_A <- soundscape_obj_CVR_A

soundscape_obj_CVR_bindf1_A@binarized_df <- soundscape_obj_CVR_bindf1_A@binarized_df[FALSE, ]
soundscape_obj_CVR_bindf2_A@binarized_df[1, 1] <- NA
soundscape_obj_CVR_bindf3_A@binarized_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_bindf4_A@binarized_df) <-
  seq(1, nrow(soundscape_obj_CVR_bindf4_A@binarized_df), 1)
soundscape_obj_CVR_bindf5_A@binarized_df[1, 1] <- 25

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A binarized_df argument is empty", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_bindf1_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A binarized_df argument contains NA values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_bindf2_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A binarized_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_bindf3_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A binarized_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_bindf4_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@binarized_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A binarized_df argument is non-binary", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_bindf5_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@binarized_df has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

# For soundscape_obj_B

soundscape_obj_CVR_bindf1_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_bindf2_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_bindf3_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_bindf4_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_bindf5_B <- soundscape_obj_CVR_B

soundscape_obj_CVR_bindf1_B@binarized_df <- soundscape_obj_CVR_bindf1_B@binarized_df[FALSE, ]
soundscape_obj_CVR_bindf2_B@binarized_df[1, 1] <- NA
soundscape_obj_CVR_bindf3_B@binarized_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_bindf4_B@binarized_df) <-
  seq(1, nrow(soundscape_obj_CVR_bindf4_B@binarized_df), 1)
soundscape_obj_CVR_bindf5_B@binarized_df[1, 1] <- 25

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B binarized_df argument is empty", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_bindf1_B
    ),
    regexp = "soundscape_obj_B@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B binarized_df argument contains NA values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_bindf2_B
    ),
    regexp = "soundscape_obj_B@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B binarized_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_bindf3_B
    ),
    regexp = "soundscape_obj_B@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B binarized_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_bindf4_B
    ),
    regexp = "soundscape_obj_B@binarized_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B binarized_df argument is non-binary", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_bindf5_B
    ),
    regexp = "soundscape_obj_B@binarized_df has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})


# Wrong aggregated_df

# For soundscape_obj_A

soundscape_obj_CVR_aggdf1_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_aggdf2_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_aggdf3_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_aggdf4_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_aggdf5_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_aggdf6_A <- soundscape_obj_CVR_A
soundscape_obj_CVR_aggdf7_A <- soundscape_obj_CVR_A

soundscape_obj_CVR_aggdf1_A@aggregated_df <- soundscape_obj_CVR_aggdf1_A@aggregated_df[FALSE, ]
soundscape_obj_CVR_aggdf2_A@aggregated_df[1, 1] <- NA
soundscape_obj_CVR_aggdf3_A@aggregated_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_aggdf4_A@aggregated_df) <-
  seq(1, nrow(soundscape_obj_CVR_aggdf4_A@aggregated_df), 1)

soundscape_obj_CVR_aggdf6_A@aggregated_df[1, 1] <- 25
soundscape_obj_CVR_aggdf7_A@aggregated_df[1, 1] <- 500
soundscape_obj_CVR_aggdf7_A@output <- "raw"

soundscape_obj_CVR_aggdf7_AB <- soundscape_obj_CVR_A
soundscape_obj_CVR_aggdf7_AB@output <- "raw"

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A aggregated_df argument is empty", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_aggdf1_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A aggregated_df argument contains NA values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_aggdf2_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A aggregated_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_aggdf3_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A aggregated_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_aggdf4_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@aggregated_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A output argument is 'incidence_freq' but the data frame contains values larger than one.", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_aggdf6_A,
      soundscape_obj_B = soundscape_obj_CVR_B
    ),
    regexp = "soundscape_obj_A@aggregated_df contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_A output argument is 'raw' but the data frame contains values larger than the number of sampling days", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_aggdf7_A,
      soundscape_obj_B = soundscape_obj_CVR_aggdf7_AB
    ),
    regexp = "soundscape_obj_A@aggregated_df contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed = TRUE
  )
})

# For soundscape_obj_B

soundscape_obj_CVR_aggdf1_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_aggdf2_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_aggdf3_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_aggdf4_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_aggdf5_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_aggdf6_B <- soundscape_obj_CVR_B
soundscape_obj_CVR_aggdf7_B <- soundscape_obj_CVR_B

soundscape_obj_CVR_aggdf1_B@aggregated_df <- soundscape_obj_CVR_aggdf1_B@aggregated_df[FALSE, ]
soundscape_obj_CVR_aggdf2_B@aggregated_df[1, 1] <- NA
soundscape_obj_CVR_aggdf3_B@aggregated_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_aggdf4_B@aggregated_df) <-
  seq(1, nrow(soundscape_obj_CVR_aggdf4_B@aggregated_df), 1)

soundscape_obj_CVR_aggdf6_B@aggregated_df[1, 1] <- 25
soundscape_obj_CVR_aggdf7_B@aggregated_df[1, 1] <- 500
soundscape_obj_CVR_aggdf7_B@output <- "raw"

soundscape_obj_CVR_aggdf7_BA <- soundscape_obj_CVR_B
soundscape_obj_CVR_aggdf7_BA@output <- "raw"

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B aggregated_df argument is empty", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_aggdf1_B
    ),
    regexp = "soundscape_obj_B@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B aggregated_df argument contains NA values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_aggdf2_B
    ),
    regexp = "soundscape_obj_B@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B aggregated_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_aggdf3_B
    ),
    regexp = "soundscape_obj_B@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B aggregated_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_aggdf4_B
    ),
    regexp = "soundscape_obj_B@aggregated_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_compare function produces the correct error message when the soundscape_obj_B output argument is 'incidence_freq' but the data frame contains values larger than one.", {
  testthat::expect_error(
    object = ss_compare(
      soundscape_obj_A = soundscape_obj_CVR_A,
      soundscape_obj_B = soundscape_obj_CVR_aggdf6_B
    ),
    regexp = "soundscape_obj_B@aggregated_df contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed = TRUE
  )
})

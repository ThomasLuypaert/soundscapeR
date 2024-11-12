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

case_study_groups <- c("A", "B", "B", "B", "B")

# 2. Start testing the ss_pcoa function

# 2.0. If the soundscape_list or qvalue arguments are missing

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument is missing", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(qvalue = 1),
    regexp = "soundscape_list argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the qvalue argument is missing", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study),
    regexp = "qvalue argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})




# 2.1. If the soundscape_list argument is not a list

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument is not a list", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = c(2,3,4), qvalue = 1),
    regexp = "soundscape_list argument is not a list Please supply a list of aggregated soundscape objects.",
    fixed = TRUE
  )
})

# 2.3. If the soundscape_list argument is not a list of S4 soundscape objects

wrong_list <- vector("list")
wrong_list[[1]] <- c(2, 3, 4)
wrong_list[[2]] <- c(1, 2 ,3)

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument is not a list of S4 soundscape objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = wrong_list, qvalue = 1),
    regexp = "soundscape_list is not a list of S4-objects of the type 'soundscape'. The list may contain different objects or empty elements. Please supply a list of soundscape objects produced by the ss_aggregate() function. Consult the package documentation for further information.",
    fixed = TRUE
  )
})

# 2.4. If some of the soundscape object arguments are wrong

# 2.4.1. The lat and lon arguments are wrong

soundscape_obj_CVR_case_study_1 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_2 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_3 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_4 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_5 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_6 <- soundscape_obj_CVR_case_study

soundscape_obj_CVR_case_study_1[[1]]@lat <- 91
soundscape_obj_CVR_case_study_2[[1]]@lat <- -91
soundscape_obj_CVR_case_study_3[[1]]@lon <- 181
soundscape_obj_CVR_case_study_4[[1]]@lon <- -181
soundscape_obj_CVR_case_study_5[[1]]@lat <- 91
soundscape_obj_CVR_case_study_5[[1]]@lon <- 181
soundscape_obj_CVR_case_study_6[[1]]@lat <- -91
soundscape_obj_CVR_case_study_6[[1]]@lon <- -181

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_1, qvalue = 1),
    regexp = "soundscape_list contains invalid latitude coordinates. Did you supply the a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_2, qvalue = 1),
    regexp = "soundscape_list contains invalid latitude coordinates. Did you supply the a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_3, qvalue = 1),
    regexp = "soundscape_list contains invalid longitude coordinate. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_4, qvalue = 1),
    regexp = "soundscape_list contains invalid longitude coordinate. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_5, qvalue = 1),
    regexp = "soundscape_list contains invalid latitude coordinates. Did you supply the a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_6, qvalue = 1),
    regexp = "soundscape_list contains invalid latitude coordinates. Did you supply the a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

#2.4.2. Wrong timezone argument

soundscape_obj_CVR_case_study_tz <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_tz[[1]]@tz <- "Emerica/Manaus"

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid timezones", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_tz, qvalue = 1),
    regexp = "soundscape_list contains invalid timezone objects Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed = TRUE
  )
})

# 2.4.3. Wrong index argument

soundscape_obj_CVR_case_study_index <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_index[[1]]@index <- "I'm not an option!"

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid index objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_index, qvalue = 1),
    regexp = "soundscape_list contains invalid index objects. Make sure the index argument inside the soundscape object is one of the available index options. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed = TRUE
  )
})

# 2.4.4. Wrong samplerate and window arguments

soundscape_obj_CVR_case_study_samplerate1 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_samplerate2 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_samplerate1[[1]]@samplerate <- -44100
soundscape_obj_CVR_case_study_samplerate2[[1]]@samplerate <- c(44100, 44100)

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid samplerate objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_samplerate1, qvalue = 1),
    regexp = "soundscape_list contains samplerate arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid samplerate objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_samplerate2, qvalue = 1),
    regexp = "soundscape_list contains samplerate arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

soundscape_obj_CVR_case_study_window1 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_window2 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_window1[[1]]@window <- -256
soundscape_obj_CVR_case_study_window2[[1]]@window <- c(256, 256)

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid window objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_window1, qvalue = 1),
    regexp = "soundscape_list contains window arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid window objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_window2, qvalue = 1),
    regexp = "soundscape_list contains window arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

# 2.4.5. Wrong binarization method

soundscape_obj_CVR_case_study_bin <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_bin[[1]]@binarization_method <- "I'm not an option!"

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarization_method objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_bin, qvalue = 1),
    regexp = "soundscape_list contains binarization_method arguments that are not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

# 2.4.6. Wrong threshold

soundscape_obj_CVR_case_study_thresh <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_thresh[[1]]@threshold <- c(1.2, 3.5)

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid threshold objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_thresh, qvalue = 1),
    regexp = "soundscape_list contains threshold arguments that are not a single numeric value. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed = TRUE
  )
})

# 2.4.7. Wrong output

soundscape_obj_CVR_case_study_output <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_output[[1]]@output <- "I'm not an option"

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid output objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_output, qvalue = 1),
    regexp = "soundscape_list contains output arguments that are not a character string describing one of the available output options. Did you supply a list of soundscapes produced using the ss_aggregate function? If so, something has gone wrong, please re-run the ss_aggregate() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed = TRUE
  )
})

# 2.4.8. Wrong merged_df

soundscape_obj_CVR_case_study_outputdf_1 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_outputdf_2 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_outputdf_3 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_outputdf_4 <- soundscape_obj_CVR_case_study

soundscape_obj_CVR_case_study_outputdf_1[[1]]@merged_df <- soundscape_obj_CVR_case_study_outputdf_1[[1]]@merged_df[FALSE, ]
soundscape_obj_CVR_case_study_outputdf_2[[1]]@merged_df[1, 1] <- NA
soundscape_obj_CVR_case_study_outputdf_3[[1]]@merged_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_case_study_outputdf_4[[1]]@merged_df) <-
  seq(1, nrow(soundscape_obj_CVR_case_study_outputdf_4[[1]]@merged_df), 1)

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid merged_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_outputdf_1, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid merged_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_outputdf_2, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid merged_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_outputdf_3, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid merged_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_outputdf_4, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.",
    fixed = TRUE
  )
})

# 2.4.9. Wrong binarized_df

soundscape_obj_CVR_case_study_binarizeddf_1 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_binarizeddf_2 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_binarizeddf_3 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_binarizeddf_4 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_binarizeddf_5 <- soundscape_obj_CVR_case_study

soundscape_obj_CVR_case_study_binarizeddf_1[[1]]@binarized_df <- soundscape_obj_CVR_case_study_binarizeddf_1[[1]]@binarized_df[FALSE, ]
soundscape_obj_CVR_case_study_binarizeddf_2[[1]]@binarized_df[1, 1] <- NA
soundscape_obj_CVR_case_study_binarizeddf_3[[1]]@binarized_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_case_study_binarizeddf_4[[1]]@binarized_df) <-
  seq(1, nrow(soundscape_obj_CVR_case_study_binarizeddf_4[[1]]@binarized_df), 1)
soundscape_obj_CVR_case_study_binarizeddf_5[[1]]@binarized_df[1,1] <- 25

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarized_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_binarizeddf_1, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarized_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_binarizeddf_2, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarized_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_binarizeddf_3, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarized_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_binarizeddf_4, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarized_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_binarizeddf_5, qvalue = 1),
    regexp = "soundscape_list contains binarized_df dataframes with values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() function.",
    fixed = TRUE
  )
})

# 2.4.10. Wrong aggregated_df

soundscape_obj_CVR_case_study_aggdf_1 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_aggdf_2 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_aggdf_3 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_aggdf_4 <- soundscape_obj_CVR_case_study

soundscape_obj_CVR_case_study_aggdf_1[[1]]@aggregated_df <- soundscape_obj_CVR_case_study_aggdf_1[[1]]@aggregated_df[FALSE, ]
soundscape_obj_CVR_case_study_aggdf_2[[1]]@aggregated_df[1, 1] <- NA
soundscape_obj_CVR_case_study_aggdf_3[[1]]@aggregated_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_CVR_case_study_aggdf_4[[1]]@aggregated_df) <-
  seq(1, nrow(soundscape_obj_CVR_case_study_aggdf_4[[1]]@aggregated_df), 1)

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid aggregated_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_aggdf_1, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid aggregated_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_aggdf_2, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid aggregated_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_aggdf_3, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function provides the correct error when the soundscape_list argument contains soundscape objects with invalid aggregated_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study_aggdf_4, qvalue = 1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.",
    fixed = TRUE
  )
})

# 2.4.11. The qvalues argument is wrong

testthat::test_that("the ss_pairdis function produces the correct error message when the qvalue argument is a character string", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = "0"
    ),
    regexp = "qvalue is a character string of length 1. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the qvalue argument is a list", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = as.list(c(1, 2, 3))
    ),
    regexp = "qvalue is a list. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the qvalue argument is a list", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = as.factor(1)
    ),
    regexp = "qvalue is not an numeric/integer value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the qvalue argument is a list", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = -1
    ),
    regexp = "qvalue is not a positive value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

# 2.4.12. The mintime and maxtime arguments are wrong

testthat::test_that("the ss_pairdis function produces the correct error message when the mintime argument does not follow the expected format", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      mintime = "noon"
    ),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the mintime argument does not follow the expected format", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      mintime = as.factor("12:00:00")
    ),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the maxtime argument does not follow the expected format", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      maxtime = "noon"
    ),
    regexp = "maxtime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the maxtime argument does not follow the expected format", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      maxtime = as.factor("12:00:00")
    ),
    regexp = "maxtime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the mintime and maxtime arguments don't follow the expected format", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      mintime = as.factor("12:00:00"),
      maxtime = as.factor("12:00:00")
    ),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed = TRUE
  )
})

# 2.4.13. The minfreq and maxfreq arguments are wrong

testthat::test_that("the ss_pairdis function produces the correct error message when the minfreq argument is a negative number", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      minfreq = -1
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the minfreq argument is larger than the upper frequency bound", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      minfreq = 50000
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the minfreq argument is not numeric", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      minfreq = "1"
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the maxfreq argument is a negative number", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      maxfreq = -1
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the maxfreq argument is larger than the upper frequency bound", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      maxfreq = 50000
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the maxfreq argument is not numeric", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      maxfreq = "1"
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the maxfreq argument is zero", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      maxfreq = 0
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pairdis function produces the correct error message when the minfreq and maxfreq argument are not in the correct format", {
  testthat::expect_error(
    object = soundscapeR::ss_pairdis(
      soundscape_list = soundscape_obj_CVR_case_study,
      qvalue = 0,
      minfreq = -1,
      maxfreq = 0
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

# 2.5. When the correct arguments are supplied

# No hierarchy or subsetting

test_that("`ss_pairdis()` works as expected when q = 0", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 0))
})

test_that("`ss_pairdis()` works as expected when q = 1", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 1))
})

test_that("`ss_pairdis()` works as expected when q = 2", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 2))
})

# No hierarchy but frequency subsetting

test_that("`ss_pairdis()` works as expected when q = 0 and frequency subsetting", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 0,
                                          minfreq = 500,
                                          maxfreq = 2000))
})

test_that("`ss_pairdis()` works as expected when q = 1 and frequency subsetting", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 1,
                                          minfreq = 500,
                                          maxfreq = 2000))
})

test_that("`ss_pairdis()` works as expected when q = 2 and frequency subsetting", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 2,
                                          minfreq = 500,
                                          maxfreq = 2000))
})

# No hierarchy but time subsetting

test_that("`ss_pairdis()` works as expected when q = 0 and temporal subsetting", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 0,
                                          mintime = "06:00:00",
                                          maxtime = "18:00:00"))
})

test_that("`ss_pairdis()` works as expected when q = 1 and temporal subsetting", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 1,
                                          mintime = "06:00:00",
                                          maxtime = "18:00:00"))
})

test_that("`ss_pairdis()` works as expected when q = 2 and temporal subsetting", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 2,
                                          mintime = "06:00:00",
                                          maxtime = "18:00:00"))
})

# No hierarchy but time and frequency subsetting

test_that("`ss_pairdis()` works as expected when q = 0 and spectro-temporal subsetting", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 0,
                                          mintime = "06:00:00",
                                          maxtime = "18:00:00",
                                          minfreq = 500,
                                          maxfreq = 2000))
})

test_that("`ss_pairdis()` works as expected when q = 1 and spectro-temporal subsetting", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 1,
                                          mintime = "06:00:00",
                                          maxtime = "18:00:00",
                                          minfreq = 500,
                                          maxfreq = 2000))
})

test_that("`ss_pairdis()` works as expected when q = 2 and spectro-temporal subsetting", {
  expect_snapshot(soundscapeR::ss_pairdis(soundscape_list = soundscape_obj_CVR_case_study,
                                          qvalue = 2,
                                          mintime = "06:00:00",
                                          maxtime = "18:00:00",
                                          minfreq = 500,
                                          maxfreq = 2000))
})

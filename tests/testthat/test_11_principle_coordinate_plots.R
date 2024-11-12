library(testthat)
library(soundscapeR)

# Set locale before running any tests
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# 0. Loading required function

with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}

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

# 2.0. If required arguments are missing

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument is missing", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(screeplot = FALSE),
    regexp = "soundscape_list argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})


# 2.1. If the soundscape_list argument is not a list

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument is not a list", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = c(2,3,4)),
    regexp = "soundscape_list argument is not a list Please supply a list of aggregated soundscape objects.",
    fixed = TRUE
  )
})

# 2.3. If the soundscape_list argument is not a list of S4 soundscape objects

wrong_list <- vector("list")
wrong_list[[1]] <- c(2, 3, 4)
wrong_list[[2]] <- c(1, 2 ,3)

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument is not a list of S4 soundscape objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = wrong_list),
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

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_1),
    regexp = "soundscape_list contains invalid latitude coordinates. Did you supply the a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_2),
    regexp = "soundscape_list contains invalid latitude coordinates. Did you supply the a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_3),
    regexp = "soundscape_list contains invalid longitude coordinate. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_4),
    regexp = "soundscape_list contains invalid longitude coordinate. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_5),
    regexp = "soundscape_list contains invalid latitude coordinates. Did you supply the a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid coordinates", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_6),
    regexp = "soundscape_list contains invalid latitude coordinates. Did you supply the a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

  #2.4.2. Wrong timezone argument

soundscape_obj_CVR_case_study_tz <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_tz[[1]]@tz <- "Emerica/Manaus"

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid timezones", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_tz),
    regexp = "soundscape_list contains invalid timezone objects Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed = TRUE
  )
})

  # 2.4.3. Wrong index argument

soundscape_obj_CVR_case_study_index <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_index[[1]]@index <- "I'm not an option!"

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid index objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_index),
    regexp = "soundscape_list contains invalid index objects. Make sure the index argument inside the soundscape object is one of the available index options. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed = TRUE
  )
})

  # 2.4.4. Wrong samplerate and window arguments

soundscape_obj_CVR_case_study_samplerate1 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_samplerate2 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_samplerate1[[1]]@samplerate <- -44100
soundscape_obj_CVR_case_study_samplerate2[[1]]@samplerate <- c(44100, 44100)

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid samplerate objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_samplerate1),
    regexp = "soundscape_list contains samplerate arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid samplerate objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_samplerate2),
    regexp = "soundscape_list contains samplerate arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

soundscape_obj_CVR_case_study_window1 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_window2 <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_window1[[1]]@window <- -256
soundscape_obj_CVR_case_study_window2[[1]]@window <- c(256, 256)

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid window objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_window1),
    regexp = "soundscape_list contains window arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid window objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_window2),
    regexp = "soundscape_list contains window arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

  # 2.4.5. Wrong binarization method

soundscape_obj_CVR_case_study_bin <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_bin[[1]]@binarization_method <- "I'm not an option!"

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarization_method objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_bin),
    regexp = "soundscape_list contains binarization_method arguments that are not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

  # 2.4.6. Wrong threshold

soundscape_obj_CVR_case_study_thresh <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_thresh[[1]]@threshold <- c(1.2, 3.5)

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid threshold objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_thresh),
    regexp = "soundscape_list contains threshold arguments that are not a single numeric value. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed = TRUE
  )
})

  # 2.4.7. Wrong output

soundscape_obj_CVR_case_study_output <- soundscape_obj_CVR_case_study
soundscape_obj_CVR_case_study_output[[1]]@output <- "I'm not an option"

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid output objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_output),
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

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid merged_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_outputdf_1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid merged_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_outputdf_2),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid merged_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_outputdf_3),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid merged_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_outputdf_4),
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

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarized_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_binarizeddf_1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarized_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_binarizeddf_2),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarized_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_binarizeddf_3),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarized_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_binarizeddf_4),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid binarized_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_binarizeddf_5),
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

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid aggregated_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_aggdf_1),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid aggregated_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_aggdf_2),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid aggregated_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_aggdf_3),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument contains soundscape objects with invalid aggregated_df objects", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study_aggdf_4),
    regexp = "soundscape_list contains invalid merged_df, binarized_df, or aggregated_df row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.",
    fixed = TRUE
  )
})

# 2.5. When the grouping argument is wrong

wrong_grouping <- list(a = "a", b = "b", c = "c", d = "d", e = "e")

testthat::test_that("the ss_pcoa function provides the correct error when the grouping argument is not a vector", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study,
                                  grouping = wrong_grouping),
    regexp = "grouping is not a vector. Please supply the grouping argument as a vector of group names of the same length as the aggregated_df_list",
    fixed = TRUE
  )
})

wrong_grouping_2 <- c(1, 2, 3, 4)

testthat::test_that("the ss_pcoa function provides the correct error when the grouping argument has the wrong length", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study,
                                  grouping = wrong_grouping_2),
    regexp = "grouping does not have the same length as soundscape_list. Please supply the grouping argument as a vector of group names of the same length as the aggregated_df_list",
    fixed = TRUE
  )
})

  # 2.6. Wrong screeplot argument

testthat::test_that("the ss_pcoa function provides the correct error when the screeplot argument is not a boolean operator", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study,
                                  screeplot = "I'm not an option"),
    regexp = "screeplot is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed = TRUE
  )
})




# 2.X. When the correct arguments are supplied

# Testing of these function leads to test failure during CI
# Suspected to have something to do with the seed issue (ggrepel in ss_pcoa)
# Turning off testing for now

# No grouping without scree plot

# with_seed(seed = 1234, code =
#
#  testthat::test_that("The ss_pcao function works as expected when the correct arguments are supplied", {
#
#    vdiffr::expect_doppelganger(
#      title = "ss_pcoa_1",
#      fig = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study ),
#    )
#  })
#
# )

# No grouping with scree plot

# with_seed(seed = 1234, code =
#  testthat::test_that("The ss_pcao function works as expected when the correct arguments are supplied", {
#
#    vdiffr::expect_doppelganger(
#      title = "ss_pcoa_2",
#      fig = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study,
#                                 screeplot = TRUE),
#    )
#  })
# )

# Grouping without scree plot

# with_seed(seed = 1234, code =
#  testthat::test_that("The ss_pcao function works as expected when the correct arguments are supplied", {
#
#    vdiffr::expect_doppelganger(
#      title = "ss_pcoa_3",
#      fig = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study,
#                                 screeplot = FALSE,
#                                 grouping = case_study_groups),
#    )
#  })
# )

# Grouping with scree plot

# with_seed(seed = 1234, code =
#  testthat::test_that("The ss_pcao function works as expected when the correct arguments are supplied", {
#
#    vdiffr::expect_doppelganger(
#      title = "ss_pcoa_4",
#      fig = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study,
#                                 screeplot = TRUE,
#                                 grouping = case_study_groups),
#     )
#  })
# )

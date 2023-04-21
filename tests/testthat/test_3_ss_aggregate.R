library(testthat)
library(soundscapeR)

# 1. Load the merged '.csv' data frame files,
# binarize the data frame and make wrong data frame types
# for testing purposes

fpath_CVR <- system.file("/extdata/ss_binarize_test/merged_soundscape_CVR.ssc",
                         package="soundscapeR")

merged_soundscape_CVR <- qs::qread(file = fpath_CVR)
merged_soundscape_CVR@fileloc <- substr(fpath_CVR, 0, nchar(fpath_CVR)-26)

soundscape_obj_CVR <- ss_binarize(merged_soundscape = merged_soundscape_CVR,
                                        method = "IsoData",
                                        value = NULL)

# 2. Start testing the ss_aggregate() function

  # 2.0. When some of the required arguments are missing

testthat::test_that("the ss_aggregate function provides the correct error when the soundscape_obj argument is missing", {

  testthat::expect_error(
    object = ss_aggregate(output = "incidence_freq"),
    regexp = "soundscape_obj argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

  # 2.1. When all the correct arguments are supplied

testthat::test_that("the ss_aggregate function works as expected when the correct arguments are supplied", {

  aggregated_soundscape_CVR <- ss_aggregate(soundscape_obj = soundscape_obj_CVR,
                                            output = "incidence_freq")

  testthat::expect_s4_class(aggregated_soundscape_CVR, "soundscape")

  testthat::expect_true(lubridate::is.POSIXct(aggregated_soundscape_CVR@first_day))
  testthat::expect_equal(as.character(aggregated_soundscape_CVR@first_day),"2015-07-10")
  testthat::expect_true(is.numeric(aggregated_soundscape_CVR@lat))
  testthat::expect_equal(aggregated_soundscape_CVR@lat,-1.7332515613268331)
  testthat::expect_true(is.numeric(aggregated_soundscape_CVR@lon))
  testthat::expect_equal(aggregated_soundscape_CVR@lon,-59.65394067433209)
  testthat::expect_true(is.character(aggregated_soundscape_CVR@tz))
  testthat::expect_equal(aggregated_soundscape_CVR@tz, "America/Manaus")
  testthat::expect_true(lubridate::is.POSIXct(aggregated_soundscape_CVR@sunrise))
  testthat::expect_equal(as.character(aggregated_soundscape_CVR@sunrise),
                         "2015-07-10 06:04:16")
  testthat::expect_true(lubridate::is.POSIXct(aggregated_soundscape_CVR@sunset))
  testthat::expect_equal(as.character(aggregated_soundscape_CVR@sunset),
                         "2015-07-10 18:05:47")
  testthat::expect_true(assertthat::is.dir(aggregated_soundscape_CVR@fileloc))
  testthat::expect_true(assertthat::is.readable(aggregated_soundscape_CVR@fileloc))
  testthat::expect_true(is.character(aggregated_soundscape_CVR@index))
  testthat::expect_equal(aggregated_soundscape_CVR@index, "CVR")
  testthat::expect_true(is.double(aggregated_soundscape_CVR@samplerate))
  testthat::expect_equal(aggregated_soundscape_CVR@samplerate, 44100)
  testthat::expect_true(is.double(aggregated_soundscape_CVR@window))
  testthat::expect_equal(aggregated_soundscape_CVR@window, 256)

  testthat::expect_true(is.character(aggregated_soundscape_CVR@binarization_method))
  testthat::expect_equal(aggregated_soundscape_CVR@binarization_method, "IsoData")
  testthat::expect_true(is.double(aggregated_soundscape_CVR@threshold))
  testthat::expect_equal(aggregated_soundscape_CVR@threshold,
                         as.double(ss_threshold(df = merged_soundscape_CVR@merged_df,
                                                method = "IsoData")))
  testthat::expect_true(is.character(aggregated_soundscape_CVR@output))
  testthat::expect_equal(aggregated_soundscape_CVR@output, "incidence_freq")

  testthat::expect_true(is.data.frame(aggregated_soundscape_CVR@merged_df))
  testthat::expect_equal(dim(aggregated_soundscape_CVR@merged_df),
                         dim(merged_soundscape_CVR@merged_df))
  testthat::expect_equal(dim(aggregated_soundscape_CVR@merged_df),
                         dim(soundscape_obj_CVR@merged_df))
  testthat::expect_true(all(apply(aggregated_soundscape_CVR@merged_df, 2, function(y) all(is.numeric(y)))))
  testthat::expect_true(assertthat::not_empty(aggregated_soundscape_CVR@merged_df))
  testthat::expect_true(assertthat::noNA(aggregated_soundscape_CVR@merged_df))

  testthat::expect_true(is.data.frame(aggregated_soundscape_CVR@binarized_df))
  testthat::expect_equal(dim(aggregated_soundscape_CVR@binarized_df),
                         dim(merged_soundscape_CVR@merged_df))
  testthat::expect_equal(dim(aggregated_soundscape_CVR@binarized_df),
                         dim(soundscape_obj_CVR@merged_df))
  testthat::expect_true(min(aggregated_soundscape_CVR@binarized_df)==0)
  testthat::expect_true(max(aggregated_soundscape_CVR@binarized_df)==1)

  testthat::expect_equal(nrow(aggregated_soundscape_CVR@aggregated_df),
                         nrow(aggregated_soundscape_CVR@merged_df))
  testthat::expect_equal(nrow(aggregated_soundscape_CVR@aggregated_df),
                         nrow(aggregated_soundscape_CVR@binarized_df))
  testthat::expect_equal(ncol(aggregated_soundscape_CVR@aggregated_df),
                         length(unique(colnames(aggregated_soundscape_CVR@merged_df))))
  testthat::expect_equal(ncol(aggregated_soundscape_CVR@aggregated_df),
                         length(unique(colnames(aggregated_soundscape_CVR@binarized_df))))

  testthat::expect_true(max(aggregated_soundscape_CVR@aggregated_df)<=1)
  testthat::expect_true(min(aggregated_soundscape_CVR@aggregated_df)>=0)

  testthat::expect_true(is.list(aggregated_soundscape_CVR@aggregated_df_per_time))
  testthat::expect_true(length(aggregated_soundscape_CVR@aggregated_df_per_time) == length(unique(colnames(aggregated_soundscape_CVR@merged_df))))
  testthat::expect_true(length(aggregated_soundscape_CVR@aggregated_df_per_time) == length(unique(colnames(aggregated_soundscape_CVR@binarized_df))))

  testthat::expect_true(all(
    sapply(aggregated_soundscape_CVR@aggregated_df_per_time, function(x) nrow(x)==nrow(aggregated_soundscape_CVR@merged_df))))
  testthat::expect_true(all(
    sapply(aggregated_soundscape_CVR@aggregated_df_per_time, function(x) nrow(x)==nrow(aggregated_soundscape_CVR@binarized_df))))

  testthat::expect_equal(lapply(aggregated_soundscape_CVR@aggregated_df_per_time,
                                function(x) ncol(x)),
                         aggregated_soundscape_CVR@effort_per_time)

  testthat::expect_true(is.list(aggregated_soundscape_CVR@effort_per_time))
  testthat::expect_equal(as.list(table(colnames(aggregated_soundscape_CVR@merged_df))),
                        aggregated_soundscape_CVR@effort_per_time)
})

testthat::test_that("the ss_aggregate function works as expected when the correct arguments are supplied", {

  aggregated_soundscape_CVR <- ss_aggregate(soundscape_obj = soundscape_obj_CVR,
                                            output = "raw")

  testthat::expect_s4_class(aggregated_soundscape_CVR, "soundscape")

  testthat::expect_true(lubridate::is.POSIXct(aggregated_soundscape_CVR@first_day))
  testthat::expect_equal(as.character(aggregated_soundscape_CVR@first_day),"2015-07-10")
  testthat::expect_true(is.numeric(aggregated_soundscape_CVR@lat))
  testthat::expect_equal(aggregated_soundscape_CVR@lat,-1.7332515613268331)
  testthat::expect_true(is.numeric(aggregated_soundscape_CVR@lon))
  testthat::expect_equal(aggregated_soundscape_CVR@lon,-59.65394067433209)
  testthat::expect_true(is.character(aggregated_soundscape_CVR@tz))
  testthat::expect_equal(aggregated_soundscape_CVR@tz, "America/Manaus")
  testthat::expect_true(lubridate::is.POSIXct(aggregated_soundscape_CVR@sunrise))
  testthat::expect_equal(as.character(aggregated_soundscape_CVR@sunrise),
                         "2015-07-10 06:04:16")
  testthat::expect_true(lubridate::is.POSIXct(aggregated_soundscape_CVR@sunset))
  testthat::expect_equal(as.character(aggregated_soundscape_CVR@sunset),
                         "2015-07-10 18:05:47")
  testthat::expect_true(assertthat::is.dir(aggregated_soundscape_CVR@fileloc))
  testthat::expect_true(assertthat::is.readable(aggregated_soundscape_CVR@fileloc))
  testthat::expect_true(is.character(aggregated_soundscape_CVR@index))
  testthat::expect_equal(aggregated_soundscape_CVR@index, "CVR")
  testthat::expect_true(is.double(aggregated_soundscape_CVR@samplerate))
  testthat::expect_equal(aggregated_soundscape_CVR@samplerate, 44100)
  testthat::expect_true(is.double(aggregated_soundscape_CVR@window))
  testthat::expect_equal(aggregated_soundscape_CVR@window, 256)

  testthat::expect_true(is.character(aggregated_soundscape_CVR@binarization_method))
  testthat::expect_equal(aggregated_soundscape_CVR@binarization_method, "IsoData")
  testthat::expect_true(is.double(aggregated_soundscape_CVR@threshold))
  testthat::expect_equal(aggregated_soundscape_CVR@threshold,
                         as.double(ss_threshold(df = merged_soundscape_CVR@merged_df,
                                                method = "IsoData")))
  testthat::expect_true(is.character(aggregated_soundscape_CVR@output))
  testthat::expect_equal(aggregated_soundscape_CVR@output, "raw")

  testthat::expect_true(is.data.frame(aggregated_soundscape_CVR@merged_df))
  testthat::expect_equal(dim(aggregated_soundscape_CVR@merged_df),
                         dim(merged_soundscape_CVR@merged_df))
  testthat::expect_equal(dim(aggregated_soundscape_CVR@merged_df),
                         dim(soundscape_obj_CVR@merged_df))
  testthat::expect_true(all(apply(aggregated_soundscape_CVR@merged_df, 2, function(y) all(is.numeric(y)))))
  testthat::expect_true(assertthat::not_empty(aggregated_soundscape_CVR@merged_df))
  testthat::expect_true(assertthat::noNA(aggregated_soundscape_CVR@merged_df))

  testthat::expect_true(is.data.frame(aggregated_soundscape_CVR@binarized_df))
  testthat::expect_equal(dim(aggregated_soundscape_CVR@binarized_df),
                         dim(merged_soundscape_CVR@merged_df))
  testthat::expect_equal(dim(aggregated_soundscape_CVR@binarized_df),
                         dim(soundscape_obj_CVR@merged_df))
  testthat::expect_true(min(aggregated_soundscape_CVR@binarized_df)==0)
  testthat::expect_true(max(aggregated_soundscape_CVR@binarized_df)==1)

  testthat::expect_equal(nrow(aggregated_soundscape_CVR@aggregated_df),
                         nrow(aggregated_soundscape_CVR@merged_df))
  testthat::expect_equal(nrow(aggregated_soundscape_CVR@aggregated_df),
                         nrow(aggregated_soundscape_CVR@binarized_df))
  testthat::expect_equal(ncol(aggregated_soundscape_CVR@aggregated_df),
                         length(unique(colnames(aggregated_soundscape_CVR@merged_df))))
  testthat::expect_equal(ncol(aggregated_soundscape_CVR@aggregated_df),
                         length(unique(colnames(aggregated_soundscape_CVR@binarized_df))))

  testthat::expect_true(max(aggregated_soundscape_CVR@aggregated_df)<= max(unlist(aggregated_soundscape_CVR@effort_per_time)))
  testthat::expect_true(min(aggregated_soundscape_CVR@aggregated_df)>=0)

  testthat::expect_true(is.list(aggregated_soundscape_CVR@aggregated_df_per_time))
  testthat::expect_true(length(aggregated_soundscape_CVR@aggregated_df_per_time) == length(unique(colnames(aggregated_soundscape_CVR@merged_df))))
  testthat::expect_true(length(aggregated_soundscape_CVR@aggregated_df_per_time) == length(unique(colnames(aggregated_soundscape_CVR@binarized_df))))

  testthat::expect_true(all(
    sapply(aggregated_soundscape_CVR@aggregated_df_per_time, function(x) nrow(x)==nrow(aggregated_soundscape_CVR@merged_df))))
  testthat::expect_true(all(
    sapply(aggregated_soundscape_CVR@aggregated_df_per_time, function(x) nrow(x)==nrow(aggregated_soundscape_CVR@binarized_df))))

  testthat::expect_equal(lapply(aggregated_soundscape_CVR@aggregated_df_per_time,
                                function(x) ncol(x)),
                         aggregated_soundscape_CVR@effort_per_time)

  testthat::expect_true(is.list(aggregated_soundscape_CVR@effort_per_time))
  testthat::expect_equal(as.list(table(colnames(aggregated_soundscape_CVR@merged_df))),
                        aggregated_soundscape_CVR@effort_per_time)
})

# 2.2. When the supplied soundscape_obj argument is wrong

    # 2.2.1. The soundscape_obj argument is not an S4 object of the type 'soundscape'

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj argument is not an S4-object of the type 'soundscape' ", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_CVR@merged_df,
                         output = "incidence_freq"),
    regexp = "soundscape_obj is not an S4-object of the type 'soundscape'. Please supply the soundscape_obj object produced by the ss_binarize() function. Consult the package documentation for further information.",
    fixed=TRUE
  )

})

  # 2.3. When the soundscape_obj elements are wrong

    # 2.3.1. The lat and lon arguments are wrong

soundscape_obj_coord_1 <- soundscape_obj_CVR
soundscape_obj_coord_2 <- soundscape_obj_CVR
soundscape_obj_coord_3 <- soundscape_obj_CVR
soundscape_obj_coord_4 <- soundscape_obj_CVR
soundscape_obj_coord_5 <- soundscape_obj_CVR
soundscape_obj_coord_6 <- soundscape_obj_CVR
soundscape_obj_coord_1@lat <- 91
soundscape_obj_coord_2@lat <- -91
soundscape_obj_coord_3@lon <- 181
soundscape_obj_coord_4@lon <- -181
soundscape_obj_coord_5@lat <- 91
soundscape_obj_coord_5@lon <- 181
soundscape_obj_coord_6@lat <- -91
soundscape_obj_coord_6@lon <- -181

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_coord_1,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_coord_2,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_coord_3,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@lon is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_coord_4,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@lon is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_coord_5,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_coord_6,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

    # 2.3.2. When the tz argument is wrong

soundscape_obj_tz <- soundscape_obj_CVR
soundscape_obj_tz@tz <- "Emarica/Manaus"

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj tz argument is wrong", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_tz,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@tz is not a recognized timezone. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed=TRUE
  )

})


    # 2.3.4. When the index argument is wrong

soundscape_obj_index <- soundscape_obj_CVR
soundscape_obj_index@index <- "I'm not an option!"

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj index argument is wrong", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_index,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@index is not a character string of one of the available index options. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed=TRUE
  )

})

    # 2.3.5. When the samplerate argument is wrong

soundscape_obj_samplerate1 <- soundscape_obj_CVR
soundscape_obj_samplerate2 <- soundscape_obj_CVR
soundscape_obj_samplerate1@samplerate <- -44100
soundscape_obj_samplerate2@samplerate <- c(44100, 44200)

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj samplerate argument is wrong", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_samplerate1,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@samplerate is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj samplerate argument is wrong", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_samplerate2,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@samplerate is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

    # 2.3.6. When the window argument is wrong

soundscape_obj_window1 <- soundscape_obj_CVR
soundscape_obj_window2 <- soundscape_obj_CVR
soundscape_obj_window1@window <- -256
soundscape_obj_window2@window <- c(256, 512)

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj window argument is wrong", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_window1,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@window is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj window argument is wrong", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_window2,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@window is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

    # 2.3.7. When the binarization_method argument is wrong

soundscape_obj_binmeth1 <- soundscape_obj_CVR
soundscape_obj_binmeth1@binarization_method <- "I'm not an option!"

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj binarization method argument is wrong", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_binmeth1,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@binarization_method is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

    # 2.3.8. When the threshold argument is wrong

soundscape_obj_thresh1 <- soundscape_obj_CVR
soundscape_obj_thresh1@threshold <- c(1.5, 1.6)

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj threshold argument is wrong", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_thresh1,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@threshold is not a single numeric value. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed=TRUE
  )

})

    # 2.3.9. When the output argument is wrong

soundscape_obj_output <- soundscape_obj_CVR
soundscape_obj_output@output <- "incidence_freq"

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj output argument is not NA", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_output,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@output is not NA. Did you supply a post-binarization or post-aggregation soundscape to the ss_threshold_check() function? Please supply the output of the ss_index_merge() function to this argument.",
    fixed=TRUE
  )

})

    # 2.3.10. The merged_df argument is wrong

soundscape_obj_merged_df1 <- soundscape_obj_CVR
soundscape_obj_merged_df2 <- soundscape_obj_CVR
soundscape_obj_merged_df3 <- soundscape_obj_CVR
soundscape_obj_merged_df4 <- soundscape_obj_CVR
soundscape_obj_merged_df5 <- soundscape_obj_CVR
soundscape_obj_merged_df1@merged_df <- soundscape_obj_merged_df1@merged_df[FALSE,]
soundscape_obj_merged_df2@merged_df[1,1] <- NA
soundscape_obj_merged_df3@merged_df[1,1] <- "I'm not numeric"
rownames(soundscape_obj_merged_df4@merged_df) <-
  seq(1,nrow(soundscape_obj_merged_df4@merged_df), 1)
colnames(soundscape_obj_merged_df5@merged_df) <-
  seq(1,ncol(soundscape_obj_merged_df5@merged_df), 1)

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj merged_df argument is empty", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_merged_df1,
                         output = "incidence_freq"),
    regexp = "soundscape_obj@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj merged_df argument contains NA values", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_merged_df2,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj merged_df argument contains non-numeric values", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_merged_df3,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj merged_df argument has incorrect row names", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_merged_df4,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@merged_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_binarize(). Make sure you're supplying the dataframe produced by the ss_binarize() function.",
    fixed=TRUE
  )

})

# testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj merged_df argument has incorrect column names", {
#
#   testthat::expect_error(
#     object = ss_aggregate(soundscape_obj = soundscape_obj_merged_df5,
#                           output = "incidence_freq"),
#     regexp = "soundscape_obj@merged_df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of ss_binarize(). Make sure you're supplying the dataframe produced by the ss_binarize() function.",
#     fixed=TRUE
#   )
#
# })

    # 2.3.11. The binarized_df argument is wrong

soundscape_obj_bindf1 <- soundscape_obj_CVR
soundscape_obj_bindf2 <- soundscape_obj_CVR
soundscape_obj_bindf3 <- soundscape_obj_CVR
soundscape_obj_bindf4 <- soundscape_obj_CVR
soundscape_obj_bindf5 <- soundscape_obj_CVR
soundscape_obj_bindf6 <- soundscape_obj_CVR
soundscape_obj_bindf1@binarized_df <- soundscape_obj_bindf1@binarized_df[FALSE,]
soundscape_obj_bindf2@binarized_df[1,1] <- NA
soundscape_obj_bindf3@binarized_df[1,1] <- "I'm not numeric"
rownames(soundscape_obj_bindf4@binarized_df) <-
  seq(1,nrow(soundscape_obj_bindf4@binarized_df), 1)
colnames(soundscape_obj_bindf5@binarized_df) <-
  seq(1,ncol(soundscape_obj_bindf5@binarized_df), 1)
soundscape_obj_bindf6@binarized_df[1,1] <- 25

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj binarized_df argument is empty", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_bindf1,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj binarized_df argument contains NA values", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_bindf2,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj binarized_df argument contains non-numeric values", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_bindf3,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_binarize() function? If so, something has gone wrong, please re-run the ss_binarize() function.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj binarized_df argument has incorrect row names", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_bindf4,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@binarized_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_binarize(). Make sure you're supplying the dataframe produced by the ss_binarize() function.",
    fixed=TRUE
  )

})

# testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj binarized_df argument has incorrect column names", {
#
#   testthat::expect_error(
#     object = ss_aggregate(soundscape_obj = soundscape_obj_bindf5,
#                           output = "incidence_freq"),
#     regexp = "soundscape_obj@binarized_df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of ss_binarize(). Make sure you're supplying the dataframe produced by the ss_binarize() function.",
#     fixed=TRUE
#   )
#
# })


testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj binarized_df argument is non-binary", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_bindf6,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@binarized_df has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_binarize() function.",
    fixed=TRUE
  )

})

    # 2.3.12. The aggregated_df argument and lists are wrong

soundscape_obj_missing <- soundscape_obj_CVR
soundscape_obj_NA_list1 <- soundscape_obj_CVR
soundscape_obj_NA_list2 <- soundscape_obj_CVR

soundscape_obj_missing@aggregated_df <- soundscape_obj_missing@merged_df
soundscape_obj_NA_list1@aggregated_df_per_time <- as.list(seq(1, 10, 1))
soundscape_obj_NA_list2@effort_per_time <- as.list(seq(1, 10, 1))

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj aggregated_df argument is not missing", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_missing,
                         output = "incidence_freq"),
    regexp = "soundscape_obj@aggregated_df is not a missing data frame. Did you supply a post-aggregation soundscape to the soundscape_obj argument of the ss_binarize() function? Please supply the output of the ss_binarize() function to this argument.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj aggregated_df_per_time argument is not a list of NAs", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_NA_list1,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@aggregated_df_per_time is not a list of NAs. Did you supply a post-aggregation soundscape to the soundscape_obj argument of the ss_binarize() function? Please supply the output of the ss_binarize() function to this argument.",
    fixed=TRUE
  )

})

testthat::test_that("the ss_aggregate function produces the correct error message when the soundscape_obj effort_per_time argument is not a list of NAs", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_NA_list2,
                          output = "incidence_freq"),
    regexp = "soundscape_obj@effort_per_time is not a list of NAs. Did you supply a post-aggregation soundscape to the soundscape_obj argument of the ss_binarize() function? Please supply the output of the ss_binarize() function to this argument.",
    fixed=TRUE
  )

})

  # 2.4. When the supplied output argument is wrong

testthat::test_that("the ss_aggregate function produces the correct error message when the supplied output argument is wrong", {

  testthat::expect_error(
    object = ss_aggregate(soundscape_obj = soundscape_obj_CVR,
                          output = "I'm not an option"),
    regexp = "output is not a character string of one of the available output options. Please consult package documentation for available options.",
    fixed=TRUE
  )

})


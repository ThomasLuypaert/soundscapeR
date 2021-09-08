library(testthat)
library(soundscapeR)

# 1. Load the path name to the index_calc output folder

fpath_output <- system.file("/extdata/index_calc/output",
                         package="soundscapeR")

# 2. Start testing the merge_csv function

  # 2.0. If some of the required arguments are missing

testthat::test_that("the merge_csv function provides the correct error when the fileloc argument is missing", {

  testthat::expect_error(
    object = merge_csv(samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "fileloc argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the samplerate argument is missing", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "samplerate argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the window argument is missing", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "window argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the index argument is missing", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "index argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the date argument is missing", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "date argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the lat argument is missing", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "lat argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the lon argument is missing", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       twilight = "sunlight"),
    regexp = "lon argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

    # 2.1. The correct arguments are provided

testthat::test_that("the merge_csv function works as expected when the correct arguments are supplied", {

  merged_soundscape <- merge_csv(fileloc = fpath_output,
                                 samplerate = 44100,
                                 window = 256,
                                 index = "CVR",
                                 date = "2015-09-05",
                                 lat = -1.915867928971629,
                                 lon = -59.48937990402315,
                                 twilight = "sunlight")

  testthat::expect_s4_class(merged_soundscape, "soundscape")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@first_day))
  testthat::expect_equal(as.character(merged_soundscape@first_day),"2015-09-05")
  testthat::expect_true(is.numeric(merged_soundscape@lat))
  testthat::expect_equal(merged_soundscape@lat,-1.915867928971629)
  testthat::expect_true(is.numeric(merged_soundscape@lon))
  testthat::expect_equal(merged_soundscape@lon,-59.48937990402315)
  testthat::expect_true(is.character(merged_soundscape@tz))
  testthat::expect_equal(merged_soundscape@tz, "America/Manaus")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@sunrise))
  testthat::expect_equal(as.character(merged_soundscape@sunrise),"2015-09-05 05:54:19")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@sunset))
  testthat::expect_equal(as.character(merged_soundscape@sunset),"2015-09-05 17:59:12")
  testthat::expect_true(assertthat::is.dir(merged_soundscape@fileloc))
  testthat::expect_true(assertthat::is.readable(merged_soundscape@fileloc))
  testthat::expect_true(is.character(merged_soundscape@index))
  testthat::expect_equal(merged_soundscape@index, "CVR")
  testthat::expect_true(is.double(merged_soundscape@samplerate))
  testthat::expect_equal(merged_soundscape@samplerate, 44100)
  testthat::expect_true(is.double(merged_soundscape@window))
  testthat::expect_equal(merged_soundscape@window, 256)
  testthat::expect_true(is.na(merged_soundscape@binarization_method))
  testthat::expect_true(is.na(merged_soundscape@threshold))
  testthat::expect_true(is.na(merged_soundscape@output))
  testthat::expect_true(is.data.frame(merged_soundscape@merged_df))
  testthat::expect_equal(dim(merged_soundscape@merged_df),c(128, 13))
  testthat::expect_true(limma::isNumeric(merged_soundscape@merged_df))
  testthat::expect_true(assertthat::not_empty(merged_soundscape@merged_df))
  testthat::expect_true(assertthat::noNA(merged_soundscape@merged_df))
  testthat::expect_true(is.data.frame(merged_soundscape@binarized_df))
  testthat::expect_equal(dim(merged_soundscape@binarized_df), c(1, 1))
  testthat::expect_equal(merged_soundscape@binarized_df[1, 1], "missing")
  testthat::expect_equal(dim(merged_soundscape@aggregated_df), c(1, 1))
  testthat::expect_equal(merged_soundscape@aggregated_df[1, 1], "missing")
  testthat::expect_true(is.list(merged_soundscape@aggregated_df_per_time))
  testthat::expect_true(all(
    sapply(merged_soundscape@aggregated_df_per_time, function(x) is.na(x))))
  testthat::expect_true(is.list(merged_soundscape@effort_per_time))
  testthat::expect_true(all(
    sapply(merged_soundscape@effort_per_time, function(x) is.na(x))))

})

testthat::test_that("the merge_csv function works as expected when the correct arguments are supplied", {

  merged_soundscape <- merge_csv(fileloc = fpath_output,
                                 samplerate = 44100,
                                 window = 256,
                                 index = "BGN",
                                 date = "2015-09-05",
                                 lat = -1.915867928971629,
                                 lon = -59.48937990402315,
                                 twilight = "sunlight")

  testthat::expect_s4_class(merged_soundscape, "soundscape")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@first_day))
  testthat::expect_equal(as.character(merged_soundscape@first_day),"2015-09-05")
  testthat::expect_true(is.numeric(merged_soundscape@lat))
  testthat::expect_equal(merged_soundscape@lat,-1.915867928971629)
  testthat::expect_true(is.numeric(merged_soundscape@lon))
  testthat::expect_equal(merged_soundscape@lon,-59.48937990402315)
  testthat::expect_true(is.character(merged_soundscape@tz))
  testthat::expect_equal(merged_soundscape@tz, "America/Manaus")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@sunrise))
  testthat::expect_equal(as.character(merged_soundscape@sunrise),"2015-09-05 05:54:19")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@sunset))
  testthat::expect_equal(as.character(merged_soundscape@sunset),"2015-09-05 17:59:12")
  testthat::expect_true(assertthat::is.dir(merged_soundscape@fileloc))
  testthat::expect_true(assertthat::is.readable(merged_soundscape@fileloc))
  testthat::expect_true(is.character(merged_soundscape@index))
  testthat::expect_equal(merged_soundscape@index, "BGN")
  testthat::expect_true(is.double(merged_soundscape@samplerate))
  testthat::expect_equal(merged_soundscape@samplerate, 44100)
  testthat::expect_true(is.double(merged_soundscape@window))
  testthat::expect_equal(merged_soundscape@window, 256)
  testthat::expect_true(is.na(merged_soundscape@binarization_method))
  testthat::expect_true(is.na(merged_soundscape@threshold))
  testthat::expect_true(is.na(merged_soundscape@output))
  testthat::expect_true(is.data.frame(merged_soundscape@merged_df))
  testthat::expect_equal(dim(merged_soundscape@merged_df),c(128, 13))
  testthat::expect_true(limma::isNumeric(merged_soundscape@merged_df))
  testthat::expect_true(assertthat::not_empty(merged_soundscape@merged_df))
  testthat::expect_true(assertthat::noNA(merged_soundscape@merged_df))
  testthat::expect_true(is.data.frame(merged_soundscape@binarized_df))
  testthat::expect_equal(dim(merged_soundscape@binarized_df), c(1, 1))
  testthat::expect_equal(merged_soundscape@binarized_df[1, 1], "missing")
  testthat::expect_equal(dim(merged_soundscape@aggregated_df), c(1, 1))
  testthat::expect_equal(merged_soundscape@aggregated_df[1, 1], "missing")
  testthat::expect_true(is.list(merged_soundscape@aggregated_df_per_time))
  testthat::expect_true(all(
    sapply(merged_soundscape@aggregated_df_per_time, function(x) is.na(x))))
  testthat::expect_true(is.list(merged_soundscape@effort_per_time))
  testthat::expect_true(all(
    sapply(merged_soundscape@effort_per_time, function(x) is.na(x))))

})

testthat::test_that("the merge_csv function works as expected when the correct arguments are supplied", {

  merged_soundscape <- merge_csv(fileloc = fpath_output,
                                 samplerate = 44100,
                                 window = 256,
                                 index = "OSC",
                                 date = "2015-09-05",
                                 lat = -1.915867928971629,
                                 lon = -59.48937990402315,
                                 twilight = "sunlight")

  testthat::expect_s4_class(merged_soundscape, "soundscape")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@first_day))
  testthat::expect_equal(as.character(merged_soundscape@first_day),"2015-09-05")
  testthat::expect_true(is.numeric(merged_soundscape@lat))
  testthat::expect_equal(merged_soundscape@lat,-1.915867928971629)
  testthat::expect_true(is.numeric(merged_soundscape@lon))
  testthat::expect_equal(merged_soundscape@lon,-59.48937990402315)
  testthat::expect_true(is.character(merged_soundscape@tz))
  testthat::expect_equal(merged_soundscape@tz, "America/Manaus")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@sunrise))
  testthat::expect_equal(as.character(merged_soundscape@sunrise),"2015-09-05 05:54:19")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@sunset))
  testthat::expect_equal(as.character(merged_soundscape@sunset),"2015-09-05 17:59:12")
  testthat::expect_true(assertthat::is.dir(merged_soundscape@fileloc))
  testthat::expect_true(assertthat::is.readable(merged_soundscape@fileloc))
  testthat::expect_true(is.character(merged_soundscape@index))
  testthat::expect_equal(merged_soundscape@index, "OSC")
  testthat::expect_true(is.double(merged_soundscape@samplerate))
  testthat::expect_equal(merged_soundscape@samplerate, 44100)
  testthat::expect_true(is.double(merged_soundscape@window))
  testthat::expect_equal(merged_soundscape@window, 256)
  testthat::expect_true(is.na(merged_soundscape@binarization_method))
  testthat::expect_true(is.na(merged_soundscape@threshold))
  testthat::expect_true(is.na(merged_soundscape@output))
  testthat::expect_true(is.data.frame(merged_soundscape@merged_df))
  testthat::expect_equal(dim(merged_soundscape@merged_df),c(256, 13))
  testthat::expect_true(limma::isNumeric(merged_soundscape@merged_df))
  testthat::expect_true(assertthat::not_empty(merged_soundscape@merged_df))
  testthat::expect_true(assertthat::noNA(merged_soundscape@merged_df))
  testthat::expect_true(is.data.frame(merged_soundscape@binarized_df))
  testthat::expect_equal(dim(merged_soundscape@binarized_df), c(1, 1))
  testthat::expect_equal(merged_soundscape@binarized_df[1, 1], "missing")
  testthat::expect_equal(dim(merged_soundscape@aggregated_df), c(1, 1))
  testthat::expect_equal(merged_soundscape@aggregated_df[1, 1], "missing")
  testthat::expect_true(is.list(merged_soundscape@aggregated_df_per_time))
  testthat::expect_true(all(
    sapply(merged_soundscape@aggregated_df_per_time, function(x) is.na(x))))
  testthat::expect_true(is.list(merged_soundscape@effort_per_time))
  testthat::expect_true(all(
    sapply(merged_soundscape@effort_per_time, function(x) is.na(x))))

})

  # 2.2. Correct error message when the wrong arguments are supplied

    # 2.2.1. The fileloc argument is wrong

testthat::test_that("the merge_csv function provides the correct error when the fileloc argument is not an existing directory", {

  testthat::expect_error(
    object = merge_csv(fileloc = paste0(fpath_output, "/IDontExist"),
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = paste0("Path ",
                    paste0("'", fpath_output, "/IDontExist", "'"),
                    " does not exist"),
    fixed = TRUE)


})

    # 2.2.2. The samplerate argument is wrong

testthat::test_that("the merge_csv function provides the correct error when the samplerate argument is not a single positive integer", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = as.factor(44100),
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "samplerate is not a single, positive integer. Consult the package documentation for more information on the samplerate.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the samplerate argument is not a single positive integer", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = -44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "samplerate is not a single, positive integer. Consult the package documentation for more information on the samplerate.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the samplerate argument is not a single positive integer", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = c(1, 2),
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "samplerate is not a single, positive integer. Consult the package documentation for more information on the samplerate.",
    fixed = TRUE)


})

    # 2.2.3. The window argument is wrong

testthat::test_that("the merge_csv function provides the correct error when the window argument is not a single positive integer", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = as.factor(256),
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "window is not a single, positive integer. Consult the package documentation for more information on the window.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the window argument is not a single positive integer", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = -256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "window is not a single, positive integer. Consult the package documentation for more information on the window.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the window argument is not a single positive integer", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = c(256, 512),
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "window is not a single, positive integer. Consult the package documentation for more information on the window.",
    fixed = TRUE)


})

    # 2.2.4. The index argument is wrong

testthat::test_that("the merge_csv function provides the correct error when the index argument is not a character string", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = as.factor("CVR"),
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "index is not a character string of one of the available spectral acoustic indices. Please consult package documentation for available options. Pay attention to capital letters and the presence of excess spaces.",
    fixed = TRUE)


})

testthat::test_that("the merge_csv function provides the correct error when the index argument is not one of the available index options", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "I'm not an option!",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "index is not a character string of one of the available spectral acoustic indices. Please consult package documentation for available options. Pay attention to capital letters and the presence of excess spaces.",
    fixed = TRUE)


})

    # 2.2.5. The date argument is wrong

testthat::test_that("the merge_csv function provides the correct error when the date argument is not a character string", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = as.factor("2015-09-05"),
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "date is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the date argument does not have the correct format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "05-09-2015",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the date argument does not have the correct format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "05/09/2015",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed = TRUE)

})

    # 2.2.6. The lat and lon arguments are wrong

testthat::test_that("the merge_csv function provides the correct error when the lat argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = "1°55'00.1",
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the lon argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = "59°28'25.7",
                       twilight = "sunlight"),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the lat argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = "1°54'57.2",
                       lon = "59°28'25.7",
                       twilight = "sunlight"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the lat argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = 91,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the lat argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -91,
                       lon = -59.48937990402315,
                       twilight = "sunlight"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the lon argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = 181,
                       twilight = "sunlight"),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the lon argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -181,
                       twilight = "sunlight"),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the lat argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = 91,
                       lon = 181,
                       twilight = "sunlight"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the lat argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -91,
                       lon = -181,
                       twilight = "sunlight"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE)

})

    # 2.2.7. The twilight argument is wrong

testthat::test_that("the merge_csv function provides the correct error when the twilight argument is not a character string", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = as.factor("sunlight")),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed = TRUE)

})

testthat::test_that("the merge_csv function provides the correct error when the twilight argument is not one of the available options", {

  testthat::expect_error(
    object = merge_csv(fileloc = fpath_output,
                       samplerate = 44100,
                       window = 256,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315,
                       twilight = "I'm not an option!"),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed = TRUE)

})

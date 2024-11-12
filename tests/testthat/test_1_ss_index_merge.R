library(testthat)
library(soundscapeR)

# Set locale before running any tests
Sys.setlocale("LC_ALL", "en_US.UTF-8")

# 1. Load the path name to the ss_index_merge output folder


extdata_files <- list.files(
  system.file("extdata/ss_index_merge_test",
    package = "soundscapeR"
  ),
  full.names = TRUE
)

if (any(extdata_files == "output$")) {
  fpath_output <- system.file("extdata/ss_index_merge_test/output",
    package = "soundscapeR"
  )
} else {
  out_dir <- gsub(pattern = "/output.zip", replacement = "", extdata_files)

  unzip(
    zipfile = extdata_files,
    exdir = out_dir
  )

  fpath_output <- system.file("extdata/ss_index_merge_test/output",
    package = "soundscapeR"
  )
}


# 2. Start testing the ss_index_merge function

# 2.0. If some of the required arguments are missing

testthat::test_that("the ss_index_merge function provides the correct error when the fileloc argument is missing", {
  testthat::expect_error(
    object = ss_index_merge(
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "fileloc argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the samplerate argument is missing", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "samplerate argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the window argument is missing", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "window argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the index argument is missing", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "index argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the date argument is missing", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "date argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the lat argument is missing", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lon = -59.48937990402315
    ),
    regexp = "lat argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the lon argument is missing", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629
    ),
    regexp = "lon argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

# 2.1. The correct arguments are provided

testthat::test_that("the ss_index_merge function works as expected when the correct arguments are supplied", {
  merged_soundscape <- ss_index_merge(
    fileloc = fpath_output,
    samplerate = 44100,
    window = 256,
    index = "CVR",
    date = "2015-09-05",
    lat = -1.915867928971629,
    lon = -59.48937990402315
  )

  testthat::expect_s4_class(merged_soundscape, "soundscape")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@first_day))
  testthat::expect_equal(as.character(merged_soundscape@first_day), "2015-09-05")
  testthat::expect_true(is.numeric(merged_soundscape@lat))
  testthat::expect_equal(merged_soundscape@lat, -1.915867928971629)
  testthat::expect_true(is.numeric(merged_soundscape@lon))
  testthat::expect_equal(merged_soundscape@lon, -59.48937990402315)
  testthat::expect_true(is.character(merged_soundscape@tz))
  testthat::expect_equal(merged_soundscape@tz, "America/Manaus")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@sunrise))
  testthat::expect_equal(as.character(merged_soundscape@sunrise), "2015-09-05 05:55:49")
  testthat::expect_true(lubridate::is.POSIXct(merged_soundscape@sunset))
  testthat::expect_equal(as.character(merged_soundscape@sunset), "2015-09-05 18:00:41")
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
  testthat::expect_equal(dim(merged_soundscape@merged_df), c(128, 12))
  testthat::expect_true(all(apply(merged_soundscape@merged_df, 2, function(y) all(is.numeric(y)))))
  testthat::expect_true(assertthat::not_empty(merged_soundscape@merged_df))
  testthat::expect_true(assertthat::noNA(merged_soundscape@merged_df))
  testthat::expect_true(is.data.frame(merged_soundscape@binarized_df))
  testthat::expect_equal(dim(merged_soundscape@binarized_df), c(1, 1))
  testthat::expect_equal(merged_soundscape@binarized_df[1, 1], "missing")
  testthat::expect_equal(dim(merged_soundscape@aggregated_df), c(1, 1))
  testthat::expect_equal(merged_soundscape@aggregated_df[1, 1], "missing")
  testthat::expect_true(is.list(merged_soundscape@aggregated_df_per_time))
  testthat::expect_true(all(
    sapply(merged_soundscape@aggregated_df_per_time, function(x) is.na(x))
  ))
  testthat::expect_true(is.list(merged_soundscape@effort_per_time))
  testthat::expect_true(all(
    sapply(merged_soundscape@effort_per_time, function(x) is.na(x))
  ))
})



# 2.2. Correct error message when the wrong arguments are supplied

# 2.2.1. The fileloc argument is wrong

testthat::test_that("the ss_index_merge function provides the correct error when the fileloc argument is not an existing directory", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = paste0(fpath_output, "/IDontExist"),
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = paste0(
      "Path ",
      paste0("'", fpath_output, "/IDontExist", "'"),
      " does not exist"
    ),
    fixed = TRUE
  )
})

# 2.2.2. The samplerate argument is wrong

testthat::test_that("the ss_index_merge function provides the correct error when the samplerate argument is not a single positive integer", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = as.factor(44100),
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "samplerate is not a single, positive integer. Consult the package documentation for more information on the samplerate.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the samplerate argument is not a single positive integer", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = -44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "samplerate is not a single, positive integer. Consult the package documentation for more information on the samplerate.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the samplerate argument is not a single positive integer", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = c(1, 2),
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "samplerate is not a single, positive integer. Consult the package documentation for more information on the samplerate.",
    fixed = TRUE
  )
})

# 2.2.3. The window argument is wrong

testthat::test_that("the ss_index_merge function provides the correct error when the window argument is not a single positive integer", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = as.factor(256),
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "window is not a single, positive integer. Consult the package documentation for more information on the window.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the window argument is not a single positive integer", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = -256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "window is not a single, positive integer. Consult the package documentation for more information on the window.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the window argument is not a single positive integer", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = c(256, 512),
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "window is not a single, positive integer. Consult the package documentation for more information on the window.",
    fixed = TRUE
  )
})

# 2.2.4. The index argument is wrong

testthat::test_that("the ss_index_merge function provides the correct error when the index argument is not a character string", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = as.factor("CVR"),
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "index is not a character string of one of the available spectral acoustic indices. Please consult package documentation for available options. Pay attention to capital letters and the presence of excess spaces.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the index argument is not one of the available index options", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "I'm not an option!",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "index is not a character string of one of the available spectral acoustic indices. Please consult package documentation for available options. Pay attention to capital letters and the presence of excess spaces.",
    fixed = TRUE
  )
})

# 2.2.5. The date argument is wrong

testthat::test_that("the ss_index_merge function provides the correct error when the date argument is not a character string", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = as.factor("2015-09-05"),
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "date is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the date argument does not have the correct format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "05-09-2015",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the date argument does not have the correct format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "05/09/2015",
      lat = -1.915867928971629,
      lon = -59.48937990402315
    ),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed = TRUE
  )
})

# 2.2.6. The lat and lon arguments are wrong

testthat::test_that("the ss_index_merge function provides the correct error when the lat argument is supplied in the degree-minute-second format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = "1°55'00.1",
      lon = -59.48937990402315
    ),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the lon argument is supplied in the degree-minute-second format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = "59°28'25.7"
    ),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the lat argument is supplied in the degree-minute-second format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = "1°54'57.2",
      lon = "59°28'25.7"
    ),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the lat argument is supplied in the degree-minute-second format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = 91,
      lon = -59.48937990402315
    ),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the lat argument is supplied in the degree-minute-second format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -91,
      lon = -59.48937990402315
    ),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the lon argument is supplied in the degree-minute-second format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = 181
    ),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the lon argument is supplied in the degree-minute-second format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -1.915867928971629,
      lon = -181
    ),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the lat argument is supplied in the degree-minute-second format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = 91,
      lon = 181
    ),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_index_merge function provides the correct error when the lat argument is supplied in the degree-minute-second format", {
  testthat::expect_error(
    object = ss_index_merge(
      fileloc = fpath_output,
      samplerate = 44100,
      window = 256,
      index = "CVR",
      date = "2015-09-05",
      lat = -91,
      lon = -181
    ),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})


# Removing the unzipped output folder

unlink(fpath_output, recursive = TRUE)

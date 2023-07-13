library(testthat)
library(soundscapeR)

# 1. Load merged csv data frame files and wrong data frame
# types for testing purposes

fpath_CVR <- system.file("extdata/ss_binarize_test/merged_soundscape_CVR.ssc",
  package = "soundscapeR"
)

merged_soundscape_CVR <- qs::qread(file = fpath_CVR)
merged_soundscape_CVR@fileloc <- substr(fpath_CVR, 0, nchar(fpath_CVR) - 26)

merged_df_CVR_empty <- merged_soundscape_CVR
merged_df_CVR_empty@merged_df <- merged_soundscape_CVR@merged_df[FALSE, ]

merged_df_CVR_NAs <- merged_soundscape_CVR
merged_df_CVR_NAs@merged_df[1, 1] <- NA

merged_df_CVR_nonnum <- merged_soundscape_CVR
merged_df_CVR_nonnum@merged_df[1, 1] <- "I'm not numeric!"

merged_df_CVR_falserows <- merged_soundscape_CVR
rownames(merged_df_CVR_falserows@merged_df) <- seq(1, length(rownames(merged_df_CVR_falserows@merged_df)), 1)

merged_df_CVR_falsecols <- merged_soundscape_CVR
colnames(merged_df_CVR_falsecols@merged_df) <- seq(1, length(colnames(merged_df_CVR_falsecols@merged_df)), 1)

# 2. Start testing the ss_threshold function

# 2.0. If required argument is missing

testthat::test_that("the ss_threshold function provides the correct error when the merged_soundscape argument is missing", {
  testthat::expect_error(
    object = ss_threshold(method = "IJDefault"),
    regexp = "merged_soundscape argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_threshold function provides the correct error when the method argument is missing", {
  testthat::expect_error(
    object = ss_threshold(merged_soundscape = merged_soundscape_CVR@merged_df),
    regexp = "method argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

# 2.1. The correct arguments are provided

testthat::test_that("the ss_threshold function works for the 'IJDefault' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "IJDefault"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "IJDefault"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Huang' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Huang"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Huang"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Huang2' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Huang2"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Huang2"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Intermodes' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Intermodes"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Intermodes"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'IsoData' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "IsoData"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "IsoData"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Li' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Li"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Li"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'MaxEntropy' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "MaxEntropy"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "MaxEntropy"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Mean' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Mean"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Mean"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'MinErrorI' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "MinErrorI"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "MinErrorI"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Minimum' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Minimum"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Minimum"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Moments' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Moments"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Moments"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Otsu' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Otsu"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Otsu"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Percentile' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Percentile"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Percentile"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'RenyiEntropy' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "RenyiEntropy"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "RenyiEntropy"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Shanbhag' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Shanbhag"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Shanbhag"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Triangle' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Triangle"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Triangle"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Yen' binarization method if the correct CVR data frame is provided", {
  function_var <- try(
    ss_threshold(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "Yen"
    ),
    silent = TRUE
  )

  test_var <- try(
    autothresholdr::auto_thresh(
      int_arr = as.integer(
        as.matrix(merged_soundscape_CVR@merged_df) * 100
      ), method = "Yen"
    ) / 100,
    silent = TRUE
  )

  testing_function <- function(x, y) {
    all(is.numeric(x[[1]]) &
      x[[1]] == y[[1]]) |
      all(class(x) == "try-error")
  }

  testthat::expect_true(testing_function(function_var, test_var))
})

testthat::test_that("the ss_threshold function works for the 'Mode' binarization method if the correct CVR data frame is provided", {
  function_var <- ss_threshold(
    merged_soundscape = merged_soundscape_CVR,
    method = "Mode"
  )

  testing_function_mode <- function(x) {
    uniqv <- unique(unlist(x@merged_df))
    uniqv[which.max(tabulate(match(unlist(x@merged_df), uniqv)))]
  }

  test_var <- testing_function_mode(merged_soundscape_CVR)

  testthat::expect_equal(function_var, test_var)
})


# 2.2. If wrong merged_soundscape argument

# 2.2.2. Argument is an empty data frame

testthat::test_that("the ss_threshold function provides the correct error message when the supplied merged_soundscape argument is an empty data frame", {
  testthat::expect_error(
    object = ss_threshold(
      merged_soundscape = merged_df_CVR_empty,
      method = "IJDefault"
    ),
    regexp = "merged_soundscape@merged_df is an empty dataframe. This functions builds on the output of ss_index_merge(). Make sure you're supplying the merged soundscape object produced by the ss_index_merge() function.", fixed = TRUE
  )
})

# 2.2.3. Argument is a data frame containing NAs

testthat::test_that("the ss_threshold function provides the correct error message when the supplied merged_soundscape argument is a data frame containing NA values", {
  testthat::expect_error(
    object = ss_threshold(
      merged_soundscape = merged_df_CVR_NAs,
      method = "IJDefault"
    ),
    regexp = "merged_soundscape@merged_df contains NA values. This functions builds on the output of ss_index_merge(). Make sure you're supplying the merged soundscape object produced by the ss_index_merge() function.", fixed = TRUE
  )
})

# 2.2.4. Argument is a data frame containing non numeric

testthat::test_that("the ss_threshold function provides the correct error message when the supplied merged_soundscape argument is a data frame containing non-numeric values", {
  testthat::expect_error(
    object = ss_threshold(
      merged_soundscape = merged_df_CVR_nonnum,
      method = "IJDefault"
    ),
    regexp = "merged_soundscape@merged_df contains non-numeric values. This functions builds on the output of ss_index_merge(). Make sure you're supplying the merged soundscape object produced by the ss_index_merge() function.", fixed = TRUE
  )
})

# 2.3. If correct merged_soundscape argument but wrong method argument

testthat::test_that("the ss_threshold function provides the correct error message when the supplied method argument is not one of the available options", {
  testthat::expect_error(
    object = ss_threshold(
      merged_soundscape = merged_soundscape_CVR,
      method = "I'm not an option"
    ),
    regexp = "method is not one of the available binarization methods. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.", fixed = TRUE
  )
})

# 3. Start testing the ss_get_mode function

# 3.0. If required argument is missing

testthat::test_that("the ss_get_mode function provides the correct error when the merged_soundscape argument is missing", {
  testthat::expect_error(
    object = ss_get_mode(),
    regexp = "merged_soundscape argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

# 3.1. Correct numeric data frame supplied

testthat::test_that("the ss_get_mode function works when a numeric data frame is supplied", {
  testing_function_mode <- function(x) {
    uniqv <- unique(unlist(x))
    uniqv[which.max(tabulate(match(unlist(x), uniqv)))]
  }

  function_var <- ss_get_mode(merged_soundscape = merged_soundscape_CVR)
  test_var <- testing_function_mode(merged_soundscape_CVR@merged_df)

  testthat::expect_equal(function_var, test_var)
})


# 3.3. merged_soundscape argument is a non-numeric data frame

testthat::test_that("the ss_get_mode function works when a numeric data frame is supplied", {
  testthat::expect_error(
    object = ss_get_mode(merged_soundscape = merged_df_CVR_nonnum),
    regexp = "merged_soundscape@merged_df is not a numeric dataframe. Please supply a valid argument to the function", fixed = TRUE
  )
})

# 4. Start testing the ss_binarize function

# 4.0. If required argument is missing

testthat::test_that("the ss_binarize function provides the correct error when the merged_soundscape argument is missing", {
  testthat::expect_error(
    object = ss_binarize(
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function provides the correct error when the method argument is missing", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_CVR,
      value = NULL
    ),
    regexp = "method argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function provides the correct error when the method argument is set to 'custom', but the value argument is missing", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_CVR,
      method = "Custom"
    ),
    regexp = "value argument is missing. If you set method to 'custom', please supply a value argument. Consult package documentation for options.",
    fixed = TRUE
  )
})

# 4.1. When the correct data frame, method
# (and potentially value) arguments are supplied

testthat::test_that("the ss_binarize function works as expected when the correct arguments are supplied", {
  binarized_soundscape_CVR <- ss_binarize(
    merged_soundscape = merged_soundscape_CVR,
    method = "IsoData",
    value = NULL
  )

  testthat::expect_s4_class(binarized_soundscape_CVR, "soundscape")
  testthat::expect_true(lubridate::is.POSIXct(binarized_soundscape_CVR@first_day))
  testthat::expect_equal(as.character(binarized_soundscape_CVR@first_day), "2015-07-10")
  testthat::expect_true(is.numeric(binarized_soundscape_CVR@lat))
  testthat::expect_equal(binarized_soundscape_CVR@lat, -1.7332515613268331)
  testthat::expect_true(is.numeric(binarized_soundscape_CVR@lon))
  testthat::expect_equal(binarized_soundscape_CVR@lon, -59.65394067433209)
  testthat::expect_true(is.character(binarized_soundscape_CVR@tz))
  testthat::expect_equal(binarized_soundscape_CVR@tz, "America/Manaus")
  testthat::expect_true(lubridate::is.POSIXct(binarized_soundscape_CVR@sunrise))
  testthat::expect_equal(
    as.character(binarized_soundscape_CVR@sunrise),
    "2015-07-10 06:04:16"
  )
  testthat::expect_true(lubridate::is.POSIXct(binarized_soundscape_CVR@sunset))
  testthat::expect_equal(
    as.character(binarized_soundscape_CVR@sunset),
    "2015-07-10 18:05:47"
  )
  testthat::expect_true(assertthat::is.dir(binarized_soundscape_CVR@fileloc))
  testthat::expect_true(assertthat::is.readable(binarized_soundscape_CVR@fileloc))
  testthat::expect_true(is.character(binarized_soundscape_CVR@index))
  testthat::expect_equal(binarized_soundscape_CVR@index, "CVR")
  testthat::expect_true(is.double(binarized_soundscape_CVR@samplerate))
  testthat::expect_equal(binarized_soundscape_CVR@samplerate, 44100)
  testthat::expect_true(is.double(binarized_soundscape_CVR@window))
  testthat::expect_equal(binarized_soundscape_CVR@window, 256)

  testthat::expect_true(is.character(binarized_soundscape_CVR@binarization_method))
  testthat::expect_equal(binarized_soundscape_CVR@binarization_method, "IsoData")
  testthat::expect_true(is.double(binarized_soundscape_CVR@threshold))
  testthat::expect_equal(
    binarized_soundscape_CVR@threshold,
    as.double(ss_threshold(
      merged_soundscape = merged_soundscape_CVR,
      method = "IsoData"
    ))
  )

  testthat::expect_true(is.na(binarized_soundscape_CVR@output))
  testthat::expect_true(is.data.frame(binarized_soundscape_CVR@merged_df))
  testthat::expect_equal(
    dim(binarized_soundscape_CVR@merged_df),
    dim(merged_soundscape_CVR@merged_df)
  )
  testthat::expect_true(all(apply(binarized_soundscape_CVR@merged_df, 2, function(y) all(is.numeric(y)))))
  testthat::expect_true(assertthat::not_empty(binarized_soundscape_CVR@merged_df))
  testthat::expect_true(assertthat::noNA(binarized_soundscape_CVR@merged_df))

  testthat::expect_true(is.data.frame(binarized_soundscape_CVR@binarized_df))
  testthat::expect_equal(
    dim(binarized_soundscape_CVR@binarized_df),
    dim(merged_soundscape_CVR@merged_df)
  )
  testthat::expect_true(min(binarized_soundscape_CVR@binarized_df) == 0)
  testthat::expect_true(max(binarized_soundscape_CVR@binarized_df) == 1)


  testthat::expect_equal(dim(binarized_soundscape_CVR@aggregated_df), c(1, 1))
  testthat::expect_equal(binarized_soundscape_CVR@aggregated_df[1, 1], "missing")
  testthat::expect_true(is.list(binarized_soundscape_CVR@aggregated_df_per_time))
  testthat::expect_true(all(
    sapply(binarized_soundscape_CVR@aggregated_df_per_time, function(x) is.na(x))
  ))
  testthat::expect_true(is.list(binarized_soundscape_CVR@effort_per_time))
  testthat::expect_true(all(
    sapply(binarized_soundscape_CVR@effort_per_time, function(x) is.na(x))
  ))
})

testthat::test_that("the ss_binarize function works as expected when the correct arguments are supplied", {
  binarized_soundscape_CVR <- ss_binarize(
    merged_soundscape = merged_soundscape_CVR,
    method = "Custom",
    value = 0.1
  )

  testthat::expect_s4_class(binarized_soundscape_CVR, "soundscape")
  testthat::expect_true(lubridate::is.POSIXct(binarized_soundscape_CVR@first_day))
  testthat::expect_equal(as.character(binarized_soundscape_CVR@first_day), "2015-07-10")
  testthat::expect_true(is.numeric(binarized_soundscape_CVR@lat))
  testthat::expect_equal(binarized_soundscape_CVR@lat, -1.7332515613268331)
  testthat::expect_true(is.numeric(binarized_soundscape_CVR@lon))
  testthat::expect_equal(binarized_soundscape_CVR@lon, -59.65394067433209)
  testthat::expect_true(is.character(binarized_soundscape_CVR@tz))
  testthat::expect_equal(binarized_soundscape_CVR@tz, "America/Manaus")
  testthat::expect_true(lubridate::is.POSIXct(binarized_soundscape_CVR@sunrise))
  testthat::expect_equal(
    as.character(binarized_soundscape_CVR@sunrise),
    "2015-07-10 06:04:16"
  )
  testthat::expect_true(lubridate::is.POSIXct(binarized_soundscape_CVR@sunset))
  testthat::expect_equal(
    as.character(binarized_soundscape_CVR@sunset),
    "2015-07-10 18:05:47"
  )
  testthat::expect_true(assertthat::is.dir(binarized_soundscape_CVR@fileloc))
  testthat::expect_true(assertthat::is.readable(binarized_soundscape_CVR@fileloc))
  testthat::expect_true(is.character(binarized_soundscape_CVR@index))
  testthat::expect_equal(binarized_soundscape_CVR@index, "CVR")
  testthat::expect_true(is.double(binarized_soundscape_CVR@samplerate))
  testthat::expect_equal(binarized_soundscape_CVR@samplerate, 44100)
  testthat::expect_true(is.double(binarized_soundscape_CVR@window))
  testthat::expect_equal(binarized_soundscape_CVR@window, 256)

  testthat::expect_true(is.character(binarized_soundscape_CVR@binarization_method))
  testthat::expect_equal(binarized_soundscape_CVR@binarization_method, "Custom")
  testthat::expect_true(is.double(binarized_soundscape_CVR@threshold))
  testthat::expect_equal(
    binarized_soundscape_CVR@threshold,
    as.double(ss_threshold(
      merged_soundscape = merged_soundscape_CVR,
      method = "IsoData"
    ))
  )

  testthat::expect_true(is.na(binarized_soundscape_CVR@output))
  testthat::expect_true(is.data.frame(binarized_soundscape_CVR@merged_df))
  testthat::expect_equal(
    dim(binarized_soundscape_CVR@merged_df),
    dim(merged_soundscape_CVR@merged_df)
  )
  testthat::expect_true(all(apply(binarized_soundscape_CVR@merged_df, 2, function(y) all(is.numeric(y)))))
  testthat::expect_true(assertthat::not_empty(binarized_soundscape_CVR@merged_df))
  testthat::expect_true(assertthat::noNA(binarized_soundscape_CVR@merged_df))

  testthat::expect_true(is.data.frame(binarized_soundscape_CVR@binarized_df))
  testthat::expect_equal(
    dim(binarized_soundscape_CVR@binarized_df),
    dim(merged_soundscape_CVR@merged_df)
  )
  testthat::expect_true(min(binarized_soundscape_CVR@binarized_df) == 0)
  testthat::expect_true(max(binarized_soundscape_CVR@binarized_df) == 1)


  testthat::expect_equal(dim(binarized_soundscape_CVR@aggregated_df), c(1, 1))
  testthat::expect_equal(binarized_soundscape_CVR@aggregated_df[1, 1], "missing")
  testthat::expect_true(is.list(binarized_soundscape_CVR@aggregated_df_per_time))
  testthat::expect_true(all(
    sapply(binarized_soundscape_CVR@aggregated_df_per_time, function(x) is.na(x))
  ))
  testthat::expect_true(is.list(binarized_soundscape_CVR@effort_per_time))
  testthat::expect_true(all(
    sapply(binarized_soundscape_CVR@effort_per_time, function(x) is.na(x))
  ))
})

# 4.2. When the supplied merged_soundscape argument is wrong

# 4.2.1. The merged_soundscape argument is not an S4 object of the type 'soundscape'

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape argument is not an S4-object of the type 'soundscape' ", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_CVR@merged_df,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape is not an S4-object of the type 'soundscape'. Please supply the merged_soundscape object produced by the ss_index_merge() function. Consult the package documentation for further information.",
    fixed = TRUE
  )
})

# 4.3. When the merged_soundscape elements are wrong

# 4.3.1. The lat and lon arguments are wrong

merged_soundscape_coord_1 <- merged_soundscape_CVR
merged_soundscape_coord_2 <- merged_soundscape_CVR
merged_soundscape_coord_3 <- merged_soundscape_CVR
merged_soundscape_coord_4 <- merged_soundscape_CVR
merged_soundscape_coord_5 <- merged_soundscape_CVR
merged_soundscape_coord_6 <- merged_soundscape_CVR
merged_soundscape_coord_1@lat <- 91
merged_soundscape_coord_2@lat <- -91
merged_soundscape_coord_3@lon <- 181
merged_soundscape_coord_4@lon <- -181
merged_soundscape_coord_5@lat <- 91
merged_soundscape_coord_5@lon <- 181
merged_soundscape_coord_6@lat <- -91
merged_soundscape_coord_6@lon <- -181

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_coord_1,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@lat is not a valid coordinate. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_coord_2,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@lat is not a valid coordinate. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_coord_3,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@lon is not a valid coordinate. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_coord_4,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@lon is not a valid coordinate. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_coord_5,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@lat is not a valid coordinate. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_coord_6,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@lat is not a valid coordinate. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

# 4.3.3. When the tz argument is wrong

merged_soundscape_tz <- merged_soundscape_CVR
merged_soundscape_tz@tz <- "Emarica/Manaus"

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape tz argument is wrong", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_tz,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@tz is not a recognized timezone. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed = TRUE
  )
})

# 4.3.4. When the fileloc argument is wrong

# merged_soundscape_fileloc <- merged_soundscape_CVR
# merged_soundscape_fileloc@fileloc <- paste0(getwd(), "/IDontExist")
#
# testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape fileloc argument is wrong", {
#
#   testthat::expect_error(
#     object = ss_binarize(merged_soundscape = merged_soundscape_fileloc,
#                          method = "IJDefault",
#                          value=NULL),
#     regexp = paste0("Path ",
#                     paste0("'", getwd(), "/IDontExist", "'"),
#                     " does not exist"),
#     fixed=TRUE
#   )
#
# })

# 4.3.5. When the index argument is wrong

merged_soundscape_index <- merged_soundscape_CVR
merged_soundscape_index@index <- "I'm not an option!"

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape index argument is wrong", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_index,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@index is not a character string of one of the available index options. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed = TRUE
  )
})

# 4.3.6. When the samplerate argument is wrong

merged_soundscape_samplerate1 <- merged_soundscape_CVR
merged_soundscape_samplerate2 <- merged_soundscape_CVR
merged_soundscape_samplerate1@samplerate <- -44100
merged_soundscape_samplerate2@samplerate <- c(44100, 44200)

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape samplerate argument is wrong", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_samplerate1,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@samplerate is not a single positive integer. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape samplerate argument is wrong", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_samplerate2,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@samplerate is not a single positive integer. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

# 4.3.7. When the window argument is wrong

merged_soundscape_window1 <- merged_soundscape_CVR
merged_soundscape_window2 <- merged_soundscape_CVR
merged_soundscape_window1@window <- -256
merged_soundscape_window2@window <- c(256, 512)

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape window argument is wrong", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_window1,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@window is not a single positive integer. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape window argument is wrong", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_window2,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@window is not a single positive integer. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

# 4.3.8. The post-binarization or post-aggregation arguments are not NA

merged_soundscape_postbin1 <- merged_soundscape_CVR
merged_soundscape_postbin2 <- merged_soundscape_CVR
merged_soundscape_postaggr <- merged_soundscape_CVR
merged_soundscape_postbin1@binarization_method <- "IsoData"
merged_soundscape_postbin2@threshold <- 1.5
merged_soundscape_postaggr@output <- "raw"


testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape binarization_method argument is not NA", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_postbin1,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@binarization_method is not NA. Did you supply a post-binarization or post-aggregation soundscape to the ss_binarize() function? Please supply the output of the ss_index_merge() function to this argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape threshold argument is not NA", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_postbin2,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@threshold is not NA. Did you supply a post-binarization or post-aggregation soundscape to the ss_binarize() function? Please supply the output of the ss_index_merge() function to this argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape output argument is not NA", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_postaggr,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@output is not NA. Did you supply a post-binarization or post-aggregation soundscape to the ss_binarize() function? Please supply the output of the ss_index_merge() function to this argument.",
    fixed = TRUE
  )
})

# 4.3.9. The merged_merged_soundscape argument is wrong

merged_soundscape_merged_df1 <- merged_soundscape_CVR
merged_soundscape_merged_df2 <- merged_soundscape_CVR
merged_soundscape_merged_df3 <- merged_soundscape_CVR
merged_soundscape_merged_df4 <- merged_soundscape_CVR
merged_soundscape_merged_df5 <- merged_soundscape_CVR
merged_soundscape_merged_df1@merged_df <- merged_soundscape_merged_df1@merged_df[FALSE, ]
merged_soundscape_merged_df2@merged_df[1, 1] <- NA
merged_soundscape_merged_df3@merged_df[1, 1] <- "I'm not numeric"
rownames(merged_soundscape_merged_df4@merged_df) <-
  seq(1, nrow(merged_soundscape_merged_df4@merged_df), 1)
colnames(merged_soundscape_merged_df5@merged_df) <-
  seq(1, ncol(merged_soundscape_merged_df5@merged_df), 1)

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape merged_merged_soundscape argument is empty", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_merged_df1,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape merged_merged_soundscape argument contains NA values", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_merged_df2,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape merged_merged_soundscape argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_merged_df3,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the merged_soundscape argument produced using the ss_index_merge() function? If so, something has gone wrong, please re-run the ss_index_merge() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape merged_merged_soundscape argument has incorrect row names", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_merged_df4,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@merged_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_index_merge(). Make sure you're supplying the merged soundscape object produced by the ss_index_merge() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape merged_merged_soundscape argument has incorrect column names", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_merged_df5,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@merged_df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of ss_index_merge(). Make sure you're supplying the merged_soundscape object produced by the ss_index_merge() function.",
    fixed = TRUE
  )
})

# 4.3.10. The binarization and aggregation arguments are not missing / NAs

merged_soundscape_missing1 <- merged_soundscape_CVR
merged_soundscape_missing2 <- merged_soundscape_CVR
merged_soundscape_NA_list1 <- merged_soundscape_CVR
merged_soundscape_NA_list2 <- merged_soundscape_CVR

merged_soundscape_missing1@binarized_df <- merged_soundscape_missing1@merged_df
merged_soundscape_missing2@aggregated_df <- merged_soundscape_missing1@merged_df
merged_soundscape_NA_list1@aggregated_df_per_time <- as.list(seq(1, 10, 1))
merged_soundscape_NA_list2@effort_per_time <- as.list(seq(1, 10, 1))

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape binarized_merged_soundscape argument is not missing", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_missing1,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@binarized_df is not a missing data frame. Did you supply a post-binarization or post-aggregation merged_soundscape to the ss_binarize() function? Please supply the output of the ss_index_merge() function to this argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape aggregated_merged_soundscape argument is not missing", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_missing2,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@aggregated_df is not a missing data frame. Did you supply a post-binarization or post-aggregation merged_soundscape to the ss_binarize() function? Please supply the output of the ss_index_merge() function to this argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape aggregated_df_per_time argument is not a list of NAs", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_NA_list1,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@aggregated_df_per_time is not a list of NAs. Did you supply a post-binarization or post-aggregation merged_soundscape to the ss_binarize() function? Please supply the output of the ss_index_merge() function to this argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the merged_soundscape effort_per_time argument is not a list of NAs", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_NA_list2,
      method = "IJDefault",
      value = NULL
    ),
    regexp = "merged_soundscape@effort_per_time is not a list of NAs. Did you supply a post-binarization or post-aggregation merged_soundscape to the ss_binarize() function? Please supply the output of the ss_index_merge() function to this argument.",
    fixed = TRUE
  )
})

# 4.4. When the supplied method argument is wrong

testthat::test_that("the ss_binarize function produces the correct error message when the method argument is not supplied as a character string", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_CVR,
      method = as.factor("IJDefault"),
      value = NULL
    ),
    regexp = "method is not a character string. Please supply the binarization methods as a character string. Consult package documentation for available method options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.", fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the method argument is not one of the available options", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_CVR,
      method = "Not an option!",
      value = NULL
    ),
    regexp = "method is not one of the available binarization method options. Please consult package documentation for available  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.", fixed = TRUE
  )
})

# 4.4. When the supplied value argument is wrong

testthat::test_that("the ss_binarize function produces the correct error message when the format of the value argument is not the right format", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_CVR,
      method = "Custom",
      value = "Not the right format!"
    ),
    regexp = "value input is not in the right format. To choose a custom binarization threshold, supply a single numeric value.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_binarize function produces the correct error message when the length of the value argument is not correct", {
  testthat::expect_error(
    object = ss_binarize(
      merged_soundscape = merged_soundscape_CVR,
      method = "Custom",
      value = c(1, 2)
    ),
    regexp = "value input is not in the right format. To choose a custom binarization threshold, supply a single numeric value.",
    fixed = TRUE
  )
})

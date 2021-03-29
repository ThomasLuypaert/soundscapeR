library(testthat)
library(soundscapeR)

# 1. Load merged csv data frame files and wrong data frame
# types for testing purposes

fpath_CVR <- system.file("/extdata/merged_df/merged_df_CVR.csv",
                         package="soundscapeR")

merged_df_CVR <- read.csv(file = fpath_CVR,
                          header = TRUE,
                          sep = ",",
                          row.names = 1,
                          check.names = F)

merged_df_CVR_matrix <- as.matrix(merged_df_CVR)

merged_df_CVR_empty <- merged_df_CVR[FALSE,]

merged_df_CVR_NAs <- merged_df_CVR
merged_df_CVR_NAs[1,1] <- NA

merged_df_CVR_nonnum <- merged_df_CVR
merged_df_CVR_nonnum[1,1] <- "I'm not numeric!"

merged_df_CVR_falserows <- merged_df_CVR
rownames(merged_df_CVR_falserows) <- seq(1, length(rownames(merged_df_CVR_falserows)), 1)

merged_df_CVR_falsecols <- merged_df_CVR
colnames(merged_df_CVR_falsecols) <- seq(1, length(colnames(merged_df_CVR_falsecols)), 1)

# 2. Start testing the threshold_df function

  # 2.0. If required argument is missing

testthat::test_that("the threshold_df function provides the correct error when the df argument is missing", {

  testthat::expect_error(
    object = threshold_df(method = "IJDefault"),
    regexp = "df argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the threshold_df function provides the correct error when the method argument is missing", {

  testthat::expect_error(
    object = threshold_df(df = merged_df_CVR),
    regexp = "method argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

  # 2.1. The correct arguments are provided

testthat::test_that("the threshold_df function works for the 'IJDefault' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "IJDefault"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "IJDefault")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'IJDefault' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Huang' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Huang"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Huang")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Huang' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Huang2' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Huang2"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Huang2")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Huang2' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Intermodes' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Intermodes"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Intermodes")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Intermodes' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'IsoData' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "IsoData"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "IsoData")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'IsoData' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Li' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Li"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Li")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Li' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'MaxEntropy' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "MaxEntropy"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "MaxEntropy")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'MaxEntropy' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Mean' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Mean"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Mean")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Mean' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'MinErrorI' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "MinErrorI"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "MinErrorI")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'MinErrorI' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Minimum' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Minimum"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Minimum")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Minimum' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Moments' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Moments"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Moments")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Moments' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Otsu' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Otsu"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Otsu")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Otsu' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Percentile' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Percentile"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Percentile")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Percentile' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'RenyiEntropy' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "RenyiEntropy"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "RenyiEntropy")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'RenyiEntropy' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Shanbhag' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Shanbhag"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Shanbhag")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Shanbhag' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Triangle' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Triangle"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Triangle")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Triangle' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Yen' binarization method if the correct CVR data frame is provided", {

  function_var <- try(threshold_df(df = merged_df_CVR,
                                   method = "Yen"),
                      silent = TRUE)

  test_var <- try(autothresholdr::auto_thresh(
    int_arr = as.integer(
      as.matrix(merged_df_CVR) * 100), method = "Yen")/100,
    silent = TRUE)

  testing_function <- function(x, y){

    all(is.numeric(x[[1]]) &
          x[[1]] == y[[1]]) |
      all(class(x) == "try-error" &
            x[[1]] == "Error : 'Yen' method failed to find threshold.\n")

  }

  testthat::expect_true(testing_function(function_var, test_var))

})

testthat::test_that("the threshold_df function works for the 'Mode' binarization method if the correct CVR data frame is provided", {

  function_var <- threshold_df(df = merged_df_CVR,
                               method = "Mode")

  testing_function_mode <- function(x){

    uniqv <- unique(unlist(x))
    uniqv[which.max(tabulate(match(unlist(x), uniqv)))]

  }

  test_var <- testing_function_mode(merged_df_CVR)

  testthat::expect_equal(function_var, test_var)


})

  # 2.2. If wrong df argument

    # 2.2.1. Argument is not a data frame

testthat::test_that("the threshold_df function provides the correct error message when the supplied df argument is not a data frame", {

  testthat::expect_error(
    object = threshold_df(df = merged_df_CVR_matrix,
                 method = "IJDefault"),
    regexp = "df is not a data frame. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.", fixed=TRUE)

})

    # 2.2.2. Argument is an empty data frame

testthat::test_that("the threshold_df function provides the correct error message when the supplied df argument is an empty data frame", {

  testthat::expect_error(
    object = threshold_df(df = merged_df_CVR_empty,
                          method = "IJDefault"),
    regexp = "df is an empty dataframe. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.", fixed=TRUE)

})

    # 2.2.3. Argument is a data frame containing NAs

testthat::test_that("the threshold_df function provides the correct error message when the supplied df argument is a data frame containing NA values", {

  testthat::expect_error(
    object = threshold_df(df = merged_df_CVR_NAs,
                          method = "IJDefault"),
    regexp = "df contains NA values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.", fixed=TRUE)

})

    # 2.2.4. Argument is a data frame containing non numeric

testthat::test_that("the threshold_df function provides the correct error message when the supplied df argument is a data frame containing non-numeric values", {

  testthat::expect_error(
    object = threshold_df(df = merged_df_CVR_nonnum,
                          method = "IJDefault"),
    regexp = "df contains non-numeric values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.", fixed=TRUE)

})

  # 2.3. If correct df argument but wrong method argument

testthat::test_that("the threshold_df function provides the correct error message when the supplied method argument is not one of the available options", {

  testthat::expect_error(
    object = threshold_df(df = merged_df_CVR,
                          method = "I'm not an option"),
    regexp = "method is not one of the available binarization methods. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.", fixed=TRUE)

})



# 3. Start testing the get_mode function

  # 3.0. If required argument is missing

testthat::test_that("the get_mode function provides the correct error when the df argument is missing", {

  testthat::expect_error(
    object = get_mode(),
    regexp = "df argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

  # 3.1. Correct numeric data frame supplied

testthat::test_that("the get_mode function works when a numeric data frame is supplied", {

  testing_function_mode <- function(x){

    uniqv <- unique(unlist(x))
    uniqv[which.max(tabulate(match(unlist(x), uniqv)))]

  }

  function_var <- get_mode(df = merged_df_CVR)
  test_var <- testing_function_mode(merged_df_CVR)

  testthat::expect_equal(function_var, test_var)

})

  # 3.2. df argument is not a data frame

testthat::test_that("the get_mode function works when a numeric data frame is supplied", {

  testthat::expect_error(
    object = get_mode(df = merged_df_CVR_matrix),
    regexp = "df is not a numeric dataframe. Please supply a valid argument to the function", fixed=TRUE
  )

})

  # 3.3. df argument is a non-numeric data frame

testthat::test_that("the get_mode function works when a numeric data frame is supplied", {

  testthat::expect_error(
    object = get_mode(df = merged_df_CVR_nonnum),
    regexp = "df is not a numeric dataframe. Please supply a valid argument to the function", fixed=TRUE
  )

})

# 4. Start testing the binarize_df function

  # 4.0. If required argument is missing

testthat::test_that("the binarize_df function provides the correct error when the df argument is missing", {

  testthat::expect_error(
    object = binarize_df(method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "df argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the binarize_df function provides the correct error when the method argument is missing", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         value = NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "method argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the binarize_df function provides the correct error when the method argument is set to 'custom', but the value argument is missing", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "custom",
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "value argument is missing. If you set method to 'custom', please supply a value argument. Consult package documentation for options.",
    fixed = TRUE)


})

testthat::test_that("the binarize_df function provides the correct error when the date argument is missing", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "date argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the binarize_df function provides the correct error when the lat argument is missing", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lon = -59.48937990402315),
    regexp = "lat argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the binarize_df function provides the correct error when the lon argument is missing", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629),
    regexp = "lon argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

  # 4.1. When the correct data frame, method
  # (and potentially value) arguments are supplied

testthat::test_that("the binarize_df function works as expected when the correct arguments are supplied", {

  function_var <- binarize_df(df = merged_df_CVR,
                              method = "IJDefault",
                              value = NULL,
                              date = "2015-09-05",
                              lat = -1.915867928971629,
                              lon = -59.48937990402315)

  testing_function_binarize <- function(x, y){

    threshold <- threshold_df(df = x, method = y)
    thresh_df <- as.data.frame(
      ifelse(x > (threshold), 1, 0))
    colnames(thresh_df) <- colnames(x)
    rownames(thresh_df) <- rownames(x)

    return(thresh_df)

  }

  test_var <- testing_function_binarize(x = merged_df_CVR,
                                        y = "IJDefault")

  testthat::expect_equal(function_var, test_var)
  testthat::expect_equal(rownames(function_var),
                         rownames(merged_df_CVR))
  testthat::expect_equal(colnames(function_var),
                         colnames(merged_df_CVR))
  testthat::expect_equal(dim(function_var),
                         dim(merged_df_CVR))
  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(limma::isNumeric(function_var))
  testthat::expect_true(!any(
    is.na(unlist(function_var))))
  testthat::expect_true(
    !(is.null(ncol(function_var)) |
        is.null(nrow(function_var))))
  testthat::expect_true(max(function_var)==1 |
                          max(function_var)==0 )
  testthat::expect_true(min(function_var)==0 |
                          min(function_var)==1)

})

testthat::test_that("the binarize_df function works as expected when the correct arguments are supplied", {

  function_var <- binarize_df(df = merged_df_CVR,
                              method = "Mode",
                              value = NULL,
                              date = "2015-09-05",
                              lat = -1.915867928971629,
                              lon = -59.48937990402315)

  testing_function_binarize <- function(x, y){

    threshold <- threshold_df(df = x, method = y)
    thresh_df <- as.data.frame(
      ifelse(x > (threshold), 1, 0))
    colnames(thresh_df) <- colnames(x)
    rownames(thresh_df) <- rownames(x)

    return(thresh_df)

  }

  test_var <- testing_function_binarize(x = merged_df_CVR,
                                        y = "Mode")

  testthat::expect_equal(function_var, test_var)
  testthat::expect_equal(rownames(function_var),
                         rownames(merged_df_CVR))
  testthat::expect_equal(colnames(function_var),
                         colnames(merged_df_CVR))
  testthat::expect_equal(dim(function_var),
                         dim(merged_df_CVR))
  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(limma::isNumeric(function_var))
  testthat::expect_true(max(function_var)==1 |
                          max(function_var)==0 )
  testthat::expect_true(min(function_var)==0 |
                          min(function_var)==1)

})

testthat::test_that("the binarize_df function works as expected when the correct arguments are supplied", {

  function_var <- binarize_df(df = merged_df_CVR,
                              method = "custom",
                              value = 1,
                              date = "2015-09-05",
                              lat = -1.915867928971629,
                              lon = -59.48937990402315)

  testing_function_binarize <- function(x, y, z){

    threshold <- as.numeric(z)
    thresh_df <- as.data.frame(
      ifelse(x > (threshold), 1, 0))
    colnames(thresh_df) <- colnames(x)
    rownames(thresh_df) <- rownames(x)

    return(thresh_df)

  }

  test_var <- testing_function_binarize(x = merged_df_CVR,
                                        y = "custom",
                                        z = 1)

  testthat::expect_equal(function_var, test_var)
  testthat::expect_equal(rownames(function_var),
                         rownames(merged_df_CVR))
  testthat::expect_equal(colnames(function_var),
                         colnames(merged_df_CVR))
  testthat::expect_equal(dim(function_var),
                         dim(merged_df_CVR))
  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(limma::isNumeric(function_var))
  testthat::expect_true(max(function_var)==1 |
                          max(function_var)==0 )
  testthat::expect_true(min(function_var)==0 |
                          min(function_var)==1)

})

  # 4.2. When the supplied df argument is wrong

    # 4.2.1. The df argument is not a data frame

testthat::test_that("the binarize_df function produces the correct error message when the df argument is not a data frame ", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR_matrix,
                         method = "IJDefault",
                         value=NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "df is not a data frame. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.", fixed=TRUE
  )

})

    # 4.2.2. The df argument is an empty data frame

testthat::test_that("the binarize_df function produces the correct error message when the df argument is an empty data frame", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR_empty,
                         method = "IJDefault",
                         value=NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "df is an empty dataframe. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.", fixed=TRUE
  )

})

    # 4.2.3. The df argument is a data frame containing NAs

testthat::test_that("the binarize_df function produces the correct error message when the df argument is a data frame containing NA values", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR_NAs,
                         method = "IJDefault",
                         value=NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "df contains NA values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.", fixed=TRUE
  )

})

    # 4.2.4. The df argument is a data frame containing
    # non-numeric values

testthat::test_that("the binarize_df function produces the correct error message when the df argument is a data frame containing non-numeric values", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR_nonnum,
                         method = "IJDefault",
                         value=NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "df contains non-numeric values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.", fixed=TRUE
  )

})

  # 4.3. When the supplied method argument is wrong

    # 4.3.1. The method argument is not a character string

testthat::test_that("the binarize_df function produces the correct error message when the method argument is not supplied as a character string", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = as.factor("IJDefault"),
                         value=NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "method is not a character string. Please supply the binarization methods as a character string. Consult package documentation for available method options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.", fixed=TRUE
  )

})

    # 4.3.2. The method argument is not one of the available
    # options

testthat::test_that("the binarize_df function produces the correct error message when the method argument is not one of the available options", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "Not an option!",
                         value=NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "method is not one of the available binarization method options. Please consult package documentation for available  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.", fixed=TRUE
  )

})

  # 4.4. When the supplied value argument is wrong

    # 4.4.1. The value argument is of the wrong type

testthat::test_that("the binarize_df function produces the correct error message when the format of the value argument is not correct", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "custom",
                         value = "Not the right format!",
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "value input is not in the right format. To choose a custom binarization threshold, supply a single numeric value.", fixed=TRUE
  )

})

    # 4.4.2. The value argument is of the wrong length

testthat::test_that("the binarize_df function produces the correct error message when the length of the value argument is not correct", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "custom",
                         value = c(1, 2),
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "value input is not in the right format. To choose a custom binarization threshold, supply a single numeric value.", fixed=TRUE
  )

})

  # 4.5. The wrong date argument is supplied

    # 4.5.1. The date argument is not a character string

testthat::test_that("the check_thresh function produces the correct error message when the date argument is not a character string", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = as.factor("2015-09-05"),
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "date is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information",
    fixed=TRUE
  )

})

    # 4.5.2. The date argument does not have the
    # correct format

testthat::test_that("the check_thresh function produces the correct error message when the date argument has the wrong format", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "05-09-2015",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the date argument has the wrong format", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "05/09/2015",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information",
    fixed=TRUE
  )

})

  # 4.6. The wrong lat and/or lon coordinates are supplied

    # 4.6.1. The lat and/or lon are not supplied in
    # decimal degrees

testthat::test_that("the check_thresh function produces the correct error message when the latitude argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = "1°55'00.1",
                         lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the longitude argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = "59°28'25.7"),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the latitude and longitude argument are supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = "1°54'57.2",
                         lon = "59°28'25.7"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

    # 4.6.2. Latitude and longitude are numerical, but
    # don't fall within the expected values existing on
    # Earth

testthat::test_that("the check_thresh function produces the correct error message when the latitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = 91,
                         lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the latitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = -91,
                         lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the longitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = 181),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the longitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -181),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the latitude and longitude arguments are numeric, but don't fit the existing values on Earth", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = 91,
                         lon = 181),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the latitude and longitude arguments are numeric, but don't fit the existing values on Earth", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = -91,
                         lon = -181),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

# 4.8. Rownames and/or column names of df argument
# not correct

testthat::test_that("the check_thresh function provides the correct error message when the row names of the supplied df argument are not frequency values", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR_falserows,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.",
    fixed = TRUE)

})

testthat::test_that("the check_thresh function provides the correct error message when the column names of the supplied df argument are not character strings indicating times in the following format: HH:MM:SS", {

  testthat::expect_error(
    object = binarize_df(df = merged_df_CVR_falsecols,
                         method = "IJDefault",
                         value = NULL,
                         date = "2015-09-05",
                         lat = -1.915867928971629,
                         lon = -59.48937990402315),
    regexp = "df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.",
    fixed = TRUE)

})

# 5. Testing the check_thresh function

  # 5.0. If required argument is missing

testthat::test_that("the check_thresh function provides the correct error when the df argument is missing", {

  testthat::expect_error(
    object = check_thresh(method = "Otsu",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "df argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the check_thresh function provides the correct error when the method argument is missing", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "method argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the check_thresh function provides the correct error when the method argument is set to 'custom', but the value argument is missing", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "custom",
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "value argument is missing. If you set method to 'custom', please supply a value argument. Consult package documentation for options.",
    fixed = TRUE)


})

testthat::test_that("the check_thresh function provides the correct error when the index argument is missing", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "Otsu",
                          value = NULL,
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "index argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the check_thresh function provides the correct error when the date argument is missing", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "Otsu",
                          value = NULL,
                          index = "CVR",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "date argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the check_thresh function provides the correct error when the lat argument is missing", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "Otsu",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lon = -59.48937990402315),
    regexp = "lat argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the check_thresh function provides the correct error when the lon argument is missing", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "Otsu",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629),
    regexp = "lon argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

  # 5.1. When the correct data frame, method, value, index,
  # date, lat and lon are supplied

testthat::test_that("the check_thresh function works as expected when the correct arguments are supplied", {

  plot <- check_thresh(df = merged_df_CVR,
                       method = "Otsu",
                       value = NULL,
                       index = "CVR",
                       date = "2015-09-05",
                       lat = -1.915867928971629,
                       lon = -59.48937990402315)

  vdiffr::expect_doppelganger("check thresh plot", plot)

})

  # 5.2. When the wrong df argument format is supplied

    # 5.2.1. The df argument is not a data frame

testthat::test_that("the check_thresh function produces the correct error message when the df argument is not a data frame.", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR_matrix,
                          method = "Otsu",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "df is not a data frame. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function",
    fixed = TRUE )

})

    # 5.2.2. The df argument is an empty data frame

testthat::test_that("the check_thresh function produces the correct error message when the df argument is an empty data frame.", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR_empty,
                          method = "Otsu",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "df is an empty dataframe. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function",
    fixed = TRUE )

})

    # 5.2.3. The df argument is a data frame containing NAs

testthat::test_that("the check_thresh function produces the correct error message when the df argument is a data frame containing NA values.", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR_NAs,
                          method = "Otsu",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "df contains NA values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function",
    fixed = TRUE )

})

    # 5.2.4. The df argument is a data frame containing
    # non-numeric values

testthat::test_that("the check_thresh function produces the correct error message when the df argument is a data frame containing non-numeric values.", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR_nonnum,
                          method = "Otsu",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "df contains non-numeric values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function",
    fixed = TRUE )

})

  # 5.3. When the wrong method argument is supplied

    # 5.3.1. The method argument is not a character string

testthat::test_that("the check_thresh function produces the correct error message when the method argument is not supplied as a character string", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = as.factor("Otsu"),
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "method is not a character string. Please supply the binarization methods as a character string. Consult package documentation for available method options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

    # 5.3.2. The method argument is not one of the
    # available options

testthat::test_that("the check_thresh function produces the correct error message when the method argument is not one of the available options", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "Not an option!",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "method is not one of the available binarization method options. Please consult package documentation for available  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

  # 5.4. When the wrong value argument is supplied

    # 5.4.1. The value argument is of the wrong type

testthat::test_that("the check_thresh function produces the correct error message when the format of the value argument is not correct", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "custom",
                          value = "Not the right format!",
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "value input is not in the right format. To choose a custom binarization threshold, supply a single numeric value.",
    fixed=TRUE
  )

})

    # 5.4.2. The value argument is of the wrong length

testthat::test_that("the check_thresh function produces the correct error message when the length of the value argument is not correct", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "custom",
                          value = c(1, 2),
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "value input is not in the right format. To choose a custom binarization threshold, supply a single numeric value.",
    fixed=TRUE
  )

})

  # 5.5. The wrong index argument is supplied

    # 5.5.1. The index argument is not a character string

testthat::test_that("the check_thresh function produces the correct error message when the index argument is not supplied as a character string", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "Otsu",
                          value = NULL,
                          index = as.factor("CVR"),
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "index is not a character string. Please supply the index as a character string. Consult package documentation for available index options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

    # 5.5.2. The index argument is not one of the
    # available options

testthat::test_that("the check_thresh function produces the correct error message when the index argument is not one of the available options", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "Not an option!",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "index is not one of the available acoustic indices. Please consult package documentation for available  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

  # 5.6. The wrong date argument is supplied

    # 5.6.1. The date argument is not a character string

testthat::test_that("the check_thresh function produces the correct error message when the date argument is not a character string", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = as.factor("2015-09-05"),
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "date is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information",
    fixed=TRUE
  )

})

    # 5.6.2. The date argument does not have the
    # correct format

testthat::test_that("the check_thresh function produces the correct error message when the date argument has the wrong format", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "05-09-2015",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the date argument has the wrong format", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "05/09/2015",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information",
    fixed=TRUE
  )

})

  # 5.7. The wrong lat and/or lon coordinates are supplied

    # 5.7.1. The lat and/or lon are not supplied in
    # decimal degrees

testthat::test_that("the check_thresh function produces the correct error message when the latitude argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = "1°55'00.1",
                          lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the longitude argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = "59°28'25.7"),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the latitude and longitude argument are supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = "1°54'57.2",
                          lon = "59°28'25.7"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

    # 5.7.2. Latitude and longitude are numerical, but
    # don't fall within the expected values existing on
    # Earth

testthat::test_that("the check_thresh function produces the correct error message when the latitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = 91,
                          lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the latitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -91,
                          lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the longitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = 181),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the longitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -181),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the latitude and longitude arguments are numeric, but don't fit the existing values on Earth", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = 91,
                          lon = 181),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

testthat::test_that("the check_thresh function produces the correct error message when the latitude and longitude arguments are numeric, but don't fit the existing values on Earth", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR,
                          method = "IJDefault",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -91,
                          lon = -181),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180",
    fixed=TRUE
  )

})

  # 5.8. Rownames and/or column names of df argument
  # not correct

testthat::test_that("the check_thresh function provides the correct error message when the row names of the supplied df argument are not frequency values", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR_falserows,
                          method = "Otsu",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315)
    ,
    regexp = "df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.",
    fixed = TRUE)

})

testthat::test_that("the check_thresh function provides the correct error message when the column names of the supplied df argument are not character strings indicating times in the following format: HH:MM:SS", {

  testthat::expect_error(
    object = check_thresh(df = merged_df_CVR_falsecols,
                          method = "Otsu",
                          value = NULL,
                          index = "CVR",
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315),
    regexp = "df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.",
    fixed = TRUE)

})



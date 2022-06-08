# Functions for applying a binarization algorithm to the
# merged dataframe

#' Calculate the mode
#'
#' @param df The merged_soundscape data frame produced by
#' \code{\link{merge_csv}}.
#' @return Returns the acoustic index value which appears most often
#' @details Function for internal use by \code{\link{threshold_df}}
#'

get_mode <- function(df) {

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(df))



  # 1. Check if input variable in the right format

  test_1 <- function(x){

    (is.data.frame(x) & limma::isNumeric(x))
  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not a numeric dataframe. Please supply a valid argument to the function")

  }

  assertthat::assert_that(test_1(df))

  # Get the mode of the dataframe or vector

  uniqv <- unique(unlist(df))
  uniqv[which.max(tabulate(match(unlist(df), uniqv)))]
}

#' Determine Binarization Threshold
#'
#' @description Determines the threshold for binarization of the
#' time-frequency dataframe of index values. Several binarization
#' algorithms are available, either based on image thresholding
#' algorithms from \code{\link[autothresholdr]{auto_thresh}}, or
#'  calculation of the mode.
#'
#' @param df The merged_soundscape data frame produced by
#' \code{\link{merge_csv}}.
#'
#' @param method The algorithm used to determine the threshold.
#'  Options are "IJDefault", "Huang", "Huang2", "Intermodes",
#'  "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI", "Minimum",
#'  "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag",
#'  "Triangle", "Yen", and "Mode".
#'  Consult \url{http://imagej.net/Auto_Threshold} for more
#'  information on algorithm methodologies.
#'
#' @details Function for internal use by \code{\link{binarize_df}},
#' however can also be called on by the user manually.
#'
#' @return Returns a numeric threshold value for subsequent
#' binarization.
#'
#' @export
#'
threshold_df <- function(df, method) {

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(df))
  assertthat::assert_that(test_0(method))


  # 1. Check if function input meets expectations

    #1.1. The supplied dataframe is a dataframe, is not empty, and does not contain NAs

  test_1 <- function(x){

    is.data.frame(x)

  }

  test_2 <- function(x){

    assertthat::not_empty(x)

  }

  test_3 <- function(x){

    assertthat::noNA(x)

  }

  test_4 <- function(x){

    limma::isNumeric(x)

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not a data frame. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is an empty dataframe. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " contains NA values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " contains non-numeric values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::assert_that(test_1(df))
  assertthat::assert_that(test_2(df))
  assertthat::assert_that(test_3(df))
  assertthat::assert_that(test_4(df))

    # 1.2. Check if the specified method is one of the available options

  test_2 <- function(x){
    assertthat::is.string(x) &
      x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li",
               "MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu",
               "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode")
  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available binarization methods. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_2(method))

  # 2. Calculate the threshold value

  if (method == "IJDefault" |
      method == "Huang" |
      method == "Huang2" |
      method == "Intermodes" |
      method == "IsoData" |
      method == "Li" |
      method == "MaxEntropy" |
      method == "Mean" |
      method == "MinErrorI" |
      method == "Minimum" |
      method == "Moments" |
      method == "Otsu" |
      method == "Percentile" |
      method == "RenyiEntropy" |
      method == "Shanbhag" |
      method == "Triangle" |
      method == "Yen") {

    df2 <- (as.integer(as.matrix(df) * 100))
    threshold <- autothresholdr::auto_thresh(int_arr = df2, method = method)
    threshold <- threshold / 100

    return(threshold)

  }

  else {

    if (method == "Mode" | method == "mode") {

      mode <- get_mode(df)

      return(mode)

    }
  }
}

#' Binarize Dataframe
#'
#' @description Separates acoustically active time-frequency bins
#'  (active=1) from background values (inactive=0) through the
#'  application of a binarization algorithm. Several binarization
#'  algorithms are available, either based on image thresholding
#'  using \code{\link[autothresholdr]{auto_thresh}}, or subtraction
#'  of the mode.
#'
#' @param merged_soundscape The merged soundscape object produced by
#' \code{\link{merge_csv}}.
#'
#' @param method The algorithm used to determine the threshold.
#'  Options are "IJDefault","Huang", "Huang2", "Intermodes",
#'  "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI", "Minimum",
#'  "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag",
#'  "Triangle", "Yen", and "Mode". To specify a custom threshold,
#'  use method="Custom" in combination with the value argument.
#'  Consult \url{http://imagej.net/Auto_Threshold} for more
#'  information on algorithm methodologies.
#'
#' @param value Optional argument used to set a custom threshold
#' value for binarization - used in combination with method="Custom".
#'
#' @return Returns a binary time-frequency dataframe of acoustic
#'  activity (active=1, inactive=0) for a set acoustic index.
#' @export
#'
binarize_df <- function(merged_soundscape,
                        method,
                        value = NULL){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  test_1 <- function(x, y){

    !(method == "custom" & (missing(x) | is.null(x)))

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. If you set method to 'custom', please supply a value argument. Consult package documentation for options.")

  }

  assertthat::assert_that(test_0(merged_soundscape))
  assertthat::assert_that(test_0(method))
  assertthat::assert_that(test_1(value))

  # 1. Check if function input meets expectations

    # 1.1. The supplied merged_soundscape argument is an S4-object of the type
    # 'soundscape', and is not empty.

  test_2 <- function(x){

   isS4(x) &
    assertthat::are_equal(class(x)[1], "soundscape")

  }

  test_3 <- function(x){

    assertthat::not_empty(x)

  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape'. Please supply the merged_soundscape object produced by the merge_csv() function. Consult the package documentation for further information.")

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is an empty S4-object of the type 'soundscape'. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function.")

  }

  assertthat::assert_that(test_2(merged_soundscape))
  assertthat::assert_that(test_3(merged_soundscape))

    # 1.2. The merged_soundscape elements are in the expected format

      # 1.2.1. The first_day argument cannot be wrong (S4 property)

      # 1.2.2. The lat and lon argument

  test_5 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_6 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_5(merged_soundscape@lat))
  assertthat::assert_that(test_6(merged_soundscape@lon))

    # 1.2.3. The time zone argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_7(merged_soundscape@tz))

    # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

    # 1.2.5. The fileloc argument
#
#   test_8 <- function(x){
#
#     assertthat::is.dir(x) & assertthat::is.readable(x)
#
#   }

  # assertthat::assert_that(test_8(merged_soundscape@fileloc))

    # 1.2.6. The index argument

  test_9 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_9(merged_soundscape@index))

    # 1.2.7. The samplerate and window arguments

  test_10 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_10(merged_soundscape@samplerate))
  assertthat::assert_that(test_10(merged_soundscape@window))

    # 1.2.8. The post-binarization arguments are NA

  test_11 <- function(x){

    is.na(x)

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not NA. Did you supply a post-binarization or post-aggregation soundscape to the binarize_df() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_11(merged_soundscape@binarization_method))
  assertthat::assert_that(test_11(merged_soundscape@threshold))
  assertthat::assert_that(test_11(merged_soundscape@output))

    # 1.2.9. The merged_df argument

  test_12 <- function(x){

    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      limma::isNumeric(x)

  }

  test_13 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x)))<= merged_soundscape@samplerate/2

  }

  test_14 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(merged_soundscape@first_day, 1, 12)," ", colnames(x)),
        tz = merged_soundscape@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function.")

  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::assert_that(test_12(merged_soundscape@merged_df))
  assertthat::assert_that(test_13(merged_soundscape@merged_df))
  assertthat::assert_that(test_14(merged_soundscape@merged_df))

    # 1.2.10. The binarized_df and aggregate_df arguments are missing

  test_15 <- function(x){

    assertthat::are_equal(x[1, 1], "missing")

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not a missing data frame. Did you supply a post-binarization or post-aggregation merged_soundscape to the binarize_df() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_15(merged_soundscape@binarized_df))
  assertthat::assert_that(test_15(merged_soundscape@aggregated_df))

  test_16 <- function(x){

    all(sapply(x, function(x) is.na(x)))

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " is not a list of NAs. Did you supply a post-binarization or post-aggregation merged_soundscape to the binarize_df() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_16(merged_soundscape@aggregated_df_per_time))
  assertthat::assert_that(test_16(merged_soundscape@effort_per_time))


    # 1.3. Check if the specified method is one of the available options

  test_17 <- function(x){
    assertthat::is.string(x)
  }

  test_18 <- function(x){
  x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom")
  }

  assertthat::on_failure(test_17) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the binarization methods as a character string. Consult package documentation for available method options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available binarization method options. Please consult package documentation for available  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_17(method))
  assertthat::assert_that(test_18(method))

    # 1.3. Check that, if value is supplied, it is a single number

  test_19 <- function(x){

    is.null(x) |
      is.numeric(x) & length(x) == 1

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " input is not in the right format. To choose a custom binarization threshold, supply a single numeric value.")

  }

  assertthat::assert_that(test_19(value))

  # 2. Calculate the binarization threshold

  if (method == "IJDefault" |
      method == "Huang" |
      method == "Huang2" |
      method == "Intermodes" |
      method == "IsoData" |
      method == "Li" |
      method == "MaxEntropy" |
      method == "Mean" |
      method == "MinErrorI" |
      method == "Minimum" |
      method == "Moments" |
      method == "Otsu" |
      method == "Percentile" |
      method == "RenyiEntropy" |
      method == "Shanbhag" |
      method == "Triangle" |
      method == "Yen" |
      method == "Mode" |
      method == "mode") {
    threshold <- threshold_df(df = merged_soundscape@merged_df, method = method)
  }

  else {
    if (method == "custom") {
      threshold <- value
    }
  }

  # 3. Perform dataframe binarization

  thresh_df <- as.data.frame(ifelse(merged_soundscape@merged_df > (threshold), 1, 0))
  colnames(thresh_df) <- colnames(merged_soundscape@merged_df)
  rownames(thresh_df) <- rownames(merged_soundscape@merged_df)

  binarized_soundscape <- methods::new("soundscape",
                                       first_day = merged_soundscape@first_day,
                                       lat = merged_soundscape@lat,
                                       lon = merged_soundscape@lon,
                                       tz = merged_soundscape@tz,
                                       sunrise = merged_soundscape@sunrise,
                                       sunset = merged_soundscape@sunset,
                                       fileloc = merged_soundscape@fileloc,
                                       index = merged_soundscape@index,
                                       samplerate = merged_soundscape@samplerate,
                                       window = merged_soundscape@window,
                                       binarization_method = method,
                                       threshold = as.numeric(threshold),
                                       merged_df = merged_soundscape@merged_df,
                                       binarized_df = thresh_df)


  binarized_soundscape

}


#' Check which binarization algorithm works for a set index
#'
#' @description Plots the merged acoustic index dataframe
#'  before and after the binarization step using a
#'  user-specified binarization algorithm. Facilitates the
#'  rapid visual comparison of binarization performance using
#'  different algorithms.
#'
#' @param merged_soundscape The merged_soundscape object produced by
#' \code{\link{merge_csv}}.
#'
#' @param method The algorithm used to determine the
#' threshold. Options are "IJDefault","Huang", "Huang2",
#'  "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean",
#'  "MinErrorI", "Minimum", "Moments", "Otsu", "Percentile",
#'  "RenyiEntropy", "Shanbhag", "Triangle", "Yen", and
#'  "Mode". To specify a custom threshold, use
#'  method="Custom" in combination with the value argument.
#'  Consult \url{http://imagej.net/Auto_Threshold} for more
#'  information on algorithm methodologies.
#'
#' @param value Optional argument used to set a custom
#'  threshold value for binarization - used in combination
#'   with method="Custom".
#'
#' @return A plot with before and after binarization to
#'  facilitate visual comparison of binarization algorithm
#'   performance.
#'
#' @export
#'
check_thresh <- function(merged_soundscape,
                         method,
                         value = NULL){

  # 0.0. Create binding for global variables

  time <- NULL


  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  test_1 <- function(x, y){

    !(method == "custom" & (missing(x) | is.null(x)))

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. If you set method to 'custom', please supply a value argument. Consult package documentation for options.")

  }

  assertthat::assert_that(test_0(merged_soundscape))
  assertthat::assert_that(test_0(method))
  assertthat::assert_that(test_1(value))

  # 1. Check if function input meets expectations

  # 1.1. The supplied merged_soundscape argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_2 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape")

  }

  test_3 <- function(x){

    assertthat::not_empty(x)

  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape'. Please supply the merged_soundscape object produced by the merge_csv() function. Consult the package documentation for further information.")

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is an empty S4-object of the type 'soundscape'. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function.")

  }

  assertthat::assert_that(test_2(merged_soundscape))
  assertthat::assert_that(test_3(merged_soundscape))

  # 1.2. The merged_soundscape elements are in the expected format

  # 1.2.1. The first_day argument cannot be wrong (S4 property)

  # 1.2.2. The lat and lon argument

  test_5 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_6 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_5(merged_soundscape@lat))
  assertthat::assert_that(test_6(merged_soundscape@lon))

  # 1.2.3. The time zone argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_7(merged_soundscape@tz))

  # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

  # 1.2.5. The fileloc argument

  # test_8 <- function(x){
  #
  #   assertthat::is.dir(x) & assertthat::is.readable(x)
  #
  # }
  #
  # assertthat::assert_that(test_8(merged_soundscape@fileloc))

  # 1.2.6. The index argument

  test_9 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_9(merged_soundscape@index))

  # 1.2.7. The samplerate and window arguments

  test_10 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_10(merged_soundscape@samplerate))
  assertthat::assert_that(test_10(merged_soundscape@window))

  # 1.2.8. The post-binarization arguments are NA

  test_11 <- function(x){

    is.na(x)

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not NA. Did you supply a post-binarization or post-aggregation soundscape to the check_thresh() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_11(merged_soundscape@binarization_method))
  assertthat::assert_that(test_11(merged_soundscape@threshold))
  assertthat::assert_that(test_11(merged_soundscape@output))

  # 1.2.9. The merged_df argument

  test_12 <- function(x){

    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      limma::isNumeric(x)

  }

  test_13 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x)))<= merged_soundscape@samplerate/2

  }

  test_14 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(merged_soundscape@first_day, 1, 12)," ", colnames(x)),
        tz = merged_soundscape@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the merged_soundscape argument produced using the merge_csv() function? If so, something has gone wrong, please re-run the merge_csv() function.")

  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function.")

  }

  assertthat::assert_that(test_12(merged_soundscape@merged_df))
  assertthat::assert_that(test_13(merged_soundscape@merged_df))
  assertthat::assert_that(test_14(merged_soundscape@merged_df))

  # 1.2.10. The binarized_df and aggregate_df arguments are missing

  test_15 <- function(x){

    assertthat::are_equal(x[1, 1], "missing")

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not a missing data frame. Did you supply a post-binarization or post-aggregation merged_soundscape to the check_thresh() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_15(merged_soundscape@binarized_df))
  assertthat::assert_that(test_15(merged_soundscape@aggregated_df))

  test_16 <- function(x){

    all(sapply(x, function(x) is.na(x)))

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " is not a list of NAs. Did you supply a post-binarization or post-aggregation merged_soundscape to the check_thresh() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_16(merged_soundscape@aggregated_df_per_time))
  assertthat::assert_that(test_16(merged_soundscape@effort_per_time))


  # 1.3. Check if the specified method is one of the available options

  test_17 <- function(x){
    assertthat::is.string(x)
  }

  test_18 <- function(x){
    x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom")
  }

  assertthat::on_failure(test_17) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the binarization methods as a character string. Consult package documentation for available method options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available binarization method options. Please consult package documentation for available  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_17(method))
  assertthat::assert_that(test_18(method))

  # 1.4. Check that, if value is supplied, it is a single number

  test_19 <- function(x){

    is.null(x) |
      is.numeric(x) & length(x) == 1

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " input is not in the right format. To choose a custom binarization threshold, supply a single numeric value.")

  }

  assertthat::assert_that(test_19(value))

  # 2. Get the before and after dataframe in the right format

    # 2.1. Make function to lengthen data frames for ggplot

  lengthen_2 <- function(df){
    tz <- df@tz
    df_2 <- df@merged_df
    df_2$frequency=as.integer(rownames(df_2))
    melt_df=reshape2::melt(df_2, id.vars="frequency")
    colnames(melt_df)=c("frequency", "time", "value")
    melt_df$frequency=as.numeric(as.character(melt_df$frequency))
    melt_df$time=as.POSIXct(strptime(paste(melt_df$time),
                                     format= "%Y-%m-%d %H:%M",
                                     tz=tz))
    melt_df
  }

  if (method == "IJDefault" |
      method == "Huang" |
      method == "Huang2" |
      method == "Intermodes" |
      method == "IsoData" |
      method == "Li" |
      method == "MaxEntropy" |
      method == "Mean" |
      method == "MinErrorI" |
      method == "Minimum" |
      method == "Moments" |
      method == "Otsu" |
      method == "Percentile" |
      method == "RenyiEntropy" |
      method == "Shanbhag" |
      method == "Triangle" |
      method == "Yen" |
      method == "Mode" |
      method == "mode") {
    threshold <- threshold_df(df = merged_soundscape@merged_df, method = method)
  }

  else {
    if (method == "custom") {
      threshold <- value
    }
  }

  df_after <- as.data.frame(
    ifelse(merged_soundscape@merged_df > (threshold), 1, 0))

  colnames(df_after) <- colnames(merged_soundscape@merged_df)

  rownames(df_after) <- rownames(merged_soundscape@merged_df)

  # 3. Figure out the time difference between two
  #consecutive recordings in the df_after df

  time_1 <- as.POSIXct(
    strptime(
      paste(substr(merged_soundscape@first_day, 1, 12),
            colnames(df_after)[1],
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz = merged_soundscape@tz))

  time_2 <- as.POSIXct(
    strptime(
      paste(substr(merged_soundscape@first_day, 1, 12),
            colnames(df_after)[2],
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz = merged_soundscape@tz))

  new_colnames <- seq.POSIXt(
    from = as.POSIXct(
      strptime(
        paste(
          substr(merged_soundscape@first_day, 1, 12),
              colnames(df_after)[1],
              sep=" "),
        format= "%Y-%m-%d %H:%M:%S",
        tz = merged_soundscape@tz)),
    to = as.POSIXct(
      strptime(
        paste(substr(merged_soundscape@first_day, 1, 12),
              colnames(merged_soundscape@merged_df)[1],
              sep=" "),
        format= "%Y-%m-%d %H:%M:%S",
        tz = merged_soundscape@tz)) + (ncol(merged_soundscape@merged_df)*(time_2-time_1)),
    by = (time_2 - time_1)
  )[1:ncol(merged_soundscape@merged_df)]

  new_colnames

  colnames(df_after) <- new_colnames

  colnames(merged_soundscape@merged_df) <- new_colnames

  df_before <- lengthen_2(df = merged_soundscape)

  df_before

  # 3.1. Test if all the objects are in the right format

  # time_1 and time_2

  test_13_1 <- function(x, y){

    lubridate::is.POSIXct(x) &
      lubridate::is.POSIXct(y)

  }

  assertthat::assert_that(test_13(x=time_1, y=time_2))

  # new_colnames

  test_14_1 <- function(x){

    !any(!sapply(x, FUN = lubridate::is.POSIXct))

  }

  assertthat::assert_that(test_14_1(new_colnames))

  # 4. Lengthen the df_after data frame

  lengthen_3 <- function(df, date, lat, lon){
    tz <- lutz::tz_lookup_coords(lat = lat, lon = lon, method = "accurate")
    df_2 <- df
    df_2$frequency=as.integer(rownames(df_2))
    melt_df=reshape2::melt(df_2, id.vars="frequency")
    colnames(melt_df)=c("frequency", "time", "value")
    melt_df$frequency=as.numeric(as.character(melt_df$frequency))
    melt_df$time=as.POSIXct(strptime(paste(melt_df$time),
                                     format= "%Y-%m-%d %H:%M",
                                     tz=tz))
    melt_df
  }

  df_after <- lengthen_3(df = df_after, date = paste0(substr(merged_soundscape@first_day, 1, 12)), lat = merged_soundscape@lat, lon = merged_soundscape@lon)

  df_after

    # 4.1. Check if the df_before and df_after lengthened
    # data frames are in the right format

  test_15 <- function(x){

    is.data.frame(x)

  }

  test_16 <- function(x){

    assertthat::not_empty(x)

  }

  test_17 <- function(x){

    assertthat::noNA(x)

  }

  test_18 <- function(x){

    nrow(x) == (nrow(merged_soundscape@merged_df)*ncol(merged_soundscape@merged_df))

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not a data frame. This functions builds on the output of merge_csv(). Make sure you're supplying the dataframe produced by the merge_csv() function")

  }

  assertthat::assert_that(test_15(df_before))
  assertthat::assert_that(test_16(df_before))
  assertthat::assert_that(test_17(df_before))
  assertthat::assert_that(test_18(df_before))

  assertthat::assert_that(test_15(df_after))
  assertthat::assert_that(test_16(df_after))
  assertthat::assert_that(test_17(df_after))
  assertthat::assert_that(test_18(df_after))


  # 4. Prepare other objects for ggplot

  timeinterval <- "12 hours"

  freqinterval <- (max(df_before$frequency)/10)

  mintime <- min(new_colnames)

  maxtime <- max(new_colnames)

  minfreq <- 0

  maxfreq <- max(df_before$frequency)

  # Make a plot

  plot_before <- suppressWarnings(

    ggplot2::ggplot(
      data = df_before,
      ggplot2::aes(x = time,
                   y = frequency,
                   fill = value)) +

    ggplot2::geom_tile() +

    viridis::scale_fill_viridis(
      option = "D",
      na.value="#383e42",
      direction = 1,
      guide = ggplot2::guide_legend(
        title.position = "top",
        title.vjust = 1,
        title.hjust = 0.5,
        nrow=1),
      breaks=round(
        seq(
          min(df_before$value),
          max(df_before$value),
          max(df_before$value)/10),
        digits = 2)) +


    ggplot2::scale_x_datetime(
      labels=scales::date_format("%H:%M", tz=merged_soundscape@tz),
      breaks = scales::date_breaks(timeinterval),
      expand = c(0,0),
      limits = c(mintime,maxtime)) +

    ggplot2::scale_y_continuous(
      limits = c(minfreq, (maxfreq+(maxfreq/10))),
      expand = c(0,0),
      breaks = seq(minfreq, maxfreq, freqinterval)) +
    ggplot2::labs(y="Frequency (Hz) \n", x="\nTime (hour of day)") +

    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.text.y = ggplot2::element_text(color = "black",
                                          size = 10),
      axis.text.x = ggplot2::element_text(color = "black",
                                          size = 10,
                                          angle = 45,
                                          hjust=1.1,
                                          vjust=1),
      axis.title.y = ggplot2::element_text(),
      axis.title.x = ggplot2::element_text(),
      plot.margin = grid::unit(c(1,1,1,1),"cm"),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(color = "black",
                                           size = 12,
                                           face = "bold")) +

    ggplot2::labs(fill="Index value")

    # ggplot2::annotate(geom= "label",
    #                   label = "BEFORE",
    #                   x = new_colnames[as.integer((length(new_colnames)/10)*9)],
    #                   y = (maxfreq - (maxfreq / 8)),
    #                   fill = "white",
    #                   color = "black",
    #                   fontface = 2)
  )

  plot_after <- suppressWarnings(

    ggplot2::ggplot(
      data = df_after,
      ggplot2::aes(x = time,
                   y = frequency,
                   fill = value)) +

    ggplot2::geom_tile() +

    viridis::scale_fill_viridis(
      option = "D",
      na.value="#383e42",
      direction = 1,
      guide = ggplot2::guide_legend(
        title.position = "top",
        title.vjust = 1,
        title.hjust = 0.5,
        nrow=1),
      breaks=seq(0, 1, 0.1)) +


    ggplot2::scale_x_datetime(
      labels=scales::date_format("%H:%M", tz=merged_soundscape@tz),
      breaks = scales::date_breaks(timeinterval),
      expand = c(0,0),
      limits = c(mintime,maxtime)) +

    ggplot2::scale_y_continuous(
      limits = c(minfreq, (maxfreq+(maxfreq/10))),
      expand = c(0,0),
      breaks = seq(minfreq, maxfreq, freqinterval)) +
    ggplot2::labs(y="Frequency (Hz) \n", x="\nTime (hour of day)") +

    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "black"),
      axis.text.y = ggplot2::element_text(color = "black",
                                          size = 10),
      axis.text.x = ggplot2::element_text(color = "black",
                                          size = 10,
                                          angle = 45,
                                          hjust=1.1,
                                          vjust=1),
      axis.title.y = ggplot2::element_text(),
      axis.title.x = ggplot2::element_text(),
      plot.margin = grid::unit(c(1,1,1,1),"cm"),
      legend.position = "top",
      legend.direction = "horizontal",
      legend.title = ggplot2::element_text(color = "black",
                                           size = 12,
                                           face = "bold")) +

    ggplot2::labs(fill="Index value")

    # ggplot2::annotate(geom= "label",
    #                   label = "AFTER",
    #                   x = new_colnames[as.integer((length(new_colnames)/10)*9)],
    #                   y = (maxfreq - (maxfreq / 8)),
    #                   fill = "white",
    #                   color = "black",
    #                   fontface = 2)
    )

  requireNamespace("patchwork")


  plot <- plot_before/plot_after

  suppressWarnings(print(plot))

}

#' Aggregate binarized dataframe
#'
#'@description Aggregates binarized spectral indices by time of day.
#' For each aggregate time period, the activity values (0 or 1)
#' are summed per frequency bin, and potentially divided by the number
#' of recordings for that time to get the proportion of
#' acoustically active recordings in each time-frequency bin
#' (the incidence frequency).
#'
#' @param binarized_soundscape The binarized soundscape object produced by
#'  \code{\link{binarize_df}}.

#' @param output Determines whether the function returns the raw
#' total number of detections (activity = 1) per time during the
#' recording period (output = "raw"), or the incidence frequency
#' (total number of detections / number of recordings for that time
#' - output = "incidence_freq).
#'
#' @return Returns a list containing three elements:
#' (i) aggregated_per_time: a list of soundscape samples
#' (the columns in the binarized_df function) grouped per
#'  unique time in the recording period; (
#'  ii) sampling_effort_per_time: a list indicating the
#'   sampling effort (number of soundscape samples) per
#'    unique time in the recording period;
#'    (iii) aggregated_df: an aggregated time-frequency
#'    data frame containing the number of acoustically
#'    active recordings in each bin for a set acoustic index.
#'
#' @export
#'
aggregate_df <- function(binarized_soundscape,
                         output = "incidence_freq"){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(binarized_soundscape))

  # 1. Check if function input meets expectations

  # 1.1. The supplied binarized_soundscape argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_2 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape")

  }

  test_3 <- function(x){

    assertthat::not_empty(x)

  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape'. Please supply the binarized_soundscape object produced by the binarize_df() function. Consult the package documentation for further information.")

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is an empty S4-object of the type 'soundscape'. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function.")

  }

  assertthat::assert_that(test_2(binarized_soundscape))
  assertthat::assert_that(test_3(binarized_soundscape))

  # 1.2. The binarized_soundscape elements are in the expected format

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

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_5(binarized_soundscape@lat))
  assertthat::assert_that(test_6(binarized_soundscape@lon))

  # 1.2.3. The time zone argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_7(binarized_soundscape@tz))

  # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

  # 1.2.5. The fileloc argument

  # test_8 <- function(x){
  #
  #   assertthat::is.dir(x) & assertthat::is.readable(x)
  #
  # }
  #
  # assertthat::assert_that(test_8(binarized_soundscape@fileloc))

  # 1.2.6. The index argument

  test_9 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_9(binarized_soundscape@index))

  # 1.2.7. The samplerate and window arguments

  test_10 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_10(binarized_soundscape@samplerate))
  assertthat::assert_that(test_10(binarized_soundscape@window))

  # 1.2.8. The binarization_method argument

  test_11 <- function(x){
    assertthat::is.string(x) & (x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom"))
  }



  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_11(binarized_soundscape@binarization_method))

  # 1.2.9. The threshold argument

  test_12 <- function(x){

    all(length(x) == 1 &
      is.double(x) & !is.na(x))

  }

  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a single numeric value. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function, and pay special attention to the value argument is you're supplying a custom threshold value.")

  }

  assertthat::assert_that(test_12(binarized_soundscape@threshold))

  # 1.2.10. The output argument

  test_14 <- function(x){

    is.na(x)

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " is not NA. Did you supply a post-binarization or post-aggregation soundscape to the check_thresh() function? Please supply the output of the merge_csv() function to this argument.")

  }

  assertthat::assert_that(test_14(binarized_soundscape@output))

  # 1.2.11. The merged_df argument

  test_15 <- function(x){

    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      limma::isNumeric(x)

  }

  test_16 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x)))<= binarized_soundscape@samplerate/2

  }

  test_17 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(binarized_soundscape@first_day, 1, 12)," ", colnames(x)),
        tz = binarized_soundscape@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the binarized_soundscape argument produced using the binarize_df() function? If so, something has gone wrong, please re-run the binarize_df() function.")

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of binarize_df(). Make sure you're supplying the dataframe produced by the binarize_df() function.")

  }

  assertthat::on_failure(test_17) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of binarize_df(). Make sure you're supplying the dataframe produced by the binarize_df() function.")

  }

  assertthat::assert_that(test_15(binarized_soundscape@merged_df))
  assertthat::assert_that(test_16(binarized_soundscape@merged_df))

  assertthat::assert_that(test_17(binarized_soundscape@merged_df))

  # 1.2.12. The binarized_df argument

  test_18 <- function(x){

    min(x) >= 0 &
      max(x) <= 1

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the binarize_df() function.")

  }

  assertthat::assert_that(test_15(binarized_soundscape@binarized_df))
  assertthat::assert_that(test_16(binarized_soundscape@binarized_df))
  assertthat::assert_that(test_17(binarized_soundscape@binarized_df))
  assertthat::assert_that(test_18(binarized_soundscape@binarized_df))

  # 1.2.13. The aggregated_df data frame and lists

  test_19 <- function(x){

    assertthat::are_equal(x[1, 1], "missing")

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " is not a missing data frame. Did you supply a post-aggregation soundscape to the binarized_soundscape argument of the binarize_df() function? Please supply the output of the binarize_df() function to this argument.")

  }

  assertthat::assert_that(test_19(binarized_soundscape@aggregated_df))

  test_20 <- function(x){

    all(sapply(x, function(x) is.na(x)))

  }

  assertthat::on_failure(test_20) <- function(call, env){

    paste0(deparse(call$x), " is not a list of NAs. Did you supply a post-aggregation soundscape to the binarized_soundscape argument of the binarize_df() function? Please supply the output of the binarize_df() function to this argument.")

  }

  assertthat::assert_that(test_20(binarized_soundscape@aggregated_df_per_time))
  assertthat::assert_that(test_20(binarized_soundscape@effort_per_time))

    # 1.3. Check if the supplied output argument is a string and
    # one of the available options.

  test_21 <- function(x){
   all(assertthat::is.string(x) & (x %in% c("raw", "incidence_freq")))
  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available output options. Please consult package documentation for available options.")

  }

  assertthat::assert_that(test_21(output))

  # Get a list of unique times in the dataframe, and sort chronologically

  colnames(binarized_soundscape@binarized_df) <- substr(colnames(binarized_soundscape@binarized_df), 1, 8)

  unique_times <- sort(
    hms::as_hms(
      strptime(
        unique(colnames(binarized_soundscape@binarized_df)),
        format = "%H:%M:%S")))

  # Subset the thresh_df per unique time, add up incidence values per time,
  # and compute the total sampling effort per time. Finally, compute the incidence
  # frequency per OSU for all OSUs in the dataframe

  aggregate_per_time <- vector("list", 0)
  aggregated_df <- vector("list", 0)
  sampling_effort_per_time <- vector("list", 0)

  for (i in 1:length(unique_times)){

    aggregate_per_time[[i]] <- binarized_soundscape@binarized_df[,grepl(as.character(unique_times[i]),
                                         x = colnames(binarized_soundscape@binarized_df))] # subset per time

    sampling_effort_per_time[[i]] <- ncol(aggregate_per_time[[i]]) # sampling effort

    aggregated_df[[i]] <- as.data.frame(rowSums(aggregate_per_time[[i]])) # incidence

    if(output=="incidence_freq"){

      aggregated_df[[i]] <- aggregated_df[[i]]/sampling_effort_per_time[[i]]

    }

    else{

      if(output=="raw"){

      }

      else{}

    }

  }


  names(aggregate_per_time) <- unique_times
  names(sampling_effort_per_time) <- unique_times

  aggregated_df <- rlist::list.cbind(aggregated_df)
  colnames(aggregated_df) <- unique_times

  aggregated_soundscape <- methods::new("soundscape",
                                        first_day = binarized_soundscape@first_day,
                                        lat = binarized_soundscape@lat,
                                        lon = binarized_soundscape@lon,
                                        tz = binarized_soundscape@tz,
                                        sunrise = binarized_soundscape@sunrise,
                                        sunset = binarized_soundscape@sunset,
                                        fileloc = binarized_soundscape@fileloc,
                                        index = binarized_soundscape@index,
                                        samplerate = binarized_soundscape@samplerate,
                                        window = binarized_soundscape@window,
                                        binarization_method = binarized_soundscape@binarization_method,
                                        threshold = binarized_soundscape@threshold,
                                        output = output,
                                        merged_df = binarized_soundscape@merged_df,
                                        binarized_df = binarized_soundscape@binarized_df,
                                        aggregated_df = aggregated_df,
                                        aggregated_df_per_time = aggregate_per_time,
                                        effort_per_time = sampling_effort_per_time)


  aggregated_soundscape


}

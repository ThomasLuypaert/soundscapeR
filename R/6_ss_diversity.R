# Functions for soundscape diversity calculations

#' Estimate soundscape diversity using Hill numbers
#'
#' @description For a set acoustic index, calculates the diversity of
#'  acoustically active Operational Sound Units (OSUs) in the soundscape.
#'  The q-parameter can be altered to modulate the diversity metric's
#'   sensitivity to abundance. The soundscape diversity metrics can be
#'    computed at various scales and resolutions. For instance, the user
#'     can explore the diversity for the whole soundscape, specify custom
#'      time and frequency limits, or use one of the built-in presets for
#'       diurnal-phase subsetting (day, night, dawn, dusk). Additionally,
#'       the user can track the change in soundscape diversity throughout
#'       the day. Finally, the soundscape diversity can be assessed for
#'       the entire frequency range, or per frequency-bin of
#'        user-defined width.
#'
#' \strong{Note:} Soundscape diversity metrics should not be used to make
#'  inference about the diversity of the real-world biological community
#'   unless verified using ground-truthing methods.
#'
#' @param aggregated_soundscape The aggregated soundscape object produced by
#'  \code{\link{ss_aggregate}} function.
#'
#' @param qvalue A positive integer or decimal number (>=0), most commonly
#'  between 0-3. This parameter modulates the sensitivity of diversity
#'  values to the relative abundance of Operational Sound Units (OSUs).
#'  A value of 0 corresponds to the richness, a value of 1 is the equivalent
#'   effective number of OSUs for the Shannon index, a value of 2 is the
#'    equivalent effective number of OSUs for the Simpson index.
#' @param subset The scale for which the soundscape diversity is computed.
#'  Options are 'total', 'day', night', 'dawn', 'dusk' and
#'  'tod' (time of day - for each unique time in the day).
#' @param minfreq A numeric value indicating the lower frequency limit
#' for which to compute the soundscape diversity. If set to default, uses
#' the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit
#' for which to compute the soundscape diversity. If set to default,
#' uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape
#'  diversity, formatted as "HH:MM:SS". If set to default, uses the
#'  earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape
#'  diversity, formatted as "HH:MM:SS". If set to default, uses the
#'  latest time for which data exists in the dataframe.
#' @param dawnstart A numeric argument. If subset == 'dawn', used to determine
#' the start of dawn. By default, dawn starts at sunrise. Expressed as
#' the time in seconds before sunrise.
#' @param dawnend A numeric argument. If subset == 'dawn', used to determine
#' the end of dawn. By default, dawn ends 1.5 hours after sunrise.
#' Expressed as the time in seconds after sunrise.
#' @param duskstart A numeric argument. If subset == 'dusk', used to determine
#' the start of dusk. By default, dusk starts 1.5 hours before sunset.
#'  Expressed as the time in seconds before sunset.
#' @param duskend A numeric argument. If subset == 'dusk', used to determine the
#'  end of dusk. By default, dusk ends at sunset. Expressed as the
#'  time in seconds after sunset.
#' @param freqseq A logical operator (TRUE/FALSE). If set to FALSE, will
#'  compute the diversity for the entire frequency range of the soundscape.
#'  If set to TRUE, will compute the diversity per frequency-bin of
#'   user-defined width (number of bins determined by nbins argument).
#' @param nbins A numeric argument. If freqseq is set to TRUE, determines
#' the number of the frequency-bins by which to divide the frequency range
#' to compute the soundscape diversity.
#' @param output A character string. Indicates the format in which
#' the soundscape diversity is expressed. Options are "percentage"
#' (the fraction between the observed soundscape diversity and the
#' maximum possible soundscape diversity), or "raw" (the number of
#' acoustically active OSUs in the soundscape). Defaults to "percentage".
#' @return Depending on the chosen parameters, returns the soundscape
#' diversity either a numeric value, a vector of values or a list of
#' vectors of values.
#' @export
ss_diversity =function(aggregated_soundscape,
                       qvalue,
                       subset="total",
                       mintime="default",
                       maxtime="default",
                       minfreq=0,
                       maxfreq="default",
                       dawnstart=0,
                       dawnend=5400,
                       duskstart=5400,
                       duskend=0,
                       freqseq=FALSE,
                       nbins=10,
                       output="percentage" ){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(aggregated_soundscape))
  assertthat::assert_that(test_0(qvalue))

  # 1. Check if function input meets expectations

  # 1.1. The supplied aggregated_soundscape argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_1 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape") &
      assertthat::not_empty(x)

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape', or is empty. Please supply the aggregated_soundscape object produced by the ss_aggregate() function. Consult the package documentation for further information.")

  }

  assertthat::assert_that(test_1(aggregated_soundscape))

  # 1.2. The aggregated_soundscape elements are in the expected format

  # 1.2.1. The first_day argument cannot be wrong (S4 property)

  # 1.2.2. The lat and lon argument

  test_3 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_4 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_3(aggregated_soundscape@lat))
  assertthat::assert_that(test_4(aggregated_soundscape@lon))

  # 1.2.3. The time zone argument

  test_5 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_5(aggregated_soundscape@tz))

  # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

  # 1.2.5. The fileloc argument
#
#   test_6 <- function(x){
#
#     assertthat::is.dir(x) & assertthat::is.readable(x)
#
#   }
#
#   assertthat::assert_that(test_6(aggregated_soundscape@fileloc))

  # 1.2.6. The index argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_7(aggregated_soundscape@index))

  # 1.2.7. The samplerate and window arguments

  test_8 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_8(aggregated_soundscape@samplerate))
  assertthat::assert_that(test_8(aggregated_soundscape@window))

  # 1.2.8. The binarization_method argument

  test_9 <- function(x){
    assertthat::is.string(x) & (x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom"))
  }



  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_9(aggregated_soundscape@binarization_method))

  # 1.2.9. The threshold argument

  test_10 <- function(x){

    all(length(x) == 1 &
          is.double(x) & !is.na(x))

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single numeric value. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the value argument is you're supplying a custom threshold value.")

  }

  assertthat::assert_that(test_10(aggregated_soundscape@threshold))

  # 1.2.10. The output argument

  test_11 <- function(x){

    all(length(x) == 1 & is.character(x) & (x %in% c("incidence_freq", "raw")))

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available output options. Did you supply the aggregated_soundscape argument produced using the ss_aggregate function? If so, something has gone wrong, please re-run the ss_aggregate() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.")

  }

  assertthat::assert_that(test_11(aggregated_soundscape@output))

  # 1.2.11. The merged_df argument

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
      max(as.numeric(rownames(x)))<= aggregated_soundscape@samplerate/2

  }

  test_14 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(aggregated_soundscape@first_day, 1, 12)," ", colnames(x)),
        tz = aggregated_soundscape@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.")

  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.")

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.")

  }

  assertthat::assert_that(test_12(aggregated_soundscape@merged_df))
  assertthat::assert_that(test_13(aggregated_soundscape@merged_df))
  assertthat::assert_that(test_14(aggregated_soundscape@merged_df))

  # 1.2.12. The binarized_df argument

  test_15 <- function(x){

    min(x) >= 0 &
      max(x) <= 1

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() function.")

  }

  assertthat::assert_that(test_12(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_13(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_14(aggregated_soundscape@binarized_df))
  assertthat::assert_that(test_15(aggregated_soundscape@binarized_df))

  # 1.2.12. The aggregated_df argument

  assertthat::assert_that(test_12(aggregated_soundscape@aggregated_df))
  assertthat::assert_that(test_13(aggregated_soundscape@aggregated_df))
  assertthat::assert_that(test_14(aggregated_soundscape@aggregated_df))

  if(aggregated_soundscape@output=="incidence_freq"){

    test_16 <- function(x){

      all(is.double(unlist(x)) & max(x) <= 1 & min(x)>= 0)

    }

    assertthat::on_failure(test_16) <- function(call, env){

      paste0(deparse(call$x), " contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the aggregated_soundscape argument produced using the ss_aggregate function? If so, something has gone wrong, please re-run the ss_aggregate() function, and pay special attention to the output argument.")

    }

    assertthat::assert_that(test_16(aggregated_soundscape@aggregated_df))
  }

  if(aggregated_soundscape@output=="raw"){

    test_16 <- function(x){

      all(all(round(unlist(x)) == unlist(x)) &
            max(x) <= max(table(colnames(aggregated_soundscape@merged_df))) &
            min(x) >= 0)

    }

    assertthat::on_failure(test_16) <- function(call, env){

      paste0(deparse(call$x), " contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the aggregated_soundscape argument produced using the ss_aggregate function? If so, something has gone wrong, please re-run the ss_aggregate() function, and pay special attention to the output argument.")

    }

    assertthat::assert_that(test_16(aggregated_soundscape@aggregated_df))
  }

  # 1.2.13. The aggregated_df_per_time argument

  test_17_1 <- function(x){

      all(sapply(x, function(x) is.data.frame(x))) &
      length(x) == ncol(aggregated_soundscape@aggregated_df)
  }

  test_17_2 <- function(x){

      all(sapply(x, function(x) nrow(x)==nrow(aggregated_soundscape@merged_df)))

  }


  assertthat::on_failure(test_17_1) <- function(call, env){

    paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the ss_aggregate function? If so, something has gone wrong, please re-run the ss_aggregate() function.")

  }

  assertthat::on_failure(test_17_2) <- function(call, env){

    paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the ss_aggregate function? If so, something has gone wrong, please re-run the ss_aggregate() function.")

  }

  assertthat::assert_that(test_17_1(aggregated_soundscape@aggregated_df_per_time))
  assertthat::assert_that(test_17_2(aggregated_soundscape@aggregated_df_per_time))

  # 1.2.14. The effort_per_time argument

  # test_18 <- function(x){
  #
  #   identical(as.list(sort(table(colnames(aggregated_soundscape@merged_df)))), x)
  #
  # }
  #
  # assertthat::on_failure(test_18) <- function(call, env){
  #
  #   paste0(deparse(call$x), " does not have the expected format. Did you supply the aggregated_soundscape argument produced using the ss_aggregate function? If so, something has gone wrong, please re-run the ss_aggregate() function.")
  #
  # }
  #
  # assertthat::assert_that(test_18(aggregated_soundscape@effort_per_time))

  # 1.3. The supplied qvalue argument is a positive integer or decimal number

  test_5 <- function(x){
      !is.character(x)
  }

  test_6 <- function(x){
    !is.list(x)
  }

  test_7 <- function(x){

   is.numeric(x)

  }

  test_8 <- function(x){

    abs(x) == x

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is a character string of length 1. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is a list. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not an numeric/integer value. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a positive value. Please supply the qvalue argument as a positive numeric or integer value.")

  }

  assertthat::assert_that(test_5(qvalue))
  assertthat::assert_that(test_6(qvalue))
  assertthat::assert_that(test_7(qvalue))
  assertthat::assert_that(test_8(qvalue))

  # 1.4. The subset argument is a character string, and one of the
  # available options

  test_9 <- function(x){
    assertthat::is.string(x)
  }

  test_10 <- function(x){
    x %in% c("total","day", "night", "dawn", "dusk", "tod")
  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the ss_diversity subset argument as a character string. Consult package documentation for available subset argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available ss_diversity subset options. Please consult package documentation for available subset argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_9(subset))
  assertthat::assert_that(test_10(subset))


  # 1.5. the supplied mintime and maxtime arguments
  # are one of the available options

  test_15 <- function(x){

    is.character(x) & (

    x == "default" |
      !is.na(as.POSIXct(x, format="%H:%M:%S")))

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.")

  }

  assertthat::assert_that(test_15(mintime))
  assertthat::assert_that(test_15(maxtime))

  # 1.6. The minfreq and maxfreq arguments follow
  # the expected values

  test_16 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
       x <= max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))) |
      x == 0

  }

  test_17 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
       x <= max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))) |
      x == "default"

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::on_failure(test_17) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::assert_that(test_16(minfreq))
  assertthat::assert_that(test_17(maxfreq))

  # 1.8. The dawnstart, dawnend, duskstart and duskend arguments are either
  # zero or a single positive integer

  test_19 <- function(x){

    x == 0 | assertthat::is.count(x)

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.")

  }

  assertthat::assert_that(test_19(dawnstart))
  assertthat::assert_that(test_19(dawnend))
  assertthat::assert_that(test_19(duskstart))
  assertthat::assert_that(test_19(duskend))

  # 1.9. The freqseq argument is a boolean flag

  test_20 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_20) <- function(call, env){

    paste0(deparse(call$x), " is not a Boolean flag (TRUE or FALSE). Please set the freqseq argument to TRUE or FALSE. Make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_20(freqseq))

  # 1.10. Check if the nbins argument abides by the
  # expected format

  test_21 <- function(x){

    assertthat::is.count(x) &
      x > 0 &
      x < nrow(aggregated_soundscape@aggregated_df)
  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.")

  }

  assertthat::assert_that(test_21(nbins))

  # 1.11. The output argument is a string and one of the available options

  test_22 <- function(x){
    assertthat::is.string(x)
  }

  test_23 <- function(x){
    x %in% c("percentage", "raw")
  }

  assertthat::on_failure(test_22) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the output argument as a character string. Consult package documentation for available output argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_23) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available ss_diversity output options. Please consult package documentation for available output argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_22(output))
  assertthat::assert_that(test_23(output))

  # 2. Create a multiplier variable to multiply the OSU values by for
  # different output options

  if (output=="raw"){
    multiplier <- 1}

  else{

    if(output=="percentage"){
      multiplier <- 100}

  }

  # 3. Create the diurnal phase subsetting objects

  tz <- aggregated_soundscape@tz

  day <- as.POSIXct(
    strptime(
      paste(substr(aggregated_soundscape@first_day, 1, 12),
            "00:00:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))

  sunrise <- aggregated_soundscape@sunrise

  sunset <- aggregated_soundscape@sunset

  # 4. Set minfreq, maxfreq, mintime and maxtime arguments

  if (maxfreq=="default"){
    maxfreq <- max(
      as.numeric(
        rownames(aggregated_soundscape@aggregated_df)))
  }

  else{maxfreq <- maxfreq}

  if (mintime=="default"){
    mintime <- min(
      as.POSIXct(
        strptime(
          paste(
            substr(aggregated_soundscape@first_day, 1, 12),
            colnames(aggregated_soundscape@aggregated_df),
            sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz=tz)))
  }

  else{mintime <- as.POSIXct(
    strptime(
      paste(
        substr(aggregated_soundscape@first_day, 1, 12),
        mintime,
        sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))}


  if (maxtime=="default"){
    maxtime <- max(
      as.POSIXct(
        strptime(
          paste(substr(aggregated_soundscape@first_day, 1, 12),
                colnames(aggregated_soundscape@aggregated_df),
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz=tz)))
  }

  else{maxtime <- as.POSIXct(
    strptime(
      paste(
        substr(aggregated_soundscape@first_day, 1, 12),
        maxtime,
        sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))}

  # 5. Set row names, column names and subsetting objects + create new df

  rownames_df <- as.numeric(
    rownames(aggregated_soundscape@aggregated_df))

  rownames_subset <- as.character(
    subset(rownames_df,
           rownames_df >= minfreq &
             rownames_df <= maxfreq))

  colnames_df <- as.POSIXct(
    strptime(
      paste(substr(aggregated_soundscape@first_day, 1, 12),
            colnames(aggregated_soundscape@aggregated_df),
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))

  colnames_subset <- as.character(
    hms::as_hms(
      subset(colnames_df,
             colnames_df >= mintime &
               colnames_df <= maxtime)))

  new_df <- aggregated_soundscape@aggregated_df[rownames_subset,colnames_subset]

  # Compute the soundscape diversity under different scenarios


  if (freqseq=="FALSE"){

    if (subset == "total"){

      soundscape_diversity <-
        hilldiv::hill_div(unlist(new_df),
                          qvalue=qvalue) / if (output=="raw"){1}
      else{
        if(output=="percentage"){
          (ncol(new_df)*nrow(new_df))
        }
      }

      soundscape_diversity <- soundscape_diversity*multiplier

      soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

      soundscape_diversity

    }
    else{

      if (subset == "tod"){

        soundscape_diversity <- c()

        for (i in 1:ncol(new_df)){
          soundscape_diversity[i] <-
            hilldiv::hill_div(unlist(new_df[[i]]),
                              qvalue = qvalue)/ if (output=="raw"){1}
          else{
            if(output=="percentage"){length(new_df[[i]])}
          }

          if(sum(unlist(new_df[[i]]))==0){
            soundscape_diversity[i] <- 0
          }
        }

        soundscape_diversity <- soundscape_diversity*multiplier

        soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

        soundscape_diversity <- as.data.frame(soundscape_diversity)

        soundscape_diversity$time <- hms::as_hms(
          (as.POSIXct(
            strptime(
              paste(substr(aggregated_soundscape@first_day, 1, 12),
                    colnames(aggregated_soundscape@aggregated_df),
                    sep=" "),
              format= "%Y-%m-%d %H:%M:%S",
              tz=tz))))

        colnames(soundscape_diversity) <- c("soundscape_div", "time_of_day")

        soundscape_diversity
      }

      else{

        if (subset == "day"){

          colnames_day <-
            as.character(
              hms::as_hms(
                subset(colnames_df,
                       colnames_df >= sunrise &
                         colnames_df <= sunset)))

          daytime_df <- aggregated_soundscape@aggregated_df[rownames_subset,colnames_day]

          soundscape_diversity <-
            hilldiv::hill_div(
              unlist(daytime_df),
              qvalue=qvalue) / if (output=="raw"){1}
          else{
            if(output=="percentage"){
              (ncol(daytime_df)*nrow(daytime_df))
            }
          }

          soundscape_diversity <- soundscape_diversity*multiplier

          soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

          soundscape_diversity

        }

        else{

          if(subset == "night"){

            colnames_night <-
              as.character(
                hms::as_hms(
                  subset(colnames_df,
                         colnames_df < sunrise |
                           colnames_df > sunset)))

            nighttime_df <- aggregated_soundscape@aggregated_df[rownames_subset,
                                                colnames_night]

            soundscape_diversity <-
              hilldiv::hill_div(
                unlist(nighttime_df),
                qvalue=qvalue) / if (output=="raw"){1}
            else{
              if(output=="percentage"){
                (ncol(nighttime_df)*nrow(nighttime_df))
              }
            }

            soundscape_diversity <- soundscape_diversity*multiplier

            soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

            soundscape_diversity
          }

          else{

            if (subset == "dawn"){

              colnames_dawn <- as.character(
                hms::as_hms(
                  subset(colnames_df,
                         colnames_df >= (sunrise - dawnstart) &
                           colnames_df <= (sunrise + dawnend))))

              dawntime_df <- aggregated_soundscape@aggregated_df[rownames_subset,
                                                 colnames_dawn]

              soundscape_diversity <-
                hilldiv::hill_div(
                  unlist(dawntime_df),
                  qvalue=qvalue) / if (output=="raw"){1}
              else{
                if(output=="percentage"){
                  (ncol(dawntime_df)*nrow(dawntime_df))
                }
              }

              soundscape_diversity <- soundscape_diversity*multiplier

              soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

              soundscape_diversity

            }

            else{

              if (subset == "dusk"){

                colnames_dusk <- as.character(
                  hms::as_hms(
                    subset(colnames_df,
                           colnames_df >= (sunset - duskstart) &
                             colnames_df <= (sunset + duskend))))

                dusktime_df <- aggregated_soundscape@aggregated_df[rownames_subset,
                                                   colnames_dusk]

                soundscape_diversity <-
                  hilldiv::hill_div(
                    unlist(dusktime_df),
                    qvalue=qvalue) / if (output=="raw"){1}
                else{
                  if(output=="percentage"){
                    (ncol(dusktime_df)*nrow(dusktime_df))
                  }
                }

                soundscape_diversity <- soundscape_diversity*multiplier

                soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

                soundscape_diversity

              }
            }
          }
        }
      }
    }
  }

  else{

    if (freqseq=="TRUE"){

      new_df$frequency <- as.numeric(rownames(new_df))
      minfreq_label <- min(new_df$frequency)
      maxfreq_label <- max(new_df$frequency)
      rownames <- as.numeric(rownames(new_df))

      freq_list_1 <- vector("list", 0)
      freq_list_2 <- vector("list", 0)
      rownames_1 <- vector("list", 0)
      rownames_2 <- vector("list", 0)


      for (i in 1:nbins){

        freq_list_1[[i]] <- subset(new_df,
                                new_df$frequency < (i*(maxfreq/nbins)))

        rownames_1[[i]] <- subset(rownames,
                                  rownames < (i*(maxfreq/nbins)))

      }

      for (i in 1:nbins){

        freq_list_2[[i]] <-
          subset(freq_list_1[[i]],
                 freq_list_1[[i]]$frequency > ((i-1)*(maxfreq/nbins)))

        freq_list_2[[i]]$frequency <- NULL

        rownames_2[[i]] <- as.character(
          subset(rownames_1[[i]],
                 rownames_1[[i]] > ((i-1)*(maxfreq/nbins))))

      }

      binnames_min <- vector("list", 0)
      binnames_max <- vector("list", 0)
      binnames_tot <- vector("list", 0)

      for (i in 1:length(freq_list_2)){

        binnames_min[[i]] <- min(as.numeric(rownames(freq_list_2[[i]]))) -
          as.integer(aggregated_soundscape@samplerate / aggregated_soundscape@window)
        binnames_max[[i]] <- max(as.numeric(rownames(freq_list_2[[i]])))
        binnames_tot[[i]] <- paste0(binnames_min[[i]], " - ", binnames_max[[i]], " Hz")
      }

      binnames_tot <- unlist(binnames_tot)

      if (subset == "total"){

        soundscape_diversity <- c()

        for (i in 1:length(freq_list_2)){

          soundscape_diversity[i] <-
            hilldiv::hill_div(
              unlist(freq_list_2[[i]]),
              qvalue=qvalue) / if (output=="raw"){1}

          else{
            if(output=="percentage"){
              (ncol(freq_list_2[[i]])*nrow(freq_list_2[[i]]))
            }
          }
        }

        for (i in 1:length(freq_list_2)){

          if (sum(unlist(freq_list_2[i]))==0){
            soundscape_diversity[i] <- 0
          }

          else{
            soundscape_diversity[i] <- soundscape_diversity[i]
          }
        }

        soundscape_diversity <- soundscape_diversity*multiplier
        soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

        soundscape_diversity <- as.data.frame(soundscape_diversity)

        soundscape_diversity$frequency_bin <- binnames_tot

        colnames(soundscape_diversity) <- c("soundscape_div", "freq_interval")

        soundscape_diversity
      }


      else{

        if (subset == "tod"){

          soundscape_diversity <- vector("list",
                                         length(freq_list_2))

          for (i in 1:length(freq_list_2)){

            soundscape_diversity[[i]] <- vector("list",
                                                ncol(freq_list_2[[i]]))

            for (j in 1:ncol(freq_list_2[[i]])){

              soundscape_diversity[[i]][[j]] <-
                hilldiv::hill_div(
                  unlist(freq_list_2[[i]][[j]]),
                  qvalue = qvalue) / if (output=="raw"){1}
              else{
                if(output=="percentage"){
                  length(freq_list_2[[i]][[j]])
                }
              }
            }
          }

          soundscape_diversity

          for (i in 1:length(freq_list_2)){

            for (j in 1:ncol(freq_list_2[[i]])){

              if (all.equal(rep(0,length(freq_list_2[[i]][[j]])),
                            freq_list_2[[i]][[j]])==TRUE){

                soundscape_diversity[[i]][[j]] <- 0

              }

              else{

                soundscape_diversity[[i]][[j]] <- soundscape_diversity[[i]][[j]]

              }
            }
          }

          for (i in 1:length(soundscape_diversity)){

            soundscape_diversity[[i]] <- unlist(soundscape_diversity[[i]])

          }

          for (i in 1:length(soundscape_diversity)){

            soundscape_diversity[[i]] <- soundscape_diversity[[i]]*multiplier

            soundscape_diversity[[i]][!is.finite(soundscape_diversity[[i]])] <- 0
            soundscape_diversity[[i]] <- as.data.frame(
              soundscape_diversity[[i]])

            soundscape_diversity[[i]]$time <-
              hms::as_hms(
                (as.POSIXct(
                  strptime(
                    paste(substr(aggregated_soundscape@first_day, 1, 12),
                          colnames(aggregated_soundscape@aggregated_df),
                          sep=" "),
                    format= "%Y-%m-%d %H:%M:%S",
                    tz=tz))))

            colnames(soundscape_diversity[[i]]) <- c("soundscape_div", "time_of_day")
          }

          names(soundscape_diversity) <- binnames_tot

          soundscape_diversity

        }

        else{

          if (subset == "day"){

            colnames_day <- as.character(
              hms::as_hms(
                subset(colnames_df,
                       colnames_df >= sunrise &
                         colnames_df <= sunset)))

            freq_list_day <- vector("list", 0)

            for (i in 1:length(freq_list_2)){

              freq_list_day[[i]] <- freq_list_2[[i]][rownames_2[[i]],
                                                     colnames_day]
            }

            soundscape_diversity <- c()

            for (i in 1:length(freq_list_day)){

              soundscape_diversity[i] <-
                hilldiv::hill_div(
                  unlist(freq_list_day[[i]]),
                  qvalue=qvalue) / if (output=="raw"){1}

              else{

                if(output=="percentage"){
                  (ncol(freq_list_day[[i]])*nrow(freq_list_day[[i]]))
                }
              }
            }

            for (i in 1:length(freq_list_day)){

              if (identical(rep(0, length(freq_list_day[[i]])),
                            freq_list_day[[i]])){
                soundscape_diversity[[i]] <- 0
              }

              else{
                soundscape_diversity[[i]] <- soundscape_diversity[[i]]
              }
            }

            soundscape_diversity <- soundscape_diversity*multiplier
            soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

            soundscape_diversity <- as.data.frame(soundscape_diversity)

            soundscape_diversity$frequency_bin <-  binnames_tot

            colnames(soundscape_diversity) <- c("soundscape_div", "freq_interval")

            soundscape_diversity

          }

          else{

            if (subset == "night"){

              colnames_night <- as.character(
                hms::as_hms(
                  subset(colnames_df,
                         colnames_df <= sunrise |
                           colnames_df >= sunset)))

              freq_list_night <- vector("list", 0)

              for (i in 1:length(freq_list_2)){

                freq_list_night[[i]] <- freq_list_2[[i]][rownames_2[[i]],
                                                         colnames_night]

              }

              soundscape_diversity <- c()

              for (i in 1:length(freq_list_night)){

                soundscape_diversity[i] <-
                  hilldiv::hill_div(
                    unlist(freq_list_night[[i]]),
                    qvalue=qvalue) / if (output=="raw"){1}

                else{

                  if(output=="percentage"){

                    (ncol(freq_list_night[[i]])*nrow(freq_list_night[[i]]))

                  }
                }
              }

              for (i in 1:length(freq_list_night)){

                if (identical(rep(0, length(freq_list_night[[i]])),
                              freq_list_night[[i]])){
                  soundscape_diversity[[i]] <- 0
                }

                else{
                  soundscape_diversity[[i]] <- soundscape_diversity[[i]]
                }
              }

              soundscape_diversity <- soundscape_diversity*multiplier
              soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

              soundscape_diversity <- as.data.frame(soundscape_diversity)

              soundscape_diversity$frequency_bin <- binnames_tot

              colnames(soundscape_diversity) <- c("soundscape_div", "freq_interval")

              soundscape_diversity

            }

            else{

              if (subset == "dawn"){

                colnames_dawn <- as.character(
                  hms::as_hms(
                    subset(colnames_df,
                           colnames_df > (sunrise-dawnstart) &
                             colnames_df <= (sunrise+dawnend))))

                freq_list_dawn <- vector("list", 0)

                for (i in 1:length(freq_list_2)){
                  freq_list_dawn[[i]] <- freq_list_2[[i]][rownames_2[[i]],
                                                          colnames_dawn]
                }

                soundscape_diversity <- c()

                for (i in 1:length(freq_list_dawn)){

                  soundscape_diversity[i] <-
                    hilldiv::hill_div(
                      unlist(freq_list_dawn[[i]]),
                      qvalue=qvalue) / if (output=="raw"){1}

                  else{

                    if(output=="percentage"){

                      (ncol(freq_list_dawn[[i]])*nrow(freq_list_dawn[[i]]))

                    }
                  }
                }

                for (i in 1:length(freq_list_dawn)){

                  if (identical(rep(0, length(freq_list_dawn[[i]])),
                                freq_list_dawn[[i]])==TRUE){
                    soundscape_diversity[[i]] <- 0
                  }

                  else{
                    soundscape_diversity[[i]] <- soundscape_diversity[[i]]
                  }
                }

                soundscape_diversity <- soundscape_diversity*multiplier

                soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

                soundscape_diversity <- as.data.frame(soundscape_diversity)

                soundscape_diversity$frequency_bin <- binnames_tot

                colnames(soundscape_diversity) <- c("soundscape_div", "freq_interval")

                soundscape_diversity

              }


              else{

                if (subset == "dusk"){

                  colnames_dusk <- as.character(
                    hms::as_hms(
                      subset(colnames_df,
                             colnames_df > (sunset - duskstart) &
                               colnames_df <= (sunset + duskend))))

                  freq_list_dusk <- vector("list", 0)

                  for (i in 1:length(freq_list_2)){
                    freq_list_dusk[[i]] <- freq_list_2[[i]][rownames_2[[i]],
                                                            colnames_dusk]
                  }


                  soundscape_diversity <- c()

                  for (i in 1:length(freq_list_dusk)){

                    soundscape_diversity[i] <-
                      hilldiv::hill_div(
                        unlist(freq_list_dusk[[i]]),
                        qvalue=qvalue) / if (output=="raw"){1}

                    else{

                      if(output=="percentage"){

                        (ncol(freq_list_dusk[[i]])*nrow(freq_list_dusk[[i]]))

                      }
                    }
                  }
                }

                  for (i in 1:length(freq_list_dusk)){

                    if (identical(rep(0, length(freq_list_dusk[[i]])),
                                  freq_list_dusk[[i]])==TRUE){
                      soundscape_diversity[[i]] <- 0
                    }

                    else{
                      soundscape_diversity[[i]] <- soundscape_diversity[[i]]
                    }
                  }

                  soundscape_diversity <- soundscape_diversity*multiplier

                  soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
                  soundscape_diversity <- as.data.frame(soundscape_diversity)

                  soundscape_diversity$frequency_bin <- binnames_tot

                  colnames(soundscape_diversity) <- c("soundscape_div", "freq_interval")

                  soundscape_diversity

              }
            }
          }
        }
      }
    }
  }
}

# Function for soundscape evenness calculation

#' Estimate the soundscape evenness using Hill numbers
#'
#' @description Calculated the soundscape evenness index for the provided soundscape object.
#' The user can explore the diversity for the whole soundscape, specify custom
#' time and frequency limits, or use one of the built-in presets for
#' diurnal-phase subsetting (day, night, dawn, dusk). Additionally, the user
#' can track the change in soundscape diversity throughout the day.
#'
#' \strong{Note:} Soundscape diversity metrics should not be used to make
#'  inference about the diversity of the real-world biological community
#'   unless verified using ground-truthing methods.
#'
#' \strong{Note:} In \href{https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13924}{Luypaert et al. (2022)},
#' we stated that the soundscape evenness index should be calculated as:
#' soundscape diversity (2D) / soundscape richness (0D).
#'
#' However, based on recent publications, a more proper way to calculate the
#' soundscape evenness is:
#' (soundscape diversity (2D) - 1) / (soundscape richness (0D) - 1).
#'
#' Therefore, from now on, we will use the latter equation to calculate the soundscape evenness index.
#'
#' @param aggregated_soundscape The aggregated soundscape object produced by
#'  \code{\link{ss_aggregate}} function.

#' @param subset The scale for which the soundscape diversity is computed.
#'  Options are 'total', 'day', night', 'dawn', 'dusk' and
#'  'tod' (time of day - for each unique time in the day).
#' @param minfreq A numeric value indicating the lower frequency limit
#' for which to compute the soundscape diversity. If set to default, uses
#' the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit
#' for which to compute the soundscape diversity. If set to default,
#' uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape
#'  diversity, formatted as "HH:MM:SS". If set to default, uses the
#'  earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape
#'  diversity, formatted as "HH:MM:SS". If set to default, uses the
#'  latest time for which data exists in the dataframe.
#' @param dawnstart A numeric argument. If subset == 'dawn', used to determine
#' the start of dawn. By default, dawn starts at sunrise. Expressed as
#' the time in seconds before sunrise.
#' @param dawnend A numeric argument. If subset == 'dawn', used to determine
#' the end of dawn. By default, dawn ends 1.5 hours after sunrise.
#' Expressed as the time in seconds after sunrise.
#' @param duskstart A numeric argument. If subset == 'dusk', used to determine
#' the start of dusk. By default, dusk starts 1.5 hours before sunset.
#'  Expressed as the time in seconds before sunset.
#' @param duskend A numeric argument. If subset == 'dusk', used to determine the
#'  end of dusk. By default, dusk ends at sunset. Expressed as the
#'  time in seconds after sunset.
#' @return Depending on the chosen parameters, returns the soundscape
#' diversity either a numeric value, a vector of values or a list of
#' vectors of values.
#' @export
ss_evenness <- function(aggregated_soundscape = aggregated_soundscape,
                        subset = "total",
                        mintime = "default",
                        maxtime = "default",
                        minfreq = 0,
                        maxfreq = "default",
                        dawnstart=0,
                        dawnend=5400,
                        duskstart=5400,
                        duskend=0){


  soundscape_richness <- soundscapeR::ss_diversity(aggregated_soundscape = aggregated_soundscape,
                                                   qvalue = 0,
                                                   subset = subset,
                                                   mintime = mintime,
                                                   maxtime = maxtime,
                                                   minfreq = minfreq,
                                                   maxfreq = maxfreq,
                                                   dawnstart = dawnstart,
                                                   dawnend = dawnend,
                                                   duskstart = duskstart,
                                                   duskend = duskend,
                                                   freqseq = FALSE,
                                                   output =  "raw")

  soundscape_diversity <- soundscapeR::ss_diversity(aggregated_soundscape = aggregated_soundscape,
                                                    qvalue = 2,
                                                    subset = subset,
                                                    mintime = mintime,
                                                    maxtime = maxtime,
                                                    minfreq = minfreq,
                                                    maxfreq = maxfreq,
                                                    dawnstart = dawnstart,
                                                    dawnend = dawnend,
                                                    duskstart = duskstart,
                                                    duskend = duskend,
                                                    freqseq = FALSE,
                                                    output =  "raw")

  if(subset == "tod"){

    soundscape_evenness <- data.frame(
      soundscape_evenness = (soundscape_diversity$soundscape_div - 1) / (soundscape_richness$soundscape_div - 1),
      time_of_day = soundscape_diversity$time_of_day)

    return(soundscape_evenness)


  } else{

  soundscape_evenness <- (soundscape_diversity - 1) / (soundscape_richness - 1)

  }

  return(soundscape_evenness)


}



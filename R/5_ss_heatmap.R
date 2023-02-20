#' Flexible Soundscape Heatmaps
#'
#' @description  Creates a soundscape heatmap to visualize
#'  the use of acoustic space in the time-frequency domain
#'   for an set acoustic index. The function is highly
#'   flexible, allowing the user maximal control over every
#'    aspect of the heatmap. Please consult the arguments
#'     section to find out more about visualization options.
#'
#' @param aggregated_soundscape The aggregated soundscape object produced by
#'  \code{\link{ss_aggregate}} function.
#'
#' @param type One of either "regular" or "polar". If set
#' to "regular", produces a regular rectangular heatmap.
#' If set to "polar", produces a polar heatmap suitable for
#'  exploring diurnal patterns.
#'
#' @param annotate One of either TRUE or FALSE. If set to
#'  TRUE, annotates the heatmap with sunrise and
#'  sunset times,´and highlights the border between the
#'   audible and ultrasonic spectrum for human hearing.
#'
#' @param timeinterval A time interval for the x-axis.
#'  Options can be found in the
#'  \code{\link[scales]{breaks_width}} documentation.
#'
#' @param mintime The lower time limit for the x-axis,
#'  formatted as "HH:MM:SS". Defaults to the earliest
#'  time for which data exists in the dataframe.
#'
#' @param maxtime The upper time limit for the x-axis,
#'  formatted as "HH:MM:SS".Defaults to the latest
#'  time for which data exists in the dataframe.
#'
#' @param freqinterval The frequency interval for the y-axis,
#'  expressed as a numeric value.
#'
#' @param minfreq The lower frequency limit for the y-axis
#' as a numeric value. Defaults to zero.
#'
#' @param maxfreq The upper frequency limit for the y-axis
#' as a numeric value. Defaults to the maximum frequency of
#'  the dataframe.
#'
#' @param nbins If \code{marginplot=TRUE}, determines the
#'  number of the frequency-bins by which to divide the
#'   frequency range to compute the soundscape richness
#'   (q=0), expressed as a single positive integer.
#'
#' @param labelsize_time If annotate=TRUE, can be used to
#'  alter the size of temporal spectrum annotation. Please
#'   provide as a single positive integer with a value > 0.
#'
#' @param labelsize_frequency If annotate=TRUE, can be used
#'  to alter the size of frequency spectrum annotation.
#'  Please provide as a single positive integer with a
#'  value > 0.
#'
#' @param labelsize_polar If type="polar", can be used to
#'  alter the size of the frequency axis labels. Please
#'   provide as a single positive integer with a value > 0.
#'
#' @param palette A character string indicating the
#' colormap option to use. Four options are available:
#'  "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"),
#'  "viridis" (or "D", the default option) and "cividis"
#'  (or "E"). Consult \url{https://www.rdocumentation.org/packages/viridisLite/versions/0.3.0/topics/viridis} for options.
#'
#' @param direction Sets the order of colors in the scale.
#' If 1, the default, the regular order is followed.
#' If -1, the order of colors is reversed.
#'
#' @param zero.black One of either TRUE or FALSE.
#' If set to TRUE, absent OSUs with incidence zero
#' will be colored black
#'
#' @param marginplot One of either TRUE or FALSE.
#' If set to TRUE, adds marginal plots to the x- and y-axes.
#' For the x-axis, the marginal plot displays the smoothed
#'  richness of acoustically active frequency bins for
#'  each time of day. For the y-axis, the marginal plot
#'   displays the smoothed richness of acoustically
#'   active time-bins for each frequency band. Note that
#'    marginal plots are not available for type="polar".
#'
#' @param n_time If marginplot=TRUE, determines the backward
#'  window length for smoothing the temporal richness.
#'  Please supply the argument as a single positive integer.
#'
#' @param n_freq If marginplot=TRUE, determines the backward
#'  window length for smoothing the frequency richness.
#'  Please supply the argument as a single positive integer.
#'
#' @param interactive One of either TRUE or FALSE.
#' If set to TRUE, an interactive plot is produced using
#'  \code{\link[plotly]{ggplotly}}. Note that interactive
#'   plots are not available for marginplot=TRUE.
#'
#' @param save One of either TRUE or FALSE. If set to TRUE,
#'  saves the plot using \code{\link[ggplot2]{ggsave}}, and
#'   the 'dir', 'filename' and 'device' arguments.
#'
#' @param dir Path of the directory to save plot to:
#' path and filename are combined to create the
#' fully qualified file name. Defaults to the working
#'  directory. For more information consult
#'   \code{\link[ggplot2]{ggsave}}.
#'
#' @param filename The file name without the extension. For
#'  more information consult \code{\link[ggplot2]{ggsave}}.
#'
#' @param device Device to use. Can either be a device
#'  function (e.g. png()), or one of "eps", "ps",
#'  "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp",
#'   "svg" or "wmf" (windows only). Defaults to "png".
#'   For more information consult
#'    \code{\link[ggplot2]{ggsave}}.
#'
#' @param width If save=TRUE, expresses the width of the
#'  saved image in milimeters. Defaults to 100 mm.
#'
#' @param height If save=TRUE, expresses the height of the
#'  saved image in milimeters. Defaults to 100 mm.
#'
#' @return Returns a ggplot heatmap object and if save=TRUE,
#'  saves the plot in a directory of choice using a
#'  specified device and filename.
#'
#' @export

ss_heatmap=function(aggregated_soundscape,
                    type="regular",
                    annotate=FALSE,
                    timeinterval="1 hour",
                    mintime="default",
                    maxtime="default",
                    freqinterval=2000,
                    minfreq=0,
                    maxfreq="default",
                    nbins=10,
                    labelsize_time=4,
                    labelsize_frequency=4,
                    labelsize_polar=3,
                    palette="D",
                    direction=1,
                    zero.black=FALSE,
                    marginplot=FALSE,
                    n_time=10,
                    n_freq=2,
                    interactive=FALSE,
                    save=FALSE,
                    dir="default",
                    filename="file",
                    device="png",
                    width=100,
                    height=100){

  # 0.0. Setting binding for global variables

  time <- value <- comma <- unit <- guides <- richness_smooth <- NULL

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(aggregated_soundscape))

  # 1. Check if function input meets expectations

  # 1.1. The supplied aggregated_soundscape argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_1 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape") &
      assertthat::not_empty(x)

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape'. Please supply the aggregated_soundscape object produced by the ss_aggregate() or ss_create() functions. Consult the package documentation for further information.")

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

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the aggregated_soundscape produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_3(aggregated_soundscape@lat))
  assertthat::assert_that(test_4(aggregated_soundscape@lon))

    # 1.2.3. The time zone argument

  test_5 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_5(aggregated_soundscape@tz))

    # 1.2.6. The index argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_7(aggregated_soundscape@index))

    # 1.2.7. The samplerate and window arguments

  test_8 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.")

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

    paste0(deparse(call$x), " is not a single numeric value. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the value argument is you're supplying a custom threshold value.")

  }

  assertthat::assert_that(test_10(aggregated_soundscape@threshold))

    # 1.2.10. The output argument

  test_11 <- function(x){

    all(length(x) == 1 & is.character(x) & (x %in% c("incidence_freq", "raw")))

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available output options. Did you supply the aggregated_soundscape argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.")

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


  # 1.3. Check if supplied type argument is one of
  # available options

  test_19 <- function(x){
    assertthat::is.string(x)
  }

  test_20 <- function(x){
    x %in% c("regular", "polar")
  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the heatmap type as a character string. Consult package documentation for available type argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_20) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available heatmap type options. Please consult package documentation for available type argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_19(type))
  assertthat::assert_that(test_20(type))

  # 1.4. Check if supplied annotate argument is one of the
  # available options

  test_21 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a Boolean flag (TRUE or FALSE). Please set annotate argument to TRUE or FALSE. Make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_21(annotate))

  # 1.5. Check if supplied timeinterval argument is one of
  # the available options

  test_22 <- function(x){

    any(stringr::str_detect(timeinterval,c("sec", "secs", "min", "mins", "hour", "hours", "day", "days", "week", "weeks", "month", "months", "year", "years"))) &
      grepl("^[[:digit:]]\\s", timeinterval)

  }

  assertthat::on_failure(test_22) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.")

  }

  assertthat::assert_that(test_22(timeinterval))

  # 1.6. Check if supplied mintime and maxtime arguments
  # are one of the available options

  test_23 <- function(x){

    x == "default" |
      !is.na(as.POSIXct(x, format="%H:%M:%S"))

  }

  assertthat::on_failure(test_23) <- function(call, env){

    paste0(deparse(call$x), " is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.")

  }

  assertthat::assert_that(test_23(mintime))
  assertthat::assert_that(test_23(maxtime))

  # 1.7. Check if the freqinterval argument is in the right format.

  test_24 <- function(x){

    assertthat::is.count(x) &
      x > min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
      x < max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))

  }

  assertthat::on_failure(test_24) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).")

  }

  assertthat::assert_that(test_24(freqinterval))

  # 1.8. Check if the minfreq and maxfreq arguments follow
  # the expected values

  test_25 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
       x <= max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))) |
      x == 0

  }

  test_26 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregated_soundscape@aggregated_df))) &
       x <= max(as.numeric(rownames(aggregated_soundscape@aggregated_df)))) |
      x == "default"

  }

  assertthat::on_failure(test_25) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::on_failure(test_26) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  # assertthat::assert_that(test_25(minfreq))
  # assertthat::assert_that(test_26(maxfreq))

  # 1.9. Check if the nbins argument abides by the
  # expected format

  test_27 <- function(x){

    assertthat::is.count(x) &
      x > 0 &
      x < nrow(aggregated_soundscape@aggregated_df)
  }

  assertthat::on_failure(test_27) <- function(call, env){

    paste0(deparse(call$x), " is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the marginplot argument. If you wish to display a marginal plot, please set marginplot = TRUE.")

  }

  assertthat::assert_that(test_27(nbins))



  # 1.11. Check if the labelsize arguments follow the
  # expected format.

  test_29 <- function(x){

    (abs(x) == x) &
      x > 0 &
      length(x) == 1

  }

  assertthat::on_failure(test_29) <- function(call, env){

    paste0(deparse(call$x), " is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.")

  }

  assertthat::assert_that(test_29(labelsize_time))
  assertthat::assert_that(test_29(labelsize_frequency))
  assertthat::assert_that(test_29(labelsize_polar))

  # 1.12. Check if the palette and direction arguments
  # abide by the expected format.

  test_30 <- function(x){

    assertthat::is.string(x) &
      x %in% c("A", "B", "C", "D", "E", "magma", "inferno",
               "plasma", "viridis", "cividis")

  }

  test_31 <- function(x){

      x %in% c(1, -1)

  }

  assertthat::on_failure(test_30) <- function(call, env){

    paste0(deparse(call$x), " is not a valid palette argument. Palette needs to be supplied as a character string of either: 'A', 'B', 'C', 'D', 'E', 'magma', 'inferno', 'plasma', 'viridis' or 'cividis'. Please supply a valid palette argument. Consult the soundscapeR of viridis package documentation for more information.")

  }

  assertthat::on_failure(test_31) <- function(call, env){

    paste0(deparse(call$x), " is not a valid direction argument. The direction argument needs to be supplied as a single integer of either 1 or -1. Please supply a valid direction argument. Consult the soundscapeR of viridis package documentation for more information.")

  }

  assertthat::assert_that(test_30(palette))
  assertthat::assert_that(test_31(direction))

  # 1.13. Check if the boolean flag arguments follow the
  # expected format (zero.black, marginplot, interactive,
  # save)

  test_32 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_32) <- function(call, env){

    paste0(deparse(call$x), " is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_32(zero.black))
  assertthat::assert_that(test_32(marginplot))
  assertthat::assert_that(test_32(interactive))
  assertthat::assert_that(test_32(save))

  test_33 <- function(x){

    if (x == TRUE){

    type =="regular" &
      interactive ==FALSE

    } else{return(TRUE)}

  }

  assertthat::on_failure(test_33) <- function(call, env){

    paste0(deparse(call$x), " is used with other arguments which are not accepted. The marginplot=TRUE argument can not be used in synergy with type='polar' or interactive = TRUE.")

  }

  assertthat::assert_that(test_33(x = marginplot))

  # 1.14. Check if the n_time and the n_freq arguments
  # follow the expected format

  test_34 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_34) <- function(call, env){
    paste0(deparse(call$x), " does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.")

  }

  assertthat::assert_that(test_34(n_time))
  assertthat::assert_that(test_34(n_freq))

  # 1.15. Check if the dir, filename and device arguments
  # follow the expected format.

  if (dir=="default"){
    dir <- getwd()
  }

  else{dir <- dir}

  test_35 <- function(x){

    assertthat::is.string(x)

  }

  test_36 <- function(x){

    assertthat::is.dir(x)

  }

  assertthat::on_failure(test_35) <- function(call, env){
    paste0(deparse(call$x), " is not a character string. The dir arguments needs to be a character string of either 'default' - or a valid pathname to an existing directory on your device. If you're working on a Windows operating system, pay attention to backslash and forwardslash.")

  }

  assertthat::assert_that(test_35(dir))
  assertthat::assert_that(test_36(dir))

  test_37 <- function(x){

    assertthat::is.string(x)

  }

  test_38 <- function(x){

    !(sub('.*\\.', '', filename) %in% c("eps", "ps","tex", "pdf", "jpeg",
                                        "tiff","png", "bmp","svg", "wmf"))

  }

  assertthat::on_failure(test_37) <- function(call, env){
    paste0(deparse(call$x), " is not a valid filename argument. The filename argument needs to be a character string.")
  }

  assertthat::on_failure(test_38) <- function(call, env){
    paste0(deparse(call$x), " is not a valid filename argument. Please make the filename argument you provide a character string without the extension.")
  }

  assertthat::assert_that(test_37(filename))
  assertthat::assert_that(test_38(filename))

  test_39 <- function(x){

    assertthat::is.string(x) &
      x %in% c("eps", "ps","tex", "pdf", "jpeg", "tiff",
               "png", "bmp","svg", "wmf")

  }

  assertthat::on_failure(test_39) <- function(call, env){
    paste0(deparse(call$x), " is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.")
  }

  assertthat::assert_that(test_39(device))

  # 1.16. Check if the supplied height and width arguments
  # follow the expected format

  test_40 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_40) <- function(call, env){
    paste0(deparse(call$x), " is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.")
  }

  assertthat::assert_that(test_40(height))
  assertthat::assert_that(test_40(width))

  # 2. Prepare variables for plotting

  tz <- aggregated_soundscape@tz

  lengthen=function(aggregated_soundscape){

    tz <- aggregated_soundscape@tz
    df <- aggregated_soundscape@aggregated_df
    df$frequency <- as.integer(rownames(df))
    melt_df <- reshape2::melt(df, id.vars="frequency")
    colnames(melt_df) <- c("frequency", "time", "value")
    melt_df$frequency <- as.numeric(
      as.character(melt_df$frequency))
    melt_df$time <- as.POSIXct(
      strptime(
        x =paste(substr(aggregated_soundscape@first_day, 1, 12),
                 melt_df$time,
                 sep=" "),
        format = "%Y-%m-%d %H:%M",
        tz = tz))
    return(melt_df)

  }

  df2 <- lengthen(aggregated_soundscape = aggregated_soundscape)

  if(zero.black==TRUE){

    color_vector <- viridis::viridis(
      length(
        sort(
          unique(
            unlist(aggregated_soundscape@aggregated_df)))),
      direction = direction,
      option = palette)

    color_vector[1] <- "#000000"

  }

  else{

    color_vector <- viridis::viridis(
    length(
      sort(
        unique(
          unlist(aggregated_soundscape@aggregated_df)))),
    direction = direction,
    option = palette)

    }

  minfreq <- minfreq

  if(maxfreq=="default"){

    maxfreq <- max(as.numeric(df2$frequency))

  }

  else{maxfreq <- maxfreq}

  if (mintime=="default"){
    mintime <- min(
      as.POSIXct(
        strptime(
          paste(substr(aggregated_soundscape@first_day, 1, 12),
                colnames(aggregated_soundscape@aggregated_df),
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = tz)))
  }

  else{
    mintime <- as.POSIXct(
      strptime(
        paste(substr(aggregated_soundscape@first_day, 1, 12),
              mintime,
              sep=" "),
        format= "%Y-%m-%d %H:%M:%S",
        tz = tz))

  }

  if (maxtime=="default"){
    maxtime <- max(
      as.POSIXct(
        strptime(
          paste(substr(aggregated_soundscape@first_day, 1, 12),
                colnames(aggregated_soundscape@aggregated_df),
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = tz)))

  }

  else{
    maxtime <- as.POSIXct(
      strptime(
        paste(substr(aggregated_soundscape@first_day, 1, 12),
              maxtime,
              sep=" "),
        format= "%Y-%m-%d %H:%M:%S",
        tz = tz))
  }

  day <- as.POSIXct(
    strptime(
      paste(substr(aggregated_soundscape@first_day, 1, 12),
            "00:00:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz = tz))

  sunrise <- aggregated_soundscape@sunrise
  sunset <- aggregated_soundscape@sunset

  midnight1 <- as.POSIXct(
    strptime(
      paste(substr(aggregated_soundscape@first_day, 1, 12),
            "00:00:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz = tz))

  midnight2 <- as.POSIXct(
    strptime(
      paste(substr(aggregated_soundscape@first_day, 1, 12),
            "23:55:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz = tz))

  if (type=="regular"){

    if (annotate==TRUE){

      if (maxfreq>22000){

        plot <-
          ggplot2::ggplot(df2,
                          ggplot2::aes(time,
                                       frequency,
                                       fill=value,
                                       color=value)) +

          ggplot2::geom_tile()+

          ggplot2::scale_fill_gradientn(
            colors = color_vector,
            na.value = "grey45",
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=1,
              keywidth = grid::unit(5, "mm")),
            breaks=seq(0, 1, 0.1),
            limits = c(0, 1))+

          ggplot2::scale_color_gradientn(
            colors = color_vector,
            na.value = "grey45",
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=1,
              keywidth = grid::unit(5, "mm")),
            breaks=seq(0, 1, 0.1),
            limits = c(0, 1))+


          ggplot2::scale_x_datetime(
            labels=scales::date_format("%H:%M", tz=tz),
            breaks = scales::breaks_width(timeinterval),
            expand = c(0,0),
            limits = c(mintime,maxtime))+

          ggplot2::scale_y_continuous(
            limits = c(minfreq, (maxfreq+(maxfreq/10))),
            expand = c(0,0),
            breaks = seq(minfreq, maxfreq, freqinterval),
            label=scales::comma)+

          ggplot2::labs(
            y="Frequency (Hz)",
            x="Time (hour of day)")+

          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(
              colour = "black"),
            axis.text.y = ggplot2::element_text(
              color = "black",
              size = 10),
            axis.text.x = ggplot2::element_text(
              color = "black",
              size = 10,
              angle = -45,
              vjust = 1.2,
              hjust = -0.3),
            axis.title.y = ggplot2::element_text(margin = grid::unit(c(0, 3, 0, 0), "mm")),
            axis.title.x = ggplot2::element_text(margin = grid::unit(c(3, 0, 0, 0), "mm")),
            plot.margin = grid::unit(c(1,1,1,1),"cm"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_text(
              color = "black",
              size = 12,
              face = "bold"))+

            ggplot2::geom_vline(
              ggplot2::aes(xintercept = as.numeric(aggregated_soundscape@sunrise)),
                           linetype="dashed",
                           color= if (direction==1){paste("white")}
                           else{paste("black")})+

            ggplot2::geom_vline(
              ggplot2::aes(xintercept = as.numeric(aggregated_soundscape@sunset)),
              linetype="dashed",
              color= if (direction==1){paste("white")}
              else{paste("black")})+

          ggplot2::geom_hline(
            yintercept = 20000,
            linetype="dashed",
            color= if (direction==1){paste("white")}
            else{paste("black")})+

          ggplot2::annotate(
            geom="rect",
            xmin =min(df2$time),
            xmax=sunrise,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            color="white",
            alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunset,
            xmax=midnight2,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            color="white",
            alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunrise,
            xmax=sunset,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#ffcc13",
            color="white",
            alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =min(df2$time),
            xmax=sunrise,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            color="black",
            alpha=0.25)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunset,
            xmax=(midnight2-(60*5)),
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            color="black",
            alpha=0.25)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunrise,
            xmax=sunset,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#ffcc13",
            color="black",
            alpha=0.25)+

          ggplot2::annotate(
            geom="rect",
            xmin =midnight2,
            xmax=midnight2+3600,
            ymin=minfreq,
            ymax=20000,
            fill="white",
            color="white",
            alpha=0.5)+

          ggplot2::annotate(
            geom="rect",
            xmin =midnight2,
            xmax=midnight2+3600,
            ymin=20000,
            ymax=maxfreq,
            fill="white",
            color="white",
            alpha=0.5)+

          ggplot2::annotate(
            "text",
            x = (sunrise-(as.numeric(
              difftime(sunrise,
                       midnight1,
                       units = "secs"))/2)),
            y = (maxfreq+(maxfreq/20)),
            label = "NIGHTTIME",
            color="white",
            fontface=2,
            size=labelsize_time)+

          ggplot2::annotate(
            "text",
            x = (sunset-(as.numeric(
              difftime(sunset,
                       sunrise,
                       units = "secs"))/2)),
            y = (maxfreq+(maxfreq/20)),
            label = "DAYTIME",
            color="black",
            fontface=2,
            size=labelsize_time)+

          ggplot2::annotate(
            "text",
            x = (midnight2-(as.numeric(
              difftime(midnight2,
                       sunset,
                       units = "secs"))/2)),
            y = (maxfreq+(maxfreq/20)),
            label = "NIGHTTIME",
            color="white",
            fontface=2,
            size=labelsize_time)+

          ggplot2::geom_vline(xintercept = midnight2) +

          ggplot2::annotate(
            "text",
            x = midnight2+2200,
            y = 20000+((maxfreq-20000)/2),
            label = "ULTRASOUND",
            color="black",
            fontface=2,
            angle=-90,
            size=labelsize_frequency)+

          ggplot2::annotate(
            "text",
            x = midnight2+2200,
            y = (20000-minfreq)/2,
            label = "AUDIBLE",
            color="black",
            fontface=2,
            angle=-90,
            size=labelsize_frequency)+

          ggplot2::labs(fill="OSU RELATIVE ABUNDANCE") +

          ggplot2::guides(color='none')


      }

      else{

        if (maxfreq<=22000){

          plot <-

            ggplot2::ggplot(
              df2,
              ggplot2::aes(time,
                           frequency,
                           fill=value,
                           color=value)) +

            ggplot2::geom_tile()+

            ggplot2::scale_fill_gradientn(
              colors = color_vector,
              na.value = "grey45",
              guide = ggplot2::guide_legend(
                title.position = "top",
                title.vjust = 1,
                title.hjust = 0.5,
                nrow=1,
                keywidth = grid::unit(5, "mm")),
              breaks=seq(0, 1, 0.1),
              limits = c(0, 1))+

            ggplot2::scale_color_gradientn(
              colors = color_vector,
              na.value = "grey45",
              guide = ggplot2::guide_legend(
                title.position = "top",
                title.vjust = 1,
                title.hjust = 0.5,
                nrow=1,
                keywidth = grid::unit(5, "mm")),
              breaks=seq(0, 1, 0.1),
              limits = c(0, 1))+

            ggplot2::scale_x_datetime(
              labels=scales::date_format("%H:%M", tz=tz),
              breaks = scales::breaks_width(timeinterval),
              expand = c(0,0),
              limits = c(mintime,maxtime))+

            ggplot2::scale_y_continuous(
              limits = c(minfreq, (maxfreq+(maxfreq/10))),
              expand = c(0,0),
              breaks = seq(minfreq, maxfreq, freqinterval),
              label=scales::comma)+

            ggplot2::labs(
              y="Frequency (Hz)",
              x="Time (hour of day)")+

            ggplot2::theme(
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              axis.line = ggplot2::element_line(
                colour = "black"),
              axis.text.y = ggplot2::element_text(
                color = "black", size = 10),
              axis.text.x = ggplot2::element_text(
                color = "black",
                size = 10,
                angle = -45,
                vjust = 1.2,
                hjust = -0.3),
              axis.title.y = ggplot2::element_text(margin = grid::unit(c(0, 3, 0, 0), "mm")),
              axis.title.x = ggplot2::element_text(margin = grid::unit(c(3, 0, 0, 0), "mm")),
              plot.margin = grid::unit(c(1,1,1,1),"cm"),
              legend.position = "top",
              legend.direction = "horizontal",
              legend.title = ggplot2::element_text(
                color = "black",
                size = 12,
                face = "bold"))+

              ggplot2::geom_vline(
                ggplot2::aes(xintercept = as.numeric(aggregated_soundscape@sunrise)),
                linetype="dashed",
                color= if (direction==1){paste("white")}
                else{paste("black")})+

              ggplot2::geom_vline(
                ggplot2::aes(xintercept = as.numeric(aggregated_soundscape@sunset)),
                linetype="dashed",
                color= if (direction==1){paste("white")}
                else{paste("black")})+

            ggplot2::geom_hline(
              yintercept = 20000,
              linetype="dashed",
              color=if (direction==1){paste("white")}
              else{paste("black")})+

            ggplot2::annotate(
              geom="rect",
              xmin =min(df2$time),
              xmax=sunrise,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#4C4B69",
              color="white",
              alpha=1)+

            ggplot2::annotate(
              geom="rect",
              xmin =sunset,
              xmax=midnight2,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#4C4B69",
              color="white",
              alpha=1)+

            ggplot2::annotate(
              geom="rect",
              xmin =sunrise,
              xmax=sunset,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#ffcc13",
              color="white",
              alpha=1)+

            ggplot2::annotate(
              geom="rect",
              xmin =min(df2$time),
              xmax=sunrise,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#4C4B69",
              color="black",
              alpha=0.25)+

            ggplot2::annotate(
              geom="rect",
              xmin =sunset,
              xmax=midnight2,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#4C4B69",
              color="black",
              alpha=0.25)+

            ggplot2::annotate(
              geom="rect",
              xmin =sunrise,
              xmax=sunset,
              ymin=maxfreq,
              ymax=(maxfreq+(maxfreq/10)),
              fill="#ffcc13",
              color="black",
              alpha=0.25)+

            ggplot2::annotate(
              geom="rect",
              xmin =midnight2,
              xmax=midnight2+3600,
              ymin=minfreq,
              ymax=20000,
              fill="white",
              color="white",
              alpha=0.5)+

            ggplot2::annotate(
              geom="rect",
              xmin =midnight2,
              xmax=midnight2+3600,
              ymin=20000,
              ymax=maxfreq,
              fill="white",
              color="white",
              alpha=0.5)+

            ggplot2::annotate(
              "text",
              x = (sunrise-(as.numeric(
                difftime(sunrise,
                         midnight1,
                         units = "secs"))/2)),
              y = (maxfreq+(maxfreq/20)),
              label = "NIGHTTIME",
              color="white",
              fontface=2)+

            ggplot2::annotate(
              "text", x = (sunset-(as.numeric(
                difftime(sunset,
                         sunrise,
                         units = "secs"))/2)),
              y = (maxfreq+(maxfreq/20)),
              label = "DAYTIME",
              color="black",
              fontface=2)+

            ggplot2::annotate(
              "text",
              x = (midnight2-(as.numeric(
                difftime(midnight2,
                         sunset,
                         units = "secs"))/2)),
              y = (maxfreq+(maxfreq/20)),
              label = "NIGHTTIME",
              color="white",
              fontface=2)+

            ggplot2::geom_vline(xintercept = midnight2)+

            ggplot2::labs(fill="OSU RELATIVE ABUNDANCE")+

            ggplot2::guides(color='none')



        }
      }
    }

    else {

      if (annotate==FALSE){

        plot <-
          ggplot2::ggplot(
            df2, ggplot2::aes(time,
                              frequency,
                              fill=value,
                              color=value)) +

          ggplot2::geom_tile()+

          ggplot2::scale_fill_gradientn(
            colors = color_vector,
            na.value = "grey45",
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=1,
              keywidth = grid::unit(5, "mm")),
            breaks=seq(0, 1, 0.1),
            limits = c(0, 1))+

          ggplot2::scale_color_gradientn(
            colors = color_vector,
            na.value = "grey45",
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=1,
              keywidth = grid::unit(5, "mm")),
            breaks=seq(0, 1, 0.1),
            limits = c(0, 1))+

          ggplot2::labs(
            y="Frequency (Hz)",
            x="Time (hour of day)")+

          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(
              colour = "black"),
            axis.text.y = ggplot2::element_text(
              color = "black", size = 10),
            axis.text.x = ggplot2::element_text(
              color = "black",
              size = 10,
              angle = -45,
              vjust = 1.2,
              hjust = -0.3),
            axis.title.y = ggplot2::element_text(margin = grid::unit(c(0, 3, 0, 0), "mm")),
            axis.title.x = ggplot2::element_text(margin = grid::unit(c(3, 0, 0, 0), "mm")),
            panel.border = ggplot2::element_rect(
              colour = "black",
              fill=NA,
              size=0.5),
            plot.margin = grid::unit(c(1,1,1,1),"cm"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_text(
              color = "black",
              size = 12,
              face = "bold"))+

          ggplot2::labs(fill="OSU RELATIVE ABUNDANCE")+

          ggplot2::guides(color='none')+

          ggplot2::scale_x_datetime(
            labels=scales::date_format("%H:%M", tz=tz),
            breaks = scales::breaks_width(timeinterval),
            expand = c(0,0),
            limits = c(mintime,maxtime))+

          ggplot2::scale_y_continuous(
            limits = c(minfreq,maxfreq),
            expand = c(0,0),
            breaks = seq(minfreq, maxfreq, freqinterval),
            label=scales::comma)



      }
    }
  }

  else{

    if (type=="polar"){

      print("Please bear with us, making polar heatmaps takes a bit longer...")
      Sys.sleep(0.0001)

      if (annotate==TRUE){

        plot <-
          ggplot2::ggplot(
            df2, ggplot2::aes(time,
                              frequency,
                              fill=value,
                              color=value)) +

          ggplot2::geom_tile()+

          ggplot2::scale_fill_gradientn(
            colors = color_vector,
            na.value = "grey45",
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=1,
              keywidth = grid::unit(5, "mm")),
            breaks=seq(0, 1, 0.1),
            limits = c(0, 1))+

          ggplot2::scale_color_gradientn(
            colors = color_vector,
            na.value = "grey45",
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=1,
              keywidth = grid::unit(5, "mm")),
            breaks=seq(0, 1, 0.1),
            limits = c(0, 1))+

          ggplot2::scale_x_datetime(
            labels=scales::date_format("%H:%M", tz=tz),
            breaks = scales::breaks_width(timeinterval),
            expand = c(0,0),
            limits = c(mintime,maxtime))+

          ggplot2::scale_y_continuous(
            limits = c(minfreq,(maxfreq+(maxfreq/10))),
            expand = c(0,0),
            breaks = seq(minfreq, maxfreq, freqinterval),
            label=scales::comma)+

          ggplot2::labs(
            y="Frequency (Hz)",
            x="Time (hour of day)")+

          ggplot2::theme(
            panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(
              colour = "black"),
            axis.text.x = ggplot2::element_text(
              color = "black",
              size = 10,
              angle = -0,
              hjust=1.1,
              vjust=1),
            axis.title.y = ggplot2::element_text(margin = grid::unit(c(0, 3, 0, 0), "mm")),
            axis.title.x = ggplot2::element_text(margin = grid::unit(c(3, 0, 0, 0), "mm")),
            panel.border = ggplot2::element_rect(
              colour = "white",
              fill=NA,
              size=0.5),
            plot.margin = grid::unit(c(1,1,1,1),"cm"),
            axis.text.y = ggplot2::element_blank(),
            axis.ticks.y = ggplot2::element_blank(),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_text(
              color = "black",
              size = 12,
              face = "bold"))+

          ggplot2::annotate(
            geom="segment",
            x=sunrise,
            xend=sunrise,
            y=minfreq,
            yend=maxfreq,
            color= if (direction==1){paste("white")}
            else{paste("black")})+

          ggplot2::annotate(
            geom="segment",
            x=sunset,
            xend=sunset,
            y=minfreq,
            yend=maxfreq,
            color= if (direction==1){paste("white")}
            else{paste("black")})+

          ggplot2::annotate(
            geom="segment",
            x=seq.POSIXt(from=min(df2$time),
                         to=max(df2$time),
                         by=3600),
            xend=seq.POSIXt(from=min(df2$time),
                            to=max(df2$time),
                            by=3600),
            y=minfreq,
            yend=maxfreq,
            color="#383e42",
            alpha=0.5,
            size=0.2)+

          ggplot2::coord_polar()+

          ggplot2::geom_hline(
            yintercept = seq(minfreq,
                             maxfreq,
                             freqinterval),
            color="#383e42",
            alpha=0.5,
            size=0.2)+

          ggplot2::annotate(
            geom="rect",
            xmin =min(df2$time),
            xmax=sunrise,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69", alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunset,
            xmax=midnight2,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunrise,
            xmax=sunset,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#ffcc13",
            alpha=1)+

          ggplot2::annotate(
            geom="rect",
            xmin =min(df2$time),
            xmax=sunrise,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69",
            alpha=0.25)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunset,
            xmax=midnight2,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#4C4B69", alpha=0.25)+

          ggplot2::annotate(
            geom="rect",
            xmin =sunrise,
            xmax=sunset,
            ymin=maxfreq,
            ymax=(maxfreq+(maxfreq/10)),
            fill="#ffcc13",
            alpha=0.25)+

          ggplot2::labs(fill="OSU RELATIVE ABUNDANCE")+

          ggplot2::guides(color='none')+

          ggplot2::annotate(
            geom="label",
            size=labelsize_polar,
            y=seq(minfreq,
                  maxfreq,
                  freqinterval),
            x=min(df2$time),
            label=as.character(seq(minfreq,
                                   maxfreq,
                                   freqinterval)))



      }

      else{

        print("Please bear with us, making polar heatmaps takes a bit longer...")
        Sys.sleep(0.0001)

        if (annotate==FALSE){

          plot <-
            ggplot2::ggplot(
              df2,
              ggplot2::aes(time,
                           frequency,
                           fill=value,
                           color=value)) +

            ggplot2::geom_tile()+

            ggplot2::scale_fill_gradientn(
              colors = color_vector,
              na.value = "grey45",
              guide = ggplot2::guide_legend(
                title.position = "top",
                title.vjust = 1,
                title.hjust = 0.5,
                nrow=1,
                keywidth = grid::unit(5, "mm")),
              breaks=seq(0, 1, 0.1),
              limits = c(0, 1))+

            ggplot2::scale_color_gradientn(
              colors = color_vector,
              na.value = "grey45",
              guide = ggplot2::guide_legend(
                title.position = "top",
                title.vjust = 1,
                title.hjust = 0.5,
                nrow=1,
                keywidth = grid::unit(5, "mm")),
              breaks=seq(0, 1, 0.1),
              limits = c(0, 1))+

            ggplot2::scale_x_datetime(
              labels=scales::date_format("%H:%M", tz=tz),
              breaks = scales::breaks_width(timeinterval),
              expand = c(0,0),
              limits = c(mintime,maxtime))+

            ggplot2::scale_y_continuous(
              limits = c(minfreq,maxfreq),
              expand = c(0,0),
              breaks = seq(minfreq,
                           maxfreq,
                           freqinterval),
              label=scales::unit_format(unit = "K"))+

            ggplot2::labs(
              y="Frequency (Hz)",
              x="Time (hour of day)")+

            ggplot2::theme(
              panel.grid.major = ggplot2::element_blank(),
              panel.grid.minor = ggplot2::element_blank(),
              panel.background = ggplot2::element_blank(),
              axis.line = ggplot2::element_line(
                colour = "black"),
              axis.text.x = ggplot2::element_text(
                color = "black",
                size = 10,
                angle = -0,
                hjust=1.1,
                vjust=1),
              axis.title.y = ggplot2::element_text(margin = grid::unit(c(0, 3, 0, 0), "mm")),
              axis.title.x = ggplot2::element_text(margin = grid::unit(c(3, 0, 0, 0), "mm")),
              panel.border = ggplot2::element_rect(
                colour = "white",
                fill=NA,
                size=0.5),
              axis.text.y = ggplot2::element_blank(),
              axis.ticks.y = ggplot2::element_blank(),
              plot.margin = grid::unit(c(1,1,1,1),"cm"),
              legend.position = "top",
              legend.direction = "horizontal",
              legend.title = ggplot2::element_text(
                color = "black",
                size = 12,
                face = "bold"))+

            ggplot2::coord_polar()+

            ggplot2::geom_hline(
              yintercept = seq(minfreq,
                               maxfreq,
                               freqinterval),
              color="#383e42",
              alpha=0.5,
              size=0.2)+

            ggplot2::annotate(
              geom="label",
              size=labelsize_polar,
              y=seq(minfreq,
                    maxfreq,
                    freqinterval),
              x=min(df2$time),
              label=as.character(seq(minfreq,
                                     maxfreq,
                                     freqinterval)))+

            ggplot2::labs(fill="OSU RELATIVE ABUNDANCE")+

            ggplot2::guides(color='none')

          plot

        }
      }
    }
  }

  if (marginplot==FALSE & interactive==FALSE & save==FALSE){
    plot
  }

  else{

    if (marginplot==FALSE & interactive==FALSE & save==TRUE){

      ggplot2::ggsave(
        filename=paste0(
          paste0(type, "_"),
          "no_margin_",
          filename,
          ".",
          device),
        plot=plot,
        device=device,
        path=dir,
        dpi = "retina",
        width = width,
        height = height,
        units = c("mm"))

      plot
    }

    else{

      if (marginplot==FALSE & interactive==TRUE & save==FALSE){
        plotly::ggplotly(plot)
      }

      else{

        if (marginplot==FALSE & interactive==TRUE & save==TRUE){

          ggplot2::ggsave(
            filename=paste0(
              paste0(
                type,
                "_"),
              "no_margin_",
              filename,
              ".",
              device),
            plot=plot,
            device=device,
            path=dir,
            dpi = "retina",
            width=width,
            height = height,
            units = c("mm"))

          plotly::ggplotly(plot)
        }

        else{

              if (marginplot==TRUE & type=="regular" & interactive==FALSE){

                heatmap <-
                  plot+ggplot2::theme(
                  plot.margin = grid::unit(
                    c(0.15, 0.15, 0, 0), "cm"),
                  legend.position = "none")

                xdata <- sounddiv(aggregated_soundscape = aggregated_soundscape,
                                  qvalue = 0,
                                  subset = "tod",
                                  minfreq = minfreq,
                                  maxfreq = maxfreq)

                colnames(xdata) <- c("richness", "time")

                xdata$richness_smooth=pracma::movavg(
                  xdata$richness,
                  n=n_time,
                  type="t")

                xplot <-
                  ggplot2::ggplot(
                  xdata,
                  ggplot2::aes(time,
                               richness_smooth))+

                  ggplot2::geom_area(alpha=0.25,
                                     fill="#440154FF")+

                  ggplot2::geom_line(color="black",
                                     size=1.2)+

                  ggplot2::ylab("Richness (%)")+

                  ggplot2::xlab(NULL)+

                  ggplot2::scale_x_time(expand = c(0,0))+

                  ggplot2::scale_y_continuous(
                    expand = c(0,0),
                    label=scales::comma)+

                  ggplot2::theme(
                    axis.title.x = ggplot2::element_blank(),
                    axis.ticks.x = ggplot2::element_blank(),
                    axis.text.x = ggplot2::element_blank(),
                    panel.border = ggplot2::element_blank(),
                    panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.background = ggplot2::element_blank(),
                    plot.margin = grid::unit(c(0, 0, 0, 0),
                                             "cm"))

                ydata <-
                  as.data.frame(
                    sounddiv(
                      aggregated_soundscape = aggregated_soundscape,
                      qvalue = 0,
                      subset = "total",
                      freqseq = TRUE,
                      nbins = nbins,
                      minfreq = minfreq,
                      maxfreq = maxfreq)[,1])

                ydata$frequency <- seq(from = minfreq,
                                       to = maxfreq,
                                       by = maxfreq/(nrow(ydata)-1))

                colnames(ydata) <- c("richness",
                                     "frequency")

                ydata$richness_smooth <- pracma::movavg(
                  ydata$richness,
                  n=n_freq,
                  type="s")

                yplot <-
                  ggplot2::ggplot(
                  ydata,
                  ggplot2::aes(frequency,
                               richness_smooth))+

                  ggplot2::geom_area(alpha=0.40,
                                     fill="#FDE725FF")+

                  ggplot2::geom_line(color="black",
                                     size=1.5)+

                  ggplot2::ylab("Richness (%)")+

                  ggplot2::xlab(NULL)+

                  ggplot2::scale_x_continuous(
                    expand = c(0,0),
                    limits = c(0,(maxfreq+(maxfreq/10))))+

                  ggplot2::scale_y_continuous(
                    expand = c(0,0),
                    label=scales::comma)+

                  ggplot2::theme(
                    axis.title.y = ggplot2::element_blank(),
                    axis.ticks.y = ggplot2::element_blank(),
                    axis.text.y = ggplot2::element_blank(),
                    panel.border = ggplot2::element_blank(),
                    panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.background = ggplot2::element_blank(),
                    plot.margin = grid::unit(c(0, 0, 0, -0),
                                             "cm"))+

                  ggplot2::coord_flip()

                combined_plot <-
                  xplot +
                  patchwork::plot_spacer() +
                  heatmap +
                  yplot +
                  patchwork::plot_layout(widths = c(3, 1),
                                         heights = c(1,3))

                if (save==TRUE){
                  ggplot2::ggsave(
                    filename=paste0(
                      paste0(type,
                             "_"),
                      "marginplot_",
                      filename,
                      ".",
                      device),
                    plot=combined_plot,
                    device=device,
                    path=dir,
                    dpi = "retina",
                    width=width,
                    height = height,
                    units = c("mm"))

                  combined_plot
                }

                else{

                  if (save==FALSE){
                    combined_plot
                  }
                }
              }
        }
      }
    }
  }

}



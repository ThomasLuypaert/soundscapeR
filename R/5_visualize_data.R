# Functions for visualizing data

# Preparing dataframe for visualization. Function for internal use.

#' Reformats the dataframe for subsequent visualization
#'
#' @description Lengthens the dataframe so it can be
#' used for subsequent visualization using ggplot2.
#'  Function intended for internal use by
#'   \code{\link{heatmapper}}, however can also be
#'   used manually by user.
#'
#' @param df The aggregated time-frequency dataframe
#'  produced by \code{\link{aggregate_df}}.
#'
#' @param date The first day of the recording period.
#' Used for managing time-objects in R.
#' Format as "YYYY-mm-dd".
#'
#' @param lat The latitude of the site at which the
#' sound files were collected.
#'
#' @param lon The longitude of the site at which the
#' sound files were collected.
#'
#' @return Returns a long dataframe with "frequency",
#'  "time" and "value" columns.
#'
#' @export
#'
lengthen=function(df, date, lat, lon){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(df))
  assertthat::assert_that(test_0(date))
  assertthat::assert_that(test_0(lat))
  assertthat::assert_that(test_0(lon))

  # 1. Check if input variable in the right format

  #1.1. The supplied dataframe is a dataframe, is not
  # empty, and does not contain NAs

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

    paste0(deparse(call$x), " is not a data frame. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe (third element in the list) produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is an empty dataframe. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe (third element in the list) produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " contains NA values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe (third element in the list) produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " contains non-numeric values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe (third element in the list) produced by the aggregate_df() function.")

  }

  assertthat::assert_that(test_1(df))
  assertthat::assert_that(test_2(df))
  assertthat::assert_that(test_3(df))
  assertthat::assert_that(test_4(df))

  # 1.2. Test that supplied date is in the right format

  test_5 <- function(x) {
    assertthat::is.string(x)
  }

  test_6 <- function(x) {
    formatted = try(as.Date(x, "%Y-%m-%d"), silent = TRUE)
    is_date = as.character(formatted) == x & !is.na(formatted)
    return(is_date)
  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.")

  }

  assertthat::assert_that(test_5(date))
  assertthat::assert_that(test_6(date))

  # 1.3. Test that lat and lon are in decimal degrees
  # and exist on Earth

  test_7 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_8 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }


  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_7(lat))
  assertthat::assert_that(test_8(lon))

  # 1.4. Check if the df argument has correct
  # row and column names

  test_9 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) > 0 &
      max(as.numeric(rownames(x)))<180000

  }

  test_10 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(date," ", colnames(x)),
        tz=lutz::tz_lookup_coords(lat=lat,
                                  lon=lon,
                                  method = "accurate"),
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    is_time <-  all(

      all(
        substr(as.character(formatted),
               12,
               nchar(as.character(formatted))) == colnames(x)
      )

      & !any(is.na(formatted))

    )

    return(is_time)

  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.")

  }

  assertthat::assert_that(test_9(df))
  assertthat::assert_that(test_10(df))

  # 2. Look up the time-zone for the lat/long combination

  tz <- lutz::tz_lookup_coords(lat=lat,
                               lon=lon,
                               method="accurate")

  # 2.1. Test if time zone exists

  # Timezone

  test_11 <- function(x) {

    return(x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the correct latitude and longitude in decimal degrees?")

  }

  assertthat::assert_that(test_11(tz))

  # 3. Add a column with frequencies from rownames

  df$frequency <- as.integer(rownames(df))

  # 4. Melt the data frame using the frequency column

  melt_df <- reshape2::melt(df, id.vars="frequency")

  colnames(melt_df) <- c("frequency", "time", "value")

  melt_df$frequency <- as.numeric(
    as.character(melt_df$frequency))

  melt_df$time <- as.POSIXct(
    strptime(
      paste(date, melt_df$time, sep=" "),
      format= "%Y-%m-%d %H:%M",
      tz=tz))

  # 5. Return the melted data frame

  return(melt_df)

}

#' Flexible Soundscape Heatmaps
#'
#' @description  Creates a soundscape heatmap to visualize
#'  the use of acoustic space in the time-frequency domain
#'   for an set acoustic index. The function is highly
#'   flexible, allowing the user maximal control over every
#'    aspect of the heatmap. Please consult the arguments
#'     section to find out more about visualization options.
#'
#' @param aggregate_list The list produced by the \code{\link{aggregate_df}} function, containing the 'aggregated_per_time', 'sampling_effort_per_time' and 'aggregated_df' elements.
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
#' @param date The first day of the recording period. Used
#'  for managing time-objects in R.
#'  Formatted as "YYYY-mm-dd".
#'
#' @param lat The latitude of the site at which the sound
#'  files were collected.
#'
#' @param lon The longitude of the site at which the sound
#'  files were collected.
#'
#' @param twilight A character string of the twilight
#' method to be used for sunrise and sunset annotation.
#' Options can be found in the
#'  \code{\link[photobiology]{day_night}} documentation.
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

heatmapper=function(aggregate_list,
                    type="regular",
                    annotate=FALSE,
                    date,
                    lat,
                    lon,
                    timeinterval="1 hour",
                    mintime="default",
                    maxtime="default",
                    freqinterval=1000,
                    minfreq=0,
                    maxfreq="default",
                    nbins=10,
                    twilight="sunlight",
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

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(aggregate_list))
  assertthat::assert_that(test_0(date))
  assertthat::assert_that(test_0(lat))
  assertthat::assert_that(test_0(lon))

  # 1. Check if function input meets expectations

  # 1.1. The supplied aggregate_list is a list, is not
  # empty, and does not contain NAs

  test_1 <- function(x){

    all(is.list(x) & length(x) == 3)

  }

  test_1_1 <- function(x){

    is.list(x)

  }

  test_2_1 <- function(x){

    !any(sapply(x, function(x) is.null(x)))

  }

  test_2_2 <- function(x){

    assertthat::not_empty(x)

  }

  test_3 <- function(x){

    assertthat::noNA(x)

  }

  test_4 <- function(x){

    limma::isNumeric(x) & is.data.frame(x)

  }


  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not a list of the correct length. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_1_1) <- function(call, env){

    paste0(deparse(call$x), " is not a list. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_2_1) <- function(call, env){

    paste0(deparse(call$x), " is an empty list. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_2_2) <- function(call, env){

    paste0(deparse(call$x), " is an empty data frame. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " contains NA values. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.")

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " contains non-numeric values. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.")

  }

  assertthat::assert_that(test_1(aggregate_list))
  assertthat::assert_that(test_1_1(aggregate_list[[1]]))
  assertthat::assert_that(test_1_1(aggregate_list[[2]]))
  assertthat::assert_that(test_2_1(aggregate_list[[1]]))
  assertthat::assert_that(test_2_1(aggregate_list[[2]]))
  assertthat::assert_that(test_2_2(aggregate_list[[3]]))
  assertthat::assert_that(test_3(aggregate_list[[1]]))
  assertthat::assert_that(test_3(aggregate_list[[2]]))
  assertthat::assert_that(test_3(aggregate_list[[3]]))
  assertthat::assert_that(test_4(aggregate_list[[3]]))

  # 1.2. Check if supplied type argument is one of
  # available options

  test_5 <- function(x){
    assertthat::is.string(x)
  }

  test_6 <- function(x){
    x %in% c("regular", "polar")
  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the heatmap type as a character string. Consult package documentation for available type argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available heatmap type options. Please consult package documentation for available type argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_5(type))
  assertthat::assert_that(test_6(type))

  # 1.3. Check if supplied annotate argument is one of the
  # available options

  test_7 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a Boolean flag (TRUE or FALSE). Please set annotate argument to TRUE or FALSE. Make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_7(annotate))

  # 1.4. Check if supplied timeinterval argument is one of
  # the available options

  test_8 <- function(x){

    any(stringr::str_detect(timeinterval,c("sec", "secs", "min", "mins", "hour", "hours", "day", "days", "week", "weeks", "month", "months", "year", "years"))) &
      grepl("^[[:digit:]]\\s", timeinterval)

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.")

  }

  assertthat::assert_that(test_8(timeinterval))

  # 1.5. Check if supplied mintime and maxtime arguments
  # are one of the available options

  test_9 <- function(x){

    x == "default" |
      !is.na(as.POSIXct(x, format="%H:%M:%S"))

  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.")

  }

  assertthat::assert_that(test_9(mintime))
  assertthat::assert_that(test_9(maxtime))

  # 1.6. Check if the freqinterval argument is in the right format.

  test_10 <- function(x){

    assertthat::is.count(x) &
      x > min(as.numeric(rownames(aggregate_list[[3]]))) &
      x < max(as.numeric(rownames(aggregate_list[[3]])))

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).")

  }

  assertthat::assert_that(test_10(freqinterval))

  # 1.7. Check if the minfreq and maxfreq arguments follow
  # the expected values

  test_11 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregate_list[[3]]))) &
       x <= max(as.numeric(rownames(aggregate_list[[3]])))) |
      x == 0

  }

  test_12 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregate_list[[3]]))) &
       x <= max(as.numeric(rownames(aggregate_list[[3]])))) |
      x == "default"

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.")

  }

  assertthat::assert_that(test_11(minfreq))
  assertthat::assert_that(test_12(maxfreq))

  # 1.8. Check if the nbins argument abides by the
  # expected format

  test_13 <- function(x){

    assertthat::is.count(x) &
      x > 0 &
      x < nrow(aggregate_list[[3]])
  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the marginplot argument. If you wish to display a marginal plot, please set marginplot = TRUE.")

  }

  assertthat::assert_that(test_13(nbins))

  # 1.9. Test that supplied date is in the right format

  test_14_1 <- function(x) {
    assertthat::is.string(x)
  }

  test_14_2 <- function(x) {
    formatted = try(as.Date(x, "%Y-%m-%d"), silent = TRUE)
    is_date = as.character(formatted) == x & !is.na(formatted)
    return(is_date)
  }

  assertthat::on_failure(test_14_1) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.")

  }

  assertthat::on_failure(test_14_2) <- function(call, env){

    paste0(deparse(call$x), " is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.")

  }

  assertthat::assert_that(test_14_1(date))
  assertthat::assert_that(test_14_2(date))

  # 1.10. Test that lat and lon are in decimal degrees and
  # exist on Earth

  test_15_1 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_15_2 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_15_1) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_15_2) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_15_1(lat))
  assertthat::assert_that(test_15_2(lon))

  # 1.11. Check if the supplied twilight argument is one
  # of the available options

  test_16 <- function(x){

    (assertthat::is.string(x) &
       x %in% c("none","rim","refraction","sunlight","civil",
                "nautical","astronomical")) |
      (is.vector(x, mode="double") & (length(x) == 1 | length(x) ==2))

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.")

  }

  assertthat::assert_that(test_16(twilight))

  # 1.12. Check if the labelsize arguments follow the
  # expected format.

  test_17 <- function(x){

    (abs(x) == x) &
      x > 0 &
      length(x) == 1

  }

  assertthat::on_failure(test_17) <- function(call, env){

    paste0(deparse(call$x), " is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.")

  }

  assertthat::assert_that(test_17(labelsize_time))
  assertthat::assert_that(test_17(labelsize_frequency))
  assertthat::assert_that(test_17(labelsize_polar))

  # 1.13. Check if the palette and direction arguments
  # abide by the expected format.

  test_18 <- function(x){

    assertthat::is.string(x) &
      x %in% c("A", "B", "C", "D", "E", "magma", "inferno",
               "plasma", "viridis", "cividis")

  }

  test_19 <- function(x){

      x %in% c(1, -1)

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " is not a valid palette argument. Palette needs to be supplied as a character string of either: 'A', 'B', 'C', 'D', 'E', 'magma', 'inferno', 'plasma', 'viridis' or 'cividis'. Please supply a valid palette argument. Consult the soundscapeR of viridis package documentation for more information.")

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " is not a valid direction argument. The direction argument needs to be supplied as a single integer of either 1 or -1. Please supply a valid direction argument. Consult the soundscapeR of viridis package documentation for more information.")

  }

  assertthat::assert_that(test_18(palette))
  assertthat::assert_that(test_19(direction))

  # 1.14. Check if the boolean flag arguments follow the
  # expected format (zero.black, marginplot, interactive,
  # save)

  test_20 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_20) <- function(call, env){

    paste0(deparse(call$x), " is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_20(zero.black))
  assertthat::assert_that(test_20(marginplot))
  assertthat::assert_that(test_20(interactive))
  assertthat::assert_that(test_20(save))

  test_21 <- function(x){

    if (x == TRUE){

    type =="regular" &
      interactive ==FALSE

    } else{return(TRUE)}

  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is used with other arguments which are not accepted. The marginplot=TRUE argument can not be used in synergy with type='polar' or interactive = TRUE.")

  }

  assertthat::assert_that(test_21(x = marginplot))

  # 1.15. Check if the n_time and the n_freq arguments
  # follow the expected format

  test_23 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_23) <- function(call, env){
    paste0(deparse(call$x), " does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.")

  }

  assertthat::assert_that(test_23(n_time))
  assertthat::assert_that(test_23(n_freq))

  # 1.16. Check if the dir, filename and device arguments
  # follow the expected format.

  if (dir=="default"){
    dir <- getwd()

  }

  else{dir <- dir}

  test_24_1 <- function(x){

    assertthat::is.string(x)

  }

  test_24_2 <- function(x){

    assertthat::is.dir(x)

  }

  assertthat::on_failure(test_24_1) <- function(call, env){
    paste0(deparse(call$x), " is not a character string. The dir arguments needs to be a character string of either 'default' - or a valid pathname to an existing directory on your device. If you're working on a Windows operating system, pay attention to backslash and forwardslash.")

  }

  assertthat::assert_that(test_24_1(dir))
  assertthat::assert_that(test_24_2(dir))

  test_25_1 <- function(x){

    assertthat::is.string(x)

  }

  test_25_2 <- function(x){

    !(sub('.*\\.', '', filename) %in% c("eps", "ps","tex", "pdf", "jpeg",
                                        "tiff","png", "bmp","svg", "wmf"))

  }

  assertthat::on_failure(test_25_1) <- function(call, env){
    paste0(deparse(call$x), " is not a valid filename argument. The filename argument needs to be a character string.")
  }

  assertthat::on_failure(test_25_2) <- function(call, env){
    paste0(deparse(call$x), " is not a valid filename argument. Please make the filename argument you provide a character string without the extension.")
  }

  assertthat::assert_that(test_25_1(filename))
  assertthat::assert_that(test_25_2(filename))

  test_26 <- function(x){

    assertthat::is.string(x) &
      x %in% c("eps", "ps","tex", "pdf", "jpeg", "tiff",
               "png", "bmp","svg", "wmf")

  }

  assertthat::on_failure(test_26) <- function(call, env){
    paste0(deparse(call$x), " is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.")
  }

  assertthat::assert_that(test_26(device))

  # 1.17. Check if the supplied height and width arguments
  # follow the expected format

  test_27 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_27) <- function(call, env){
    paste0(deparse(call$x), " is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.")
  }

  assertthat::assert_that(test_27(height))
  assertthat::assert_that(test_27(width))


  # 2. Prepare variables for plotting

  tz <- lutz::tz_lookup_coords(lat=lat,
                               lon=lon,
                               method="accurate")

  test_28 <- function(x) {

    return(x %in% (OlsonNames()))

  }

  assertthat::assert_that(test_28(tz))

  df2 <- lengthen(aggregate_list[[3]],
                  paste0(date),
                  lat = lat,
                  lon = lon)

  test_29 <- function(x){

    is.data.frame(x)

  }

  test_30 <- function(x){

    assertthat::not_empty(x)

  }

  test_31 <- function(x){

    assertthat::noNA(x)

  }

  test_32 <- function(x, y){

    nrow(x) == (nrow(y)*ncol(y))

  }

  assertthat::assert_that(test_29(df2))
  assertthat::assert_that(test_30(df2))
  assertthat::assert_that(test_31(df2))
  assertthat::assert_that(test_32(x = df2,
                                  y = aggregate_list[[3]]))

  if(zero.black==TRUE){
    df2[df2==0] <- NA

    test_33 <- function(x){

      is.na(any(x==0))

    }

    assertthat::assert_that(test_33(df2))

  }

  else{}

  if (mintime=="default"){
    mintime <- min(
      as.POSIXct(
        strptime(
          paste(date,
                colnames(aggregate_list[[3]]),
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz=tz)))
  }

  else{
    mintime <- as.POSIXct(
      strptime(
        paste(date,
              mintime,
              sep=" "),
        format= "%Y-%m-%d %H:%M:%S",
        tz=tz))

  }

  test_34 <- function(x){

    !is.na(as.POSIXct(x, format="%Y-%m-%d %H:%M:%S"))

  }

  assertthat::assert_that(test_34(mintime))

  if (maxtime=="default"){
    maxtime <- max(
      as.POSIXct(
        strptime(
          paste(date,
                colnames(aggregate_list[[3]]),
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz=tz)))

  }

  else{
    maxtime <- as.POSIXct(
      strptime(
        paste(date,
              maxtime,
              sep=" "),
        format= "%Y-%m-%d %H:%M:%S",
        tz=tz))
  }

  assertthat::assert_that(test_34(maxtime))

  if (maxfreq=="default"){
    maxfreq <- max(df2$frequency)
  }

  else{maxfreq <- maxfreq}

  day <- as.POSIXct(
    strptime(
      paste(date,
            "00:00:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))

  test_38 <- function(x){

    !is.na(as.POSIXct(x, format="%Y-%m-%d %H:%M:%S"))

  }

  assertthat::assert_that(test_38(day))

  points <- data.frame(lon = lon, lat = lat)

  assertthat::assert_that(test_29(points))
  assertthat::assert_that(test_30(points))
  assertthat::assert_that(test_31(points))

  suntimes <- photobiology::day_night(
    date = date,
    tz= tz,
    geocode = points,
    twilight = twilight,
    unit.out = "datetime")

  assertthat::assert_that(test_29(suntimes))
  assertthat::assert_that(test_30(suntimes))

  sunrise <- as.POSIXct(suntimes$sunrise,
                        tz = tz,
                        format = "%Y-%m-%d %H:%M:%S")
  sunset <- as.POSIXct(suntimes$sunset,
                       tz = tz,
                       format = "%Y-%m-%d %H:%M:%S")

  assertthat::assert_that(test_31(sunrise))
  assertthat::assert_that(test_31(sunset))

  midnight1 <- as.POSIXct(
    strptime(
      paste(date,
            "00:00:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))

  assertthat::assert_that(test_38(midnight1))

  midnight2 <- as.POSIXct(
    strptime(
      paste(date,
            "23:50:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))

  assertthat::assert_that(test_38(midnight2))

  if (type=="regular"){

    if (annotate==TRUE){

      if (maxfreq>22000){

        plot <-
          ggplot2::ggplot(df2,
                          ggplot2::aes(time,
                                       frequency,
                                       fill=value)) +

          ggplot2::geom_tile()+
          viridis::scale_fill_viridis(
            option=palette,
            na.value="black",
            direction = direction,
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=1),
            breaks=seq(0, 1, 0.1))+

          ggplot2::scale_x_datetime(
            labels=scales::date_format("%H:%M", tz=tz),
            breaks = scales::breaks_width(timeinterval),
            expand = c(0,0),
            limits = c(mintime,maxtime))+

          ggplot2::scale_y_continuous(
            limits = c(minfreq, (maxfreq+(maxfreq/10))),
            expand = c(0,0),
            breaks = seq(minfreq, maxfreq, freqinterval))+

          ggplot2::labs(
            y="Frequency (Hz) \n",
            x="\nTime (hour of day)")+

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
              angle = 45,
              hjust=1.1,
              vjust=1),
            axis.title.y = ggplot2::element_text(),
            axis.title.x = ggplot2::element_text(),
            plot.margin = grid::unit(c(1,1,1,1),"cm"),
            legend.position = "top",
            legend.direction = "horizontal",
            legend.title = ggplot2::element_text(
              color = "black",
              size = 12,
              face = "bold"))+

            ggplot2::geom_vline(
              ggplot2::aes(xintercept = as.numeric(suntimes$sunrise)),
                           linetype="dashed",
                           color= if (direction==1){paste("white")}
                           else{paste("black")})+

            ggplot2::geom_vline(
              ggplot2::aes(xintercept = as.numeric(suntimes$sunset)),
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

          ggplot2::labs(fill="OSU INCIDENCE")
      }

      else{

        if (maxfreq<=22000){

          plot <-

            ggplot2::ggplot(
              df2,
              ggplot2::aes(time,
                           frequency,
                           fill=value)) +

            ggplot2::geom_tile()+

            viridis::scale_fill_viridis(
              option=palette,
              na.value="black",
              direction = direction,
              guide = ggplot2::guide_legend(
                title.position = "top",
                title.vjust = 1,
                title.hjust = 0.5,
                nrow=1),
              breaks=seq(0, 1, 0.1))+

            ggplot2::scale_x_datetime(
              labels=scales::date_format("%H:%M", tz=tz),
              breaks = scales::breaks_width(timeinterval),
              expand = c(0,0),
              limits = c(mintime,maxtime))+

            ggplot2::scale_y_continuous(
              limits = c(minfreq, (maxfreq+(maxfreq/10))),
              expand = c(0,0),
              breaks = seq(minfreq, maxfreq, freqinterval))+

            ggplot2::labs(
              y="Frequency (Hz) \n",
              x="\nTime (hour of day)")+

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
                angle = 45,
                hjust=1.1,
                vjust=1),
              axis.title.y = ggplot2::element_text(),
              axis.title.x = ggplot2::element_text(),
              plot.margin = grid::unit(c(1,1,1,1),"cm"),
              legend.position = "top",
              legend.direction = "horizontal",
              legend.title = ggplot2::element_text(
                color = "black",
                size = 12,
                face = "bold"))+

              ggplot2::geom_vline(
                ggplot2::aes(xintercept = as.numeric(suntimes$sunrise)),
                linetype="dashed",
                color= if (direction==1){paste("white")}
                else{paste("black")})+

              ggplot2::geom_vline(
                ggplot2::aes(xintercept = as.numeric(suntimes$sunset)),
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

            ggplot2::labs(fill="OSU INCIDENCE")
        }
      }
    }

    else {

      if (annotate==FALSE){

        plot <-
          ggplot2::ggplot(
            df2, ggplot2::aes(time,
                              frequency,
                              fill=value)) +

          ggplot2::geom_tile()+

          viridis::scale_fill_viridis(
            option=palette,
            na.value="black",
            direction = direction,
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=1),
            breaks=seq(0, 1, 0.1))+

          ggplot2::scale_x_datetime(
            labels=scales::date_format("%H:%M", tz=tz),
            breaks = scales::breaks_width(timeinterval),
            expand = c(0,0),
            limits = c(mintime,maxtime))+

          ggplot2::scale_y_continuous(
            limits = c(minfreq,maxfreq),
            expand = c(0,0),
            breaks = seq(minfreq, maxfreq, freqinterval))+

          ggplot2::labs(
            y="Frequency (Hz) \n",
            x="\nTime (hour of day)")+

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
              angle = 45,
              hjust=1.1,
              vjust=1),
            axis.title.y = ggplot2::element_text(),
            axis.title.x = ggplot2::element_text(),
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

          ggplot2::labs(fill="OSU INCIDENCE")

      }
    }
  }

  else{

    if (type=="polar"){

      if (annotate==TRUE){

        plot <-
          ggplot2::ggplot(
            df2, ggplot2::aes(time,
                              frequency,
                              fill=value)) +

          ggplot2::geom_tile()+

          viridis::scale_fill_viridis(
            option=palette,
            na.value="black",
            direction = direction,
            guide = ggplot2::guide_legend(
              title.position = "top",
              title.vjust = 1,
              title.hjust = 0.5,
              nrow=2),
            breaks=seq(0, 1, 0.1))+

          ggplot2::scale_x_datetime(
            labels=scales::date_format("%H:%M", tz=tz),
            breaks = scales::breaks_width(timeinterval),
            expand = c(0,0),
            limits = c(mintime,maxtime))+

          ggplot2::scale_y_continuous(
            limits = c(minfreq,(maxfreq+(maxfreq/10))),
            expand = c(0,0),
            breaks = seq(minfreq, maxfreq, freqinterval))+

          ggplot2::labs(
            y="Frequency (Hz) \n",
            x="\nTime (hour of day)")+

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
            axis.title.y = ggplot2::element_text(),
            axis.title.x = ggplot2::element_text(),
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
            geom="label",
            size=labelsize_polar,
            y=seq(minfreq,
                  maxfreq,
                  freqinterval),
            x=min(df2$time),
            label=as.character(seq(minfreq,
                                   maxfreq,
                                   freqinterval)))+

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

          ggplot2::labs(fill="OSU INCIDENCE")

      }

      else{

        if (annotate==FALSE){

          plot <-
            ggplot2::ggplot(
              df2,
              ggplot2::aes(time,
                           frequency,
                           fill=value)) +

            ggplot2::geom_tile()+

            viridis::scale_fill_viridis(
              option=palette,
              na.value="black",
              direction = direction,
              guide = ggplot2::guide_legend(
                title.position = "top",
                title.vjust = 1,
                title.hjust = 0.5,
                nrow=2),
              breaks=seq(0, 1, 0.1))+

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
              y="Frequency (Hz) \n",
              x="\nTime (hour of day)")+

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
              axis.title.y = ggplot2::element_text(),
              axis.title.x = ggplot2::element_text(),
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

            ggplot2::labs(fill="OSU INCIDENCE")

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

                xdata <- sounddiv(aggregate_list = aggregate_list,
                                  qvalue = 0,
                                  subset = "tod",
                                  date = date,
                                  lat = lat,
                                  lon = lon,
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
                    expand = c(0,0))+

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
                      aggregate_list = aggregate_list,
                      qvalue = 0,
                      subset = "total",
                      date = date,
                      lat = lat,
                      lon = lon,
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
                    expand = c(0,0))+

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



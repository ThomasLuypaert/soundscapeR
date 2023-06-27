# Functions for visualizing soundscape diversity by time of day


time_of_day <- soundscape_div <- frequency <- freq <- soundscape_div_smooth <- prop_sound_div <- NULL

#' Visualize Soundscape Diversity by Time of Day
#'
#' @description Produces plots showing the variation in soundscape diversity by time-of-day. Soundscape diversity can be shown for the full frequency range, or the relative contribution of frequency-bins with user-specified width.
#'
#' @param soundscape_obj The soundscape object produced by
#'  \code{\link{ss_aggregate}} function.
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param graphtype The type of plot which is produced.
#'
#'Options are:
#'
#'\emph{'total'}:
#'
#'An area chart showing the soundscape diversity by time-of-day for the entire frequency range.
#'
#'\emph{'frequency'}:
#'
#'A stacked area chart showing the relative contribution of frequency bins with user-defined width to the total soundscape diversity by time-of-day.
#'
#'\emph{'normfreq'}:
#'
#'A percentage stacked area chart showing the normalized relative contribution of frequency bins with user-defined width to the soundscape diversity by time-of-day.
#'
#'\emph{'linefreq'}:
#'
#'A line chart showing the relative contribution of frequency bins with user-defined width to the soundscape diversity by time-of-day.
#' @param minfreq The lower frequency limit for which to visualize the soundscape diversity, expressed as a numeric value.
#' Defaults to the lowest frequency for which data exists in the dataframe.
#' @param maxfreq The upper frequency limit for which to visualize the soundscape diversity, expressed as a numeric value.
#' Defaults to the highest frequency for which data exists in the dataframe.
#' @param nbins If graphtype='frequency'/'normfreq'/'linefreq', determines the number of the frequency-bins by which to divide the frequency range to compute the
#' relative contribution of each bin to the total diversity.
#' @param timeinterval A time interval for the x-axis. Options can be found in the \code{\link[scales]{date_breaks}} documentation.
#' @param smooth One of either TRUE or FALSE. If set to TRUE, applies a moving average filter for smoothing the diversity by time-of-day.
#' @param movavg If smooth=TRUE, determines the width of the moving average filter. Consult \code{\link[pracma]{movavg}} for more information.
#' @param interactive One of either TRUE or FALSE. If set to TRUE, an interactive plot is produced using \code{\link[plotly]{ggplotly}}.
#' @param save One of either TRUE or FALSE. If set to TRUE, saves the plot using \code{\link[ggplot2]{ggsave}}, and the 'dir', 'filename' and 'device'
#' arguments.
#' @param dir Path of the directory to save plot to: path and filename are combined to create the fully qualified file name.
#' Defaults to the working directory. For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param filename The file name without the extention. For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of
#' "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' Defaults to "png". For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#'@param width If save=TRUE, expresses the width of the saved image in milimeters. Defaults to 100 mm.
#' @param height If save=TRUE, expresses the height of the saved image in milimeters. Defaults to 100 mm.
#'
#' @return Returns a ggplot object and if save=TRUE, saves the plot in a directory of choice using a specified device and filename.
#' @export
ss_diversity_plot=function(soundscape_obj,
                          qvalue,
                          graphtype = "total",
                          minfreq = 0,
                          maxfreq = "default",
                          nbins = 10,
                          timeinterval = "1 hour",
                          smooth = TRUE,
                          movavg = 6,
                          interactive = FALSE,
                          save = FALSE,
                          dir = "default",
                          filename = "file.png",
                          device = "png",
                          output = "percentage",
                          width = 150,
                          height = 150){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(soundscape_obj))
  assertthat::assert_that(test_0(qvalue))

  # 1. Check if function input meets expectations

  # 1.1. The supplied soundscape_obj argument is an S4-object of the type
  # 'soundscape', and is not empty.

  test_1 <- function(x){

    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape") &
      assertthat::not_empty(x)

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape'. Please supply the soundscape_obj object produced by the ss_aggregate() or ss_create() functions. Consult the package documentation for further information.")

  }

  assertthat::assert_that(test_1(soundscape_obj))

  # 1.2. The soundscape_obj elements are in the expected format

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

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_3(soundscape_obj@lat))
  assertthat::assert_that(test_4(soundscape_obj@lon))

  # 1.2.3. The time zone argument

  test_5 <- function(x){

    assertthat::is.string(x) & (x %in% (OlsonNames()))

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_5(soundscape_obj@tz))

  # 1.2.4. The sunrise and sunset arguments cannot be wrong (s4 property)

  # 1.2.6. The index argument

  test_7 <- function(x){

    assertthat::is.string(x) & (x %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                         "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_7(soundscape_obj@index))

  # 1.2.7. The samplerate and window arguments

  test_8 <- function(x){

    assertthat::is.count(x)

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_8(soundscape_obj@samplerate))
  assertthat::assert_that(test_8(soundscape_obj@window))

  # 1.2.8. The binarization_method argument

  test_9 <- function(x){
    assertthat::is.string(x) & (x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom"))
  }



  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_9(soundscape_obj@binarization_method))

  # 1.2.9. The threshold argument

  test_10 <- function(x){

    all(length(x) == 1 &
          is.double(x) & !is.na(x))

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not a single numeric value. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the value argument is you're supplying a custom threshold value.")

  }

  assertthat::assert_that(test_10(soundscape_obj@threshold))

  # 1.2.10. The output argument

  test_11 <- function(x){

    all(length(x) == 1 & is.character(x) & (x %in% c("incidence_freq", "raw")))

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a character string describing one of the available output options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.")

  }

  assertthat::assert_that(test_11(soundscape_obj@output))

  # 1.2.11. The merged_df argument

  test_12 <- function(x){

    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      all(apply(x, 2, function(y) all(is.numeric(y))))

  }

  test_13 <- function(x){

    (abs(as.numeric(rownames(x)[1]))+
       abs(as.numeric(rownames(x)[2])))>3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x)))<= soundscape_obj@samplerate/2

  }

  test_14 <- function(x){

    formatted <-  try(
      as.POSIXct(
        paste0(substr(soundscape_obj@first_day, 1, 12)," ", colnames(x)),
        tz = soundscape_obj@tz,
        format="%Y-%m-%d %H:%M:%S"),
      silent = TRUE)

    !any(sapply(formatted, function(y) is.na(y)))

  }


  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.")

  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.")

  }


  assertthat::assert_that(test_12(soundscape_obj@merged_df))
  assertthat::assert_that(test_13(soundscape_obj@merged_df))

  # 1.2.12. The binarized_df argument

  test_15 <- function(x){

    min(x) >= 0 &
      max(x) <= 1

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() or ss_create() function.")

  }

  assertthat::assert_that(test_12(soundscape_obj@binarized_df))
  assertthat::assert_that(test_13(soundscape_obj@binarized_df))
  assertthat::assert_that(test_15(soundscape_obj@binarized_df))

  # 1.2.12. The aggregated_df argument

  assertthat::assert_that(test_12(soundscape_obj@aggregated_df))
  assertthat::assert_that(test_13(soundscape_obj@aggregated_df))
  assertthat::assert_that(test_14(soundscape_obj@aggregated_df))

  if(soundscape_obj@output=="incidence_freq"){

    test_16 <- function(x){

      all(is.double(unlist(x)) & max(x) <= 1 & min(x)>= 0)

    }

    assertthat::on_failure(test_16) <- function(call, env){

      paste0(deparse(call$x), " contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.")

    }

    assertthat::assert_that(test_16(soundscape_obj@aggregated_df))
  }

  if(soundscape_obj@output=="raw"){

    test_16 <- function(x){

      all(all(round(unlist(x)) == unlist(x)) &
            max(x) <= max(table(colnames(soundscape_obj@merged_df))) &
            min(x) >= 0)

    }

    assertthat::on_failure(test_16) <- function(call, env){

      paste0(deparse(call$x), " contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.")

    }

    assertthat::assert_that(test_16(soundscape_obj@aggregated_df))
  }



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

  # 1.4. The graphtype argument is a character string, and one of the
  # available options

  test_9 <- function(x){
    assertthat::is.string(x)
  }

  test_10 <- function(x){
    x %in% c("total","frequency", "normfreq", "linefreq")
  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the ss_diversity_plot graphtype argument as a character string. Consult package documentation for available graphtype argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available ss_diversity_plot graphtype options. Please consult package documentation for available graphtype argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_9(graphtype))
  assertthat::assert_that(test_10(graphtype))

  # 1.5. The minfreq and maxfreq arguments follow
  # the expected values

  test_16 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(soundscape_obj@aggregated_df))) &
       x <= max(as.numeric(rownames(soundscape_obj@aggregated_df)))) |
      x == 0

  }

  test_17 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(soundscape_obj@aggregated_df))) &
       x <= max(as.numeric(rownames(soundscape_obj@aggregated_df)))) |
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

  # 1.6. Check if the nbins argument abides by the
  # expected format

  test_21 <- function(x){

    assertthat::is.count(x) &
      x > 0 &
      x < nrow(soundscape_obj@aggregated_df)
  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.")

  }

  assertthat::assert_that(test_21(nbins))

  # 1.7. Check if supplied timeinterval argument is one of
  # the available options

  test_22 <- function(x){

    any(stringr::str_detect(timeinterval,c("sec", "secs", "min", "mins", "hour", "hours", "day", "days", "week", "weeks", "month", "months", "year", "years"))) &
      grepl("^[[:digit:]]\\s", timeinterval)

  }

  assertthat::on_failure(test_22) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.")

  }

  assertthat::assert_that(test_22(timeinterval))

  # 1.8. Check if the boolean flag arguments (smooth, interactive and save) are in the
  # correct format

  test_21 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a Boolean flag (TRUE or FALSE). Please set argument argument to TRUE or FALSE. Make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_21(smooth))
  assertthat::assert_that(test_21(interactive))
  assertthat::assert_that(test_21(save))

  # 1.9. Check if the supplied movavg argument abides by the expected format

  test_22 <- function(x){

    assertthat::is.count(x) &
      x > 0 &
      x < ncol(soundscape_obj@aggregated_df)
  }

  assertthat::on_failure(test_22) <- function(call, env){

    paste0(deparse(call$x), " is not a valid movavg argument. Please supply the movavg argument as a single positive integer with a value larger than zero and smaller than the number of unique times in the recording period.")

  }

  assertthat::assert_that(test_22(movavg))

  # 1.10. Check if the dir, filename and device arguments
  # follow the expected format.

  if (dir=="default"){
    dir <- getwd()
  }

  else{dir <- dir}

  test_35 <- function(x){

    assertthat::is.string(x)

  }

  assertthat::on_failure(test_35) <- function(call, env){
    paste0(deparse(call$x), " is not a character string. The dir arguments needs to be a character string of either 'default' - or a valid pathname to an existing directory on your device. If you're working on a Windows operating system, pay attention to backslash and forwardslash.")

  }

  assertthat::assert_that(test_35(dir))

  test_37 <- function(x){

    assertthat::is.string(x)

  }

  test_38 <- function(x){

    (sub('.*\\.', '', x) %in% c("eps", "ps","tex", "pdf", "jpeg",
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

  # 1.11. Check that the output argument is in the correct format

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

    paste0(deparse(call$x), " is not one of the available ss_diversity_plot output options. Please consult package documentation for available output argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_22(output))
  assertthat::assert_that(test_23(output))

  # 1.12. Check if the supplied height and width arguments
  # follow the expected format

  test_40 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_40) <- function(call, env){
    paste0(deparse(call$x), " is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.")
  }

  assertthat::assert_that(test_40(height))
  assertthat::assert_that(test_40(width))


  # Create time-handling and subsetting objects

   tz <- soundscape_obj@tz

  if (minfreq=="default"){
    minfreq <- min(as.numeric(rownames(soundscape_obj@aggregated_df)))
  }

  else{minfreq <- minfreq}

  if (maxfreq=="default"){
    maxfreq <- max(as.numeric(rownames(soundscape_obj@aggregated_df)))
  }

  else{maxfreq  <- maxfreq}

  if (dir=="default"){
    dir <- getwd()
  }

  else{dir <- dir}

   # Check if plotly is installed

   if (interactive == TRUE){

     if(nchar(system.file(package='plotly'))==0){

       cat("The 'plotly' R-package needs to be installed before using this function \n")
       cat("Use: 'install.packages('plotly')' to install the package and try again...")
       Sys.sleep(0.00001)
       stop()

     }

     else{}

   }

   # Create graphs

  if (graphtype=="total"){

    total_tod <- ss_diversity(soundscape_obj = soundscape_obj,
                          qvalue = qvalue,
                          subset = "tod",
                          minfreq = minfreq,
                          maxfreq = maxfreq,
                          freqseq = FALSE ,
                          nbins = nbins,
                          output = output)

    total_tod$soundscape_div_smooth <- pracma::movavg(total_tod$soundscape_div,
                                                      movavg,
                                                      type = "t")

    total_tod$time_of_day <- as.POSIXct(
      paste0(soundscape_obj@first_day,
             " ",
             total_tod$time_of_day),
      tz = soundscape_obj@tz)

    if (smooth==TRUE){

    plot <-
      ggplot2::ggplot(total_tod,
                      ggplot2::aes(time_of_day, soundscape_div_smooth)) +

      ggplot2::geom_area(alpha=0.25,
                         fill="#440154FF") +

      ggplot2::geom_line(color="#440154FF",
                         size=1) +

      ggplot2::ylab(if(output=="percentage" & qvalue > 0){"Soundscape diversity (%)\n"}
                    else {if(output == "percentage" & qvalue == 0){"Soundscape richness (%)\n"}
                      else{if(output == "raw" & qvalue >0){"Soundscape diversity (# OSUs)\n"}
                        else{if(output == "raw" & qvalue == 0){"Soundscape richness (# OSUs)\n"}
                          else{}}}}) +

      ggplot2::xlab("\nTime of day (h)") +

      ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                           tz = tz),
                       breaks = scales::date_breaks(timeinterval),
                       expand = c(0,0)) +

      ggplot2::scale_y_continuous(expand = c(0,0),
                                  limits = c(0,
                                             max(total_tod$soundscape_div_smooth)+10)) +

      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.background = ggplot2::element_blank(),
                     plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                              "cm"),
                     panel.border = ggplot2::element_blank(),
                     axis.line = ggplot2::element_line(colour = "black"),
                     axis.text.x = ggplot2::element_text(color = "black",
                                                         size = 10,
                                                         angle = -45,
                                                         vjust = 1.2,
                                                         hjust = -0.3),
                     axis.text.y = ggplot2::element_text(color = "black",
                                                         size = 10)) +

      ggplot2::annotate(geom = "text",
                        x = (max(total_tod$time_of_day)-10000),
                        y=if(output=="percentage"){95}
                        else{(max(total_tod$soundscape_div_smooth)+7.5)},
                        label = paste0("q-value = ",
                                       qvalue),
                        size = 5)

    if (interactive==TRUE){

      plotly::ggplotly(plot)

    }


    else{plot}

    }


    else{

      if (smooth==FALSE){

        plot <-

          ggplot2::ggplot(total_tod,
                          ggplot2::aes(time_of_day,
                                       soundscape_div)) +

          ggplot2::geom_area(alpha = 0.25,
                             fill="#440154FF") +

          ggplot2::geom_line(color = "#440154FF",
                             size=1) +

          ggplot2::ylab(if(output=="percentage" & qvalue > 0){"Soundscape diversity (%)\n"}
                        else {if(output == "percentage" & qvalue == 0){"Soundscape richness (%)\n"}
                          else{if(output == "raw" & qvalue >0){"Soundscape diversity (# OSUs)\n"}
                            else{if(output == "raw" & qvalue == 0){"Soundscape richness (# OSUs)\n"}
                              else{}}}}) +

          ggplot2::xlab("\nTime of day (h)") +

          ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                 tz = tz),
                           breaks = scales::date_breaks(timeinterval),
                           expand = c(0,0)) +

          ggplot2::scale_y_continuous(expand = c(0,0),
                                      limits=c(0,
                                               max(total_tod$soundscape_div)+10)) +

          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                  "cm"),
                         panel.border = ggplot2::element_blank(),
                         axis.line = ggplot2::element_line(colour = "black"),
                         axis.text.x = ggplot2::element_text(color = "black",
                                                             size = 10,
                                                             angle = -45,
                                                             vjust = 1.2,
                                                             hjust = -0.3),
                         axis.text.y = ggplot2::element_text(color = "black",
                                                             size = 10)) +

          ggplot2::annotate(geom = "text",
                            x = (max(total_tod$time_of_day)-10000),
                            y=if(output=="percentage"){95}
                            else{(max(total_tod$soundscape_div)+7.5)},
                            label = paste0("q-value = ",
                                           qvalue),
                            size=5)

        if (interactive==TRUE){
          plotly::ggplotly(plot)
        }

        else{plot}

      }
    }
  }

  else{

    if (graphtype=="frequency"){

      freq_tod <- ss_diversity(soundscape_obj = soundscape_obj,
                           qvalue = qvalue,
                           subset = "tod",
                           minfreq = minfreq,
                           maxfreq = maxfreq,
                           freqseq = TRUE ,
                           nbins = nbins,
                           output = "raw")

      total_tod <- ss_diversity(soundscape_obj = soundscape_obj,
                            qvalue = qvalue,
                            subset = "tod",
                            minfreq = minfreq,
                            maxfreq = maxfreq,
                            freqseq = FALSE ,
                            nbins = nbins,
                            output = output)

      total_tod$soundscape_div_smooth <- pracma::movavg(total_tod$soundscape_div,
                                                        movavg,
                                                        type = "t")

      if (output == "percentage"){

        for (i in 1:length(freq_tod)){

            freq_tod[[i]][,1] <- (freq_tod[[i]][,1] /
                                    nrow(soundscape_obj@aggregated_df))*100

        }
      }

      for (i in 1:length(freq_tod)){
        freq_tod[[i]]$soundscape_div_smooth <- pracma::movavg(freq_tod[[i]][,1],
                                                              movavg,
                                                              type = "t")


        freq_tod[[i]]$frequency <- names(freq_tod)[i]
      }

      max_freq_raw <- max(total_tod$soundscape_div) + 15
      max_freq_smooth <- max(total_tod$soundscape_div_smooth) + 15

      freq_tod <- dplyr::bind_rows(freq_tod)

      freq_tod$time_of_day <- as.POSIXct(
        paste0(soundscape_obj@first_day,
               " ",
               freq_tod$time_of_day),
        tz = soundscape_obj@tz)

      freq_tod <- freq_tod[order(as.integer(sub("-.*$", "", freq_tod$frequency))),]
      freq_tod$frequency <- factor(freq_tod$frequency,
                                   levels = unique(freq_tod$frequency))

      if (smooth=="TRUE"){

        plot <-
          ggplot2::ggplot(freq_tod,
                          ggplot2::aes(time_of_day, soundscape_div_smooth,
                                       fill = frequency,
                                       color = frequency)) +

          ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE),
                             size = 0.5) +

          ggplot2::ylab(if(output=="percentage" & qvalue > 0){"Soundscape diversity (%)\n"}
                        else {if(output == "percentage" & qvalue == 0){"Soundscape richness (%)\n"}
                          else{if(output == "raw" & qvalue >0){"Soundscape diversity (# OSUs)\n"}
                            else{if(output == "raw" & qvalue == 0){"Soundscape richness (# OSUs)\n"}
                              else{}}}}) +

          ggplot2::xlab("Time of day (h)") +

          ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                 tz = tz),
                           breaks = scales::date_breaks(timeinterval),
                           expand = c(0,0)) +

          ggplot2::scale_y_continuous(expand = c(0,0),
                                      limits=c(0,max_freq_smooth)) +

          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                  "cm"),
                         panel.border = ggplot2::element_blank(),
                         axis.line = ggplot2::element_line(colour = "black"),
                         axis.text.x = ggplot2::element_text(color = "black",
                                                             size = 10,
                                                             angle = -45,
                                                             vjust = 1.2,
                                                             hjust = -0.3),
                         axis.text.y = ggplot2::element_text(color = "black",
                                                             size = 10),
                         legend.justification=c(0.5,1),
                         legend.position=c(0.5, 1)) +

          viridis::scale_fill_viridis(
            alpha = 0.9,
            discrete = TRUE,
            guide = ggplot2::guide_legend(title = NULL,
                                          direction ="horizontal",
                                          nrow = 2,
                                          label.position = "top")) +

          viridis::scale_color_viridis(
            discrete = TRUE,
            guide = "none")

        if (interactive==TRUE){
          plotly::ggplotly(plot)}

        else{plot}

      }

      else{

        if (smooth=="FALSE"){

            plot <-
              ggplot2::ggplot(freq_tod, ggplot2::aes(time_of_day,
                                                     soundscape_div,
                                                     fill = frequency,
                                                     color = frequency)) +

              ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +

              ggplot2::ylab(if(output=="percentage" & qvalue > 0){"Soundscape diversity (%)\n"}
                            else {if(output == "percentage" & qvalue == 0){"Soundscape richness (%)\n"}
                              else{if(output == "raw" & qvalue >0){"Soundscape diversity (# OSUs)\n"}
                                else{if(output == "raw" & qvalue == 0){"Soundscape richness (# OSUs)\n"}
                                  else{}}}}) +

              ggplot2::xlab("Time of day (h)") +

              ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                     tz = tz),
                               breaks = scales::date_breaks(timeinterval),
                               expand = c(0,0)) +

              ggplot2::scale_y_continuous(expand = c(0,0),
                                          limits=c(0,max_freq_raw)) +

              ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                             panel.grid.minor = ggplot2::element_blank(),
                             panel.background = ggplot2::element_blank(),
                             plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                      "cm"),
                             panel.border = ggplot2::element_blank(),
                             axis.line = ggplot2::element_line(colour = "black"),
                             axis.text.x = ggplot2::element_text(color = "black",
                                                                 size = 10,
                                                                 angle = -45,
                                                                 vjust = 1.2,
                                                                 hjust = -0.3),
                             axis.text.y = ggplot2::element_text(color = "black",
                                                                 size = 10),
                             legend.justification = c(0.5,1),
                             legend.position = c(0.5, 1)) +

              viridis::scale_fill_viridis(
                alpha = 0.9,
                discrete = TRUE,
                guide=ggplot2::guide_legend(title = NULL,
                                            direction = "horizontal",
                                            nrow = 2,
                                            label.position = "top")) +

              viridis::scale_color_viridis(
                discrete = TRUE,
                guide = "none")


            if (interactive==TRUE){
              plotly::ggplotly(plot)}

            else{plot}
        }
      }
    }

    else{

      if (graphtype=="normfreq"){

        freq_tod <- ss_diversity(soundscape_obj = soundscape_obj,
                             qvalue = qvalue,
                             subset = "tod",
                             minfreq = minfreq,
                             maxfreq = maxfreq,
                             freqseq = TRUE ,
                             nbins = nbins,
                             output = "raw")

        for (i in 1:length(freq_tod)){

          freq_tod[[i]]$soundscape_div_smooth <- pracma::movavg(freq_tod[[i]][,1],
                                                                movavg,
                                                                type="t")
          freq_tod[[i]]$frequency <- names(freq_tod)[i]
        }

        freq_tod=dplyr::bind_rows(freq_tod)

        freq_tod$time_of_day <- as.POSIXct(
          paste0(soundscape_obj@first_day,
                 " ",
                 freq_tod$time_of_day),
          tz = soundscape_obj@tz)

        freq_tod <- freq_tod[order(as.integer(sub("-.*$", "", freq_tod$frequency))),]
        freq_tod$frequency <- factor(freq_tod$frequency,
                                     levels = unique(freq_tod$frequency))


        if (smooth==TRUE){

          plot <-

            freq_tod %>%

            dplyr::group_by(time_of_day) %>%

            dplyr::mutate(prop_sound_div = soundscape_div_smooth /
                            sum(soundscape_div_smooth)) %>%

            dplyr::ungroup()

          time_of_day <- unique(plot$time_of_day)
          frequency <- unique(plot$frequency)

          plot_2 <- expand.grid(time_of_day = time_of_day, frequency = frequency)

          plot <- dplyr::full_join(plot, plot_2, by = c("time_of_day" = "time_of_day", "frequency" = "frequency")) %>%
            dplyr::mutate(prop_sound_div = ifelse(is.na(prop_sound_div), 0, prop_sound_div)) %>%
            dplyr::arrange(time_of_day, frequency)

          plot <-

            ggplot2::ggplot(plot, ggplot2::aes(x = time_of_day,
                             y = prop_sound_div,
                             group = frequency,
                             fill = frequency,
                             color = frequency)) +

            ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE))+

            ggplot2::ylab("Contribution to total diversity") +

            ggplot2::xlab("Time of day (h)") +

            ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M",
                                                                 tz = tz),
                             breaks = scales::date_breaks(timeinterval),
                             expand = c(0,0)) +

            ggplot2::scale_y_continuous(expand = c(0,0))+

            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),

                           panel.grid.minor = ggplot2::element_blank(),
                           panel.background = ggplot2::element_blank(),
                           plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                    "cm"),
                           panel.border = ggplot2::element_blank(),
                           axis.line = ggplot2::element_line(colour = "black"),
                           axis.text.x = ggplot2::element_text(color = "black",
                                                               size = 10,
                                                               angle = -45,
                                                               vjust = 1.2,
                                                               hjust = -0.3),
                           axis.text.y = ggplot2::element_text(color = "black",
                                                               size = 10),
                           legend.position = "top",
                           legend.key.width = grid::unit(3,
                                                         "cm")) +

            viridis::scale_fill_viridis(
              alpha = 0.9,
              discrete = TRUE,
              guide=ggplot2::guide_legend(title = NULL,
                                          direction = "horizontal",
                                          nrow = 2,
                                          label.position = "top")) +

            viridis::scale_color_viridis(
              discrete = TRUE,
              guide = "none")

          if (interactive==TRUE){
            plotly::ggplotly(plot)}

          else{plot}

        }

        else{

          if (smooth==FALSE){

            plot <-
              freq_tod %>%

              dplyr::group_by(time_of_day) %>%

              dplyr::mutate(prop_sound_div = soundscape_div / sum(soundscape_div)) %>%

              dplyr::ungroup() %>%

              ggplot2::ggplot(ggplot2::aes(x = time_of_day,
                                           y = prop_sound_div,
                                           fill = frequency,
                                           color = frequency)) +

                ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +

                ggplot2::ylab("Contribution to total diversity") +

                ggplot2::xlab("Time of day (h)") +

                ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                       tz = tz),
                                 breaks = scales::date_breaks(timeinterval),
                                 expand = c(0,0)) +

                ggplot2::scale_y_continuous(expand = c(0,0),
                                            limits = c(0,1)) +

                ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.background = ggplot2::element_blank(),
                               plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                        "cm"),
                               panel.border = ggplot2::element_blank(),
                               axis.line = ggplot2::element_line(colour = "black"),
                               axis.text.x = ggplot2::element_text(color = "black",
                                                                   size = 10,
                                                                   angle = -45,
                                                                   vjust = 1.2,
                                                                   hjust = -0.3),
                               axis.text.y = ggplot2::element_text(color = "black",
                                                                   size = 10),
                               legend.position = "top",
                               legend.key.width = grid::unit(3,
                                                             "cm"),
                               aspect.ratio = 0.3) +

                viridis::scale_fill_viridis(
                  alpha = 0.9,
                  discrete = TRUE,
                  guide=ggplot2::guide_legend(title = NULL,
                                             direction = "horizontal",
                                             nrow = 2,
                                             label.position = "top")) +

              viridis::scale_color_viridis(
                discrete = TRUE,
                guide = "none")

            if (interactive==TRUE){
              plotly::ggplotly(plot)}

            else{plot}

          }
        }
      }

      else{

        if (graphtype=="linefreq"){

          freq_tod <- ss_diversity(soundscape_obj = soundscape_obj,
                               qvalue=qvalue,
                               subset = "tod",
                               minfreq=minfreq,
                               maxfreq=maxfreq,
                               freqseq=TRUE ,
                               nbins=nbins,
                               output=output)


          for (i in 1:length(freq_tod)){
            freq_tod[[i]]$soundscape_div_smooth <- pracma::movavg(freq_tod[[i]][,1],
                                                                  movavg,
                                                                  type="t")
            freq_tod[[i]]$frequency <- names(freq_tod)[i]
          }

          freq_tod=dplyr::bind_rows(freq_tod)

          freq_tod$time_of_day <- as.POSIXct(
            paste0(soundscape_obj@first_day,
                   " ",
                   freq_tod$time_of_day),
            tz = soundscape_obj@tz)

          freq_tod <- freq_tod[order(as.integer(sub("-.*$", "", freq_tod$frequency))),]
          freq_tod$frequency <- factor(freq_tod$frequency,
                                       levels = unique(freq_tod$frequency))


          if (smooth==TRUE){

            plot <-
              freq_tod %>%

              ggplot2::ggplot(ggplot2::aes(x = time_of_day,
                                           y = soundscape_div_smooth)) +

              ggplot2::geom_line(ggplot2::aes(color = frequency),
                                 size=1) +

              ggplot2::geom_area(ggplot2::aes(fill = frequency),
                                 alpha=0.2) +

              viridis::scale_color_viridis(discrete = TRUE) +

              viridis::scale_fill_viridis(discrete = TRUE) +

              ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                             panel.grid.minor = ggplot2::element_blank(),
                             panel.background = ggplot2::element_blank(),
                             plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                      "cm"),
                             panel.border = ggplot2::element_blank(),
                             axis.line = ggplot2::element_line(colour = "black"),
                             axis.text.x = ggplot2::element_text(color = "black",
                                                                 size = 10,
                                                                 angle = -45,
                                                                 vjust = 1.2,
                                                                 hjust = -0.3),
                             axis.text.y = ggplot2::element_text(color = "black",
                                                                 size = 10),
                             legend.position = "none",
                             strip.background = ggplot2::element_rect(color = "black",
                                                                      fill = "white"),
                             strip.text = ggplot2::element_text(color = "black",
                                                                size = 10)) +

              ggplot2::ylab(if(output=="percentage" & qvalue > 0){"Soundscape diversity (%)\n"}
                            else {if(output == "percentage" & qvalue == 0){"Soundscape richness (%)\n"}
                              else{if(output == "raw" & qvalue >0){"Soundscape diversity (# OSUs)\n"}
                                else{if(output == "raw" & qvalue == 0){"Soundscape richness (# OSUs)\n"}
                                  else{}}}}) +

              ggplot2::xlab("Time of day (h)") +

              ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                     tz = tz),
                               breaks = scales::date_breaks(timeinterval),
                               expand = c(0,0)) +

              ggplot2::scale_y_continuous(expand = c(0,0)) +

              ggplot2::facet_wrap(~frequency, nrow = 1)

            if (interactive==TRUE){
              plotly::ggplotly(plot)}

            else{plot}
          }

          else{

            if (smooth==FALSE){

              labels <-
                paste0(
                  as.integer(
                    seq((minfreq-minfreq),
                        (maxfreq-(maxfreq/nbins)),
                        (maxfreq/nbins))),
                  "-",
                  as.integer(
                    seq((maxfreq/nbins),
                        maxfreq,
                        (maxfreq/nbins))),
                  " ",
                  "Hz")

              names(labels) <- seq((minfreq+(maxfreq/nbins)),
                                   maxfreq,
                                   (maxfreq/nbins))

              plot <-
                freq_tod %>%

                ggplot2::ggplot(ggplot2::aes(x = time_of_day,
                                             y = soundscape_div)) +

                ggplot2::geom_line(ggplot2::aes(color = frequency),
                                   size = 1) +

                ggplot2::geom_area(ggplot2::aes(fill = frequency),
                                   alpha=0.2) +

                viridis::scale_color_viridis(discrete = TRUE) +

                viridis::scale_fill_viridis(discrete = TRUE) +

                ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                               panel.grid.minor = ggplot2::element_blank(),
                               panel.background = ggplot2::element_blank(),
                               plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5),
                                                        "cm"),
                               panel.border = ggplot2::element_blank(),
                               axis.line = ggplot2::element_line(colour = "black"),
                               axis.text.x = ggplot2::element_text(color = "black",
                                                                   size = 10,
                                                                   angle = -45,
                                                                   vjust = 1.2,
                                                                   hjust = -0.3),
                               axis.text.y = ggplot2::element_text(color = "black",
                                                                   size = 10),
                               legend.position = "none",
                               strip.background = ggplot2::element_rect(color = "black",
                                                                        fill = "white"),
                               strip.text = ggplot2::element_text(color = "black",
                                                                  size = 10)) +

                ggplot2::ylab(if(output=="percentage" & qvalue > 0){"Soundscape diversity (%)\n"}
                              else {if(output == "percentage" & qvalue == 0){"Soundscape richness (%)\n"}
                                else{if(output == "raw" & qvalue >0){"Soundscape diversity (# OSUs)\n"}
                                  else{if(output == "raw" & qvalue == 0){"Soundscape richness (# OSUs)\n"}
                                    else{}}}}) +

                ggplot2::xlab("Time of day (h)") +

                ggplot2::scale_x_datetime(labels = scales::date_format("%H:%M",
                                                                       tz = tz),
                                          breaks = scales::date_breaks(timeinterval),
                                          expand = c(0,0)) +

                ggplot2::scale_y_continuous(expand = c(0,0)) +

                ggplot2::facet_wrap(~frequency, nrow = 1)

              if (interactive==TRUE){
                plotly::ggplotly(plot)}

              else{plot}

            }
          }
        }
      }
    }
  }

  if (save==TRUE){
    ggplot2::ggsave(filename = paste0(paste0(graphtype,
                                             "_"),
                                      filename,
                                      ".",
                                      device),
                    plot = plot,
                    device = device,
                    path = dir,
                    dpi = "retina",
                    width = width,
                    height = height,
                    units = c("mm"))

    if (interactive==TRUE){
      plotly::ggplotly(plot)}

    else{plot}
  }

  else{
    if (interactive==TRUE){
      plotly::ggplotly(plot)}

    else{plot}
    }
}

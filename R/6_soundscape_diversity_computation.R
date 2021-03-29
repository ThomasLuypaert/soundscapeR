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
#' @param aggregate_list The list produced by the \code{\link{aggregate_df}}
#'  function.
#' @param qvalue A positive integer or decimal number (>=0), most commonly
#'  between 0-3. This parameter modulates the sensitivity of diversity
#'  values to the relative abundance of Operational Sound Units (OSUs).
#'  A value of 0 corresponds to the richness, a value of 1 is the equivalent
#'   effective number of OSUs for the Shannon index, a value of 2 is the
#'    equivalent effective number of OSUs for the Simpson index.
#' @param subset The scale for which the soundscape diversity is computed.
#'  Options are 'total', 'day', night', 'dawn', 'dusk' and
#'  'tod' (time of day - for each unique time in the day).
#' @param date The first day of the recording period. Used for managing
#' time-objects in R. Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were
#' collected, expressed in decimal degrees.
#' @param lon The longitude of the site at which the sound files were
#' collected, expressed in decimal degrees.
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
#' @param twilight A character string of the twilight method to be used
#'  for sunrise and sunset as the boundary between day and night.
#' Options can be found in the \code{\link[photobiology]{day_night}}
#'  documentation.
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
sounddiv=function(aggregate_list,
                  qvalue,
                  subset="total",
                  date,
                  lat,
                  lon,
                  mintime="default",
                  maxtime="default",
                  minfreq=0,
                  maxfreq="default",
                  twilight="sunlight",
                  dawnstart=0,
                  dawnend=5400,
                  duskstart=5400,
                  duskend=0,
                  freqseq=FALSE,
                  nbins=10,
                  output="percentage"){

  # 0. Check if the arguments are missing

  test_0 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_0) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_0(aggregate_list))
  assertthat::assert_that(test_0(qvalue))
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

  # 1.2. The supplied qvalue argument is a positive integer or decimal number

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

  # 1.3. The subset argument is a character string, and one of the
  # available options

  test_9 <- function(x){
    assertthat::is.string(x)
  }

  test_10 <- function(x){
    x %in% c("total","day", "night", "dawn", "dusk", "tod")
  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the sounddiv subset argument as a character string. Consult package documentation for available subset argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available sounddiv subset options. Please consult package documentation for available subset argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_9(subset))
  assertthat::assert_that(test_10(subset))

  # 1.4. the supplied date is in the right format

  test_11 <- function(x) {
    assertthat::is.string(x)
  }

  test_12 <- function(x) {
    formatted = try(as.Date(x, "%Y-%m-%d"), silent = TRUE)
    is_date = as.character(formatted) == x & !is.na(formatted)
    return(is_date)
  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.")

  }

  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.")

  }

  assertthat::assert_that(test_11(date))
  assertthat::assert_that(test_12(date))

  # 1.5. The lat and lon are in decimal degrees and
  # exist on Earth

  test_13 <- function(x){

    is.numeric(x) &
      x >= -90 &
      x <= 90

  }

  test_14 <- function(x){

    is.numeric(x) &
      x >= -180 &
      x <= 180

  }

  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_13(lat))
  assertthat::assert_that(test_14(lon))

  # 1.6. the supplied mintime and maxtime arguments
  # are one of the available options

  test_15 <- function(x){

    x == "default" |
      !is.na(as.POSIXct(x, format="%H:%M:%S"))

  }

  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.")

  }

  assertthat::assert_that(test_15(mintime))
  assertthat::assert_that(test_15(maxtime))

  # 1.7. The minfreq and maxfreq arguments follow
  # the expected values

  test_16 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregate_list[[3]]))) &
       x <= max(as.numeric(rownames(aggregate_list[[3]])))) |
      x == 0

  }

  test_17 <- function(x){
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(aggregate_list[[3]]))) &
       x <= max(as.numeric(rownames(aggregate_list[[3]])))) |
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

  # 1.8. The supplied twilight argument is one
  # of the available options

  test_18 <- function(x){

    (assertthat::is.string(x) &
       x %in% c("none","rim","refraction","sunlight","civil",
                "nautical","astronomical")) |
      (is.vector(x, mode="double") & (length(x) == 1 | length(x) ==2))

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.")

  }

  assertthat::assert_that(test_18(twilight))

  # 1.9. The dawnstart, dawnend, duskstart and duskend arguments are either
  # zero or a single positive integer

  test_19 <- function(x){

    x == 0 | assertthat::is.count(x)

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.")

  }

  assertthat::assert_that(test_19(dawnstart))
  assertthat::assert_that(test_19(dawnend))
  assertthat::assert_that(test_19(duskstart))
  assertthat::assert_that(test_19(duskend))

  # 1.10. The freqseq argument is a boolean flag

  test_20 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_20) <- function(call, env){

    paste0(deparse(call$x), " is not a Boolean flag (TRUE or FALSE). Please set the freqseq argument to TRUE or FALSE. Make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_20(freqseq))

  # 1.11. Check if the nbins argument abides by the
  # expected format

  test_21 <- function(x){

    assertthat::is.count(x) &
      x > 0 &
      x < nrow(aggregate_list[[3]])
  }

  assertthat::on_failure(test_21) <- function(call, env){

    paste0(deparse(call$x), " is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.")

  }

  assertthat::assert_that(test_21(nbins))

  # 1.12. The output argument is a string and one of the available options

  test_22 <- function(x){
    assertthat::is.string(x)
  }

  test_23 <- function(x){
    x %in% c("percentage", "raw")
  }

  assertthat::on_failure(test_22) <- function(call, env){

    paste0(deparse(call$x), " is not a character string. Please supply the heatmap type as a character string. Consult package documentation for available output argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::on_failure(test_23) <- function(call, env){

    paste0(deparse(call$x), " is not one of the available sounddiv output options. Please consult package documentation for available output argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

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

  tz <- lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")

  test_24 <- function(x) {

    return(x %in% (OlsonNames()))

  }

  assertthat::assert_that(test_24(tz))

  day <- as.POSIXct(
    strptime(
      paste(date,
            "00:00:00",
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))

  test_25 <- function(x){

    !is.na(as.POSIXct(x, format="%Y-%m-%d %H:%M:%S"))

  }

  assertthat::assert_that(test_25(day))

  points <- data.frame(lon = lon, lat = lat)

  test_26 <- function(x){

    is.data.frame(x)

  }

  test_27 <- function(x){

    assertthat::not_empty(x)

  }

  test_28 <- function(x){

    assertthat::noNA(x)

  }

  assertthat::assert_that(test_26(points))
  assertthat::assert_that(test_27(points))
  assertthat::assert_that(test_28(points))

  suntimes <- photobiology::day_night(
    date = date,
    tz= tz,
    geocode = points,
    twilight = twilight,
    unit.out = "datetime")

  assertthat::assert_that(test_26(suntimes))
  assertthat::assert_that(test_27(suntimes))

  sunrise <- as.POSIXct(suntimes$sunrise,
                        tz = tz,
                        format = "%Y-%m-%d %H:%M:%S")

  sunset <- as.POSIXct(suntimes$sunset,
                       tz = tz,
                       format = "%Y-%m-%d %H:%M:%S")

  assertthat::assert_that(test_25(sunrise))
  assertthat::assert_that(test_25(sunset))

  # 4. Set minfreq, maxfreq, mintime and maxtime arguments

  if (maxfreq=="default"){
    maxfreq <- max(as.numeric(rownames(aggregate_list[[3]])))
  }

  else{maxfreq <- maxfreq}

  if (mintime=="default"){
    mintime <- min(as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  }

  else{mintime <- as.POSIXct(strptime(paste(date, mintime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  assertthat::assert_that(test_25(mintime))

  if (maxtime=="default"){
    maxtime <- max(as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  }

  else{maxtime <- as.POSIXct(strptime(paste(date, maxtime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  assertthat::assert_that(test_25(mintime))

  # 5. Set row names, column names and subsetting objects + create new df

  rownames_df <- as.numeric(
    rownames(aggregate_list[[3]]))

  rownames_subset <- as.character(
    subset(rownames_df,
           rownames_df >= minfreq &
             rownames_df <= maxfreq))

  colnames_df <- as.POSIXct(
    strptime(
      paste(date,
            colnames(aggregate_list[[3]]),
            sep=" "),
      format= "%Y-%m-%d %H:%M:%S",
      tz=tz))

  colnames_subset <- as.character(
    hms::as_hms(
      subset(colnames_df,
             colnames_df >= mintime &
               colnames_df <= maxtime)))

  new_df <- aggregate_list[[3]][rownames_subset,colnames_subset]

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
              paste(date,
                    colnames(aggregate_list[[3]]),
                    sep=" "),
              format= "%Y-%m-%d %H:%M:%S",
              tz=tz))))

        colnames(soundscape_diversity) <-
          c(paste0("soundscape_div",
                   " (q=", qvalue, ")"),
            "time_of_day")

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

          daytime_df <- aggregate_list[[3]][rownames_subset,colnames_day]

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

            nighttime_df <- aggregate_list[[3]][rownames_subset,
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

              dawntime_df <- aggregate_list[[3]][rownames_subset,
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

                dusktime_df <- aggregate_list[[3]][rownames_subset,
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

        soundscape_diversity$frequency_bin <-
          paste0(
            seq((minfreq-minfreq),
                (maxfreq-(maxfreq/nbins)),
                maxfreq/nbins),
            "-",
            seq(((minfreq-minfreq)+(maxfreq/nbins)),
                maxfreq,
                maxfreq/nbins),
            " Hz")

        colnames(soundscape_diversity) <-
          c(paste0("soundscape_div",
                   " (q=",
                   qvalue,
                   ")"),
            "freq_interval")

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
                    paste(date,
                          colnames(aggregate_list[[3]]),
                          sep=" "),
                    format= "%Y-%m-%d %H:%M:%S",
                    tz=tz))))

            colnames(soundscape_diversity[[i]]) <-
              c(
                paste0("soundscape_div",
                       " (q=",
                       qvalue,
                       ")"),
                "time_of_day")
          }

          names(soundscape_diversity) <-
            paste0(seq((minfreq-minfreq),
                       (maxfreq-(maxfreq/nbins)),
                       maxfreq/nbins),
                   "-",
                   seq(((minfreq-minfreq)+(maxfreq/nbins)),
                       maxfreq,
                       maxfreq/nbins),
                   " Hz")

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
            soundscape_diversity$frequency_bin <-
              paste0(
                seq((minfreq-minfreq),
                    (maxfreq-(maxfreq/nbins)),
                    maxfreq/nbins),
                "-",
                seq(((minfreq-minfreq)+(maxfreq/nbins)),
                    maxfreq,
                    maxfreq/nbins),
                " Hz")

            colnames(soundscape_diversity) <-
              c(paste0("soundscape_div_day",
                       " (q=",
                       qvalue,
                       ")"),
                "freq_interval")

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

              soundscape_diversity$frequency_bin <-
                paste0(
                  seq((minfreq-minfreq),
                      (maxfreq-(maxfreq/nbins)),
                      maxfreq/nbins),
                  "-",
                  seq(((minfreq-minfreq)+(maxfreq/nbins)),
                      maxfreq,
                      maxfreq/nbins),
                  " Hz")

              colnames(soundscape_diversity) <-
                c(paste0("soundscape_div_night",
                         " (q=",
                         qvalue,
                         ")"),
                  "freq_interval")

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

                soundscape_diversity$frequency_bin <-
                  paste0(
                    seq((minfreq-minfreq),
                        (maxfreq-(maxfreq/nbins)),
                        maxfreq/nbins),
                    "-",
                    seq(((minfreq-minfreq)+(maxfreq/nbins)),
                        maxfreq,
                        maxfreq/nbins),
                    " Hz")

                colnames(soundscape_diversity) <-
                  c(paste0("soundscape_div_dawn",
                           " (q=",
                           qvalue,
                           ")"),
                    "freq_interval")

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

                  soundscape_diversity$frequency_bin <-
                    paste0(
                      seq((minfreq-minfreq),
                          (maxfreq-(maxfreq/nbins)),
                          maxfreq/nbins),
                      "-",
                      seq(((minfreq-minfreq)+(maxfreq/nbins)),
                          maxfreq,
                          maxfreq/nbins),
                      " Hz")

                  colnames(soundscape_diversity) <-
                    c(paste0("soundscape_div_dusk",
                             " (q=",
                             qvalue,
                             ")"),
                      "freq_interval")

                  soundscape_diversity

              }
            }
          }
        }
      }
    }
  }
}

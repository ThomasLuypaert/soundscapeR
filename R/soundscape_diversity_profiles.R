utils::globalVariables(c("div_order", "eff_osus"))

#' Visualize soundscape diversity profiles - only for internal use
#'
#' @param aggregate_list The list produced by the \code{\link{aggregate_df}} function.
#' @param qvalues A numeric vector of sequential q-values for which to compute
#'  the diversity profiles, generally ranging between 0-5.
#'  \emph{e.g.:} \code{seq(0, 5, 0.1)} yields a vector between 0 and 5 with
#'  an increment value of 0.1. The smaller the increment value, the
#'  smoother the diversity profiles.
#' @param subset The scale for which the soundscape diversity is computed.
#'  Options are 'total', 'day', night', 'dawn', 'dusk' and 'tod'
#'  (time of day - for each unique time in the day).
#' @param minfreq A numeric value indicating the lower frequency limit for
#'  which to compute the soundscape diversity. If set to default, uses the
#'   lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit for
#'  which to compute the soundscape diversity. If set to default, uses the
#'   highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape
#'  diversity, formatted as "HH:MM:SS". If set to default, uses the earliest
#'   time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape
#'  diversity, formatted as "HH:MM:SS". If set to default, uses the latest
#'   time for which data exists in the dataframe.
#' @param date The first day of the recording period. Used for managing
#' time-objects in R. Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were
#' collected, expressed in decimal degrees.
#' @param lon The longitude of the site at which the sound files were
#'  collected, expressed in decimal degrees.
#' @param twilight A character string of the twilight method to be used for
#'  sunrise and sunset as the boundary between day and night. Options can
#'  be found in the \code{\link[photobiology]{day_night}} documentation.
#' @param dawnstart A numeric argument. If subset='dawn', used to determine
#' the start of dawn. By default, dawn starts at sunrise. Expressed as
#' the time in seconds before sunrise.
#' @param dawnend A numeric argument. If subset='dawn', used to determine
#' the end of dawn. By default, dawn ends 1.5 hours after sunrise.
#' Expressed as the time in seconds after sunrise.
#' @param duskstart A numeric argument. If subset='dusk', used to determine
#'  the start of dusk. By default, dusk starts 1.5 hours before sunset.
#'   Expressed as the time in seconds before sunset.
#' @param duskend A numeric argument. If subset='dusk', used to determine
#' the end of dusk. By default, dusk ends at sunset. Expressed as the time
#' in seconds after sunset.
#' @param display A character string. Indicates the output format, one of
#'  either "plot" or "table".
#' @param output A character string. Indicates the format in which the
#'  soundscape diversity is expressed. Options are "percentage"
#'  (the fraction between the observed soundscape diversity and the
#'  maximum possible soundscape diversity), or "raw" (the number of
#'   acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#' @return A soundscape diversity profile plot or table.
sounddiv_profile_internal <- function(aggregate_list, qvalues=seq(0, 5, 0.1), subset="total",
                                      minfreq="default", maxfreq="default", mintime="default",
                                      maxtime="default",date, lat, lon, twilight="sunlight",
                                      dawnstart=0, dawnend=5400, duskstart=5400, duskend=0,
                                      display="plot", output="percentage"){

  # 0. Providing binding for global variable

  rownames_aggregate_list <- NULL

  # Data preparations

  tz <- lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")

  day <- as.POSIXct(strptime(paste(date, "00:00", sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))
  points <- as.data.frame(t(as.data.frame(c(lat, lon))))
  colnames(points) <- c("lat","lon")
  suntimes <- photobiology::day_night(date=day,
                                      tz=lubridate::tz(day),
                                      geocode = points,
                                      twilight = twilight,
                                      unit.out = "datetime")
  sunrise <- suntimes$sunrise
  sunset <- suntimes$sunset

  if (minfreq=="default"){
    freq1 <- min(as.numeric(rownames(aggregate_list[[3]])))
  }

  else{freq1 <- minfreq}

  if (maxfreq=="default"){
    freq2 <- max(as.numeric(rownames(aggregate_list[[3]])))
  }

  else{freq2 <- maxfreq}

  if (mintime=="default"){
    time1 <- min(as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "),
                                     format= "%Y-%m-%d %H:%M:%S",
                                     tz=tz)))
  }

  else{
    time1 <- as.POSIXct(strptime(paste(date, mintime, sep=" "),
                                 format= "%Y-%m-%d %H:%M:%S",
                                 tz=tz))
  }

  if (maxtime=="default"){
    time2 <- max(as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "),
                                     format= "%Y-%m-%d %H:%M:%S",
                                     tz=tz)))
  }

  else{
    time2 <- as.POSIXct(strptime(paste(date, maxtime, sep=" "),
                                 format= "%Y-%m-%d %H:%M:%S",
                                 tz=tz))
  }

  rownames_df <- as.numeric(rownames(aggregate_list[[3]]))
  rownames_subset <- as.character(subset(rownames_aggregate_list[[3]], rownames_df>=freq1&rownames_df<=freq2))

  colnames_df <- as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "),
                                     format= "%Y-%m-%d %H:%M:%S",
                                     tz=tz))

  colnames_subset <- as.character(hms::as_hms(subset(colnames_df, colnames_df>=time1&colnames_df<=time2)))

  new_df <- aggregate_list[[3]][rownames_subset,colnames_subset]

  # Getting the diversity profile data

  if (subset=="total"){
    sounddiv_profile <- hilldiv::div_profile(count = unlist(new_df),
                                             qvalues = qvalues,
                                             level = "alpha")

    if(output=="percentage"){
      sounddiv_profile <- (sounddiv_profile/(ncol(new_df)*nrow(new_df)))*100
    }

    else {}

  }

  else{

    if (subset=="day"){
      colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))
      daytime_df=aggregate_list[[3]][rownames_subset,colnames_day]

      sounddiv_profile <- hilldiv::div_profile(count=unlist(daytime_df),
                                               qvalues = qvalues,
                                               level="alpha")

      if(output=="percentage"){
        sounddiv_profile <- (sounddiv_profile/(ncol(daytime_df)*nrow(daytime_df)))*100
      }

      else {}
    }

    else{

      if (subset=="night"){
        colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<sunrise|colnames_df>sunset)))
        nighttime_df=aggregate_list[[3]][rownames_subset,colnames_night]

        sounddiv_profile <- hilldiv::div_profile(count=unlist(nighttime_df),
                                                 qvalues = qvalues,
                                                 level="alpha")

        if(output=="percentage"){
          sounddiv_profile <- (sounddiv_profile/(ncol(nighttime_df)*nrow(nighttime_df)))*100
        }

        else {}

      }

      else{

        if (subset=="dawn"){
          colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))
          dawntime_df=aggregate_list[[3]][rownames_subset,colnames_dawn]

          sounddiv_profile <- hilldiv::div_profile(count = unlist(dawntime_df),
                                                   qvalues = qvalues,
                                                   level="alpha")

          if(output=="percentage"){
            sounddiv_profile <- (sounddiv_profile/(ncol(dawntime_df)*nrow(dawntime_df)))*100
          }

          else {}
        }

        else{

          if (subset=="dusk"){
            colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunset-duskstart)&colnames_df<=(sunset+duskend))))
            dusktime_df=aggregate_list[[3]][rownames_subset,colnames_dusk]

            sounddiv_profile <- hilldiv::div_profile(count=unlist(dusktime_df),
                                                     qvalues = qvalues,
                                                     level="alpha")

            if(output=="percentage"){
              sounddiv_profile <- (sounddiv_profile/(ncol(dusktime_df)*nrow(dusktime_df)))*100
            }

            else {}

          }
        }
      }
    }
  }

  sounddiv_profile

}

########################################################

#' Visualize soundscape diversity profiles - only for internal use
#'
#' @param aggregate_list The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param qvalues A numeric vector of sequential q-values for which to compute the diversity profiles, generally ranging between 0-5. \emph{e.g.:} \code{seq(0, 5, 0.1)} yields a vector between 0 and 5 with an increment value of 0.1. The smaller the increment value, the smoother the diversity profiles.
#' @param subset The scale for which the soundscape diversity is computed. Options are 'total', 'day',
#' 'night', 'dawn', 'dusk' and 'tod' (time of day - for each unique time in the day).
#' @param minfreq A numeric value indicating the lower frequency limit for which to compute the soundscape diversity. If set to default, uses the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit for which to compute the soundscape diversity. If set to default, uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the latest time for which data exists in the dataframe.
#' @param date The first day of the recording period. Used for managing time-objects in R.
#' Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected, expressed in decimal degrees.
#' @param lon The longitude of the site at which the sound files were collected, expressed in decimal degrees.
#' @param twilight A character string of the twilight method to be used for sunrise and sunset as the boundary between day and night.
#' Options can be found in the \code{\link[photobiology]{day_night}} documentation.
#' @param dawnstart A numeric argument. If subset='dawn', used to determine the start of dawn. By default, dawn starts at sunrise. Expressed as the time in seconds before sunrise.
#' @param dawnend A numeric argument. If subset='dawn', used to determine the end of dawn. By default, dawn ends 1.5 hours after sunrise. Expressed as the time in seconds after sunrise.
#' @param duskstart A numeric argument. If subset='dusk', used to determine the start of dusk. By default, dusk starts 1.5 hours before sunset. Expressed as the time in seconds before sunset.
#' @param duskend A numeric argument. If subset='dusk', used to determine the end of dusk. By default, dusk ends at sunset. Expressed as the time in seconds after sunset.
#' @param display A character string. Indicates the output format, one of either "plot" or "table".
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#' @return A soundscape diversity profile plot or table.
sounddiv_profile_point_internal <- function(aggregate_list, qvalues=seq(0, 5, 0.1), subset="total",
                                            minfreq="default", maxfreq="default",
                                            mintime="default", maxtime="default",date, lat,
                                            lon, twilight="sunlight", dawnstart=0,
                                            dawnend=5400, duskstart=5400, duskend=0,
                                            display="plot", output="percentage"){

  # Data preparations

  tz <- lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")

  day <- as.POSIXct(strptime(paste(date, "00:00", sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))
  points <- as.data.frame(t(as.data.frame(c(lat, lon))))
  colnames(points) <- c("lat","lon")
  suntimes <- photobiology::day_night(date=day,
                                      tz=lubridate::tz(day),
                                      geocode = points,
                                      twilight = twilight,
                                      unit.out = "datetime")
  sunrise <- suntimes$sunrise
  sunset <- suntimes$sunset

  if (minfreq=="default"){
    freq1 <- min(as.numeric(rownames(aggregate_list[[3]])))
  }

  else{freq1 <- minfreq}

  if (maxfreq=="default"){
    freq2 <- max(as.numeric(rownames(aggregate_list[[3]])))
  }

  else{freq2 <- maxfreq}

  if (mintime=="default"){
    time1 <- min(as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "),
                                     format= "%Y-%m-%d %H:%M:%S",
                                     tz=tz)))
  }

  else{
    time1 <- as.POSIXct(strptime(paste(date, mintime, sep=" "),
                                 format= "%Y-%m-%d %H:%M:%S",
                                 tz=tz))
  }

  if (maxtime=="default"){
    time2 <- max(as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "),
                                     format= "%Y-%m-%d %H:%M:%S",
                                     tz=tz)))
  }

  else{
    time2 <- as.POSIXct(strptime(paste(date, maxtime, sep=" "),
                                 format= "%Y-%m-%d %H:%M:%S",
                                 tz=tz))
  }

  rownames_df <- as.numeric(rownames(aggregate_list[[3]]))
  rownames_subset <- as.character(subset(rownames_df, rownames_df>=freq1&rownames_df<=freq2))

  colnames_df <- as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "),
                                     format= "%Y-%m-%d %H:%M:%S",
                                     tz=tz))

  colnames_subset <- as.character(hms::as_hms(subset(colnames_df, colnames_df>=time1&colnames_df<=time2)))

  new_df <- aggregate_list[[3]][rownames_subset,colnames_subset]

  # Getting the diversity profile data

  if (subset=="total"){

    sounddiv_profile_point <- hilldiv::div_profile(count=unlist(new_df),
                                                   qvalues = seq(min(qvalues),
                                                                 max(qvalues),
                                                                 1),
                                                   level="alpha")

    if(output=="percentage"){
      sounddiv_profile_point <- (sounddiv_profile_point/(ncol(new_df)*nrow(new_df)))*100
    }

    else {}

  }

  else{

    if (subset=="day"){
      colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))
      daytime_df=aggregate_list[[3]][rownames_subset,colnames_day]

      sounddiv_profile_point <- hilldiv::div_profile(count=unlist(daytime_df),
                                                     qvalues = seq(min(qvalues),
                                                                   max(qvalues),
                                                                   1),
                                                     level="alpha")

      if(output=="percentage"){
        sounddiv_profile_point <- (sounddiv_profile_point/(ncol(daytime_df)*nrow(daytime_df)))*100
      }

      else {}
    }

    else{

      if (subset=="night"){
        colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<sunrise|colnames_df>sunset)))
        nighttime_df=aggregate_list[[3]][rownames_subset,colnames_night]

        sounddiv_profile_point <- hilldiv::div_profile(count=unlist(nighttime_df),
                                                       qvalues = seq(min(qvalues),
                                                                     max(qvalues),
                                                                     1),
                                                       level="alpha")
        if(output=="percentage"){
          sounddiv_profile_point <- (sounddiv_profile_point/(ncol(nighttime_df)*nrow(nighttime_df)))*100
        }

        else {}
      }

      else{

        if (subset=="dawn"){
          colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))
          dawntime_df=aggregate_list[[3]][rownames_subset,colnames_dawn]

          sounddiv_profile_point <- hilldiv::div_profile(count=unlist(dawntime_df),
                                                         qvalues = seq(min(qvalues),
                                                                       max(qvalues),
                                                                       1),
                                                         level="alpha")

          if(output=="percentage"){
            sounddiv_profile_point <- (sounddiv_profile_point/(ncol(dawntime_df)*nrow(dawntime_df)))*100
          }

          else {}
        }

        else{

          if (subset=="dusk"){
            colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunset-duskstart)&colnames_df<=(sunset+duskend))))
            dusktime_df=aggregate_list[[3]][rownames_subset,colnames_dusk]


            sounddiv_profile_point <- hilldiv::div_profile(count=unlist(dusktime_df),
                                                           qvalues = seq(min(qvalues),
                                                                         max(qvalues),
                                                                         1),
                                                           level="alpha")

            if(output=="percentage"){
              sounddiv_profile_point <- (sounddiv_profile_point/(ncol(dusktime_df)*nrow(dusktime_df)))*100
            }

            else {}
          }
        }
      }
    }
  }

  sounddiv_profile_point

}

####################



#' Compute and Visualize Soundscape Diversity Profiles
#'
#' @description Gives the soundscape diversity as a continuous function of the order of diversity q. Soundscape diversity profiles are a useful visualization tool to characterize the abundance of OSUs in the system, and assess the evenness.
#'
#' @param aggregate_list The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param qvalues A numeric vector of sequential q-values for which to compute the diversity profiles, generally ranging between 0-5. \emph{e.g.:} \code{seq(0, 5, 0.1)} yields a vector between 0 and 5 with an increment value of 0.1. The smaller the increment value, the smoother the diversity profiles.
#' @param subset The scale for which the soundscape diversity is computed. Options are 'total', 'day',
#' 'night', 'dawn', 'dusk' and 'tod' (time of day - for each unique time in the day).
#' @param minfreq A numeric value indicating the lower frequency limit for which to compute the soundscape diversity. If set to default, uses the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit for which to compute the soundscape diversity. If set to default, uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the latest time for which data exists in the dataframe.
#' @param date The first day of the recording period. Used for managing time-objects in R. Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected, expressed in decimal degrees.
#' @param lon The longitude of the site at which the sound files were collected, expressed in decimal degrees.
#' @param twilight A character string of the twilight method to be used for sunrise and sunset as the boundary between day and night.
#' Options can be found in the \code{\link[photobiology]{day_night}} documentation.
#' @param dawnstart A numeric argument. If subset='dawn', used to determine the start of dawn. By default, dawn starts at sunrise. Expressed as the time in seconds before sunrise.
#' @param dawnend A numeric argument. If subset='dawn', used to determine the end of dawn. By default, dawn ends 1.5 hours after sunrise. Expressed as the time in seconds after sunrise.
#' @param duskstart A numeric argument. If subset='dusk', used to determine the start of dusk. By default, dusk starts 1.5 hours before sunset. Expressed as the time in seconds before sunset.
#' @param duskend A numeric argument. If subset='dusk', used to determine the end of dusk. By default, dusk ends at sunset. Expressed as the time in seconds after sunset.
#' @param display A character string. Indicates the output format, one of either "plot" or "table".
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#' @return A soundscape diversity profile plot or table.
#' @export

sounddiv_prof <- function(aggregate_list,
                          qvalues=seq(0, 5, 0.1),
                          subset="total",
                          minfreq="default",
                          maxfreq="default",
                          mintime="default",
                          maxtime="default",
                          date,
                          lat,
                          lon,
                          twilight="sunlight",
                          dawnstart=0,
                          dawnend=5400,
                          duskstart=5400,
                          duskend=0,
                          display="plot",
                          output="percentage"){

  # 0. Providing binding for global variable

  rownames_aggregate_list <- NULL

  # Data preparations

  tz <- lutz::tz_lookup_coords(lat = lat,
                               lon = lon,
                               method = "accurate")

  day <- as.POSIXct(
    strptime(
      paste(date,
            "00:00",
            sep = " "),
      format = "%Y-%m-%d %H:%M",
      tz = tz))

  points <- data.frame(lon = lon, lat = lat)

  suntimes <- photobiology::day_night(date = day,
                                      tz = tz,
                                      geocode = points,
                                      twilight = twilight,
                                      unit.out = "datetime")
  sunrise <- as.POSIXct(suntimes$sunrise,
                        tz = tz,
                        format = "%Y-%m-%d %H:%M:%S")

  sunset <- as.POSIXct(suntimes$sunset,
                       tz = tz,
                       format = "%Y-%m-%d %H:%M:%S")

  if (minfreq=="default"){
    freq1 <- min(as.numeric(rownames(aggregate_list[[3]])))
  }

  else{freq1 <- minfreq}

  if (maxfreq=="default"){
    freq2 <- max(as.numeric(rownames(aggregate_list[[3]])))
  }

  else{freq2 <- maxfreq}

  if (mintime=="default"){
    time1 <- min(as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "),
                                     format= "%Y-%m-%d %H:%M:%S",
                                     tz=tz)))
  }

  else{
    time1 <- as.POSIXct(strptime(paste(date, mintime, sep=" "),
                                 format= "%Y-%m-%d %H:%M:%S",
                                 tz=tz))
  }

  if (maxtime=="default"){
    time2 <- max(as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "),
                                     format= "%Y-%m-%d %H:%M:%S",
                                     tz=tz)))
  }

  else{
    time2 <- as.POSIXct(strptime(paste(date, maxtime, sep=" "),
                                 format= "%Y-%m-%d %H:%M:%S",
                                 tz=tz))
  }

  rownames_df <- as.numeric(rownames(aggregate_list[[3]]))
  rownames_subset <- as.character(subset(rownames_aggregate_list[[3]], rownames_df>=freq1&rownames_df<=freq2))

  colnames_df <- as.POSIXct(strptime(paste(date, colnames(aggregate_list[[3]]), sep=" "),
                                     format= "%Y-%m-%d %H:%M:%S",
                                     tz=tz))

  colnames_subset <- as.character(hms::as_hms(subset(colnames_df, colnames_df>=time1&colnames_df<=time2)))

  new_df <- aggregate_list[[3]][rownames_subset,colnames_subset]

  # Getting the diversity profile data

  if (subset=="total"){
    sounddiv_profile <- hilldiv::div_profile(count = unlist(new_df),
                                             qvalues = qvalues,
                                             level = "alpha")

    if(output=="percentage"){
      sounddiv_profile <- (sounddiv_profile/(ncol(new_df)*nrow(new_df)))*100
    }

    else {}

  }

  else{

    if (subset=="day"){
      colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))
      daytime_df=aggregate_list[[3]][rownames_subset,colnames_day]

      sounddiv_profile <- hilldiv::div_profile(count=unlist(daytime_df),
                                               qvalues = qvalues,
                                               level="alpha")

      if(output=="percentage"){
        sounddiv_profile <- (sounddiv_profile/(ncol(daytime_df)*nrow(daytime_df)))*100
      }

      else {}
    }

    else{

      if (subset=="night"){
        colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<sunrise|colnames_df>sunset)))
        nighttime_df=aggregate_list[[3]][rownames_subset,colnames_night]

        sounddiv_profile <- hilldiv::div_profile(count=unlist(nighttime_df),
                                                 qvalues = qvalues,
                                                 level="alpha")

        if(output=="percentage"){
          sounddiv_profile <- (sounddiv_profile/(ncol(nighttime_df)*nrow(nighttime_df)))*100
        }

        else {}

      }

      else{

        if (subset=="dawn"){
          colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))
          dawntime_df=aggregate_list[[3]][rownames_subset,colnames_dawn]

          sounddiv_profile <- hilldiv::div_profile(count = unlist(dawntime_df),
                                                   qvalues = qvalues,
                                                   level="alpha")

          if(output=="percentage"){
            sounddiv_profile <- (sounddiv_profile/(ncol(dawntime_df)*nrow(dawntime_df)))*100
          }

          else {}
        }

        else{

          if (subset=="dusk"){
            colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunset-duskstart)&colnames_df<=(sunset+duskend))))
            dusktime_df=aggregate_list[[3]][rownames_subset,colnames_dusk]

            sounddiv_profile <- hilldiv::div_profile(count=unlist(dusktime_df),
                                                     qvalues = qvalues,
                                                     level="alpha")

            if(output=="percentage"){
              sounddiv_profile <- (sounddiv_profile/(ncol(dusktime_df)*nrow(dusktime_df)))*100
            }

            else {}

          }
        }
      }
    }
  }

  sounddiv_profile



  if (methods::is(object = aggregate_list[[3]], class2 = "data.frame")){
    sounddiv_profile <- sounddiv_profile_internal(aggregate_list=aggregate_list, qvalues=qvalues, subset=subset,
                                                  minfreq=minfreq, maxfreq=maxfreq,
                                                  mintime=mintime, maxtime=maxtime,
                                                  date=date, lat=lat, lon=lon,
                                                  twilight=twilight, dawnstart=dawnstart,
                                                  dawnend=dawnend, duskstart = duskstart,
                                                  duskend=duskend, display=display,
                                                  output=output)

    sounddiv_profile_point <- sounddiv_profile_point_internal(aggregate_list=aggregate_list,
                                                              qvalues=qvalues,
                                                              subset=subset,
                                                              minfreq=minfreq,
                                                              maxfreq=maxfreq,
                                                              mintime=mintime,
                                                              maxtime=maxtime,
                                                              date=date, lat=lat, lon=lon,
                                                              twilight=twilight,
                                                              dawnstart=dawnstart,
                                                              dawnend=dawnend,
                                                              duskstart=duskstart,
                                                              duskend=duskend,
                                                              display=display,
                                                              output=output)

    if (display=="plot"){

      sounddiv_profile <- as.data.frame(sounddiv_profile)
      sounddiv_profile$div_order <- rownames(sounddiv_profile)
      colnames(sounddiv_profile) <- c("eff_osus", "div_order")

      sounddiv_profile_point <- as.data.frame(sounddiv_profile_point)
      sounddiv_profile_point$div_order <- rownames(sounddiv_profile_point)
      colnames(sounddiv_profile_point) <- c("eff_osus", "div_order")


      sounddiv_plot <- ggplot2::ggplot(sounddiv_profile,
                                       ggplot2::aes(div_order, eff_osus, group=1)) +
        ggplot2::geom_line(color="#17a680", size=2)+
        ggplot2::geom_point(data=sounddiv_profile_point,shape=21, size=2, fill="white")+
        ggplot2::ylab(if (output=="percentage"){"Effective number of OSUs (%)"} else {"Effective number of OSUs"})+
        ggplot2::xlab("Order of diversity (q)")+
        ggplot2::scale_x_discrete(breaks = seq(min(sounddiv_profile$div_order),
                                               max(sounddiv_profile$div_order), 1))+
        ggplot2::theme_minimal()


      sounddiv_plot
    }

    else{

      if (display=="table"){
        sounddiv_profile <- as.data.frame(sounddiv_profile)
        sounddiv_profile$div_order <- rownames(sounddiv_profile)
        colnames(sounddiv_profile) <- c("eff_osus", "div_order")

        sounddiv_profile
      }
    }
  }

  else{

    if (methods::is(object = aggregate_list[[3]], class2 = "list")){

      sounddiv_profile <- vector("list", 0)
      sounddiv_profile_point <- vector("list", 0)

      for (i in 1:length(aggregate_list[[3]])){
        sounddiv_profile[[i]] <- sounddiv_profile_internal(aggregate_list=aggregate_list[[3]][[i]],
                                                           qvalues=qvalues,
                                                           subset=subset,
                                                           minfreq=minfreq,
                                                           maxfreq=maxfreq,
                                                           mintime=mintime,
                                                           maxtime=maxtime,
                                                           date=date,
                                                           lat=lat,
                                                           lon=lon,
                                                           twilight=twilight,
                                                           dawnstart=dawnstart,
                                                           dawnend=dawnend,
                                                           duskstart = duskstart,
                                                           duskend=duskend,
                                                           display=display,
                                                           output = output )

        sounddiv_profile_point[[i]] <- sounddiv_profile_point_internal(aggregate_list=aggregate_list[[3]][[i]],
                                                                 qvalues=qvalues,
                                                                 subset=subset,
                                                                 minfreq=minfreq,
                                                                 maxfreq=maxfreq,
                                                                 mintime=mintime,
                                                                 maxtime=maxtime,
                                                                 date=date,
                                                                 lat=lat,
                                                                 lon=lon,
                                                                 twilight=twilight,
                                                                 dawnstart=dawnstart,
                                                                 dawnend=dawnend,
                                                                 duskstart = duskstart,
                                                                 duskend=duskend,
                                                                 display=display,
                                                                 output=output)
      }

      names(sounddiv_profile) <- names(aggregate_list[[3]])
      names(sounddiv_profile_point) <- names(aggregate_list[[3]])


        for (i in 1:length(sounddiv_profile)){
          sounddiv_profile[[i]] <- as.data.frame(sounddiv_profile[[i]])
          sounddiv_profile[[i]]$factor <- names(sounddiv_profile)[i]
          sounddiv_profile[[i]]$div_order <- rownames(sounddiv_profile[[i]])
          colnames(sounddiv_profile[[i]]) <- c("eff_osus", "factor", "div_order")
        }

        sounddiv_profile <- dplyr::bind_rows(sounddiv_profile)

        for (i in 1:length(sounddiv_profile_point)){
          sounddiv_profile_point[[i]] <- as.data.frame(sounddiv_profile_point[[i]])
          sounddiv_profile_point[[i]]$factor <- names(sounddiv_profile_point)[i]
          sounddiv_profile_point[[i]]$div_order <- rownames(sounddiv_profile_point[[i]])
          colnames(sounddiv_profile_point[[i]]) <- c("eff_osus", "factor", "div_order")
        }

        sounddiv_profile_point <- dplyr::bind_rows(sounddiv_profile_point)


        if (display=="table"){
          sounddiv_profile
        }

        else{

          if (display=="plot"){

            sounddiv_plot <- ggplot2::ggplot(sounddiv_profile,
                                             ggplot2::aes(div_order, eff_osus, group=factor,
                                                          colour=factor)) +
              ggplot2::geom_line(size=2)+
              ggplot2::geom_point(data=sounddiv_profile_point,shape=21, size=3, fill="white", colour="black")+
              ggplot2::ylab(if (output=="percentage"){"Effective number of OSUs (%)"} else {"Effective number of OSUs"})+
              ggplot2::xlab("Order of diversity (q)")+
              ggplot2::scale_x_discrete(breaks = seq(min(sounddiv_profile$div_order),
                                                     max(sounddiv_profile$div_order), 1))+
              viridis::scale_color_viridis(discrete = TRUE)+
              ggplot2::theme_minimal()


            sounddiv_plot

          }
    }
  }
  }
}






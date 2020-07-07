# Functions for soundscape diversity calculations -------------------------------------

#' Estimate Soundscape Diversity using Hill Numbers
#'
#' @description For a set acoustic index, calculates the diversity of acoustically active Operational Sound Units (OSUs) in the soundscape. The q-parameter can be altered to modulate the diversity's sensitivity to abundance. The soundscape diversity metrics can be computed at various scales and resolutions. For instance, the user can explore the diversity for the whole soundscape, specify custom time and frequency limits, or use one of the built-in presets for diurnal-phase subsetting (day, night, dawn, dusk). Additionally, the user can track the change in soundscape diversity throughout the day. Finally, the soundscape diversity can be assessed for the entire frequency range, or per frequency-bin of user-defined width.
#'
#' \strong{Note:} Soundscape diversity metrics should not be used to make inference about the diversity of the real-world biological community unless verified using ground-truthing methods.
#'
#' For more information regarding the relationship between the soundscape diversity metrics, acoustic space use and real-world diversity, consult:
#'
#' Aide, T. M., Hernández-Serna, A., Campos-Cerqueira, M., Acevedo-Charry, O., & Deichmann, J. L. (2017). Species richness (of insects) drives the use of acoustic space in the tropics. Remote Sensing, 9(11), 1096.
#' \url{https://doi.org/10.3390/rs9111096}
#'
#' Burivalova, Z., Towsey, M., Boucher, T., Truskinger, A., Apelis, C., Roe, P., & Game, E. T. (2018). Using soundscapes to detect variable degrees of human influence on tropical forests in Papua New Guinea. Conservation Biology, 32(1), 205-215.
#' \url{https://doi.org/10.1111/cobi.12968}
#'
#'
#' @param df The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param type The scale for which the soundscape diversity is computed. Options are 'total', 'day',
#' 'night', 'dawn', 'dusk' and 'tod' (time of day - for each unique time in the day).
#' @param date The first day of the recording period. Used for managing time-objects in R.
#' Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected, expressed in decimal degrees.
#' @param lon The longitude of the site at which the sound files were collected, expressed in decimal degrees.
#' @param minfreq A numeric value indicating the lower frequency limit for which to compute the soundscape diversity. If set to default, uses the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit for which to compute the soundscape diversity. If set to default, uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the latest time for which data exists in the dataframe.
#' @param twilight A character string of the twilight method to be used for sunrise and sunset as the boundary between day and night.
#' Options can be found in the \code{\link[photobiology]{day_night}} documentation.
#' @param dawnstart A numeric argument. If type='dawn', used to determine the start of dawn. By default, dawn starts at sunrise. Expressed as the time in seconds before sunrise.
#' @param dawnend A numeric argument. If type='dawn', used to determine the end of dawn. By default, dawn ends 1.5 hours after sunrise. Expressed as the time in seconds after sunrise.
#' @param duskstart A numeric argument. If type='dusk', used to determine the start of dusk. By default, dusk starts 1.5 hours before sunset. Expressed as the time in seconds before sunset.
#' @param duskend A numeric argument. If type='dusk', used to determine the end of dusk. By default, dusk ends at sunset. Expressed as the time in seconds after sunset.
#' @param freqseq A logical operator (TRUE/FALSE). If set to FALSE, will compute the diversity for the entire frequency range of the soundscape. If set to TRUE, will compute the diversity per frequency-bin of user-defined width (number of bins determined by nbins argument).
#' @param nbins A numeric argument. If freqseq is set to TRUE, determines the number of the frequency-bins by which to divide the frequency range to compute the soundscape diversity.
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#' @return Depending on the chosen parameters, returns the soundscape diversity either a numeric value, a vector of values or a list of vectors of values.
#' @export
sounddiv=function(df, qvalue, type="total",date, lat, lon, minfreq="default", maxfreq="default", mintime="default", maxtime="default", twilight="sunlight", dawnstart=0, dawnend=5400, duskstart=5400, duskend=0, freqseq=FALSE ,nbins=10, output="percentage"){

  if (output=="raw"){multiplier <- 1} else{ if(output=="percentage"){multiplier <- 100} else{print("Error: invalid output argument - consult package documentation for options")}}

  tz <- lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")

  day <- as.POSIXct(strptime(paste(date, "00:00", sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))
  points <- as.data.frame(t(as.data.frame(c(lat, lon))))
  colnames(points) <- c("lat","lon")
  suntimes <- photobiology::day_night(date=day, tz=lubridate::tz(day),geocode = points,
                                      twilight = twilight, unit.out = "datetime")
  sunrise <- suntimes$sunrise
  sunset <- suntimes$sunset

  if (minfreq=="default"){
    freq1 <- min(as.numeric(rownames(df)))
  }

  else{freq1 <- minfreq}

  if (maxfreq=="default"){
    freq2 <- max(as.numeric(rownames(df)))
  }

  else{freq2 <- maxfreq}

  if (mintime=="default"){
    time1 <- min(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  }

  else{time1 <- as.POSIXct(strptime(paste(date, mintime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  if (maxtime=="default"){
    time2 <- max(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  }

  else{time2 <- as.POSIXct(strptime(paste(date, maxtime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  rownames_df <- as.numeric(rownames(df))
  rownames_subset <- as.character(subset(rownames_df, rownames_df>=freq1&rownames_df<=freq2))

  colnames_df <- as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))
  colnames_subset <- as.character(hms::as_hms(subset(colnames_df, colnames_df>=time1&colnames_df<=time2)))

  new_df <- df[rownames_subset,colnames_subset]

  if (freqseq=="FALSE"){

    if (type=="total"){

      soundscape_diversity <- hilldiv::hill_div(unlist(new_df), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(new_df)*nrow(new_df))} else{print("Error: invalid output argument")}}
      soundscape_diversity <- soundscape_diversity*multiplier
      soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
      soundscape_diversity

    }
    else{

      if (type=="tod"){

        soundscape_diversity <- c()

        for (i in 1:ncol(new_df)){
          soundscape_diversity[i] <- hilldiv::hill_div(unlist(new_df[[i]]), qvalue = qvalue)/ if (output=="raw"){1} else{if(output=="percentage"){length(new_df[[i]])} else{print("Error: invalid output argument")}}
        }

        soundscape_diversity <- soundscape_diversity*multiplier
        soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
        soundscape_diversity <- as.data.frame(soundscape_diversity)
        soundscape_diversity$time <- hms::as_hms((as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))))

        colnames(soundscape_diversity) <- c(paste0("soundscape_div", " (q=", qvalue, ")"), "time_of_day")
        soundscape_diversity
      }

      else{

        if (type=="day"){

          colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))
          daytime_df=df[rownames_subset,colnames_day]

          soundscape_diversity=hilldiv::hill_div(unlist(daytime_df), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(daytime_df)*nrow(daytime_df))} else{print("Error: invalid output argument")}}
          soundscape_diversity=soundscape_diversity*multiplier
          soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
          soundscape_diversity

        }

        else{

          if(type=="night"){

            colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<sunrise|colnames_df>sunset)))
            nighttime_df=df[rownames_subset,colnames_night]

            soundscape_diversity=hilldiv::hill_div(unlist(nighttime_df), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(nighttime_df)*nrow(nighttime_df))} else{print("Error: invalid output argument")}}
            soundscape_diversity=soundscape_diversity*multiplier
            soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
            soundscape_diversity
          }

          else{

            if (type=="dawn"){

              colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))
              dawntime_df=df[rownames_subset,colnames_dawn]

              soundscape_diversity=hilldiv::hill_div(unlist(dawntime_df), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(dawntime_df)*nrow(dawntime_df))} else{print("Error: invalid output argument")}}
              soundscape_diversity=soundscape_diversity*multiplier
              soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
              soundscape_diversity

            }

            else{

              if (type=="dusk"){

                colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunset-duskstart)&colnames_df<=(sunset+duskend))))
                dusktime_df=df[rownames_subset,colnames_dusk]

                soundscape_diversity=hilldiv::hill_div(unlist(dusktime_df), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(dusktime_df)*nrow(dusktime_df))} else{print("Error: invalid output argument")}}
                soundscape_diversity=soundscape_diversity*multiplier
                soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
                soundscape_diversity

              }

              else{print("Error: invalid type argument - please consult package documentation for options")}

            }
          }
        }
      }
    }
  }

  else{

    if (freqseq=="TRUE"){

      freq=seq(freq1, freq2, freq2/nbins)

      new_df$frequency=as.numeric(rownames(new_df))

      freq_list_1=vector("list", 0)
      freq_list_2=vector("list", 0)

      for (i in 1:length(freq)){
        freq_list_1[[i]]=subset(new_df, new_df$frequency<(i*(freq2/nbins)))
      }

      for (i in 1:length(freq)){
        freq_list_2[[i]]=subset(freq_list_1[[i]], freq_list_1[[i]]$frequency>((i-1)*(freq2/nbins)))
        freq_list_2[[i]]$frequency=NULL
      }


      if (type=="total"){

        soundscape_diversity=c()

        for (i in 1:length(freq_list_2)){
          soundscape_diversity[i]=hilldiv::hill_div(unlist(freq_list_2[[i]]), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_2[[i]])*nrow(freq_list_2[[i]]))} else{print("Error: invalid output argument")}}
        }

        soundscape_diversity=soundscape_diversity*multiplier
        soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
        soundscape_diversity <- as.data.frame(soundscape_diversity)
        soundscape_diversity$frequency_bin <- paste0(seq((freq1-freq1), (freq2-(freq2/nbins)), freq2/nbins), "-", seq(((freq1-freq1)+(freq2/nbins)), freq2, freq2/nbins), " Hz")

        colnames(soundscape_diversity) <- c(paste0("soundscape_div", " (q=", qvalue, ")"), "freq_interval")

        soundscape_diversity
      }


      else{

        if (type=="tod"){

          soundscape_diversity=vector("list", length(freq_list_2))

          for (i in 1:length(freq_list_2)){
            soundscape_diversity[[i]]=vector("list", ncol(freq_list_2[[i]]))
            for (j in 1:ncol(freq_list_2[[i]])){
              soundscape_diversity[[i]][[j]]=hilldiv::hill_div(unlist(freq_list_2[[i]][[j]]), qvalue = qvalue)/if (output=="raw"){1} else{if(output=="percentage"){length(freq_list_2[[i]][[j]])} else{print("Error: invalid output argument")}}
            }
            soundscape_diversity[[i]]=unlist(soundscape_diversity[[i]])
          }

          for (i in 1:length(soundscape_diversity)){
            soundscape_diversity[[i]]=soundscape_diversity[[i]]*multiplier
            soundscape_diversity[[i]][!is.finite(soundscape_diversity[[i]])] <- 0
            soundscape_diversity[[i]] <- as.data.frame(soundscape_diversity[[i]])
            soundscape_diversity[[i]]$time <- hms::as_hms((as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))))
            colnames(soundscape_diversity[[i]]) <- c(paste0("soundscape_div", " (q=", qvalue, ")"), "time_of_day")
          }

          names(soundscape_diversity) <- paste0(seq((freq1-freq1), (freq2-(freq2/nbins)), freq2/nbins), "-", seq(((freq1-freq1)+(freq2/nbins)), freq2, freq2/nbins), " Hz")

          soundscape_diversity

        }

        else{

          if (type=="day"){

            colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))

            freq_list_day=vector("list", 0)

            for (i in 1:length(freq_list_2)){
              freq_list_day[[i]]=freq_list_2[[i]][rownames_subset, colnames_day]
            }

            soundscape_diversity=c()

            for (i in 1:length(freq_list_day)){
              soundscape_diversity[i]=hilldiv::hill_div(unlist(freq_list_day[[i]]), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_day[[i]])*nrow(freq_list_day[[i]]))} else{print("Error: invalid output argument")}}
            }

            soundscape_diversity=soundscape_diversity*multiplier
            soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
            soundscape_diversity <- as.data.frame(soundscape_diversity)
            soundscape_diversity$frequency_bin <- paste0(seq((freq1-freq1), (freq2-(freq2/nbins)), freq2/nbins), "-", seq(((freq1-freq1)+(freq2/nbins)), freq2, freq2/nbins), " Hz")
            colnames(soundscape_diversity) <- c(paste0("soundscape_div_day", " (q=", qvalue, ")"), "freq_interval")
            soundscape_diversity

          }

          else{

            if (type=="night"){

              colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<=sunrise|colnames_df>=sunset)))

              freq_list_night=vector("list", 0)

              for (i in 1:length(freq_list_2)){
                freq_list_night[[i]]=freq_list_2[[i]][, colnames_night]
              }

              soundscape_diversity=c()

              for (i in 1:length(freq_list_night)){
                soundscape_diversity[i]=hilldiv::hill_div(unlist(freq_list_night[[i]]), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_night[[i]])*nrow(freq_list_night[[i]]))} else{print("Error: invalid output argument")}}
              }

              soundscape_diversity=soundscape_diversity*multiplier
              soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
              soundscape_diversity <- as.data.frame(soundscape_diversity)
              soundscape_diversity$frequency_bin <- paste0(seq((freq1-freq1), (freq2-(freq2/nbins)), freq2/nbins), "-", seq(((freq1-freq1)+(freq2/nbins)), freq2, freq2/nbins), " Hz")
              colnames(soundscape_diversity) <- c(paste0("soundscape_div_night", " (q=", qvalue, ")"), "freq_interval")
              soundscape_diversity

            }

            else{

              if (type=="dawn"){

                colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))

                freq_list_dawn=vector("list", 0)

                for (i in 1:length(freq_list_2)){
                  freq_list_dawn[[i]]=freq_list_2[[i]][, colnames_dawn]
                }

                soundscape_diversity=c()

                for (i in 1:length(freq_list_dawn)){
                  soundscape_diversity[i]=hilldiv::hill_div(unlist(freq_list_dawn[[i]]), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_dawn[[i]])*nrow(freq_list_dawn[[i]]))} else{print("Error: invalid output argument")}}
                }

                soundscape_diversity=soundscape_diversity*multiplier
                soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
                soundscape_diversity <- as.data.frame(soundscape_diversity)
                soundscape_diversity$frequency_bin <- paste0(seq((freq1-freq1), (freq2-(freq2/nbins)), freq2/nbins), "-", seq(((freq1-freq1)+(freq2/nbins)), freq2, freq2/nbins), " Hz")
                colnames(soundscape_diversity) <- c(paste0("soundscape_div_dawn", " (q=", qvalue, ")"), "freq_interval")
                soundscape_diversity

              }


              else{

                if (type=="dusk"){

                  colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>(sunset-duskstart)&colnames_df<=(sunset+duskend))))

                  freq_list_dusk=vector("list", 0)

                  for (i in 1:length(freq_list_2)){
                    freq_list_dusk[[i]]=freq_list_2[[i]][, colnames_dusk]
                  }


                  soundscape_diversity=c()

                  for (i in 1:length(freq_list_dusk)){
                    soundscape_diversity[i]=hilldiv::hill_div(unlist(freq_list_dusk[[i]]), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_dusk[[i]])*nrow(freq_list_dusk[[i]]))} else{print("Error: invalid output argument")}}
                  }

                  soundscape_diversity=soundscape_diversity*multiplier
                  soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
                  soundscape_diversity <- as.data.frame(soundscape_diversity)
                  soundscape_diversity$frequency_bin <- paste0(seq((freq1-freq1), (freq2-(freq2/nbins)), freq2/nbins), "-", seq(((freq1-freq1)+(freq2/nbins)), freq2, freq2/nbins), " Hz")
                  colnames(soundscape_diversity) <- c(paste0("soundscape_div_dusk", " (q=", qvalue, ")"), "freq_interval")
                  soundscape_diversity

                }

                else{print("Error: invalid type argument - please consult package documentation for options")}

              }
            }
          }
        }
      }
    }

    else{print("Error: invalid freqseq argument - please consult package documentation for options")}

  }
}


#' Estimate Soundscape Diversity using Hill Numbers
#'
#' @description Same as soundscape_Hill, but modified for simpler internal use by sounddiv_by_time
#'
#' @param df The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param qvalue A parameter which modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A positive integer or decimal number (>=0), most commonly between 0-3. A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective species for the Simpson index.
#' @param type The scale for which the soundscape diversity is computed. Options are 'total', 'day',
#' 'night', 'dawn', 'dusk' and 'tod' (time of day - for each unique time in the day).
#' @param date The first day of the recording period. Used for managing time-objects in R.
#' Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected.
#' @param lon The longitude of the site at which the sound files were collected.
#' @param minfreq The lower frequency limit for which to compute the soundscape diversity, expressed as a numeric value.
#' Defaults to the lowest frequency for which data exists in the dataframe.
#' @param maxfreq The upper frequency limit for which to compute the soundscape diversity, expressed as a numeric value.
#' Defaults to the highest frequency for which data exists in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". Defaults to the
#' earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". Defaults to the
#'  latest time for which data exists in the dataframe.
#' @param twilight A character string of the twilight method to be used for sunrise and sunset as the boundary between day and night.
#' Options can be found in the \code{\link[photobiology]{day_night}} documentation.
#' @param dawnstart If type='dawn', used to determine the start of dawn. By default, dawn starts at sunrise. Expressed as the
#' time in seconds before sunrise.
#' @param dawnend If type='dawn', used to determine the end of dawn. By default, dawn ends 1.5 hours after sunrise. Expressed as the
#' time in seconds after sunrise.
#' @param duskstart If type='dusk', used to determine the start of dusk. By default, dusk starts 1.5 hours before sunset. Expressed as the
#' time in seconds before sunset.
#' @param duskend If type='dusk', used to determine the end of dusk. By default, dusk ends at sunset. Expressed as the
#' time in seconds after sunset.
#' @param freqseq One of either TRUE or FALSE. If set to FALSE, will compute the diversity for the entire frequency range of the soundscape.
#' If set to TRUE, will compute the diversity per frequency-bin of user-defined width.
#' @param nbins If freqseq is set to TRUE, determines the number of the frequency-bins by which to divide the frequency range to compute the
#' soundscape diversity, expressed as a numeric constant.
#' @param output The format in which the soundscape diversity is expressed. Options are "percentage"
#' (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw"
#' (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#' @return Depending on the chosen parameters, returns the soundscape diversity either a numeric value, a vector of values or a list of vectors of values.
sounddiv_internal=function(df, qvalue, type="total",date, lat, lon, minfreq="default", maxfreq="default", mintime="default", maxtime="default", twilight="sunlight", dawnstart=0, dawnend=5400, duskstart=5400, duskend=0, freqseq=FALSE ,nbins=10, output="percentage"){

  if (output=="raw"){multiplier <- 1} else{ if(output=="percentage"){multiplier <- 100} else{print("Error: invalid output argument - consult package documentation for options")}}

  tz <- lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")

  day <- as.POSIXct(strptime(paste(date, "00:00", sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))
  points <- as.data.frame(t(as.data.frame(c(lat, lon))))
  colnames(points) <- c("lat","lon")
  suntimes <- photobiology::day_night(date=day, tz=lubridate::tz(day),geocode = points,
                                      twilight = twilight, unit.out = "datetime")
  sunrise <- suntimes$sunrise
  sunset <- suntimes$sunset

  if (minfreq=="default"){
    freq1 <- min(as.numeric(rownames(df)))
  }

  else{freq1 <- minfreq}

  if (maxfreq=="default"){
    freq2 <- max(as.numeric(rownames(df)))
  }

  else{freq2 <- maxfreq}

  if (mintime=="default"){
    time1 <- min(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  }

  else{time1 <- as.POSIXct(strptime(paste(date, mintime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  if (maxtime=="default"){
    time2 <- max(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  }

  else{time2 <- as.POSIXct(strptime(paste(date, maxtime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  rownames_df <- as.numeric(rownames(df))
  rownames_subset <- as.character(subset(rownames_df, rownames_df>=freq1&rownames_df<=freq2))

  colnames_df <- as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))
  colnames_subset <- as.character(hms::as_hms(subset(colnames_df, colnames_df>=time1&colnames_df<=time2)))

  new_df <- df[rownames_subset,colnames_subset]

  if (freqseq=="FALSE"){

    if (type=="total"){

      soundscape_diversity <- hilldiv::hill_div(unlist(new_df), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(new_df)*nrow(new_df))} else{print("Error: invalid output argument")}}
      soundscape_diversity <- soundscape_diversity*multiplier
      soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
      soundscape_diversity

    }
    else{

      if (type=="tod"){

        soundscape_diversity <- c()

        for (i in 1:ncol(new_df)){
          soundscape_diversity[i] <- hilldiv::hill_div(unlist(new_df[[i]]), qvalue = qvalue)/ if (output=="raw"){1} else{if(output=="percentage"){length(new_df[[i]])} else{print("Error: invalid output argument")}}
        }

        soundscape_diversity <- soundscape_diversity*multiplier
        soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
        soundscape_diversity
      }

      else{

        if (type=="day"){

          colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))
          daytime_df=df[rownames_subset,colnames_day]

          soundscape_diversity=hilldiv::hill_div(unlist(daytime_df), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(daytime_df)*nrow(daytime_df))} else{print("Error: invalid output argument")}}
          soundscape_diversity=soundscape_diversity*multiplier
          soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
          soundscape_diversity

        }

        else{

          if(type=="night"){

            colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<sunrise|colnames_df>sunset)))
            nighttime_df=df[rownames_subset,colnames_night]

            soundscape_diversity=hilldiv::hill_div(unlist(nighttime_df), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(nighttime_df)*nrow(nighttime_df))} else{print("Error: invalid output argument")}}
            soundscape_diversity=soundscape_diversity*multiplier
            soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
            soundscape_diversity
          }

          else{

            if (type=="dawn"){

              colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))
              dawntime_df=df[rownames_subset,colnames_dawn]

              soundscape_diversity=hilldiv::hill_div(unlist(dawntime_df), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(dawntime_df)*nrow(dawntime_df))} else{print("Error: invalid output argument")}}
              soundscape_diversity=soundscape_diversity*multiplier
              soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
              soundscape_diversity

            }

            else{

              if (type=="dusk"){

                colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunset-duskstart)&colnames_df<=(sunset+duskend))))
                dusktime_df=df[rownames_subset,colnames_dusk]

                soundscape_diversity=hilldiv::hill_div(unlist(dusktime_df), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(dusktime_df)*nrow(dusktime_df))} else{print("Error: invalid output argument")}}
                soundscape_diversity=soundscape_diversity*multiplier
                soundscape_diversity[!is.finite(soundscape_diversity)] <- 0
                soundscape_diversity

              }

              else{print("Error: invalid type argument - please consult package documentation for options")}

            }
          }
        }
      }
    }
  }

  else{

    if (freqseq=="TRUE"){

      freq=seq(freq1, freq2, freq2/nbins)

      new_df$frequency=as.numeric(rownames(new_df))

      freq_list_1=vector("list", 0)
      freq_list_2=vector("list", 0)

      for (i in 1:length(freq)){
        freq_list_1[[i]]=subset(new_df, new_df$frequency<(i*(freq2/nbins)))
      }

      for (i in 1:length(freq)){
        freq_list_2[[i]]=subset(freq_list_1[[i]], freq_list_1[[i]]$frequency>((i-1)*(freq2/nbins)))
        freq_list_2[[i]]$frequency=NULL
      }


      if (type=="total"){

        soundscape_diversity=c()

        for (i in 1:length(freq_list_2)){
          soundscape_diversity[i]=hilldiv::hill_div(unlist(freq_list_2[[i]]), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_2[[i]])*nrow(freq_list_2[[i]]))} else{print("Error: invalid output argument")}}
        }

        soundscape_diversity=soundscape_diversity*multiplier
        soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

        soundscape_diversity
      }


      else{

        if (type=="tod"){

          soundscape_diversity=vector("list", length(freq_list_2))

          for (i in 1:length(freq_list_2)){
            soundscape_diversity[[i]]=vector("list", ncol(freq_list_2[[i]]))
            for (j in 1:ncol(freq_list_2[[i]])){
              soundscape_diversity[[i]][[j]]=hilldiv::hill_div(unlist(freq_list_2[[i]][[j]]), qvalue = qvalue)/if (output=="raw"){1} else{if(output=="percentage"){length(freq_list_2[[i]][[j]])} else{print("Error: invalid output argument")}}
            }
            soundscape_diversity[[i]]=unlist(soundscape_diversity[[i]])
          }

          for (i in 1:length(soundscape_diversity)){
            soundscape_diversity[[i]]=soundscape_diversity[[i]]*multiplier
            soundscape_diversity[[i]][!is.finite(soundscape_diversity[[i]])] <- 0
          }

            soundscape_diversity

          }

        else{

          if (type=="day"){

            colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))

            freq_list_day=vector("list", 0)

            for (i in 1:length(freq_list_2)){
              freq_list_day[[i]]=freq_list_2[[i]][rownames_subset, colnames_day]
            }

            soundscape_diversity=c()

            for (i in 1:length(freq_list_day)){
              soundscape_diversity[i]=hilldiv::hill_div(unlist(freq_list_day[[i]]), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_day[[i]])*nrow(freq_list_day[[i]]))} else{print("Error: invalid output argument")}}
            }

            soundscape_diversity=soundscape_diversity*multiplier
            soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

            soundscape_diversity

          }

          else{

            if (type=="night"){

              colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<=sunrise|colnames_df>=sunset)))

              freq_list_night=vector("list", 0)

              for (i in 1:length(freq_list_2)){
                freq_list_night[[i]]=freq_list_2[[i]][, colnames_night]
              }

              soundscape_diversity=c()

              for (i in 1:length(freq_list_night)){
                soundscape_diversity[i]=hilldiv::hill_div(unlist(freq_list_night[[i]]), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_night[[i]])*nrow(freq_list_night[[i]]))} else{print("Error: invalid output argument")}}
              }

              soundscape_diversity=soundscape_diversity*multiplier
              soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

              soundscape_diversity

            }

            else{

              if (type=="dawn"){

                colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))

                freq_list_dawn=vector("list", 0)

                for (i in 1:length(freq_list_2)){
                  freq_list_dawn[[i]]=freq_list_2[[i]][, colnames_dawn]
                }

                soundscape_diversity=c()

                for (i in 1:length(freq_list_dawn)){
                  soundscape_diversity[i]=hilldiv::hill_div(unlist(freq_list_dawn[[i]]), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_dawn[[i]])*nrow(freq_list_dawn[[i]]))} else{print("Error: invalid output argument")}}
                }

                soundscape_diversity=soundscape_diversity*multiplier
                soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

                soundscape_diversity

              }


              else{

                if (type=="dusk"){

                  colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>(sunset-duskstart)&colnames_df<=(sunset+duskend))))

                  freq_list_dusk=vector("list", 0)

                  for (i in 1:length(freq_list_2)){
                    freq_list_dusk[[i]]=freq_list_2[[i]][, colnames_dusk]
                  }


                  soundscape_diversity=c()

                  for (i in 1:length(freq_list_dusk)){
                    soundscape_diversity[i]=hilldiv::hill_div(unlist(freq_list_dusk[[i]]), qvalue=qvalue)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_dusk[[i]])*nrow(freq_list_dusk[[i]]))} else{print("Error: invalid output argument")}}
                  }

                  soundscape_diversity=soundscape_diversity*multiplier
                  soundscape_diversity[!is.finite(soundscape_diversity)] <- 0

                  soundscape_diversity

                }

                else{print("Error: invalid type argument - please consult package documentation for options")}

              }
            }
          }
        }
      }
    }

    else{print("Error: invalid freqseq argument - please consult package documentation for options")}

  }
}


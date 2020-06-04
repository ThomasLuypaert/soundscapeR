# Functions for soundscape diversity calculations -------------------------------------

  #2) Diversity metrics

    #2.1: Soundscape richness (Proportion of acoustically active time-frequency bins)

#' Estimate Soundscape Richness
#'
#' @description For a set acoustic index, calculates the richness of acoustically active (value>0)
#' time-frequency bins in the soundscape, also expressed as the 'acoustic space use' or
#' 'soundscape saturation'. The soundscape richness can be computed at various scales and resolutions.
#' For instance, the user can explore the richness for the whole soundscape, specify custom time and frequency limits,
#' or use one of the built-in presets for diurnal-phase subsetting (day, night, dawn, dusk).
#' Additionally, the user can track the change in soundscape richness throughout the day. Finally,
#' the richness can be assessed for the entire frequency range, or per frequency-bin of user-defined width.
#'
#' Note: the soundscape richness metric should not be used to make inference about the richness of the
#' real-world biological community unless verified using ground-truthing methods.
#'
#' For more information regarding the relationship between the soundscape richness and real-world diversity, consult:
#'
#' Aide, T. M., Hernández-Serna, A., Campos-Cerqueira, M., Acevedo-Charry, O., & Deichmann, J. L. (2017). Species richness (of insects) drives the use of acoustic space in the tropics. Remote Sensing, 9(11), 1096.
#' \url{https://doi.org/10.3390/rs9111096}
#'
#' Burivalova, Z., Towsey, M., Boucher, T., Truskinger, A., Apelis, C., Roe, P., & Game, E. T. (2018). Using soundscapes to detect variable degrees of human influence on tropical forests in Papua New Guinea. Conservation Biology, 32(1), 205-215.
#' \url{https://doi.org/10.1111/cobi.12968}
#'
#'
#' @param df The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param type The scale for which the soundscape richness is computed. Options are 'total', 'day',
#' 'night', 'dawn', 'dusk' and 'tod' (time of day - for each unique time in the day).
#' @param date The first day of the recording period. Used for managing time-objects in R. \cr
#' Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected.
#' @param lon The longitude of the site at which the sound files were collected.
#' @param minfreq The lower frequency limit for which to compute the soundscape richness, expressed as a numeric value.
#' Defaults to the lowest frequency for which data exists in the dataframe.
#' @param maxfreq The upper frequency limit for which to compute the soundscape richness, expressed as a numeric value.
#' Defaults to the highest frequency for which data exists in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape richness, formatted as "HH:MM:SS". Defaults to the
#' earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape richness, formatted as "HH:MM:SS". Defaults to the
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
#' @param freqseq One of either TRUE or FALSE. If set to FALSE, will compute the richness for the entire frequency range of the soundscape.
#' If set to TRUE, will compute the richness per frequency-bin of user-defined width.
#' @param nbins If freqseq is set to TRUE, determines the number of the frequency-bins by which to divide the frequency range to compute the
#' soundscape richness, expressed as a numeric constant.
#' @param output The format in which the soundscape richness is expressed. Options are "percentage"
#' (the fraction between the observed soundscape richness and the maximum possible soundscape richness), or "raw"
#' (the number of acoustically active time-frequency bins in the soundscape). Defaults to "percentage".
#'
#' @return Depending on the chosen parameters, returns the soundscape richness either a numeric value, a vector of values or a list of vectors of values.
#' @export
soundscape_richness=function(df, type="total",date, lat, lon, minfreq="default", maxfreq="default", mintime="default", maxtime="default", twilight="sunlight", dawnstart=0, dawnend=5400, duskstart=5400, duskend=0, freqseq=FALSE ,nbins=10, output="percentage"){

  if (output=="raw"){multiplier=1} else{ if(output=="percentage"){multiplier=100} else{print("Error: invalid output argument - consult package documentation for options")}}

  tz=lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")

  day=as.POSIXct(strptime(paste(date, "00:00", sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))
  points=as.data.frame(t(as.data.frame(c(lat, lon))))
  colnames(points)=c("lat","lon")
  suntimes=photobiology::day_night(date=day, tz=lubridate::tz(day),geocode = points, twilight = twilight, unit.out = "datetime")
  sunrise=suntimes$sunrise
  sunset=suntimes$sunset

  if (minfreq=="default"){
    freq1=min(as.numeric(rownames(df)))
  }

  else{freq1=minfreq}

  if (maxfreq=="default"){
    freq2=max(as.numeric(rownames(df)))
  }

  else{freq2=maxfreq}

  if (mintime=="default"){
    time1=min(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  }

  else{time1=as.POSIXct(strptime(paste(date, mintime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  if (maxtime=="default"){
    time2=max(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  }

  else{time2=as.POSIXct(strptime(paste(date, maxtime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  rownames_df=as.numeric(rownames(df))
  rownames_subset=as.character(subset(rownames_df, rownames_df>=freq1&rownames_df<=freq2))

  colnames_df=as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))
  colnames_subset=as.character(hms::as_hms(subset(colnames_df, colnames_df>=time1&colnames_df<=time2)))

  new_df=df[rownames_subset,colnames_subset]

  if (freqseq=="FALSE"){

    if (type=="total"){

      richness=hilldiv::hill_div(subset(unlist(new_df), unlist(new_df)>0), qvalue=0)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(new_df)*nrow(new_df))} else{print("Error: invalid output argument")}}
      richness=richness*multiplier
      richness

    }
    else{

      if (type=="tod"){

        richness=c()

        for (i in 1:ncol(new_df)){
          richness[i]=hilldiv::hill_div(subset(unlist(new_df[[i]]), unlist(new_df[[i]])>0), qvalue = 0)/ if (output=="raw"){1} else{if(output=="percentage"){length(new_df[[i]])} else{print("Error: invalid output argument")}}
        }

        richness=richness*multiplier
        richness

      }

      else{

        if (type=="day"){

          colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))
          daytime_df=df[rownames_subset,colnames_day]

          richness=hilldiv::hill_div(subset(unlist(daytime_df), unlist(daytime_df)>0), qvalue=0)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(daytime_df)*nrow(daytime_df))} else{print("Error: invalid output argument")}}
          richness=richness*multiplier
          richness

        }

        else{

          if(type=="night"){

            colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<sunrise|colnames_df>sunset)))
            nighttime_df=df[rownames_subset,colnames_night]

            richness=hilldiv::hill_div(subset(unlist(nighttime_df), unlist(nighttime_df)>0), qvalue=0)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(nighttime_df)*nrow(nighttime_df))} else{print("Error: invalid output argument")}}
            richness=richness*multiplier
            richness
          }

          else{

            if (type=="dawn"){

              colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))
              dawntime_df=df[rownames_subset,colnames_dawn]

              richness=hilldiv::hill_div(subset(unlist(dawntime_df), unlist(dawntime_df)>0), qvalue=0)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(dawntime_df)*nrow(dawntime_df))} else{print("Error: invalid output argument")}}
              richness=richness*multiplier
              richness

            }

            else{

              if (type=="dusk"){

                colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunset-duskstart)&colnames_df<=(sunset+duskend))))
                dusktime_df=df[rownames_subset,colnames_dusk]

                richness=hilldiv::hill_div(subset(unlist(dusktime_df), unlist(dusktime_df)>0), qvalue=0)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(dusktime_df)*nrow(dusktime_df))} else{print("Error: invalid output argument")}}
                richness=richness*multiplier
                richness

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

        richness=c()

        for (i in 1:length(freq_list_2)){
          richness[i]=hilldiv::hill_div(subset(unlist(freq_list_2[[i]]), unlist(freq_list_2[[i]])>0), qvalue=0)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_2[[i]])*nrow(freq_list_2[[i]]))} else{print("Error: invalid output argument")}}
        }

        richness=richness*multiplier
        richness
      }


      else{

        if (type=="tod"){

          richness=vector("list", length(freq_list_2))

          for (i in 1:length(freq_list_2)){
            richness[[i]]=vector("list", ncol(freq_list_2[[i]]))
            for (j in 1:ncol(freq_list_2[[i]])){
              richness[[i]][[j]]=hilldiv::hill_div(subset(unlist(freq_list_2[[i]][[j]]), unlist(freq_list_2[[i]][[j]])>0), qvalue = 0)/if (output=="raw"){1} else{if(output=="percentage"){length(freq_list_2[[i]][[j]])} else{print("Error: invalid output argument")}}
            }
            richness[[i]]=unlist(richness[[i]])
          }

          for (i in 1:length(richness)){
            richness[[i]]=richness[[i]]*multiplier
          }

          richness

        }

        else{

          if (type=="day"){

            colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))

            freq_list_day=vector("list", 0)

            for (i in 1:length(freq_list_2)){
              freq_list_day[[i]]=freq_list_2[[i]][rownames_subset, colnames_day]
            }

            richness=c()

            for (i in 1:length(freq_list_day)){
              richness[i]=hilldiv::hill_div(subset(unlist(freq_list_day[[i]]), unlist(freq_list_day[[i]])>0), qvalue=0)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_day[[i]])*nrow(freq_list_day[[i]]))} else{print("Error: invalid output argument")}}
            }

            richness=richness*multiplier
            richness

          }

          else{

            if (type=="night"){

              colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<=sunrise|colnames_df>=sunset)))

              freq_list_night=vector("list", 0)

              for (i in 1:length(freq_list_2)){
                freq_list_night[[i]]=freq_list_2[[i]][, colnames_night]
              }

              richness=c()

              for (i in 1:length(freq_list_night)){
                richness[i]=hilldiv::hill_div(subset(unlist(freq_list_night[[i]]), unlist(freq_list_night[[i]])>0), qvalue=0)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_night[[i]])*nrow(freq_list_night[[i]]))} else{print("Error: invalid output argument")}}
              }

              richness=richness*multiplier
              richness

            }

            else{

              if (type=="dawn"){

                colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))

                freq_list_dawn=vector("list", 0)

                for (i in 1:length(freq_list_2)){
                  freq_list_dawn[[i]]=freq_list_2[[i]][, colnames_dawn]
                }

                richness=c()

                for (i in 1:length(freq_list_dawn)){
                  richness[i]=hilldiv::hill_div(subset(unlist(freq_list_dawn[[i]]), unlist(freq_list_dawn[[i]])>0), qvalue=0)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_dawn[[i]])*nrow(freq_list_dawn[[i]]))} else{print("Error: invalid output argument")}}
                }

                richness=richness*multiplier
                richness

              }


              else{

                if (type=="dusk"){

                  colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>(sunset-duskstart)&colnames_df<=(sunset+duskend))))

                  freq_list_dusk=vector("list", 0)

                  for (i in 1:length(freq_list_2)){
                    freq_list_dusk[[i]]=freq_list_2[[i]][, colnames_dusk]
                  }


                  richness=c()

                  for (i in 1:length(freq_list_dusk)){
                    richness[i]=hilldiv::hill_div(subset(unlist(freq_list_dusk[[i]]), unlist(freq_list_dusk[[i]])>0), qvalue=0)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_dusk[[i]])*nrow(freq_list_dusk[[i]]))} else{print("Error: invalid output argument")}}
                  }

                  richness=richness*multiplier
                  richness

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

#2.2: Soundscape diversity (Shannon diversity of acoustically active time-frequency bins)

#' Estimate Soundscape Diversity
#'
#' @description Operates in a similar way as \code{\link{soundscape_richness}}. For a set acoustic index,
#' calculates the Shannon-diversity of time-frequency bins in the soundscape. In this case, each time-frequency
#' bin is treated as a "species", and the proportion of acoustically active recordings
#' for that bin the relative abundance. The soundscape diversity can be computed at various scales and resolutions.
#' For instance, the user can explore the diversity for the whole soundscape, specify custom time and frequency limits,
#' or use one of the built-in presets for diurnal-phase subsetting (day, night, dawn, dusk).
#' Additionally, the user can track the change in soundscape diversity throughout the day. Finally,
#' the diversity can be assessed for the entire frequency range, or per frequency-bin of user-defined width.
#'
#' Note: the soundscape diversity metric should not be used to make inference about the diversity of the
#' real-world biological community unless verified using ground-truthing methods.
#'
#'
#' @param df The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param type The scale for which the soundscape diversity is computed. Options are 'total', 'day',
#' 'night', 'dawn', 'dusk' and 'tod' (time of day - for each unique time in the day).
#' @param date The first day of the recording period. Used for managing time-objects in R. \cr
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
#' @param nbins If freqseq is set to TRUE, determines the number of the frequency-bins by which divide the frequency range to compute the
#' soundscape diversity, expressed as a numeric constant.
#' @param output The format in which the soundscape diversity is expressed. Options are "percentage"
#' (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw"
#' (The raw Shannon-diversity of time-frequency bins). Defaults to "percentage".
#'
#' @return Depending on the chosen parameters, returns the soundscape diversity either a numeric value, a vector of values or a list of vectors of values.
#' @keywords internal
soundscape_diversity=function(df, type="total",date, lat, lon, minfreq="default", maxfreq="default", mintime="default", maxtime="default", twilight="sunlight", dawnstart=0, dawnend=5400, duskstart=5400, duskend=0, freqseq=FALSE ,nbins=10, output="percentage"){

  if (output=="raw"){multiplier=1} else{ if(output=="percentage"){multiplier=100} else{print("Error: invalid output argument - consult package documentation for options")}}

  tz=lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")

  day=as.POSIXct(strptime(paste(date, "00:00", sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))
  points=as.data.frame(t(as.data.frame(c(lat, lon))))
  colnames(points)=c("lat","lon")
  suntimes=photobiology::day_night(date=day, tz=lubridate::tz(day),geocode = points, twilight = twilight, unit.out = "datetime")
  sunrise=suntimes$sunrise
  sunset=suntimes$sunset

  if (minfreq=="default"){
    freq1=min(as.numeric(rownames(df)))
  }

  else{freq1=minfreq}

  if (maxfreq=="default"){
    freq2=max(as.numeric(rownames(df)))
  }

  else{freq2=maxfreq}

  if (mintime=="default"){
    time1=min(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  }

  else{time1=as.POSIXct(strptime(paste(date, mintime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  if (maxtime=="default"){
    time2=max(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  }

  else{time2=as.POSIXct(strptime(paste(date, maxtime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  rownames_df=as.numeric(rownames(df))
  rownames_subset=as.character(subset(rownames_df, rownames_df>=freq1&rownames_df<=freq2))

  colnames_df=as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))
  colnames_subset=as.character(hms::as_hms(subset(colnames_df, colnames_df>=time1&colnames_df<=time2)))

  new_df=df[rownames_subset,colnames_subset]

  if (freqseq=="FALSE"){

    if (type=="total"){

      diversity=(hilldiv::hill_div(unlist(new_df), qvalue=1)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(new_df)*nrow(new_df))} else{print("Error: invalid output argument")}})
      diversity=diversity*multiplier
      diversity=ifelse(is.na(diversity), 0, diversity)
      diversity

    }

    else{

      if (type=="tod"){

        diversity=c()

        for (i in 1:ncol(new_df)){
          diversity[i]=(hilldiv::hill_div(new_df[[i]], qvalue = 1)/if (output=="raw"){1} else{if(output=="percentage"){length(new_df[[i]])} else{print("Error: invalid output argument")}})
          diversity[i]=diversity[i]*multiplier
          diversity[i]=ifelse(is.na(diversity[i]), 0, diversity[i])
        }

        diversity

      }

      else{

        if (type=="day"){

          colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))
          daytime_df=df[rownames_subset,colnames_day]


          diversity=(hilldiv::hill_div(unlist(daytime_df), qvalue=1)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(daytime_df)*nrow(daytime_df))} else{print("Error: invalid output argument")}})
          diversity=diversity*multiplier
          diversity=ifelse(is.na(diversity), 0, diversity)

          diversity

        }

        else{

          if(type=="night"){

            colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<sunrise|colnames_df>sunset)))
            nighttime_df=df[rownames_subset,colnames_night]

            diversity=(hilldiv::hill_div(unlist(nighttime_df), qvalue=1)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(nighttime_df)*nrow(nighttime_df))} else{print("Error: invalid output argument")}})
            diversity=diversity*multiplier
            diversity=ifelse(is.na(diversity), 0, diversity)

            diversity

          }

          else{

            if (type=="dawn"){

              colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))
              dawntime_df=df[rownames_subset,colnames_dawn]

              diversity=(hilldiv::hill_div(unlist(dawntime_df), qvalue=1)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(dawntime_df)*nrow(dawntime_df))} else{print("Error: invalid output argument")}})
              diversity=diversity*multiplier
              diversity=ifelse(is.na(diversity), 0, diversity)

              diversity

            }

            else{

              if (type=="dusk"){

                colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>=(sunset-duskstart)&colnames_df<=(sunset+duskend))))
                dusktime_df=df[rownames_subset,colnames_dusk]

                diversity=(hilldiv::hill_div(unlist(dusktime_df), qvalue=1)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(dusktime_df)*nrow(dusktime_df))} else{print("Error: invalid output argument")}})
                diversity=diversity*multiplier
                diversity=ifelse(is.na(diversity), 0, diversity)

                diversity

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

      freq=seq(freq1, freq2, (freq2/nbins))

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

        diversity=c()


        for (i in 1:length(freq_list_2)){
          diversity[i]=(hilldiv::hill_div(unlist(freq_list_2[[i]]), qvalue=1)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_2[[i]])*nrow(freq_list_2[[i]]))} else{print("Error: invalid output argument")}})
          diversity[i]=diversity[i]*multiplier
          diversity[i]=ifelse(is.na(diversity[i]), 0, diversity[i])
        }

        diversity
      }


      else{

        if (type=="tod"){

          diversity=vector("list", length(freq_list_2))

          for (i in 1:length(freq_list_2)){
            diversity[[i]]=vector("list", ncol(freq_list_2[[i]]))
            for (j in 1:ncol(freq_list_2[[i]])){
              diversity[[i]][[j]]=(hilldiv::hill_div(unlist(freq_list_2[[i]][[j]]), qvalue = 1)/if (output=="raw"){1} else{if(output=="percentage"){length(unlist(freq_list_2[[i]][[j]]))} else{print("Error: invalid output argument")}})
              diversity[[i]][[j]]=diversity[[i]][[j]]*multiplier
              diversity[[i]][[j]]=ifelse(is.na(diversity[[i]][[j]]), 0, diversity[[i]])
            }
            diversity[[i]]=unlist(diversity[[i]])
          }

          diversity
        }

        else{

          if (type=="day"){

            colnames_day=as.character(hms::as_hms(subset(colnames_df, colnames_df>=sunrise&colnames_df<=sunset)))

            freq_list_day=vector("list", 0)

            for (i in 1:length(freq_list_2)){
              freq_list_day[[i]]=freq_list_2[[i]][rownames_subset, colnames_day]
            }

            diversity=c()

            for (i in 1:length(freq_list_day)){
              diversity[i]=(hilldiv::hill_div(unlist(freq_list_day[[i]]), qvalue=1)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_day[[i]])*nrow(freq_list_day[[i]]))} else{print("Error: invalid output argument")}})
              diversity[i]=diversity[i]*multiplier
              diversity[i]=ifelse(is.na(diversity[i]), 0, diversity[i])
            }

            diversity

          }

          else{

            if (type=="night"){

              colnames_night=as.character(hms::as_hms(subset(colnames_df, colnames_df<=sunrise|colnames_df>=sunset)))

              freq_list_night=vector("list", 0)

              for (i in 1:length(freq_list_2)){
                freq_list_night[[i]]=freq_list_2[[i]][, colnames_night]
              }

              diversity=c()

              for (i in 1:length(freq_list_night)){
                diversity[i]=(hilldiv::hill_div(unlist(freq_list_night[[i]]), qvalue=1)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_night[[i]])*nrow(freq_list_night[[i]]))} else{print("Error: invalid output argument")}})
                diversity[i]=diversity[i]*multiplier
                diversity[i]=ifelse(is.na(diversity[i]), 0, diversity[i])
              }

              diversity

            }

            else{

              if (type=="dawn"){

                colnames_dawn=as.character(hms::as_hms(subset(colnames_df, colnames_df>(sunrise-dawnstart)&colnames_df<=(sunrise+dawnend))))

                freq_list_dawn=vector("list", 0)

                for (i in 1:length(freq_list_2)){
                  freq_list_dawn[[i]]=freq_list_2[[i]][, colnames_dawn]
                }

                diversity=c()

                for (i in 1:length(freq_list_dawn)){
                  diversity[i]=(hilldiv::hill_div(unlist(freq_list_dawn[[i]]), qvalue=1)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_dawn[[i]])*nrow(freq_list_dawn[[i]]))} else{print("Error: invalid output argument")}})
                  diversity[i]=diversity[i]*multiplier
                  diversity[i]=ifelse(is.na(diversity[i]), 0, diversity[i])
                }

                diversity

              }


              else{

                if (type=="dusk"){

                  colnames_dusk=as.character(hms::as_hms(subset(colnames_df, colnames_df>(sunset-duskstart)&colnames_df<=(sunset+duskend))))

                  freq_list_dusk=vector("list", 0)

                  for (i in 1:length(freq_list_2)){
                    freq_list_dusk[[i]]=freq_list_2[[i]][, colnames_dusk]
                  }


                  diversity=c()

                  for (i in 1:length(freq_list_dusk)){
                    diversity[i]=(hilldiv::hill_div(unlist(freq_list_dusk[[i]]), qvalue=1)/if (output=="raw"){1} else{if(output=="percentage"){(ncol(freq_list_dusk[[i]])*nrow(freq_list_dusk[[i]]))} else{print("Error: invalid output argument")}})
                    diversity[i]=diversity[i]*multiplier
                    diversity[i]=ifelse(is.na(diversity[i]), 0, diversity[i])
                  }

                  diversity

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

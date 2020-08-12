# Alpha, Beta and Gamma partitioning + dissimilarity measures

# Partitioning the diversity of the overal system


#' Partitioning Soundscape Diversity into Alpha, Beta and Gamma components
#'
#' @description The diversity of a system can generally be broken down into three components:
#' Alpha, Beta and Gamma diversity.
#'
#' For Hill numbers these three components take a multiplicative relationship: Gamma = Alpha x Beta
#'
#' - \bold{Alpha diversity}: The average diversity of the subsystems
#'
#' - \bold{Beta diversity}: The compositional heterogeneity between subsystems.
#'
#' The beta diversity component can be computed as: Beta = Gamma / Alpha.
#'
#' Beta represents the effective number of equally large and completely unique subsystems within the system. As Beta quantifies the ratio between Gamma and Alpha, it can also be seen as the number of times more diverse the whole system is in effective number of OSUs compared to its constituent subsystems on average. The beta diversity ranges from 1 to N (the number of subsystems in the system).
#'
#' - \bold{Gamma diversity}: The average diversity of the whole system
#'


#' @param df_list A list of dataframes of equal dimensions, each dataframe being produced by \code{\link{aggregate_df}}.
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param hier_table A matrix indicating the relationship between the soundscapes in the df_list. The first column lists the names of all the soundscapes in the df_list, other columns can be used to group soundscapes into higher hierarchical levels. If no hierarchy table is supplied, the function defaults to a 2-level diversity partitioning.
#' @param type The scale for which the soundscape diversity is computed. Options are 'total', 'day',
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
#' @param dawnstart A numeric argument. If type='dawn', used to determine the start of dawn. By default, dawn starts at sunrise. Expressed as the time in seconds before sunrise.
#' @param dawnend A numeric argument. If type='dawn', used to determine the end of dawn. By default, dawn ends 1.5 hours after sunrise. Expressed as the time in seconds after sunrise.
#' @param duskstart A numeric argument. If type='dusk', used to determine the start of dusk. By default, dusk starts 1.5 hours before sunset. Expressed as the time in seconds before sunset.
#' @param duskend A numeric argument. If type='dusk', used to determine the end of dusk. By default, dusk ends at sunset. Expressed as the time in seconds after sunset.
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#' @return A dataframe of diversity values
#' @export
sounddiv_part <- function(df_list, qvalue, hier_table="default", type="total",minfreq="default",
                               maxfreq="default",mintime="default", maxtime="default",date, lat, lon,
                               twilight="sunlight",dawnstart=0, dawnend=5400, duskstart=5400,
                               duskend=0,output="percentage"){

  if (class(df_list)=="data.frame"){
    errorCondition(message = "Invalid df_list class - please supply a list of dataframes")
  }

  else{

    if (class(df_list)=="list"){

      # Checking whether the arguments supplied are single values or vectors of values
      # If arguments are single values, turns them into vector of repeated values of the
      # same length as the list of dataframes

      if (length(minfreq)==1){
        minfreq <- rep(minfreq, length(df_list))
      } else{ if (length(minfreq)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(maxfreq)==1){
        maxfreq <- rep(maxfreq, length(df_list))
      } else{ if (length(maxfreq)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(mintime)==1){
        mintime <- rep(mintime, length(df_list))
      } else{ if (length(mintime)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(maxtime)==1){
        maxtime <- rep(maxtime, length(df_list))
      } else{ if (length(maxtime)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(date)==1){
        date <- rep(date, length(df_list))
      } else{ if (length(date)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(lat)==1){
        lat <- rep(lat, length(df_list))
      } else{ if (length(lat)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(lon)==1){
        lon <- rep(lon, length(df_list))
      } else{ if (length(lon)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(twilight)==1){
        twilight <- rep(twilight, length(df_list))
      } else{ if (length(twilight)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(dawnstart)==1){
        dawnstart <- rep(dawnstart, length(df_list))
      } else{ if (length(dawnstart)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(dawnend)==1){
        dawnend <- rep(dawnend, length(df_list))
      } else{ if (length(dawnend)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(duskstart)==1){
        duskstart <- rep(duskstart, length(df_list))
      } else{ if (length(duskstart)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(duskend)==1){
        duskend <- rep(duskend, length(df_list))
      } else{ if (length(duskend)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}


      # Preparing the subsetting arguments (timezone, date, lat, lon, etc...)

      # Timezone

      tz <- vector("list", 0)

      for (i in 1:length(df_list)){
        tz[[i]] <- lutz::tz_lookup_coords(lat=lat[i], lon=lon[i], method="accurate")
      }

      # Day

      day <- vector("list", 0)

      for (i in 1:length(df_list)){
        day[[i]] <- as.POSIXct(strptime(paste(date[i], "00:00", sep=" "),
                                   format= "%Y-%m-%d %H:%M",
                                   tz=tz[[i]]))
      }

      # Coordinates

      points <- vector("list", 0)

      for (i in 1:length(df_list)){
        points[[i]] <- as.data.frame(t(as.data.frame(c(lat[i], lon[i]))))
        colnames(points[[i]]) <- c("lat","lon")
      }

      # Sunset and sunrise times

      suntimes <- vector("list", 0)

      for (i in 1:length(df_list)){
        suntimes[[i]] <- photobiology::day_night(date=day[[i]],
                                                 tz=lubridate::tz(day[[i]]),
                                                 geocode = points[[i]],
                                                 twilight = twilight[i],
                                                 unit.out = "datetime")
      }

      sunrise <- vector("list", 0)

      for (i in 1:length(df_list)){
        sunrise[[i]] <- suntimes[[i]]$sunrise
      }

      sunset <- vector("list", 0)

      for (i in 1:length(df_list)){
        sunset[[i]] <- suntimes[[i]]$sunset
      }

      # Frequency subsets

      freq1 <- vector("list", 0)

      for (i in 1:length(df_list)){
        if (minfreq[i]=="default"){
          freq1[[i]] <- min(as.numeric(rownames(df_list[[i]])))
        }

        else{
          freq1[[i]] <- minfreq[i]
        }
      }

      freq2 <- vector("list", 0)

      for (i in 1:length(df_list)){
        if (maxfreq[i]=="default"){
          freq2[[i]] <- max(as.numeric(rownames(df_list[[i]])))
        }

        else{
          freq2[[i]] <- maxfreq[i]
        }
      }

      # Time subsets

      time1 <- vector("list", 0)

      for (i in 1:length(df_list)){
        if (mintime[i]=="default"){
          time1[[i]] <- min(as.POSIXct(strptime(paste(date[i],colnames(df_list[[i]]),sep=" "),
                                                format= "%Y-%m-%d %H:%M:%S",
                                                tz=tz[[i]])))
        }

        else{
          time1[[i]] <- mintime[i]
        }
      }

      time2 <- vector("list", 0)

      for (i in 1:length(df_list)){
        if (maxtime[i]=="default"){
          time2[[i]] <- max(as.POSIXct(strptime(paste(date[i],colnames(df_list[[i]]),sep=" "),
                                                format= "%Y-%m-%d %H:%M:%S",
                                                tz=tz[[i]])))
        }

        else{
          time2[[i]] <- maxtime[i]
        }
      }


      # Subsetting vectors

      rownames_df <- vector("list", 0)
      rownames_subset <- vector("list", 0)
      colnames_df <- vector("list", 0)
      colnames_subset <- vector("list", 0)
      new_df <- vector("list", 0)

      for (i in 1:length(df_list)){
        rownames_df[[i]] <- as.numeric(rownames(df_list[[i]]))

        rownames_subset[[i]] <- as.character(subset(rownames_df[[i]],
                                                    rownames_df[[i]]>=freq1[[i]]&
                                                      rownames_df[[i]]<=freq2[[i]]))

        colnames_df[[i]] <- as.POSIXct(strptime(paste(date[i],colnames(df_list[[i]]),sep=" "),
                                                format= "%Y-%m-%d %H:%M:%S",
                                                tz=tz[[i]]))

        colnames_subset[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                                colnames_df[[i]]
                                                                >=time1[[i]]
                                                                & colnames_df[[i]]
                                                                <=time2[[i]])))

        new_df[[i]] <- df_list[[i]][rownames_subset[[i]], colnames_subset[[i]]]

      }

      if (type=="total"){

        soundscape_df <- vector("list", 0)

        for (i in 1:length(new_df)){
          soundscape_df[[i]] <- unlist(new_df[[i]])

        }

        soundscape_df <- as.data.frame(dplyr::bind_cols(soundscape_df))

        soundscape_df
      }

      else{

        if (type=="day"){

          colnames_day <- vector("list", 0)
          daytime_df <- vector("list", 0)

          for(i in 1:length(df_list)){
            colnames_day[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                                 colnames_df[[i]]
                                                                 >=sunrise[[i]]
                                                                 &colnames_df[[i]]
                                                                 <=sunset[[i]])))

            daytime_df[[i]] <- df_list[[i]][rownames_subset[[i]],colnames_day[[i]]]
          }

          soundscape_df <- vector("list", 0)

          for (i in 1:length(daytime_df)){

            soundscape_df[[i]] <- unlist(daytime_df[[i]])


          }

          soundscape_df <- dplyr::bind_cols(soundscape_df)

        }

        else{

          if (type=="night"){

            colnames_night <- vector("list", 0)
            nighttime_df <- vector("list", 0)

            for(i in 1:length(df_list)){
              colnames_night[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                                     colnames_df[[i]]
                                                                     <sunrise[[i]]
                                                                     |colnames_df[[i]]
                                                                     >sunset[[i]])))

              nighttime_df[[i]] <- df_list[[i]][rownames_subset[[i]],colnames_night[[i]]]
            }

            soundscape_df <- vector("list", 0)

            for (i in 1:length(nighttime_df)){

              soundscape_df[[i]] <- unlist(nighttime_df[[i]])


            }

            soundscape_df <- dplyr::bind_cols(soundscape_df)

          }

          else{

            if (type=="dawn"){

              colnames_dawn <- vector("list", 0)
              dawntime_df <- vector("list", 0)

              for(i in 1:length(df_list)){
                colnames_dawn[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                                      colnames_df[[i]]
                                                                      >=(sunrise[[i]]
                                                                         -dawnstart[i])
                                                                      &colnames_df[[i]]
                                                                      <=(sunrise[[i]]
                                                                         +dawnend[i]))))

                dawntime_df[[i]] <- df_list[[i]][rownames_subset[[i]],colnames_dawn[[i]]]
              }

              soundscape_df <- vector("list", 0)

              for (i in 1:length(dawntime_df)){

                soundscape_df[[i]] <- unlist(dawntime_df[[i]])


              }

              soundscape_df <- dplyr::bind_cols(soundscape_df)

            }

            else{

              if (type=="dusk"){

                colnames_dusk <- vector("list", 0)
                dusktime_df <- vector("list", 0)

                for(i in 1:length(df_list)){

                  colnames_dusk[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                                        colnames_df[[i]]
                                                                        >=(sunset[[i]]
                                                                           -duskstart[i])
                                                                        &colnames_df[[i]]
                                                                        <=(sunset[[i]]
                                                                           +duskend[i]))))

                  dusktime_df[[i]] <- df_list[[i]][rownames_subset[[i]],colnames_dusk[[i]]]
                }

                soundscape_df <- vector("list", 0)

                for (i in 1:length(dusktime_df)){

                  soundscape_df[[i]] <- unlist(dusktime_df[[i]])


                }

                soundscape_df <- dplyr::bind_cols(soundscape_df)

              }

              else{errorCondition("Invalid type argument, please consult package documentation for options")}

            }
          }
        }
      }

      if (hier_table=="default"){
      soundscape_part <- hilldiv::div_part(countable = soundscape_df,
                                           qvalue = qvalue)
      }

      else{

        soundscape_part <- hilldiv::div_part(countable = soundscape_df,
                                             qvalue = qvalue,
                                             hierarchy = hier_table)
      }

      soundscape_part_table <- dplyr::bind_rows(unlist(soundscape_part))

      colnames(soundscape_part_table) <- c("levels",
                                           "q",
                                           paste0("alpha_l",
                                                  seq(1, (length(soundscape_part$Hill_numbers)-1), 1)),
                                           "gamma",
                                           paste0("N",
                                                  seq(1, length(soundscape_part$Sample_size), 1)),
                                           paste0("beta_l",
                                                  seq(1, (length(soundscape_part$Hill_numbers)-1), 1)))


      if (output=="percentage"){
        soundscape_part_table[3:(3+length(soundscape_part$Hill_numbers)-1)] <- (soundscape_part_table[3:(3+length(soundscape_part$Hill_numbers)-1)]/nrow(soundscape_df))*100
      }

      else {
        if (output=="raw"){}

        else{
          errorCondition("Invalid output argument - please consult package documentation for options")
        }
        }
    }

    else{errorCondition("Invalid df_list class - please supply a list of dataframes")}
  }

  soundscape_part_table

}

#########################################################################################################


# Getting the pairwise dissimilarities between groups in the system

#' Pairwise Beta diversity and dissimilarity values between subsystems in the system
#'
#' @description Computation of pairwise dissimularities among soundscapes.
#'
#' Regular Beta diversity varies between 1-N, its value being dependent on the number of subsystems under consideration (N). As such, due to its dependence on the number of subsystems, it cannot directly be used as a measure of dissimilarity among communities. Instead, several simple transformations can be performed on the Beta values to get dissimilarity indices ranging between 0-1.
#'
#'
#' - Sorensen-type (local) overlap:
#'
#' Quantifies the average proportion of a sub-system's OSUs which are shared across all considered sub-systems. It quantifies the overlap (similarity) from the sub-system perspective. To make this into a dissimilarity metric, we take the one-complement (1-Sorensen overlap), being the average proportion of non-shared OSUs in the system.
#'
#' - Jaccard-type (regional) overlap:
#'
#' Quantifies the effective proportion of OSUs which are shared across all subsystems. It quantifies overlap (similarity) from the perspective of the overall system. To make this into a dissimilarity metric, we take the one-complement (1 - Jaccard overlap), being the effective proportion of non-shard OSUs in the whole system.
#'
#' - Sorensen-type turnover:
#'
#' Quantifies the normalized turnover rate of OSUs from the perspective of the subsystem (alpha) - or the proportion of a subsystem which changes across subsystems. Once again, we take the one-complement as a dissimilarity measure.
#'
#' - Jaccard-type turnover:
#'
#' Quantifies the normalized OSU turnover rate from the perspective of the whole system (gamma). Once more, the one-complement gives us our dissimilarity measure.
#'
#'
#'
#' @param df_list A list of dataframes of equal dimensions, each dataframe being produced by \code{\link{aggregate_df}}.
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param hier_table A matrix indicating the relationship between the soundscapes in the df_list. The first column lists the names of all the soundscapes in the df_list, other columns can be used to group soundscapes into higher hierarchical levels. If no hierarchy table is supplied, the function defaults to a 2-level diversity partitioning.
#' @param level A positive integer. If a multi-level hierarchical structure exists in the data, as supplied in the hierarchy table, this argument indicates the hierarchical level for which to compute the pairwise values. level=1 computes the pairwise values at the lowest hierarchical level (sub-systems or individual soundscapes), whereas level=2,...,X refers to higher levels of grouping, with the level value corresponding to the column number of the grouping variable in the hierarchy table.
#' @param type The scale for which the soundscape diversity is computed. Options are 'total', 'day', night', 'dawn', 'dusk' and 'tod' (time of day - for each unique time in the day).
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
#' @param dawnstart A numeric argument. If type='dawn', used to determine the start of dawn. By default, dawn starts at sunrise. Expressed as the time in seconds before sunrise.
#' @param dawnend A numeric argument. If type='dawn', used to determine the end of dawn. By default, dawn ends 1.5 hours after sunrise. Expressed as the time in seconds after sunrise.
#' @param duskstart A numeric argument. If type='dusk', used to determine the start of dusk. By default, dusk starts 1.5 hours before sunset. Expressed as the time in seconds before sunset.
#' @param duskend A numeric argument. If type='dusk', used to determine the end of dusk. By default, dusk ends at sunset. Expressed as the time in seconds after sunset.
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#' @return A list of pairwise matrices for the beta diversity, and the one-complement for the Sorensen-type overlap, Jaccard-type overlap, Sorensen-type turnover and Jaccard-type turnover.
#' @export

sounddiv_pairdis <- function(df_list, qvalue, hier_table="default", level="default",
                             type="total",minfreq="default", maxfreq="default",
                             mintime="default",maxtime="default",date, lat, lon,
                             twilight="sunlight",dawnstart=0,dawnend=5400, duskstart=5400,
                             duskend=0,output="percentage"){

  if (class(df_list)=="data.frame"){
    errorCondition(message = "Invalid df_list class - please supply a list of dataframes")
  }

  else{

    if (class(df_list)=="list"){

      # Checking whether the arguments supplied are single values or vectors of values
      # If arguments are single values, turns them into vector of repeated values of the
      # same length as the list of dataframes

      if (length(minfreq)==1){
        minfreq <- rep(minfreq, length(df_list))
      } else{ if (length(minfreq)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(maxfreq)==1){
        maxfreq <- rep(maxfreq, length(df_list))
      } else{ if (length(maxfreq)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(mintime)==1){
        mintime <- rep(mintime, length(df_list))
      } else{ if (length(mintime)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(maxtime)==1){
        maxtime <- rep(maxtime, length(df_list))
      } else{ if (length(maxtime)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(date)==1){
        date <- rep(date, length(df_list))
      } else{ if (length(date)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(lat)==1){
        lat <- rep(lat, length(df_list))
      } else{ if (length(lat)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(lon)==1){
        lon <- rep(lon, length(df_list))
      } else{ if (length(lon)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(twilight)==1){
        twilight <- rep(twilight, length(df_list))
      } else{ if (length(twilight)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(dawnstart)==1){
        dawnstart <- rep(dawnstart, length(df_list))
      } else{ if (length(dawnstart)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(dawnend)==1){
        dawnend <- rep(dawnend, length(df_list))
      } else{ if (length(dawnend)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(duskstart)==1){
        duskstart <- rep(duskstart, length(df_list))
      } else{ if (length(duskstart)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}

      if (length(duskend)==1){
        duskend <- rep(duskend, length(df_list))
      } else{ if (length(duskend)==length(df_list)){}
        else{errorCondition("Invalid length of minfreq - please supply value of length 1 or the length of the list of dataframes")}}


      # Preparing the subsetting arguments (timezone, date, lat, lon, etc...)

      # Timezone

      tz <- vector("list", 0)

      for (i in 1:length(df_list)){
        tz[[i]] <- lutz::tz_lookup_coords(lat=lat[i], lon=lon[i], method="accurate")
      }

      # Day

      day <- vector("list", 0)

      for (i in 1:length(df_list)){
        day[[i]] <- as.POSIXct(strptime(paste(date[i], "00:00", sep=" "),
                                        format= "%Y-%m-%d %H:%M",
                                        tz=tz[[i]]))
      }

      # Coordinates

      points <- vector("list", 0)

      for (i in 1:length(df_list)){
        points[[i]] <- as.data.frame(t(as.data.frame(c(lat[i], lon[i]))))
        colnames(points[[i]]) <- c("lat","lon")
      }

      # Sunset and sunrise times

      suntimes <- vector("list", 0)

      for (i in 1:length(df_list)){
        suntimes[[i]] <- photobiology::day_night(date=day[[i]],
                                                 tz=lubridate::tz(day[[i]]),
                                                 geocode = points[[i]],
                                                 twilight = twilight[i],
                                                 unit.out = "datetime")
      }

      sunrise <- vector("list", 0)

      for (i in 1:length(df_list)){
        sunrise[[i]] <- suntimes[[i]]$sunrise
      }

      sunset <- vector("list", 0)

      for (i in 1:length(df_list)){
        sunset[[i]] <- suntimes[[i]]$sunset
      }

      # Frequency subsets

      freq1 <- vector("list", 0)

      for (i in 1:length(df_list)){
        if (minfreq[i]=="default"){
          freq1[[i]] <- min(as.numeric(rownames(df_list[[i]])))
        }

        else{
          freq1[[i]] <- minfreq[i]
        }
      }

      freq2 <- vector("list", 0)

      for (i in 1:length(df_list)){
        if (maxfreq[i]=="default"){
          freq2[[i]] <- max(as.numeric(rownames(df_list[[i]])))
        }

        else{
          freq2[[i]] <- maxfreq[i]
        }
      }

      # Time subsets

      time1 <- vector("list", 0)

      for (i in 1:length(df_list)){
        if (mintime[i]=="default"){
          time1[[i]] <- min(as.POSIXct(strptime(paste(date[i],colnames(df_list[[i]]),sep=" "),
                                                format= "%Y-%m-%d %H:%M:%S",
                                                tz=tz[[i]])))
        }

        else{
          time1[[i]] <- mintime[i]
        }
      }

      time2 <- vector("list", 0)

      for (i in 1:length(df_list)){
        if (maxtime[i]=="default"){
          time2[[i]] <- max(as.POSIXct(strptime(paste(date[i],colnames(df_list[[i]]),sep=" "),
                                                format= "%Y-%m-%d %H:%M:%S",
                                                tz=tz[[i]])))
        }

        else{
          time2[[i]] <- maxtime[i]
        }
      }


      # Subsetting vectors

      rownames_df <- vector("list", 0)
      rownames_subset <- vector("list", 0)
      colnames_df <- vector("list", 0)
      colnames_subset <- vector("list", 0)
      new_df <- vector("list", 0)

      for (i in 1:length(df_list)){
        rownames_df[[i]] <- as.numeric(rownames(df_list[[i]]))

        rownames_subset[[i]] <- as.character(subset(rownames_df[[i]],
                                                    rownames_df[[i]]>=freq1[[i]]&
                                                      rownames_df[[i]]<=freq2[[i]]))

        colnames_df[[i]] <- as.POSIXct(strptime(paste(date[i],colnames(df_list[[i]]),sep=" "),
                                                format= "%Y-%m-%d %H:%M:%S",
                                                tz=tz[[i]]))

        colnames_subset[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                                colnames_df[[i]]
                                                                >=time1[[i]]
                                                                & colnames_df[[i]]
                                                                <=time2[[i]])))

        new_df[[i]] <- df_list[[i]][rownames_subset[[i]], colnames_subset[[i]]]

      }


      if (type=="total"){

        soundscape_df <- vector("list", 0)

        for (i in 1:length(new_df)){
          soundscape_df[[i]] <- unlist(new_df[[i]])

        }

        soundscape_df <- as.data.frame(dplyr::bind_cols(soundscape_df))
        colnames(soundscape_df) <- names(df_list)
        rownames(soundscape_df) <- paste0("OSU", seq(1, nrow(soundscape_df), 1))

      }

      else{

        if (type=="day"){

          colnames_day <- vector("list", 0)
          daytime_df <- vector("list", 0)

          for(i in 1:length(df_list)){
            colnames_day[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                                 colnames_df[[i]]
                                                                 >=sunrise[[i]]
                                                                 &colnames_df[[i]]
                                                                 <=sunset[[i]])))

            daytime_df[[i]] <- df_list[[i]][rownames_subset[[i]],colnames_day[[i]]]
          }

          soundscape_df <- vector("list", 0)

          for (i in 1:length(daytime_df)){

            soundscape_df[[i]] <- unlist(daytime_df[[i]])


          }

          soundscape_df <- dplyr::bind_cols(soundscape_df)
          colnames(soundscape_df) <- names(df_list)
          rownames(soundscape_df) <- paste0("OSU", seq(1, nrow(soundscape_df), 1))

        }

        else{

          if (type=="night"){

            colnames_night <- vector("list", 0)
            nighttime_df <- vector("list", 0)

            for(i in 1:length(df_list)){
              colnames_night[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                                     colnames_df[[i]]
                                                                     <sunrise[[i]]
                                                                     |colnames_df[[i]]
                                                                     >sunset[[i]])))

              nighttime_df[[i]] <- df_list[[i]][rownames_subset[[i]],colnames_night[[i]]]
            }

            soundscape_df <- vector("list", 0)

            for (i in 1:length(nighttime_df)){

              soundscape_df[[i]] <- unlist(nighttime_df[[i]])


            }

            soundscape_df <- dplyr::bind_cols(soundscape_df)
            colnames(soundscape_df) <- names(df_list)

          }

          else{

            if (type=="dawn"){

              colnames_dawn <- vector("list", 0)
              dawntime_df <- vector("list", 0)

              for(i in 1:length(df_list)){
                colnames_dawn[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                                      colnames_df[[i]]
                                                                      >=(sunrise[[i]]
                                                                         -dawnstart[i])
                                                                      &colnames_df[[i]]
                                                                      <=(sunrise[[i]]
                                                                         +dawnend[i]))))

                dawntime_df[[i]] <- df_list[[i]][rownames_subset[[i]],colnames_dawn[[i]]]
              }

              soundscape_df <- vector("list", 0)

              for (i in 1:length(dawntime_df)){

                soundscape_df[[i]] <- unlist(dawntime_df[[i]])


              }

              soundscape_df <- dplyr::bind_cols(soundscape_df)
              colnames(soundscape_df) <- names(df_list)
              rownames(soundscape_df) <- paste0("OSU", seq(1, nrow(soundscape_df), 1))

            }

            else{

              if (type=="dusk"){

                colnames_dusk <- vector("list", 0)
                dusktime_df <- vector("list", 0)

                for(i in 1:length(df_list)){

                  colnames_dusk[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                                        colnames_df[[i]]
                                                                        >=(sunset[[i]]
                                                                           -duskstart[i])
                                                                        &colnames_df[[i]]
                                                                        <=(sunset[[i]]
                                                                           +duskend[i]))))

                  dusktime_df[[i]] <- df_list[[i]][rownames_subset[[i]],colnames_dusk[[i]]]
                }

                soundscape_df <- vector("list", 0)

                for (i in 1:length(dusktime_df)){

                  soundscape_df[[i]] <- unlist(dusktime_df[[i]])


                }

                soundscape_df <- dplyr::bind_cols(soundscape_df)
                colnames(soundscape_df) <- names(df_list)
                rownames(soundscape_df) <- paste0("OSU", seq(1, nrow(soundscape_df), 1))

              }

              else{errorCondition("Invalid type argument, please consult package documentation for options")}

            }
          }
        }
      }

      if(missing(hier_table)){
        soundscape_pairdis <- hilldiv::pair_dis(countable = soundscape_df,
                                                qvalue = qvalue)
      }

      else{
        soundscape_pairdis <- hilldiv::pair_dis(countable = soundscape_df,
                                                qvalue = qvalue,
                                                hierarchy = hier_table,
                                                level = level)
      }

      soundscape_pairdis

    }

    else{errorCondition("Invalid df_list class - please supply a list of dataframes")}

  }
}


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


#' @param soundscape_list A list of dataframes of equal dimensions, each dataframe being produced by \code{\link{ss_aggregate}}.
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param hier_table A matrix indicating the relationship between the soundscapes in the soundscape_list. The first column lists the names of all the soundscapes in the soundscape_list, other columns can be used to group soundscapes into higher hierarchical levels. If no hierarchy table is supplied, the function defaults to a 2-level diversity partitioning.
#' @param minfreq A numeric value indicating the lower frequency limit for which to compute the soundscape diversity. If set to default, uses the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit for which to compute the soundscape diversity. If set to default, uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the latest time for which data exists in the dataframe.
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#' @return A dataframe of diversity values
#' @export
ss_divpart <- function(soundscape_list,
                       qvalue,
                       hier_table = NA,
                       minfreq = 0,
                       maxfreq = "default",
                       mintime = "default",
                       maxtime = "default",
                       output="percentage"){

  # 0. Testing

  ## At some point, test that all the soundscapes have the same dimensions


  ## Check that, if subsetting, the soundscapes all have the same day and night length

  # 1. Preparing the subsetting arguments

  # Frequency subsetting

  minfreq <- minfreq

  if(maxfreq=="default"){

    maxfreq <- max(as.numeric(rownames(soundscape_list[[1]]@aggregated_df)))

  }

  else{maxfreq <- maxfreq}

  if (mintime=="default"){

    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)){

      mintime_list[[i]] <- min(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                  colnames(soundscape_list[[i]]@aggregated_df),
                  sep=" "),
            format= "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz)))

    }
  }

  else{

    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)){

      mintime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                mintime,
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz))

    }

  }


  if (maxtime=="default"){

    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)){

      maxtime_list[[i]] <- max(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                  colnames(soundscape_list[[i]]@aggregated_df),
                  sep=" "),
            format= "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz)))

    }

  }

  else{

    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)){

      maxtime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                maxtime,
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz))

    }
  }

  # 2. Subsetting the soundscapes based on the time and freq arguments

  # Subsetting vectors

  rownames_df <- vector("list", length(soundscape_list))
  rownames_subset <- vector("list", length(soundscape_list))
  colnames_df <- vector("list", length(soundscape_list))
  colnames_subset <- vector("list", length(soundscape_list))
  new_soundscape_list <- vector("list", length(soundscape_list))

  for (i in 1:length(soundscape_list)){

    rownames_df[[i]] <- as.numeric(rownames(soundscape_list[[i]]@aggregated_df))

    rownames_subset[[i]] <- as.character(subset(rownames_df[[i]],
                                                rownames_df[[i]]>=minfreq&
                                                  rownames_df[[i]]<=maxfreq))

    colnames_df[[i]] <- as.POSIXct(strptime(paste(soundscape_list[[i]]@first_day,
                                                  colnames(soundscape_list[[i]]@aggregated_df),sep=" "),
                                            format= "%Y-%m-%d %H:%M:%S",
                                            tz=soundscape_list[[i]]@tz))

    colnames_subset[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                            colnames_df[[i]]
                                                            >=mintime_list[[i]]
                                                            & colnames_df[[i]]
                                                            <=maxtime_list[[i]])))

    new_soundscape_list[[i]] <- soundscape_list[[i]]@aggregated_df[rownames_subset[[i]], colnames_subset[[i]]]

  }

  # 2. Prepare the soundscape list to be in the correct format

  # 2.1. Create site-by-osu df

  site_by_OSU_matrix <-
    as.data.frame(
      t(do.call(rbind,
                lapply(new_soundscape_list,
                       function(x) unlist(x)))))

  rownames(site_by_OSU_matrix) <- paste0("OSU_",
                                         seq(1, nrow(site_by_OSU_matrix), 1))

  colnames(site_by_OSU_matrix) <- as.character(names(soundscape_list))


  # 3. Partition the soundscape diversity into its gamma, alpha and beta components

  if (is.na(hier_table)){

    soundscape_part <- hilldiv::div_part(countable = site_by_OSU_matrix,
                                         qvalue = qvalue)
  }

  else{

    soundscape_part <- hilldiv::div_part(countable = site_by_OSU_matrix,
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
    soundscape_part_table[3:(3+length(soundscape_part$Hill_numbers)-1)] <- (soundscape_part_table[3:(3+length(soundscape_part$Hill_numbers)-1)]/nrow(site_by_OSU_matrix))*100
  }

  else {}

  return(soundscape_part_table)

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
#' - Sorensen-subset (local) overlap:
#'
#' Quantifies the average proportion of a sub-system's OSUs which are shared across all considered sub-systems. It quantifies the overlap (similarity) from the sub-system perspective. To make this into a dissimilarity metric, we take the one-complement (1-Sorensen overlap), being the average proportion of non-shared OSUs in the system.
#'
#' - Jaccard-subset (regional) overlap:
#'
#' Quantifies the effective proportion of OSUs which are shared across all subsystems. It quantifies overlap (similarity) from the perspective of the overall system. To make this into a dissimilarity metric, we take the one-complement (1 - Jaccard overlap), being the effective proportion of non-shard OSUs in the whole system.
#'
#' - Sorensen-subset turnover:
#'
#' Quantifies the normalized turnover rate of OSUs from the perspective of the subsystem (alpha) - or the proportion of a subsystem which changes across subsystems. Once again, we take the one-complement as a dissimilarity measure.
#'
#' - Jaccard-subset turnover:
#'
#' Quantifies the normalized OSU turnover rate from the perspective of the whole system (gamma). Once more, the one-complement gives us our dissimilarity measure.
#'
#'
#'
#' @param soundscape_list A list of dataframes of equal dimensions, each dataframe being produced by \code{\link{ss_aggregate}}.
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param hier_table A matrix indicating the relationship between the soundscapes in the soundscape_list. The first column lists the names of all the soundscapes in the soundscape_list, other columns can be used to group soundscapes into higher hierarchical levels. If no hierarchy table is supplied, the function defaults to a 2-level diversity partitioning.
#' @param minfreq A numeric value indicating the lower frequency limit for which to compute the soundscape diversity. If set to default, uses the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit for which to compute the soundscape diversity. If set to default, uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the latest time for which data exists in the dataframe.
#' @return A list of pairwise matrices for the beta diversity, and the one-complement for the Sorensen-subset overlap, Jaccard-subset overlap, Sorensen-subset turnover and Jaccard-subset turnover.
#' @export

ss_pairdis <- function(soundscape_list,
                       qvalue = 0,
                       hier_table=NA,
                       minfreq=0,
                       maxfreq="default",
                       mintime="default",
                       maxtime="default"){


  # 0. Testing

  ## At some point, test that all the soundscapes have the same dimensions


  ## Check that, if subsetting, the soundscapes all have the same day and night length

  # 1. Preparing the subsetting arguments

  # Frequency subsetting

  minfreq <- minfreq

  if(maxfreq=="default"){

    maxfreq <- max(as.numeric(rownames(soundscape_list[[1]]@aggregated_df)))

  }

  else{maxfreq <- maxfreq}

  if (mintime=="default"){

    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)){

      mintime_list[[i]] <- min(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                  colnames(soundscape_list[[i]]@aggregated_df),
                  sep=" "),
            format= "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz)))

    }
  }

  else{

    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)){

      mintime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                mintime,
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz))

    }

  }


  if (maxtime=="default"){

    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)){

      maxtime_list[[i]] <- max(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                  colnames(soundscape_list[[i]]@aggregated_df),
                  sep=" "),
            format= "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz)))

    }

  }

  else{

    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)){

      maxtime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                maxtime,
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz))

    }
  }

  # 2. Subsetting the soundscapes based on the time and freq arguments

  # Subsetting vectors

  rownames_df <- vector("list", length(soundscape_list))
  rownames_subset <- vector("list", length(soundscape_list))
  colnames_df <- vector("list", length(soundscape_list))
  colnames_subset <- vector("list", length(soundscape_list))
  new_soundscape_list <- vector("list", length(soundscape_list))

  for (i in 1:length(soundscape_list)){

    rownames_df[[i]] <- as.numeric(rownames(soundscape_list[[i]]@aggregated_df))

    rownames_subset[[i]] <- as.character(subset(rownames_df[[i]],
                                                rownames_df[[i]]>=minfreq&
                                                  rownames_df[[i]]<=maxfreq))

    colnames_df[[i]] <- as.POSIXct(strptime(paste(soundscape_list[[i]]@first_day,
                                                  colnames(soundscape_list[[i]]@aggregated_df),sep=" "),
                                            format= "%Y-%m-%d %H:%M:%S",
                                            tz=soundscape_list[[i]]@tz))

    colnames_subset[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                            colnames_df[[i]]
                                                            >=mintime_list[[i]]
                                                            & colnames_df[[i]]
                                                            <=maxtime_list[[i]])))

    new_soundscape_list[[i]] <- soundscape_list[[i]]@aggregated_df[rownames_subset[[i]], colnames_subset[[i]]]

  }

  # 2. Prepare the soundscape list to be in the correct format

  # 2.1. Create site-by-osu df

  site_by_OSU_matrix <-
    as.data.frame(
      t(do.call(rbind,
                lapply(new_soundscape_list,
                       function(x) unlist(x)))))

  rownames(site_by_OSU_matrix) <- paste0("OSU_",
                                         seq(1, nrow(site_by_OSU_matrix), 1))

  colnames(site_by_OSU_matrix) <- as.character(names(soundscape_list))


  # Calculate the pairwise dissimilarities

  if(is.na(hier_table)){

    soundscape_pairdis <- pair_dis_modified(countable = site_by_OSU_matrix,
                                            qvalue = qvalue)

  }

  else{
    soundscape_pairdis <- pair_dis_modified(countable = site_by_OSU_matrix,
                                            qvalue = qvalue,
                                            hierarchy = hier_table)
  }

  soundscape_pairdis


}

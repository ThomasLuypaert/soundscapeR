# Functions for importing and merging index csv files, thresholding and aggregating -------------------------------------------------

# Functions

#' Import and Merge Acoustic Index '.csv' Files
#'
#' @description For an acoustic index of choice, imports all spectral index '.csv' files
#' produced by the \code{\link{index_calc}} function, and merges them into a time-by-frequency dataframe.
#'
#' @param fileloc The full-length path to the directory where the output directories of \code{index_calc} are located.
#' @param samplerate The sampling rate specified in the \code{index_calc} function.
#' @param window The window length specified in the \code{index_calc} function.
#' @param index The acoustic index of interest. Options are "BGN", "PMN", "CVR", "EVN",
#' "ENT", "ACI", "OSC", "SPT", "RHZ", "RVT", "RPS" and "RNS". For a brief description of indices,
#' consult the \code{\link{index_calc}} documentation
#' @param lat The latitude of the site at which the sound files were collected.
#' @param lon The longitude of the site at which the sound files were collected.
#' @return Returns a time-by-frequency dataframe of acoustic index values for all files in the recording period.
#' @export
#'
merge_csv=function(fileloc, samplerate, window, index, lat, lon){
  tz=lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")
  setwd(fileloc)
  folder <- getwd()
  filenames <- list.files(folder, pattern = paste0("\\__Towsey.Acoustic.", index, ".csv$"), recursive = TRUE)
  merged_df <- Reduce(rbind, lapply(filenames, utils::read.csv))
  merged_df <- as.matrix(merged_df)
  merged_df <- t(merged_df)
  merged_df <- as.data.frame(merged_df)
  merged_df <- merged_df[c(2:nrow(merged_df)),]

  if (index=="OSC"){
    frequency_bins <- as.integer(seq(from=(samplerate/2)/(window), to=samplerate/2, by=(((samplerate/2)/(window)))))
  }

 else{
    frequency_bins <- as.integer(seq(from=(samplerate/2)/(window/2), to=samplerate/2, by=(samplerate/2)/(window/2)))
  }

  row.names(merged_df) <- as.integer(frequency_bins)
  colnames(merged_df) <-hms::as_hms(as.POSIXct(substr(filenames, nchar(filenames)-40, nchar(filenames)-26), format= "%Y%m%d_%H%M%S", tz=tz))
  merged_df=merged_df[seq(dim(merged_df)[1],1),]
  merged_df
}

#' Calculate the mode
#'
#' @param df The time-frequency dataframe of index values produced by \code{merge_csv}
#' @return Returns the acoustic index value which appears most often
#' @details Function for internal use by \code{\link{threshold_df}}
#'

get_mode <- function(df) {
  uniqv <- unique(unlist(df))
  uniqv[which.max(tabulate(match(unlist(df), uniqv)))]
}

#' Determine Binarization Threshold
#'
#' @description Determines the threshold for binarization of the
#' time-frequency dataframe of index values. Several binarization algorithms are available, either based on image thresholding
#' algorithms from \code{\link[autothresholdr]{auto_thresh}}, or calculation of the mode.
#'
#' @param df The time-frequency dataframe of index values produced by \code{merge_csv}.
#'
#' @param method The algorithm used to determine the threshold. Options are "IJDefault",
#'  "Huang", "Huang2", "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI", "Minimum",
#'  "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen", and "Mode.
#'  Consult \url{http://imagej.net/Auto_Threshold} for more information on algorithm methodologies.
#'
#' @details Function for internal use by \code{\link{binarize_df}}, however can also be called on by the user manually.
#' @return Returns a numeric threshold value for subsequent binarization.
#' @export
#'
threshold_df=function(df, method){
  if (method=="IJDefault"|method=="Huang"|method=="Huang2"|method=="Intermodes"|method=="IsoData"|method=="Li"|method=="MaxEntropy"|method=="Mean"|method=="MinErrorI"|method=="Minimum"|method=="Moments"|method=="Otsu"|method=="Percentile"|method=="RenyiEntropy"|method=="Shanbhag"|method=="Triangle"|method=="Yen"){
    df2=(as.integer(as.matrix(df)*100000))
    threshold=autothresholdr::auto_thresh(int_arr = df2, method=method)
    threshold=threshold/100000
    threshold
  } else {
    if (method=="Mode"|method=="mode"){
      mode=get_mode(unlist(df))
      mode
    } else {
      print("Error: please supply valid method argument - consult package documentation for options")
    }
  }
}

#' Binarize Dataframe
#'
#' @description Separates acoustically active time-frequency bins (active=1) from background values (inactive=0)
#' through the application of a binarization algorithm. Several binarization algorithms are available,
#' either based on image thresholding using \code{\link[autothresholdr]{auto_thresh}}, or subtraction of the mode.
#'
#' @param df The time-frequency dataframe of index values produced by \code{merge_csv}.
#' @param method The algorithm used to determine the threshold. Options are "IJDefault",
#'  "Huang", "Huang2", "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI", "Minimum",
#'  "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen", and "Mode".
#'  To specify a custom threshold, use method="Custom" in combination with the value argument.
#'  Consult \url{http://imagej.net/Auto_Threshold} for more information on algorithm methodologies.
#' @param value Optional argument used to set a custom threshold value for binarization - used in combination with
#' method="Custom".
#' @param strictness A numeric value with which the threshold value is multiplied. Used for slight tweaking of
#' the threshold value. Reduce the threshold value using values <0, increase using values >1. Defaults to 1.
#'
#' @return Returns a binary time-frequency dataframe of acoustic activity (active=1, inactive=0) for a set acoustic index.
#' @export
#'
binarize_df=function(df, method, value=NULL, strictness=1){

  if (method=="IJDefault"|method=="Huang"|method=="Huang2"|method=="Intermodes"|method=="IsoData"|method=="Li"|method=="MaxEntropy"|method=="Mean"|method=="MinErrorI"|method=="Minimum"|method=="Moments"|method=="Otsu"|method=="Percentile"|method=="RenyiEntropy"|method=="Shanbhag"|method=="Triangle"|method=="Yen"|method=="Mode"|method=="mode"){
    threshold=threshold_df(df=df, method=method)
  }

  else{

    if (method=="custom"){
      threshold=value
    }
  }

  thresh_df=as.data.frame(ifelse(df>(threshold*strictness) ,1, 0))
  colnames(thresh_df)=colnames(df)
  rownames(thresh_df)=rownames(df)
  thresh_df
}


#' Aggregate Dataframe
#'
#' @description Pools binarized spectral indices by time of day using a user-defined aggregation interval.
#' For each aggregate time period, the activity values (0 or 1) are summed per frequency bin, and divided by
#' the number of recordings for that time interval to get the proportion of acoustically active recordings in
#' each time-frequency bin.
#'
#' @param df The binary time-frequency dataframe of acoustic activity (active=1, inactive=0) produced by \code{\link{binarize_df}}
#' @param aggregation The time interval by which binarized spectral index vectors are pooled. Options (in minutes) are 1, 5, 10, 20, 30 and 60.
#' If non-continuous recordings are used, make sure the interval between recordings does not exceed the aggregation interval.
#'
#' @param date The first day of the recording period. Used for managing time-objects in R. Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected.
#' @param lon The longitude of the site at which the sound files were collected.
#' @return Returns an aggregated time-frequency dataframe of the proportion of acoustically active recordings
#' in each bin for a set acoustic index.
#' @export
#'
aggregate_df=function(df, aggregation, date, lat, lon){

  tz=lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")

  times=unique(substr(colnames(df), 1, 2))

  times_2 <- as.list(as.numeric(times))
  names(times_2) <- times

  times_2 <- times_2[order(unlist(times_2), decreasing=FALSE)]

  times <- names(times_2)


  colnames=hms::as_hms(seq.POSIXt(from =min(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))),
                                  to=max(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))),
                                  by=(aggregation*60)))

  list_1=vector("list", 0)
  list_2=vector("list", 0)
  list_3=vector("list", 0)
  list_4=vector("list", 0)
  list_5=vector("list", 0)

  if (aggregation==60)

  {

    for (i in 1:length(times)){
      list_1[[i]]=length(grep(x=substr(colnames(df), 1, 2), pattern = times[i]))
      list_2[[i]]=df[grep(x=substr(colnames(df), 1, 2), pattern = times[i])]
      list_3=lapply(list_2, rowSums)
      list_4=mapply("/", list_3, list_1, SIMPLIFY = FALSE)}

    aggregate_df=do.call(dplyr::bind_rows, list_4)
    aggregate_df=as.data.frame(t(aggregate_df))
    colnames(aggregate_df)=colnames
    #aggregate_df=aggregate_df[seq(dim(aggregate_df)[1],1),] # inverts the order of the rows in the dataframe
    row_names=rownames(df)
    aggregate_df=as.data.frame(aggregate_df)
    row.names(aggregate_df)=row_names
    aggregate_df

  }

  else {

    if (aggregation==30)

    {

      for (i in 1:length(times)){
        list_1[[i]]=df[grep(x=substr(colnames(df), 1, 2), pattern = times[i])]
        subset_1=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))<30)
        subset_2=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>20)
        list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_1])
        list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_2])
      }

      index <- vector(mode = 'logical', length = 0)

      for (i in 1:length(list_2)){
        if (ncol(list_2[[i]])==0){
          index[i]=FALSE
        } else{index[i]=TRUE}
      }

      list_2=list_2[index]

      list_3=lapply(list_2, rowSums)

      for (i in 1:length(list_3)){
        names(list_3[[i]])=rownames(df)
      }

      for (j in 1:length(list_2)){
        list_4[[j]]=ncol(list_2[[j]])}

      list_5=mapply("/", list_3, list_4, SIMPLIFY = FALSE)

      aggregate_df=do.call(dplyr::bind_rows, list_5)
      aggregate_df=as.data.frame(t(aggregate_df))
      colnames(aggregate_df)=colnames
      #aggregate_df=aggregate_df[seq(dim(aggregate_df)[1],1),] # inverts the order of the rows in the dataframe
      row_names=rownames(df)
      aggregate_df=as.data.frame(aggregate_df)
      row.names(aggregate_df)=row_names
      aggregate_df

    }

    else {

      if (aggregation==20)

      {

        for (i in 1:length(times)){
          list_1[[i]]=df[grep(x=substr(colnames(df), 1, 2), pattern = times[i])]
          subset_1=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))<20)
          subset_2=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>=20 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<40)
          subset_3=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>=40)
          list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_1])
          list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_2])
          list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_3])
        }

        index <- vector(mode = 'logical', length = 0)

        for (i in 1:length(list_2)){
          if (ncol(list_2[[i]])==0){
            index[i]=FALSE
          } else{index[i]=TRUE}
        }

        list_2=list_2[index]

        list_3=lapply(list_2, rowSums)

        for (i in 1:length(list_3)){
          names(list_3[[i]])=rownames(df)
        }

        for (j in 1:length(list_2)){
          list_4[[j]]=ncol(list_2[[j]])}

        list_5=mapply("/", list_3, list_4, SIMPLIFY = FALSE)

        aggregate_df=do.call(dplyr::bind_rows, list_5)
        aggregate_df=as.data.frame(t(aggregate_df))
        colnames(aggregate_df)=colnames
        #aggregate_df=aggregate_df[seq(dim(aggregate_df)[1],1),] # inverts the order of the rows in the dataframe
        row_names=rownames(df)
        aggregate_df=as.data.frame(aggregate_df)
        row.names(aggregate_df)=row_names
        aggregate_df

      }

      else {

        if (aggregation==10)

        {

          for (i in 1:length(times)){
            list_1[[i]]=df[grep(x=substr(colnames(df), 1, 2), pattern = times[i])]
            subset_1=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))<10)
            subset_2=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>00 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<20)
            subset_3=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>10 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<30)
            subset_4=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>20 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<40)
            subset_5=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>30 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<50)
            subset_6=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>40)
            list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_1])
            list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_2])
            list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_3])
            list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_4])
            list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_5])
            list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_6])
          }

          index <- vector(mode = 'logical', length = 0)

          for (i in 1:length(list_2)){
            if (ncol(list_2[[i]])==0){
              index[i]=FALSE
            } else{index[i]=TRUE}
          }

          list_2=list_2[index]

          list_3=lapply(list_2, rowSums)

          for (i in 1:length(list_3)){
            names(list_3[[i]])=rownames(df)
          }

          for (j in 1:length(list_2)){
            list_4[[j]]=ncol(list_2[[j]])}

          list_5=mapply("/", list_3, list_4, SIMPLIFY = FALSE)

          aggregate_df=do.call(dplyr::bind_rows, list_5)
          aggregate_df=as.data.frame(t(aggregate_df))
          colnames(aggregate_df)=colnames
          #aggregate_df=aggregate_df[seq(dim(aggregate_df)[1],1),] # inverts the order of the rows in the dataframe
          row_names=rownames(df)
          aggregate_df=as.data.frame(aggregate_df)
          row.names(aggregate_df)=row_names
          aggregate_df

        }

        else {

          if (aggregation==5)

          {

            for (i in 1:length(times)){
              list_1[[i]]=df[grep(x=substr(colnames(df), 1, 2), pattern = times[i])]
              subset_1=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))<05)
              subset_2=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>00 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<10)
              subset_3=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>05 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<15)
              subset_4=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>10 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<20)
              subset_5=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>15 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<25)
              subset_6=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>20 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<30)
              subset_7=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>25 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<35)
              subset_8=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>30 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<40)
              subset_9=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>35 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<45)
              subset_10=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>40 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<50)
              subset_11=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>45 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<55)
              subset_12=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>50)
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_1])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_2])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_3])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_4])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_5])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_6])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_7])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_8])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_9])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_10])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_11])
              list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_12])
            }

            index <- vector(mode = 'logical', length = 0)

            for (i in 1:length(list_2)){
              if (ncol(list_2[[i]])==0){
                index[i]=FALSE
              } else{index[i]=TRUE}
            }

            list_2=list_2[index]

            list_3=lapply(list_2, rowSums)

            for (i in 1:length(list_3)){
              names(list_3[[i]])=rownames(df)
            }

            for (j in 1:length(list_2)){
              list_4[[j]]=ncol(list_2[[j]])}

            list_5=mapply("/", list_3, list_4, SIMPLIFY = FALSE)

            aggregate_df=do.call(dplyr::bind_rows, list_5)
            aggregate_df=as.data.frame(t(aggregate_df))
            colnames(aggregate_df)=colnames
            #aggregate_df=aggregate_df[seq(dim(aggregate_df)[1],1),] # inverts the order of the rows in the dataframe
            row_names=rownames(df)
            aggregate_df=as.data.frame(aggregate_df)
            row.names(aggregate_df)=row_names
            aggregate_df

          }

          else {

            if (aggregation==1)

            {

              for (i in 1:length(times)){
                list_1[[i]]=df[grep(x=substr(colnames(df), 1, 2), pattern = times[i])]
                subset_1=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))<01)
                subset_2=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>00 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<02)
                subset_3=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>01 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<03)
                subset_4=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>02 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<04)
                subset_5=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>03 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<05)
                subset_6=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>04 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<06)
                subset_7=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>05 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<07)
                subset_8=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>06 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<06)
                subset_9=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>07 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<09)
                subset_10=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>08 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<10)
                subset_11=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>09 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<11)
                subset_12=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>10 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<12)
                subset_13=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>11 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<13)
                subset_14=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>12 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<14)
                subset_15=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>13 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<15)
                subset_16=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>14 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<16)
                subset_17=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>15 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<17)
                subset_18=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>16 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<18)
                subset_19=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>17 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<19)
                subset_20=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>18 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<20)
                subset_21=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>19 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<21)
                subset_22=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>20 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<22)
                subset_23=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>21 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<23)
                subset_24=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>22 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<24)
                subset_25=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>23 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<25)
                subset_26=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>24 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<26)
                subset_27=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>25 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<27)
                subset_28=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>26 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<28)
                subset_29=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>27 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<29)
                subset_30=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>28 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<30)
                subset_31=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>29 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<31)
                subset_32=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>30 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<32)
                subset_33=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>31 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<33)
                subset_34=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>32 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<34)
                subset_35=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>33 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<35)
                subset_36=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>34 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<36)
                subset_37=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>35 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<37)
                subset_38=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>36 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<38)
                subset_39=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>37 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<39)
                subset_40=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>38 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<40)
                subset_41=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>39 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<41)
                subset_42=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>40 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<42)
                subset_43=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>41 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<43)
                subset_44=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>42 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<44)
                subset_45=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>43 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<45)
                subset_46=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>44 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<46)
                subset_47=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>45 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<47)
                subset_48=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>46 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<48)
                subset_49=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>47 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<49)
                subset_50=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>48 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<50)
                subset_51=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>49 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<51)
                subset_52=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>50 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<52)
                subset_53=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>51 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<53)
                subset_54=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>52 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<54)
                subset_55=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>53 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<55)
                subset_56=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>54 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<56)
                subset_57=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>55 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<57)
                subset_58=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>56 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<58)
                subset_59=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>57 & as.numeric(substr(colnames(list_1[[i]]), 4, 5))<59)
                subset_60=subset(colnames(list_1[[i]]), as.numeric(substr(colnames(list_1[[i]]), 4, 5))>58)
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_1])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_2])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_3])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_4])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_5])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_6])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_7])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_8])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_9])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_10])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_11])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_12])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_13])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_14])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_15])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_16])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_17])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_18])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_19])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_20])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_21])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_22])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_23])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_24])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_25])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_26])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_27])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_28])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_29])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_30])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_31])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_32])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_33])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_34])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_35])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_36])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_37])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_38])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_39])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_40])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_41])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_42])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_43])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_44])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_45])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_46])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_47])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_48])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_49])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_50])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_51])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_52])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_53])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_54])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_55])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_56])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_57])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_58])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_59])
                list_2[[length(list_2)+1]]=as.data.frame(list_1[[i]][,subset_60])
              }

              index <- vector(mode = 'logical', length = 0)

              for (i in 1:length(list_2)){
                if (ncol(list_2[[i]])==0){
                  index[i]=FALSE
                } else{index[i]=TRUE}
              }

              list_2=list_2[index]

              list_3=lapply(list_2, rowSums)

              for (i in 1:length(list_3)){
                names(list_3[[i]])=rownames(df)
              }

              for (j in 1:length(list_2)){
                list_4[[j]]=ncol(list_2[[j]])}

              list_5=mapply("/", list_3, list_4, SIMPLIFY = FALSE)

              aggregate_df=do.call(dplyr::bind_rows, list_5)
              aggregate_df=as.data.frame(t(aggregate_df))
              colnames(aggregate_df)=colnames
              #aggregate_df=aggregate_df[seq(dim(aggregate_df)[1],1),] # inverts the order of the rows in the dataframe
              row_names=rownames(df)
              aggregate_df=as.data.frame(aggregate_df)
              row.names(aggregate_df)=row_names
              aggregate_df

            }

            else {

              print("Error: invalid aggregation interval - check package documentation for options")

            }
          }
        }
      }
    }
  }
}

#' Normalize index values using min-max normalization
#'
#' @description Normalizes the index values to scale between 0-1 using min-max normalization.
#'
#' @param df The time-frequency dataframe of index values produced by \code{merge_csv}
#'
#' @return Returns a time-by-frequency dataframe of nornalized acoustic index values.
#' @export
#'
normalize_index <- function(df){
  norm_df <- (df - min(df))/(max(df)-min(df))
  return(norm_df)
}

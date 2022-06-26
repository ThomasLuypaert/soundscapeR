
#' Split soundscapes into 24h samples of acoustic trait space
#'
#' @param binarized_soundscape The binarized soundscape object produced by the \code{\link{binarize_df}} function.
#' @param dailyfiles The number of files produced per 24h period the soundscape was sampled. This parameter depends on the sampling regime. The function defaults to 1440, which is the number of 1-min files produced per 24h period using a continuous sampling regime.
#' @param maxdays The maximum number of full 24h periods in the acoustic survey. Defaults to 10 days.
#'
#' @return Returns an updated soundscape object containing the binarized_df, but split per 24h sample of the acoustic trait space.
#' @export
#'
#' @examples
split_df <- function(binarized_soundscape,
                     dailyfiles = 1440,
                     maxdays = 10){

  duration_start <- seq(1, ((dailyfiles*maxdays)-dailyfiles+1), dailyfiles)
  duration_end <- seq(dailyfiles, (dailyfiles*maxdays), dailyfiles)


  sample_list <- vector("list", length = length(duration_end))

  for (i in 1:length(duration_start)){

    sample_list[[i]] <- binarized_soundscape

  }


  names(sample_list) <- seq(1, length(duration_start), 1)


  for (i in 1:length(sample_list)){

    if(ncol(sample_list[[i]]@binarized_df) >= duration_end[i]){

      sample_list[[i]]@merged_df <- sample_list[[i]]@merged_df[,duration_start[i]:duration_end[i]]
      sample_list[[i]]@binarized_df <- sample_list[[i]]@binarized_df[,duration_start[i]:duration_end[i]]

    }

    else{

      sample_list[[i]] <- as.list(c(NA))

    }

  }


  if (length(which(sapply(sample_list, function(x) is.list(x))))==0){

    sample_list <- sample_list

  }

  else{

    sample_list <- sample_list[-which(sapply(sample_list, function(x) is.list(x)))]

  }

  for (i in 1:length(sample_list)){

    sample_list[[i]] <- sample_list[[i]]@binarized_df

  }

  split_soundscape <- methods::new("soundscape",
                                   first_day = binarized_soundscape@first_day,
                                   lat = binarized_soundscape@lat,
                                   lon = binarized_soundscape@lon,
                                   tz = binarized_soundscape@tz,
                                   sunrise = binarized_soundscape@sunrise,
                                   sunset = binarized_soundscape@sunset,
                                   fileloc = binarized_soundscape@fileloc,
                                   index = binarized_soundscape@index,
                                   samplerate = binarized_soundscape@samplerate,
                                   window = binarized_soundscape@window,
                                   binarization_method = binarized_soundscape@binarization_method,
                                   threshold = binarized_soundscape@threshold,
                                   merged_df = binarized_soundscape@merged_df,
                                   binarized_df = binarized_soundscape@binarized_df,
                                   split_df = sample_list)

  split_soundscape

}

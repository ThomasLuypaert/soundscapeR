
#' Calculate and OSU-by-incidence matrix for the soundscape object
#'
#' @param split_soundscape The soundscape object obtained after using the 'split_df' function.
#'
#' @return Returns a soundscape object containing a new OSU-by-incidence matrix
#' @export
osu_matrix <- function(split_soundscape){


inc_mat <- split_soundscape@split_df

for (i in 1:length(inc_mat)){

    inc_mat[[i]] <-
      unlist(inc_mat[[i]])

  }

  inc_mat <- as.data.frame(
    t(data.frame(do.call(rbind,inc_mat))))

  rownames(inc_mat) <- paste0("OSU",
                                   seq(1,
                                       nrow(inc_mat),
                                       1))

  inc_mat <- as.matrix(inc_mat)

  osu_matrix_soundscape <- methods::new("soundscape",
                                   first_day = split_soundscape@first_day,
                                   lat = split_soundscape@lat,
                                   lon = split_soundscape@lon,
                                   tz = split_soundscape@tz,
                                   sunrise = split_soundscape@sunrise,
                                   sunset = split_soundscape@sunset,
                                   fileloc = split_soundscape@fileloc,
                                   index = split_soundscape@index,
                                   samplerate = split_soundscape@samplerate,
                                   window = split_soundscape@window,
                                   binarization_method = split_soundscape@binarization_method,
                                   threshold = split_soundscape@threshold,
                                   merged_df = split_soundscape@merged_df,
                                   binarized_df = split_soundscape@binarized_df,
                                   split_df = split_soundscape@split_df,
                                   osu_matrix = inc_mat)

  osu_matrix_soundscape

}

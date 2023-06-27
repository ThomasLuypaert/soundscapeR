#' @name ss_find_files
#' @title Find all .wav files in a parent directory
#' @description This function takes the path to a parent directory and recursively looks for all directories and subdirectories
#' containing .wav files. The full-length path to each discovered .wav files is saved in a named list maintaining
#' directory structure.
#' @param parent_directory The full-length path to a directory containing .wav files, or a directory containing
#' directories with .wav files. If the latter, this function will look for .wav files recursively in all subdirectories.
#'
#' @return A named list with full-length paths to the '.wav' files contained in the parent directory or subdirectories
#'
#' @export
#'
ss_find_files <- function(parent_directory) {

  directories_with_wavs <- list()

  # Check for .wav files in the parent directory non-recursively
  wav_files <- list.files(parent_directory, pattern = "\\.wav$", full.names = TRUE, recursive = FALSE)

  if (length(wav_files) > 0) {
    # If .wav files are found in the parent directory, add them to the list with the basename of the directory
    directories_with_wavs[[basename(parent_directory)]] <- wav_files
  } else {
    # If no .wav files are found in the parent directory, check subdirectories recursively
    subdirectories <- list.dirs(parent_directory, recursive = FALSE, full.names = TRUE)

    # Use vapply instead of for loop
    subdirectories_with_wavs <- vapply(subdirectories, function(subdir) {
      wav_files <- list.files(subdir, pattern = "\\.wav$", full.names = TRUE, recursive = FALSE)

      if (length(wav_files) > 0) {
        # If .wav files are found in a subdirectory, add the subdirectory path to the list with the basename of the subdirectory
        directories_with_wavs[[basename(subdir)]] <<- wav_files
        return(TRUE)
      }
      return(FALSE)
    }, logical(1))

    # Check no directory or subdirectories have .wav files
    if (!any(subdirectories_with_wavs)) {
      stop("No .wav files found in the input directory or its subdirectories")
    }
  }

  return(directories_with_wavs)

}


#' @name ss_assess_files
#' @title heck and clean the detected folders containing '.wav' files
#' @description This function takes the output list of the `ss_find_files` function and performs several checks on the
#' files. For instance, the function automatically detects the sampling regime of all files, and checks whether the time interval between
#' adjacent files in a folder deviates from the expected sampling regime (e.g. due to missing files). Moreover, the function
#' allows the user to subset the number of files per folder to contain only full sampling days (remove partially sampled days from the study).
#' @param file_locs The output of the `ss_find_files` function
#' @param full_days A boolean operator specifying whether the function should subset the files in each detected folder to full sampling days.
#' Defaults to TRUE.
#'
#' @return A named list with full-length paths to the '.wav' files contained in the parent directory or subdirectories after checking and cleaning.
#'
#' @export
ss_assess_files <- function(file_locs, full_days = TRUE){

  # 1. Find the sampling regime of each folder (median timeinterval between sound files)

  sample_regime <- vector("list")

  for (i in 1:length(file_locs)){

    regime <- sapply(file_locs[[i]],
                     function(x) sub(".*_(\\d{8}_\\d{6})Z\\.wav", "\\1", x))

    regime <- diff(unlist(lapply(regime, function(x) as.POSIXct(strptime(x,
                                                                         "%Y%m%d_%H%M%S",
                                                                         tz = "America/Manaus")))))/60

    median_regime <- stats::median(regime)

    sample_regime[[i]] <- regime

    # 2. Check if any of the files deviate from the expected sampling regime
    # For instance: are some files missing from the expected sequence

    if(any(regime > median_regime)){

      outliers <- c()

      for (j in 2:length(regime)) {
        if (as.numeric(regime[j] - regime[j-1], units = "secs") > median_regime) {
          outliers <- c(outliers, j)
        }
      }

      cat("Irregular timeinterval detected for: ", names(file_locs)[i], "\n")
      cat("Based on the expected sampling regime, there are missing files...")

      cat(basename(file_locs[[i]][outliers]))

      stop("Irregular timeintervals detected - check files")


    }

    else{

      # 3. If no missing files, assess how many full sampling days per site
      # Subset the fileloc list to contain only full days

      files_per_day <- 1440/as.numeric(median_regime)

    }

  }

  if(full_days == TRUE){

  subset_to_closest_multiple <- function(my_list, multiple) {
    # Create a new list to store the subsetted vectors
    my_list_subset <- vector("list", length(my_list))

    # Loop over each vector in the list
    for (i in seq_along(my_list)) {
      # Check if the length of the vector is a multiple of 288
      if (length(my_list[[i]]) %% multiple == 0) {
        my_list_subset[[i]] <- my_list[[i]]  # No need to subset
      } else {
        # Subset the vector to the closest multiple of 288
        num_to_keep <- length(my_list[[i]]) %/% multiple * multiple
        my_list_subset[[i]] <- my_list[[i]][1:num_to_keep]
      }
    }

    return(my_list_subset)
  }

  subset_list <- subset_to_closest_multiple(my_list = file_locs, multiple = files_per_day)

  cat("Sampling regime for study: 1 minute in", median_regime, "minutes \n")
  cat("Number of files per day:", files_per_day, "files \n")
  cat("The file_locs argument has been subsetted to contain only full sampling days")

  return(subset_list)

  }

  else{

    cat("Sampling regime for study: 1 minute in", median_regime, "minutes \n")
    cat("Number of files per day:", files_per_day, "files \n")
    cat("The data still contains partially sampled days - be careful with downstream analysis...")

    return(file_locs)

  }

}


#' @name CVR_computation
#' @title Calculate the 'Acoustic Cover' (CVR) spectral acoustic index for a sound file
#' @description This function is used to calculate the 'Acoustic Cover' spectral acoustic index.
#' @param file The full-length path to a .wav file
#' @param window The window length used for the Fast-Fourier Transformation. Defaults to 256
#' @param theta The cut-off dB-value above which sound is considered present in a frequency bin. Defaults to 3 dB.
#'
#' @return A list of CVR-values
CVR_computation <- function(file, window = 256, theta = 3){

  Time <- Amplitude <- NULL

  # Load sound recording
  sound <- tuneR::readWave(file) # sound_file

  # Extract spectrogram
  raw_spectro <- seewave::spectro(sound,
                                  wl = window, # window
                                  wn = "hamming",
                                  plot = FALSE)$amp

  # Smooth spectrogram with a moving window of 3.
  raw_spectro <- zoo::rollapply(raw_spectro, width = 3, FUN = seewave::meandB, partial = TRUE)

  # Truncate values below -90
  raw_spectro[raw_spectro < -90] <- -90

  # Function to calculate a 100-bin histogram, smooth the values, and return the bin with the largest count.
  dB_mode_per_row <- function(x){
    x <- as.numeric(x)
    seq.100 <- seq(from = min(x), to = max(x), length.out = 100)
    sound_hist <- graphics::hist(as.numeric(x), breaks = seq.100, plot = FALSE)

    # Smooth counts with a moving window of length 5.
    sound_hist$counts <- zoo::rollapply(sound_hist$counts, width = 5, FUN = seewave::meandB, partial = TRUE)

    # Find bin with the largest number of counts.
    Mode <- sound_hist$mids[which.max(sound_hist$counts)]

    return(Mode)
  } # / dB_mode_per_row function

  # Calculate mode of each frequency band using above function
  spectro_mode <- apply(X = raw_spectro, MARGIN = 1, FUN = dB_mode_per_row)

  # Smooth modal values to reduce banding in the spectrogram.
  spectro_mode <- zoo::rollapply(spectro_mode, width = 5, FUN = seewave::meandB, partial = TRUE)

  # Subtract mode from each frequency band of the spectrogram
  spectro_less_mode <- raw_spectro - spectro_mode

  # Truncate negative values to 0
  spectro_less_mode[spectro_less_mode < 0] <- 0

  # Adaptive level equalisation function. Calculates mean dB value of the neighbourhood. If this is less than the threshold theta, replace the central cell with the minimum dB value of the neighbourhood.
  # Assumes a 9 x 3 neighbourhood to match Towsey (2017)
  ad_lev_eq <- function(x){
    x_no_na <- stats::na.omit(x)
    if(length(x_no_na) == 0) {
      return(NA)
    } else if(seewave::meandB(x_no_na) > 3) {
      return(x[[14]])
    } else {
      x[[14]] <- min(x_no_na)
      return(x[[14]])
    }
  } # / ad_lev_eq function

  # Convert the spectrogram matrix to a spatRaster
  temp_rast <- terra::rast(spectro_less_mode)

  # Square sliding window, ART < 10 seconds.
  temp_rast_ale <- terra::focal(temp_rast, w = c(9,3), fun = ad_lev_eq)

  # Extract as dataframe, taking dimensions from the original raw spectrogram. Need to rename column afterwards.
  ale_spectro <- data.frame("Freq" = rep(1:nrow(raw_spectro), each = ncol(raw_spectro)), "Time" = rep(1:ncol(raw_spectro), times = nrow(raw_spectro)), terra::as.data.frame(temp_rast_ale))

  colnames(ale_spectro)[3] <- "Amplitude"

  # Finally, calculate the CVR.
  # Termed the Activity (ACTsp) in Towsey (2017): "The fraction of cells in each noise-reduced frequency bin whose value exceeds the threshold, θ = 3 dB."
  # Pivot to wide format.
  ale_spectro_wide <- tidyr::pivot_wider(ale_spectro, names_from = Time, values_from = Amplitude)

  # Calculate number of cells over 3dB threshold in each row, as a fraction of the total.
  ale_spectro_wide$CVR_index <- rowSums(ale_spectro_wide > 3) / (ncol(ale_spectro_wide) - 1)

  return(ale_spectro_wide$CVR_index)

}


#' @name ss_index_calc
#' @title Calculate the 'Acoustic Cover' (CVR) spectral acoustic index values for a folder of sound files
#' @description For a given folder of sound files, this function is used to calculate the CVR-index for each 1-min file.
#' @param file_list A list containing the full-length path to each '.wav' file in a folder. This list can be obtained using the
#' `ss_find_files` and `ss_assess_files` functions.
#' @param output_dir The full-length path to an output directory on your device. If not specified, will default to the location where
#' the raw sound files are saved.
#' @param window The window length used for the Fast-Fourier Transformation. Defaults to 256
#' @param theta The cut-off dB-value above which sound is considered present in a frequency bin. Defaults to 3 dB.
#' @param parallel A boolean operator indicating whether parallel processing should be used to calculate the CVR-index files.
#' Defaults to FALSE.
#'
#' @return A list of CVR-values for each sound file
#' @export
ss_index_calc <- function(file_list,
                          output_dir = NA,
                          window = 256,
                          theta = 3,
                          parallel = FALSE) {

  Time <- Amplitude <- i <- NULL

  # Make an output folder

  if(is.na(output_dir)){

    base_dir <- dirname(file_list[1])

    output_path <- file.path(base_dir, window)

  }


  else{

    output_path <- file.path(output_dir, window)

  }

  if(dir.exists(output_path)){

    output_existing <- list.files(path = output_path,
                                  pattern = "CVR.csv")

    output_expected <- paste0(gsub(x = basename(file_list),pattern = ".wav", replacement = "") , "_CVR.csv")

    if(length(list.files(path = output_path,
                         pattern = "CVR.csv")) > 0){

      file_list_new <- file_list[!sapply(output_expected, function(x) x %in% output_existing)]

      cat("An output folder with CVR output files is already present - continuing the calculation for missing output files only... \n")
      cat(paste0("Computing ", length(file_list_new), " missing CVR output files out of ", length(file_list), " detected sound files... \n"))
      Sys.sleep(0.0001)


    }

    else{file_list_new <- file_list}

  }

  else{dir.create(output_path)}



  # Calculate CVR-index files

  if(parallel == TRUE){

    n_cores <- parallel::detectCores()-2

    n_files <- length(file_list_new)

    # Initialize parallel processing

    cl <- parallel::makeCluster(n_cores)
    doSNOW::registerDoSNOW(cl)

    # progress bar

    pb <- progress::progress_bar$new(
      format = "Progress = :perc [:bar] Elapsed = :elapsed | Remaining= :eta",
      total = n_files,
      width = 60)

    # allowing progress bar to be used in foreach -----------------------------
    progress <- function(n){
      pb$tick()
    }

    opts <- list(progress = progress)

    # Loop through each file and perform fft in parallel

    requireNamespace(foreach)

    CVR_list <- foreach::foreach(i = 1:n_files, .packages = c("tuneR", "seewave", "soundscapeR"),
                                 .options.snow = opts) %dopar% {

                                   sound <- tuneR::readWave(file_list_new[i]) # sound_file

                                   # Extract spectrogram
                                   raw_spectro <- seewave::spectro(sound,
                                                                   wl = window, # window
                                                                   wn = "hamming",
                                                                   plot = FALSE)$amp

                                   # Smooth spectrogram with a moving window of 3.
                                   raw_spectro <- zoo::rollapply(raw_spectro, width = 3, FUN = seewave::meandB, partial = TRUE)

                                   # Truncate values below -90
                                   raw_spectro[raw_spectro < -90] <- -90

                                   # Function to calculate a 100-bin histogram, smooth the values, and return the bin with the largest count.
                                   dB_mode_per_row <- function(x){
                                     x <- as.numeric(x)
                                     seq.100 <- seq(from = min(x), to = max(x), length.out = 100)
                                     sound_hist <- graphics::hist(as.numeric(x), breaks = seq.100, plot = FALSE)

                                     # Smooth counts with a moving window of length 5.
                                     sound_hist$counts <- zoo::rollapply(sound_hist$counts, width = 5, FUN = seewave::meandB, partial = TRUE)

                                     # Find bin with the largest number of counts.
                                     Mode <- sound_hist$mids[which.max(sound_hist$counts)]

                                     return(Mode)
                                   } # / dB_mode_per_row function

                                   # Calculate mode of each frequency band using above function
                                   spectro_mode <- apply(X = raw_spectro, MARGIN = 1, FUN = dB_mode_per_row)

                                   # Smooth modal values to reduce banding in the spectrogram.
                                   spectro_mode <- zoo::rollapply(spectro_mode, width = 5, FUN = seewave::meandB, partial = TRUE)

                                   # Subtract mode from each frequency band of the spectrogram
                                   spectro_less_mode <- raw_spectro - spectro_mode

                                   # Truncate negative values to 0
                                   spectro_less_mode[spectro_less_mode < 0] <- 0

                                   # Adaptive level equalisation function. Calculates mean dB value of the neighbourhood. If this is less than the threshold theta, replace the central cell with the minimum dB value of the neighbourhood.
                                   # Assumes a 9 x 3 neighbourhood to match Towsey (2017)
                                   ad_lev_eq <- function(x){
                                     x_no_na <- stats::na.omit(x)
                                     if(length(x_no_na) == 0) {
                                       return(NA)
                                     } else if(seewave::meandB(x_no_na) > 3) {
                                       return(x[[14]])
                                     } else {
                                       x[[14]] <- min(x_no_na)
                                       return(x[[14]])
                                     }
                                   } # / ad_lev_eq function

                                   # Convert the spectrogram matrix to a spatRaster
                                   temp_rast <- terra::rast(spectro_less_mode)

                                   # Square sliding window, ART < 10 seconds.
                                   temp_rast_ale <- terra::focal(temp_rast, w = c(9,3), fun = ad_lev_eq)

                                   # Extract as dataframe, taking dimensions from the original raw spectrogram. Need to rename column afterwards.
                                   ale_spectro <- data.frame("Freq" = rep(1:nrow(raw_spectro), each = ncol(raw_spectro)), "Time" = rep(1:ncol(raw_spectro), times = nrow(raw_spectro)), terra::as.data.frame(temp_rast_ale))

                                   colnames(ale_spectro)[3] <- "Amplitude"

                                   # Finally, calculate the CVR.
                                   # Termed the Activity (ACTsp) in Towsey (2017): "The fraction of cells in each noise-reduced frequency bin whose value exceeds the threshold, θ = 3 dB."
                                   # Pivot to wide format.
                                   ale_spectro_wide <- tidyr::pivot_wider(ale_spectro, names_from = Time, values_from = Amplitude)

                                   # Calculate number of cells over 3dB threshold in each row, as a fraction of the total.
                                   ale_spectro_wide$CVR_index <- rowSums(ale_spectro_wide > 3) / (ncol(ale_spectro_wide) - 1)

                                   utils::write.csv(x = t(as.data.frame(ale_spectro_wide$CVR_index)),file = paste0(output_path, "/", gsub(x = basename(file_list_new[i]),pattern = ".wav", replacement = "") , "_CVR.csv"))

                                   return(ale_spectro_wide$CVR_index)

                                 }

    names(CVR_list) <- basename(file_list_new)

    parallel::stopCluster(cl)
    gc()

  }

  else{

    CVR_list <- vector("list", length(file_list_new))

    for(j in 1:length(file_list_new)){

      print(paste0(round(((j/length(file_list_new))*100), digits = 1), " %"))
      Sys.sleep(0.00000000000001)

      CVR_list[[j]] <- CVR_computation(file = file_list_new[j],
                                       window = window,
                                       theta = theta)

      utils::write.csv(x = t(as.data.frame(CVR_list[[j]])),file = paste0(output_path, "/", gsub(x = basename(file_list_new[j]),pattern = ".wav", replacement = "") , "_CVR.csv"))

    }

    names(CVR_list) <- basename(file_list_new)

  }

  return(CVR_list)


}






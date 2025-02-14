# nocov start

#' @name ss_find_files
#' @title Find all sound files in a parent directory. Allowed formats are '.wav', '.wac', '.mp3' and '.flac'.
#' @description This function takes the path to a parent directory and recursively looks for all directories and subdirectories
#' containing sound files. The full-length path to each discovered sound file is saved in a named list maintaining
#' directory structure.
#' @param parent_directory The full-length path to a directory containing sound files, or a directory containing
#' directories with sound files. If the latter, this function will look for sound files recursively in all subdirectories.
#'
#' @return A named list with full-length paths to the sound files contained in the parent directory or subdirectories
#'
#' @export
#'
#' @examples
#'
#' # Get file path to sound files
#' fpath <- system.file("extdata", package = "soundscapeR")
#'
#' # Finding files
#' file_locs <- ss_find_files(parent_directory = fpath)
#'
ss_find_files <- function(parent_directory) {

  directories_with_sounds <- list()

  # Check for sound files in the parent directory non-recursively
  sound_files <- list.files(parent_directory, "\\.(wav|mp3|flac|wac|WAV|MP3|FLAC|WAC)$", full.names = TRUE, recursive = FALSE)

  if (length(sound_files) > 0) {
    # If sound files are found in the parent directory, add them to the list with the basename of the directory
    directories_with_sounds[[basename(parent_directory)]] <- sound_files
  } else {
    # If no sound files are found in the parent directory, check subdirectories recursively
    subdirectories <- list.dirs(parent_directory, recursive = FALSE, full.names = TRUE)

    # Use vapply instead of for loop
    subdirectories_with_sounds <- vapply(subdirectories, function(subdir) {
      sound_files <- list.files(subdir, pattern = "\\.(wav|mp3|flac|wac|WAV|MP3|FLAC|WAC)$", full.names = TRUE, recursive = FALSE)

      if (length(sound_files) > 0) {
        # If sound files are found in a subdirectory, add the subdirectory path to the list with the basename of the subdirectory
        directories_with_sounds[[basename(subdir)]] <<- sound_files
        return(TRUE)
      }
      return(FALSE)
    }, logical(1))

    # Check no directory or subdirectories have sound files
    if (!any(subdirectories_with_sounds)) {
      stop("No sound files found in the input directory or its subdirectories")
    }
  }

  return(directories_with_sounds)
}

#' @name ss_convert_files
#' @title Check if the sound files are in the '.wav' file format, if not, convert the files and save them.
#' @description This function takes the output list of the `ss_find_files` function and checks whether the sound files in the folder
#' are of the format '.wav'. If not, the function will covert the sound files to a '.wav' format.
#' @param file_locs The output of the `ss_find_files` function
#' @param replace A boolean operator to indicate whether the original non-wav file should be replaced (replace = TRUE) with its .wav file equivalent.
#' Note that this will lead to original file to be deleted, so make sure you have a back-up of the original file. Defaults to replace = FALSE.
#' @param verbose A boolean operator to indicate how chatty the function should be. Using verbose = TRUE leads to increased messaging on what is happening
#' to the files in file_locs.
#'
#' @return In case some files are not in the '.wav' format, creates a new directory called 'wav_files' containing the sound files converted to '.wav' format.
ss_convert_files <- function(file_locs, replace = FALSE, verbose = FALSE){

  # 1. Grab file extensions for the sound files in file_locs

  filelocs <- unlist(file_locs)

  extensions <- sapply(filelocs, function(x) tolower(tools::file_ext(x)))

  # 2. Check if any of the file extensions or not .wav or .WAV

  if(any(!extensions %in% c("wav", "WAV"))){

    # 2.1. Grab the files that are not wav

    not_wav <- filelocs[which(!extensions %in% c("wav", "WAV"))]
    not_wav_ext <- extensions[which(!extensions %in% c("wav", "WAV"))]

    cli::cli_h1("1. Found {length(not_wav)} non-wav files: CONVERTING")

    # 2.2. Convert different formats to wav

    for (i in 1:length(not_wav_ext)){

      # MP3 files

      if (not_wav_ext[i] %in% c("mp3", "MP3")){

        if(verbose == TRUE){

          cli::cli_h2("Converting {basename(not_wav[i])}")

          cli::cli_alert_info("Converting {basename(not_wav_ext[i])} from .mp3 to .wav")

        }

        wav_file <- tuneR::readMP3(filename = not_wav[i])

        new_filename <- gsub(pattern = "MP3", replacement = ".wav", gsub(pattern = ".mp3", replacement = ".wav", not_wav[i]))

        tuneR::writeWave(object = wav_file, filename = new_filename)

        if(verbose == TRUE){
          cli::cli_alert_success("Conversion successful!")
        }

        # Delete original file if replace == TRUE

        if(replace == TRUE){

          file.remove(not_wav[i])

          if(verbose == TRUE){
            cli::cli_alert_success("Removed original file...")

          }

        }

      }

      # WAC files

      if (not_wav_ext[i] %in% c("wac", "WAC")){

        if(verbose == TRUE){

          cli::cli_h2("Converting {basename(not_wav[i])}")

          cli::cli_alert_info("Converting {basename(not_wav_ext[i])} from .wac to .wav")

        }

        wav_file <- bioacoustics::read_wac(file = not_wav_ext[i])
        new_filename <- gsub(pattern = c(".wac", ".WAC"), replacement = ".wav", not_wav[i])
        tuneR::writeWave(object = wav_file, filename = new_filename)

        if(verbose == TRUE){

          cli::cli_alert_success("Conversion successful!")

        }

        # Delete original file if replace == TRUE

        if(replace == TRUE){

          file.remove(not_wav[i])

          if(verbose == TRUE){
            cli::cli_alert_success("Removed original file...")

          }

        }

      }

    }

    # FLAC files

    if (any(not_wav_ext %in% c("flac", "FLAC"))){

      flac_count <- sum((not_wav_ext %in% c("flac", "FLAC")))

      cli::cli_alert_danger("Found {flac_count} '.flac' files. Conversion of FLAC files currently not working...")
      cli::cli_alert_info("For bulk conversion of flac files, consider using: cloudconvert.com/flac-to-wav")


      # cli::cli_alert_warning("To read FLAC files in R, an external software needs to be installed: xiph.org/flac/index.html")
      # cli::cli_alert_warning("This function will only work if the flac software is installed in the default directory (e.g. for Windows C:/Program Files/FLAC), otherwise an error will be thrown")
      #
      # cli::cli_alert_info("Attempting to convert {basename(not_wav_ext[i])} from .flac to .wav")
      #
      # wav_file <- tryCatch({
      #
      # warbleR::read_sound_file(X = not_wav_ext[i])
      #
      # }, error = function(e) {cli::cli_alert_danger("FLAC software not installed or found, skipping conversion of flac files...")})
      #
      # new_filename <- gsub(pattern = c(".flac", ".FLAC"), replacement = ".wav", not_wav[i])
      # tuneR::writeWave(object = wav_file, filename = new_filename)
      #
      # cli::cli_alert_success("Conversion successful!")

    }

  }

  else{

    cli::cli_alert_success("All sound files already in .wav format")

  }

}

#' @name get_wav_duration
#' @title Check the duration of a sound file
#' @description This function takes the full length path to a sound file and returns its duration in seconds.
#' @param file_path A full length path to the location of a sound file.
#' @keywords internal
#'
#' @return The duration of a .wav sound file in seconds.
#'
get_wav_duration <- function(file_path) {

  file_info <- tuneR::readWave(filename = file_path, header = TRUE)

  duration <- round(file_info$samples / file_info$sample.rate)

  return(duration)
}

#' @name ss_split_files
#' @title Check if a file is the correct length, and if not, split it into 60-second chunks.
#' @description This function takes the output list of the `ss_find_files` function and checks whether the sound files in the folder
#' are the correct length (60 seconds). If not, the function splits the sound files into 60-second chunks and saves the files in a new
#' sub-directory called 'split_files'.
#' @param file_locs The output of the `ss_find_files` function
#' @param verbose A boolean operator to indicate how chatty the function should be. Using verbose = TRUE leads to increased messaging on what is happening
#' to the files in file_locs.
#'
#' @return In case some files are longer than 60 seconds, creates a new directory called 'split_files' containing the sound files split into 60-second chunks.
#'
ss_split_files <- function(file_locs, verbose = FALSE) {

  # 1. CHECK IF ANY SOUND FILES NEED SPLITTING

  # soundfiles <- lapply(file_locs, function(x) sapply(x, function(y) tuneR::readWave(y)))


  soundfiles_length <- lapply(file_locs, function(x) sapply(x, function(y) get_wav_duration(y)))


  if (any(unlist(soundfiles_length) > 60)) {

    cli::cli_h1("2. Detected files > 60 seconds: SPLITTING")

    # Check if any sound files have a length not divisible by 60

    if (any(unlist(soundfiles_length) %% 60 != 0)) {

      cli::cli_alert_warning("One or more sound files have a length not divisible by 60, could lead to partial information loss after splitting")
      cli::cli_alert_warning("{ unlist(file_locs)[which(unlist(soundfiles_length) %% 60 != 0)]}")
    }

    needs_splitting <- file_locs[which(sapply(soundfiles_length, function(x) any(x > 60)))]
    wav_duration <- soundfiles_length[which(sapply(soundfiles_length, function(x) any(x > 60)))]

    for (i in 1:length(needs_splitting)){

      needs_splitting[[i]] <- needs_splitting[[i]][soundfiles_length[[i]]>60]
      wav_duration[[i]] <- wav_duration[[i]][soundfiles_length[[i]]>60]

    }


    wav_splitting <- lapply(needs_splitting, function(x) sapply(x, function(y) tuneR::readWave(y)))


    for (i in seq_along(needs_splitting)) {

      dirname <- basename(dirname(needs_splitting[[i]][1]))

      if(verbose == TRUE){

        cli::cli_alert_info("Splitting files for {dirname}")

      }

      # Create new directory for split sound files

      split_dir <- file.path(dirname(needs_splitting[[i]][1]), "split_files")
      dir.create(split_dir, showWarnings = FALSE)

      for (j in 1:length(needs_splitting[[i]])) {
        # Read the sound file

        audio <- wav_splitting[[i]][[j]]

        # Extract date/time information

        file_name <- basename(needs_splitting[[i]][j])
        name_parts <- unlist(strsplit(file_name, "_"))
        site_name <- name_parts[1]
        date <- name_parts[2]
        time <- name_parts[3]

        base_datetime <- as.POSIXct(paste0(date, time), format = "%Y%m%d%H%M%S")

        # Calculate number of chunks

        total_duration <- wav_duration[[i]][j]

        num_chunks <- ceiling(total_duration / 60)

        # Cut into 60-second chunks

        chunks <- list()

        for (k in 1:num_chunks) {
          start_sec <- (k - 1) * 60
          end_sec <- min(k * 60, total_duration)
          chunk <- audio[(start_sec * audio@samp.rate + 1):(end_sec * audio@samp.rate)]

          # Update datetime naming

          updated_datetime <- base_datetime + (k - 1) * 60
          updated_date <- format(updated_datetime, "%Y%m%d")
          updated_time <- format(updated_datetime, "%H%M%S")

          updated_filename <- paste0(site_name, "_", updated_date, "_", updated_time, ".wav")

          tuneR::writeWave(chunk, file.path(split_dir, updated_filename))
        }
      }
    }

    cli::cli_alert_success("Files successfully split, new files can be found in the sub-directory called 'split_files'")
  } else {
    if (any(unlist(soundfiles_length) < 60)) {
      cli::cli_abort("Detected sound files with a duration shorter than 60 seconds - cannot be handled by soundscapeR, aborting...")
    } else {
      cli::cli_alert_success("All sound files are 60 seconds, no splitting required!")
    }
  }
}


#' @name ss_assess_files
#' @title Check and clean the detected folders containing sound files
#' @description This function takes the output list of the `ss_find_files` function and performs several checks on the
#' files. Firstly, the function checks whether all the sound files in the file_locs are of the format '.wav'. If not, the function tries
#' to convert the sound files to a '.wav' format. Note that, at present,`soundscapeR` doesn't accommodate file conversion from the '.flac' format
#' due to its reliance on external software. Converted files will be saved in the same folder as their non-wav file equivalents.
#' Second, the function will check whether all the '.wav' files in file_locs have the required sixty-second length used for index computation.
#' Finally, the function automatically detects the sampling regime of all files, and checks whether the time interval between
#' adjacent files in a folder deviates from the expected sampling regime (e.g. due to missing files). Moreover, the function
#' allows the user to subset the number of files per folder to contain only full sampling days (remove partially sampled days from the study).
#' @param file_locs The output of the `ss_find_files` function
#' @param full_days A boolean operator specifying whether the function should subset the files in each detected folder to full sampling days.
#' Defaults to TRUE.
#' @param replace A boolean operator to indicate whether the original non-wav file should be replaced (replace = TRUE) with its .wav file equivalent.
#' Note that this will lead to original file to be deleted, so make sure you have a back-up of the original file. Defaults to replace = FALSE.
#' @param check_filedur A boolean operator to indicate if you want to check the duration of your sound file, and split files if the file length
#' exceeds 60 seconds per file. The soundscapeR package expects sound files to be of 60 second duration. This step can be time-consuming, so this step is turned off by default.
#' @param verbose A boolean operator to indicate how chatty the function should be. Using verbose = TRUE leads to increased messaging on what is happening
#' to the files in file_locs.
#'
#' @return A named list with full-length paths to the '.wav' files contained in the parent directory or subdirectories after checking and cleaning.
#'
#' @export
#'
#' @examples
#'
#' # File prepration
#' fpath <- system.file("extdata", package = "soundscapeR")
#' file_locs <- ss_find_files(parent_directory = fpath)
#'
#' # No subsetting
#' file_locs_clean <- ss_assess_files(file_locs = file_locs, full_days = TRUE)
#'
#' # Subsetting to full days only
#' file_locs_clean <- ss_assess_files(file_locs = file_locs, full_days = TRUE)
#'
ss_assess_files <- function(file_locs, replace = FALSE, check_filedur = FALSE, full_days = TRUE, verbose = FALSE) {

  #1. Check if the sound files in file_locs are all in .wav format

  ss_convert_files(file_locs = file_locs, replace = replace, verbose = verbose)

  file_locs_new <- lapply(file_locs, function(x) dirname(x[1]))

  for (i in 1:length(file_locs)){

    file_locs_new[[i]] <- list.files(file_locs_new[[i]],
                                     "\\.(wav|WAV)$",
                                     full.names = TRUE,
                                     recursive = FALSE)

  }

  names(file_locs_new) <- unlist(lapply(file_locs_new, function(x) basename(dirname(x[1]))))


  #2. Check if the sound files in file_locs are all of 60-second length

  if (check_filedur == TRUE){

    ss_split_files(file_locs = file_locs_new, verbose = verbose)

  }

  file_locs_new_2 <- lapply(file_locs_new, function(x) dirname(x[1]))

  if(any(unlist(lapply(file_locs_new_2, function(x) dir.exists(file.path(x, "split_files")))))){

    file_locs_split <- which(unlist(lapply(file_locs_new_2, function(x) dir.exists(file.path(x, "split_files")))))

    for (i in seq_along(file_locs_split)){

      file_locs_new_2[i] <- paste0(file_locs_new_2[i], "/split_files")

    }

  }

  for (i in 1:length(file_locs_new_2)){

    file_locs_new_2[[i]] <- list.files(file_locs_new_2[[i]],
                                       "\\.(wav|WAV)$",
                                       full.names = TRUE,
                                       recursive = FALSE)

  }


  # 2. Find the sampling regime of each folder (median timeinterval between sound files)

  cli::cli_h1("3. Testing sampling regime")

  sample_regime <- vector("list")

  files_per_day <- vector("list")

  to_remove <- vector("list", length = 0)

  for (i in 1:length(file_locs_new_2)) {

    # Check the sampling regime per directory in file_llocs


    regime <- sapply(
      file_locs_new_2[[i]],
      function(x) sub(".*?(\\d{8}_\\d{6})(Z)?\\.[wW][aA][vV]", "\\1", x)
    )


    regime <- diff(unlist(lapply(regime, function(x) {
      as.POSIXct(strptime(x,
                          "%Y%m%d_%H%M%S",
                          tz = "America/Manaus"
      ))
    }))) / 60

    median_regime <- stats::median(regime)

    sample_regime[[i]] <- regime

    files_per_day[[i]] <- 1440 / as.numeric(median_regime)

    if(verbose == TRUE){

      cli::cli_h2("Processing {basename(dirname(file_locs_new_2[[i]][1]))}:")

      if(any(is.na(regime))){

        cli::cli_alert_danger("Sampling regime could not be detected for one or more files due to incorrect file naming.")

      }

      else{
        cli::cli_alert_info("Sampling regime for study: 1 minute in {median_regime} minutes")
        cli::cli_alert_info("Number of files per day: {files_per_day[[i]]} files")

      }

    }

    # 2. Check if any of the files deviate from the expected sampling regime
    # For instance: are some files missing from the expected sequence


    if(any(is.na(regime))){

      to_remove <- append(x = to_remove, values = i)
      cli::cli_alert_warning("{names(file_locs_new_2)[i]} will be removed from the clean files list due issues with file timestamps.")
      next

    }


    if (any(regime != median_regime)) {

      problematic_files <- basename(file_locs_new_2[[i]][which(regime != median_regime)])

      fileloc_names <- names(file_locs_new_2)[i]

      cli::cli_alert_danger("Irregular timeinterval detected for: {fileloc_names}")
      cli::cli_alert_info("Based on the expected sampling regime, there are missing or irregular files...")

      if (length(problematic_files) > 0) {

        cli::cli_alert("Problematic files: {paste(problematic_files, collapse = ', ')}")  # Format output
      } else {
        cli::cli_alert_warning("No specific missing files could be identified.")
      }

      to_remove <- append(x = to_remove, values = i)
      cli::cli_alert_warning("{names(file_locs_new_2)[i]} will be removed from the clean files list due to a irregular sampling regime")
      next

      #cli::cli_abort("Irregular timeintervals detected - check files")
    }
  }

  # Remove directories that have wrong file naming

  cli::cli_alert_info("{to_remove}")

  if(length(to_remove) > 0){

    file_locs_new_2 <- file_locs_new_2[-unlist(to_remove)]
    files_per_day <- files_per_day[-unlist(to_remove)]

  }


  if (full_days == TRUE) {

    subset_to_closest_multiple <- function(my_list, multiple) {
      # Create a new list to store the subsetted vectors
      my_list_subset <- vector("list", length(my_list))

      # Loop over each vector in the list
      for (i in 1:length(my_list)) {
        # Check if the length of the vector is a multiple of 288
        if (length(my_list[[i]]) %% multiple[[i]] == 0) {
          my_list_subset[[i]] <- my_list[[i]] # No need to subset
        } else {
          # Subset the vector to the closest multiple of 288
          num_to_keep <- length(my_list[[i]]) %/% multiple[[i]] * multiple[[i]]
          my_list_subset[[i]] <- my_list[[i]][1:num_to_keep]
        }
      }

      return(my_list_subset)
    }

    subset_list <- subset_to_closest_multiple(my_list = file_locs_new_2, multiple = files_per_day)
    cli::cli_alert_success("The file_locs argument has been subsetted to contain only full sampling days")

    return(subset_list)

  } else {
    cli::cli_alert_warning("The data still contains partially sampled days - be careful with downstream analysis...")

    return(file_locs)
  }
}

CVR_computation <- function(file, window = 256, theta = 3, threshold = 3) {
  # Define C++ functions

  # mean dB for rows in matrix

  Rcpp::cppFunction(
    "NumericMatrix rollapply_meandB(NumericMatrix x, int width) {
  int rows = x.nrow();
  int cols = x.ncol();
  NumericMatrix out(rows, cols);

  for(int k = 0; k < cols; k++) {
    NumericVector column = x(_, k);

    for(int i = 0; i < rows; i++) {
      // Center alignment: width should be centered around the current point
      int start = std::max(0, i - width / 2);
      int end = std::min(i + width / 2, rows - 1); // ensure the end index does not exceed the length of the column
      NumericVector window = column[Range(start, end)];

      // Handling NA values
      if (std::any_of(window.begin(), window.end(), [](double v) { return NumericVector::is_na(v); })) {
        out(i, k) = NA_REAL;
        continue;
      }

      double sum = 0.0;
      for(int j = 0; j < window.size(); j++) {
        sum += pow(10.0, window[j] / 10.0);
      }

      double meanDB = 10 * log10(sum / window.size());
      out(i, k) = meanDB;
    }
  }

  return out;
}"
  )

  # mean dB for a vector
  Rcpp::cppFunction(
    "NumericVector rollapply_meandB_vector(NumericVector x, int width) {
  int n = x.size();
  NumericVector out(n);

  for(int i = 0; i < n; i++) {
    // Center alignment: width should be centered around the current point
    int start = std::max(0, i - width / 2);
    int end = std::min(i + width / 2, n - 1);
    NumericVector window = x[Range(start, end)];

    // Handling NA values
    if (std::any_of(window.begin(), window.end(), [](double v) { return NumericVector::is_na(v); })) {
      out[i] = NA_REAL;
      continue;
    }

    double sum = 0.0;
    for(int j = 0; j < window.size(); j++) {
      sum += pow(10.0, window[j] / 10.0);
    }

    double meanDB = 10 * log10(sum / window.size());
    out[i] = meanDB;
  }

  return out;
}"
  )

  # Adaptive Level Equalisation (Towsey 2017)

  Rcpp::cppFunction(
    "NumericMatrix adaptive_level_equalisation(NumericMatrix x, int windowRowSize, int windowColSize, double ALE_theta) {
  int rows = x.nrow();
  int cols = x.ncol();
  NumericMatrix out(rows, cols);

  // Initialize output matrix with NA values
  std::fill(out.begin(), out.end(), NumericVector::get_na());

  // Create padded matrix
  NumericMatrix padded(rows + windowRowSize - 1, cols + windowColSize - 1);
  int rowPad = (windowRowSize - 1) / 2;
  int colPad = (windowColSize - 1) / 2;
  for(int i = 0; i < rows; i++) {
    for(int j = 0; j < cols; j++) {
      padded(i + rowPad, j + colPad) = x(i, j);
    }
  }

  // Allocate window vector once
  NumericVector window(windowRowSize * windowColSize);

  for(int i = 0; i < rows; i++) {
    for(int j = 0; j < cols; j++) {
      int windowIndex = 0;
      for(int k = 0; k < windowRowSize; k++) {
        for(int l = 0; l < windowColSize; l++) {
          double value = padded(i + k, j + l);
          // Assume no NA values
          window[windowIndex++] = value;
        }
      }

      if(windowIndex > 0) {
        // Calculate meanDB
        double sum = std::accumulate(window.begin(), window.begin() + windowIndex, 0.0, [](double a, double b) {
          return a + std::pow(10.0, b / 10.0);
        });
        double meanDB = 10.0 * std::log10(sum / windowIndex);

        // Apply threshold theta
        out(i, j) = meanDB > ALE_theta ? x(i, j) : *std::min_element(window.begin(), window.begin() + windowIndex);
      }
    }
  }

  return out;
}"
  )

  # Define R functions

  # dB mode per row, uses rollapply_meandB_vector C++ function complied above.
  dB_mode_per_row <- function(x, width = 5) {
    x <- as.numeric(x)
    seq_100 <- seq(from = min(x), to = max(x), length.out = 100)
    sound_hist <- graphics::hist(as.numeric(x), breaks = seq_100, plot = FALSE)

    # Smooth counts with a moving window
    sound_hist$counts <- rollapply_meandB_vector(sound_hist$counts, width)

    # Find bin with the largest number of counts.
    mode <- sound_hist$mids[which.max(sound_hist$counts)]

    return(mode)
  }

  # Load sound recording
  sound <- tuneR::readWave(file)

  # Extract spectrogram
  raw_spectro <- seewave::spectro(sound,
                                  wl = window,
                                  wn = "hamming",
                                  plot = FALSE
  )$amp

  # Smooth spectrogram with a moving window of 3. Truncating values below -90
  raw_spectro <- pmax(rollapply_meandB(raw_spectro, width = 3), -90)

  # Calculate mode of each frequency band using above function.
  spectro_mode <- apply(raw_spectro, 1, FUN = dB_mode_per_row)

  # Smooth the mode using a moving average filter
  spectro_mode <- rollapply_meandB_vector(spectro_mode, width = 5)

  # Subtract the resulting background noise values from the values in each frequency bin. Truncate negative values to zero.
  spectro_less_mode <- pmax(raw_spectro - spectro_mode, 0)

  # For each neighbourhood (3 frames C 9 frequency bins) centred on any element/pixel in the spectrogram, calculate the average spectrogram value, D.
  # If D is less than a user determined threshold, N8, set the value of the central element/pixel equal to the minimum in the neighbourhood
  ale_matrix <- adaptive_level_equalisation(spectro_less_mode, windowRowSize = 9, windowColSize = 3, ALE_theta = 3)


  # Finally, calculate the CVR.
  # Termed the Activity (ACTsp) in Towsey (2017): "The fraction of cells in each noise-reduced frequency bin whose value exceeds the threshold, N8 = 3 dB."
  # Calculate number of cells over 3dB threshold in each row, as a fraction of the total.
  CVR_index <- rowSums(ale_matrix > threshold) / (ncol(ale_matrix))

  return(CVR_index)
}



#' @name ss_index_calc
#' @title Calculate the 'Acoustic Cover' (CVR) spectral acoustic index values for a folder of sound files
#' @description For a given folder of sound files, this function is used to calculate the CVR-index for each 1-min file.
#' @param file_list A list containing the full-length path to each '.wav' file in a folder. This list can be obtained using the
#' `ss_find_files` and `ss_assess_files` functions.
#' @param output_dir The full-length path to an output directory on your device. If not specified, will default to the location where
#' the raw sound files are saved.
#' @param window The window length used for the Fast-Fourier Transformation. Defaults to 256
#' @param theta For each neighbourhood (3 frames C 9 frequency bins) centred on any element/pixel in the spectrogram, calculate the average spectrogram value, D. If D is less than a user determined threshold, N8 (theta), set the value of the central element/pixel equal to the minimum in the neighbourhood
#' @param threshold The cut-off dB-value above which sound is considered present in a frequency bin. Defaults to 3 dB.
#'
#' @return CVR-index output files in '.csv' form, stored in output_dir
#' @export
#'
#' @examples
#'
#' # File preparation
#' fpath <- system.file("extdata", package = "soundscapeR")
#' file_locs <- ss_find_files(parent_directory = fpath)
#' file_locs_clean <- ss_assess_files(file_locs = file_locs, full_days = FALSE)

#' # Index calculation
#' ss_index_calc(file_list = file_locs_clean[[1]], window = 256)
#'
ss_index_calc <- function(file_list,
                          output_dir = NA,
                          window = 256,
                          theta = 3,
                          threshold = 3) {
  # Make an output folder

  if (is.na(output_dir)) {
    base_dir <- dirname(file_list[1])
    output_path <- file.path(base_dir, window)
  } else {
    output_path <- file.path(output_dir, window)
  }

  if (dir.exists(output_path)) {
    output_existing <- list.files(
      path = output_path,
      pattern = "CVR.csv"
    )

    output_expected <- paste0(gsub(x = basename(file_list), pattern = ".wav|.WAV", replacement = ""), "_CVR.csv")

    if (length(list.files(
      path = output_path,
      pattern = "CVR.csv"
    )) > 0) {
      file_list_new <- file_list[!sapply(output_expected, function(x) x %in% output_existing)]

      length_file_list_new <- length(file_list_new)

      if (length_file_list_new == 0) {
        cli::cli_abort("All expected CVR index files are already present in the output folder, cancelling index computation...")
      }

      length_file_list <- length(file_list)

      cli::cli_alert_warning("An output folder with CVR output files is already present - continuing the calculation for missing output files only...")
      cli::cli_alert_info("Computing {length_file_list_new} missing CVR output files out of {length_file_list} detected sound files...")
    } else {
      file_list_new <- file_list
    }
  } else {
    dir.create(output_path)
    file_list_new <- file_list
  }

  CVR_list <- vector("list", length(file_list_new))

  progress_bar <- function() {
    cli::cli_progress_bar("Calculating indices", total = length(file_list_new))

    for (i in 1:length(file_list_new)) {
      cli::cli_progress_update()

      CVR_list[[i]] <- CVR_computation(
        file = file_list_new[i],
        window = window,
        theta = theta,
        threshold = threshold
      )

      utils::write.csv(x = t(as.data.frame(CVR_list[[i]])), file = paste0(output_path, "/", gsub(x = basename(file_list_new[i]), pattern = ".wav|.WAV", replacement = ""), "_CVR.csv"))
    }

    cli::cli_progress_done()
  }

  progress_bar()

  names(CVR_list) <- basename(file_list_new)

  cli::cli_alert_success("CVR-index output files are found in: {output_path}")
  Sys.sleep(0.0001)
}

# nocov end

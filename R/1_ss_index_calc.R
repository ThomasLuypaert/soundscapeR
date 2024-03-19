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
#' @examples
#'
#' # Get file path to '.wav' files
#' fpath <- system.file("extdata", package="soundscapeR")
#'
#' # Finding files
#' file_locs <- ss_find_files(parent_directory = fpath)
#'
ss_find_files <- function(parent_directory) {
  directories_with_wavs <- list()

  # Check for .wav files in the parent directory non-recursively
  wav_files <- list.files(parent_directory, "\\.wav$|\\.WAV$", full.names = TRUE, recursive = FALSE)

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
#'
#' @examples
#'
#' # File prepration
#' fpath <- system.file("extdata", package="soundscapeR")
#' file_locs <- ss_find_files(parent_directory = fpath)
#'
#' # No subsetting
#' file_locs_clean <- ss_assess_files(file_locs = file_locs, full_days = TRUE)
#'
#' # Subsetting to full days only
#' file_locs_clean <- ss_assess_files(file_locs = file_locs, full_days = TRUE)
#'
ss_assess_files <- function(file_locs, full_days = TRUE) {
  # 1. Find the sampling regime of each folder (median timeinterval between sound files)

  sample_regime <- vector("list")

  for (i in 1:length(file_locs)) {
    regime <- sapply(
      file_locs[[i]],
      function(x) sub(".*_(\\d{8}_\\d{6})Z\\.wav", "\\1", x)
    )

    regime <- diff(unlist(lapply(regime, function(x) {
      as.POSIXct(strptime(x,
        "%Y%m%d_%H%M%S",
        tz = "America/Manaus"
      ))
    }))) / 60

    median_regime <- stats::median(regime)

    sample_regime[[i]] <- regime

    # 2. Check if any of the files deviate from the expected sampling regime
    # For instance: are some files missing from the expected sequence

    if (any(regime > median_regime)) {
      outliers <- c()

      for (j in 2:length(regime)) {
        if (as.numeric(regime[j] - regime[j - 1], units = "secs") > median_regime) {
          outliers <- c(outliers, j)
        }
      }

      cat("Irregular timeinterval detected for: ", names(file_locs)[i], "\n")
      cat("Based on the expected sampling regime, there are missing files...")

      cat(basename(file_locs[[i]][outliers]))

      stop("Irregular timeintervals detected - check files")
    } else {
      # 3. If no missing files, assess how many full sampling days per site
      # Subset the fileloc list to contain only full days

      files_per_day <- 1440 / as.numeric(median_regime)
    }
  }

  if (full_days == TRUE) {
    subset_to_closest_multiple <- function(my_list, multiple) {
      # Create a new list to store the subsetted vectors
      my_list_subset <- vector("list", length(my_list))

      # Loop over each vector in the list
      for (i in seq_along(my_list)) {
        # Check if the length of the vector is a multiple of 288
        if (length(my_list[[i]]) %% multiple == 0) {
          my_list_subset[[i]] <- my_list[[i]] # No need to subset
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
  } else {
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
#' @param theta For each neighbourhood (3 frames × 9 frequency bins) centred on any element/pixel in the spectrogram, calculate the average spectrogram value, ā. If ā is less than a user determined threshold, θ (theta), set the value of the central element/pixel equal to the minimum in the neighbourhood
#' @param threshold The cut-off dB-value above which sound is considered present in a frequency bin. Defaults to 3 dB.
#'
#' @return A list of CVR-values
#'
CVR_computation <- function(file, window = 256, theta = 3, threshold = 3) {

  # Define C++ functions

  # mean dB for rows in matrix

  Rcpp::cppFunction(
    'NumericMatrix rollapply_meandB(NumericMatrix x, int width) {
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
}'
  )

  # mean dB for a vector
  Rcpp::cppFunction(
    'NumericVector rollapply_meandB_vector(NumericVector x, int width) {
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
}'
  )

  # Adaptive Level Equalisation (Towsey 2017)

  Rcpp::cppFunction(
    'NumericMatrix adaptive_level_equalisation(NumericMatrix x, int windowRowSize, int windowColSize, double ALE_theta) {
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
}'
  )

  # Define R functions

  # dB mode per row, uses rollapply_meandB_vector C++ function complied above.
  dB_mode_per_row <- function(x, width = 5){
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
  raw_spectro <- seewave::spectro(sound, wl = window,
                                  wn = "hamming",
                                  plot = FALSE)$amp

  # Smooth spectrogram with a moving window of 3. Truncating values below -90
  raw_spectro <- pmax(rollapply_meandB(raw_spectro, width = 3), -90)

  # Calculate mode of each frequency band using above function.
  spectro_mode <- apply(raw_spectro, 1, FUN = dB_mode_per_row)

  # Smooth the mode using a moving average filter
  spectro_mode <- rollapply_meandB_vector(spectro_mode, width = 5)

  # Subtract the resulting background noise values from the values in each frequency bin. Truncate negative values to zero.
  spectro_less_mode <- pmax(raw_spectro - spectro_mode, 0)

  # For each neighbourhood (3 frames × 9 frequency bins) centred on any element/pixel in the spectrogram, calculate the average spectrogram value, ā.
  # If ā is less than a user determined threshold, θ, set the value of the central element/pixel equal to the minimum in the neighbourhood
  ale_matrix <- adaptive_level_equalisation(spectro_less_mode, windowRowSize = 9, windowColSize = 3, ALE_theta = 3)


  # Finally, calculate the CVR.
  # Termed the Activity (ACTsp) in Towsey (2017): "The fraction of cells in each noise-reduced frequency bin whose value exceeds the threshold, θ = 3 dB."
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
#' @param theta For each neighbourhood (3 frames × 9 frequency bins) centred on any element/pixel in the spectrogram, calculate the average spectrogram value, ā. If ā is less than a user determined threshold, θ (theta), set the value of the central element/pixel equal to the minimum in the neighbourhood
#' @param threshold The cut-off dB-value above which sound is considered present in a frequency bin. Defaults to 3 dB.
#'
#' @return CVR-index output files in '.csv' form, stored in output_dir
#' @export
#'
#' @examples
#'
#' # File preparation
#' fpath <- system.file("extdata", package="soundscapeR")
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

      cat("An output folder with CVR output files is already present - continuing the calculation for missing output files only... \n")
      cat(paste0("Computing ", length(file_list_new), " missing CVR output files out of ", length(file_list), " detected sound files... \n"))
      Sys.sleep(0.0001)
    } else {
      file_list_new <- file_list
    }
  } else {
    dir.create(output_path)
    file_list_new <- file_list
  }

    CVR_list <- vector("list", length(file_list_new))

    for (i in 1:length(file_list_new)) {

      print(paste0(round(((i / length(file_list_new)) * 100), digits = 1), " %"))

      Sys.sleep(0.00000000000001)

      CVR_list[[i]] <- CVR_computation(
        file = file_list_new[i],
        window = window,
        theta = theta,
        threshold = threshold
      )

      utils::write.csv(x = t(as.data.frame(CVR_list[[i]])), file = paste0(output_path, "/", gsub(x = basename(file_list_new[i]), pattern = ".wav", replacement = ""), "_CVR.csv"))
    }

    names(CVR_list) <- basename(file_list_new)

    print(paste0("CVR-index output files are found in: ", output_path))
    Sys.sleep(0.0001)

}

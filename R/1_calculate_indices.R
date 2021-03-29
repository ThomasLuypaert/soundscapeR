# Functions for calculating acoustic indices
# !) Prior to running this command, make sure you have:

# 1) Installed the latest version of AnalysisPrograms.exe on:
# https://github.com/QutEcoacoustics/audio-analysis/releases
# 2) Renamed your files to abide by the following naming rules:
# https://ap.qut.ecoacoustics.info/basics/dates.html

#' Set Configuration Parameters for Acoustic Index Computation
#'
#' @description Alters and saves the configuration file used by
#' external software 'AnalysisPrograms.exe' for the computation of
#' acoustic indices.
#'
#' @param progloc The full-length path to the location of the
#' 'AnalysisPrograms.exe' software.
#' @param samplerate The number of times the sound was sampled each second.
#' This is a fixed parameter determined by your recording setup, although
#'  downsampling to a lower sampling rate is possible.
#' @param window A variable of the Short-time Fourier Transformation,
#' expressed as the number of samples. The window size of choice
#' depends on the fundamental frequency, intensity and change of the signal
#' of interest, and influences the temporal and frequency resolution
#' of the analysis. The window size is generally a power of 2.
#'
#' @details The \code{index_config()} function is normally only used internally
#' by the \code{index_calc()} function, although it is possible for the
#' user to edit the configuration file manually.
#'
#' @return Edits and saves the '.yml' configuration file which is
#' used by the AnalysisPrograms software for index computation.
#'
#' @export
index_config <- function(progloc, samplerate = 41000, window = 256) {

  # 1. Check if function inputs meet expectations

    # 1.1. progloc is an existing, writable directory

  test_1 <- function(x){
    assertthat::is.dir(x)
  }

  assertthat::on_failure(test_1) <- function(call, env) {

    paste0(deparse(call$x), " is not an existing directory. If you're using a windows device, make sure to change backslashes to forwards slashes in the path.")

  }

  assertthat::assert_that(test_1(progloc))

  test_1_1 <- function(x){
    assertthat::is.writeable(x)
  }

  assertthat::on_failure(test_1_1) <- function(call, env) {

    paste0(deparse(call$x), " is not a writable directory. Change permissions.")

  }

  assertthat::assert_that(test_1_1(progloc))

    # 1.2. samplerate is a single, positive integer

  test_2 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " is not a single, positive integer. Consult the package documentation for more information on the samplerate.")

  }

  assertthat::assert_that(test_2(samplerate))

    # 1.3. window is a single, positive integer

  test_3 <- function(x){
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is not a single, positive integer. Consult the package documentation for more information on the window")

  }

  assertthat::assert_that(test_3(window))

    # 1.4. produce a warning message if window is not a power of two

  if(!as.integer(log(window, base=2)) == (log(window, base = 2))){
    cat("\n Chosen window size is not a power of two. This is a warning message, \n if your window size was chosen purposefully, proceed as planned by pressing Y. \n If you want to abort, press N.", "\n")
    Sys.sleep(0.000000000001)

    question1 <- readline("Would you like to proceed with the chosen window size? (Y/N)")

    if(regexpr(question1, 'y', ignore.case = TRUE) == 1){
    }
    else{

      if(regexpr(question1, 'n', ignore.case = TRUE) == 1){
        print("Index computation aborted.")
        Sys.sleep(0.0000000000000000000000000001)
       stop()
      } else {
        print("The option you have chosen is not valid - index computation aborted")

        Sys.sleep(0.00000000000000000000000000000001)
        stop()
      }
    }

  }

  # 2. Import the yaml file into R, and check


  config <- as.data.frame(
    yaml::read_yaml(file = paste0(
      progloc, "/ConfigFiles/Towsey.Acoustic.yml"
    ))
  )

    # 2.1. Check if the configuration file can be read and is writable

  test_5_1 <- function(x){

    assertthat::is.writeable(x)
    assertthat::is.readable(x)

  }

  assertthat::on_failure(test_5_1) <- function(call, env){

    paste0(deparse(call$x), " the configuration file is not writable. Change permissions.")

  }

  assertthat::assert_that(test_5_1(
    paste0(progloc, "/ConfigFiles/Towsey.Acoustic.yml")))

    # 2.1. check if yaml config file succesfully imported as dataframe

  test_5_2 <- function(x){
    is.data.frame(x)
  }

  assertthat::on_failure(test_5_2) <- function(call, env){

    paste0(deparse(call$x), " is not a dataframe. The yaml configuration file could not be imported succesfully.")

  }

  assertthat::assert_that(test_5_2(config))

  # 3. Edit configuration file and check

  config$SegmentDuration <- 60
  config$IndexCalculationDuration <- 60
  config$ResampleRate <- as.integer(samplerate)
  config$FrameLength <- as.integer(window)

    # 3.1. check if the segment duration was succesfully changed to 60 seconds

  test_6 <- function(x){
    assertthat::are_equal(x, 60)
  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " could not be changed to 60 seconds.")

  }

  assertthat::assert_that(test_6(config$SegmentDuration))

    # 3.2. check if the index calculation duration was successfully changed to 60 seconds.

  test_7 <- function(x){
  assertthat::are_equal(x, 60)
  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " could not be changed to 60 seconds.")

  }

  assertthat::assert_that(test_7(config$IndexCalculationDuration))

    # 3.3. check if the sampling rate was successfully changed to the specified value.

  test_8 <- function(x){
    assertthat::are_equal(x, as.integer(samplerate))
  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " could not be changed to the specified sampling rate.")

  }

  assertthat::assert_that(test_8(config$ResampleRate))

    # 3.4. check if the window was successfully changed to the specified value.

  test_9 <- function(x){
    assertthat::are_equal(x, as.integer(window))
  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " could not be changed to the specified window size.")

  }

  assertthat::assert_that(test_9(config$FrameLength))

  # 4. Overwrite the original yaml file with the edited version


  yaml::write_yaml(config, file <- paste0(
    progloc, "/ConfigFiles/Towsey.Acoustic.Custom.yml"
  ))

    # 4.1. check if configuration file successfully changed.

  config_2 <- as.data.frame(
    yaml::read_yaml(file = paste0(
      progloc, "/ConfigFiles/Towsey.Acoustic.Custom.yml"
    ))
  )

  test_10 <- function(x){

    assertthat::are_equal(x, config)

  }

  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " could not be succesfully overwrite previous configuration file.")

  }

  assertthat::assert_that(test_10(config_2))

}


#' Compute Acoustic Indices
#'
#' @description Calls on the external software 'AnalysisPrograms.exe'
#' to compute a set of acoustic indices using user-specified
#' configuration parameters.
#'
#' @param fileloc The full-length path to the sound file for which to
#' compute indices.
#' @param progloc The full-length path to the location of
#' 'AnalysisPrograms.exe'.
#' @param samplerate The number of times the sound was sampled each second.
#' This is a fixed parameter determined by your recording setup, although
#' downsampling to a lower sampling rate is possible.
#' @param window A variable of the Short-time Fourier Transformation,
#' expressed as the number of samples. The window size of choice depends on
#' the fundamental frequency, intensity and change of the signal of interest,
#' and influences the temporal and frequency resolution of the analysis.
#' The window size is generally a power of 2.
#'
#' @details
#' The default duration of sound files for index computation is 60 seconds.
#' If the file length exceeds 60 seconds, the file is automatically cut into
#' 60 second segments for further analysis.
#'
#' \bold{The following spectral indices will be computed:}
#'
#' \emph{Background Noise (BGN)}:
#' The mode of the distribution of decibel values in each
#' frequency bin, representing the “background” intensity value.
#' This index captures the acoustic energy which persists throughout the
#' duration of the sound file, regardless of its origin
#' (biophonic, geophonic or anthrophonic).
#'
#' \emph{Power minus noise (PMN)}:
#' The difference between the maximum decibel value in
#' each frequency bin and the corresponding BGN decibel value.
#'
#' \emph{Acoustic cover (CVR)}:
#' The fraction of active elements in each noise-reduced
#' frequency bin where the amplitude exceeds a 3-dB threshold.
#'
#' \emph{Number of events (EVN)}:
#' The number of times the decibel value in a noise-reduced
#' frequency bin crosses the 3-dB threshold from lower to higher values.
#'
#' \emph{Temporal entropy (ENT)}:
#' A measure of acoustic energy concentration in each noise-reduced
#' frequency bin.
#'
#' \emph{Acoustic Complexity Index (ACI)}:
#' A measure quantifying the variability in
#' intensity values in each noise-reduced frequency bin. It is widely used as a
#' measure of biophony in recordings, however remains highly sensitive to
#' non-biological sources of sound.
#'
#' \emph{Oscillation Index (OSC)}:
#' ...
#'
#' \emph{Spectral Peak Tracks (SPT)}:
#' A measure of the presence of spectral peak
#' tracks in a noise-reduced frequency bin.
#'
#' \emph{Ridge indices (RHZ, RVT, RPS, RNS)}:
#' A set of indices based on the presence of formants
#' in the harmonic structure of many animal vocalizations, calculated in the
#' four directions of the ridge slope (horizontal (RHZ), vertical (RVT),
#' upward slope (RPS), downward slope (RNS)). Formants in the mid-band are
#' typically due to birdsong, whereas vertical formants are typical for
#' non-biological sounds such as rain drops and electrical clicks.
#'
#' For index computation this function calls on the external software
#' 'AnalysisPrograms.exe' by the QUT Ecoacoustics Research Group
#' (\url{https://github.com/QutEcoacoustics/audio-analysis}).Make sure the
#' software is installed prior to commencing the analysis. Additionally, the
#' software requires sound files to be named according to certain standards
#' (\url{https://ap.qut.ecoacoustics.info/basics/dates.html}).
#'
#' \strong{Software citation}:
#' Michael Towsey, Anthony Truskinger, Mark Cottman-Fields, & Paul Roe.
#' (2018, March 5). Ecoacoustics Audio Analysis Software v18.03.0.41
#' (Version v18.03.0.41). Zenodo. http://doi.org/10.5281/zenodo.1188744
#'
#' @return Creates a directory with '.csv' files for the aforementioned
#' set of spectral and summary indices. The directory is created in the same
#' place as the original sound file.
#'
#' @export
#'
index_calc <- function(fileloc, progloc, samplerate = 41000, window = 256) {

  # 1. Check if function input meets expectations

    # 1.1. fileloc is an existing directory

  test_1 <- function(x){
    assertthat::is.dir(x)
    assertthat::is.readable(x)
  }

  assertthat::on_failure(test_1) <- function(call, env) {

    paste0(deparse(call$x), " is not a readable directory. If you're using a windows device, make sure to change backslashes to forwards slashes in the path.")

  }

  assertthat::assert_that(test_1(fileloc))

  # 2. Create output directory, and list files in fileloc + checking

  base_output_directory <- paste0(fileloc, "/Output")

  files <- list.files(fileloc,
    pattern = "*.wav|*.WAV|*.mp3|
                      *.ogg|*.flac|*.wv|*.webm|*.wma",
    full.names = TRUE
  )

    # 2.1. check if 'files' is a character vector, not empty, without NAs

  test_4_1 <- function(x){
    is.vector(x, mode = "character")
  }

  assertthat::on_failure(test_4_1) <- function(call, env){

    paste0(deparse(call$x), " is not a character vector containing the path to each file. Check if the correct file location name was supplied, and that files have the correct extension (consult package information for accepted file types)")

  }

  assertthat::assert_that(test_4_1(files))

  test_4_2 <- function(x){
    assertthat::not_empty(x)
  }

  assertthat::on_failure(test_4_2) <- function(call, env){

    paste0(deparse(call$x), " is an empty character vector, files were not succesfully read by list.files(). Check if the correct file location was supplied, and that files have the correct extention (consult package information for accepted file types")

  }

  assertthat::assert_that(test_4_2(files))

  test_4_3 <- function(x){
    assertthat::noNA(x)
  }

  assertthat::on_failure(test_4_3) <- function(call, env){

    paste0(deparse(call$x), " contains NAs. Check if the correct file location was supplied, and that files have the correct extention (consult package information for accepted file types")

  }

  assertthat::assert_that(test_4_3(files))

  #3. Edit the configuration file

  index_config(progloc, samplerate = samplerate, window = window)

  #4. Calculate acoustic indices

  for (i in 1:length(files)) {

    # Alert start

    message("Processing ", files[i])

    # get just the name of the file
    file_name <- basename(files[i])

    # make a folder for results
    output_directory <- normalizePath(
      file.path(
        base_output_directory, file_name
      )
    )
    dir.create(output_directory, recursive = TRUE)

    # prepare command
    command <- sprintf(
      'audio2csv "%s" "%s" "%s" -l 1 -p ',
      files[i],
      paste0(
        progloc,
        "/ConfigFiles/Towsey.Acoustic.Custom.yml"
      ),
      output_directory
    )

    # finally, execute the command
    system2(paste0(progloc, "/AnalysisPrograms.exe"), command)

    print(paste0(((i/length(files))*100), " %"))
    Sys.sleep(0.00000000000000000000000000000000000000000000000000001)

  }

    # 4.1. Check if the output folder was successfully created


  test_5 <- function(x){

    assertthat::is.dir(x)

  }

  assertthat::on_failure(test_4_3) <- function(call, env){

    paste0(deparse(call$x), " directory does not exist, no output folder was created.")

  }

  assertthat::assert_that(test_5(paste0(fileloc, "/Output")))

}

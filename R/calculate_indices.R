# Functions for calculating acoustic indices -------------------------------------------------

# !) Prior to running this command, make sure you have:

#1) Installed the latest version of AnalysisPrograms.exe on: https://github.com/QutEcoacoustics/audio-analysis/releases
#2) Renamed your files to abide by the following naming rules: https://ap.qut.ecoacoustics.info/basics/dates.html

#' Set Configuration Parameters for Acoustic Index Computation
#'
#' @description Alters and saves the configuration file used by external software 'AnalysisPrograms.exe'
#' for the computation of acoustic indices.
#'
#' @param progloc The full-length path to the location of the 'AnalysisPrograms.exe' software.
#' @param samplerate The number of times the sound was sampled each second. This is a fixed
#'     parameter determined by your recording setup, although downsampling to a lower sampling
#'     rate is possible.
#' @param window A variable of the Short-time Fourier Transformation, expressed as the number
#' of samples. The window size of choice depends on the fundamental frequency, intensity and change
#' of the signal of interest, and influences the temporal and frequency resolution of the analysis.
#' The window size is generally a power of 2.
#'
#' @details The \code{index_config} function is normally only used internally by the \code{\link{index_calc}} function,
#' although it is possible for the user to edit the configuration file manually.
#'
#' @return Edits and saves the '.yml' configuration file which is used by the AnalysisPrograms software for
#' index computation.
#'
#' @export
index_config=function(progloc, samplerate=41000, window=256){
  config=as.data.frame(yaml::read_yaml(file=paste0(progloc, "/ConfigFiles/Towsey.Acoustic.yml")))
  config$SegmentDuration=60
  config$IndexCalculationDuration=60
  config$ResampleRate=as.integer(samplerate)
  config$FrameLength=as.integer(window)
  yaml::write_yaml(config, file=paste0(progloc, "/ConfigFiles/Towsey.Acoustic.Custom.yml"))
}


#' Compute Acoustic Indices
#'
#' @description Calls on the external software 'AnalysisPrograms.exe' to compute a set of
#' acoustic indices using user-specified configuration parameters.
#'
#' @param fileloc The full-length path to the sound file for which to compute indices.
#' @param progloc The full-length path to the location of 'AnalysisPrograms.exe'.
#' @param samplerate The number of times the sound was sampled each second. This is a fixed
#'     parameter determined by your recording setup, although downsampling to a lower sampling
#'     rate is possible.
#' @param window A variable of the Short-time Fourier Transformation, expressed as the number
#' of samples. The window size of choice depends on the fundamental frequency, intensity and change
#' of the signal of interest, and influences the temporal and frequency resolution of the analysis.
#' The window size is generally a power of 2.
#'
#' @details
#' The default duration of sound files for index computation is 60 seconds. If the file length
#' exceeds 60 seconds, the file is automatically cut into 60 second segments for further analysis.
#'
#' \bold{The following spectral indices will be computed:}
#'
#' \emph{Background Noise (BGN)}:
#' The mode of the distribution of decibel values in each
#' frequency bin, representing the “background” intensity value. This index captures
#' the acoustic energy which persists throughout the duration of the sound file,
#' regardless of its origin (biophonic, geophonic or anthrophonic).
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
#' A measure of acoustic energy concentration in each noise-reduced frequency bin.
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
#' in the harmonic structure of many animal vocalizations, calculated in the four
#' directions of the ridge slope (horizontal (RHZ), vertical (RVT), upward slope (RPS),
#' downward slope (RNS)). Formants in the mid-band are typically due to birdsong,
#' whereas vertical formants are typical for non-biological sounds such as rain drops
#' and electrical clicks.
#'
#' For index computation this function calls on the external software 'AnalysisPrograms.exe'
#' by the QUT Ecoacoustics Research Group (\url{https://github.com/QutEcoacoustics/audio-analysis}).
#' Make sure the software is installed prior to commencing the analysis. Additionally, the software requires
#' sound files to be named according to certain standards (\url{https://ap.qut.ecoacoustics.info/basics/dates.html}).
#'
#' \strong{Software citation}:
#' Michael Towsey, Anthony Truskinger, Mark Cottman-Fields, & Paul Roe. (2018, March 5).
#' Ecoacoustics Audio Analysis Software v18.03.0.41 (Version v18.03.0.41). Zenodo.
#' http://doi.org/10.5281/zenodo.1188744
#'
#' @return Creates a directory with '.csv' files for the aforementioned set of spectral and summary indices.
#' The directory is created in the same place as the original sound file.
#'
#' @export
#'
index_calc=function(fileloc, progloc, samplerate=41000, window=256){
  directory <- fileloc
  base_output_directory <- paste0(fileloc, "/Output")
  files <- list.files(directory, pattern = "*.wav|*.WAV|*.mp3|*.ogg|*.flac|*.wv|*.webm|*.wma", full.names = TRUE)

  index_config(progloc, samplerate=samplerate, window=window)

  i=0
  vector_i=c()

  for(file in files) {

    # start timer

    tictoc::tic(quiet = TRUE)

    # Alert start

    message("Processing ", file)

    # get just the name of the file
    file_name <- basename(file)

    # make a folder for results
    output_directory <- normalizePath(file.path(base_output_directory, file_name))
    dir.create(output_directory, recursive = TRUE)

    # prepare command
    command <- sprintf('audio2csv "%s" "%s" "%s" -l 1 -p ', file, paste0(progloc, "/ConfigFiles/Towsey.Acoustic.Custom.yml"), output_directory)

    # finally, execute the command
    system2(paste0(progloc, "/AnalysisPrograms.exe"), command)

    # Print progress & estimated remaining time

    i=i+1
    timer=tictoc::toc()
    vector_i[i]=as.numeric(((as.numeric(timer[[2]])-as.numeric(timer[[1]]))/3600))

    print(paste0("Progress:", " ",((i/length(files))*100), " ", "%"))
    print(paste0("Estimated remaining time:", " ",  hms::hms(chron::times((as.numeric(mean(vector_i))*(as.numeric(length(files))-as.numeric(i)))/24))))
    Sys.sleep(0.01)
  }
}


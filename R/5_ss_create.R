# Function for creating a soundscape object

#' Creating a soundscape object
#'
#' @description A wrapper function that combined the ss_index_merge, ss_binarize and ss_aggregate functions
#' to create a soundscape object in a single step
#'
#' @param fileloc The full-length path to the directory where the
#' output of the \code{\link{ss_index_calc}} function is located.
#'
#' @param samplerate The sampling rate specified in the \code{\link{ss_index_calc}}
#' function.
#'
#' @param window The window length specified in the \code{\link{ss_index_calc}}
#' function.
#'
#' @param index The acoustic index of interest. Options are
#' "BGN", "PMN", "CVR", "EVN", "ENT", "ACI", "OSC", "SPT", "RHZ",
#' "RVT", "RPS" and "RNG". For a brief description of indices, consult
#' the \code{\link{ss_index_calc}} documentation. Note that the soundscape diversity metrics
#' that can be calculated downstream have only been tested using the Acoustic Cover (CVR) index
#'
#' @param date The first day of the recording period. Used for managing time-objects in R.
#'  Formatted as "YYYY-mm-dd".
#'
#' @param lat The latitude of the site at which the sound files were
#' collected. Coordinates should be specified in decimal degrees as a
#'  numerical variable.
#'
#' @param lon The longitude of the site at which the sound files were
#'  collected. Coordinates should be specified in decimal degrees as a
#'   numerical variable.
#'
#' @param method The algorithm used to determine the threshold.
#'  Options are "IJDefault","Huang", "Huang2", "Intermodes",
#'  "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI", "Minimum",
#'  "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag",
#'  "Triangle", "Yen", and "Mode". To specify a custom threshold,
#'  use method="Custom" in combination with the value argument.
#'  Consult \url{http://imagej.net/Auto_Threshold} for more
#'  information on algorithm methodologies.
#'
#' @param value Optional argument used to set a custom threshold
#' value for binarization - used in combination with method="Custom".
#'
#' @param output Determines whether the function returns the raw
#' total number of detections per time during the
#' recording period (output = "raw"), or the incidence frequency
#' (total number of detections / number of recordings for that time
#' - output = "incidence_freq).
#'
#' @return Returns a soundscape object.
#'
#' @export
#'
ss_create <- function(fileloc,
                      samplerate,
                      window,
                      index = "CVR",
                      date,
                      lat,
                      lon,
                      method,
                      value,
                      output) {
  # 1. Merge the index files into a soundscape

  merged_soundscape <- ss_index_merge(
    fileloc = fileloc,
    samplerate = samplerate,
    window = window,
    index = index,
    date = date,
    lat = lat,
    lon = lon
  )

  print("Merging of index files complete...")
  Sys.sleep(0.000000000000000000000000000001)

  # 2. Binarize the index files

  if (missing(value)) {
    binarized_soundscape <- soundscapeR::ss_binarize(
      merged_soundscape = merged_soundscape,
      method = method
    )
  } else {
    binarized_soundscape <- soundscapeR::ss_binarize(
      merged_soundscape = merged_soundscape,
      method = method,
      value = value
    )
  }

  print("Binarizing soundscape complete...")
  Sys.sleep(0.000000000000000000000000000001)

  # 3. Aggregate the index files

  aggregated_soundscape <- soundscapeR::ss_aggregate(
    binarized_soundscape = binarized_soundscape,
    output = output
  )

  print("Aggregating soundscape complete: a soundscape object has been created!")
  Sys.sleep(0.000000000000000000000000000001)

  return(aggregated_soundscape)
}

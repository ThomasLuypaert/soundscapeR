#' @name amazon_soundscape
#'
#' \code{amazon_soundscape}: Soundscape data from the Brazilian Amazon.
#'
#' Soundscape data from the Brazilian Amazon, collected at the Cristalino Ecolodge in November 2019.
#' The data was collected using an AudioMoth recorder, sampling the soundscape for 1-min every 10-min at a
#' 384,000 Hz sampling rate and medium gain for approximately 12 days. This resulted in 1603 sound files.
#' This raw sound data was processed using the \code{soundscapeR} package. For each file, a set of spectral
#' acoustic indices was computed using the \code{\link{index_calc}} function. Next, all spectral index '.csv' files
#' for the Acoustic Cover index ('CVR') were merged into a single time-frequency dataframe of index values. Finally,
#' dataframe was subsetted to include frequencies between 0-21,000 Hz.
#'
#' @docType data
#'
#' @usage data(grav)
#'
#' @format A time-by-frequency dataframe with 56 rows and 1603 columns. Rows represent frequency bins and columns show the time
#' of day when the recording was taken. For each time-frequency bin, the 'Acoustic Cover' index value is given.
#'
#' @keywords datasets
#'
#' @source "Amazon Biodiversity & Carbon Expeditions - unpublished.
#'

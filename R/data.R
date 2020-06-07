#' Soundscape data from the Brazilian Amazon - Acoustic Cover Index
#'
#' Soundscape data from the Brazilian Amazon, collected at the Cristalino Ecolodge in November 2019.
#' The data was collected using an AudioMoth recorder, sampling the soundscape for 1-min every 10-min at a
#' 384,000 Hz sampling rate and medium gain for approximately 12 days. This resulted in 1603 sound files.
#' This raw sound data was processed using the \code{soundscapeR} package. For each file, a set of spectral
#' acoustic indices was computed using the \code{\link{index_calc}} function. Next, all spectral index '.csv' files
#' for the Acoustic Cover index ('CVR') were merged into a single time-frequency dataframe of index values. Finally,
#' dataframe was subsetted to include frequencies between 0-21,000 Hz.
#'
#' @name amazon_soundscape_CVR
#' @docType data
#' @author Thomas Luypaert \email{thomas.luypaert@@nmbu.no}
#' @keywords data datasets
#' @usage data(amazon_soundscape_CVR)
#' @format A time-frequency data frame with 56 rows and 1603 columns. Rows represent frequency bins and columns show the time
#' of day when the recording was taken. For each time-frequency bin, the 'Acoustic Cover' index value is given.
#'
#' @source Amazon Biodiversity & Carbon Expeditions (unpublished)
"amazon_soundscape_CVR"

#' Soundscape data from the Brazilian Amazon - Acoustic Complexity Index
#'
#' Soundscape data from the Brazilian Amazon, collected at the Cristalino Ecolodge in November 2019.
#' The data was collected using an AudioMoth recorder, sampling the soundscape for 1-min every 10-min at a
#' 384,000 Hz sampling rate and medium gain for approximately 12 days. This resulted in 1603 sound files.
#' This raw sound data was processed using the \code{soundscapeR} package. For each file, a set of spectral
#' acoustic indices was computed using the \code{\link{index_calc}} function. Next, all spectral index '.csv' files
#' for the Acoustic Complexity Index ('ACI') were merged into a single time-frequency dataframe of index values. Finally,
#' dataframe was subsetted to include frequencies between 0-21,000 Hz.
#'
#' @name amazon_soundscape_ACI
#' @docType data
#' @author Thomas Luypaert \email{thomas.luypaert@@nmbu.no}
#' @keywords data datasets
#' @usage data(amazon_soundscape_ACI)
#' @format A time-frequency data frame with 56 rows and 1603 columns. Rows represent frequency bins and columns show the time
#' of day when the recording was taken. For each time-frequency bin, the 'Acoustic Complexity Index' value is given.
#'
#' @source Amazon Biodiversity & Carbon Expeditions (unpublished)
"amazon_soundscape_ACI"

#' Soundscape data from the Brazilian Amazon - Temporal Entropy Index
#'
#' Soundscape data from the Brazilian Amazon, collected at the Cristalino Ecolodge in November 2019.
#' The data was collected using an AudioMoth recorder, sampling the soundscape for 1-min every 10-min at a
#' 384,000 Hz sampling rate and medium gain for approximately 12 days. This resulted in 1603 sound files.
#' This raw sound data was processed using the \code{soundscapeR} package. For each file, a set of spectral
#' acoustic indices was computed using the \code{\link{index_calc}} function. Next, all spectral index '.csv' files
#' for the Temporal Entropy index ('ENT') were merged into a single time-frequency dataframe of index values. Finally,
#' dataframe was subsetted to include frequencies between 0-21,000 Hz.
#'
#' @name amazon_soundscape_ENT
#' @docType data
#' @author Thomas Luypaert \email{thomas.luypaert@@nmbu.no}
#' @keywords data datasets
#' @usage data(amazon_soundscape_ENT)
#' @format A time-frequency data frame with 56 rows and 1603 columns. Rows represent frequency bins and columns show the time
#' of day when the recording was taken. For each time-frequency bin, the 'Temporal Entropy' index value is given.
#'
#' @source Amazon Biodiversity & Carbon Expeditions (unpublished)
"amazon_soundscape_ENT"

#' Soundscape data from the Brazilian Amazon - Acoustic Cover Index
#'
#' Soundscape data from the Brazilian Amazon, collected at the Cristalino Ecolodge in November 2019.
#' The data was collected using an AudioMoth recorder, sampling the soundscape for 1-min every 10-min at a
#' 384,000 Hz sampling rate and medium gain for approximately 12 days. This resulted in 1603 sound files.
#' This raw sound data was processed using the \code{soundscapeR} package. For each file, a set of spectral
#' acoustic indices was computed using the \code{\link{index_calc}} function. Next, all spectral index '.csv' files
#' for the Horizontal Ridge Index ('RHZ') were merged into a single time-frequency dataframe of index values. Finally,
#' dataframe was subsetted to include frequencies between 0-21,000 Hz.
#'
#' @name amazon_soundscape_RHZ
#' @docType data
#' @author Thomas Luypaert \email{thomas.luypaert@@nmbu.no}
#' @keywords data datasets
#' @usage data(amazon_soundscape_RHZ)
#' @format A time-frequency data frame with 56 rows and 1603 columns. Rows represent frequency bins and columns show the time
#' of day when the recording was taken. For each time-frequency bin, the 'Horizontal Ridge Index' value is given.
#'
#' @source Amazon Biodiversity & Carbon Expeditions (unpublished)
"amazon_soundscape_RHZ"

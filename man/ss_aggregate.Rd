% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/4_ss_aggregate.R
\name{ss_aggregate}
\alias{ss_aggregate}
\title{Aggregate binarized dataframe}
\usage{
ss_aggregate(binarized_soundscape, output = "incidence_freq")
}
\arguments{
\item{binarized_soundscape}{The binarized soundscape object produced by
\code{\link{ss_binarize}}.}

\item{output}{Determines whether the function returns the raw
total number of detections (activity = 1) per time during the
recording period (output = "raw"), or the incidence frequency
(total number of detections / number of recordings for that time
- output = "incidence_freq).}
}
\value{
Returns a soundscape object.
}
\description{
Aggregates binarized spectral indices by time of day.
For each aggregate time period, the activity values (0 or 1)
are summed per frequency bin, and potentially divided by the number
of recordings for that time to get the proportion of
acoustically active recordings in each time-frequency bin
(the incidence frequency).
}

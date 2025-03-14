% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/6_ss_diversity.R
\name{ss_diversity}
\alias{ss_diversity}
\title{Estimate soundscape diversity using Hill numbers}
\usage{
ss_diversity(
  soundscape_obj,
  qvalue,
  subset = "total",
  mintime = "default",
  maxtime = "default",
  minfreq = 0,
  maxfreq = "default",
  dawnstart = 0,
  dawnend = 5400,
  duskstart = 5400,
  duskend = 0,
  freqseq = FALSE,
  nbins = 10,
  output = "percentage"
)
}
\arguments{
\item{soundscape_obj}{The aggregated soundscape object produced by
\code{\link{ss_aggregate}} function.}

\item{qvalue}{A positive integer or decimal number (>=0), most commonly
between 0-3. This parameter modulates the sensitivity of diversity
values to the relative abundance of Operational Sound Units (OSUs).
A value of 0 corresponds to the richness, a value of 1 is the equivalent
 effective number of OSUs for the Shannon index, a value of 2 is the
  equivalent effective number of OSUs for the Simpson index.}

\item{subset}{The scale for which the soundscape diversity is computed.
Options are 'total', 'day', night', 'dawn', 'dusk' and
'tod' (time of day - for each unique time in the day).}

\item{mintime}{The lower time limit for which to compute the soundscape
diversity, formatted as "HH:MM:SS". If set to default, uses the
earliest time for which data exists in the dataframe.}

\item{maxtime}{The upper time limit for which to compute the soundscape
diversity, formatted as "HH:MM:SS". If set to default, uses the
latest time for which data exists in the dataframe.}

\item{minfreq}{A numeric value indicating the lower frequency limit
for which to compute the soundscape diversity. If set to default, uses
the lowest available frequency in the dataframe.}

\item{maxfreq}{A numeric value indicating the upper frequency limit
for which to compute the soundscape diversity. If set to default,
uses the highest available frequency in the dataframe.}

\item{dawnstart}{A numeric argument. If subset == 'dawn', used to determine
the start of dawn. By default, dawn starts at sunrise. Expressed as
the time in seconds before sunrise.}

\item{dawnend}{A numeric argument. If subset == 'dawn', used to determine
the end of dawn. By default, dawn ends 1.5 hours after sunrise.
Expressed as the time in seconds after sunrise.}

\item{duskstart}{A numeric argument. If subset == 'dusk', used to determine
the start of dusk. By default, dusk starts 1.5 hours before sunset.
 Expressed as the time in seconds before sunset.}

\item{duskend}{A numeric argument. If subset == 'dusk', used to determine the
end of dusk. By default, dusk ends at sunset. Expressed as the
time in seconds after sunset.}

\item{freqseq}{A logical operator (TRUE/FALSE). If set to FALSE, will
compute the diversity for the entire frequency range of the soundscape.
If set to TRUE, will compute the diversity per frequency-bin of
 user-defined width (number of bins determined by nbins argument).}

\item{nbins}{A numeric argument. If freqseq is set to TRUE, determines
the number of the frequency-bins by which to divide the frequency range
to compute the soundscape diversity.}

\item{output}{A character string. Indicates the format in which
the soundscape diversity is expressed. Options are "percentage"
(the fraction between the observed soundscape diversity and the
maximum possible soundscape diversity), or "raw" (the number of
acoustically active OSUs in the soundscape). Defaults to "percentage".}
}
\value{
Depending on the chosen parameters, returns the soundscape
diversity either a numeric value, a vector of values or a list of
vectors of values.
}
\description{
For a set acoustic index, calculates the diversity of
 acoustically active Operational Sound Units (OSUs) in the soundscape.
 The q-parameter can be altered to modulate the diversity metric's
  sensitivity to abundance. The soundscape diversity metrics can be
   computed at various scales and resolutions. For instance, the user
    can explore the diversity for the whole soundscape, specify custom
     time and frequency limits, or use one of the built-in presets for
      diurnal-phase subsetting (day, night, dawn, dusk). Additionally,
      the user can track the change in soundscape diversity throughout
      the day. Finally, the soundscape diversity can be assessed for
      the entire frequency range, or per frequency-bin of
       user-defined width.

\strong{Note:} Soundscape diversity metrics should not be used to make
 inference about the diversity of the real-world biological community
  unless verified using ground-truthing methods.
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/1_ss_index_calc.R
\name{ss_convert_files}
\alias{ss_convert_files}
\title{Check if the sound files are in the '.wav' file format, if not, convert the files and save them.}
\usage{
ss_convert_files(file_locs, replace = FALSE, verbose = FALSE)
}
\arguments{
\item{file_locs}{The output of the `ss_find_files` function}

\item{replace}{A boolean operator to indicate whether the original non-wav file should be replaced (replace = TRUE) with its .wav file equivalent.
Note that this will lead to original file to be deleted, so make sure you have a back-up of the original file. Defaults to replace = FALSE.}

\item{verbose}{A boolean operator to indicate how chatty the function should be. Using verbose = TRUE leads to increased messaging on what is happening
to the files in file_locs.}
}
\value{
In case some files are not in the '.wav' format, creates a new directory called 'wav_files' containing the sound files converted to '.wav' format.
}
\description{
This function takes the output list of the `ss_find_files` function and checks whether the sound files in the folder
are of the format '.wav'. If not, the function will covert the sound files to a '.wav' format.
}

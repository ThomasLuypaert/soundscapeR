soundscapeR: soundscape diversity quantification
================
Thomas Luypaert, Anderson Saldanha Bueno, Carlos Augusto Peres, Torbjørn
Haugaasen

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![R-CMD-check](https://github.com/ThomasLuypaert/soundscapeR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ThomasLuypaert/soundscapeR/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

|                                                                   |                                                                                                                                                                                                                                                                                                                                                                                                                         |
|-------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| ![soundscaper hexsticker](man/figures/soundscaper_hexsticker.png) | The goal of `soundscapeR` is to provide a standardized analytical pipeline for the computation, exploration, visualization and diversity quantification of soundscapes. The package is designed to work with either continuous or regular-interval long-duration acoustic recordings, and can handle both audible and ultrasonic recordings. More information about the workflow can be found in Luypaert et al. (2022) |

# 0. Priors

This R-package uses spectral index output files computed using the
[‘AnalysisPrograms’](https://ap.qut.ecoacoustics.info/) software tool,
developed by the [QUT Ecoacoustics
Group](https://research.ecosounds.org/). Although all the steps in the
analytical pipeline can be performed using `soundscapeR`, the package
requires ‘AnalysisPrograms’ in the background.

As such, prior to using `soundscapeR`, please head over to the
[‘AnalysisPrograms’ download
page](https://ap.qut.ecoacoustics.info/basics/installing.html?tabs=windows),
and install the latest version of the software tool on your device.

*!) In the latest version of `soundscapeR`, ‘AnalysisPrograms’ is
automatically installed*

Let’s take a look where the software tool is saved on your device:

``` r
location_software <- paste0(base::system.file("extdata", package = "soundscapeR", mustWork = TRUE), 
                              "/AnalysisPrograms")

print(location_software)
#> [1] "C:/Users/thlu/AppData/Local/Programs/R/R-4.2.0/library/soundscapeR/extdata/AnalysisPrograms"
```

# 1. Raw acoustic data

The workflow we present here makes use of eco-acoustic data, or acoustic
recordings collected at large timescales (e.g. days, weeks, months or
even years!), using either a regular interval (e.g. 1 minute out of
every 10 minutes recorded) or continuous sampling regime. The workflow
accommodates the use recordings in ultrasound, but beware that the
spectral acoustic indices we use are not tested for this.

For the purposes of this vignette, a few raw sound files are provided in
the package data. These sound files were collected at the Balbina
Hydroelectric Reservoir in Brazilian Amazonia using a 1 min / 5 min
sampling regime and a 44,100 Hz sampling rate.

Let’s take a look at where the raw data is saved on your device:

``` r
location_soundfiles <- paste0(base::system.file("extdata", package = "soundscapeR", mustWork = TRUE), 
                              "/raw_sound_files")

print(location_soundfiles)
#> [1] "C:/Users/thlu/AppData/Local/Programs/R/R-4.2.0/library/soundscapeR/extdata/raw_sound_files"
```

Now, let’s check which raw sound files are included in the package:

``` r
list.files(location_soundfiles)
#>  [1] "256"                            "G10_Coata_20151017_000000Z.wav"
#>  [3] "G10_Coata_20151017_000500Z.wav" "G10_Coata_20151017_001000Z.wav"
#>  [5] "G10_Coata_20151017_001500Z.wav" "G10_Coata_20151017_002000Z.wav"
#>  [7] "G10_Coata_20151017_002500Z.wav" "G10_Coata_20151017_003000Z.wav"
#>  [9] "G10_Coata_20151017_003500Z.wav" "G10_Coata_20151017_004000Z.wav"
#> [11] "G10_Coata_20151017_004500Z.wav" "G10_Coata_20151017_005000Z.wav"
#> [13] "G10_Coata_20151017_005500Z.wav"
```

Look at that! The package contains 12 sound files collected on the 17th
of October 2015, between midnight and 1 AM (00:00 - 00:55) using a 1 min
/ 5 min sampling regime, as previously mentioned. In the next section,
we will use this sample data to demonstrate how acoustic indices are
calculated.

# 2. Calculating acoustic indices

For the first step in our workflow, we are going to calculate the
spectral index files for the long-duration acoustic recordings collected
at our site of interest. To do this, we will use the `index_calc()`
function on the raw sound files contained in the package:

``` r
soundscapeR::index_calc(fileloc = location_soundfiles, 
                        progloc = location_software, 
                        samplerate = 44100, 
                        window = 256, 
                        parallel = TRUE)
#> [1] TRUE
```

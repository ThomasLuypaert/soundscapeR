soundscapeR: visualization and richness estimation of soundscapes
================
Thomas Luypaert

<!-- README.md is generated from README.Rmd. Please edit that file -->

<!-- badges: start -->

<!-- badges: end -->

The goal of `soundscapeR` is to provide a standardized analytical
pipeline for the computation, visualization and richness estimation of
soundscapes. The package is designed to work with either continuous or
regular-interval long-duration acoustic recordings, and can handle both
audible and ultrasonic recordings.

## Background

The theoretical background of the `soundscapeR` package is set in the
field of **soundscape ecology**, an area of research which is aimed at
deriving ecological information from the pooled sounds of a population,
community or landscape, also referred to as the **soundscape**.
Soundscape ecology is predicated on the theory of **acoustic niche
partitioning**, which states that acoustic space is a core ecological
resource for which vocally sympatric animals compete, leading to the
partitioning of the acoustic niche in the frequency- and time-domain
through natural selection. As such, it is believed that the diversity of
the vocalizing community can be deducted by looking at the
spectro-temporal diversity of the soundscape.

Substantial progress has been made towards establishing the relationship
the spectro-temporal richness and diversity of the soundscape and
real-life biodiversity. For instance, Aide et al.
([2017](#ref-aide2017species)) measured the **acoustic space use**, or
the proportion of acoustic space which is saturated with sound, and
found a strong positive correlation with both the regional avian species
richness and the richness of unique vocalizations in the recordings.
Similarly, both Burivalova et al. ([2018](#ref-burivalova2018using)) and
Burivalova et al. ([2019](#ref-burivalova2019using)) found strong
positive correlations between the saturation of acoustic space and the
richness of sound types in the recordings.

Still, for newcomers in the field of soundscape ecology, the analytical
processes underlying these findings often remain obscure, with methods
being published without code for application or locked behind a paywall.
Additionally, the vast size of acoustic datasets and time-consuming
nature of aural data exploration make it difficult to get a sense of the
data at hand. In the pursuit of alleviating some of these current
limitations, we developed the `soundscapeR` package as an open-source,
standardized and user-friendly analytical pipeline in R , containing a
range of functions for the calculation, visualization and richness
estimation of soundscapes .

## Installation

You can install the released version of `soundscapeR` from
[GitHub](https://github.com/) with:

``` r
githubinstall("soundscapeR")
```

## The `soundscapeR` workflow

The `soundscapeR` package builds on the methodologies from the
aformentioned studies, providing a streamlined and standardized workflow
for the computation and visualization of acoustic space at various
scales and resolutions. To optimally capture how the acoustic space is
used for different types of sound, we expand the methodology to include
a wider range of acoustic indices, each capturing unique acoustic
features while being minimally intercorrelated. Additionally, the
`soundscapeR` workflow contains a range of highly flexible visualization
tools which allow the user to explore the soundscape visually with
minimal effort.

Below, you can find an example of the `soundscapeR` workflow in action.

### 0\. Priors

The `soundscapeR` package calls on the external software **QUT
Ecoacoustics Analysis Programs**, written in C\#, for the computation of
acoustic indices - see Towsey et al.
([2020](#ref-EcoacousticsAudioAnalysisSoftware)). Prior to starting
analyses in `soundscapeR`, download the ‘AnalysisPrograms.exe’ (AP)
software following
[this](https://research.ecosounds.org/tutorials/ap/practical) link.

Additionally, the QUT Ecoacoustics Analysis Programs software requires
sound files to be named based on one of the following
[formats](https://ap.qut.ecoacoustics.info/basics/dates.html). Make sure
files are named correctly before starting.

**Note:** Make sure you have a back-up of your files before attempting
to rename them, as there is nothing more annoying than losing important
file metadata because of faulty renaming\!

### 1\. The data

The `soundscapeR` packages works on long-duration acoustic recordings,
either recorded continuously or using a regular sampling interval
(*e.g.* 1 min/ 5 min).

For demonstration purposes the package comes with four test datasets on
which to try out the functions. The raw data from which the test
datasets were derived consists of 1603 sound files from the Brazilian
Amazon, recorded at a sampling rate of 384,000 Hz at medium gain, using
a sampling regime of 1-min of recording every 10-min.

### 2\. Computing acoustic indices

The first step of the `soundscapeR` workflow is to compute the acoustic
indices using the `index_calc` command. These indices will later be used
to explore the various ways in which the acoustic space is used.

The following acoustic indices are computed - definitions reproduced
from Towsey ([2017](#ref-towsey2017calculation)):

| Index                                                                 | Abbreviation            | Description                                                                                                                                                                                                                                                                                                                        |
| --------------------------------------------------------------------- | ----------------------- | ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Background noise                                                      | BGN                     | The mode of the distribution of decibel values in each frequency bin, representing the “background” intensity value. This index captures the acoustic energy which persists throughout the duration of the sound file, regardless of its origin (biophonic, geophonic or anthrophonic).                                            |
| Power-minus-noise                                                     | PMN                     | The difference between the maximum decibel value in each frequency bin and the corresponding BGN decibel value.                                                                                                                                                                                                                    |
| Acoustic cover                                                        | CVR                     | The fraction of active elements in each noise-reduced frequency bin where the amplitude exceeds a 3-dB threshold.                                                                                                                                                                                                                  |
| Number of events                                                      | EVN                     | The number of times the decibel value in a noise-reduced frequency bin crosses the 3-dB threshold from lower to higher values.                                                                                                                                                                                                     |
| Temporal entropy                                                      | ENT                     | A measure of acoustic energy concentration in each noise-reduced frequency bin.                                                                                                                                                                                                                                                    |
| Acoustic complexity index                                             | ACI                     | A measure quantifying the variability in intensity values in each noise-reduced frequency bin. It is widely used as a measure of biophony in recordings, however remains highly sensitive to non-biological sources of sound.                                                                                                      |
| Oscillation index                                                     | OSC                     | …                                                                                                                                                                                                                                                                                                                                  |
| Spectral peak tracks                                                  | SPT                     | A measure of the presence of spectral peak tracks in a noise-reduced frequency bin.                                                                                                                                                                                                                                                |
| Ridge indices (Horizontal / Vertical / Upward slope / Downward slope) | (RHZ / RVT / RPS / RNS) | A set of indices based on the presence of formants in the harmonic structure of many animal vocalizations, calculated in the four directions of the ridge slope. Formants in the mid-band are typically due to birdsong, whereas vertical formants are typical for non-biological sounds such as rain drops and electrical clicks. |

For this tutorial, to save you from downloading the raw data and
computing the indices yourself - both of which are rather time consuming
and computationally intensive - the `soundscapeR` package comes with
four test datasets for you to experiment with.

To obtain the test datasets, first, we computed a set of acoustic
indices for all sound files in the recording period using the
`index_calc` function:

`index_calc(fileloc="full-length-path-to-audiofiles",
progloc="full-length-path-to-AP-location", indexlength=60,
samplerate=384000, window=1024)`

Acoustic recordings are processed following the methods outlined in
Towsey ([2017](#ref-towsey2017calculation)), converting 1-min sound
files to spectrograms using short-time Fourier transform with
non-overlapping windows and user-defined window size and sampling rate.
Next, a set of spectral acoustic indices is calculated, each spectral
index consisting of a vector of N-values, one for each frequency bin in
the spectrogram.

For each sound file, the output of the `index_calc` function is stored
in a separate folder located in the same directory as the sound files.
For each spectral index, a ‘.csv’ file with index values is stored in
the output folder. For more information about which acoustic indices are
computed, consult the `index_calc` documentation.

### 3\. Concatenating acoustic index *‘.csv’* files

Next, using the `merge_csv` command, we merged the *‘.csv’* files into a
time-frequency dataframe for an index of choice. The `soundscapeR`
package contains test datasets for four acoustic indices: the **Acoustic
Cover Index** **(CVR)**, the **Acoustic Complexity Index** **(ACI)**,
the **Temporal Entropy Index** **(ENT)** and the **Horizontal Ridge
Index** **(RHZ)**.

`amazon_soundscape_CVR=merge_csv(fileloc =
"full-length-path-to-output-location", samplerate = 384000, window=1024,
index="CVR", lat=-9.595264, lon=-55.932848)`

`amazon_soundscape_ACI=merge_csv(fileloc =
"full-length-path-to-output-location", samplerate = 384000, window=1024,
index="ACI", lat=-9.595264, lon=-55.932848)`

`amazon_soundscape_ENT=merge_csv(fileloc =
"full-length-path-to-output-location", samplerate = 384000, window=1024,
index="ENT", lat=-9.595264, lon=-55.932848)`

`amazon_soundscape_RHZ=merge_csv(fileloc =
"full-length-path-to-output-location", samplerate = 384000, window=1024,
index="RHZ", lat=-9.595264, lon=-55.932848)`

This command takes the individual *‘.csv’* files for the index fo choice
from each folder recursively and merges them together into a single
time-frequency dataframe. For the purposes of this tutorial, we
subsetted the soundscape datasets to include only the frequencies
between 0-21,000 Hz.

The output of these commands is available as testing data in the
`soundscapeR` package:

``` r
library(soundscapeR)
data("amazon_soundscape_CVR")
data("amazon_soundscape_ACI")
data("amazon_soundscape_ENT")
data("amazon_soundscape_RHZ")
```

Now that you know how our testing data was derived, let’s get started\!
In the next section we will focus on a single acoustic index, the
**Acoustic Cover Index**.

First, lets have a look at the testing data:

``` r
head(amazon_soundscape_CVR)[1:5]
#>         10:10:00   10:20:00   10:30:00   10:40:00   10:50:00
#> 21000 0.06702222 0.06804444 0.08928889 0.08213333 0.05853333
#> 20625 0.06191111 0.06591111 0.08995556 0.08364444 0.05697778
#> 20250 0.06133333 0.06591111 0.08097778 0.07795556 0.05586667
#> 19875 0.05871111 0.06155556 0.07595556 0.07960000 0.05315556
#> 19500 0.05968889 0.05911111 0.07395556 0.07346667 0.05168889
#> 19125 0.06426667 0.06080000 0.07506667 0.07448889 0.05240000
tail(amazon_soundscape_CVR)[1:5]
#>        10:10:00   10:20:00    10:30:00    10:40:00   10:50:00
#> 2250 0.22657778 0.13515556 0.107555556 0.115911111 0.12697778
#> 1875 0.16284444 0.08368889 0.055200000 0.053600000 0.06168889
#> 1500 0.07728889 0.03097778 0.011466667 0.013111111 0.02208889
#> 1125 0.06048889 0.01817778 0.007022222 0.008533333 0.01622222
#> 750  0.23728889 0.18426667 0.193022222 0.201244444 0.18706667
#> 375  0.21355556 0.16968889 0.181111111 0.183911111 0.16591111

min(colnames(amazon_soundscape_CVR))
#> [1] "00:00:00"
max(colnames(amazon_soundscape_CVR))
#> [1] "23:50:00"
```

As you can see, for each value of the ‘CVR’ index, we have the time at
which recordings were taken as the column names, and the frequency bin
for each value as the row names. Using `head()` and `tail()` reveals
that we have ‘CVR’ values from 0-21,000 Hz, separated into frequency
bins of 375 Hz width. The `min()` and `max()` show us we have data for
the whole day, from midnight (“00:00:00”) to 10-min to midnight
(“23:50:00”).

### 4\. Thresholding the spectral index dataframe

The next step in the `soundscapeR` workflow is thresholding. This step
is used to separate the acoustically active time-frequency bins (the
signal) from the background values (the noise). This is done with the
`binarize_df` function, which uses a binarization algorithm to turn
index values into either 1 (active) or 0 (inactive).

``` r
amazon_binarized=binarize_df(df=amazon_soundscape_CVR, method = "Otsu")
```

Several binarization algorithms from the `autothresholdr` package are
available - see Landini et al. ([2017](#ref-autothresholdr)). Consult
the following [link](https://imagej.net/Auto_Threshold) for more
information about the available algorithms.

It is import to check the output of our thresholding step and compare
the result to the original data to make sure the algorithm is working
properly. Luckily, `soundscapeR` has a quick interactive visualization
tool based of the `d3heatmap` package by Cheng and Galili
([2018](#ref-d3heatmap)) to do just this:

``` r
before_thresholding=quick_heatmap(amazon_soundscape_CVR)
after_thresholding=quick_heatmap(amazon_binarized)
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" /><img src="man/figures/README-unnamed-chunk-6-2.png" width="100%" />

A quick visual inspection of the interactive heatmaps produced by
`quick_heatmap` reveals the thresholding succesfully separated the
signal from the background noise. Try out several methods to get the
best one.

### 5\. Aggregating the acoustic activity vectors by time-of-day

Now that we have succesfully separated the signal from the noise, we
will pool our acoustic activity vectors (the columns of the dataframe
produced by ) by time-of-day using a specified aggregation interval, sum
the activity values for the same frequency bin in each time period, and
divide the sum by the number of recordings available for that time.
Doing so, we get a time-frequency dataframe with the proportion of
acoustically active recordings in each time-frequency bin during the
recording period.

Let’s try and aggregate our recordings at several intervals, starting at
10-min, the highest possible resolution we can obtain considering we
only collected our recordings at 10-min intervals:

``` r
amazon_aggregated_10=aggregate_df(amazon_binarized, 10, "2019-11-12", lat=-9.595264, lon=-55.932848)

amazon_aggregated_20=aggregate_df(amazon_binarized, 20, "2019-11-12", lat=-9.595264, lon=-55.932848)

amazon_aggregated_30=aggregate_df(amazon_binarized, 30, "2019-11-12", lat=-9.595264, lon=-55.932848)

amazon_aggregated_60=aggregate_df(amazon_binarized, 60, "2019-11-12", lat=-9.595264, lon=-55.932848)
```

### 6\. Visualizing the soundscape

At this point we have done the major manipulations on our data, and we
are ready to take a look at the soundscape. Do do this, the
`soundscapeR` package contains the highly flexible `heatmapper`
function.

In it’s simplest form, `heatmapper` produces a rudimentary heatmap
displaying how the acoustic space is used in the time-frequency domain
for the index of choice.

Let’s have a look at the effect of aggregating at different resolutions:

``` r
simple_heatmap_10=heatmapper(amazon_aggregated_10, type="regular", annotate = FALSE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848) 

simple_heatmap_20=heatmapper(amazon_aggregated_20, type="regular", annotate = FALSE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848)

simple_heatmap_30=heatmapper(amazon_aggregated_30, type="regular", annotate = FALSE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848)

simple_heatmap_60=heatmapper(amazon_aggregated_60, type="regular", annotate = FALSE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848)
```

``` r
simple_heatmap_10
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
simple_heatmap_20
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

``` r
simple_heatmap_30
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

``` r
simple_heatmap_60
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

As you can see, aggregating at shorter durations increases the
resolution of the soundscape plot, at the expense of having less
recordings for each time to measure the use of acoustic space. The
choice of aggregation interval will depend on how many recordings are
available for each time period to estimate the acoustic space use
reliably.

Now, let’s have a look at the various plotting options offered by
`heatmapper`.

First, `heatmapper` is capable of producing two kinds of plots using the
`type` argument:

``` r
# type="regular"
regular_heatmap_10=heatmapper(amazon_aggregated_10, type="regular", annotate = FALSE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848) 

# type="polar"

polar_heatmap_10=heatmapper(amazon_aggregated_10, type="polar", annotate = FALSE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848) 
```

``` r
regular_heatmap_10
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />

``` r
polar_heatmap_10
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" />

The polar heatmap represents a useful way of investigating diurnal
patterns in the use of acoustic space. For instance, this plot reveals
that the acoustic space for the ‘CVR’ index is more filled in the
nighttime compared to the daytime.

Let’s investigate this further by looking at another plotting option in
`heatmapper`, the `annotate` argument:

``` r
# type="regular" and annotate=TRUE

regular_heatmap_10_annotated=heatmapper(amazon_aggregated_10, type="regular", annotate = TRUE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848) 

# type="polar" and annotate=TRUE

polar_heatmap_10_annotated=heatmapper(amazon_aggregated_10, type="polar", annotate = TRUE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848) 
```

``` r
regular_heatmap_10_annotated
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="100%" />

``` r
polar_heatmap_10_annotated
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="100%" />

If `annotate=TRUE`, `heatmapper` calculates the sunrise and sunset times
for the soundscape based on the supplied date and geographic coordinates
using the `photobiology` package (Aphalo ([2015](#ref-photobiology))),
and annotates the plot. Additionally, `heatmapper` annotates the
boundary between the audible and ultrasonic spectrum at 20,000 Hz.

**Note:** The time of sunrise and sunset is determined by which type of
solar elevation angle is considered. The `twilight` argument allows the
user to specify which method is used, or define a custom solar elevation
angle in degrees. Check out the
[`photobiology`](https://www.rdocumentation.org/packages/photobiology/versions/0.9.6/topics/day_night)
package documentation for options.

As we can see, the soundscape is mostly filled at night, and a slight
lag can be observed after sunset before the vocalizations commence.
Additionally, we can observe a dawn chorus right after sunrise, where
the soundscape is filled over a larger part of the frequency range.

If we would like to interact with the plot to explore the values of the
time-frequency bins, we can set `interactive=TRUE`:

``` r

# interactive=TRUE 

regular_heatmap_10_interactive=heatmapper(amazon_aggregated_10, type="regular", annotate = TRUE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848, interactive = TRUE) 
```

<img src="man/figures/README-unnamed-chunk-20-1.png" width="100%" />

The interactive mode can be used to find out the proportion of acoustic
recordings which were active for each time-frequency bin in the
recording period. Moreover, it can be used to zoom into certain parts of
the plot and investigate patterns up close. Note that the interactive
plot is not yet supported for `type="polar"`.

**Note:** The GitHub README file does not support interactive plots, so
a screenshot of the *‘.html’* widget is displayed.

If we would like to dive even further into the patterns of acoustic
space use, we can add marginal plots displaying the richness of
acoustically active frequency-bin for each time, and acoustically active
time-bins for each frequency band using the `marginplot` argument:

``` r

# marginplot=TRUE

regular_heatmap_10_marginplot=heatmapper(amazon_aggregated_10, type="regular", annotate = TRUE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848, marginplot = TRUE, nbins=10)
#> Registered S3 methods overwritten by 'huge':
#>   method    from   
#>   plot.sim  BDgraph
#>   print.sim BDgraph
#> Registered S3 method overwritten by 'geiger':
#>   method            from
#>   unique.multiPhylo ape
```

``` r
regular_heatmap_10_marginplot
```

<img src="man/figures/README-unnamed-chunk-22-1.png" width="100%" />

The addition of the margin plot confirms our previous findings. The
richness of acoustically active frequency bins is highest at nighttime,
with a peak at the dawn chorus (purple plot). Moreover, throughout the
whole time period, the frequency space is most full between 2000-6000 Hz
(yellow plot).

Finally, the `heatmapper` function allows us to play around with some
aesthetic properties such as color scales to produce the plot of your
liking using the `palette` and `direction` arguments:

``` r

# palette="D" and direction=1
regular_heatmap_10_palette=heatmapper(amazon_aggregated_10, type="regular", annotate = TRUE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848, marginplot = TRUE, nbins=10, palette = "A", direction = 1) 
```

``` r
regular_heatmap_10_palette
```

<img src="man/figures/README-unnamed-chunk-24-1.png" width="100%" />

### 7\. Estimating soundscape richness metrics

Next up in the soundscaper workflow, we can continue our exploration of
acoustic space use in the time-frequency domain by calculating several
soundscape richness metrics.

As the margin plots revealed earlier, we can measure how *“full”* the
acoustic space is at several scales and resolutions by calculating the
*“richness”* of acoustically active (value\>0) time-frequency bins. We
do this using the `soundscape_richness` function.

**Note:** Here we make the distinction between the **soundscape
richness** as the number of time-frequency bins in the soundscape with a
value \> 0 - and the **acoustic space use** or **soundscape saturation**
as the proportion of the acoustic space which is used, expressed in
percent (richness divided by the number of time-frequency bins in the
soundscape).

First, let’s have a look at the soundscape richness and saturation for
the total soundscape. To determine whether the richness of saturation is
computed, the `output` argument is used:

``` r

# For richness: output="raw"

soundscape_richness(amazon_aggregated_10, type="total", date="2019-11-12", lat=-9.595264, lon=-55.932848, output="raw")
#> [1] 1708

# For saturation: output="percentage"

soundscape_richness(amazon_aggregated_10, type="total", date="2019-11-12", lat=-9.595264, lon=-55.932848, output="percentage")
#> [1] 21.18056
```

Based on `soundscape_richness`, the soundscape contains 1708
acoustically active time-frequency bins for the ‘CVR’ index, meaning
21.2% of the total acoustic space is in use.

Now, let’s continue looking at the saturation of acoustic space at
different times of day:

``` r

# Daytime

soundscape_richness(amazon_aggregated_10, type="day", date="2019-11-12", lat=-9.595264, lon=-55.932848, output="percentage")
#> [1] 14.19048

# Nighttime

soundscape_richness(amazon_aggregated_10, type="night", date="2019-11-12", lat=-9.595264, lon=-55.932848, output="percentage")
#> [1] 28.77847

# Dawn

soundscape_richness(amazon_aggregated_10, type="dawn", date="2019-11-12", lat=-9.595264, lon=-55.932848, output="percentage")
#> [1] 57.34127

# Dusk

soundscape_richness(amazon_aggregated_10, type="dusk", date="2019-11-12", lat=-9.595264, lon=-55.932848, output="percentage")
#> [1] 4.563492

# Each time of day

soundscape_richness(amazon_aggregated_10, type="tod", date="2019-11-12", lat=-9.595264, lon=-55.932848, output="percentage")
#>   [1]  21.428571  32.142857  32.142857  32.142857  28.571429  23.214286
#>   [7]  23.214286  26.785714  30.357143  28.571429  25.000000  23.214286
#>  [13]  26.785714  25.000000  30.357143  30.357143  53.571429  30.357143
#>  [19]  48.214286 100.000000  30.357143  55.357143  32.142857  28.571429
#>  [25]  39.285714  26.785714  44.642857  37.500000  37.500000  48.214286
#>  [31]  28.571429  32.142857  83.928571  66.071429  51.785714  60.714286
#>  [37]  50.000000  76.785714  33.928571  28.571429  64.285714  83.928571
#>  [43]  60.714286  55.357143  62.500000  60.714286  17.857143  21.428571
#>  [49]  26.785714  16.071429   3.571429   7.142857   3.571429  12.500000
#>  [55]   8.928571  10.714286   3.571429   3.571429   3.571429   1.785714
#>  [61]   7.142857   3.571429   3.571429   0.000000   0.000000   0.000000
#>  [67]   0.000000   3.571429   1.785714   1.785714   0.000000   0.000000
#>  [73]   0.000000   0.000000   1.785714   0.000000   0.000000   0.000000
#>  [79]   0.000000   0.000000   0.000000   0.000000   1.785714   0.000000
#>  [85]   1.785714   0.000000   0.000000   1.785714   3.571429   0.000000
#>  [91]   0.000000   1.785714   0.000000   1.785714   3.571429   1.785714
#>  [97]   1.785714   0.000000   0.000000   3.571429   5.357143   5.357143
#> [103]   3.571429   7.142857   5.357143   5.357143   5.357143   5.357143
#> [109]   5.357143   3.571429   3.571429   3.571429   5.357143   8.928571
#> [115]  12.500000  14.285714  25.000000  21.428571  21.428571  23.214286
#> [121]  23.214286  26.785714  28.571429  23.214286  26.785714  19.642857
#> [127]  26.785714  25.000000  37.500000  33.928571  32.142857  75.000000
#> [133]  23.214286  32.142857  28.571429  30.357143  25.000000  28.571429
#> [139]  25.000000  23.214286  32.142857  30.357143  32.142857  30.357143
```

As we previously found out, the soundscape saturation during the day
(14.2%) is lower than during the night (28.8%). Moreover, as suspected,
the saturation is highest during the dawn chorus (57.3%). The dusk
period has the lowest soundscape saturation (4.6%).

**Note:** The definition of dusk and dawns depends on the region, the
system of interest and the personal opinion of the researcher. The
`dawnstart`, `dawnend`, `duskstart` and `duskend` arguments allow the
user to specify exactly when dawn and dusk take place compared to
sunrise and sunset.

We can investigate in which part of the frequency domain the acoustic
space is most full using the `freqseq` and `nbins` arguments:

``` r

# Nighttime soundscape saturation per frequency band

soundscape_richness(amazon_aggregated_10, type="night", date="2019-11-12", lat=-9.595264, lon=-55.932848, output="percentage", freqseq = TRUE, nbins=14)
#>  [1] 11.111111 89.855072 86.956522 90.338164  9.661836 45.893720 18.357488
#>  [8]  3.864734  4.830918  3.864734 14.975845  8.695652  6.763285  1.449275
```

As previously mentioned, the lower frequency bins between 0-6000 Hz have
the highest soundscape saturation. We can explore these patterns further
in the next section.

### 8\. Visualizing Soundscape Saturation

If we want to visualize how the soundscape saturation changes throughout
the day, and how different frequency bins contribute to the total
soundscape saturation, the `richness_by_time` function can do just that.

Several graph types exists:

``` r
richness_by_time_total=richness_by_time(amazon_aggregated_10, "total", "2019-11-12",lat=-9.595264, lon=-55.932848, nbins=14, smooth=TRUE, interactive = FALSE)
```

``` r
richness_by_time_total
```

<img src="man/figures/README-unnamed-chunk-29-1.png" width="100%" />

``` r
richness_by_time_frequency=richness_by_time(amazon_aggregated_10, "frequency", "2019-11-12",lat=-9.595264, lon=-55.932848, nbins=14, smooth=TRUE, interactive = FALSE)
```

``` r
richness_by_time_frequency
```

<img src="man/figures/README-unnamed-chunk-31-1.png" width="100%" />

``` r
richness_by_time_normfreq=richness_by_time(amazon_aggregated_10, "normfreq", "2019-11-12",lat=-9.595264, lon=-55.932848, nbins=14, smooth=TRUE, interactive = FALSE)
```

``` r
richness_by_time_normfreq
```

<img src="man/figures/README-unnamed-chunk-33-1.png" width="100%" />

``` r
richness_by_time_linefreq=richness_by_time(amazon_aggregated_10, "linefreq", "2019-11-12",lat=-9.595264, lon=-55.932848, nbins=14, smooth=TRUE, interactive = FALSE, timeinterval = "2 hours")
```

``` r
richness_by_time_linefreq
```

<img src="man/figures/README-unnamed-chunk-35-1.png" width="100%" />

These plots allow the user to see the variation in total soundscape
saturation througout the day, and the contribution of frequency-bins
with user-defined width to the total soundscape saturation at each
unique time of day.

## 9\. Compare acoustic space use for different types of sound

The set of indices included in the `soundscapeR` package each capture
unique properties of sound, and are thus sensitive to different sound
sources. The ways in which the acoustic space is used by different types
of sound can be compared by looking at different indices for the same
soundscape.

Let’s do a quick visual comparison of four different indices for which
test data is included in the `soundscapeR` package:

``` r
# Thresholding

  # CVR

amazon_binarized_CVR=binarize_df(df=amazon_soundscape_CVR, method = "Otsu")

  # ACI
amazon_binarized_ACI=binarize_df(df=amazon_soundscape_ACI, method = "Mode", strictness = 0.9)

  # ENT

amazon_binarized_ENT=binarize_df(df=amazon_soundscape_ENT, method="Otsu", strictness = 1.2)

  #RHZ

amazon_binarized_RHZ=binarize_df(df=amazon_soundscape_RHZ, method="Otsu")
```

**Note:** As you can see in the code above, a different binarization
algorithm was used for the Acoustic Complexity Index. Modal subtraction
with an adapted *strictness* worked best in this case. Always verify the
thresholding output before proceeding.

``` r
# Aggregation 

  # CVR

amazon_aggregated_CVR=aggregate_df(amazon_binarized_CVR, 10, "2019-11-12", lat=-9.595264, lon=-55.932848)

  # ACI

amazon_aggregated_ACI=aggregate_df(amazon_binarized_ACI, 10, "2019-11-12", lat=-9.595264, lon=-55.932848)

  # ENT

amazon_aggregated_ENT=aggregate_df(amazon_binarized_ENT, 10, "2019-11-12", lat=-9.595264, lon=-55.932848)

  # RHZ

amazon_aggregated_RHZ=aggregate_df(amazon_binarized_RHZ, 10, "2019-11-12", lat=-9.595264, lon=-55.932848)
```

``` r
# Visualization

  # CVR

regular_heatmap_annotated_CVR=heatmapper(amazon_aggregated_CVR, type="regular", annotate = TRUE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848, palette = "D") 

  # ACI

regular_heatmap_annotated_ACI=heatmapper(amazon_aggregated_ACI, type="regular", annotate = TRUE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848, palette = "D") 

  # ENT

regular_heatmap_annotated_ENT=heatmapper(amazon_aggregated_ENT, type="regular", annotate = TRUE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848, palette = "D") 

  # RHZ

regular_heatmap_annotated_RHZ=heatmapper(amazon_aggregated_RHZ, type="regular", annotate = TRUE, timeinterval = "1 hour", freqinterval = 2000, date="2019-11-12", lat=-9.595264, lon=-55.932848, palette = "D") 
```

``` r
regular_heatmap_annotated_CVR
```

<img src="man/figures/README-unnamed-chunk-39-1.png" width="100%" />

``` r
regular_heatmap_annotated_ACI
```

<img src="man/figures/README-unnamed-chunk-40-1.png" width="100%" />

``` r
regular_heatmap_annotated_ENT
```

<img src="man/figures/README-unnamed-chunk-41-1.png" width="100%" />

``` r
regular_heatmap_annotated_RHZ
```

<img src="man/figures/README-unnamed-chunk-42-1.png" width="100%" />

The comparison of the acoustic space use heatmaps for the different
indices reveals two main things:

  - Certain indices are more sensitive to non-biological noise than
    other. For instance, the acoustic complexity index shows several
    vertical bands which have high values across the frequency spectrum.
    This is often indicative of some source of non-biological sound, in
    this case likely tropical rain. Similarly, the Temporal entropy
    index also has vertical banding. This often occurs when, for
    instance, a gust of wind *‘saturates’* the microphone, generating
    highly concentrated sound across all frequency bands in the sound
    file. For these types of indices, before any biological patterns can
    be explored, removal of files with heavy rain or wind might be
    required.

  - Different indices reveal different patterns. Per example, for the
    ACI, the patterns at 2000-10,000 Hz between 02:00-10:00, which are
    likely of biological origin, show a different structure than
    previously seen for the CVR index. Additionally, the ACI picks up a
    constant source of sound of unknown origin at 12,000 Hz, 15,000 Hz
    and 20,000 Hz.

## Summary

The `soundscapeR` package contains a range of powerful functions for the
computation of acoustic indices, each of which capture unique properties
of the soundscape. Moreover, for each of these indices, the package
contains functions for the visualization of acoustic space use and
calculation of soundscape richness metrics at various scales and
resolutions.

The workflow can be used to investigate the change in soundscape
properties along environmental gradients, or after interventions. It
provides a useful tool to investigate where (frequency domain) and when
(time domain) in the soundscape each unique type of sound (acoustic
index of choice) is being lost/gained.

**Note:** `soundscapeR` output should not be used to make inference
about real-life community richness or biodiversity before a relationship
between the index of choice and ground-truthed biodiversity has been
established.

## References

<div id="refs" class="references">

<div id="ref-aide2017species">

Aide, T Mitchell, Andres Hernández-Serna, Marconi Campos-Cerqueira,
Orlando Acevedo-Charry, and Jessica L Deichmann. 2017. “Species Richness
(of Insects) Drives the Use of Acoustic Space in the Tropics.” *Remote
Sensing* 9 (11): 1096.

</div>

<div id="ref-photobiology">

Aphalo, Pedro J. 2015. “The R4photobiology Suite.” *UV4Plants Bulletin*
2015 (1): 21–29. <https://doi.org/10.19232/uv4pb.2015.1.14>.

</div>

<div id="ref-burivalova2018using">

Burivalova, Zuzana, Michael Towsey, Tim Boucher, Anthony Truskinger,
Cosmas Apelis, Paul Roe, and Edward T Game. 2018. “Using Soundscapes to
Detect Variable Degrees of Human Influence on Tropical Forests in Papua
New Guinea.” *Conservation Biology* 32 (1): 205–15.

</div>

<div id="ref-burivalova2019using">

Burivalova, Zuzana, Bambang Wahyudi, Timothy M Boucher, Peter Ellis,
Anthony Truskinger, Michael Towsey, Paul Roe, Delon Marthinus, Bronson
Griscom, and Edward T Game. 2019. “Using Soundscapes to Investigate
Homogenization of Tropical Forest Diversity in Selectively Logged
Forests.” *Journal of Applied Ecology* 56 (11): 2493–2504.

</div>

<div id="ref-d3heatmap">

Cheng, Joe, and Tal Galili. 2018. *D3heatmap: Interactive Heat Maps
Using ’Htmlwidgets’ and ’D3.js’*.
<https://CRAN.R-project.org/package=d3heatmap>.

</div>

<div id="ref-autothresholdr">

Landini, G., D. A. Randell, S. Fouad, and A. Galton. 2017. “Automatic
Thresholding from the Gradients of Region Boundaries.” *Journal of
Microscopy* 265 (2): 185–95.

</div>

<div id="ref-EcoacousticsAudioAnalysisSoftware">

Towsey, Michael, Anthony Truskinger, Mark Cottman-Fields, and Paul Roe.
2020. *Ecoacoustics Audio Analysis Software* (version 20.2.0.99).
<http://doi.org/10.5281/zenodo.1188744>.

</div>

<div id="ref-towsey2017calculation">

Towsey, Michael W. 2017. “The Calculation of Acoustic Indices Derived
from Long-Duration Recordings of the Natural Environment.”

</div>

</div>

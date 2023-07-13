#' Create differential soundscape heatmaps
#'
#' @description  Creates a differential soundscape heatmap that shows the
#' difference in OSU presence and prevalence between two soundscape objects.
#'
#' @param soundscape_obj_A A soundscape object produced by the \code{\link{ss_create}}
#'  function (or ss_index_merge, ss_binarize, and ss_aggregate in sequence). This will be
#'  the first soundscape for comparison.
#'
#' @param soundscape_obj_B A soundscape object produced by the \code{\link{ss_create}}
#'  function (or ss_index_merge, ss_binarize, and ss_aggregate in sequence). This will be
#'  the second soundscape for comparison.
#'
#' @param type One of either "regular" or "polar". If set
#' to "regular", produces a regular rectangular heatmap.
#' If set to "polar", produces a polar heatmap suitable for
#'  exploring diurnal patterns.
#'
#' @param timeinterval A time interval for the x-axis.
#'  Options can be found in the
#'  \code{\link[scales]{breaks_width}} documentation.
#'
#' @param mintime The lower time limit for the x-axis,
#'  formatted as "HH:MM:SS". Defaults to the earliest
#'  time for which data exists in the dataframe.
#'
#' @param maxtime The upper time limit for the x-axis,
#'  formatted as "HH:MM:SS".Defaults to the latest
#'  time for which data exists in the dataframe.
#'
#' @param freqinterval The frequency interval for the y-axis,
#'  expressed as a numeric value.
#'
#' @param minfreq The lower frequency limit for the y-axis
#' as a numeric value. Defaults to zero.
#'
#' @param maxfreq The upper frequency limit for the y-axis
#' as a numeric value. Defaults to the maximum frequency of
#'  the dataframe.
#'
#' @param labelsize_time If annotate=TRUE, can be used to
#'  alter the size of temporal spectrum annotation. Please
#'   provide as a single positive integer with a value > 0.
#'
#' @param labelsize_frequency If annotate=TRUE, can be used
#'  to alter the size of frequency spectrum annotation.
#'  Please provide as a single positive integer with a
#'  value > 0.
#'
#' @param labelsize_polar If type="polar", can be used to
#'  alter the size of the frequency axis labels. Please
#'   provide as a single positive integer with a value > 0.
#'
#' @param palette A character string indicating the
#' colormap option to use. Four options are available:
#'  "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"),
#'  "viridis" (or "D", the default option) and "cividis"
#'  (or "E"). Consult \url{https://www.rdocumentation.org/packages/viridisLite/versions/0.3.0/topics/viridis} for options.
#'
#' @param direction Sets the order of colors in the scale.
#' If 1, the default, the regular order is followed.
#' If -1, the order of colors is reversed.
#'
#' @param zero.black One of either TRUE or FALSE.
#' If set to TRUE, absent OSUs with incidence zero
#' will be colored black
#'
#' @param interactive One of either TRUE or FALSE.
#' If set to TRUE, an interactive plot is produced using
#'  \code{\link[plotly]{ggplotly}}.
#'
#' @return Returns a differential heatmap
#'
#' @export

ss_compare <- function(soundscape_obj_A,
                       soundscape_obj_B,
                       type = "regular",
                       timeinterval = "1 hour",
                       mintime = "default",
                       maxtime = "default",
                       freqinterval = 2000,
                       minfreq = 0,
                       maxfreq = "default",
                       labelsize_time = 4,
                       labelsize_frequency = 4,
                       labelsize_polar = 3,
                       palette = "B",
                       direction = 1,
                       zero.black = FALSE,
                       interactive = FALSE) {
  # 0. Tests

  # 0.0. Setting binding for global variables

  time <- value <- comma <- unit <- guides <- NULL

  # 0.1. Testing functions

  # 0.1.1. Check if the arguments are missing

  test_1 <- function(x) {
    !missing(x)
  }

  assertthat::on_failure(test_1) <- function(call, env) {
    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")
  }

  assertthat::assert_that(test_1(soundscape_obj_A))
  assertthat::assert_that(test_1(soundscape_obj_B))

  # 0.1.2. Check if soundscape_obj arguments meet expectations

  # The supplied soundscape_obj arguments are S4-object of the type
  # 'soundscape', and is not empty.

  test_2 <- function(x) {
    isS4(x) &
      assertthat::are_equal(class(x)[1], "soundscape") &
      assertthat::not_empty(x)
  }

  assertthat::on_failure(test_2) <- function(call, env) {
    paste0(deparse(call$x), " is not an S4-object of the type 'soundscape', or is empty. Please supply the soundscape_obj object produced by the ss_aggregate() function. Consult the package documentation for further information.")
  }

  assertthat::assert_that(test_2(soundscape_obj_A))
  assertthat::assert_that(test_2(soundscape_obj_B))

  # 0.1.3. The soundscape_obj elements are in the expected format

  # The lat and lon argument

  test_3 <- function(x) {
    is.numeric(x) &
      x >= -90 &
      x <= 90
  }

  test_4 <- function(x) {
    is.numeric(x) &
      x >= -180 &
      x <= 180
  }

  assertthat::on_failure(test_3) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")
  }

  assertthat::on_failure(test_4) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid coordinate. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create function? If so, something has gone wrong, please re-run the ss_index_merge(), ss_binarize() and ss_aggregate() or ss_create() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")
  }

  assertthat::assert_that(test_3(soundscape_obj_A@lat))
  assertthat::assert_that(test_4(soundscape_obj_A@lon))
  assertthat::assert_that(test_3(soundscape_obj_B@lat))
  assertthat::assert_that(test_4(soundscape_obj_B@lon))

  # The time zone argument

  test_5 <- function(x) {
    assertthat::is.string(x) & (x %in% (OlsonNames()))
  }

  assertthat::on_failure(test_5) <- function(call, env) {
    paste0(deparse(call$x), " is not a recognized timezone. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")
  }

  assertthat::assert_that(test_5(soundscape_obj_A@tz))
  assertthat::assert_that(test_5(soundscape_obj_B@tz))

  # The index argument

  test_6 <- function(x) {
    assertthat::is.string(x) & (x %in% c(
      "BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
      "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"
    ))
  }

  assertthat::on_failure(test_6) <- function(call, env) {
    paste0(deparse(call$x), " is not a character string of one of the available index options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")
  }

  assertthat::assert_that(test_6(soundscape_obj_A@index))
  assertthat::assert_that(test_6(soundscape_obj_B@index))

  # The samplerate and window arguments

  test_7 <- function(x) {
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_7) <- function(call, env) {
    paste0(deparse(call$x), " is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.")
  }

  assertthat::assert_that(test_7(soundscape_obj_A@samplerate))
  assertthat::assert_that(test_7(soundscape_obj_A@window))
  assertthat::assert_that(test_7(soundscape_obj_B@samplerate))
  assertthat::assert_that(test_7(soundscape_obj_B@window))

  # The binarization_method argument

  test_8 <- function(x) {
    assertthat::is.string(x) & (x %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen", "Mode", "custom"))
  }



  assertthat::on_failure(test_8) <- function(call, env) {
    paste0(deparse(call$x), " is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")
  }

  assertthat::assert_that(test_8(soundscape_obj_A@binarization_method))
  assertthat::assert_that(test_8(soundscape_obj_B@binarization_method))

  # The threshold argument

  test_9 <- function(x) {
    all(length(x) == 1 &
      is.double(x) & !is.na(x))
  }

  assertthat::on_failure(test_9) <- function(call, env) {
    paste0(deparse(call$x), " is not a single numeric value. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the value argument is you're supplying a custom threshold value.")
  }

  assertthat::assert_that(test_9(soundscape_obj_A@threshold))
  assertthat::assert_that(test_9(soundscape_obj_B@threshold))

  # The output argument

  test_10 <- function(x) {
    all(length(x) == 1 & is.character(x) & (x %in% c("incidence_freq", "raw")))
  }

  assertthat::on_failure(test_10) <- function(call, env) {
    paste0(deparse(call$x), " is not a character string describing one of the available output options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.")
  }

  assertthat::assert_that(test_10(soundscape_obj_A@output))
  assertthat::assert_that(test_10(soundscape_obj_B@output))

  # The merged_df argument

  test_11 <- function(x) {
    is.data.frame(x) &
      assertthat::not_empty(x) &
      assertthat::noNA(x) &
      all(apply(x, 2, function(y) all(is.numeric(y))))
  }

  test_12_A <- function(x) {
    (abs(as.numeric(rownames(x)[1])) +
      abs(as.numeric(rownames(x)[2]))) > 3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x))) <= soundscape_obj_A@samplerate / 2
  }

  test_12_B <- function(x) {
    (abs(as.numeric(rownames(x)[1])) +
      abs(as.numeric(rownames(x)[2]))) > 3 &
      min(as.numeric(rownames(x))) >= 0 &
      max(as.numeric(rownames(x))) <= soundscape_obj_B@samplerate / 2
  }

  test_13_A <- function(x) {
    formatted <- try(
      as.POSIXct(
        paste0(substr(soundscape_obj_A@first_day, 1, 12), " ", colnames(x)),
        tz = soundscape_obj_A@tz,
        format = "%Y-%m-%d %H:%M:%S"
      ),
      silent = TRUE
    )

    !any(sapply(formatted, function(y) is.na(y)))
  }

  test_13_B <- function(x) {
    formatted <- try(
      as.POSIXct(
        paste0(substr(soundscape_obj_B@first_day, 1, 12), " ", colnames(x)),
        tz = soundscape_obj_B@tz,
        format = "%Y-%m-%d %H:%M:%S"
      ),
      silent = TRUE
    )

    !any(sapply(formatted, function(y) is.na(y)))
  }


  assertthat::on_failure(test_11) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.")
  }

  assertthat::on_failure(test_12_A) <- function(call, env) {
    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.")
  }

  assertthat::on_failure(test_12_B) <- function(call, env) {
    paste0(deparse(call$x), " does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.")
  }

  assertthat::assert_that(test_11(soundscape_obj_A@merged_df))
  assertthat::assert_that(test_12_A(soundscape_obj_A@merged_df))

  assertthat::assert_that(test_11(soundscape_obj_B@merged_df))
  assertthat::assert_that(test_12_B(soundscape_obj_B@merged_df))

  # The binarized_df argument

  test_14 <- function(x) {
    min(x) >= 0 &
      max(x) <= 1
  }

  assertthat::on_failure(test_14) <- function(call, env) {
    paste0(deparse(call$x), " has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() or ss_create() function.")
  }

  assertthat::assert_that(test_11(soundscape_obj_A@binarized_df))
  assertthat::assert_that(test_12_A(soundscape_obj_A@binarized_df))
  assertthat::assert_that(test_13_A(soundscape_obj_A@binarized_df))
  assertthat::assert_that(test_14(soundscape_obj_A@binarized_df))

  assertthat::assert_that(test_11(soundscape_obj_B@binarized_df))
  assertthat::assert_that(test_12_B(soundscape_obj_B@binarized_df))
  assertthat::assert_that(test_13_B(soundscape_obj_B@binarized_df))
  assertthat::assert_that(test_14(soundscape_obj_B@binarized_df))

  # The aggregated_df argument

  assertthat::assert_that(test_11(soundscape_obj_A@aggregated_df))
  assertthat::assert_that(test_12_A(soundscape_obj_A@aggregated_df))
  assertthat::assert_that(test_13_A(soundscape_obj_A@aggregated_df))

  assertthat::assert_that(test_11(soundscape_obj_B@aggregated_df))
  assertthat::assert_that(test_12_B(soundscape_obj_B@aggregated_df))
  assertthat::assert_that(test_13_B(soundscape_obj_B@aggregated_df))

  if (soundscape_obj_A@output == "incidence_freq" & soundscape_obj_B@output == "incidence_freq") {
    test_15 <- function(x) {
      all(is.double(unlist(x)) & max(x) <= 1 & min(x) >= 0)
    }

    assertthat::on_failure(test_15) <- function(call, env) {
      paste0(deparse(call$x), " contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.")
    }

    assertthat::assert_that(test_15(soundscape_obj_A@aggregated_df))
    assertthat::assert_that(test_15(soundscape_obj_B@aggregated_df))
  } else {
    if (soundscape_obj_A@output == "raw" & soundscape_obj_B@output == "raw") {
      test_15_A <- function(x) {
        all(all(round(unlist(x)) == unlist(x)) &
          max(x) <= max(table(colnames(soundscape_obj_A@merged_df))) &
          min(x) >= 0)
      }

      test_15_B <- function(x) {
        all(all(round(unlist(x)) == unlist(x)) &
          max(x) <= max(table(colnames(soundscape_obj_B@merged_df))) &
          min(x) >= 0)
      }

      assertthat::on_failure(test_15_A) <- function(call, env) {
        paste0(deparse(call$x), " contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.")
      }

      assertthat::on_failure(test_15_B) <- function(call, env) {
        paste0(deparse(call$x), " contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.")
      }

      assertthat::assert_that(test_15_A(soundscape_obj_A@aggregated_df))
      assertthat::assert_that(test_15_B(soundscape_obj_B@aggregated_df))
    } else {
      print("soundscape_obj_A and/or soundscape_obj_B don't have the correct output argument")
      Sys.sleep(0.0000000000000001)
    }
  }


  # 0.1.4. Check if supplied arguments follow the expected format

  # Type

  test_16 <- function(x) {
    assertthat::is.string(x)
  }

  test_17 <- function(x) {
    x %in% c("regular", "polar")
  }

  assertthat::on_failure(test_16) <- function(call, env) {
    paste0(deparse(call$x), " is not a character string. Please supply the heatmap type as a character string. Consult package documentation for available type argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")
  }

  assertthat::on_failure(test_17) <- function(call, env) {
    paste0(deparse(call$x), " is not one of the available heatmap type options. Please consult package documentation for available type argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")
  }

  assertthat::assert_that(test_16(type))
  assertthat::assert_that(test_17(type))

  # Timeinterval

  test_19 <- function(x) {
    any(stringr::str_detect(timeinterval, c("sec", "secs", "min", "mins", "hour", "hours", "day", "days", "week", "weeks", "month", "months", "year", "years"))) &
      grepl("^[[:digit:]]\\s", timeinterval)
  }

  assertthat::on_failure(test_19) <- function(call, env) {
    paste0(deparse(call$x), " is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.")
  }

  assertthat::assert_that(test_19(timeinterval))

  # mintime and maxtime

  test_20 <- function(x) {
    x == "default" |
      !is.na(as.POSIXct(x, format = "%H:%M:%S"))
  }

  assertthat::on_failure(test_20) <- function(call, env) {
    paste0(deparse(call$x), " is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.")
  }

  assertthat::assert_that(test_20(mintime))
  assertthat::assert_that(test_20(maxtime))

  # freqinterval

  test_21_A <- function(x) {
    assertthat::is.count(x) &
      x > min(as.numeric(rownames(soundscape_obj_A@aggregated_df))) &
      x < max(as.numeric(rownames(soundscape_obj_A@aggregated_df)))
  }

  test_21_B <- function(x) {
    assertthat::is.count(x) &
      x > min(as.numeric(rownames(soundscape_obj_B@aggregated_df))) &
      x < max(as.numeric(rownames(soundscape_obj_B@aggregated_df)))
  }

  assertthat::on_failure(test_21_A) <- function(call, env) {
    paste0(deparse(call$x), " is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).")
  }

  assertthat::on_failure(test_21_B) <- function(call, env) {
    paste0(deparse(call$x), " is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).")
  }

  assertthat::assert_that(test_21_A(freqinterval))
  assertthat::assert_that(test_21_B(freqinterval))

  # minfreq and maxfreq

  test_22_A <- function(x) {
    (assertthat::is.count(x) &
      x >= min(as.numeric(rownames(soundscape_obj_A@aggregated_df))) &
      x <= max(as.numeric(rownames(soundscape_obj_A@aggregated_df)))) |
      x == 0
  }

  test_22_B <- function(x) {
    (assertthat::is.count(x) &
      x >= min(as.numeric(rownames(soundscape_obj_B@aggregated_df))) &
      x <= max(as.numeric(rownames(soundscape_obj_B@aggregated_df)))) |
      x == 0
  }

  test_23_A <- function(x) {
    (assertthat::is.count(x) &
      x >= min(as.numeric(rownames(soundscape_obj_A@aggregated_df))) &
      x <= max(as.numeric(rownames(soundscape_obj_A@aggregated_df)))) |
      x == "default"
  }

  test_23_B <- function(x) {
    (assertthat::is.count(x) &
      x >= min(as.numeric(rownames(soundscape_obj_B@aggregated_df))) &
      x <= max(as.numeric(rownames(soundscape_obj_B@aggregated_df)))) |
      x == "default"
  }

  assertthat::on_failure(test_22_A) <- function(call, env) {
    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.")
  }

  assertthat::on_failure(test_22_B) <- function(call, env) {
    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.")
  }

  assertthat::on_failure(test_23_A) <- function(call, env) {
    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.")
  }

  assertthat::on_failure(test_23_B) <- function(call, env) {
    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.")
  }

  assertthat::assert_that(test_22_A(minfreq))
  assertthat::assert_that(test_23_A(maxfreq))

  assertthat::assert_that(test_22_B(minfreq))
  assertthat::assert_that(test_23_B(maxfreq))


  # labelsize

  test_24 <- function(x) {
    (abs(x) == x) &
      x > 0 &
      length(x) == 1
  }

  assertthat::on_failure(test_24) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.")
  }

  assertthat::assert_that(test_24(labelsize_time))
  assertthat::assert_that(test_24(labelsize_frequency))
  assertthat::assert_that(test_24(labelsize_polar))

  # palette and direction

  test_25 <- function(x) {
    assertthat::is.string(x) &
      x %in% c(
        "A", "B", "C", "D", "E", "magma", "inferno",
        "plasma", "viridis", "cividis"
      )
  }

  test_26 <- function(x) {
    x %in% c(1, -1)
  }

  assertthat::on_failure(test_25) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid palette argument. Palette needs to be supplied as a character string of either: 'A', 'B', 'C', 'D', 'E', 'magma', 'inferno', 'plasma', 'viridis' or 'cividis'. Please supply a valid palette argument. Consult the soundscapeR of viridis package documentation for more information.")
  }

  assertthat::on_failure(test_26) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid direction argument. The direction argument needs to be supplied as a single integer of either 1 or -1. Please supply a valid direction argument. Consult the soundscapeR of viridis package documentation for more information.")
  }

  assertthat::assert_that(test_25(palette))
  assertthat::assert_that(test_26(direction))

  # Boolean flag arguments (zero.black, interactive)

  test_27 <- function(x) {
    assertthat::is.flag(x)
  }

  assertthat::on_failure(test_27) <- function(call, env) {
    paste0(deparse(call$x), " is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.")
  }

  assertthat::assert_that(test_27(zero.black))
  assertthat::assert_that(test_27(interactive))


  # 1. Preparing the soundscape heatmaps

  heatmap_A <- ss_heatmap(
    soundscape_obj = soundscape_obj_A,
    type = type,
    annotate = FALSE,
    timeinterval = timeinterval,
    mintime = mintime,
    maxtime = maxtime,
    freqinterval = freqinterval,
    minfreq = minfreq,
    maxfreq = maxfreq,
    labelsize_time = labelsize_time,
    labelsize_frequency = labelsize_frequency,
    labelsize_polar = labelsize_polar,
    palette = palette,
    direction = direction,
    zero.black = zero.black,
    interactive = interactive
  )

  heatmap_B <- ss_heatmap(
    soundscape_obj = soundscape_obj_B,
    type = type,
    annotate = FALSE,
    timeinterval = timeinterval,
    mintime = mintime,
    maxtime = maxtime,
    freqinterval = freqinterval,
    minfreq = minfreq,
    maxfreq = maxfreq,
    labelsize_time = labelsize_time,
    labelsize_frequency = labelsize_frequency,
    labelsize_polar = labelsize_polar,
    palette = palette,
    direction = direction,
    zero.black = zero.black,
    interactive = interactive
  )

  # 2. Preparing the differential soundscape heatmap

  # 2.1. Convert the soundscapes to matrices

  if (soundscape_obj_A@output == "raw") {
    soundscape_obj_A@aggregated_df <- soundscape_obj_A@aggregated_df / soundscape_obj_A@effort_per_time[1]
  } else {}

  if (soundscape_obj_B@output == "raw") {
    soundscape_obj_B@aggregated_df <- soundscape_obj_B@aggregated_df / soundscape_obj_B@effort_per_time[1]
  } else {}

  matrix_A <- as.matrix(soundscape_obj_A@aggregated_df)
  matrix_B <- as.matrix(soundscape_obj_B@aggregated_df)

  # 2.2. Subtract the OSU values of the matrices (B - A)

  differential_soundscape_matrix <- matrix_B - matrix_A

  # 2.3. Turn into data frame that can be plotted with ggplot

  differential_soundscape_matrix <- as.data.frame(differential_soundscape_matrix)
  differential_soundscape_matrix$frequency <- as.integer(rownames(soundscape_obj_A@aggregated_df))
  differential_soundscape_matrix <- reshape2::melt(differential_soundscape_matrix, id.vars = "frequency")
  colnames(differential_soundscape_matrix) <- c("frequency", "time", "value")

  differential_soundscape_matrix$frequency <- as.numeric(
    as.character(differential_soundscape_matrix$frequency)
  )

  differential_soundscape_matrix$time <- as.POSIXct(
    strptime(
      x = paste(substr(soundscape_obj_A@first_day, 1, 12),
        differential_soundscape_matrix$time,
        sep = " "
      ),
      format = "%Y-%m-%d %H:%M",
      tz = soundscape_obj_A@tz
    )
  )

  # 2.4. Preparing the arguments of the differential heatmap

  minfreq <- minfreq

  if (maxfreq == "default") {
    maxfreq <- max(as.numeric(differential_soundscape_matrix$frequency))
  } else {
    maxfreq <- maxfreq
  }

  if (mintime == "default") {
    mintime <- min(
      as.POSIXct(
        strptime(
          paste(substr(soundscape_obj_A@first_day, 1, 12),
            colnames(soundscape_obj_A@aggregated_df),
            sep = " "
          ),
          format = "%Y-%m-%d %H:%M:%S",
          tz = soundscape_obj_A@tz
        )
      )
    )
  } else {
    mintime <- as.POSIXct(
      strptime(
        paste(substr(soundscape_obj_A@first_day, 1, 12),
          mintime,
          sep = " "
        ),
        format = "%Y-%m-%d %H:%M:%S",
        tz = soundscape_obj_A@tz
      )
    )
  }

  if (maxtime == "default") {
    maxtime <- max(
      as.POSIXct(
        strptime(
          paste(substr(soundscape_obj_A@first_day, 1, 12),
            colnames(soundscape_obj_A@aggregated_df),
            sep = " "
          ),
          format = "%Y-%m-%d %H:%M:%S",
          tz = soundscape_obj_A@tz
        )
      )
    )
  } else {
    maxtime <- as.POSIXct(
      strptime(
        paste(substr(soundscape_obj_A@first_day, 1, 12),
          maxtime,
          sep = " "
        ),
        format = "%Y-%m-%d %H:%M:%S",
        tz = soundscape_obj_A@tz
      )
    )
  }

  day <- as.POSIXct(
    strptime(
      paste(substr(soundscape_obj_A@first_day, 1, 12),
        "00:00:00",
        sep = " "
      ),
      format = "%Y-%m-%d %H:%M:%S",
      tz = soundscape_obj_A@tz
    )
  )

  midnight1 <- as.POSIXct(
    strptime(
      paste(substr(soundscape_obj_A@first_day, 1, 12),
        "00:00:00",
        sep = " "
      ),
      format = "%Y-%m-%d %H:%M:%S",
      tz = soundscape_obj_A@tz
    )
  )

  midnight2 <- as.POSIXct(
    strptime(
      paste(substr(soundscape_obj_A@first_day, 1, 12),
        "23:55:00",
        sep = " "
      ),
      format = "%Y-%m-%d %H:%M:%S",
      tz = soundscape_obj_A@tz
    )
  )

  if (type == "regular") {
    differential_soundscape_plot <-
      ggplot2::ggplot(
        differential_soundscape_matrix,
        ggplot2::aes(
          x = time,
          y = frequency,
          fill = value,
          color = value
        )
      ) +
      ggplot2::geom_tile(ggplot2::aes(alpha = abs(value))) +
      ggplot2::scale_fill_gradientn(
        colours = c("#006c99", "white", "#a62a00"),
        values = c(0, 0.5, 1),
        breaks = seq(-1, 1, 0.5),
        labels = c("More in A", "-0.5", "Equal", "0.5", "More in B"),
        guide = ggplot2::guide_colorbar(
          title.position = "top",
          title.vjust = 1,
          title.hjust = 0.5,
          nrow = 1,
          barwidth = grid::unit(x = 7, units = "cm"),
          title = "DIFFERENCE IN OSU COMPOSITION"
        )
      ) +
      ggplot2::scale_color_gradientn(
        colours = c("#006c99", "white", "#a62a00"),
        values = c(0, 0.5, 1),
        breaks = seq(-1, 1, 0.5),
        labels = c("More in A", "-0.5", "Equal", "0.5", "More in B"),
        guide = "none"
      ) +
      ggplot2::scale_x_datetime(
        labels = scales::date_format("%H:%M", tz = soundscape_obj_A@tz),
        breaks = scales::breaks_width(timeinterval),
        expand = c(0, 0),
        limits = c(mintime, maxtime)
      ) +
      ggplot2::scale_y_continuous(
        limits = c(minfreq, maxfreq),
        expand = c(0, 0),
        breaks = seq(minfreq, maxfreq, freqinterval),
        label = scales::comma
      ) +
      ggplot2::labs(
        y = "Frequency (Hz)",
        x = "Time (hour of day)"
      ) +
      ggplot2::theme(
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(
          colour = "black"
        ),
        axis.text.y = ggplot2::element_text(
          color = "black",
          size = 10
        ),
        axis.text.x = ggplot2::element_text(
          color = "black",
          size = 10,
          angle = -45,
          vjust = 1.2,
          hjust = -0.3
        ),
        axis.title.y = ggplot2::element_text(margin = grid::unit(c(0, 3, 0, 0), "mm")),
        axis.title.x = ggplot2::element_text(margin = grid::unit(c(3, 0, 0, 0), "mm")),
        plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
        legend.position = "top",
        legend.direction = "horizontal",
        legend.title = ggplot2::element_text(
          color = "black",
          size = 12,
          face = "bold"
        )
      ) +
      ggplot2::scale_alpha(guide = "none")
  } else {
    if (type == "polar") {
      differential_soundscape_plot <-
        ggplot2::ggplot(
          differential_soundscape_matrix,
          ggplot2::aes(
            x = time,
            y = frequency,
            fill = value,
            color = value
          )
        ) +
        ggplot2::geom_tile(ggplot2::aes(alpha = abs(value))) +
        ggplot2::scale_fill_gradientn(
          colours = c("#006c99", "white", "#a62a00"),
          values = c(0, 0.5, 1),
          breaks = seq(-1, 1, 0.5),
          labels = c("More in A", "-0.5", "Equal", "0.5", "More in B"),
          guide = ggplot2::guide_colorbar(
            title.position = "top",
            title.vjust = 1,
            title.hjust = 0.5,
            nrow = 1,
            barwidth = grid::unit(x = 7, units = "cm"),
            title = "DIFFERENCE IN OSU COMPOSITION"
          )
        ) +
        ggplot2::scale_color_gradientn(
          colours = c("#006c99", "white", "#a62a00"),
          values = c(0, 0.5, 1),
          breaks = seq(-1, 1, 0.5),
          labels = c("More in A", "-0.5", "Equal", "0.5", "More in B"),
          guide = "none"
        ) +
        ggplot2::scale_x_datetime(
          labels = scales::date_format("%H:%M", tz = soundscape_obj_A@tz),
          breaks = scales::breaks_width(timeinterval),
          expand = c(0, 0),
          limits = c(mintime, maxtime)
        ) +
        ggplot2::scale_y_continuous(
          limits = c(minfreq, maxfreq),
          expand = c(0, 0),
          breaks = seq(minfreq, maxfreq, freqinterval),
          label = scales::comma
        ) +
        ggplot2::labs(
          y = "Frequency (Hz)",
          x = "Time (hour of day)"
        ) +
        ggplot2::theme(
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(
            colour = "black"
          ),
          axis.text.y = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_text(
            color = "black",
            size = 10,
            angle = -0,
            vjust = 1.2,
            hjust = -0.3
          ),
          axis.title.y = ggplot2::element_text(margin = grid::unit(c(0, 3, 0, 0), "mm")),
          axis.title.x = ggplot2::element_text(margin = grid::unit(c(3, 0, 0, 0), "mm")),
          plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
          legend.position = "top",
          legend.direction = "horizontal",
          legend.title = ggplot2::element_text(
            color = "black",
            size = 12,
            face = "bold"
          )
        ) +
        ggplot2::coord_polar() +
        ggplot2::annotate(
          geom = "label",
          size = labelsize_polar,
          y = seq(
            minfreq,
            maxfreq,
            freqinterval
          ),
          x = min(differential_soundscape_matrix$time),
          label = as.character(seq(
            minfreq,
            maxfreq,
            freqinterval
          ))
        ) +
        ggplot2::scale_alpha(guide = "none")
    } else {
      print("You supplied an invalid 'type' argument, please choose one of: 'regular' or 'polar'")
    }
  }

  if (interactive == TRUE) {
    if (interactive == TRUE & save == FALSE) {
      differential_soundscape_plot <- differential_soundscape_plot +
        ggplot2::scale_fill_gradientn(
          colours = c("#006c99", "white", "#a62a00"),
          values = c(0, 0.5, 1),
          breaks = seq(-1, 1, 0.5),
          labels = c("More in A", "-0.5", "Equal", "0.5", "More in B"),
          guide = ggplot2::guide_colorbar(
            title.position = "top",
            title.vjust = 1,
            title.hjust = 0.5,
            nrow = 1,
            barwidth = grid::unit(x = 7, units = "cm"),
            title = "OSU DIFF."
          )
        )

      if (nchar(system.file(package = "plotly")) == 0) {
        cat("The 'plotly' R-package needs to be installed before using this function \n")
        cat("Use: 'install.packages('plotly')' to install the package and try again...")
        Sys.sleep(0.00001)
        stop()
      } else {
        differential_soundscape_plot <- plotly::ggplotly(differential_soundscape_plot)
        heatmap_A <- plotly::ggplotly(heatmap_A)
        heatmap_B <- plotly::ggplotly(heatmap_B)

        total_soundscape_difference_plot <- plotly::subplot(heatmap_A, differential_soundscape_plot, heatmap_B, nrows = 1)

        total_soundscape_difference_plot <- total_soundscape_difference_plot %>%
          plotly::layout(autosize = F, width = 1800, height = 500)

        return(total_soundscape_difference_plot)
      }
    }
  } else {
    requireNamespace("patchwork")

    total_soundscape_difference_plot <- heatmap_A + differential_soundscape_plot + heatmap_B

    total_soundscape_difference_plot
  }
}

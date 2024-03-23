# Alpha, Beta and Gamma partitioning + dissimilarity measures

# Partitioning the diversity of the overal system

#' Partitioning Soundscape Diversity into Alpha, Beta and Gamma components
#'
#' @description The diversity of a system can generally be broken down into three components:
#' Alpha, Beta and Gamma diversity.
#'
#' For Hill numbers these three components take a multiplicative relationship: Gamma = Alpha x Beta
#'
#' - \bold{Alpha diversity}: The average diversity of the subsystems
#'
#' - \bold{Beta diversity}: The compositional heterogeneity between subsystems.
#'
#' The beta diversity component can be computed as: Beta = Gamma / Alpha.
#'
#' Beta represents the effective number of equally large and completely unique subsystems within the system. As Beta quantifies the ratio between Gamma and Alpha, it can also be seen as the number of times more diverse the whole system is in effective number of OSUs compared to its constituent subsystems on average. The beta diversity ranges from 1 to N (the number of subsystems in the system).
#'
#' - \bold{Gamma diversity}: The average diversity of the whole system
#'


#' @param soundscape_list A list of soundscape objects of equal dimensions, each soundscape object being produced by \code{\link{ss_create}} (or \code{\link{ss_index_merge}}, \code{\link{ss_binarize}} and \code{\link{ss_aggregate}} in sequence).
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param hier_table A matrix indicating the relationship between the soundscapes in the soundscape_list. The first column lists the names of all the soundscapes in the soundscape_list, other columns can be used to group soundscapes into higher hierarchical levels. If no hierarchy table is supplied, the function defaults to a 2-level diversity partitioning.
#' @param minfreq A numeric value indicating the lower frequency limit for which to compute the soundscape diversity. If set to default, uses the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit for which to compute the soundscape diversity. If set to default, uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the latest time for which data exists in the dataframe.
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#' @return A dataframe of diversity values
#' @export
ss_divpart <- function(soundscape_list,
                       qvalue,
                       hier_table = NULL,
                       minfreq = 0,
                       maxfreq = "default",
                       mintime = "default",
                       maxtime = "default",
                       output = "percentage") {
  # 0. Testing

  # 0.1. Check that the soundscape_list argument

  # Is it missing?

  test_1 <- function(x) {
    !missing(x)
  }

  assertthat::on_failure(test_1) <- function(call, env) {
    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")
  }

  assertthat::assert_that(test_1(soundscape_list))
  assertthat::assert_that(test_1(qvalue))

  # Is it a list?

  test_2 <- function(x) {
    is.list(x)
  }

  assertthat::on_failure(test_2) <- function(call, env) {
    paste0(deparse(call$x), " argument is not a list Please supply a list of aggregated soundscape objects.")
  }

  assertthat::assert_that(test_2(soundscape_list))


  # Are all elements of the list S4-object of the type 'soundscape' and not empty.

  test_3 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          isS4(y) &
            assertthat::are_equal(class(y)[1], "soundscape") &
            assertthat::not_empty(y)
        }
      ) == TRUE
    )
  }

  assertthat::on_failure(test_3) <- function(call, env) {
    paste0(deparse(call$x), " is not a list of S4-objects of the type 'soundscape'. The list may contain different objects or empty elements. Please supply a list of soundscape objects produced by the ss_aggregate() function. Consult the package documentation for further information.")
  }

  assertthat::assert_that(test_3(soundscape_list))

  # 0.2. Check if all the components of the soundscape objects in the list follow the expected format

  # 0.1.3. The soundscape elements are in the expected format

  # The lat and lon argument

  test_4 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          is.numeric(y@lat) &
            y@lat >= -90 &
            y@lat <= 90
        }
      ) == TRUE
    )
  }

  test_5 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          is.numeric(y@lon) &
            y@lon >= -180 &
            y@lon <= 180
        }
      ) == TRUE
    )
  }

  assertthat::on_failure(test_4) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid latitude coordinates. Did you supply the a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")
  }

  assertthat::on_failure(test_5) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid longitude coordinate. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")
  }

  assertthat::assert_that(test_4(soundscape_list))
  assertthat::assert_that(test_5(soundscape_list))

  # The time zone argument

  test_6 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          assertthat::is.string(y@tz) &
            (y@tz %in% (OlsonNames()))
        }
      ) == TRUE
    )
  }

  assertthat::on_failure(test_6) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid timezone objects Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")
  }

  assertthat::assert_that(test_6(soundscape_list))

  # The index argument

  test_7 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          assertthat::is.string(y@index) &
            (y@index %in% c(
              "BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
              "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"
            ))
        }
      ) == TRUE
    )
  }

  assertthat::on_failure(test_7) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid index objects. Make sure the index argument inside the soundscape object is one of the available index options. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")
  }

  assertthat::assert_that(test_7(soundscape_list))

  # The samplerate and window arguments

  test_8 <- function(x) {
    all(
      sapply(
        x,
        function(y) assertthat::is.count(y@samplerate)
      ) == TRUE
    )
  }

  test_9 <- function(x) {
    all(
      sapply(
        x,
        function(y) assertthat::is.count(y@window)
      ) == TRUE
    )
  }

  assertthat::on_failure(test_8) <- function(call, env) {
    paste0(deparse(call$x), " contains samplerate arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.")
  }

  assertthat::on_failure(test_9) <- function(call, env) {
    paste0(deparse(call$x), " contains window arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.")
  }

  assertthat::assert_that(test_8(soundscape_list))
  assertthat::assert_that(test_9(soundscape_list))

  # The binarization_method argument

  test_10 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          assertthat::is.string(y@binarization_method) &
            (y@binarization_method %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li", "MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu", "Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen", "Mode", "custom"))
        }
      ) == TRUE
    )
  }



  assertthat::on_failure(test_10) <- function(call, env) {
    paste0(deparse(call$x), " contains binarization_method arguments that are not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")
  }

  assertthat::assert_that(test_10(soundscape_list))

  # The threshold argument

  test_11 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          all(length(y@threshold) == 1 &
                is.double(y@threshold) & !is.na(y@threshold))
        }
      ) == TRUE
    )
  }

  assertthat::on_failure(test_11) <- function(call, env) {
    paste0(deparse(call$x), " contains threshold arguments that are not a single numeric value. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the value argument is you're supplying a custom threshold value.")
  }

  assertthat::assert_that(test_11(soundscape_list))

  # The output argument

  test_12 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          all(length(y@output) == 1 &
                is.character(y@output) &
                (y@output %in% c("incidence_freq", "raw")))
        }
      ) == TRUE
    )
  }

  assertthat::on_failure(test_12) <- function(call, env) {
    paste0(deparse(call$x), " contains output arguments that are not a character string describing one of the available output options. Did you supply a list of soundscapes produced using the ss_aggregate function? If so, something has gone wrong, please re-run the ss_aggregate() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.")
  }

  assertthat::assert_that(test_12(soundscape_list))

  # The merged_df argument

  test_13 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          is.data.frame(y@merged_df) &
            assertthat::not_empty(y@merged_df) &
            assertthat::noNA(y@merged_df) &
            all(apply(y@merged_df, 2, function(y) all(is.numeric(y))))
        }
      ) == TRUE
    )
  }

  test_14 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          (abs(as.numeric(rownames(y@merged_df)[1])) +
             abs(as.numeric(rownames(y@merged_df)[2]))) > 3 &
            min(as.numeric(rownames(y@merged_df))) >= 0 &
            max(as.numeric(rownames(y@merged_df))) <= y@samplerate / 2
        }
      ) == TRUE
    )
  }

  test_15 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          !any(sapply(try(
            as.POSIXct(
              paste0(substr(y@first_day, 1, 12), " ", colnames(y@merged_df)),
              tz = y@tz,
              format = "%Y-%m-%d %H:%M:%S"
            ),
            silent = TRUE
          ), function(z) is.na(z)))
        }
      ) == TRUE
    )
  }


  assertthat::on_failure(test_13) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.")
  }

  assertthat::on_failure(test_14) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.")
  }


  assertthat::on_failure(test_15) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.")
  }


  assertthat::assert_that(test_13(soundscape_list))
  assertthat::assert_that(test_14(soundscape_list))
  assertthat::assert_that(test_15(soundscape_list))

  # The binarized_df argument

  test_13_1 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          is.data.frame(y@binarized_df) &
            assertthat::not_empty(y@binarized_df) &
            assertthat::noNA(y@binarized_df) &
            all(apply(y@binarized_df, 2, function(y) all(is.numeric(y))))
        }
      ) == TRUE
    )
  }

  test_14_1 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          (abs(as.numeric(rownames(y@binarized_df)[1])) +
             abs(as.numeric(rownames(y@binarized_df)[2]))) > 3 &
            min(as.numeric(rownames(y@binarized_df))) >= 0 &
            max(as.numeric(rownames(y@binarized_df))) <= y@samplerate / 2
        }
      ) == TRUE
    )
  }

  test_15_1 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          !any(sapply(try(
            as.POSIXct(
              paste0(substr(y@first_day, 1, 12), " ", colnames(y@binarized_df)),
              tz = y@tz,
              format = "%Y-%m-%d %H:%M:%S"
            ),
            silent = TRUE
          ), function(z) is.na(z)))
        }
      ) == TRUE
    )
  }

  test_16 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          min(y@binarized_df) >= 0 &
            max(y@binarized_df) <= 1
        }
      ) == TRUE
    )
  }

  assertthat::on_failure(test_13_1) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.")
  }

  assertthat::on_failure(test_14_1) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.")
  }


  assertthat::on_failure(test_15_1) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.")
  }

  assertthat::on_failure(test_16) <- function(call, env) {
    paste0(deparse(call$x), " contains binarized_df dataframes with values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() function.")
  }

  assertthat::assert_that(test_13_1(soundscape_list))
  assertthat::assert_that(test_14_1(soundscape_list))
  assertthat::assert_that(test_15_1(soundscape_list))
  assertthat::assert_that(test_16(soundscape_list))

  # The aggregated_df argument

  test_13_2 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          is.data.frame(y@aggregated_df) &
            assertthat::not_empty(y@aggregated_df) &
            assertthat::noNA(y@aggregated_df) &
            all(apply(y@aggregated_df, 2, function(y) all(is.numeric(y))))
        }
      ) == TRUE
    )
  }

  test_14_2 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          (abs(as.numeric(rownames(y@aggregated_df)[1])) +
             abs(as.numeric(rownames(y@aggregated_df)[2]))) > 3 &
            min(as.numeric(rownames(y@aggregated_df))) >= 0 &
            max(as.numeric(rownames(y@aggregated_df))) <= y@samplerate / 2
        }
      ) == TRUE
    )
  }

  test_15_2 <- function(x) {
    all(
      sapply(
        x,
        function(y) {
          !any(sapply(try(
            as.POSIXct(
              paste0(substr(y@first_day, 1, 12), " ", colnames(y@aggregated_df)),
              tz = y@tz,
              format = "%Y-%m-%d %H:%M:%S"
            ),
            silent = TRUE
          ), function(z) is.na(z)))
        }
      ) == TRUE
    )
  }

  assertthat::on_failure(test_13_2) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.")
  }

  assertthat::on_failure(test_14_2) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.")
  }


  assertthat::on_failure(test_15_2) <- function(call, env) {
    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.")
  }

  assertthat::assert_that(test_13_2(soundscape_list))
  assertthat::assert_that(test_14_2(soundscape_list))
  assertthat::assert_that(test_15_2(soundscape_list))

  # 0.1.4. Check if the other supplied arguments follow the expected format

  # the supplied mintime and maxtime arguments are one of the available options

  test_16 <- function(x) {
    is.character(x) & (

      x == "default" |
        !is.na(as.POSIXct(x, format = "%H:%M:%S")))
  }

  assertthat::on_failure(test_16) <- function(call, env) {
    paste0(deparse(call$x), " is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.")
  }

  assertthat::assert_that(test_16(mintime))
  assertthat::assert_that(test_16(maxtime))

  # The minfreq and maxfreq arguments follow the expected values

  test_17 <- function(x) {
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(soundscape_list[[1]]@aggregated_df))) &
       x <= max(as.numeric(rownames(soundscape_list[[1]]@aggregated_df)))) |
      x == 0
  }

  test_18 <- function(x) {
    (assertthat::is.count(x) &
       x >= min(as.numeric(rownames(soundscape_list[[1]]@aggregated_df))) &
       x <= max(as.numeric(rownames(soundscape_list[[1]]@aggregated_df)))) |
      x == "default"
  }

  assertthat::on_failure(test_17) <- function(call, env) {
    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.")
  }

  assertthat::on_failure(test_18) <- function(call, env) {
    paste0(deparse(call$x), " is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.")
  }

  assertthat::assert_that(test_17(minfreq))
  assertthat::assert_that(test_18(maxfreq))

  # The output argument follows expected format

  test_19 <- function(x) {
    assertthat::is.string(x)
  }

  test_20 <- function(x) {
    x %in% c("percentage", "raw")
  }

  assertthat::on_failure(test_19) <- function(call, env) {
    paste0(deparse(call$x), " is not a character string. Please supply the output argument as a character string. Consult package documentation for available output argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")
  }

  assertthat::on_failure(test_20) <- function(call, env) {
    paste0(deparse(call$x), " is not one of the available ss_diversity output options. Please consult package documentation for available output argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")
  }

  assertthat::assert_that(test_19(output))
  assertthat::assert_that(test_20(output))

  # the qvalue argument follows the expected format

  test_21 <- function(x) {
    !is.character(x)
  }

  test_22 <- function(x) {
    !is.list(x)
  }

  test_23 <- function(x) {
    is.numeric(x)
  }

  test_24 <- function(x) {
    abs(x) == x
  }

  assertthat::on_failure(test_21) <- function(call, env) {
    paste0(deparse(call$x), " is a character string of length 1. Please supply the qvalue argument as a positive numeric or integer value.")
  }

  assertthat::on_failure(test_22) <- function(call, env) {
    paste0(deparse(call$x), " is a list. Please supply the qvalue argument as a positive numeric or integer value.")
  }

  assertthat::on_failure(test_23) <- function(call, env) {
    paste0(deparse(call$x), " is not an numeric/integer value. Please supply the qvalue argument as a positive numeric or integer value.")
  }

  assertthat::on_failure(test_24) <- function(call, env) {
    paste0(deparse(call$x), " is not a positive value. Please supply the qvalue argument as a positive numeric or integer value.")
  }

  assertthat::assert_that(test_21(qvalue))
  assertthat::assert_that(test_22(qvalue))
  assertthat::assert_that(test_23(qvalue))
  assertthat::assert_that(test_24(qvalue))


  ## At some point, test that all the soundscapes have the same dimensions

  if (qvalue < 0) stop("q value needs to be positive (equal or higher than zero)")
  if (qvalue == 1) {
    qvalue <- 0.99999
  }


  ## Check that, if subsetting, the soundscapes all have the same day and night length

  # 1. Preparing the subsetting arguments

  # Frequency subsetting

  minfreq <- minfreq

  if (maxfreq == "default") {
    maxfreq <- max(as.numeric(rownames(soundscape_list[[1]]@aggregated_df)))
  } else {
    maxfreq <- maxfreq
  }

  if (mintime == "default") {
    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)) {
      mintime_list[[i]] <- min(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
              colnames(soundscape_list[[i]]@aggregated_df),
              sep = " "
            ),
            format = "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz
          )
        )
      )
    }
  } else {
    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)) {
      mintime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
            mintime,
            sep = " "
          ),
          format = "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz
        )
      )
    }
  }


  if (maxtime == "default") {
    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)) {
      maxtime_list[[i]] <- max(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
              colnames(soundscape_list[[i]]@aggregated_df),
              sep = " "
            ),
            format = "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz
          )
        )
      )
    }
  } else {
    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)) {
      maxtime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
            maxtime,
            sep = " "
          ),
          format = "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz
        )
      )
    }
  }

  # 2. Subsetting the soundscapes based on the time and freq arguments

  # Subsetting vectors

  rownames_df <- vector("list", length(soundscape_list))
  rownames_subset <- vector("list", length(soundscape_list))
  colnames_df <- vector("list", length(soundscape_list))
  colnames_subset <- vector("list", length(soundscape_list))
  new_soundscape_list <- vector("list", length(soundscape_list))

  for (i in 1:length(soundscape_list)) {
    rownames_df[[i]] <- as.numeric(rownames(soundscape_list[[i]]@aggregated_df))

    rownames_subset[[i]] <- as.character(subset(
      rownames_df[[i]],
      rownames_df[[i]] >= minfreq &
        rownames_df[[i]] <= maxfreq
    ))

    colnames_df[[i]] <- as.POSIXct(strptime(
      paste(soundscape_list[[i]]@first_day,
        colnames(soundscape_list[[i]]@aggregated_df),
        sep = " "
      ),
      format = "%Y-%m-%d %H:%M:%S",
      tz = soundscape_list[[i]]@tz
    ))

    colnames_subset[[i]] <- as.character(hms::as_hms(subset(
      colnames_df[[i]],
      colnames_df[[i]]
      >= mintime_list[[i]] &
        colnames_df[[i]]
        <= maxtime_list[[i]]
    )))

    new_soundscape_list[[i]] <- soundscape_list[[i]]@aggregated_df[rownames_subset[[i]], colnames_subset[[i]]]
  }

  # 2. Prepare the soundscape list to be in the correct format

  # 2.1. Create site-by-osu df

  site_by_OSU_matrix <-
    as.data.frame(
      t(do.call(
        rbind,
        lapply(
          new_soundscape_list,
          function(x) unlist(x)
        )
      ))
    )

  rownames(site_by_OSU_matrix) <- paste0(
    "OSU_",
    seq(1, nrow(site_by_OSU_matrix), 1)
  )

  colnames(site_by_OSU_matrix) <- as.character(names(soundscape_list))

  # 3. Partition the soundscape diversity into its gamma, alpha and beta components

  # 3.1. Helper function

  hill_part <- function(OSU_matrix, qvalue, hierarchy_table) {
    if (sum(colSums(OSU_matrix)) != ncol(OSU_matrix)) {
      if (is.null(dim(OSU_matrix))) {
        OSU_matrix <- OSU_matrix / sum(OSU_matrix)
      } else {
        OSU_matrix <- sweep(OSU_matrix, 2, colSums(OSU_matrix), FUN = "/")
      }
    }

    # 0. Helper functions

    alpha <- function(OSU_matrix, qvalue) {
      weight <- rep(1 / ncol(OSU_matrix), ncol(OSU_matrix))
      pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(x) !all(x == 0)), ])
      pi_w <- sweep(pi, 2, weight, "*")
      pi_w_q <- pi_w^qvalue
      pi_w_q[!pi] <- 0
      N <- length(weight)
      alpha_div <- sum(rowSums(pi_w_q))^(1 / (1 - qvalue)) / N
      return(alpha_div)
    }

    gamma <- function(OSU_matrix, qvalue) {
      weight <- rep(1 / ncol(OSU_matrix), ncol(OSU_matrix))
      pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(z) !all(z == 0)), ])
      pi_w <- sweep(pi, 2, weight, "*")
      gamma_div <- sum(rowSums(pi_w)^qvalue)^(1 / (1 - qvalue))
      return(gamma_div)
    }

    is_nested <- function(hierarchy_table) {
      if (is.null(dim(hierarchy_table)) == TRUE) stop("The hierarchy_table object is not a two-dimensional table.")
      leveln <- ncol(hierarchy_table)
      logic_vector <- c()
      if (leveln == 2) {
        return(TRUE)
      }
      if (leveln > 2) {
        for (i in c((leveln - 1):2)) {
          levelonly <- length(unique(hierarchy_table[, i]))
          levelparent <- nrow(unique(hierarchy_table[, c(i, i + 1)]))
          logic <- levelonly == levelparent
          logic_vector <- c(logic_vector, logic)
        }
        return(all(logic_vector))
      }
    }

    # 1. If no hierarchy_table table was supplied


    if (missing(hierarchy_table)) {
      level_1 <- alpha(
        OSU_matrix = OSU_matrix,
        qvalue = qvalue
      )

      level_2 <- gamma(
        OSU_matrix = OSU_matrix,
        qvalue = qvalue
      )

      div_vector <- c(level_1, level_2)
      names(div_vector) <- c("Level_1", "Level_2")

      # Beta

      beta <- level_2 / level_1

      # Sample size

      N <- c(N1 = ncol(OSU_matrix), N2 = 1)

      # Return values

      if (qvalue == 0.99999) {
        qvalue <- 1
      }

      results <- list("Hierarchical_levels" = 2, "Order_diversity" = qvalue, "Hill_numbers" = div_vector, "Sample_size" = N, "Beta" = beta)

      return(results)
    } else {
      # 2. If a hierarchy_table was supplied

      if (!missing(hierarchy_table)) {
        # Check nestedness of hierarchy_table
        if (is_nested(hierarchy_table) == FALSE) stop("The groups in the hierarchy table are not nested.")


        # Count number of levels

        leveln <- ncol(hierarchy_table)
        levels <- paste(rep("Level_", leveln + 1), seq(1:(leveln + 1)), sep = "")

        # Convert hierarchy columns to character
        hierarchy_table <- apply(hierarchy_table, MARGIN = c(1, 2), as.character)
        colnames(hierarchy_table) <- levels[-length(levels)]

        # Generate aggregated OTU tables

        OSU_tables_2 <- list()
        OSU_tables_2[[1]] <- OSU_matrix
        OSU_matrix_sub <- OSU_matrix
        for (i in c(2:leveln)) {
          OSU_matrix_sub <- merge(t(OSU_matrix_sub), unique(hierarchy_table[, c(i - 1, i)]), by.x = "row.names", by.y = as.character(levels[i - 1]))
          OSU_matrix_sub <- OSU_matrix_sub[, -1]
          OSU_matrix_sub <- stats::aggregate(subset(OSU_matrix_sub, select = rownames(OSU_matrix)), by = list(OSU_matrix_sub[, as.character(levels[i])]), FUN = sum)
          rownames(OSU_matrix_sub) <- OSU_matrix_sub[, 1]
          OSU_matrix_sub <- t(OSU_matrix_sub[, -1])
          OSU_tables_2[[i]] <- OSU_matrix_sub
        }

        # Generate vector of diversities at different hierarchical levels

        # Generate vector of diversities at different hierarchical levels
        div_vector <- c()
        for (i in c(1:(leveln + 1))) {
          if (i == 1) {
            # If lowest level
            div_vector <- c(div_vector, alpha(OSU_tables_2[[1]], qvalue))
          } else if (i == leveln + 1) {
            # If highest level

            div_vector <- c(div_vector, gamma(OSU_tables_2[[1]], qvalue))
          } else {
            # Intermediate level
            div_vector <- c(div_vector, alpha(OSU_tables_2[[i]], qvalue))
          }
        }


        # Name levels
        names(div_vector) <- levels


        # Get beta values
        beta_vector <- c()
        N_vector <- c()
        for (b in c(1:(leveln))) {
          beta <- div_vector[b + 1] / div_vector[b]
          names(beta) <- paste("B", paste(b, b + 1, sep = "_"), sep = "")
          beta_vector <- c(beta_vector, beta)
          N <- ncol(OSU_tables_2[[b]])
          N_vector <- c(N_vector, N)
        }

        N_vector <- c(N_vector, 1)
        names(N_vector) <- paste(rep("N", leveln + 1), seq(1:(leveln + 1)), sep = "")

        # Return values
        if (qvalue == 0.99999) {
          qvalue <- 1
        }
        results <- list("Hierarchical_levels" = (leveln + 1), "Order_diversity" = qvalue, "Hill_numbers" = div_vector, "Sample_size" = N_vector, "Beta" = beta_vector)
        return(results)
      }
    }
  }

  # Diversity partitioning


  if (is.null(hier_table)) {
    soundscape_part <- hill_part(
      OSU_matrix = site_by_OSU_matrix,
      qvalue = qvalue
    )
  } else {
    soundscape_part <- hill_part(
      OSU_matrix = site_by_OSU_matrix,
      qvalue = qvalue,
      hierarchy_table = hier_table
    )
  }

  soundscape_part_table <- dplyr::bind_rows(unlist(soundscape_part))

  colnames(soundscape_part_table) <- c(
    "levels",
    "q",
    paste0(
      "alpha_l",
      seq(1, (length(soundscape_part$Hill_numbers) - 1), 1)
    ),
    "gamma",
    paste0(
      "N",
      seq(1, length(soundscape_part$Sample_size), 1)
    ),
    paste0(
      "beta_l",
      seq(1, (length(soundscape_part$Hill_numbers) - 1), 1)
    )
  )


  if (output == "percentage") {
    soundscape_part_table[3:(3 + length(soundscape_part$Hill_numbers) - 1)] <- (soundscape_part_table[3:(3 + length(soundscape_part$Hill_numbers) - 1)] / nrow(site_by_OSU_matrix)) * 100
  } else {}

  return(soundscape_part_table)
}


#########################################################################################################


# Getting the pairwise dissimilarities between groups in the system

#' Pairwise Beta diversity and dissimilarity values between subsystems in the system
#'
#' @description Computation of pairwise dissimularities among soundscapes.
#'
#' Regular Beta diversity varies between 1-N, its value being dependent on the number of subsystems under consideration (N). As such, due to its dependence on the number of subsystems, it cannot directly be used as a measure of dissimilarity among communities. Instead, several simple transformations can be performed on the Beta values to get dissimilarity indices ranging between 0-1.
#'
#'
#' - Sorensen-subset (local) overlap:
#'
#' Quantifies the average proportion of a sub-system's OSUs which are shared across all considered sub-systems. It quantifies the overlap (similarity) from the sub-system perspective. To make this into a dissimilarity metric, we take the one-complement (1-Sorensen overlap), being the average proportion of non-shared OSUs in the system.
#'
#' - Jaccard-subset (regional) overlap:
#'
#' Quantifies the effective proportion of OSUs which are shared across all subsystems. It quantifies overlap (similarity) from the perspective of the overall system. To make this into a dissimilarity metric, we take the one-complement (1 - Jaccard overlap), being the effective proportion of non-shard OSUs in the whole system.
#'
#' - Sorensen-subset turnover:
#'
#' Quantifies the normalized turnover rate of OSUs from the perspective of the subsystem (alpha) - or the proportion of a subsystem which changes across subsystems. Once again, we take the one-complement as a dissimilarity measure.
#'
#' - Jaccard-subset turnover:
#'
#' Quantifies the normalized OSU turnover rate from the perspective of the whole system (gamma). Once more, the one-complement gives us our dissimilarity measure.
#'
#'
#'
#' @param soundscape_list A list of dataframes of equal dimensions, each dataframe being produced by \code{\link{ss_aggregate}}.
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param hier_table A matrix indicating the relationship between the soundscapes in the soundscape_list. The first column lists the names of all the soundscapes in the soundscape_list, other columns can be used to group soundscapes into higher hierarchical levels. If no hierarchy table is supplied, the function defaults to a 2-level diversity partitioning.
#' @param minfreq A numeric value indicating the lower frequency limit for which to compute the soundscape diversity. If set to default, uses the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit for which to compute the soundscape diversity. If set to default, uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the latest time for which data exists in the dataframe.
#' @return A list of pairwise matrices for the beta diversity, and the one-complement for the Sorensen-subset overlap, Jaccard-subset overlap, Sorensen-subset turnover and Jaccard-subset turnover.
#' @export

ss_pairdis <- function(soundscape_list,
                       qvalue = 0,
                       hier_table = NULL,
                       minfreq = 0,
                       maxfreq = "default",
                       mintime = "default",
                       maxtime = "default") {
  # 0. Testing

  ## At some point, test that all the soundscapes have the same dimensions


  ## Check that, if subsetting, the soundscapes all have the same day and night length

  # 0. Helper functions

  pairwise_dis <- function(OSU_matrix, qvalue, hier_table, metric) {
    if (sum(colSums(OSU_matrix)) != ncol(OSU_matrix)) {
      if (is.null(dim(OSU_matrix))) {
        OSU_matrix <- OSU_matrix / sum(OSU_matrix)
      } else {
        OSU_matrix <- sweep(OSU_matrix, 2, colSums(OSU_matrix), FUN = "/")
      }
    }

    if (missing(metric)) {
      metric <- c("C", "U", "V", "S")
    }

    # Helper functions

    beta_dissimilarity <- function(beta, qvalue, N, metric, type) {
      # Helper functions

      alpha <- function(OSU_matrix, qvalue) {
        weight <- rep(1 / ncol(OSU_matrix), ncol(OSU_matrix))
        pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(x) !all(x == 0)), ])
        pi_w <- sweep(pi, 2, weight, "*")
        pi_w_q <- pi_w^qvalue
        pi_w_q[!pi] <- 0
        N <- length(weight)
        alpha_div <- sum(rowSums(pi_w_q))^(1 / (1 - qvalue)) / N
        return(alpha_div)
      }

      gamma <- function(OSU_matrix, qvalue) {
        weight <- rep(1 / ncol(OSU_matrix), ncol(OSU_matrix))
        pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(z) !all(z == 0)), ])
        pi_w <- sweep(pi, 2, weight, "*")
        gamma_div <- sum(rowSums(pi_w)^qvalue)^(1 / (1 - qvalue))
        return(gamma_div)
      }

      CqN <- function(beta, qvalue, N) {
        if (qvalue == 1) {
          qvalue <- 0.99999
        }
        value <- ((1 / beta)^(qvalue - 1) - (1 / N)^(qvalue - 1)) / (1 - (1 / N)^(qvalue - 1))
        return(value)
      }

      SqN <- function(beta, N) {
        value <- ((1 / beta) - 1 / N) / (1 - 1 / N)
        return(value)
      }

      UqN <- function(beta, qvalue, N) {
        if (qvalue == 1) {
          qvalue <- 0.99999
        }
        value <- ((1 / beta)^(1 - qvalue) - (1 / N)^(1 - qvalue)) / (1 - (1 / N)^(1 - qvalue))
        return(value)
      }

      VqN <- function(beta, N) {
        value <- (N - beta) / (N - 1)
        return(value)
      }

      # Quality-check and warnings

      if (is.numeric(beta)) {
        betan <- length(beta)
        betas <- beta
        Ns <- N
      }

      if (is.list(beta)) {
        betan <- length(beta$Beta)
        qvalue <- beta$Order_diversity
        betas <- beta$Beta
        Ns <- beta$Sample_size[-length(beta$Sample_size)]
      }

      ###### MULTIPLE HIERARCHIES NEED TO BE ADDED, and similarity functions updated
      results <- list()

      # Sørensen-type overlap (CqN, 1-CqN)
      if ("C" %in% metric) {
        CqNs <- c()
        for (i in c(1:betan)) {
          CqNs <- c(CqNs, CqN(betas[i], qvalue, Ns[i]))
          names(CqNs)[i] <- paste("L", paste(i, i + 1, sep = "_"), sep = "")
        }
        if (type == "dissimilarity") {
          rCqNs <- 1 - CqNs
          results <- append(results, list(CqN = rCqNs))
        } else {
          results <- append(results, list(CqN = CqNs))
        }
      }

      # Jaccard-type overlap (UqN, 1-UqN)
      if ("U" %in% metric) {
        UqNs <- c()
        for (i in c(1:betan)) {
          UqNs <- c(UqNs, UqN(betas[i], qvalue, Ns[i]))
          names(UqNs)[i] <- paste("L", paste(i, i + 1, sep = "_"), sep = "")
        }
        if (type == "dissimilarity") {
          rUqNs <- 1 - UqNs
          results <- append(results, list(UqN = rUqNs))
        } else {
          results <- append(results, list(UqN = UqNs))
        }
      }

      # Sørensen-type turnover-complement (VqN, 1-VqN)
      if ("V" %in% metric) {
        VqNs <- c()
        for (i in c(1:betan)) {
          VqNs <- c(VqNs, VqN(betas[i], Ns[i]))
          names(VqNs)[i] <- paste("L", paste(i, i + 1, sep = "_"), sep = "")
        }
        if (type == "dissimilarity") {
          rVqNs <- 1 - VqNs
          results <- append(results, list(VqN = rVqNs))
        } else {
          results <- append(results, list(VqN = VqNs))
        }
      }

      # Jaccard-type turnover-complement (SqN, 1-SqN)
      if ("S" %in% metric) {
        SqNs <- c()
        for (i in c(1:betan)) {
          SqNs <- c(SqNs, SqN(betas[i], Ns[i]))
          names(SqNs)[i] <- paste("L", paste(i, i + 1, sep = "_"), sep = "")
        }
        if (type == "dissimilarity") {
          rSqNs <- 1 - SqNs
          results <- append(results, list(SqN = rSqNs))
        } else {
          results <- append(results, list(SqN = SqNs))
        }
      }

      return(results)
    }


    # Count number of levels
    if (!missing(hier_table)) {
      leveln <- ncol(hier_table)
    } else {
      leveln <- 1
    }
    levels <- paste("Level_", seq(1, leveln, 1), sep = "")
    if (!missing(hier_table)) {
      colnames(hier_table) <- levels
    }

    # Generate aggregated OTU tables
    OSU_tables_2 <- list()
    OSU_tables_2[[1]] <- OSU_matrix
    if (leveln > 1) {
      OSU_matrix_sub <- OSU_matrix
      for (i in c(2:leveln)) {
        OSU_matrix_sub <- merge(t(OSU_matrix_sub), unique(hier_table[, c(i - 1, i)]), by.x = "row.names", by.y = as.character(levels[i - 1]))
        OSU_matrix_sub <- OSU_matrix_sub[, -1]
        OSU_matrix_sub <- stats::aggregate(subset(OSU_matrix_sub, select = rownames(OSU_matrix)), by = list(OSU_matrix_sub[, as.character(levels[i])]), FUN = mean)
        rownames(OSU_matrix_sub) <- OSU_matrix_sub[, 1]
        OSU_matrix_sub <- t(OSU_matrix_sub[, -1])
        OSU_tables_2[[i]] <- OSU_matrix_sub
      }
    }

    # Generate results
    results <- list()
    names <- c()
    for (i in c(1:leveln)) {
      # Generate matrices
      OSU_table_sub <- OSU_tables_2[[i]]
      indices <- sort(colnames(OSU_table_sub))

      beta_matrix <- matrix(rep(NA, length(indices)^2), nrow = length(indices), ncol = length(indices))
      colnames(beta_matrix) <- indices
      rownames(beta_matrix) <- indices
      if ("C" %in% metric) {
        CqN_matrix <- beta_matrix
      }
      if ("U" %in% metric) {
        UqN_matrix <- beta_matrix
      }
      if ("V" %in% metric) {
        VqN_matrix <- beta_matrix
      }
      if ("S" %in% metric) {
        SqN_matrix <- beta_matrix
      }


      # Populate matrices
      for (x in indices) {
        for (y in indices) {
          if (is.na(beta_matrix[x, y])) { # to avoid repeating mirror operations
            combination <- OSU_table_sub[, c(y, x)]

            if (identical(x, y) == TRUE) {
              beta <- NA
            } else {
              alpha <- function(OSU_matrix, qvalue) {
                weight <- rep(1 / ncol(OSU_matrix), ncol(OSU_matrix))
                pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(x) !all(x == 0)), ])
                pi_w <- sweep(pi, 2, weight, "*")
                pi_w_q <- pi_w^qvalue
                pi_w_q[!pi] <- 0
                N <- length(weight)
                alpha_div <- sum(rowSums(pi_w_q))^(1 / (1 - qvalue)) / N
                return(alpha_div)
              }

              gamma <- function(OSU_matrix, qvalue) {
                weight <- rep(1 / ncol(OSU_matrix), ncol(OSU_matrix))
                pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(z) !all(z == 0)), ])
                pi_w <- sweep(pi, 2, weight, "*")
                gamma_div <- sum(rowSums(pi_w)^qvalue)^(1 / (1 - qvalue))
                return(gamma_div)
              }


              alpha <- alpha(OSU_matrix = combination, qvalue = qvalue)
              gamma <- gamma(OSU_matrix = combination, qvalue = qvalue)
              beta <- gamma / alpha
              beta_matrix[y, x] <- beta

              if ("C" %in% metric) {
                CqN_matrix[y, x] <- beta_dissimilarity(beta = beta, qvalue = qvalue, N = 2, metric = "C", type = "dissimilarity")$CqN
              }

              if ("U" %in% metric) {
                UqN_matrix[y, x] <- beta_dissimilarity(beta = beta, qvalue = qvalue, N = 2, metric = "U", type = "dissimilarity")$UqN
              }

              if ("V" %in% metric) {
                VqN_matrix[y, x] <- beta_dissimilarity(beta = beta, qvalue = qvalue, N = 2, metric = "V", type = "dissimilarity")$VqN
              }

              if ("S" %in% metric) {
                SqN_matrix[y, x] <- beta_dissimilarity(beta = beta, qvalue = qvalue, N = 2, metric = "S", type = "dissimilarity")$SqN
              }
            }
          }
        }
      }

      # Append matrices to results
      results <- append(results, list(Beta = beta_matrix))
      if ("C" %in% metric) {
        results <- append(results, list(CqN = CqN_matrix))
      }
      if ("U" %in% metric) {
        results <- append(results, list(UqN = UqN_matrix))
      }
      if ("V" %in% metric) {
        results <- append(results, list(VqN = VqN_matrix))
      }
      if ("S" %in% metric) {
        results <- append(results, list(SqN = SqN_matrix))
      }

      # Append matrix names
      names <- c(names, paste(paste("Level_", i, sep = ""), "beta", sep = "_"))
      if ("C" %in% metric) {
        names <- c(names, paste(paste("L", i, sep = ""), "CqN", sep = "_"))
      }
      if ("U" %in% metric) {
        names <- c(names, paste(paste("L", i, sep = ""), "UqN", sep = "_"))
      }
      if ("V" %in% metric) {
        names <- c(names, paste(paste("L", i, sep = ""), "VqN", sep = "_"))
      }
      if ("S" %in% metric) {
        names <- c(names, paste(paste("L", i, sep = ""), "SqN", sep = "_"))
      }
    }

    # Modify names
    names(results) <- names
    return(results)
  }

  # 1. Preparing the subsetting arguments

  # Frequency subsetting

  minfreq <- minfreq

  if (maxfreq == "default") {
    maxfreq <- max(as.numeric(rownames(soundscape_list[[1]]@aggregated_df)))
  } else {
    maxfreq <- maxfreq
  }

  if (mintime == "default") {
    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)) {
      mintime_list[[i]] <- min(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
              colnames(soundscape_list[[i]]@aggregated_df),
              sep = " "
            ),
            format = "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz
          )
        )
      )
    }
  } else {
    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)) {
      mintime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
            mintime,
            sep = " "
          ),
          format = "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz
        )
      )
    }
  }


  if (maxtime == "default") {
    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)) {
      maxtime_list[[i]] <- max(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
              colnames(soundscape_list[[i]]@aggregated_df),
              sep = " "
            ),
            format = "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz
          )
        )
      )
    }
  } else {
    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)) {
      maxtime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
            maxtime,
            sep = " "
          ),
          format = "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz
        )
      )
    }
  }

  # 2. Subsetting the soundscapes based on the time and freq arguments

  # Subsetting vectors

  rownames_df <- vector("list", length(soundscape_list))
  rownames_subset <- vector("list", length(soundscape_list))
  colnames_df <- vector("list", length(soundscape_list))
  colnames_subset <- vector("list", length(soundscape_list))
  new_soundscape_list <- vector("list", length(soundscape_list))

  for (i in 1:length(soundscape_list)) {
    rownames_df[[i]] <- as.numeric(rownames(soundscape_list[[i]]@aggregated_df))

    rownames_subset[[i]] <- as.character(subset(
      rownames_df[[i]],
      rownames_df[[i]] >= minfreq &
        rownames_df[[i]] <= maxfreq
    ))

    colnames_df[[i]] <- as.POSIXct(strptime(
      paste(soundscape_list[[i]]@first_day,
        colnames(soundscape_list[[i]]@aggregated_df),
        sep = " "
      ),
      format = "%Y-%m-%d %H:%M:%S",
      tz = soundscape_list[[i]]@tz
    ))

    colnames_subset[[i]] <- as.character(hms::as_hms(subset(
      colnames_df[[i]],
      colnames_df[[i]]
      >= mintime_list[[i]] &
        colnames_df[[i]]
        <= maxtime_list[[i]]
    )))

    new_soundscape_list[[i]] <- soundscape_list[[i]]@aggregated_df[rownames_subset[[i]], colnames_subset[[i]]]
  }

  # 2. Prepare the soundscape list to be in the correct format

  # 2.1. Create site-by-osu df

  site_by_OSU_matrix <-
    as.data.frame(
      t(do.call(
        rbind,
        lapply(
          new_soundscape_list,
          function(x) unlist(x)
        )
      ))
    )

  rownames(site_by_OSU_matrix) <- paste0(
    "OSU_",
    seq(1, nrow(site_by_OSU_matrix), 1)
  )

  colnames(site_by_OSU_matrix) <- as.character(names(soundscape_list))



  # Calculate the pairwise dissimilarities

  if (is.null(hier_table)) {
    soundscape_pairdis <- pairwise_dis(
      OSU_matrix = site_by_OSU_matrix,
      qvalue = qvalue
    )
  } else {
    soundscape_pairdis <- pairwise_dis(
      OSU_matrix = site_by_OSU_matrix,
      qvalue = qvalue,
      hierarchy = hier_table
    )
  }

  soundscape_pairdis
}

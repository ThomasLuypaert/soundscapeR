# Function for importing and merging index csv files

#' Import and Merge Acoustic Index '.csv' Files
#'
#' @description For an acoustic index of choice, imports all
#' spectral index '.csv' files produced by the \code{\link{ss_index_calc}}
#' function, and merges them into a time-by-frequency data frame.
#'
#' @param fileloc The full-length path to the directory where the
#' output directory of the \code{\link{ss_index_calc}} function are located.
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
#' the \code{\link{ss_index_calc}} documentation. For the "BGN" index, the minimum index
#'  value was added to all data frame values to obtain a data frame of positive
#'  values while retaining the relationship between individual OSUs
#'
#' @param date The first day of the recording period. Used
#'  for managing time-objects in R.
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
#' @param timezone_offset A time zone offset to apply (e.g., "07:00:00" for 7 hours ahead or "-05:00:00" for 5 hours behind). Defaults to no offset ("00:00:00").
#'
#' @return Returns a time-by-frequency dataframe of acoustic index values
#' for all files in the recording period.
#'
#' @export
#'
ss_index_merge <- function(fileloc,
                           samplerate,
                           window,
                           index,
                           date,
                           lat,
                           lon,
                           timezone_offset = "00:00:00") {
  # 0. Check if the arguments are missing

  test_0 <- function(x) {
    !missing(x)
  }

  assertthat::on_failure(test_0) <- function(call, env) {
    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")
  }

  assertthat::assert_that(test_0(fileloc))
  assertthat::assert_that(test_0(samplerate))
  assertthat::assert_that(test_0(window))
  assertthat::assert_that(test_0(index))
  assertthat::assert_that(test_0(date))
  assertthat::assert_that(test_0(lat))
  assertthat::assert_that(test_0(lon))

  # 1. Check if function inputs meet expectations

  # 1.1. fileloc is an existing, writable directory

  test_1 <- function(x) {
    assertthat::is.dir(x)
  }

  assertthat::assert_that(test_1(fileloc))

  test_2 <- function(x) {
    assertthat::is.readable(x)
  }

  assertthat::on_failure(test_2) <- function(call, env) {
    paste0(deparse(call$x), " is not a readable directory. Please change the permissions and try again.")
  }

  assertthat::assert_that(test_2(fileloc))

  # 1.2. samplerate is a single, positive integer

  test_3 <- function(x) {
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_3) <- function(call, env) {
    paste0(deparse(call$x), " is not a single, positive integer. Consult the package documentation for more information on the samplerate.")
  }

  assertthat::assert_that(test_3(samplerate))

  # 1.3. window is a single, positive integer

  test_4 <- function(x) {
    assertthat::is.count(x)
  }

  assertthat::on_failure(test_4) <- function(call, env) {
    paste0(deparse(call$x), " is not a single, positive integer. Consult the package documentation for more information on the window.")
  }

  assertthat::assert_that(test_4(window))

  # 1.4. produce a warning message if window is not a power of two

  if (!as.integer(log(window, base = 2)) == (log(window, base = 2))) {
    cli::cli_alert_warning("Chosen window size is not a power of two. This is a warning message, if your window size was chosen purposefully, proceed as planned by pressing Y. If you want to abort, press N.")

    question1 <- readline("Would you like to proceed with the chosen window size? (Y/N)")

    if (regexpr(question1, "y", ignore.case = TRUE) == 1) {
    } else {
      if (regexpr(question1, "n", ignore.case = TRUE) == 1) {
        cli::cli_abort("Index computation aborted.")
      } else {
        cli::cli_abort("The option you have chosen is not valid - index computation aborted")
      }
    }
  }

  # 1.5. check if specified index is one of available options

  test_5 <- function(x) {
    assertthat::is.string(x) &
      x %in% c(
        "BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
        "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"
      )
  }

  assertthat::on_failure(test_5) <- function(call, env) {
    paste0(deparse(call$x), " is not a character string of one of the available spectral acoustic indices. Please consult package documentation for available options. Pay attention to capital letters and the presence of excess spaces.")
  }

  assertthat::assert_that(test_5(index))

  # 1.6. Check if the supplied date argument is in the correct format

  test_6 <- function(x) {
    assertthat::is.string(x)
  }

  test_7 <- function(x) {
    formatted <- try(as.Date(x, "%Y-%m-%d"), silent = TRUE)
    is_date <- as.character(formatted) == x & !is.na(formatted)
    return(is_date)
  }

  assertthat::on_failure(test_6) <- function(call, env) {
    paste0(deparse(call$x), " is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.")
  }

  assertthat::on_failure(test_7) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.")
  }

  assertthat::assert_that(test_6(date))
  assertthat::assert_that(test_7(date))

  # 1.7. Check if latitude and longitude are specified in decimal degrees and
  # exist on Earth

  test_8 <- function(x) {
    is.numeric(x) &
      x >= -90 &
      x <= 90
  }

  test_9 <- function(x) {
    is.numeric(x) &
      x >= -180 &
      x <= 180
  }

  assertthat::on_failure(test_8) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")
  }

  assertthat::on_failure(test_9) <- function(call, env) {
    paste0(deparse(call$x), " is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")
  }

  assertthat::assert_that(test_8(lat))
  assertthat::assert_that(test_9(lon))

  # 1.8 Check that the timezone_offset argument has the correct format

  test_10 <- function(x) {
    is.character(x)
  }

  assertthat::assert_that(test_10(timezone_offset),
                          msg = "timezone_offset must be a string.")


  test_11 <- function(x) {
      grepl("^[+-]?[0-9]{2}:[0-9]{2}:[0-9]{2}$", x)
    }

  assertthat::assert_that(test_11(timezone_offset),
                          msg = "timezone_offset must be in the format 'HH:MM:SS' or '[+/-]HH:MM:SS'.")

  ### START

  cli::cli_h1(paste0("Processing folder: ", basename(fileloc)))

  # 2. Get timezone offset in seconds
  tz_offset_hms <- hms::as_hms(timezone_offset)  # Convert the string into a hms object
  sign_offset <- ifelse(substr(timezone_offset, 1, 1) == "-", -1, 1)
  tz_offset_seconds <- as.numeric(tz_offset_hms) * sign_offset


  # 3. Get a list of the acoustic index '.csv' files in the specified file location


  folders <- file.path(paste0(fileloc, "/", window))

  filenames <- list.files(folders,
    pattern = paste0("CVR.csv$"),
    recursive = TRUE,
    full.names = TRUE
  )

  # 4. Merge csv files into a dataframe

  merged_df <- as.data.frame(t(Reduce(rbind, lapply(filenames, function(x) data.table::fread(x)))[, -1]))

  # 5. Give the df rownames

  if (index == "OSC") {
    frequency_bins <- as.integer(
      seq(
        from = (samplerate / 2) / (window),
        to = samplerate / 2,
        by = (((samplerate / 2) / (window)))
      )
    )
  } else {
    frequency_bins <- as.integer(
      seq(
        from = (samplerate / window),
        to = samplerate / 2,
        by = (samplerate / window)
      )
    )
  }

  row.names(merged_df) <- as.integer(frequency_bins)

  # 6. Give the df colnames

  tz <- lutz::tz_lookup_coords(lat = lat, lon = lon, method = "accurate")

  colnames(merged_df) <- hms::as_hms(
    as.POSIXct(
      strptime(stringr::str_extract(filenames, "\\d{8}_\\d{6}"),
               "%Y%m%d_%H%M%S",
               tz = "UTC"  # first assume UTC time zone
      ) + tz_offset_seconds
    )
  )

  merged_df <- merged_df[seq(dim(merged_df)[1], 1), ]

  if (index == "BGN") {
    merged_df <- merged_df + abs(min(merged_df))
  } else {}

  # 7. Create some more useful metadata elements for the soundscape object

  day <- as.POSIXct(
    strptime(
      paste(date,
        "00:00:00",
        sep = " "
      ),
      format = "%Y-%m-%d %H:%M:%S",
      tz = tz
    )
  )

  points <- data.frame(lon = lon, lat = lat)

  suntimes <- suncalc::getSunlightTimes(
    date = as.Date(date),
    keep = c("sunrise", "sunriseEnd", "sunset", "sunsetStart"),
    lat = lat,
    lon = lon,
    tz = tz
  )


  sunrise <- as.POSIXct(suntimes$sunrise,
    tz = tz,
    format = "%Y-%m-%d %H:%M:%S"
  )
  sunset <- as.POSIXct(suntimes$sunset,
    tz = tz,
    format = "%Y-%m-%d %H:%M:%S"
  )

  merged_soundscape <- methods::new("soundscape",
    fileloc = fileloc,
    index = index,
    samplerate = samplerate,
    window = window,
    first_day = day,
    lat = lat,
    lon = lon,
    tz = tz,
    sunrise = sunrise,
    sunset = sunset,
    timezone_offset = timezone_offset,
    merged_df = merged_df
  )


  merged_soundscape
}

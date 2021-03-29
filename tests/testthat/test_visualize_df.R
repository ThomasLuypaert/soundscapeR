library(testthat)
library(soundscapeR)

# 1. Load the merged '.csv' data frame files,
# binarize the data frame, aggregate the data frame, and make
#wrong data frame types for testing purposes

fpath_CVR <- system.file("/extdata/merged_df/merged_df_CVR.csv",
                         package="soundscapeR")

merged_df_CVR <- read.csv(file = fpath_CVR,
                          header = TRUE,
                          sep = ",",
                          row.names = 1,
                          check.names = F)

binarized_df_CVR <- binarize_df(df = merged_df_CVR,
                                method = "Otsu",
                                value = NULL,
                                date = "2015-09-05",
                                lat = -1.915867928971629,
                                lon = -59.48937990402315)

aggregated_df_CVR <- aggregate_df(df = binarized_df_CVR,
                                  output = "incidence_freq",
                                  date = "2015-09-05",
                                  lat = -1.915867928971629,
                                  lon = -59.48937990402315)

aggregate_df_nolist_1 <- aggregated_df_CVR
aggregate_df_nolist_1[[1]] <- as.matrix(unlist(aggregate_df_nolist_1[[1]]))

aggregate_df_nolist_2 <- aggregated_df_CVR
aggregate_df_nolist_2[[2]] <- as.matrix(unlist(aggregate_df_nolist_2[[2]]))

aggregated_df_CVR_empty <- aggregated_df_CVR
aggregated_df_CVR_empty[[3]] <- aggregated_df_CVR[[3]][FALSE,]

aggregate_df_empty_1 <- aggregated_df_CVR
aggregate_df_empty_1[[1]] <- vector("list", length = 10)

aggregate_df_empty_2 <- aggregated_df_CVR
aggregate_df_empty_2[[2]] <- vector("list", length = 10)

aggregate_df_empty_3 <- aggregated_df_CVR
aggregate_df_empty_3[[3]] <- aggregated_df_CVR[[3]][FALSE,]

aggregated_df_CVR_matrix <- aggregated_df_CVR
aggregated_df_CVR_matrix[[3]] <- as.matrix(
  aggregated_df_CVR[[3]])

aggregated_df_CVR_NAs <- aggregated_df_CVR
aggregated_df_CVR_NAs[[3]][1,1] <- NA

aggregated_df_CVR_NAs_1 <- aggregated_df_CVR
aggregated_df_CVR_NAs_1[[1]][[1]] <- NA

aggregated_df_CVR_NAs_1 <- aggregated_df_CVR
aggregated_df_CVR_NAs_1[[1]][[1]] <- NA

aggregated_df_CVR_NAs_2 <- aggregated_df_CVR
aggregated_df_CVR_NAs_2[[2]][[1]] <- NA

aggregated_df_CVR_NAs_3 <- aggregated_df_CVR
aggregated_df_CVR_NAs_3[[3]][1,1] <- NA

aggregated_df_CVR_nonnum <- aggregated_df_CVR
aggregated_df_CVR_nonnum[[3]][1,1] <- "I'm not numeric!"

aggregated_df_CVR_falserows <- aggregated_df_CVR
rownames(aggregated_df_CVR_falserows[[3]]) <- seq(1, length(rownames(aggregated_df_CVR_falserows[[3]])), 1)

aggregated_df_CVR_falsecols <- aggregated_df_CVR
colnames(aggregated_df_CVR_falsecols[[3]]) <- seq(1, length(colnames(aggregated_df_CVR_falsecols[[3]])), 1)

# 2. Start testing the lengthen function

  # 2.0. If required argument is missing

testthat::test_that("the lengthen function provides the correct error when the df argument is missing", {

  testthat::expect_error(
    object = lengthen(date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "df argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the lengthen function provides the correct error when the date argument is missing", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "date argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the lengthen function provides the correct error when the lat argument is missing", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lon = -59.48937990402315),
    regexp = "lat argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the lengthen function provides the correct error when the lon argument is missing", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lat = -1.915867928971629),
    regexp = "lon argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})


  # 2.1. When all the correct arguments are supplied

testthat::test_that("the lengthen function works as expected when the correct input arguments are supplied",{

  function_var <- lengthen(df = aggregated_df_CVR[[3]],
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315)

  testing_function_lengthen <- function(df, date, lat, lon){

    tz <- lutz::tz_lookup_coords(lat=lat,
                                 lon=lon,
                                 method="accurate")

    df$frequency <- as.integer(rownames(df))

    melt_df <- reshape2::melt(df, id.vars="frequency")

    colnames(melt_df) <- c("frequency", "time", "value")

    melt_df$frequency <- as.numeric(
      as.character(melt_df$frequency))

    melt_df$time <- as.POSIXct(
      strptime(
        paste(date, melt_df$time, sep=" "),
        format= "%Y-%m-%d %H:%M",
        tz=tz))

    return(melt_df)

  }

  test_var <- testing_function_lengthen(
    df = aggregated_df_CVR[[3]],
    date = "2015-09-05",
    lat = -1.915867928971629,
    lon = -59.48937990402315)

  testthat::expect_equal(function_var, test_var)
  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(!any(is.na(function_var)))
  testthat::expect_true(
    !(is.null(ncol(function_var)) |
        is.null(nrow(function_var))))

  testthat::expect_true(is.numeric(function_var[,1]))
  testthat::expect_true(
    !any(
      is.na(
        as.POSIXct(function_var[,2],
                   format = "%Y-%m-&d %H:%M:%S"))))
  testthat::expect_true(is.numeric(function_var[,3]))

  testthat::expect_true(
    all(
      sort(unique(function_var[,1])) ==
        sort(as.numeric(rownames(aggregated_df_CVR[[3]]))))
  )

  testthat::expect_true(

    all(

      sort(
        hms::as_hms(
          colnames(aggregated_df_CVR[[3]]))) ==

        sort(
          hms::as_hms(
            sapply(
              unique(as.character(function_var[,2])),
              function(x) substr(x,
                                 12,
                                 25))))
      )
    )

  testthat::expect_true(

    all(
      sort(
        unique(
          unlist(
            aggregated_df_CVR[[3]]))) ==

        sort(
          unique(
            function_var[,3]))

    )
  )

  testthat::expect_equal(colnames(function_var),
                         c("frequency", "time", "value"))




})

  # 2.2. If the wrong df argument is supplied

    # 2.2.1. Argument is not a data frame

testthat::test_that("the lengthen function provides the correct error message when the supplied df argument is not a data frame", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR_matrix[[3]],
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "df is not a data frame. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe (third element in the list) produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.2. Argument is an empty data frame

testthat::test_that("the lengthen function provides the correct error message when the supplied df argument is an empty data frame", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR_empty[[3]],
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "df is an empty dataframe. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe (third element in the list) produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.3. Argument is a data frame containing NAs

testthat::test_that("the lengthen function provides the correct error message when the supplied df argument is na data frame containing NA values", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR_NAs[[3]],
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "df contains NA values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe (third element in the list) produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.4. Argument is a data frame containing non numeric
    # values

testthat::test_that("the lengthen function provides the correct error message when the supplied df argument is a data frame containing non-numeric values", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR_nonnum[[3]],
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "df contains non-numeric values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe (third element in the list) produced by the aggregate_df() function.",
    fixed=TRUE)

})

  # 2.3. The data argument is not in the right format

    # 2.3.1. The date argument is not a character string

testthat::test_that("the lengthen function produces the correct error message when the date argument is not a character string", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = as.factor("2015-09-05"),
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "date is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed=TRUE
  )

})

    # 2.3.2. The date argument does not have the
    # correct format

testthat::test_that("the lengthen function produces the correct error message when the date argument does not have the correct format.", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "05-09-2015",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed=TRUE
  )

})

testthat::test_that("the lengthen function produces the correct error message when the date argument does not have the correct format.", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "05/09/2015",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed=TRUE
  )

})


  # 2.4 The lat and lon argument are not in the
  # correct format

    # 2.4.1. The lat and/or lon are not supplied in
    # decimal degrees

testthat::test_that("the lengthen function produces the correct error message when the latitude argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lat = "1°55'00.1",
                      lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the lengthen function produces the correct error message when the longitude argument is supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = "59°28'25.7"),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the lengthen function produces the correct error message when the latitude and longitude argument are supplied in the degree-minute-second format", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lat = "1°54'57.2",
                      lon = "59°28'25.7"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

    #2.4.2. Latitude and longitude are numerical, but
    # don't fall within the expected values existing on
    # Earth

testthat::test_that("the lengthen function produces the correct error message when the latitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lat = 91,
                      lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the lengthen function produces the correct error message when the latitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lat = -91,
                      lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the lengthen function produces the correct error message when the longitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = 181),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the lengthen function produces the correct error message when the longitude argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -181),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the lengthen function produces the correct error message when the latitude and longitude arguments are numeric, but don't fit the existing values on Earth", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lat = 91,
                      lon = 181),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the lengthen function produces the correct error message when the latitude and longitude arguments are numeric, but don't fit the existing values on Earth", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR[[3]],
                      date = "2015-09-05",
                      lat = -91,
                      lon = -181),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

  # 4.5. Rownames and/or column names of df argument
  # not correct

testthat::test_that("the lengthen function provides the correct error message when the row names of the supplied df argument are not frequency values", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR_falserows[[3]],
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed = TRUE)

})

testthat::test_that("the lengthen function provides the correct error message when the column names of the supplied df argument are not character strings indicating times in the following format: HH:MM:SS", {

  testthat::expect_error(
    object = lengthen(df = aggregated_df_CVR_falsecols[[3]],
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed = TRUE)

})

# 3. Start testing the heatmapper function

  # 3.0. If required argument is missing

testthat::test_that("the heatmapper function provides the correct error when the aggregate_list argument is missing", {

  testthat::expect_error(
    object = heatmapper(date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the heatmapper function provides the correct error when the date argument is missing", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "date argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the heatmapper function provides the correct error when the lat argument is missing", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lon = -59.48937990402315),
    regexp = "lat argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

testthat::test_that("the heatmapper function provides the correct error when the lon argument is missing", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629),
    regexp = "lon argument is missing. Please supply the missing argument.",
    fixed = TRUE)


})

  # 3.1. When all the correct arguments are supplied

    # 3.1.1. type = 'regular' and annotate = FALSE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- suppressWarnings(
    heatmapper(aggregate_list = aggregated_df_CVR,
               type = "regular",
               annotate = FALSE,
               date = "2015-09-05",
               lat = -1.915867928971629,
               lon = -59.48937990402315)
    )


  vdiffr::expect_doppelganger("heatmapper_regular_no_anno", plot)

})

    # 3.1.2. type = 'regular' and annotate = TRUE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- suppressWarnings(
    heatmapper(aggregate_list = aggregated_df_CVR,
               type = "regular",
               annotate = TRUE,
               date = "2015-09-05",
               lat = -1.915867928971629,
               lon = -59.48937990402315)
  )


  vdiffr::expect_doppelganger("heatmapper_regular_anno", plot)

})

    # 3.1.3. type = 'polar' and annotate = FALSE

 # testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
 #
 #   plot <- suppressWarnings(
 #     heatmapper(aggregate_list = aggregated_df_CVR,
 #                type = "polar",
 #                annotate = FALSE,
 #                date = "2015-09-05",
 #                lat = -1.915867928971629,
 #                lon = -59.48937990402315)
 #   )
 #
 #
 #   vdiffr::expect_doppelganger("heatmapper_polar_no_anno", plot)
 #
 # })

    # 3.1.4. type = 'polar' and annotate = TRUE

 # testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
 #
 #   plot <- suppressWarnings(
 #     heatmapper(aggregate_list = aggregated_df_CVR,
 #                type = "polar",
 #                annotate = TRUE,
 #                date = "2015-09-05",
 #                lat = -1.915867928971629,
 #                lon = -59.48937990402315)
 #   )
 #
 #
 #   vdiffr::expect_doppelganger("heatmapper_polar_anno", plot)
 #
 # })

  # 3.1.5. type = 'regular', annotate = FALSE & subsetting

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- suppressWarnings(
    heatmapper(aggregate_list = aggregated_df_CVR,
               type = "regular",
               annotate = FALSE,
               minfreq = 2000,
               maxfreq = 15000,
               mintime = "04:00:00",
               maxtime = "20:00:00",
               date = "2015-09-05",
               lat = -1.915867928971629,
               lon = -59.48937990402315)
  )


  vdiffr::expect_doppelganger("heatmapper_regular_no_anno_subset", plot)

})


testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- suppressWarnings(
    heatmapper(aggregate_list = aggregated_df_CVR,
               type = "regular",
               annotate = TRUE,
               minfreq = 2000,
               maxfreq = 15000,
               mintime = "04:00:00",
               maxtime = "20:00:00",
               date = "2015-09-05",
               lat = -1.915867928971629,
               lon = -59.48937990402315)
  )


  vdiffr::expect_doppelganger("heatmapper_regular_anno_subset", plot)

})

    # 3.1.4. If type = 'regular' and zero.black = TRUE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- suppressWarnings(
    heatmapper(aggregate_list = aggregated_df_CVR,
               type = "regular",
               zero.black = TRUE ,
               date = "2015-09-05",
               lat = -1.915867928971629,
               lon = -59.48937990402315)
  )


  vdiffr::expect_doppelganger("heatmapper_regular_zero.black", plot)

})

  # 3.1.5. If type = 'polar' and zero.black = TRUE
#
# testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
#
#   plot <- suppressWarnings(
#     heatmapper(aggregate_list = aggregated_df_CVR,
#                type = "polar",
#                zero.black = TRUE ,
#                date = "2015-09-05",
#                lat = -1.915867928971629,
#                lon = -59.48937990402315)
#   )
#
#
#   vdiffr::expect_doppelganger("heatmapper_polar_zero.black", plot)
#
# })

    # 3.1.6. If type = 'regular' and maxfreq > 22000 and direction = 1

# testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
#
#   plot <- suppressWarnings(
#     heatmapper(aggregate_list = aggregated_df_CVR,
#                type = "regular",
#                date = "2015-09-05",
#                lat = -1.915867928971629,
#                lon = -59.48937990402315,
#                maxfreq = 22001)
#   )
#
#
#   vdiffr::expect_doppelganger("heatmapper_regular_maxfreq_22001_dir1", plot)
#
# })

    # 3.1.7. If type = 'regular' and maxfreq > 22000 and direction = -1

# testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
#
#   plot <- suppressWarnings(
#     heatmapper(aggregate_list = aggregated_df_CVR,
#                type = "regular",
#                date = "2015-09-05",
#                lat = -1.915867928971629,
#                lon = -59.48937990402315,
#                maxfreq = 22001,
#                direction = -1)
#   )
#
#
#   vdiffr::expect_doppelganger("heatmapper_regular_maxfreq_22001_dir-1", plot)
#
# })


    # 3.1.8. If type = 'regular' and direction = -1

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- suppressWarnings(
    heatmapper(aggregate_list = aggregated_df_CVR,
               type = "regular",
               date = "2015-09-05",
               lat = -1.915867928971629,
               lon = -59.48937990402315,
               direction = -1)
  )


  vdiffr::expect_doppelganger("heatmapper_regular_dir1", plot)

})

    # 3.1.9. If type = 'polar' and direction = -1

# testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {
#
#   plot <- suppressWarnings(
#     heatmapper(aggregate_list = aggregated_df_CVR,
#                type = "polar",
#                date = "2015-09-05",
#                lat = -1.915867928971629,
#                lon = -59.48937990402315,
#                direction = -1)
#   )
#
#
#   vdiffr::expect_doppelganger("heatmapper_polar_dir-1", plot)
#
# })

    # 3.1.10. If marginplot = FALSE, interactive = FALSE, save = TRUE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- suppressWarnings(
    heatmapper(aggregate_list = aggregated_df_CVR,
               type = "regular",
               date = "2015-09-05",
               lat = -1.915867928971629,
               lon = -59.48937990402315,
               marginplot = FALSE,
               interactive = FALSE,
               save = TRUE,
               dir = getwd(),
               filename = "file",
               device = "png")
  )


  vdiffr::expect_doppelganger("heatmapper_regular_nomarg_noint_save", plot)

})

    # 3.1.11. If marginplot = FALSE, interactive = TRUE, save = FALSE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- suppressWarnings(
    heatmapper(aggregate_list = aggregated_df_CVR,
               type = "regular",
               date = "2015-09-05",
               lat = -1.915867928971629,
               lon = -59.48937990402315,
               marginplot = FALSE,
               interactive = TRUE,
               save = FALSE)
  )


  vdiffr::expect_doppelganger("heatmapper_regular_nomarg_int_nosave", plot)

})

    # 3.1.12. If marginplot = FALSE, interactive = TRUE, save = TRUE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- suppressWarnings(
    heatmapper(aggregate_list = aggregated_df_CVR,
               type = "regular",
               date = "2015-09-05",
               lat = -1.915867928971629,
               lon = -59.48937990402315,
               marginplot = FALSE,
               interactive = TRUE,
               save = FALSE)
  )


  vdiffr::expect_doppelganger("heatmapper_regular_nomarg_int_save", plot)

})

    # 3.1.13. If marginplot = TRUE, interactive = FALSE, save = FALSE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregate_list = aggregated_df_CVR,
                     type = "regular",
                     date = "2015-09-05",
                     lat = -1.915867928971629,
                     lon = -59.48937990402315,
                     marginplot = TRUE,
                     interactive = FALSE,
                     save = FALSE)

  vdiffr::expect_doppelganger("heatmapper_regular_marg_noint_nosave", plot)

})

    # 3.1.14. If marginplot = TRUE, interactive = FALSE, save = TRUE

testthat::test_that("the heatmapper function works as expected when the correct arguments are supplied", {

  plot <- heatmapper(aggregate_list = aggregated_df_CVR,
                     type = "regular",
                     date = "2015-09-05",
                     lat = -1.915867928971629,
                     lon = -59.48937990402315,
                     marginplot = TRUE,
                     interactive = FALSE,
                     save = TRUE)

  vdiffr::expect_doppelganger("heatmapper_regular_marg_noint_save", plot)

})


  # 3.2. If the wrong aggregate_list argument is supplied

    # 3.2.1. aggregate_list argument is not a list

testthat::test_that("the heatmapper function provides the correct error message when the supplied aggregate_list argument is not a list", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR[[3]],
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list is not a list of the correct length. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 3.2.2. aggregate_list[[1]] is not a list

testthat::test_that("the heatmapper function provides the correct error message when the supplied aggregate_list argument's first element is not a list", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregate_df_nolist_1,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list[[1]] is not a list. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 3.2.3. aggregate_list[[2]] is not a list

testthat::test_that("the heatmapper function provides the correct error message when the supplied aggregate_list argument's second element is not a list", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregate_df_nolist_2,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list[[2]] is not a list. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 3.2.4. aggregate_list[[1]] is an empty list

testthat::test_that("the heatmapper function provides the correct error message when the supplied aggregate_list first element is an empty list", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregate_df_empty_1,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list[[1]] is an empty list. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 3.2.5. aggregate_list[[2]] is an empty list

testthat::test_that("the heatmapper function provides the correct error message when the supplied aggregate_list second element is an empty list", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregate_df_empty_2,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list[[2]] is an empty list. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

      # 3.2.6. aggregate_list[[3]] is an empty data frame

testthat::test_that("the heatmapper function provides the correct error message when the supplied aggregate_list third element is an empty data frame", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregate_df_empty_3,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list[[3]] is an empty data frame. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 3.2.7. aggregate_list[[1]] contains NAs

testthat::test_that("the heatmapper function provides the correct error message when the supplied aggregate_list first element contains NA values", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR_NAs_1,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list[[1]] contains NA values. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 3.2.8. aggregate_list[[2]] contains NAs

testthat::test_that("the heatmapper function provides the correct error message when the supplied aggregate_list second element contains NA values", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR_NAs_2,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list[[2]] contains NA values. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 3.2.9. aggregate_list[[3]] contains NAs

testthat::test_that("the heatmapper function provides the correct error message when the supplied aggregate_list third element contains NA values", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR_NAs_3,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list[[3]] contains NA values. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 3.2.10. aggregate_list[[3]] is a data frame containing
    # non-numeric values

testthat::test_that("the heatmapper function provides the correct error message when the supplied aggregate_list third element is a data frame containing non-numeric values", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR_nonnum,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "aggregate_list[[3]] contains non-numeric values. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

  # 3.3. If the wrong type argument is supplied

    # 3.3.1. The type argument is not a character string

testthat::test_that("the heatmapper function provides the correct error message when the supplied type argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        type = as.factor("regular"),
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "type is not a character string. Please supply the heatmap type as a character string. Consult package documentation for available type argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE)

})

    # 3.3.2. The type argument is not one of the available options

testthat::test_that("the heatmapper function provides the correct error message when the supplied type argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        type = "I'm not an option!",
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "type is not one of the available heatmap type options. Please consult package documentation for available type argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE)

})

  # 3.4. The wrong annotate argument is supplied

    # 3.4.1. The supplied annotate argument is not a boolean flag

testthat::test_that("the heatmapper function provides the correct error message when the supplied annotate argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        annotate = "I'm not a boolean flag!",
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "annotate is not a Boolean flag (TRUE or FALSE). Please set annotate argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied annotate argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        annotate = "TRUE",
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "annotate is not a Boolean flag (TRUE or FALSE). Please set annotate argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed=TRUE)

})

  # 3.5. The wrong timeinterval argument is supplied

    # 3.5.1. The supplied timeinterval argument is not one of the options

testthat::test_that("the heatmapper function provides the correct error message when the supplied timeinterval argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        timeinterval = "half an hour"),
    regexp = "timeinterval is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied timeinterval argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        timeinterval = "thirty minutes"),
    regexp = "timeinterval is not one of the available timeinterval options. Please make sure the timeinterval argument is a character string of the following format: n unit (with n = number, and unit = one of 'sec', 'secs', 'min', 'mins', 'hour', 'hours', 'day', 'days', 'week', 'weeks', 'month', 'months', 'year', 'years'). Please consult the scales::breaks_width() documentation for more information.",
    fixed=TRUE)

})

  # 3.6. The wrong mintime and maxtime arguments are supplied

    # 3.6.1. The mintime argument is in the wrong format

testthat::test_that("the heatmapper function provides the correct error message when the supplied mintime argument does not fit the expected format", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        mintime = "09-00-00"),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE)

})

    # 3.6.2. The maxtime argument is in the wrong format

testthat::test_that("the heatmapper function provides the correct error message when the supplied maxtime argument does not fit the expected format", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        maxtime = "18-00-00"),
    regexp = "maxtime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE)

})

    # 3.6.3. The mintime and maxtime arguments are in the wrong format

testthat::test_that("the heatmapper function provides the correct error message when the supplied mintime and maxtime arguments don't not fit the expected format", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        mintime = "09-00-00",
                        maxtime = "18-00-00"),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE)

})

  # 3.7. The wrong freqinterval argument is supplied

    # 3.7.1. The supplied freqinterval argument is not a
    # single positive integer

testthat::test_that("the heatmapper function provides the correct error message when the supplied freqinterval argument is not a single positive integer", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        freqinterval = 1.000001),
    regexp = "freqinterval is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).",
    fixed=TRUE)

})

    # 3.7.2. The supplied freqinterval argument is out of the frequency
    # bounds of the data frame

testthat::test_that("the heatmapper function provides the correct error message when the supplied freqinterval argument is larger than the upper frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        freqinterval = 25000),
    regexp = "freqinterval is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied freqinterval argument is lower than the lower frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        freqinterval = 1),
    regexp = "freqinterval is not a single positive integer, or is outside of the frequency bounds of the data frame. Please supply the frequency interval as a single positive integer which falls without the data frame's frequency bounds (min frequency < freqinterval < max frequency).",
    fixed=TRUE)

})

  # 3.8. The wrong minfreq and maxfreq arguments are supplied

    # 3.8.1. The minfreq argument is not a single positive integer

testthat::test_that("the heatmapper function provides the correct error message when the supplied minfreq argument is not a single positive integer", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        minfreq = 1.0001),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

    # 3.8.2. The maxfreq argument is not a single positive integer

testthat::test_that("the heatmapper function provides the correct error message when the supplied maxfreq argument is not a single positive integer", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        maxfreq = 1.0001),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

    # 3.8.3. The minfreq and maxfreq arguments are not a single
    # positive integers

testthat::test_that("the heatmapper function provides the correct error message when the supplied minfreq and maxfreq arguments are not a single positive integers", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        minfreq = 1.0001,
                        maxfreq = 1.0001),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

    # 3.8.4. The minfreq argument falls outside of the data frame
    # frequency bounds

testthat::test_that("the heatmapper function provides the correct error message when the supplied minfreq argument is lower than the lower frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        minfreq = 1),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied minfreq argument is higher than the upper frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        minfreq = 25000),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

    # 3.8.5. The maxfreq argument falls outside of the data frame
    # frequency bounds

testthat::test_that("the heatmapper function provides the correct error message when the supplied maxfreq argument is lower than the lower frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        maxfreq = 1),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied maxfreq argument is higher than the upper frequency boundary of the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        maxfreq = 25000),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

    # 3.8.6. The maxfreq argument is zero

testthat::test_that("the heatmapper function provides the correct error message when the supplied maxfreq argument is zero", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        maxfreq = 0),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

  # 3.9. The wrong nbins argument is supplied

    # 3.9.1. The nbins argument is not single positive integer

testthat::test_that("the heatmapper function provides the correct error message when the supplied nbins argument is not a single positive integer", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        nbins = -1),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the marginplot argument. If you wish to display a marginal plot, please set marginplot = TRUE.",
    fixed=TRUE)

})

    # 3.9.2. The nbins argument is zero

testthat::test_that("the heatmapper function provides the correct error message when the supplied nbins argument is zero", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        nbins = -1),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the marginplot argument. If you wish to display a marginal plot, please set marginplot = TRUE.",
    fixed=TRUE)

})

    # 3.9.3. The nbins argument is higher than the number of bins (rows)
    # in the data frame

testthat::test_that("the heatmapper function provides the correct error message when the supplied nbins argument is higher than the number of bins (rows) in the data frame", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        nbins = 100000),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the marginplot argument. If you wish to display a marginal plot, please set marginplot = TRUE.",
    fixed=TRUE)

})

  # 3.10. The wrong date argument is supplied

    # 3.10.1. The date argument is not a character string

testthat::test_that("the heatmapper function provides the correct error message when the supplied date argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = as.factor("2015-09-05"),
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "date is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed=TRUE)

})

    # 3.10.2. The date argument does not follow the expected format

testthat::test_that("the heatmapper function provides the correct error message when the supplied date argument does not follow the expected format", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "05-09-2015",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied date argument does not follow the expected format", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "05/09/2015",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed=TRUE)

})


  # 3.11. The wrong lat and lon arguments are supplied

    # 3.11.1. The lat and/or lon are not supplied in decimal degrees

testthat::test_that("the heatmapper function provides the correct error message when the supplied lat argument is not supplied in decimal degrees", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = "1°55'00.1",
                        lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied lon argument is not supplied in decimal degrees", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = "59°28'25.7"),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied lat and lon arguments are not supplied in decimal degrees", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = "1°54'57.2",
                        lon = "59°28'25.7"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

    # 3.11.2. The lat and lon are numerical, but don't fall within the
    # expected values existing on Earth

testthat::test_that("the heatmapper function provides the correct error message when the supplied lat argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = 91,
                        lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied lat argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -91,
                        lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied lon argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = 181),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied lon argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -181),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied lat and lon arguments are numeric, but don't fit the existing values on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = 91,
                        lon = 181),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied lat and lon arguments are numeric, but don't fit the existing values on Earth", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -91,
                        lon = -181),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

  # 3.12. The wrong twilight argument is supplied

    # 3.12.1. The twilight argument is not a string

testthat::test_that("the heatmapper function provides the correct error message when the supplied twilight argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        twilight = as.factor("sunlight")),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE)

})

    # 3.12.2. The twilight argument is not one of the available options

testthat::test_that("the heatmapper function provides the correct error message when the supplied twilight argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        twilight = "I'm not an option!"),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the supplied twilight argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        twilight = as.vector(2.15, mode="integer")),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE)

})

  # 3.13. The wrong labelsize arguments are provided

    # 3.13.1. labelsize_time is zero

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_time argument is zero", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        labelsize_time = 0),
    regexp = "labelsize_time is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

    # 3.13.2. labelsize_frequency is zero

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_frequency argument is zero", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        labelsize_frequency = 0),
    regexp = "labelsize_frequency is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

    # 3.13.3. labelsize_polar is zero

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_polar argument is zero", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        labelsize_polar = 0),
    regexp = "labelsize_polar is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

    # 3.13.4. labelsize_time is not a single positive integer

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_time argument is not a single positive number", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        labelsize_time = -2),
    regexp = "labelsize_time is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

    # 3.13.5. labelsize_frequency is not a single positive integer

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_frequency argument is not a single positive number", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        labelsize_frequency = -2),
    regexp = "labelsize_frequency is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})


    # 3.13.6. labelsize_polar is not a single positive integer

testthat::test_that("the heatmapper function provides the correct error message when the supplied labelsize_polar argument is not a single positive number", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        labelsize_polar = -2),
    regexp = "labelsize_polar is not a valid labelsize argument. The labelsize_... arguments need to be provided as a single positive number with a value large than 0.",
    fixed=TRUE)

})

  # 3.14. The wrong palette argument is provided

    # 3.14.1. The palette argument is not a character string

testthat::test_that("the heatmapper function provides the correct error message when the supplied palette argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        palette = as.factor("magma")),
    regexp = "palette is not a valid palette argument. Palette needs to be supplied as a character string of either: 'A', 'B', 'C', 'D', 'E', 'magma', 'inferno', 'plasma', 'viridis' or 'cividis'. Please supply a valid palette argument. Consult the soundscapeR of viridis package documentation for more information.",
    fixed=TRUE)

})

    # 2.14.2. The palette argument is not one of the available options

testthat::test_that("the heatmapper function provides the correct error message when the supplied palette argument is not one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        palette = "I'm not an option!"),
    regexp = "palette is not a valid palette argument. Palette needs to be supplied as a character string of either: 'A', 'B', 'C', 'D', 'E', 'magma', 'inferno', 'plasma', 'viridis' or 'cividis'. Please supply a valid palette argument. Consult the soundscapeR of viridis package documentation for more information.",
    fixed=TRUE)

})

  # 3.15. The wrong direction argument is provided

    # 3.15.1. The direction argument is not 1 or -1


testthat::test_that("the heatmapper function provides the correct error message when the supplied direction argument is one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        direction = 2),
    regexp = "direction is not a valid direction argument. The direction argument needs to be supplied as a single integer of either 1 or -1. Please supply a valid direction argument. Consult the soundscapeR of viridis package documentation for more information.",
    fixed=TRUE)

})

  # 3.16. Check if the boolean arguments follow the expected format

    # 3.16.1. Is the zero.black argument a boolean flag?

testthat::test_that("the heatmapper function provides the correct error message when the supplied zero.black argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        zero.black = "Not a boolean flag!"),
    regexp = "zero.black is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE)

})

    # 3.16.2. Is the marginplot argument a boolean flag?

testthat::test_that("the heatmapper function provides the correct error message when the supplied marginplot argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = "Not a boolean flag!"),
    regexp = "marginplot is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE)

})

    # 3.16.3. Is the interactive argument a boolean flag?

testthat::test_that("the heatmapper function provides the correct error message when the supplied interactive argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        interactive = "Not a boolean flag!"),
    regexp = "interactive is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE)

})

    # 3.16.4. Is the save argument a boolean flag?

testthat::test_that("the heatmapper function provides the correct error message when the supplied save argument is not a boolean flag", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        save = "Not a boolean flag!"),
    regexp = "save is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.",
    fixed=TRUE)

})

  # 3.17. Check if, when the marginplot argument is set to TRUE,
  # correct additional argument are supplied, and arguments are not in
  # conflict

    # 3.17.1. marginplot = TRUE but type = 'polar'

testthat::test_that("the heatmapper function provides the correct error message when the marginplot argument is set to true, but the type argument is set to 'polar'", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        type = "polar",
                        interactive = FALSE,
                        nbins = 10,
                        n_time = 5,
                        n_freq = 5
                        ),
    regexp = "marginplot is used with other arguments which are not accepted. The marginplot=TRUE argument can not be used in synergy with type='polar' or interactive = TRUE.",
    fixed=TRUE)

})

    # 3.17.2. marginplot = TRUE but interactive = TRUE

testthat::test_that("the heatmapper function provides the correct error message when the marginplot argument is set to true, but the type argument is set to 'polar'", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        type = "regular",
                        interactive = TRUE,
                        nbins = 10,
                        n_time = 5,
                        n_freq = 5
    ),
    regexp = "marginplot is used with other arguments which are not accepted. The marginplot=TRUE argument can not be used in synergy with type='polar' or interactive = TRUE.",
    fixed=TRUE)

})

 # 3.18. The n_time and n_freq arguments don't follow the expected format

    # 3.18.1. The wrong n_time argument is supplied

testthat::test_that("the heatmapper function provides the correct error message when the n_time argument is not an integer'", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = 3.25,
                        n_freq = 5
    ),
    regexp = "n_time does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the n_time argument is not an integer'", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = "5",
                        n_freq = 5
    ),
    regexp = "n_time does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the n_time argument is not positive'", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = -5,
                        n_freq = 5
    ),
    regexp = "n_time does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

    # 3.18.2. The wrong n_freq argument is supplied

testthat::test_that("the heatmapper function provides the correct error message when the n_freq argument is not an integer'", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = 5,
                        n_freq = 3.25
    ),
    regexp = "n_freq does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the n_freq argument is not an integer'", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = 5,
                        n_freq = "5"
    ),
    regexp = "n_freq does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the n_freq argument is not positive'", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        marginplot = TRUE,
                        nbins = 10,
                        n_time = 5,
                        n_freq = -5
    ),
    regexp = "n_freq does not have the correct
           format. Please supply the argument as a single
           positive integer. Consult package documentation
           for more information.",
    fixed=TRUE)

})

  # 3.19. Check if the dir, filename and device argument follow
  # the expected format

    # 3.19.1. The dir argument is wrong

testthat::test_that("the heatmapper function provides the correct error message when the dir argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        save = TRUE,
                        dir = as.factor(getwd()),
                        filename = "test" ,
                        device = "png"
    ),
    regexp = "dir is not a character string. The dir arguments needs to be a character string of either 'default' - or a valid pathname to an existing directory on your device. If you're working on a Windows operating system, pay attention to backslash and forwardslash.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the dir argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        save = TRUE,
                        dir = paste0(getwd(), "/IDontExist"),
                        filename = "test" ,
                        device = "png"
    ),
    regexp = paste0("Path ", paste0("'", getwd(), "/IDontExist", "'"), " does not exist"),
    fixed=TRUE)

})

    # 3.19.2. The filename argument is wrong

testthat::test_that("the heatmapper function provides the correct error message when the filename argument is not a character string", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        save = TRUE,
                        dir = getwd(),
                        filename = as.factor("test"),
                        device = "png"
    ),
    regexp = "filename is not a valid filename argument. The filename argument needs to be a character string.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the filename argument ends with an extension", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        save = TRUE,
                        dir = getwd(),
                        filename = "filename.png",
                        device = "png"
    ),
    regexp = "filename is not a valid filename argument. Please make the filename argument you provide a character string without the extension.",
    fixed=TRUE)

})

    # 3.19.3. The device argument is wrong

testthat::test_that("the heatmapper function provides the correct error message when the device argument is not a string", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        save = TRUE,
                        dir = getwd(),
                        filename = "filename",
                        device = as.factor("png")
    ),
    regexp = "device is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.",
    fixed=TRUE)

})

testthat::test_that("the heatmapper function provides the correct error message when the device argument is one of the available options", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        save = TRUE,
                        dir = getwd(),
                        filename = "filename",
                        device = "I'm not an option!"
    ),
    regexp = "device is not a valid device argument. The device argument needs to be a character string, and one of the following options: eps, ps, tex, pdf, jpeg, tiff, png, bmp, svg, wmf.",
    fixed=TRUE)

})

  # 3.20. The width and height arguments don't follow the expected format

    # 3.20.1. The wrong width argument is supplied

testthat::test_that("the heatmapper function provides the correct error message when the width argument is a negative integer", {

  testthat::expect_error(
    object = heatmapper(aggregate_list = aggregated_df_CVR,
                        date = "2015-09-05",
                        lat = -1.915867928971629,
                        lon = -59.48937990402315,
                        save = TRUE,
                        dir = getwd(),
                        filename = "filename",
                        device = "png",
                        width = -1,
                        height = 200
    ),
    regexp = "width is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
    fixed=TRUE)
})

  testthat::test_that("the heatmapper function provides the correct error message when the width argument is a string", {

    testthat::expect_error(
      object = heatmapper(aggregate_list = aggregated_df_CVR,
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315,
                          save = TRUE,
                          dir = getwd(),
                          filename = "filename",
                          device = "png",
                          width = "200",
                          height = 200
      ),
      regexp = "width is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
      fixed=TRUE)

})

  testthat::test_that("the heatmapper function provides the correct error message when the width argument is not an integer", {

    testthat::expect_error(
      object = heatmapper(aggregate_list = aggregated_df_CVR,
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315,
                          save = TRUE,
                          dir = getwd(),
                          filename = "filename",
                          device = "png",
                          width = 21.50,
                          height = 200
      ),
      regexp = "width is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
      fixed=TRUE)

  })

    # 3.20.2. The wrong length argument is supplied

  testthat::test_that("the heatmapper function provides the correct error message when the length argument is a negative integer", {

    testthat::expect_error(
      object = heatmapper(aggregate_list = aggregated_df_CVR,
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315,
                          save = TRUE,
                          dir = getwd(),
                          filename = "filename",
                          device = "png",
                          width = 200,
                          height = -1
      ),
      regexp = "height is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
      fixed=TRUE)
  })

  testthat::test_that("the heatmapper function provides the correct error message when the height argument is a string", {

    testthat::expect_error(
      object = heatmapper(aggregate_list = aggregated_df_CVR,
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315,
                          save = TRUE,
                          dir = getwd(),
                          filename = "filename",
                          device = "png",
                          width = 200,
                          height = "200"
      ),
      regexp = "height is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
      fixed=TRUE)

  })

  testthat::test_that("the heatmapper function provides the correct error message when the height argument is not an integer", {

    testthat::expect_error(
      object = heatmapper(aggregate_list = aggregated_df_CVR,
                          date = "2015-09-05",
                          lat = -1.915867928971629,
                          lon = -59.48937990402315,
                          save = TRUE,
                          dir = getwd(),
                          filename = "filename",
                          device = "png",
                          width = 200,
                          height = 21.50
      ),
      regexp = "height is not a valid argument. The height and width arguments needs to be supplied as a single positive integer. The height and width argument use the unit 'mm'. When values are too large, the figure will fail to plot/save.",
      fixed=TRUE)

  })



pcoa1 <- pcoa2 <- site <- axis <- pe <- patchwork <- NULL


#' Principle Coordinate plots for a list of soundscapes
#'
#' @description Creates a Principle Coordinate plot, using a dimensionality reduction
#' approach to plot the distances between soundscapes in 2D space based on the Bray-Curtis dissimilarity.
#'
#' @param soundscape_list A list of soundscape objects. Each object in the
#' list should be produced using the \code{\link{ss_create}} function (or ss_index_merge,
#' ss_binarize, and ss_aggregate in sequence).
#'
#' @param grouping A numeric or character vector indicating potential grouping of the elements in the 'soundscape_list' object.
#' Make sure that the grouping vector has the same length as the 'soundscape_list' argument.
#'
#' @param screeplot A boolean operator indicating whether the function should produce a screeplot in addition to the PCoA plot.
#' Options are: TRUE and FALSE.
#'
#' @return Returns a principal coordinate plot
#'
#' @export

ss_pcoa <- function(soundscape_list,
                    grouping = NA,
                    screeplot = FALSE){

  # 0. Testing

  # 0.1. Check that the soundscape_list argument

  # Is it missing?

  test_1 <- function(x){

    !missing(x)

  }

  assertthat::on_failure(test_1) <- function(call, env){

    paste0(deparse(call$x), " argument is missing. Please supply the missing argument.")

  }

  assertthat::assert_that(test_1(soundscape_list))

  # Is it a list?

  test_2 <- function(x){

    is.list(x)

  }

  assertthat::on_failure(test_2) <- function(call, env){

    paste0(deparse(call$x), " argument is not a list Please supply a list of aggregated soundscape objects.")

  }

  assertthat::assert_that(test_2(soundscape_list))

  # Are all elements of the list S4-object of the type 'soundscape' and not empty.

  test_3 <- function(x){

    all(
      sapply(x,
             function(y) isS4(y) &
               assertthat::are_equal(class(y)[1], "soundscape") &
               assertthat::not_empty(y)
             ) ==TRUE)

  }

  assertthat::on_failure(test_3) <- function(call, env){

    paste0(deparse(call$x), " is not a list of S4-objects of the type 'soundscape'. The list may contain different objects or empty elements. Please supply a list of soundscape objects produced by the ss_aggregate() function. Consult the package documentation for further information.")

  }

  assertthat::assert_that(test_3(soundscape_list))

  # 0.2. Check if all the components of the soundscape objects in the list follow the expected format

  # 0.1.3. The soundscape elements are in the expected format

  # The lat and lon argument

  test_4 <- function(x){

    all(
      sapply(x,
             function(y) is.numeric(y@lat) &
               y@lat >= -90 &
               y@lat <= 90
      ) ==TRUE)

  }

  test_5 <- function(x){

    all(
      sapply(x,
             function(y) is.numeric(y@lon) &
               y@lon >= -180 &
               y@lon <= 180

      ) ==TRUE)

  }

  assertthat::on_failure(test_4) <- function(call, env){

    paste0(deparse(call$x), " contains invalid latitude coordinates. Did you supply the a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::on_failure(test_5) <- function(call, env){

    paste0(deparse(call$x), " contains invalid longitude coordinate. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.")

  }

  assertthat::assert_that(test_4(soundscape_list))
  assertthat::assert_that(test_5(soundscape_list))

  # The time zone argument

  test_6 <- function(x){

    all(
      sapply(x,
             function(y) assertthat::is.string(y@tz) &
               (y@tz %in% (OlsonNames()))

      ) ==TRUE)

  }

  assertthat::on_failure(test_6) <- function(call, env){

    paste0(deparse(call$x), " contains invalid timezone objects Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).")

  }

  assertthat::assert_that(test_6(soundscape_list))

  # The index argument

  test_7 <- function(x){

    all(
      sapply(x,
             function(y) assertthat::is.string(y@index) &
               (y@index %in% c("BGN", "PMN", "CVR", "EVN", "ENT", "ACI",
                                                  "OSC", "SPT", "RHZ", "RVT", "RPS", "RNG"))

      ) ==TRUE)


  }

  assertthat::on_failure(test_7) <- function(call, env){

    paste0(deparse(call$x), " contains invalid index objects. Make sure the index argument inside the soundscape object is one of the available index options. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.")

  }

  assertthat::assert_that(test_7(soundscape_list))

  # The samplerate and window arguments

  test_8 <- function(x){

    all(
      sapply(x,
             function(y) assertthat::is.count(y@samplerate)

      ) ==TRUE)

  }

  test_9 <- function(x){

    all(
      sapply(x,
             function(y) assertthat::is.count(y@window)

      ) ==TRUE)

  }

  assertthat::on_failure(test_8) <- function(call, env){

    paste0(deparse(call$x), " contains samplerate arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::on_failure(test_9) <- function(call, env){

    paste0(deparse(call$x), " contains window arguments that are not a single positive integer. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the samplerate and window arguments.")

  }

  assertthat::assert_that(test_8(soundscape_list))
  assertthat::assert_that(test_9(soundscape_list))

  # The binarization_method argument

  test_10 <- function(x){

    all(
      sapply(x,
             function(y) assertthat::is.string(y@binarization_method) &
               (y@binarization_method %in% c("IJDefault", "Huang", "Huang2", "Intermodes", "IsoData", "Li","MaxEntropy", "Mean", "MinErrorI", "Minimum", "Moments", "Otsu","Percentile", "RenyiEntropy", "Shanbhag", "Triangle", "Yen","Mode", "custom"))

      ) ==TRUE)

  }



  assertthat::on_failure(test_10) <- function(call, env){

    paste0(deparse(call$x), " contains binarization_method arguments that are not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.")

  }

  assertthat::assert_that(test_10(soundscape_list))

  # The threshold argument

  test_11 <- function(x){

    all(
      sapply(x,
             function(y) all(length(y@threshold) == 1 &
                   is.double(y@threshold) & !is.na(y@threshold))

      ) ==TRUE)

  }

  assertthat::on_failure(test_11) <- function(call, env){

    paste0(deparse(call$x), " contains threshold arguments that are not a single numeric value. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_binarize() and ss_aggregate() functions, and pay special attention to the value argument is you're supplying a custom threshold value.")

  }

  assertthat::assert_that(test_11(soundscape_list))

  # The output argument

  test_12 <- function(x){

    all(
      sapply(x,
             function(y) all(length(y@output) == 1 &
                               is.character(y@output) &
                               (y@output %in% c("incidence_freq", "raw")))

      ) ==TRUE)


  }

  assertthat::on_failure(test_12) <- function(call, env){

    paste0(deparse(call$x), " contains output arguments that are not a character string describing one of the available output options. Did you supply a list of soundscapes produced using the ss_aggregate function? If so, something has gone wrong, please re-run the ss_aggregate() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.")

  }

  assertthat::assert_that(test_12(soundscape_list))

  # The merged_df argument

  test_13 <- function(x){

    all(
      sapply(x,
             function(y) is.data.frame(y@merged_df) &
               assertthat::not_empty(y@merged_df) &
               assertthat::noNA(y@merged_df) &
               all(apply(y@merged_df, 2, function(y) all(is.numeric(y))))

      ) ==TRUE)

  }

  test_14 <- function(x){

    all(
      sapply(x,
             function(y) (abs(as.numeric(rownames(y@merged_df)[1]))+
                            abs(as.numeric(rownames(y@merged_df)[2])))>3 &
               min(as.numeric(rownames(y@merged_df))) >= 0 &
               max(as.numeric(rownames(y@merged_df)))<= y@samplerate/2

      ) ==TRUE)

  }

  test_15 <- function(x){

    all(
      sapply(x,
             function(y) !any(sapply(try(
               as.POSIXct(
                 paste0(substr(y@first_day, 1, 12)," ", colnames(y@merged_df)),
                 tz = y@tz,
                 format="%Y-%m-%d %H:%M:%S"),
               silent = TRUE), function(z) is.na(z)))

      ) ==TRUE)

  }


  assertthat::on_failure(test_13) <- function(call, env){

    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df data frames. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply a list of soundscapes produced using the ss_aggregate() function? If so, something has gone wrong, please re-run the ss_aggregate() function.")

  }

  assertthat::on_failure(test_14) <- function(call, env){

    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.")

  }


  assertthat::on_failure(test_15) <- function(call, env){

    paste0(deparse(call$x), " contains invalid merged_df, binarized_df, or aggregated_df column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of ss_aggregate(). Make sure you're supplying the dataframe produced by the ss_aggregate() function.")

  }


  assertthat::assert_that(test_13(soundscape_list))
  assertthat::assert_that(test_14(soundscape_list))
  assertthat::assert_that(test_15(soundscape_list))

  # The binarized_df argument

  test_16 <- function(x){

    all(
      sapply(x,
             function(y) min(y@binarized_df) >= 0 &
               max(y@binarized_df) <= 1

      ) ==TRUE)

  }

  assertthat::on_failure(test_16) <- function(call, env){

    paste0(deparse(call$x), " contains binarized_df dataframes with values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() function.")

  }

  assertthat::assert_that(test_13(soundscape_list))
  assertthat::assert_that(test_14(soundscape_list))
  assertthat::assert_that(test_15(soundscape_list))
  assertthat::assert_that(test_16(soundscape_list))

  # The aggregated_df argument

  assertthat::assert_that(test_13(soundscape_list))
  assertthat::assert_that(test_14(soundscape_list))
  assertthat::assert_that(test_15(soundscape_list))

  # 0.1.4. Check if the other supplied arguments follow the expected format

  # grouping

  test_17 <- function(x){

    (all(mode(x) %in% c("numeric","character"))) | all(is.na(x))

  }

  test_18 <- function(x){

    (all(length(x) == length(soundscape_list))) | all(is.na(x))

  }

  assertthat::on_failure(test_17) <- function(call, env){

    paste0(deparse(call$x), " is not a vector. Please supply the grouping argument as a vector of group names of the same length as the aggregated_df_list")

  }

  assertthat::on_failure(test_18) <- function(call, env){

    paste0(deparse(call$x), " does not have the same length as soundscape_list. Please supply the grouping argument as a vector of group names of the same length as the aggregated_df_list")

  }

  assertthat::assert_that(test_17(grouping))
  assertthat::assert_that(test_18(grouping))

  # screeplot

  test_19 <- function(x){

    assertthat::is.flag(x)

  }

  assertthat::on_failure(test_19) <- function(call, env){

    paste0(deparse(call$x), " is not a boolean flag. Please supply the argument as one of either TRUE or FALSE, and make sure the argument is not a character string.")

  }

  assertthat::assert_that(test_19(screeplot))

  # 1. Check if ggrepel is installed

  if(nchar(system.file(package='ggrepel'))==0){

    cat("The 'ggrepel' R-package needs to be installed before using this function \n")
    cat("Use: 'install.packages('ggrepel')' to install the package and try again...")
    Sys.sleep(0.00001)
    stop()

  }

  else{


  # 1. Check that a list of soundscapes contain the 'raw' incidences, not the 'incidence_freq'

  if(all(sapply(soundscape_list, function(x) x@output == "raw")) == FALSE){

    wrong_output <- which(sapply(soundscape_list,
                                 function(x) x@output == "raw") == FALSE)

    for (i in 1:length(wrong_output)){

      soundscape_list[[wrong_output[i]]]@aggregated_df <- soundscape_list[[wrong_output[i]]]@aggregated_df * soundscape_list[[wrong_output[i]]]@effort_per_time[1]
      soundscape_list[[wrong_output[i]]]@output <- "raw"

    }

  }

  # 2. Create a site-by-OSU matrix of OSU incidence frequencies

site_by_OSU_matrix <-
  as.data.frame(
    do.call(what = rbind,
            args = lapply(soundscape_list,
                          function(x) unlist(x@aggregated_df))))

colnames(site_by_OSU_matrix) <- paste0("OSU_", seq(1, ncol(site_by_OSU_matrix), 1))

  #3. Remove the OSUs that have zero-detection across all soundscapes

site_by_OSU_matrix <- site_by_OSU_matrix[, which(!colSums(site_by_OSU_matrix) == 0)]

  # 4. Calculate the Bray-Curtis distances between the soundscapes

soundscape_dist <- vegan::vegdist(site_by_OSU_matrix,  method = "bray")

  # 5. Calculate the PCoA values for the soundscapes

soundscape_pcoa <- stats::cmdscale(soundscape_dist,
                                   k = 2,
                                   eig = TRUE,
                                   add = TRUE)


soundscape_pcoa_points <- soundscape_pcoa$points
colnames(soundscape_pcoa_points) <- c("pcoa1", "pcoa2")

soundscape_pcoa_points <-
  soundscape_pcoa_points %>%
  tidyr::as_tibble(rownames = "samples") %>%
  dplyr::mutate(site = rownames(soundscape_pcoa_points))

soundscape_pcoa_percent_explained <- (soundscape_pcoa$eig / sum(soundscape_pcoa$eig))*100

  # 6. Plotting the PCoA plot

if(any(is.na(grouping))){}

else{

  soundscape_pcoa_points$factor <- as.character(grouping)

}


if(any(is.na(grouping))){

  soundscape_pcoa_plot <-

    soundscape_pcoa_points %>%

    ggplot2::ggplot(ggplot2::aes(pcoa1, pcoa2, label = site))+
    ggplot2::geom_point(shape = 21, stroke = 1, size = 4)+
    ggplot2::labs(x = paste0("PCo 1 (", format(round(soundscape_pcoa_percent_explained[1], 1),
                                      nsmall = 1,
                                      trim = TRUE), "%)"),
         y = paste0("PCo 2 (", format(round(soundscape_pcoa_percent_explained[2], 1),
                                      nsmall = 1,
                                      trim = TRUE), "%)")) +

    ggplot2::scale_x_continuous(expand = c(0.1, 0.1))+
    ggplot2::scale_y_continuous(expand = c(0.1, 0.1))+
    ggrepel::geom_text_repel(ggplot2::aes(label = site),
                    box.padding = 0.5,
                    segment.color = NA,
                    size = 5)+

    ggplot2::theme_classic() +

    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = 16,
                                  margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = ggplot2::element_text(size = 16,
                                  margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
      axis.text = ggplot2::element_text(size = 14),
      plot.margin = grid::unit(c(1,1,1,1), "cm")
    )

}


else{

  soundscape_pcoa_plot <-

    soundscape_pcoa_points %>%
    ggplot2::ggplot(ggplot2::aes(pcoa1, pcoa2, label = site))+
    ggplot2::geom_point(ggplot2::aes(fill = factor),
               shape = 21, stroke = 1, size = 4)+
    ggplot2::labs(x = paste0("PCo 1 (", format(round(soundscape_pcoa_percent_explained[1], 1),
                                      nsmall = 1,
                                      trim = TRUE), "%)"),
         y = paste0("PCo 2 (", format(round(soundscape_pcoa_percent_explained[2], 1),
                                      nsmall = 1,
                                      trim = TRUE), "%)")) +

    ggplot2::scale_x_continuous(expand = c(0.1, 0.1))+
    ggplot2::scale_y_continuous(expand = c(0.1, 0.1))+
    ggrepel::geom_text_repel(ggplot2::aes(label = site),
                    box.padding = 0.5,
                    segment.color = NA,
                    size = 5)+

    ggplot2::theme_classic() +

    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = 16,
                                  margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = ggplot2::element_text(size = 16,
                                  margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
      axis.text = ggplot2::element_text(size = 14),
      plot.margin = grid::unit(c(1,1,1,1), "cm"),
      legend.position = "none") +

    viridis::scale_fill_viridis(discrete = TRUE,
                                begin = 0.2,
                                end = 0.8,
                                option = "B")

}

if(screeplot == TRUE){

  soundscape_screeplot <-

    tidyr::tibble(pe = cumsum(soundscape_pcoa_percent_explained),
           axis = 1:length(soundscape_pcoa_percent_explained)) %>%
    ggplot2::ggplot(ggplot2::aes(axis, pe))+
    ggplot2::geom_line(size = 1) +
    ggplot2::geom_point(shape = 21,
               stroke = 1,
               size = 3,
               fill = "white",
               color = "black")+
    ggplot2::theme_classic() +

    ggplot2::ylab("Cumulative percentage variation explained")+
    ggplot2::xlab("Number of principle coordinate axes")+

    ggplot2::theme(
      axis.title.y = ggplot2::element_text(size = 16,
                                  margin = ggplot2::margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = ggplot2::element_text(size = 16,
                                  margin = ggplot2::margin(t = 20, r = 0, b = 0, l = 0)),
      axis.text = ggplot2::element_text(size = 14),
      plot.margin = grid::unit(c(1,1,1,1), "cm"),
      legend.position = "none") +

    ggplot2::scale_y_continuous(limits = c(0, 101))

  requireNamespace("patchwork")

  soundscape_pcoa_plot_combined <- soundscape_pcoa_plot +
    soundscape_screeplot +
    patchwork::plot_layout(nrow = 1)

  return(soundscape_pcoa_plot_combined)

} else{return(soundscape_pcoa_plot)}

}

}

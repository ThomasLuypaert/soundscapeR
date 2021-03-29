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

# 2. Start testing the sounddiv function

# 2.0. If required arguments are missing

testthat::test_that("the sounddiv function provides the correct error when the aggregate_list argument is missing", {

  testthat::expect_error(
    object = sounddiv(qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

testthat::test_that("the sounddiv function provides the correct error when the qvalue argument is missing", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "qvalue argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

testthat::test_that("the sounddiv function provides the correct error when the date argument is missing", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "date argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

testthat::test_that("the sounddiv function provides the correct error when the lat argument is missing", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lon = -59.48937990402315),
    regexp = "lat argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

testthat::test_that("the sounddiv function provides the correct error when the lon argument is missing", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629),
    regexp = "lon argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

  # 2.1. When all the correct arguments are supplied

    # 2.1.1. freqseq = FALSE, output = "percentage" and subset = total

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "total",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "percentage")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(all(function_var >= 0 & function_var <= 100))

})

    # 2.1.2. freqseq = FALSE, output = "percentage" and subset = day

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "day",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "percentage")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(all(function_var >= 0 & function_var <= 100))

})

    # 2.1.3. freqseq = FALSE, output = "percentage" and subset = night

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "night",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "percentage")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(all(function_var >= 0 & function_var <= 100))

})

    # 2.1.4. freqseq = FALSE, output = "percentage" and subset = dawn

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "dawn",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "percentage")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(all(function_var >= 0 & function_var <= 100))

})

    # 2.1.5. freqseq = FALSE, output = "percentage" and subset = dusk

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "dusk",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "percentage")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(all(function_var >= 0 & function_var <= 100))

})

    # 2.1.6. freqseq = FALSE, output = "percentage" and subset = tod

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "tod",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "percentage")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==length(unique(colnames(aggregated_df_CVR[[3]]))))
  testthat::expect_true(is.numeric(function_var[,1]))
  testthat::expect_true(all(function_var[,1] >= 0 & function_var[,1] <= 100))
  testthat::expect_true(hms::is_hms(function_var[,2]))

})

    # 2.1.7. freqseq = FALSE, output = "raw" and subset = total

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "total",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "raw")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(function_var <= dim(
    aggregated_df_CVR[[3]])[1] * dim(aggregated_df_CVR[[3]])[2])

})

    # 2.1.8. freqseq = FALSE, output = "raw" and subset = day

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "day",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "raw")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(function_var <= dim(
    aggregated_df_CVR[[3]])[1] * dim(aggregated_df_CVR[[3]])[2])

})

    # 2.1.9. freqseq = FALSE, output = "raw" and subset = night

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "night",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "raw")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(function_var <= dim(
    aggregated_df_CVR[[3]])[1] * dim(aggregated_df_CVR[[3]])[2])

})

    # 2.1.10. freqseq = FALSE, output = "raw" and subset = dawn

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "dawn",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "raw")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(function_var <= dim(
    aggregated_df_CVR[[3]])[1] * dim(aggregated_df_CVR[[3]])[2])

})

    # 2.1.11. freqseq = FALSE, output = "raw" and subset = dusk

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "dusk",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "raw")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(function_var <= dim(
    aggregated_df_CVR[[3]])[1] * dim(aggregated_df_CVR[[3]])[2])

})

    # 2.1.12. freqseq = FALSE, output = "raw" and subset = tod

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "tod",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "raw")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==length(unique(colnames(aggregated_df_CVR[[3]]))))
  testthat::expect_true(is.numeric(function_var[,1]))
  testthat::expect_true(all(function_var[,1] <= nrow(aggregated_df_CVR[[3]])))
  testthat::expect_true(hms::is_hms(function_var[,2]))

})

    # 2.1.13. freqseq = TRUE, output = "percentage" and subset = total

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "total",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "percentage")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==10)
  testthat::expect_true(is.vector(function_var[,1], mode = "numeric"))
  testthat::expect_true(is.vector(function_var[,2], mode = "character"))
  testthat::expect_true(all(max(function_var[,1]) >= 0
                            & max(function_var[,1]) <= 100))

})

    # 2.1.14. freqseq = TRUE, output = "percentage" and subset = day

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "day",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "percentage")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==10)
  testthat::expect_true(is.vector(function_var[,1], mode = "numeric"))
  testthat::expect_true(is.vector(function_var[,2], mode = "character"))
  testthat::expect_true(all(max(function_var[,1]) >= 0
                            & max(function_var[,1]) <= 100))

})

    # 2.1.15. freqseq = TRUE, output = "percentage" and subset = night

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "night",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "percentage")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==10)
  testthat::expect_true(is.vector(function_var[,1], mode = "numeric"))
  testthat::expect_true(is.vector(function_var[,2], mode = "character"))
  testthat::expect_true(all(max(function_var[,1]) >= 0
                            & max(function_var[,1]) <= 100))

})

    # 2.1.16. freqseq = TRUE, output = "percentage" and subset = dawn

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "dawn",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "percentage")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==10)
  testthat::expect_true(is.vector(function_var[,1], mode = "numeric"))
  testthat::expect_true(is.vector(function_var[,2], mode = "character"))
  testthat::expect_true(all(max(function_var[,1]) >= 0
                            & max(function_var[,1]) <= 100))

})

    # 2.1.17. freqseq = TRUE, output = "percentage" and subset = dusk

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "dusk",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "percentage")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==10)
  testthat::expect_true(is.vector(function_var[,1], mode = "numeric"))
  testthat::expect_true(is.vector(function_var[,2], mode = "character"))
  testthat::expect_true(all(max(function_var[,1]) >= 0
                            & max(function_var[,1]) <= 100))

})

    # 2.1.18. freqseq = TRUE, output = "percentage" and subset = tod

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "tod",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "percentage")

  testthat::expect_true(is.list(function_var))
  testthat::expect_true(length(function_var)==10)

  testthat::expect_true(
    all(
      sapply(function_var, function(x) is.data.frame(x))))

  testthat::expect_true(
    all(
      sapply(function_var, function(x) ncol(x)==2)))

  testthat::expect_true(
    all(
      sapply(function_var, function(x) nrow(x)==length(unique(colnames(aggregated_df_CVR[[3]]))))))

  testthat::expect_true(
    all(
      sapply(function_var, function(x) round(x[,1]) >= 0 & round(x[,1]) <= 100)))

  testthat::expect_true(
    all(
      sapply(function_var, function(x) hms::is_hms(x[,2]))))

})

    # 2.1.19. freqseq = TRUE, output = "raw" and
    # subset = total

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "total",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "raw")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==10)
  testthat::expect_true(is.vector(function_var[,1], mode = "numeric"))
  testthat::expect_true(is.vector(function_var[,2], mode = "character"))

})

    # 2.1.20. freqseq = TRUE, output = "raw" and
    # subset = day

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "day",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "raw")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==10)
  testthat::expect_true(is.vector(function_var[,1], mode = "numeric"))
  testthat::expect_true(is.vector(function_var[,2], mode = "character"))

})

    # 2.1.21. freqseq = TRUE, output = "raw" and subset = night

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "night",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "raw")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==10)
  testthat::expect_true(is.vector(function_var[,1], mode = "numeric"))
  testthat::expect_true(is.vector(function_var[,2], mode = "character"))

})

    # 2.1.22. freqseq = TRUE, output = "raw" and subset = dawn

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "dawn",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "raw")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==10)
  testthat::expect_true(is.vector(function_var[,1], mode = "numeric"))
  testthat::expect_true(is.vector(function_var[,2], mode = "character"))

})

    # 2.1.23. freqseq = TRUE, output = "raw" and subset = dusk

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "dusk",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "raw")

  testthat::expect_true(is.data.frame(function_var))
  testthat::expect_true(ncol(function_var)==2)
  testthat::expect_true(nrow(function_var)==10)
  testthat::expect_true(is.vector(function_var[,1], mode = "numeric"))
  testthat::expect_true(is.vector(function_var[,2], mode = "character"))

})

    # 2.1.24. freqseq = TRUE, output = "raw" and subset = tod

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "tod",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = TRUE,
                           nbins = 10,
                           output = "raw")

  testthat::expect_true(is.list(function_var))
  testthat::expect_true(length(function_var)==10)

  testthat::expect_true(
    all(
      sapply(function_var, function(x) is.data.frame(x))))

  testthat::expect_true(
    all(
      sapply(function_var, function(x) ncol(x)==2)))

  testthat::expect_true(
    all(
      sapply(function_var, function(x) nrow(x)==length(unique(colnames(aggregated_df_CVR[[3]]))))))

  testthat::expect_true(
    all(
      sapply(function_var, function(x) hms::is_hms(x[,2]))))

})

    # 2.1.25. with minfreq, maxfreq, mintime and maxtime

testthat::test_that("the sounddiv function works as expected when the correct input argument are supplied", {

  function_var <- sounddiv(aggregate_list = aggregated_df_CVR,
                           qvalue = 1,
                           subset = "total",
                           date = "2015-09-05",
                           lat = -1.915867928971629,
                           lon = -59.48937990402315,
                           freqseq = FALSE,
                           output = "percentage",
                           minfreq = 500,
                           maxfreq = 10000,
                           mintime = "06:00:00",
                           maxtime = "18:00:00")

  testthat::expect_true(is.numeric(function_var))
  testthat::expect_true(is.vector(function_var))
  testthat::expect_true(length(function_var)==1)
  testthat::expect_true(all(function_var >= 0 & function_var <= 100))

})

  # 2.2. If the wrong aggregate_list argument is supplied

    # 2.2.1. aggregate_list argument is not a list

testthat::test_that("the sounddiv function provides the correct error message when the supplied aggregate_list argument is not a list", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR[[3]],
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list is not a list of the correct length. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.2. aggregate_list[[1]] is not a list

testthat::test_that("the sounddiv function provides the correct error message when the supplied aggregate_list argument's first element is not a list", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregate_df_nolist_1,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list[[1]] is not a list. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.3. aggregate_list[[2]] is not a list

testthat::test_that("the sounddiv function provides the correct error message when the supplied aggregate_list argument's second element is not a list", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregate_df_nolist_2,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list[[2]] is not a list. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.4. aggregate_list[[1]] is an empty list

testthat::test_that("the sounddiv function provides the correct error message when the supplied aggregate_list first element is an empty list", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregate_df_empty_1,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list[[1]] is an empty list. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.5. aggregate_list[[2]] is an empty list

testthat::test_that("the sounddiv function provides the correct error message when the supplied aggregate_list second element is an empty list", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregate_df_empty_2,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list[[2]] is an empty list. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.6. aggregate_list[[3]] is an empty data frame

testthat::test_that("the sounddiv function provides the correct error message when the supplied aggregate_list third element is an empty data frame", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregate_df_empty_3,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list[[3]] is an empty data frame. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.7. aggregate_list[[1]] contains NAs

testthat::test_that("the sounddiv function provides the correct error message when the supplied aggregate_list first element contains NA values", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR_NAs_1,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list[[1]] contains NA values. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.8. aggregate_list[[2]] contains NAs

testthat::test_that("the sounddiv function provides the correct error message when the supplied aggregate_list second element contains NA values", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR_NAs_2,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list[[2]] contains NA values. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.9. aggregate_list[[3]] contains NAs

testthat::test_that("the sounddiv function provides the correct error message when the supplied aggregate_list third element contains NA values", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR_NAs_3,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list[[3]] contains NA values. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

    # 2.2.10. aggregate_list[[3]] is a data frame containing
    # non-numeric values

testthat::test_that("the sounddiv function provides the correct error message when the supplied aggregate_list third element is a data frame containing non-numeric values", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR_nonnum,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "aggregate_list[[3]] contains non-numeric values. This functions builds on the output of aggregate_df(). Make sure you're supplying the output produced by the aggregate_df() function.",
    fixed=TRUE)

})

  # 2.3. The wrong qvalue argument is supplied

    # 2.3.1. The qvalue argument is a character string

testthat::test_that("the sounddiv function provides the correct error message when the supplied qvalue argument is a character string or vector of character strings", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = "1",
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "qvalue is a character string of length 1. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE)

})

    # 2.3.2. The qvalue argument is a list

testthat::test_that("the sounddiv function provides the correct error message when the supplied qvalue argument is not positive", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = as.list(1),
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "qvalue is a list. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE)

})

    # 2.3.3. The qvalue argument is not positive

testthat::test_that("the sounddiv function provides the correct error message when the supplied qvalue argument is not a numeric / integer value.", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = as.factor(1.17),
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "qvalue is not an numeric/integer value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE)

})

    # 2.3.4. The qvalue argument is not positive

testthat::test_that("the sounddiv function provides the correct error message when the supplied qvalue argument is not positive", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = -1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "qvalue is not a positive value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE)

})

  # 2.4. The wrong subset argument is supplied

    # 2.4.1. The subset argument is not a character string

testthat::test_that("the sounddiv function provides the correct error message when the supplied subset argument is not a character string", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      subset = as.factor("total"),
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "subset is not a character string. Please supply the sounddiv subset argument as a character string. Consult package documentation for available subset argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE)

})

    # 2.4.2. The subset argument is not one of the available options

testthat::test_that("the sounddiv function provides the correct error message when the supplied subset argument is not one of the available options", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      subset = "I'm not an option!",
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "subset is not one of the available sounddiv subset options. Please consult package documentation for available subset argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE)

})

  # 2.5. The wrong date argument is supplied

    # 2.5.1. The date argument is not a character string

testthat::test_that("the sounddiv function provides the correct error message when the supplied date argument is not a character string", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = as.factor("2015-09-05"),
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "date is not a character string. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed=TRUE)

})

    # 2.5.2. The date argument is not in the correct format

testthat::test_that("the sounddiv function provides the correct error message when the supplied date argument is not in the correct format", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "05-09-2015",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied date argument is not in the correct format", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "05/09/2015",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315),
    regexp = "date is not a valid date. Please supply the date as a character string using the following format: YYYY-mm-dd. Please consult package documentation for more information.",
    fixed=TRUE)

})

  # 2.6. The wrong lat and lon arguments are supplied

    # 2.6.1. The lat and/or lon are not supplied in decimal degrees

testthat::test_that("the sounddiv function provides the correct error message when the supplied lat argument is not supplied in decimal degrees", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = "1°55'00.1",
                      lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied lon argument is not supplied in decimal degrees", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = "59°28'25.7"),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied lat and lon arguments are not supplied in decimal degrees", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = "1°54'57.2",
                      lon = "59°28'25.7"),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

    # 2.6.2. The lat and lon are numerical, but don't fall within the
    # expected values existing on Earth

testthat::test_that("the sounddiv function provides the correct error message when the supplied lat argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = 91,
                      lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied lat argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -91,
                      lon = -59.48937990402315),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied lon argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = 181),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied lon argument is numeric, but doesn't fit the existing values on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -181),
    regexp = "lon is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied lat and lon argument are numeric, but don't fit the existing values on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = 91,
                      lon = 181),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied lat and lon argument are numeric, but don't fit the existing values on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -91,
                      lon = -181),
    regexp = "lat is not a valid coordinate. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE)

})

  # 2.7. The wrong mintime and maxtime arguments are supplied

    # 2.7.1. The mintime argument is in the wrong format

testthat::test_that("the sounddiv function provides the correct error message when the supplied mintime argument does not fit the expected format", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      mintime = "09-00-00"),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE)

})

    # 2.7.2. The maxtime argument is in the wrong format

testthat::test_that("the sounddiv function provides the correct error message when the supplied maxtime argument does not fit the expected format", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      maxtime = "18-00-00"),
    regexp = "maxtime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE)

})

    # 2.7.3. The mintime and maxtime arguments are in the wrong format

testthat::test_that("the sounddiv function provides the correct error message when the supplied mintime and maxtime arguments don't not fit the expected format", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      mintime = "09-00-00",
                      maxtime = "18-00-00"),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE)

})

  # 2.8. The wrong minfreq and maxfreq arguments are supplied

    # 2.8.1. The minfreq argument is not a single positive integer

testthat::test_that("the sounddiv function provides the correct error message when the supplied minfreq argument is not a single positive integer", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      minfreq = 1.0001),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

    # 2.8.2. The maxfreq argument is not a single positive integer

testthat::test_that("the sounddiv function provides the correct error message when the supplied minfreq argument is not a single positive integer", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      maxfreq = 1.0001),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

    # 2.8.3. The minfreq and maxfreq arguments are not a single
    # positive integers

testthat::test_that("the sounddiv function provides the correct error message when the supplied minfreq and maxfreq arguments are not a single positive integers", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      minfreq = 1.0001,
                      maxfreq = 1.0001),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

    # 2.8.4. The minfreq argument falls outside of the data frame
    # frequency bounds

testthat::test_that("the sounddiv function provides the correct error message when the supplied minfreq argument is lower than the lower frequency boundary of the data frame", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      minfreq = 1),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})


testthat::test_that("the sounddiv function provides the correct error message when the supplied minfreq argument is higher than the upper frequency boundary of the data frame", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      minfreq = 25000),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

    # 2.8.5. The maxfreq argument falls outside of the data frame
    # frequency bounds

testthat::test_that("the sounddiv function provides the correct error message when the supplied maxfreq argument is lower than the lower frequency boundary of the data frame", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      maxfreq = 1),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied maxfreq argument is higher than the upper frequency boundary of the data frame", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      maxfreq = 25000),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

    # 2.8.6. The maxfreq argument is zero

testthat::test_that("the sounddiv function provides the correct error message when the supplied maxfreq argument is zero", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      maxfreq = 0),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE)

})

  # 2.9. The wrong twilight argument is supplied

    # 2.9.1. The twilight argument is not a string

testthat::test_that("the sounddiv function provides the correct error message when the supplied twilight argument is not a character string", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      twilight = as.factor("sunlight")),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE)

})

    # 2.9.2. The twilight argument is not one of the available options

testthat::test_that("the sounddiv function provides the correct error message when the supplied twilight argument is not one of the available options", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      twilight = "I'm not an options!"),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied twilight argument is not one of the available options", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      twilight = as.vector(2.15, mode="integer")),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE)

})

  # 2.10. The wrong dawnstart/dawnend/duskstart/duskend arguments are supplied

    # 2.10.1. The argument is a character string

testthat::test_that("the sounddiv function provides the correct error message when the supplied dawnstart argument is a character string", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      dawnstart = "5400"),
    regexp = "dawnstart is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied dawnend argument is a character string", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      dawnend = "5400"),
    regexp = "dawnend is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied duskstart argument is a character string", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      duskstart = "5400"),
    regexp = "duskstart is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied duskend argument is a character string", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      duskend = "5400"),
    regexp = "duskend is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

    # 2.10.2. The argument is not an integer

testthat::test_that("the sounddiv function provides the correct error message when the supplied dawnstart argument is not an integer", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      dawnstart = 5400.95),
    regexp = "dawnstart is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied dawnend argument is not an integer", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      dawnend = 5400.95),
    regexp = "dawnend is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied duskstart argument is not an integer", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      duskstart = 5400.95),
    regexp = "duskstart is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied duskend argument is not an integer", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      duskend = 5400.95),
    regexp = "duskend is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

    # 2.10.3. The supplied argument is not positive


testthat::test_that("the sounddiv function provides the correct error message when the supplied dawnstart argument is not positive", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      dawnstart = -5400),
    regexp = "dawnstart is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied dawnend argument is not positive", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      dawnend = -5400),
    regexp = "dawnend is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied duskstart argument is not positive", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      duskstart = -5400),
    regexp = "duskstart is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied duskend argument is not positive", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      duskend = -5400),
    regexp = "duskend is not in a valid dawnstart/dawnend/duskstart/duskend argument. The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE)

})

  # 2.11. The wrong freqseq argument is supplied

testthat::test_that("the sounddiv function provides the correct error message when the supplied freqseq argument is not a boolean flag", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      freqseq = "I'm not a boolean flag!"),
    regexp = "freqseq is not a Boolean flag (TRUE or FALSE). Please set the freqseq argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed=TRUE)

})

testthat::test_that("the sounddiv function provides the correct error message when the supplied freqseq argument is not a boolean flag", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      freqseq = "TRUE"),
    regexp = "freqseq is not a Boolean flag (TRUE or FALSE). Please set the freqseq argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed=TRUE)

})

  # 2.11. The wrong nbins argument is supplied

    # 2.11.1. The nbins argument is not a single positive integer

testthat::test_that("the sounddiv function provides the correct error message when the supplied nbins argument is not a single positive integer", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      freqseq = TRUE,
                      nbins = -1),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE)

})

    # 2.11.2. The nbins argument is zero

testthat::test_that("the sounddiv function provides the correct error message when the supplied nbins argument is zero", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      freqseq = TRUE,
                      nbins = 0),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE)

})

    # 2.11.3. The nbins argument is higher than the number of bins (rows)
    # in the data frame

testthat::test_that("the sounddiv function provides the correct error message when the supplied nbins argument is higher than the number of bins (rows) in the data frame", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      freqseq = TRUE,
                      nbins = 100000),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE)

})

  # 2.12. The wrong output argument is supplied

    # 2.12.1. The output argument is not a character string

testthat::test_that("the sounddiv function provides the correct error message when the supplied output argument is not a character string", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      output = as.factor("percentage")),
    regexp = "output is not a character string. Please supply the heatmap type as a character string. Consult package documentation for available output argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE)

})

    # 2.12.2. The output argument is not one of the available options

testthat::test_that("the sounddiv function provides the correct error message when the supplied output argument is not one of the available options", {

  testthat::expect_error(
    object = sounddiv(aggregate_list = aggregated_df_CVR,
                      qvalue = 1,
                      date = "2015-09-05",
                      lat = -1.915867928971629,
                      lon = -59.48937990402315,
                      output = "I'm not an option"),
    regexp = "output is not one of the available sounddiv output options. Please consult package documentation for available output argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE)

})





library(testthat)
library(soundscapeR)

# 1. Load the merged '.csv' data frame files,
# binarize the data frame, aggregate the data frame, and make
#wrong data frame types for testing purposes

fpath_CVR <- system.file("/extdata/merged_soundscape/merged_soundscape_CVR.ssc",
                         package="soundscapeR")

fpath_test <- system.file("/extdata/test_data",
                         package="soundscapeR")

merged_soundscape_CVR <- qs::qread(file = fpath_CVR)
merged_soundscape_CVR@fileloc <- substr(fpath_CVR, 0, nchar(fpath_CVR)-26)

binarized_soundscape_CVR <- binarize_df(merged_soundscape = merged_soundscape_CVR,
                                        method = "Otsu",
                                        value = NULL)

aggregated_soundscape_CVR <- aggregate_df(binarized_soundscape = binarized_soundscape_CVR,
                                          output = "incidence_freq")

aggregated_soundscape_CVR_raw <- aggregate_df(binarized_soundscape = binarized_soundscape_CVR,
                                          output = "raw")

test_no_freqseq <- qs::qread(file = paste0(fpath_test, "/list_no_freqseq.qs"))

test_freqseq <- qs::qread(file = paste0(fpath_test, "/list_freqseq.qs"))

# 2. Start testing the sounddiv function

  # 2.0. If required arguments are missing

testthat::test_that("the sounddiv function provides the correct error when the aggregated_soundscape argument is missing", {

  testthat::expect_error(
    object = sounddiv(qvalue = 1),
    regexp = "aggregated_soundscape argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

testthat::test_that("the sounddiv function provides the correct error when the qvalue argument is missing", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_CVR),
    regexp = "qvalue argument is missing. Please supply the missing argument.",
    fixed = TRUE)

})

  # 2.1. If the correct arguments are supplied

    # 2.1.1. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'total' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                          qvalue = 0,
                          output = "percentage")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[1]][1]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                          qvalue = 1,
                          output = "percentage")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[1]][2]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                          qvalue = 2,
                          output = "percentage")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[1]][3]))

})

    # 2.1.2. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'total' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                          qvalue = 0,
                          output = "raw")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[1]][4]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                          qvalue = 1,
                          output = "raw")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[1]][5]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                          qvalue = 2,
                          output = "raw")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[1]][6]))

})

    # 2.1.3. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'tod' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                          qvalue = 0,
                          output = "percentage",
                          subset = "tod")

  testthat::expect_equal(sounddiv, test_no_freqseq[[2]][[1]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "tod")

  testthat::expect_equal(sounddiv, test_no_freqseq[[2]][[2]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "tod")

  testthat::expect_equal(sounddiv, test_no_freqseq[[2]][[3]])

})

    # 2.1.4. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'tod' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "tod")

  testthat::expect_equal(sounddiv, test_no_freqseq[[2]][[4]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "tod")

  testthat::expect_equal(sounddiv, test_no_freqseq[[2]][[5]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "tod")

  testthat::expect_equal(sounddiv, test_no_freqseq[[2]][[6]])

})

    # 2.1.5. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'day' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "day")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[3]][[1]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "day")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[3]][[2]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "day")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[3]][[3]]))

})

    # 2.1.6. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'day' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "day")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[3]][[4]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "day")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[3]][[5]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "day")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[3]][[6]]))

})

    # 2.1.7. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'night' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "night")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[4]][[1]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "night")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[4]][[2]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "night")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[4]][[3]]))

})

    # 2.1.8. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'night' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "night")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[4]][[4]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "night")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[4]][[5]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "night")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[4]][[6]]))

})

    # 2.1.9. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'dawn' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "dawn")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[5]][[1]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "dawn")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[5]][[2]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "dawn")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[5]][[3]]))

})

    # 2.1.10. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'dawn' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "dawn")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[5]][[4]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "dawn")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[5]][[5]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "dawn")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[5]][[6]]))

})

    # 2.1.11. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'dusk' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "dusk")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[6]][[1]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "dusk")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[6]][[2]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "dusk")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[6]][[3]]))

})

    # 2.1.12. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'dusk' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "dusk")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[6]][[4]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "dusk")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[6]][[5]]))

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "dusk")

  testthat::expect_equal(sounddiv, as.double(test_no_freqseq[[6]][[6]]))

})

    # 2.1.13. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'total' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "total",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[1]][[1]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "total",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[1]][[2]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "total",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[1]][[3]])

})

    # 2.1.14. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'total' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "total",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[1]][[4]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "total",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[1]][[5]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "total",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[1]][[6]])

})

    # 2.1.15. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'tod' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "tod",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[2]][[1]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "tod",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[2]][[2]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "tod",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[2]][[3]])

})

    # 2.1.16. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'tod' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "tod",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[2]][[4]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "tod",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[2]][[5]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "tod",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[2]][[6]])

})

    # 2.1.17. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'day' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "day",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[3]][[1]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "day",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[3]][[2]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "day",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[3]][[3]])

})

    # 2.1.18. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'day' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "day",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[3]][[4]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "day",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[3]][[5]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "day",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[3]][[6]])

})

    # 2.1.19. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'night' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "night",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[4]][[1]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "night",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[4]][[2]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "night",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[4]][[3]])

})

    # 2.1.20. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'night' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "night",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[4]][[4]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "night",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[4]][[5]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "night",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[4]][[6]])

})

    # 2.1.21. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'dawn' AND
    # output = "percentage"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "dawn",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[5]][[1]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "dawn",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[5]][[2]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "dawn",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[5]][[3]])

})

    # 2.1.22. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'dawn' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "dawn",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[5]][[4]])

})


testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "dawn",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[5]][[5]])

})


testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "dawn",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[5]][[6]])

})

    # 2.1.23. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'dusk' AND
    # output = "percentage"


testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "dusk",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[6]][[1]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "dusk",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[6]][[2]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "dusk",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[6]][[3]])

})

    # 2.1.24. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'dusk' AND
    # output = "raw"

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "dusk",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[6]][[4]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "dusk",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[6]][[5]])

})

testthat::test_that("the sounddiv function works as expected when the correct arguments are supplied", {

  sounddiv <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "dusk",
                       freqseq = TRUE)

  testthat::expect_equal(sounddiv, test_freqseq[[6]][[6]])

})

  # 2.2. When the supplied aggregated_soundscape argument is wrong

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape argument is not an S4-object of the type 'soundscape'", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_CVR@merged_df,
                      qvalue = 0),
    regexp = "aggregated_soundscape is not an S4-object of the type 'soundscape', or is empty. Please supply the aggregated_soundscape object produced by the aggregate_df() function. Consult the package documentation for further information.",
    fixed=TRUE
  )

})

  # 2.3. When the aggregated_soundscape elements are wrong

    # 2.3.1. The lat and lon arguments are wrong

aggregated_soundscape_coord_1 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_2 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_3 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_4 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_5 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_6 <- aggregated_soundscape_CVR
aggregated_soundscape_coord_1@lat <- 91
aggregated_soundscape_coord_2@lat <- -91
aggregated_soundscape_coord_3@lon <- 181
aggregated_soundscape_coord_4@lon <- -181
aggregated_soundscape_coord_5@lat <- 91
aggregated_soundscape_coord_5@lon <- 181
aggregated_soundscape_coord_6@lat <- -91
aggregated_soundscape_coord_6@lon <- -181

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_coord_1,
                      qvalue = 0),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_coord_2,
                      qvalue = 0),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_coord_3,
                      qvalue = 0),
    regexp = "aggregated_soundscape@lon is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_coord_4,
                      qvalue = 0),
    regexp = "aggregated_soundscape@lon is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_coord_5,
                      qvalue = 0),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape lat and lon argument don't match existing coordinates on Earth", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_coord_6,
                      qvalue = 0),
    regexp = "aggregated_soundscape@lat is not a valid coordinate. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed=TRUE
  )

})

   # 2.3.2. When the tz argument is wrong

aggregated_soundscape_tz <- aggregated_soundscape_CVR
aggregated_soundscape_tz@tz <- "Emarica/Manaus"

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape tz argument is wrong", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_tz,
                      qvalue = 0),
    regexp = "aggregated_soundscape@tz is not a recognized timezone. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed=TRUE
  )

})

    # 2.3.3. When the fileloc argument is wrong

# aggregated_soundscape_fileloc <- aggregated_soundscape_CVR
# aggregated_soundscape_fileloc@fileloc <- paste0(getwd(), "/IDontExist")
#
# testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape fileloc argument is wrong", {
#
#   testthat::expect_error(
#     object = sounddiv(aggregated_soundscape = aggregated_soundscape_fileloc,
#                       qvalue = 0),
#     regexp = paste0("Path ",
#                     paste0("'", getwd(), "/IDontExist", "'"),
#                     " does not exist"),
#     fixed=TRUE
#   )
#
# })

    # 2.3.4. When the index argument is wrong

aggregated_soundscape_index <- aggregated_soundscape_CVR
aggregated_soundscape_index@index <- "I'm not an option!"

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape index argument is wrong", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_index,
                      qvalue = 0),
    regexp = "aggregated_soundscape@index is not a character string of one of the available index options. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed=TRUE
  )

})

    # 2.3.5. When the samplerate argument is wrong

aggregated_soundscape_samplerate1 <- aggregated_soundscape_CVR
aggregated_soundscape_samplerate2 <- aggregated_soundscape_CVR
aggregated_soundscape_samplerate1@samplerate <- -44100
aggregated_soundscape_samplerate2@samplerate <- c(44100, 44200)

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape samplerate argument is wrong", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_samplerate1,
                      qvalue = 0),
    regexp = "aggregated_soundscape@samplerate is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape samplerate argument is wrong", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_samplerate2,
                      qvalue = 0),
    regexp = "aggregated_soundscape@samplerate is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

    # 2.3.6. When the window argument is wrong

aggregated_soundscape_window1 <- aggregated_soundscape_CVR
aggregated_soundscape_window2 <- aggregated_soundscape_CVR
aggregated_soundscape_window1@window <- -256
aggregated_soundscape_window2@window <- c(256, 512)

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape window argument is wrong", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_window1,
                      qvalue = 0),
    regexp = "aggregated_soundscape@window is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape window argument is wrong", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_window2,
                      qvalue = 0),
    regexp = "aggregated_soundscape@window is not a single positive integer. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the samplerate and window arguments.",
    fixed=TRUE
  )

})

    # 2.3.7. When the binarization_method argument is wrong

aggregated_soundscape_binmeth1 <- aggregated_soundscape_CVR
aggregated_soundscape_binmeth1@binarization_method <- "I'm not an option!"

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape window argument is wrong", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_binmeth1,
                      qvalue = 0),
    regexp = "aggregated_soundscape@binarization_method is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

    # 2.3.8. When the threshold argument is wrong

aggregated_soundscape_thresh1 <- aggregated_soundscape_CVR
aggregated_soundscape_thresh1@threshold <- c(1.5, 1.6)

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape threshold argument is wrong", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_thresh1,
                      qvalue = 0),
    regexp = "aggregated_soundscape@threshold is not a single numeric value. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the binarize_df() and aggregate_df() functions, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed=TRUE
  )

})

    # 2.3.9. When the output argument is wrong

aggregated_soundscape_output1 <- aggregated_soundscape_CVR
aggregated_soundscape_output2 <- aggregated_soundscape_CVR
aggregated_soundscape_output1@output <- "I'm not  an option"
aggregated_soundscape_output2@output <- c("raw", "incidence_freq")

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape output argument is wrong", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_output1,
                      qvalue = 0),
    regexp = "aggregated_soundscape@output is not a character string describing one of the available output options. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape output argument is wrong", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_output2,
                      qvalue = 0),
    regexp = "aggregated_soundscape@output is not a character string describing one of the available output options. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed=TRUE
  )

})

    # 2.3.10. The merged_df argument is wrong

aggregated_soundscape_merged_df1 <- aggregated_soundscape_CVR
aggregated_soundscape_merged_df2 <- aggregated_soundscape_CVR
aggregated_soundscape_merged_df3 <- aggregated_soundscape_CVR
aggregated_soundscape_merged_df4 <- aggregated_soundscape_CVR
aggregated_soundscape_merged_df5 <- aggregated_soundscape_CVR
aggregated_soundscape_merged_df1@merged_df <- aggregated_soundscape_merged_df1@merged_df[FALSE,]
aggregated_soundscape_merged_df2@merged_df[1,1] <- NA
aggregated_soundscape_merged_df3@merged_df[1,1] <- "I'm not numeric"
rownames(aggregated_soundscape_merged_df4@merged_df) <-
  seq(1,nrow(aggregated_soundscape_merged_df4@merged_df), 1)
colnames(aggregated_soundscape_merged_df5@merged_df) <-
  seq(1,ncol(aggregated_soundscape_merged_df5@merged_df), 1)

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape merged_df argument is empty", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_merged_df1,
                      qvalue = 0),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape merged_df argument contains NA values", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_merged_df2,
                      qvalue = 0),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape merged_df argument contains non-numeric values", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_merged_df3,
                      qvalue = 0),
    regexp = "aggregated_soundscape@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape merged_df argument has incorrect row names", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_merged_df4,
                      qvalue = 0),
    regexp = "aggregated_soundscape@merged_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape merged_df argument has incorrect column names", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_merged_df5,
                      qvalue = 0),
    regexp = "aggregated_soundscape@merged_df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})

    # 2.3.11. The binarized_df argument is wrong

aggregated_soundscape_bindf1 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf2 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf3 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf4 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf5 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf6 <- aggregated_soundscape_CVR
aggregated_soundscape_bindf1@binarized_df <- aggregated_soundscape_bindf1@binarized_df[FALSE,]
aggregated_soundscape_bindf2@binarized_df[1,1] <- NA
aggregated_soundscape_bindf3@binarized_df[1,1] <- "I'm not numeric"
rownames(aggregated_soundscape_bindf4@binarized_df) <-
  seq(1,nrow(aggregated_soundscape_bindf4@binarized_df), 1)
colnames(aggregated_soundscape_bindf5@binarized_df) <-
  seq(1,ncol(aggregated_soundscape_bindf5@binarized_df), 1)
aggregated_soundscape_bindf6@binarized_df[1,1] <- 25

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape binarized_df argument is empty", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_bindf1,
                      qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape binarized_df argument contains NA values", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_bindf2,
                      qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape binarized_df argument contains non-numeric values", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_bindf3,
                      qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape binarized_df argument has incorrect row names", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_bindf4,
                      qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape binarized_df argument has incorrect column names", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_bindf5,
                      qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape binarized_df argument is non-binary", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_bindf6,
                      qvalue = 0),
    regexp = "aggregated_soundscape@binarized_df has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the aggregate_df() function.",
    fixed=TRUE
  )

})

    # 2.3.12. The aggregated_df argument is wrong

aggregated_soundscape_aggdf1 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf2 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf3 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf4 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf5 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf6 <- aggregated_soundscape_CVR
aggregated_soundscape_aggdf1@aggregated_df <- aggregated_soundscape_aggdf1@aggregated_df[FALSE,]
aggregated_soundscape_aggdf2@aggregated_df[1,1] <- NA
aggregated_soundscape_aggdf3@aggregated_df[1,1] <- "I'm not numeric"
rownames(aggregated_soundscape_aggdf4@aggregated_df) <-
  seq(1,nrow(aggregated_soundscape_aggdf4@aggregated_df), 1)
colnames(aggregated_soundscape_aggdf5@aggregated_df) <-
  seq(1,ncol(aggregated_soundscape_aggdf5@aggregated_df), 1)
aggregated_soundscape_aggdf6@aggregated_df[1,1] <- 25

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape aggregated_df argument is empty", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_aggdf1,
                      qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape aggregated_df argument contains NA values", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_aggdf2,
                      qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape aggregated_df argument contains non-numeric values", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_aggdf3,
                      qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the aggregated_soundscape argument produced using the aggregate_df() function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape aggregated_df argument has incorrect row names", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_aggdf4,
                      qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape aggregated_df argument has incorrect column names", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_aggdf5,
                      qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df does not have the correct column names. Please make sure the column names indicate the time of day expressed as a character string in the following format: HH:MM::SS. This functions builds on the output of aggregate_df(). Make sure you're supplying the dataframe produced by the aggregate_df() function.",
    fixed=TRUE
  )

})


testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape aggregated_df argument contains values outside of the expected range", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape =aggregated_soundscape_aggdf6,
                      qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function, and pay special attention to the output argument.",
    fixed=TRUE
  )

})

    # 2.3.13. The aggregated_df_per_time argument is wrong

aggregated_soundscape_aggtime1 <- aggregated_soundscape_CVR
aggregated_soundscape_aggtime2 <- aggregated_soundscape_CVR
aggregated_soundscape_aggtime3 <- aggregated_soundscape_CVR

aggregated_soundscape_aggtime1@aggregated_df_per_time <-
  as.list(seq(1, length(aggregated_soundscape_CVR@aggregated_df_per_time), 1))

aggregated_soundscape_aggtime2@aggregated_df_per_time <-
  lapply(aggregated_soundscape_CVR@aggregated_df_per_time, function(x) x[,1])

aggregated_soundscape_aggtime3@aggregated_df_per_time <-
  aggregated_soundscape_CVR@aggregated_df_per_time[[1]]

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape aggregated_df_per_time argument does not have the expected values", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_aggtime1,
                      qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df_per_time does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape aggregated_df_per_time argument does not have the expected dimensions", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_aggtime2,
                      qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df_per_time does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape aggregated_df_per_time argument does not have the expected length", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape = aggregated_soundscape_aggtime3,
                      qvalue = 0),
    regexp = "aggregated_soundscape@aggregated_df_per_time does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.",
    fixed=TRUE
  )

})

    # 2.3.14. The effort_per_time argument is wrong

# aggregated_soundscape_efftime <- aggregated_soundscape_CVR
#
# aggregated_soundscape_efftime@effort_per_time <-
#   as.list(seq(1, length(aggregated_soundscape_CVR@effort_per_time), 1))
#
# testthat::test_that("the sounddiv function produces the correct error message when the aggregated_soundscape effort_per_time argument does not have the expected format", {
#
#   testthat::expect_error(
#     object = sounddiv(aggregated_soundscape= aggregated_soundscape_efftime,
#                       qvalue = 0),
#     regexp = "aggregated_soundscape@effort_per_time does not have the expected format. Did you supply the aggregated_soundscape argument produced using the aggregate_df function? If so, something has gone wrong, please re-run the aggregate_df() function.",
#     fixed=TRUE
#   )
#
# })

  # 2.4. The qvalues argument is wrong

testthat::test_that("the sounddiv function produces the correct error message when the qvalue argument is a character string", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = "0"),
    regexp = "qvalue is a character string of length 1. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the qvalue argument is a list", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = as.list(c(1, 2, 3))),
    regexp = "qvalue is a list. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the qvalue argument is a list", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = as.factor(1)),
    regexp = "qvalue is not an numeric/integer value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the qvalue argument is a list", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = -1),
    regexp = "qvalue is not a positive value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed=TRUE
  )

})

  # 2.5. The subset argument is wrong

testthat::test_that("the sounddiv function produces the correct error message when the subset argument is not a character string", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = as.factor("total")),
    regexp = "subset is not a character string. Please supply the sounddiv subset argument as a character string. Consult package documentation for available subset argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the subset argument is not one of the available options", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "I'm not an option!"),
    regexp = "subset is not one of the available sounddiv subset options. Please consult package documentation for available subset argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

  # 2.6. The mintime and maxtime arguments are wrong

testthat::test_that("the sounddiv function produces the correct error message when the mintime argument does not follow the expected format", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      mintime = "noon" ),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the mintime argument does not follow the expected format", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      mintime = as.factor("12:00:00") ),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the maxtime argument does not follow the expected format", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      maxtime = "noon" ),
    regexp = "maxtime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the maxtime argument does not follow the expected format", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      maxtime = as.factor("12:00:00") ),
    regexp = "maxtime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the mintime and maxtime arguments don't follow the expected format", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      mintime = as.factor("12:00:00"),
                      maxtime = as.factor("12:00:00")),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed=TRUE
  )

})

  # 2.7. The minfreq and maxfreq arguments are wrong

testthat::test_that("the sounddiv function produces the correct error message when the minfreq argument is a negative number", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      minfreq = -1),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the minfreq argument is larger than the upper frequency bound", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      minfreq = 50000),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the minfreq argument is not numeric", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      minfreq = "1"),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the maxfreq argument is a negative number", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      maxfreq = -1),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the maxfreq argument is larger than the upper frequency bound", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      maxfreq = 50000),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the maxfreq argument is not numeric", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      maxfreq = "1"),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the maxfreq argument is zero", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      maxfreq = 0),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the minfreq and maxfreq argument are not in the correct format", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      minfreq = -1,
                      maxfreq = 0),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed=TRUE
  )

})

  # 2.8. The wrong twilight argument is supplied

testthat::test_that("the sounddiv function produces the correct error message when the twilight argument is not a character string", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      twilight = as.factor("sunlight")),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the twilight argument is not one of the available options", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      twilight = "I'm not an option"),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the twilight argument is not one of the available options", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      twilight = as.vector(2.15, mode="integer")),
    regexp = "twilight is not a valid twilight argument. The twilight argument needs to be either a character string indicating one of the following: none, rim, refraction, sunlight, civil, nautical or astronomical - or a numeric vector of length 1 or 2 indicating the solar elevation angle(s) in degrees (negative if below the horizon). For more information, please consult the soundscapeR and photobiology package documentations.",
    fixed=TRUE
  )

})

  # 2.9. The wrong dawnstart, dawnend, duskstart or duskend arguments are supplied

testthat::test_that("the sounddiv function produces the correct error message when the dawnstart argument is not zero or a single, positive integer", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      dawnstart = -1),
    regexp = "dawnstart is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the dawnstart argument is a character vector", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      dawnstart = "5000"),
    regexp = "dawnstart is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE
  )

})


testthat::test_that("the sounddiv function produces the correct error message when the dawnend argument is not zero or a single, positive integer", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      dawnend = -1),
    regexp = "dawnend is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the dawnend argument is a character vector", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      dawnend = "5000"),
    regexp = "dawnend is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE
  )

})


testthat::test_that("the sounddiv function produces the correct error message when the duskstart argument is not zero or a single, positive integer", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      duskstart = -1),
    regexp = "duskstart is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the duskstart argument is a character vector", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      duskstart = "5000"),
    regexp = "duskstart is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE
  )

})


testthat::test_that("the sounddiv function produces the correct error message when the duskend argument is not zero or a single, positive integer", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      duskend = -1),
    regexp = "duskend is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the duskend argument is a character vector", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      subset = "total",
                      duskend = "5000"),
    regexp = "duskend is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed=TRUE
  )

})

  # 2.10. The wrong freqseq argument is supplied

testthat::test_that("the sounddiv function produces the correct error message when the freqseq argument is not a boolean flag", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      freqseq = "TRUE"),
    regexp = "freqseq is not a Boolean flag (TRUE or FALSE). Please set the freqseq argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed=TRUE
  )

})

  # 2.11. The nbins argument is wrong

testthat::test_that("the sounddiv function produces the correct error message when the nbins argument is a single positive integer", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      freqseq = TRUE,
                      nbins = -1),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the nbins argument is a character vector", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      freqseq = TRUE,
                      nbins = "20"),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the nbins argument is zero", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      freqseq = TRUE,
                      nbins = 0),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the nbins argument is larger than the number of rows in the data frame", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      freqseq = TRUE,
                      nbins = 5000),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed=TRUE
  )

})

  # 2.12. The output argument is wrong

testthat::test_that("the sounddiv function produces the correct error message when the output argument is not a character string", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      output = 62),
    regexp = "output is not a character string. Please supply the output argument as a character string. Consult package documentation for available output argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

testthat::test_that("the sounddiv function produces the correct error message when the output argument is not one of the available options", {

  testthat::expect_error(
    object = sounddiv(aggregated_soundscape= aggregated_soundscape_CVR,
                      qvalue = 0,
                      output = "I'm not an option!"),
    regexp = "output is not one of the available sounddiv output options. Please consult package documentation for available output argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed=TRUE
  )

})

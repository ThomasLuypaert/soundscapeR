library(testthat)
library(soundscapeR)

# 1. Load the merged '.csv' data frame files,
# binarize the data frame, aggregate the data frame, and make
# wrong data frame types for testing purposes

fpath_CVR <- system.file("extdata/ss_binarize_test/merged_soundscape_CVR.ssc",
  package = "soundscapeR"
)

fpath_test <- system.file("extdata/test_data",
  package = "soundscapeR"
)

merged_soundscape_CVR <- qs::qread(file = fpath_CVR)
merged_soundscape_CVR@fileloc <- substr(fpath_CVR, 0, nchar(fpath_CVR) - 26)

binarized_soundscape_CVR <- ss_binarize(
  merged_soundscape = merged_soundscape_CVR,
  method = "IsoData",
  value = NULL
)

soundscape_obj_CVR <- ss_aggregate(
  binarized_soundscape = binarized_soundscape_CVR,
  output = "incidence_freq"
)

soundscape_obj_CVR_raw <- ss_aggregate(
  binarized_soundscape = binarized_soundscape_CVR,
  output = "raw"
)

test_no_freqseq <- qs::qread(file = paste0(fpath_test, "/list_no_freqseq.qs"))

test_freqseq <- qs::qread(file = paste0(fpath_test, "/list_freqseq.qs"))

# 2. Start testing the ss_diversity function

# 2.0. If required arguments are missing

testthat::test_that("the ss_diversity function provides the correct error when the soundscape_obj argument is missing", {
  testthat::expect_error(
    object = ss_diversity(qvalue = 1),
    regexp = "soundscape_obj argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function provides the correct error when the qvalue argument is missing", {
  testthat::expect_error(
    object = ss_diversity(soundscape_obj = soundscape_obj_CVR),
    regexp = "qvalue argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})

# 2.1. If the correct arguments are supplied

# 2.1.1. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'total' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[1]][1]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[1]][2]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[1]][3]))
})

# 2.1.2. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'total' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[1]][4]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[1]][5]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[1]][6]))
})

# 2.1.3. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'tod' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "tod"
  )

  testthat::expect_equal(ss_diversity, test_no_freqseq[[2]][[1]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "tod"
  )

  testthat::expect_equal(ss_diversity, test_no_freqseq[[2]][[2]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "tod"
  )

  testthat::expect_equal(ss_diversity, test_no_freqseq[[2]][[3]])
})

# 2.1.4. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'tod' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "tod"
  )

  testthat::expect_equal(ss_diversity, test_no_freqseq[[2]][[4]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "tod"
  )

  testthat::expect_equal(ss_diversity, test_no_freqseq[[2]][[5]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "tod"
  )

  testthat::expect_equal(ss_diversity, test_no_freqseq[[2]][[6]])
})

# 2.1.5. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'day' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "day"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[3]][[1]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "day"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[3]][[2]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "day"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[3]][[3]]))
})

# 2.1.6. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'day' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "day"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[3]][[4]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "day"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[3]][[5]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "day"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[3]][[6]]))
})

# 2.1.7. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'night' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "night"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[4]][[1]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "night"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[4]][[2]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "night"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[4]][[3]]))
})

# 2.1.8. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'night' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "night"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[4]][[4]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "night"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[4]][[5]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "night"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[4]][[6]]))
})

# 2.1.9. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'dawn' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "dawn"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[5]][[1]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "dawn"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[5]][[2]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "dawn"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[5]][[3]]))
})

# 2.1.10. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'dawn' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "dawn"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[5]][[4]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "dawn"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[5]][[5]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "dawn"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[5]][[6]]))
})

# 2.1.11. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'dusk' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "dusk"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[6]][[1]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "dusk"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[6]][[2]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "dusk"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[6]][[3]]))
})

# 2.1.12. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'dusk' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "dusk"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[6]][[4]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "dusk"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[6]][[5]]))
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "dusk"
  )

  testthat::expect_equal(ss_diversity, as.double(test_no_freqseq[[6]][[6]]))
})

# 2.1.13. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'total' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "total",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[1]][[1]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "total",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[1]][[2]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "total",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[1]][[3]])
})

# 2.1.14. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'total' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "total",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[1]][[4]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "total",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[1]][[5]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "total",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[1]][[6]])
})

# 2.1.15. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'tod' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "tod",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[2]][[1]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "tod",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[2]][[2]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "tod",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[2]][[3]])
})

# 2.1.16. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'tod' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "tod",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[2]][[4]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "tod",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[2]][[5]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "tod",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[2]][[6]])
})

# 2.1.17. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'day' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "day",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[3]][[1]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "day",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[3]][[2]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "day",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[3]][[3]])
})

# 2.1.18. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'day' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "day",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[3]][[4]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "day",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[3]][[5]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "day",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[3]][[6]])
})

# 2.1.19. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'night' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "night",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[4]][[1]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "night",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[4]][[2]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "night",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[4]][[3]])
})

# 2.1.20. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'night' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "night",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[4]][[4]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "night",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[4]][[5]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "night",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[4]][[6]])
})

# 2.1.21. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'dawn' AND
# output = "percentage"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "dawn",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[5]][[1]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "dawn",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[5]][[2]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "dawn",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[5]][[3]])
})

# 2.1.22. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'dawn' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "dawn",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[5]][[4]])
})


testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "dawn",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[5]][[5]])
})


testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "dawn",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[5]][[6]])
})

# 2.1.23. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'dusk' AND
# output = "percentage"


testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "percentage",
    subset = "dusk",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[6]][[1]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "percentage",
    subset = "dusk",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[6]][[2]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "percentage",
    subset = "dusk",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[6]][[3]])
})

# 2.1.24. Freqseq = TRUE, q-value = 0, 1 and 2, subset = 'dusk' AND
# output = "raw"

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 0,
    output = "raw",
    subset = "dusk",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[6]][[4]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 1,
    output = "raw",
    subset = "dusk",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[6]][[5]])
})

testthat::test_that("the ss_diversity function works as expected when the correct arguments are supplied", {
  ss_diversity <- ss_diversity(
    soundscape_obj = soundscape_obj_CVR,
    qvalue = 2,
    output = "raw",
    subset = "dusk",
    freqseq = TRUE
  )

  testthat::expect_equal(ss_diversity, test_freqseq[[6]][[6]])
})

# 2.2. When the supplied soundscape_obj argument is wrong

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj argument is not an S4-object of the type 'soundscape'", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR@merged_df,
      qvalue = 0
    ),
    regexp = "soundscape_obj is not an S4-object of the type 'soundscape'. Please supply the soundscape_obj object produced by the ss_aggregate() or ss_create() functions. Consult the package documentation for further information.",
    fixed = TRUE
  )
})

# 2.3. When the soundscape_obj elements are wrong

# 2.3.1. The lat and lon arguments are wrong

soundscape_obj_coord_1 <- soundscape_obj_CVR
soundscape_obj_coord_2 <- soundscape_obj_CVR
soundscape_obj_coord_3 <- soundscape_obj_CVR
soundscape_obj_coord_4 <- soundscape_obj_CVR
soundscape_obj_coord_5 <- soundscape_obj_CVR
soundscape_obj_coord_6 <- soundscape_obj_CVR
soundscape_obj_coord_1@lat <- 91
soundscape_obj_coord_2@lat <- -91
soundscape_obj_coord_3@lon <- 181
soundscape_obj_coord_4@lon <- -181
soundscape_obj_coord_5@lat <- 91
soundscape_obj_coord_5@lon <- 181
soundscape_obj_coord_6@lat <- -91
soundscape_obj_coord_6@lon <- -181

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_coord_1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_coord_2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_coord_3,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lon is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_coord_4,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lon is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_coord_5,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj lat and lon argument don't match existing coordinates on Earth", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_coord_6,
      qvalue = 0
    ),
    regexp = "soundscape_obj@lat is not a valid coordinate. Did you supply the soundscape_obj produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create function, and pay special attention to the required coordinate format. Make sure you supply numerical decimal coordinates. Latitude values should range between -90 and 90. Longitude values should range between -180 and 180.",
    fixed = TRUE
  )
})

# 2.3.2. When the tz argument is wrong

soundscape_obj_tz <- soundscape_obj_CVR
soundscape_obj_tz@tz <- "Emarica/Manaus"

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj tz argument is wrong", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_tz,
      qvalue = 0
    ),
    regexp = "soundscape_obj@tz is not a recognized timezone. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the required date and coordinate formats (these are used to calculate the time zone).",
    fixed = TRUE
  )
})

# 2.3.4. When the index argument is wrong

soundscape_obj_index <- soundscape_obj_CVR
soundscape_obj_index@index <- "I'm not an option!"

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj index argument is wrong", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_index,
      qvalue = 0
    ),
    regexp = "soundscape_obj@index is not a character string of one of the available index options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the index argument. Supply the index argument as a character string, and consult package documentation for index options.",
    fixed = TRUE
  )
})

# 2.3.5. When the samplerate argument is wrong

soundscape_obj_samplerate1 <- soundscape_obj_CVR
soundscape_obj_samplerate2 <- soundscape_obj_CVR
soundscape_obj_samplerate1@samplerate <- -44100
soundscape_obj_samplerate2@samplerate <- c(44100, 44200)

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj samplerate argument is wrong", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_samplerate1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@samplerate is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj samplerate argument is wrong", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_samplerate2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@samplerate is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

# 2.3.6. When the window argument is wrong

soundscape_obj_window1 <- soundscape_obj_CVR
soundscape_obj_window2 <- soundscape_obj_CVR
soundscape_obj_window1@window <- -256
soundscape_obj_window2@window <- c(256, 512)

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj window argument is wrong", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_window1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@window is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj window argument is wrong", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_window2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@window is not a single positive integer. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the samplerate and window arguments.",
    fixed = TRUE
  )
})

# 2.3.7. When the binarization_method argument is wrong

soundscape_obj_binmeth1 <- soundscape_obj_CVR
soundscape_obj_binmeth1@binarization_method <- "I'm not an option!"

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj window argument is wrong", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_binmeth1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarization_method is not a character string describing one of the available binarization method options. Please consult package documentation for available options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

# 2.3.8. When the threshold argument is wrong

soundscape_obj_thresh1 <- soundscape_obj_CVR
soundscape_obj_thresh1@threshold <- c(1.5, 1.6)

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj threshold argument is wrong", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_thresh1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@threshold is not a single numeric value. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the value argument is you're supplying a custom threshold value.",
    fixed = TRUE
  )
})

# 2.3.9. When the output argument is wrong

soundscape_obj_output1 <- soundscape_obj_CVR
soundscape_obj_output2 <- soundscape_obj_CVR
soundscape_obj_output1@output <- "I'm not  an option"
soundscape_obj_output2@output <- c("raw", "incidence_freq")

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj output argument is wrong", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_output1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@output is not a character string describing one of the available output options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj output argument is wrong", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_output2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@output is not a character string describing one of the available output options. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument. Options are: 'incidence_freq' and 'raw', please supply them to the output argument as a character string.",
    fixed = TRUE
  )
})

# 2.3.10. The merged_df argument is wrong

soundscape_obj_merged_df1 <- soundscape_obj_CVR
soundscape_obj_merged_df2 <- soundscape_obj_CVR
soundscape_obj_merged_df3 <- soundscape_obj_CVR
soundscape_obj_merged_df4 <- soundscape_obj_CVR
soundscape_obj_merged_df5 <- soundscape_obj_CVR
soundscape_obj_merged_df1@merged_df <- soundscape_obj_merged_df1@merged_df[FALSE, ]
soundscape_obj_merged_df2@merged_df[1, 1] <- NA
soundscape_obj_merged_df3@merged_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_merged_df4@merged_df) <-
  seq(1, nrow(soundscape_obj_merged_df4@merged_df), 1)
colnames(soundscape_obj_merged_df5@merged_df) <-
  seq(1, ncol(soundscape_obj_merged_df5@merged_df), 1)

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj merged_df argument is empty", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_merged_df1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj merged_df argument contains NA values", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_merged_df2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj merged_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_merged_df3,
      qvalue = 0
    ),
    regexp = "soundscape_obj@merged_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj merged_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_merged_df4,
      qvalue = 0
    ),
    regexp = "soundscape_obj@merged_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})


# 2.3.11. The binarized_df argument is wrong

soundscape_obj_bindf1 <- soundscape_obj_CVR
soundscape_obj_bindf2 <- soundscape_obj_CVR
soundscape_obj_bindf3 <- soundscape_obj_CVR
soundscape_obj_bindf4 <- soundscape_obj_CVR
soundscape_obj_bindf5 <- soundscape_obj_CVR
soundscape_obj_bindf6 <- soundscape_obj_CVR
soundscape_obj_bindf1@binarized_df <- soundscape_obj_bindf1@binarized_df[FALSE, ]
soundscape_obj_bindf2@binarized_df[1, 1] <- NA
soundscape_obj_bindf3@binarized_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_bindf4@binarized_df) <-
  seq(1, nrow(soundscape_obj_bindf4@binarized_df), 1)
colnames(soundscape_obj_bindf5@binarized_df) <-
  seq(1, ncol(soundscape_obj_bindf5@binarized_df), 1)
soundscape_obj_bindf6@binarized_df[1, 1] <- 25

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj binarized_df argument is empty", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_bindf1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj binarized_df argument contains NA values", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_bindf2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj binarized_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_bindf3,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarized_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj binarized_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_bindf4,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarized_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})



testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj binarized_df argument is non-binary", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_bindf6,
      qvalue = 0
    ),
    regexp = "soundscape_obj@binarized_df has values smaller than 0 or greater than 1. The function expects a binary data frame which is the output of the binarization step using the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

# 2.3.12. The aggregated_df argument is wrong

soundscape_obj_aggdf1 <- soundscape_obj_CVR
soundscape_obj_aggdf2 <- soundscape_obj_CVR
soundscape_obj_aggdf3 <- soundscape_obj_CVR
soundscape_obj_aggdf4 <- soundscape_obj_CVR
soundscape_obj_aggdf5 <- soundscape_obj_CVR
soundscape_obj_aggdf6 <- soundscape_obj_CVR
soundscape_obj_aggdf7 <- soundscape_obj_CVR
soundscape_obj_aggdf1@aggregated_df <- soundscape_obj_aggdf1@aggregated_df[FALSE, ]
soundscape_obj_aggdf2@aggregated_df[1, 1] <- NA
soundscape_obj_aggdf3@aggregated_df[1, 1] <- "I'm not numeric"
rownames(soundscape_obj_aggdf4@aggregated_df) <-
  seq(1, nrow(soundscape_obj_aggdf4@aggregated_df), 1)
colnames(soundscape_obj_aggdf5@aggregated_df) <-
  seq(1, ncol(soundscape_obj_aggdf5@aggregated_df), 1)
soundscape_obj_aggdf6@aggregated_df[1, 1] <- 25

soundscape_obj_aggdf7@aggregated_df[1, 1] <- 500
soundscape_obj_aggdf7@output <- "raw"

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj aggregated_df argument is empty", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_aggdf1,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj aggregated_df argument contains NA values", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_aggdf2,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj aggregated_df argument contains non-numeric values", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_aggdf3,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df is not a valid data frame. It is possible the argument is not a data frame, is empty, or contains NA/non-numeric values. Did you supply the soundscape_obj argument produced using the ss_aggregate() or ss_create() functions? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj aggregated_df argument has incorrect row names", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_aggdf4,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df does not have the correct row names. Please make sure the row names indicate the frequency values. This functions builds on the output of ss_aggregate() or ss_create(). Make sure you're supplying the dataframe produced by the ss_aggregate() or ss_create() functions.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj aggregated_df argument contains values outside of the expected range", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_aggdf6,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df contains values smaller than 0 or larger than 1. The expected range of incidence_freq values ranges between 0-1. Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the soundscape_obj aggregated_df argument contains values outside of the expected range", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_aggdf7,
      qvalue = 0
    ),
    regexp = "soundscape_obj@aggregated_df contains values smaller than zero, or larger than the maximum number of soundscape samples per time. The expected range of raw values ranges between 0 and the maximum number of soundscape samples (24-hour recording days). Did you supply the soundscape_obj argument produced using the ss_aggregate or ss_create function? If so, something has gone wrong, please re-run the ss_aggregate() or ss_create() function, and pay special attention to the output argument.",
    fixed = TRUE
  )
})


# 2.4. The qvalues argument is wrong

testthat::test_that("the ss_diversity function produces the correct error message when the qvalue argument is a character string", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = "0"
    ),
    regexp = "qvalue is a character string of length 1. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the qvalue argument is a list", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = as.list(c(1, 2, 3))
    ),
    regexp = "qvalue is a list. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the qvalue argument is a list", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = as.factor(1)
    ),
    regexp = "qvalue is not an numeric/integer value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the qvalue argument is a list", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = -1
    ),
    regexp = "qvalue is not a positive value. Please supply the qvalue argument as a positive numeric or integer value.",
    fixed = TRUE
  )
})

# 2.5. The subset argument is wrong

testthat::test_that("the ss_diversity function produces the correct error message when the subset argument is not a character string", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = as.factor("total")
    ),
    regexp = "subset is not a character string. Please supply the ss_diversity subset argument as a character string. Consult package documentation for available subset argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the subset argument is not one of the available options", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "I'm not an option!"
    ),
    regexp = "subset is not one of the available ss_diversity subset options. Please consult package documentation for available subset argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

# 2.6. The mintime and maxtime arguments are wrong

testthat::test_that("the ss_diversity function produces the correct error message when the mintime argument does not follow the expected format", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      mintime = "noon"
    ),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the mintime argument does not follow the expected format", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      mintime = as.factor("12:00:00")
    ),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the maxtime argument does not follow the expected format", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      maxtime = "noon"
    ),
    regexp = "maxtime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the maxtime argument does not follow the expected format", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      maxtime = as.factor("12:00:00")
    ),
    regexp = "maxtime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the mintime and maxtime arguments don't follow the expected format", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      mintime = as.factor("12:00:00"),
      maxtime = as.factor("12:00:00")
    ),
    regexp = "mintime is not one a valid format. The mintime or maxtime arguments need to be a character string formatted as %H:%M:%S. Please consult the package documentation for further information.",
    fixed = TRUE
  )
})

# 2.7. The minfreq and maxfreq arguments are wrong

testthat::test_that("the ss_diversity function produces the correct error message when the minfreq argument is a negative number", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      minfreq = -1
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the minfreq argument is larger than the upper frequency bound", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      minfreq = 50000
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the minfreq argument is not numeric", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      minfreq = "1"
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the maxfreq argument is a negative number", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      maxfreq = -1
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the maxfreq argument is larger than the upper frequency bound", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      maxfreq = 50000
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the maxfreq argument is not numeric", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      maxfreq = "1"
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the maxfreq argument is zero", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      maxfreq = 0
    ),
    regexp = "maxfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or a character string set to default. Please provide a maxfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the minfreq and maxfreq argument are not in the correct format", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      minfreq = -1,
      maxfreq = 0
    ),
    regexp = "minfreq is not a single positive integer which falls within the frequency bounds of the provided data frame (minimum frequency < minfreq < maximum frequency), or equals 0. Please provide a minfreq argument which abides by the expected format. For more information, please consult the package documentation.",
    fixed = TRUE
  )
})


# 2.9. The wrong dawnstart, dawnend, duskstart or duskend arguments are supplied

testthat::test_that("the ss_diversity function produces the correct error message when the dawnstart argument is not zero or a single, positive integer", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      dawnstart = -1
    ),
    regexp = "dawnstart is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the dawnstart argument is a character vector", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      dawnstart = "5000"
    ),
    regexp = "dawnstart is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_diversity function produces the correct error message when the dawnend argument is not zero or a single, positive integer", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      dawnend = -1
    ),
    regexp = "dawnend is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the dawnend argument is a character vector", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      dawnend = "5000"
    ),
    regexp = "dawnend is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_diversity function produces the correct error message when the duskstart argument is not zero or a single, positive integer", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      duskstart = -1
    ),
    regexp = "duskstart is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the duskstart argument is a character vector", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      duskstart = "5000"
    ),
    regexp = "duskstart is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed = TRUE
  )
})


testthat::test_that("the ss_diversity function produces the correct error message when the duskend argument is not zero or a single, positive integer", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      duskend = -1
    ),
    regexp = "duskend is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the duskend argument is a character vector", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      subset = "total",
      duskend = "5000"
    ),
    regexp = "duskend is not a valid dawnstart/dawnend/duskstart/duskend format The dawn and dusk timing arguments need to be either zero, or a single positive integer. Please consult package documentation for more information.",
    fixed = TRUE
  )
})

# 2.10. The wrong freqseq argument is supplied

testthat::test_that("the ss_diversity function produces the correct error message when the freqseq argument is not a boolean flag", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      freqseq = "TRUE"
    ),
    regexp = "freqseq is not a Boolean flag (TRUE or FALSE). Please set the freqseq argument to TRUE or FALSE. Make sure the argument is not a character string.",
    fixed = TRUE
  )
})

# 2.11. The nbins argument is wrong

testthat::test_that("the ss_diversity function produces the correct error message when the nbins argument is a single positive integer", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      freqseq = TRUE,
      nbins = -1
    ),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the nbins argument is a character vector", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      freqseq = TRUE,
      nbins = "20"
    ),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the nbins argument is zero", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      freqseq = TRUE,
      nbins = 0
    ),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the nbins argument is larger than the number of rows in the data frame", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      freqseq = TRUE,
      nbins = 5000
    ),
    regexp = "nbins is not a valid nbins format. The nbins argument needs to be provided as a single positive integer. The value should range between 1 and the number of rows in the data frame. Please note that the nbins argument works in synergy with the freqseq argument. Make sure freqseq is set to TRUE if you wish to specify the nbins argument.",
    fixed = TRUE
  )
})

# 2.12. The output argument is wrong

testthat::test_that("the ss_diversity function produces the correct error message when the output argument is not a character string", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      output = 62
    ),
    regexp = "output is not a character string. Please supply the output argument as a character string. Consult package documentation for available output argument options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

testthat::test_that("the ss_diversity function produces the correct error message when the output argument is not one of the available options", {
  testthat::expect_error(
    object = ss_diversity(
      soundscape_obj = soundscape_obj_CVR,
      qvalue = 0,
      output = "I'm not an option!"
    ),
    regexp = "output is not one of the available ss_diversity output options. Please consult package documentation for available output argument  options. Make sure the name matches the package documentation, and pay attention to capitals or excess spaces.",
    fixed = TRUE
  )
})

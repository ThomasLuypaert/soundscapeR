library(testthat)
library(soundscapeR)

# 0. Loading required function

with_seed <- function(seed, code) {
  code <- substitute(code)
  orig.seed <- .Random.seed
  on.exit(.Random.seed <<- orig.seed)
  set.seed(seed)
  eval.parent(code)
}

# 1. Load the merged '.csv' data frame files,
# binarize the data frame, aggregate the data frame, and make
# wrong data frame types for testing purposes

fpath_CVR_case_study <- system.file("extdata/case_study/merged_CVR_256_case_study.ssc",
  package = "soundscapeR"
)

merged_soundscape_CVR_case_study <- qs::qread(file = fpath_CVR_case_study)

for (i in 1:length(merged_soundscape_CVR_case_study)) {
  merged_soundscape_CVR_case_study[[i]]@fileloc <- substr(fpath_CVR_case_study, 0, nchar(fpath_CVR_case_study) - 26)
}

binarized_soundscape_CVR_case_study <- lapply(
  merged_soundscape_CVR_case_study,
  function(x) {
    ss_binarize(
      merged_soundscape = x,
      method = "IsoData",
      value = NULL
    )
  }
)

soundscape_obj_CVR_case_study <- lapply(
  binarized_soundscape_CVR_case_study,
  function(x) {
    ss_aggregate(
      binarized_soundscape = x,
      output = "incidence_freq"
    )
  }
)

case_study_groups <- c("A", "B", "B", "B", "B")

# 2. Start testing the ss_pcoa function

# 2.0. If required arguments are missing

testthat::test_that("the ss_pcoa function provides the correct error when the soundscape_list argument is missing", {
  testthat::expect_error(
    object = soundscapeR::ss_pcoa(screeplot = FALSE),
    regexp = "soundscape_list argument is missing. Please supply the missing argument.",
    fixed = TRUE
  )
})


# 2.1. When the correct arguments are supplied

# No grouping without scree plot

with_seed(seed = 1234, code =

 testthat::test_that("The ss_pcao function works as expected when the correct arguments are supplied", {

   vdiffr::expect_doppelganger(
     title = "ss_pcoa_1",
     fig = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study ),
   )
 })

)

# No grouping with scree plot

with_seed(seed = 1234, code =
 testthat::test_that("The ss_pcao function works as expected when the correct arguments are supplied", {

   vdiffr::expect_doppelganger(
     title = "ss_pcoa_2",
     fig = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study,
                                screeplot = TRUE),
   )
 })
)

# Grouping without scree plot

with_seed(seed = 1234, code =
 testthat::test_that("The ss_pcao function works as expected when the correct arguments are supplied", {

   vdiffr::expect_doppelganger(
     title = "ss_pcoa_3",
     fig = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study,
                                screeplot = FALSE,
                                grouping = case_study_groups),
   )
 })
)

# Grouping with scree plot

with_seed(seed = 1234, code =
 testthat::test_that("The ss_pcao function works as expected when the correct arguments are supplied", {

   vdiffr::expect_doppelganger(
     title = "ss_pcoa_4",
     fig = soundscapeR::ss_pcoa(soundscape_list = soundscape_obj_CVR_case_study,
                                screeplot = TRUE,
                                grouping = case_study_groups),
    )
 })
)

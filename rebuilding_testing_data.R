test_no_freqseq <- qs::qread(file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/test_data/list_no_freqseq.qs")

fpath_CVR <- system.file("/extdata/merged_soundscape/merged_soundscape_CVR.ssc",
                         package="soundscapeR")

merged_soundscape_CVR <- qs::qread(file = fpath_CVR)
merged_soundscape_CVR@fileloc <- substr(fpath_CVR, 0, nchar(fpath_CVR)-26)

binarized_soundscape_CVR <- binarize_df(merged_soundscape = merged_soundscape_CVR,
                                        method = "IsoData",
                                        value = NULL)

aggregated_soundscape_CVR <- aggregate_df(binarized_soundscape = binarized_soundscape_CVR,
                                          output = "incidence_freq")

aggregated_soundscape_CVR_raw <- aggregate_df(binarized_soundscape = binarized_soundscape_CVR,
                                              output = "raw")

# Re-creating the "test_no_freqseq" list

test_no_freqseq_new <- vector("list", length=6)

# Test 1 - total soundscape

no_freqseq_total_1_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "total",
                                 output = "percentage")

no_freqseq_total_1_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "total",
                                 output = "percentage")

no_freqseq_total_1_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "total",
                                 output = "percentage")

no_freqseq_total_2_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "total",
                                 output = "raw")

no_freqseq_total_2_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "total",
                                 output = "raw")

no_freqseq_total_2_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "total",
                                 output = "raw")

no_freqseq_total <- as.data.frame(
  cbind(no_freqseq_total_1_1,
        no_freqseq_total_1_2,
        no_freqseq_total_1_3,
        no_freqseq_total_2_1,
        no_freqseq_total_2_2,
        no_freqseq_total_2_3))

colnames(no_freqseq_total) <- c("test_1_1",
                                "test_1_2",
                                "test_1_3",
                                "test_2_1",
                                "test_2_2",
                                "test_2_3")

test_no_freqseq_new[[1]] <- no_freqseq_total

# Test 3 "tod"

no_freqseq_tod_3_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "tod",
                                 output = "percentage")

no_freqseq_tod_3_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "tod",
                                 output = "percentage")

no_freqseq_tod_3_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "tod",
                                 output = "percentage")

no_freqseq_tod_4_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "tod",
                                 output = "raw")

no_freqseq_tod_4_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "tod",
                                 output = "raw")

no_freqseq_tod_4_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "tod",
                                 output = "raw")

no_freqseq_tod <- list(
  no_freqseq_tod_3_1,
  no_freqseq_tod_3_2,
  no_freqseq_tod_3_3,
  no_freqseq_tod_4_1,
  no_freqseq_tod_4_2,
  no_freqseq_tod_4_3)

test_no_freqseq_new[[2]] <- no_freqseq_tod

# Test 5 - day


no_freqseq_day_5_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "day",
                                 output = "percentage")

no_freqseq_day_5_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "day",
                                 output = "percentage")

no_freqseq_day_5_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "day",
                                 output = "percentage")

no_freqseq_day_6_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "day",
                                 output = "raw")

no_freqseq_day_6_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "day",
                                 output = "raw")

no_freqseq_day_6_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "day",
                                 output = "raw")

no_freqseq_day <- as.data.frame(
  cbind(no_freqseq_day_5_1,
        no_freqseq_day_5_2,
        no_freqseq_day_5_3,
        no_freqseq_day_6_1,
        no_freqseq_day_6_2,
        no_freqseq_day_6_3))

colnames(no_freqseq_day) <- c("test_5_1",
                                "test_5_2",
                                "test_5_3",
                                "test_6_1",
                                "test_6_2",
                                "test_6_3")

test_no_freqseq_new[[3]] <- no_freqseq_day

# Test 7 - Night


no_freqseq_night_7_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "night",
                                 output = "percentage")

no_freqseq_night_7_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "night",
                                 output = "percentage")

no_freqseq_night_7_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "night",
                                 output = "percentage")

no_freqseq_night_8_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "night",
                                 output = "raw")

no_freqseq_night_8_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "night",
                                 output = "raw")

no_freqseq_night_8_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "night",
                                 output = "raw")

no_freqseq_night <- as.data.frame(
  cbind(no_freqseq_night_7_1,
        no_freqseq_night_7_2,
        no_freqseq_night_7_3,
        no_freqseq_night_8_1,
        no_freqseq_night_8_2,
        no_freqseq_night_8_3))

colnames(no_freqseq_night) <- c("test_7_1",
                                "test_7_2",
                                "test_7_3",
                                "test_8_1",
                                "test_8_2",
                                "test_8_3")

test_no_freqseq_new[[4]] <- no_freqseq_night

# Test 9 - Dawn


no_freqseq_dawn_9_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "dawn",
                                 output = "percentage")

no_freqseq_dawn_9_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "dawn",
                                 output = "percentage")

no_freqseq_dawn_9_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "dawn",
                                 output = "percentage")

no_freqseq_dawn_10_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "dawn",
                                 output = "raw")

no_freqseq_dawn_10_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "dawn",
                                 output = "raw")

no_freqseq_dawn_10_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "dawn",
                                 output = "raw")

no_freqseq_dawn <- as.data.frame(
  cbind(no_freqseq_dawn_9_1,
        no_freqseq_dawn_9_2,
        no_freqseq_dawn_9_3,
        no_freqseq_dawn_10_1,
        no_freqseq_dawn_10_2,
        no_freqseq_dawn_10_3))

colnames(no_freqseq_dawn) <- c("test_9_1",
                                "test_9_2",
                                "test_9_3",
                                "test_10_1",
                                "test_10_2",
                                "test_10_3")

test_no_freqseq_new[[5]] <- no_freqseq_dawn

# Test 11 - Dusk


no_freqseq_dusk_11_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "dusk",
                                 output = "percentage")

no_freqseq_dusk_11_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "dusk",
                                 output = "percentage")

no_freqseq_dusk_11_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "dusk",
                                 output = "percentage")

no_freqseq_dusk_12_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 0,
                                 subset = "dusk",
                                 output = "raw")

no_freqseq_dusk_12_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 1,
                                 subset = "dusk",
                                 output = "raw")

no_freqseq_dusk_12_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                                 qvalue = 2,
                                 subset = "dusk",
                                 output = "raw")

no_freqseq_dusk <- as.data.frame(
  cbind(no_freqseq_dusk_11_1,
        no_freqseq_dusk_11_2,
        no_freqseq_dusk_11_3,
        no_freqseq_dusk_12_1,
        no_freqseq_dusk_12_2,
        no_freqseq_dusk_12_3))

colnames(no_freqseq_dusk) <- c("test_11_1",
                                "test_11_2",
                                "test_11_3",
                                "test_12_1",
                                "test_12_2",
                                "test_12_3")

test_no_freqseq_new[[6]] <- no_freqseq_dusk

qs::qsave(x = test_no_freqseq_new, file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/test_data/list_no_freqseq.qs")


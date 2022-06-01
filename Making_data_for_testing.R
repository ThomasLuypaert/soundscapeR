############## merged_soundscapes ############

# BGN

merged_soundscape_BGN <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "BGN",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# PMN

merged_soundscape_PMN <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "PMN",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# CVR

merged_soundscape_CVR <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "CVR",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# EVN

merged_soundscape_EVN <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "EVN",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# ENT

merged_soundscape_ENT <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "ENT",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# ACI

merged_soundscape_ACI <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "ACI",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# OSC

merged_soundscape_OSC <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "OSC",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# SPT

merged_soundscape_SPT <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "SPT",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# RHZ

merged_soundscape_RHZ <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "RHZ",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# RVT

merged_soundscape_RVT <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "RVT",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# RPS

merged_soundscape_RPS <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "RPS",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# RNG

merged_soundscape_RNG <- merge_csv(fileloc = "F:/SARs_paper/Abusado",
                                   samplerate = 44100,
                                   window = 256,
                                   index = "RNG",
                                   date = "2015-09-05",
                                   lat = -1.915867928971629,
                                   lon = -59.48937990402315,
                                   twilight = "sunlight")

# Saving

qs::qsave(x = merged_soundscape_BGN,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_BGN.ssc")

qs::qsave(x = merged_soundscape_PMN,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_PMN.ssc")

qs::qsave(x = merged_soundscape_CVR,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_CVR.ssc")

qs::qsave(x = merged_soundscape_EVN,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_EVN.ssc")

qs::qsave(x = merged_soundscape_ENT,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_ENT.ssc")

qs::qsave(x = merged_soundscape_ACI,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_ACI.ssc")

qs::qsave(x = merged_soundscape_OSC,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_OSC.ssc")

qs::qsave(x = merged_soundscape_SPT,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_SPT.ssc")

qs::qsave(x = merged_soundscape_RHZ,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_RHZ.ssc")

qs::qsave(x = merged_soundscape_RVT,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_RVT.ssc")

qs::qsave(x = merged_soundscape_RPS,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_RPS.ssc")

qs::qsave(x = merged_soundscape_RNG,
          file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/merged_soundscape/merged_soundscape_RNG.ssc")



################## test_no_freqseq ########################

nofreqseq <- vector("list", length = 6)

  ## 1. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'total' AND output = "raw"


# Test 1_1

test1_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                               qvalue = 0,
                               output = "percentage")

# Test 1_2

test1_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 1,
                    output = "percentage")

# Test 1_3

test1_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 2,
                    output = "percentage")

# Test 2_1

test2_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 0,
                    output = "raw")

# Test 2_2

test2_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 1,
                    output = "raw")

# Test 2_3

test2_3 <-  sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                               qvalue = 2,
                               output = "raw")

# Saving results in object

nofreqseq[[1]] <- data.frame(test_1_1 = test1_1,
                             test_1_2 = test1_2,
                             test_1_3 = test1_3,
                             test_2_1 = test2_1,
                             test_2_2 = test2_2,
                             test_2_3 = test2_3)

  ## 2. # 2.1.3. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'tod' AND
# output = "percentage"

tod_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 0,
                  output = "percentage",
                  subset = "tod")

tod_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 1,
                  output = "percentage",
                  subset = "tod")

tod_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 2,
                  output = "percentage",
                  subset = "tod")

tod_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 0,
                  output = "raw",
                  subset = "tod")

tod_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
         qvalue = 1,
         output = "raw",
         subset = "tod")

tod_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 2,
                  output = "raw",
                  subset = "tod")

# Saving result in object

tod_total <- list(tod_1,
                  tod_2,
                  tod_3,
                  tod_4,
                  tod_5,
                  tod_6)

names(tod_total) <- c("percentage_q0",
                      "percentage_q1",
                      "percentage_q2",
                      "raw_q0",
                      "raw_q1",
                      "raw_q2")

nofreqseq[[2]] <- tod_total


  ## 3. # 2.1.5. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'day' AND
# output = "percentage"

day_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 0,
                  output = "percentage",
                  subset = "day")

day_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 1,
                  output = "percentage",
                  subset = "day")

day_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 2,
                  output = "percentage",
                  subset = "day")

day_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 0,
                  output = "raw",
                  subset = "day")

day_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 1,
                  output = "raw",
                  subset = "day")

day_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                  qvalue = 2,
                  output = "raw",
                  subset = "day")

# Saving results in object

day_total <- list(day_1,
                  day_2,
                  day_3,
                  day_4,
                  day_5,
                  day_6)

names(day_total) <- c("percentage_q0",
                      "percentage_q1",
                      "percentage_q2",
                      "raw_q0",
                      "raw_q1",
                      "raw_q2")

nofreqseq[[3]] <- day_total

  ## 4. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'night' AND output = "percentage"

night_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 0,
                    output = "percentage",
                    subset = "night")

night_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 1,
                    output = "percentage",
                    subset = "night")

night_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 2,
                    output = "percentage",
                    subset = "night")

night_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 0,
                    output = "raw",
                    subset = "night")

night_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 1,
                    output = "raw",
                    subset = "night")

night_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 2,
                    output = "raw",
                    subset = "night")

# Saving results in object

night_total <- list(night_1,
                    night_2,
                    night_3,
                    night_4,
                    night_5,
                    night_6)

names(night_total) <- c("percentage_q0",
                        "percentage_q1",
                        "percentage_q2",
                        "raw_q0",
                        "raw_q1",
                        "raw_q2")

nofreqseq[[4]] <- night_total

## 5. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'dawn' AND output = "percentage"

dawn_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 0,
                    output = "percentage",
                    subset = "dawn")

dawn_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 1,
                    output = "percentage",
                    subset = "dawn")

dawn_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 2,
                    output = "percentage",
                    subset = "dawn")

dawn_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 0,
                    output = "raw",
                    subset = "dawn")

dawn_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 1,
                    output = "raw",
                    subset = "dawn")

dawn_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 2,
                    output = "raw",
                    subset = "dawn")

# Saving results in object

dawn_total <- list(dawn_1,
                    dawn_2,
                    dawn_3,
                    dawn_4,
                    dawn_5,
                    dawn_6)

names(dawn_total) <- c("percentage_q0",
                        "percentage_q1",
                        "percentage_q2",
                        "raw_q0",
                        "raw_q1",
                        "raw_q2")

nofreqseq[[5]] <- dawn_total

## 6. Freqseq = FALSE, q-value = 0, 1 and 2, subset = 'dusk' AND output = "percentage"

dusk_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 0,
                    output = "percentage",
                    subset = "dusk")

dusk_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 1,
                    output = "percentage",
                    subset = "dusk")

dusk_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 2,
                    output = "percentage",
                    subset = "dusk")

dusk_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 0,
                    output = "raw",
                    subset = "dusk")

dusk_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 1,
                    output = "raw",
                    subset = "dusk")

dusk_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                    qvalue = 2,
                    output = "raw",
                    subset = "dusk")

# Saving results in object

dusk_total <- list(dusk_1,
                    dusk_2,
                    dusk_3,
                    dusk_4,
                    dusk_5,
                    dusk_6)

names(dusk_total) <- c("percentage_q0",
                        "percentage_q1",
                        "percentage_q2",
                        "raw_q0",
                        "raw_q1",
                        "raw_q2")

nofreqseq[[6]] <- dusk_total
names(nofreqseq) <- c("total",
                      "tod",
                      "day",
                      "night",
                      "dawn",
                      "dusk")

qs::qsave(x = nofreqseq, file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/test_data/list_no_freqseq.qs")


######## test_freqseq #####################

  ## 1. Total

freqseq <- vector("list", length=6)

freq_total_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                         qvalue = 0,
                         output = "percentage",
                         subset = "total",
                         freqseq = TRUE)

freq_total_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                         qvalue = 1,
                         output = "percentage",
                         subset = "total",
                         freqseq = TRUE)

freq_total_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                         qvalue = 2,
                         output = "percentage",
                         subset = "total",
                         freqseq = TRUE)

freq_total_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                         qvalue = 0,
                         output = "raw",
                         subset = "total",
                         freqseq = TRUE)

freq_total_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                         qvalue = 1,
                         output = "raw",
                         subset = "total",
                         freqseq = TRUE)

freq_total_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                         qvalue = 2,
                         output = "raw",
                         subset = "total",
                         freqseq = TRUE)

freq_total <- list(freq_total_1,
                   freq_total_2,
                   freq_total_3,
                   freq_total_4,
                   freq_total_5,
                   freq_total_6)

names(freq_total) <- c("percentage_q0",
                       "percentage_q1",
                       "percentage_q2",
                       "raw_q0",
                       "raw_q1",
                       "raw_q2")

freqseq[[1]] <- freq_total

  ## 2. tod

tod_freq_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "tod",
                       freqseq = TRUE)

tod_freq_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "tod",
                       freqseq = TRUE)

tod_freq_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "tod",
                       freqseq = TRUE)

tod_freq_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "tod",
                       freqseq = TRUE)

tod_freq_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "tod",
                       freqseq = TRUE)

tod_freq_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "tod",
                       freqseq = TRUE)

tod_freq_total <- list(tod_freq_1,
                    tod_freq_2,
                    tod_freq_3,
                    tod_freq_4,
                    tod_freq_5,
                    tod_freq_6)

names(tod_freq_total) <- c("percentage_q0",
                           "percentage_q1",
                           "percentage_q2",
                           "raw_q0",
                           "raw_q1",
                           "raw_q2")

freqseq[[2]] <- tod_freq_total


  ## 3. day

day_freq_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "day",
                       freqseq = TRUE)

day_freq_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "day",
                       freqseq = TRUE)

day_freq_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "day",
                       freqseq = TRUE)

day_freq_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "day",
                       freqseq = TRUE)

day_freq_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "day",
                       freqseq = TRUE)

day_freq_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "day",
                       freqseq = TRUE)

day_freq_total <- list(day_freq_1,
                       day_freq_2,
                       day_freq_3,
                       day_freq_4,
                       day_freq_5,
                       day_freq_6)

names(day_freq_total) <- c("percentage_q0",
                           "percentage_q1",
                           "percentage_q2",
                           "raw_q0",
                           "raw_q1",
                           "raw_q2")

freqseq[[3]] <- day_freq_total

## 4. night

night_freq_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "night",
                       freqseq = TRUE)

night_freq_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "night",
                       freqseq = TRUE)

night_freq_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "night",
                       freqseq = TRUE)

night_freq_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "night",
                       freqseq = TRUE)

night_freq_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "night",
                       freqseq = TRUE)

night_freq_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "night",
                       freqseq = TRUE)

night_freq_total <- list(night_freq_1,
                       night_freq_2,
                       night_freq_3,
                       night_freq_4,
                       night_freq_5,
                       night_freq_6)

names(night_freq_total) <- c("percentage_q0",
                           "percentage_q1",
                           "percentage_q2",
                           "raw_q0",
                           "raw_q1",
                           "raw_q2")

freqseq[[4]] <- night_freq_total

## 5. dawn

dawn_freq_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "dawn",
                       freqseq = TRUE)

dawn_freq_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "dawn",
                       freqseq = TRUE)

dawn_freq_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "dawn",
                       freqseq = TRUE)

dawn_freq_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "dawn",
                       freqseq = TRUE)

dawn_freq_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "dawn",
                       freqseq = TRUE)

dawn_freq_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "dawn",
                       freqseq = TRUE)

dawn_freq_total <- list(dawn_freq_1,
                       dawn_freq_2,
                       dawn_freq_3,
                       dawn_freq_4,
                       dawn_freq_5,
                       dawn_freq_6)

names(dawn_freq_total) <- c("percentage_q0",
                           "percentage_q1",
                           "percentage_q2",
                           "raw_q0",
                           "raw_q1",
                           "raw_q2")

freqseq[[5]] <- dawn_freq_total

## 6. dusk

dusk_freq_1 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "percentage",
                       subset = "dusk",
                       freqseq = TRUE)

dusk_freq_2 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "percentage",
                       subset = "dusk",
                       freqseq = TRUE)

dusk_freq_3 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "percentage",
                       subset = "dusk",
                       freqseq = TRUE)

dusk_freq_4 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 0,
                       output = "raw",
                       subset = "dusk",
                       freqseq = TRUE)

dusk_freq_5 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 1,
                       output = "raw",
                       subset = "dusk",
                       freqseq = TRUE)

dusk_freq_6 <- sounddiv(aggregated_soundscape = aggregated_soundscape_CVR,
                       qvalue = 2,
                       output = "raw",
                       subset = "dusk",
                       freqseq = TRUE)

dusk_freq_total <- list(dusk_freq_1,
                       dusk_freq_2,
                       dusk_freq_3,
                       dusk_freq_4,
                       dusk_freq_5,
                       dusk_freq_6)

names(dusk_freq_total) <- c("percentage_q0",
                           "percentage_q1",
                           "percentage_q2",
                           "raw_q0",
                           "raw_q1",
                           "raw_q2")

freqseq[[6]] <- dusk_freq_total

names(freqseq) <- c("total",
                    "tod",
                    "day",
                    "night",
                    "dawn",
                    "dusk")

qs::qsave(x = freqseq, file = "D:/OneDrive - Norwegian University of Life Sciences/Work_computer/PhD Norway/Analysis/R_package/Final/soundscapeR/inst/extdata/test_data/list_freqseq.qs")



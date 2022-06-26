# Create classes for saving the soundscape objects at the various stages
# of the soundscapeR workflow

  # 1. An aggregated_soundscape class

setClass("soundscape",
         representation(first_day="POSIXct",
                        lat="numeric",
                        lon="numeric",
                        tz="character",
                        sunrise="POSIXct",
                        sunset="POSIXct",
                        fileloc="character",
                        index="character",
                        samplerate="numeric",
                        window="numeric",
                        binarization_method = "character",
                        threshold = "numeric",
                        output="character",
                        merged_df="data.frame",
                        binarized_df = "data.frame",
                        split_soundscape = "list",
                        aggregated_df="data.frame",
                        aggregated_df_per_time="list",
                        effort_per_time="list"),
         prototype(first_day = NA_character_,
                   lat = NA_real_,
                   lon = NA_real_,
                   tz = NA_character_,
                   sunrise = NA_character_,
                   sunset = NA_character_,
                   fileloc = NA_character_,
                   index = NA_character_,
                   samplerate = NA_real_,
                   window = NA_real_,
                   binarization_method = NA_character_,
                   threshold = NA_real_,
                   output = NA_character_,
                   merged_df = data.frame(a=NA, b=NA),
                   binarized_df = data.frame(a="missing"),
                   split_soundscape = as.list(rep(NA, 10)),
                   aggregated_df = data.frame(a="missing"),
                   aggregated_df_per_time = as.list(rep(NA, 10)),
                   effort_per_time = as.list(rep(NA, 10))))

setMethod("show",
          "soundscape",
          function(object) {
            cat("\n")
            cat(crayon::bold("1. ",
                             crayon::underline("Soundscape metadata"),
                             "\n"))
            cat("\n")
            cat(crayon::bold("    Sampling point metadata: ", "\n"))
            cat("\n")
            cat("    First day of recording: ",
                as.character(object@first_day),
                "\n")
            cat("    Latitude of sampling point: ", object@lat, "\n")
            cat("    Longitude of sampling point: ", object@lon, "\n")
            cat("    Time zone of sampling point: ", object@tz, "\n")
            cat("    Sunrise time at sampling point: ",
                as.character(hms::as_hms(round(object@sunrise))),
                "\n")
            cat("    Sunset time at sampling point: ",
                as.character(hms::as_hms(round(object@sunset))),
                "\n")
            cat("\n")
            cat(crayon::bold("    Acoustic index metadata: ", "\n"))
            cat("\n")
            cat("    Path to raw sound files: ",
                object@fileloc,
                "\n")
            cat("    Spectral index used: ",
                object@index,
                "\n")
            cat("    Sampling rate of the recording: ",
                object@samplerate,
                " Hz",
                "\n")
            cat("    Window size used in FFT: ",
                object@window,
                " samples",
                "\n")
            cat("    Frequency resolution: ",
                object@samplerate/object@window,
                " Hz",
                "\n")
            cat("    Temporal resolution: ",
                object@window/object@samplerate,
                " ms",
                "\n")
            cat("\n")

            if(object@binarized_df[1, 1]=="missing"){

              cat(crayon::bold("    Workflow update: ",
                               "\n"))
              cat("\n")
              cat(crayon::red("    The workflow goes as follows:", crayon::bold(" Merge -"), "Binarize -", "Aggregate"))

              cat("\n")

              cat(crayon::red("    The binarization step has not yet been performed.",
                  "\n"))

              cat(crayon::red("    It appears your next step is: 'binarize_df()'",
                  "\n"))
            }

            else{

              cat(crayon::bold("    Data frame binarization metadata: ", "\n"))
              cat("\n")
              cat("    Used binarization algorithm: ",
                  object@binarization_method,
                  "\n")
              cat("    Binarization threshold: ",
                  object@threshold,
                  "\n")

            }

            cat("\n")

            if(object@aggregated_df[1, 1]=="missing"){

              if(object@binarized_df[1, 1]=="missing"){
              }

              else{

                cat(crayon::bold("    Workflow update: ",
                                 "\n"))
                cat("\n")
                cat(crayon::red("    The workflow goes as follows:", crayon::bold(" Merge -", "Binarize -"), "Aggregate"))

                cat("\n")

                cat(crayon::red("    The aggregation step has not yet been performed.",
                                "\n"))

                cat(crayon::red("    It appears your next step is: 'aggregate_df()'",
                                "\n"))

              }
            }

            else{

              cat(crayon::bold("    Aggregated data frame metadata: ",
                               "\n"))
              cat("\n")
              cat("    Output format: ", object@output,
                  "\n")
              cat("    Data frame frequency range: ",
                  min(
                    as.numeric(
                      rownames(
                        object@aggregated_df))), "-",
              max(
                as.numeric(
                  rownames(
                    object@aggregated_df))), "Hz",
              "\n")

              cat("    Data frame time range: ",
                  min(
                    as.character(
                      hms::as_hms(
                      colnames(
                        object@aggregated_df)))), "-",
                  max(
                    as.character(hms::as_hms(
                      colnames(
                        object@aggregated_df)))),
                  "\n")

            }

            cat("\n")
            cat(crayon::bold("2. ",
                             crayon::underline("Soundscape data"),
                             "\n"))
            cat("\n")
            cat(crayon::bold("    Merged data frame data: ", "\n"))
            cat("\n")
            cat("Columns 1 to 5 and rows 1 to 5 displayed", "\n")
            cat("\n")
            print(head(object@merged_df)[1:5,1:5])
            cat("\n")

            if(object@binarized_df[1, 1]=="missing"){

              cat(crayon::bold("    Binarized data frame data: ", "\n"))
              cat("\n")

              cat(crayon::red("    The binarization step has not yet been performed.",
                  "\n"))

            }

            else{
              cat(crayon::bold("    Binarized data frame data: ", "\n"))
              cat("\n")
              cat("Columns 1 to 5 and rows 1 to 5 displayed", "\n")
              cat("\n")
              print(head(object@binarized_df)[1:5,1:5])

            }

            cat("\n")
            cat(crayon::bold("    Aggregated data frame data: ", "\n"))
            cat("\n")

            if(object@aggregated_df[1, 1]=="missing"){

              cat(crayon::red("    The aggregation step has not yet been performed."))

            }

            else{

            cat("   ",
                crayon::underline("Aggregated data frame:"),
                "\n")
            cat("\n")
            cat("Columns 1 to 5 and rows 1 to 5 displayed", "\n")
            cat("\n")
            print(head(object@aggregated_df)[1:5,1:5])
            cat("\n")
            cat("   ",
                crayon::underline("Aggregated data frame per time:"),
                "\n")
            cat("\n")
            cat("First list element displayed: ",
                colnames(object@aggregated_df_per_time[[1]])[1],
                "\n")
            cat("\n")
            cat("Columns 1 to 5 and rows 1 to 5 displayed", "\n")
            cat("\n")
            print(object@aggregated_df_per_time[[1]][1:5, 1:5])
            cat("\n")
            cat("   ",
                crayon::underline("Number of soundscape samples per time (sampling effort):"),
                "\n")
            cat("\n")
            cat("List elements 1 to 5 displayed", "\n")
            cat("\n")
            print(object@effort_per_time[1:5])

            }
          }
)

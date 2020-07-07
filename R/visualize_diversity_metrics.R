# Functions for visualizing soundscape diversity by time of day

  #1) Plots showing soundscape diversity by time of day

utils::globalVariables(c("time_of_day", "soundscape_div", "frequency", "freq", "soundscape_div_smooth",
                         "prop_sound_div"))

#' Visualize Soundscape Diversity by Time of Day
#'
#' @description Produces plots showing the variation in soundscape diversity by time-of-day. Soundscape diversity can be shown for the full frequency range, or the relative contribution of frequency-bins with user-specified width.
#'
#' @param df The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param graphtype The type of plot which is produced. \cr
#' \cr
#'Options are:\cr
#'\cr
#'\emph{'total'}:
#'\cr
#'An area chart showing the soundscape diversity by time-of-day for the entire frequency range.\cr
#'\cr
#'\emph{'frequency'}:
#'\cr
#'A stacked area chart showing the relative contribution of frequency bins with user-defined width to the total soundscape diversity by time-of-day.\cr
#'\cr
#'\emph{'normfreq'}:
#'\cr
#'A percentage stacked area chart showing the normalized relative contribution of frequency bins with user-defined width to the soundscape diversity by time-of-day.\cr
#'\cr
#'\emph{'linefreq'}:
#'\cr
#'A line chart showing the relative contribution of frequency bins with user-defined width to the soundscape diversity by time-of-day.\cr
#' @param date The first day of the recording period. Used for managing time-objects in R. \cr
#' Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected.
#' @param lon The longitude of the site at which the sound files were collected.
#' @param minfreq The lower frequency limit for which to visualize the soundscape diversity, expressed as a numeric value.
#' Defaults to the lowest frequency for which data exists in the dataframe.
#' @param maxfreq The upper frequency limit for which to visualize the soundscape diversity, expressed as a numeric value.
#' Defaults to the highest frequency for which data exists in the dataframe.
#' @param nbins If graphtype='frequency'/'normfreq'/'linefreq', determines the number of the frequency-bins by which to divide the frequency range to compute the
#' relative contribution of each bin to the total diversity.
#' @param timeinterval A time interval for the x-axis. Options can be found in the \code{\link[scales]{date_breaks}} documentation.
#' @param smooth One of either TRUE or FALSE. If set to TRUE, applies a moving average filter for smoothing the diversity by time-of-day.
#' @param movavg If smooth=TRUE, determines the width of the moving average filter. Consult \code{\link[pracma]{movavg}} for more information.
#' @param interactive One of either TRUE or FALSE. If set to TRUE, an interactive plot is produced using \code{\link[plotly]{ggplotly}}.
#' @param save One of either TRUE or FALSE. If set to TRUE, saves the plot using \code{\link[ggplot2]{ggsave}}, and the 'dir', 'filename' and 'device'
#' arguments.
#' @param dir Path of the directory to save plot to: path and filename are combined to create the fully qualified file name.
#' Defaults to the working directory. For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param filename The file name without the extention. For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of
#' "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' Defaults to "png". For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#' @return Returns a ggplot object and if save=TRUE, saves the plot in a directory of choice using a specified device and filename.
#' @export
sounddiv_by_time=function(df, qvalue, graphtype="total", date, lat, lon, minfreq="default", maxfreq="default", nbins=10, timeinterval="1 hour", smooth=TRUE, movavg=6,interactive=FALSE, save=FALSE, dir="default", filename="file", device="png", output="percentage"){

  tz=lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")

  if (minfreq=="default"){
    minfreq=min(as.numeric(rownames(df)))
  }

  else{minfreq=minfreq}

  if (maxfreq=="default"){
    maxfreq=max(as.numeric(rownames(df)))
  }

  else{maxfreq=maxfreq}

  if (dir=="default"){
    dir=getwd()
  }

  else{dir=dir}

  if (graphtype=="total"){

    total_tod=sounddiv_internal(df=df,
                              qvalue=qvalue,
                              type="tod",
                              date=date,lat=lat,
                              lon=lon,
                              minfreq=minfreq,
                              maxfreq=(maxfreq-1),
                              twilight="sunlight",
                              freqseq=FALSE ,
                              nbins=nbins,
                              output=output)

    total_tod <- as.data.frame(total_tod)

    total_tod$time_of_day <- as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))

    colnames(total_tod) <- c("soundscape_div", "time_of_day")


    total_tod$soundscape_div_smooth=pracma::movavg(total_tod$soundscape_div, movavg, type="t")

    if (smooth==TRUE){

    plot=ggplot2::ggplot(total_tod, ggplot2::aes(time_of_day, soundscape_div_smooth))+
      ggplot2::geom_area(alpha=0.25, fill="#440154FF")+
      ggplot2::geom_line(color="#440154FF", size=1)+
      ggplot2::ylab(if(output=="percentage"){"Soundscape diversity (%)\n"} else{"Soundscape diversity (# OSUs)\n"})+
      ggplot2::xlab("\nTime of day (h)")+
      ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                       breaks = scales::date_breaks(timeinterval),
                       expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,if(output=="percentage"){100}else{max(total_tod$soundscape_div_smooth)+10}))+
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
            axis.text.y = ggplot2::element_text(color = "black", size = 10))+
      ggplot2::annotate(geom="text", x=(max(total_tod$time_of_day)-10000), y=if(output=="percentage"){95} else{(max(total_tod$soundscape_div_smooth)+7.5)}, label=paste0("q-value = ", qvalue), size=5)

    if (interactive==TRUE){
      plotly::ggplotly(plot)}

    else{plot}

    }

    else{

      if (smooth==FALSE){

        plot=ggplot2::ggplot(total_tod, ggplot2::aes(time_of_day, soundscape_div))+
          ggplot2::geom_area(alpha=0.25, fill="#440154FF")+
          ggplot2::geom_line(color="#440154FF", size=1)+
          ggplot2::ylab(if(output=="percentage"){"Total soundscape diversity (%)\n"} else{"Total soundscape diversity (# OSUs)\n"})+
          ggplot2::xlab("\nTime of day (h)")+
          ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                           breaks = scales::date_breaks(timeinterval),
                           expand = c(0,0))+
          ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,if(output=="percentage"){100}else{max(total_tod$soundscape_div)+10}))+
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                panel.border = ggplot2::element_blank(),
                axis.line = ggplot2::element_line(colour = "black"),
                axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
                axis.text.y = ggplot2::element_text(color = "black", size = 10))+
          ggplot2::annotate(geom="text", x=(max(total_tod$time_of_day)-10000), y=if(output=="percentage"){95} else{(max(total_tod$soundscape_div)+7.5)}, label=paste0("q-value = ", qvalue), size=5)

        if (interactive==TRUE){
          plotly::ggplotly(plot)}

        else{plot}

      } else{errorCondition(message = "Invalid smooth argument, consult package documentation for options")}

      }
    }

  else{

    if (graphtype=="frequency"){


      freq_tod=sounddiv_internal(df=df,
                                        qvalue=qvalue,
                                        type="tod",
                                        date=date,
                                        lat=lat,
                                        lon=lon,
                                        minfreq=minfreq,
                                        maxfreq=(maxfreq-1),
                                        twilight="sunlight",
                                        freqseq=TRUE ,
                                        nbins=nbins,
                                        output="raw")

      if(output=="percentage"){
        for (i in 1:length(freq_tod)){
          freq_tod[[i]] <- (freq_tod[[i]]/nrow(df))*100
        }
      } else {}

      freq_tod_smooth=vector("list", 0)

      for (i in 1:length(freq_tod)){
        freq_tod_smooth[[i]]=pracma::movavg(freq_tod[[i]], movavg, type="t")
      }

      names(freq_tod)=seq(minfreq, maxfreq, (maxfreq/nbins))
      names(freq_tod_smooth)=seq(minfreq, maxfreq, (maxfreq/nbins))

      freq_tod_maxlim <- max(rowSums(dplyr::bind_rows(freq_tod)))
      freq_tod_smooth_maxlim <- max(rowSums(dplyr::bind_rows(freq_tod_smooth)))

      freq_tod=dplyr::bind_rows(freq_tod)
      freq_tod_smooth=dplyr::bind_rows(freq_tod_smooth)


      freq_tod$time=as.POSIXct((strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
      freq_tod_smooth$time=as.POSIXct((strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
      freq_tod=reshape2::melt(freq_tod, id.vars=c("time"))
      freq_tod_smooth=reshape2::melt(freq_tod_smooth, id.vars=c("time"))

      colnames(freq_tod)=c("time_of_day", "frequency", "soundscape_div")
      colnames(freq_tod_smooth)=c("time_of_day", "frequency", "soundscape_div")

      freq_tod$frequency=as.factor(freq_tod$frequency)
      freq_tod_smooth$frequency=as.factor(freq_tod_smooth$frequency)

      if (smooth=="TRUE"){


        plot= ggplot2::ggplot(freq_tod_smooth, ggplot2::aes(time_of_day, soundscape_div, fill=frequency))+
          ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE))+
          ggplot2::ylab(if(output=="percentage"){"Soundscape diversity (%)"} else {"Soundscape diversity (#OSUs)"})+
          ggplot2::xlab("Time of day (h)")+
          ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                           breaks = scales::date_breaks(timeinterval),
                           expand = c(0,0))+
          ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,if(output=="percentage"){100}else{freq_tod_smooth_maxlim+10}))+
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                panel.border = ggplot2::element_blank(),
                axis.line = ggplot2::element_line(colour = "black"),
                axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
                axis.text.y = ggplot2::element_text(color = "black", size = 10),
                legend.justification=c(0.5,1),
                legend.position=c(0.5, 1))+
          viridis::scale_fill_viridis(discrete = TRUE,
                                      guide=ggplot2::guide_legend(title=NULL,direction ="horizontal", nrow=2, label.position = "top"),
                                      labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz"))

        if (interactive==TRUE){
          plotly::ggplotly(plot)}

        else{plot}

      }

      else{

        if (smooth=="FALSE"){

            plot= ggplot2::ggplot(freq_tod, ggplot2::aes(time_of_day, soundscape_div, fill=frequency))+
              ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE))+
              ggplot2::ylab(if(output=="percentage"){"Soundscape diversity (%)"} else {"Soundscape diversity (# OSUs)"})+
              ggplot2::xlab("Time of day (h)")+
              ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                               breaks = scales::date_breaks(timeinterval),
                               expand = c(0,0))+
              ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,if(output=="percentage"){100}else{freq_tod_maxlim+10}))+
              ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.background = ggplot2::element_blank(),
                    plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                    panel.border = ggplot2::element_blank(),
                    axis.line = ggplot2::element_line(colour = "black"),
                    axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
                    axis.text.y = ggplot2::element_text(color = "black", size = 10),
                    legend.justification=c(0.5,1),
                    legend.position=c(0.5, 1))+
              viridis::scale_fill_viridis(discrete = TRUE,
                                          guide=ggplot2::guide_legend(title=NULL,direction ="horizontal", nrow=2, label.position = "top"),
                                          labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz"))

            if (interactive==TRUE){
              plotly::ggplotly(plot)}

            else{plot}

        }

        else{errorCondition(message = "Invalid smooth argument - please consult package documentation for options")}

      }
    }

    else{

      if (graphtype=="normfreq"){

        freq_tod=sounddiv_internal(df=df,
                                          qvalue=qvalue,
                                          type="tod",
                                          date=date,
                                          lat=lat,
                                          lon=lon,
                                          minfreq=minfreq,
                                          maxfreq=(maxfreq-1),
                                          twilight="sunlight",
                                          freqseq=TRUE ,
                                          nbins=nbins,
                                          output="raw")

        freq_tod_smooth=vector("list", 0)

        for (i in 1:length(freq_tod)){
          freq_tod_smooth[[i]]=pracma::movavg(freq_tod[[i]], movavg, type="t")
        }

        names(freq_tod)=seq(minfreq, maxfreq, (maxfreq/nbins))
        names(freq_tod_smooth)=seq(minfreq, maxfreq, (maxfreq/nbins))

        freq_tod=dplyr::bind_rows(freq_tod)
        freq_tod_smooth=dplyr::bind_rows(freq_tod_smooth)


        freq_tod$time=as.POSIXct((strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
        freq_tod_smooth$time=as.POSIXct((strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
        freq_tod=reshape2::melt(freq_tod, id.vars=c("time"))
        freq_tod_smooth=reshape2::melt(freq_tod_smooth, id.vars=c("time"))

        colnames(freq_tod)=c("time_of_day", "frequency", "soundscape_div")
        colnames(freq_tod_smooth)=c("time_of_day", "frequency", "soundscape_div")

        freq_tod$frequency=as.factor(freq_tod$frequency)
        freq_tod_smooth$frequency=as.factor(freq_tod_smooth$frequency)

        if (smooth==TRUE){

          plot = freq_tod_smooth %>%
            dplyr::group_by(time_of_day) %>%
            dplyr::mutate(prop_sound_div = soundscape_div / sum(soundscape_div)) %>%
            dplyr::ungroup() %>%

            ggplot2::ggplot(ggplot2::aes(x=time_of_day, y=prop_sound_div, fill=frequency)) +
            ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
            ggplot2::ylab("Contribution to total soundscape diversity")+
            ggplot2::xlab("Time of day (h)")+
            ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                             breaks = scales::date_breaks(timeinterval),
                             expand = c(0,0))+
            ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,1))+
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                  panel.grid.minor = ggplot2::element_blank(),
                  panel.background = ggplot2::element_blank(),
                  plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                  panel.border = ggplot2::element_blank(),
                  axis.line = ggplot2::element_line(colour = "black"),
                  axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
                  axis.text.y = ggplot2::element_text(color = "black", size = 10),
                  legend.position="top",
                  legend.key.width = grid::unit(3,"cm"),
                  aspect.ratio = 0.3)+
            viridis::scale_fill_viridis(discrete = TRUE,
                                        guide=ggplot2::guide_legend(title=NULL,direction ="horizontal", nrow=2, label.position = "top"),
                                        labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz"))

          if (interactive==TRUE){
            plotly::ggplotly(plot)}

          else{plot}

        }

        else{

          if (smooth==FALSE){

            plot= freq_tod %>%
              dplyr::group_by(time_of_day) %>%
              dplyr::mutate(prop_sound_div = soundscape_div / sum(soundscape_div)) %>%
              dplyr::ungroup() %>%
              ggplot2::ggplot( ggplot2::aes(x=time_of_day, y=prop_sound_div, fill=frequency)) +
                ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
                ggplot2::ylab("Contribution to total soundscape diversity")+
                ggplot2::xlab("Time of day (h)")+
                ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                                 breaks = scales::date_breaks(timeinterval),
                                 expand = c(0,0))+
                ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,1))+
                ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.background = ggplot2::element_blank(),
                      plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                      panel.border = ggplot2::element_blank(),
                      axis.line = ggplot2::element_line(colour = "black"),
                      axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
                      axis.text.y = ggplot2::element_text(color = "black", size = 10),
                      legend.position="top",
                      legend.key.width = grid::unit(3,"cm"),
                      aspect.ratio = 0.3)+
                viridis::scale_fill_viridis(discrete = TRUE,
                                            guide=ggplot2::guide_legend(title=NULL,direction ="horizontal", nrow=2, label.position = "top"),
                                            labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz"))

            if (interactive==TRUE){
              plotly::ggplotly(plot)}

            else{plot}

          }

          else{errorCondition(message = "Invalid smooth argument - please consult package documentation for options")}

        }

      }

      else{

        if (graphtype=="linefreq"){

          freq_tod=sounddiv_internal(df=df,
                                            qvalue=qvalue,
                                            type="tod",
                                            date=date,
                                            lat=lat,
                                            lon=lon,
                                            minfreq=minfreq,
                                            maxfreq=(maxfreq-1),
                                            twilight="sunlight",
                                            freqseq=TRUE ,
                                            nbins=nbins,
                                            output="raw")

          if (output=="percentage"){
            for (i in 1:length(freq_tod)){
            freq_tod[[i]]=(freq_tod[[i]]/nrow(df))*100
            }
          }

          freq_tod_smooth=vector("list", 0)

          for (i in 1:length(freq_tod)){
            freq_tod_smooth[[i]]=pracma::movavg(freq_tod[[i]], movavg, type="t")
          }

          names(freq_tod)=seq(minfreq, maxfreq, (maxfreq/nbins))
          names(freq_tod_smooth)=seq(minfreq, maxfreq, (maxfreq/nbins))

          freq_tod=dplyr::bind_rows(freq_tod)
          freq_tod_smooth=dplyr::bind_rows(freq_tod_smooth)


          freq_tod$time=as.POSIXct((strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
          freq_tod_smooth$time=as.POSIXct((strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
          freq_tod=reshape2::melt(freq_tod, id.vars=c("time"))
          freq_tod_smooth=reshape2::melt(freq_tod_smooth, id.vars=c("time"))

          colnames(freq_tod)=c("time_of_day", "frequency", "soundscape_div")
          colnames(freq_tod_smooth)=c("time_of_day", "frequency", "soundscape_div")

          freq_tod$frequency=as.factor(freq_tod$frequency)
          freq_tod_smooth$frequency=as.factor(freq_tod_smooth$frequency)


          if (smooth==TRUE){

            labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz")
            names(labels)=seq(minfreq, maxfreq, (maxfreq/nbins))

            tmp <- freq_tod_smooth %>%
              dplyr::mutate(frequency2=frequency)

            plot= tmp %>%
              ggplot2::ggplot(ggplot2::aes(x=time_of_day, y=soundscape_div)) +
              ggplot2::geom_line( ggplot2::aes(color=frequency), size=1 )+
              ggplot2::geom_area( ggplot2::aes(fill=frequency), alpha=0.2 )+
              viridis::scale_color_viridis(discrete = TRUE) +
              viridis::scale_fill_viridis(discrete = TRUE)+
              ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                    panel.grid.minor = ggplot2::element_blank(),
                    panel.background = ggplot2::element_blank(),
                    plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                    panel.border = ggplot2::element_blank(),
                    axis.line = ggplot2::element_line(colour = "black"),
                    axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
                    axis.text.y = ggplot2::element_text(color = "black", size = 10),
                    legend.position="none",
                    strip.background = ggplot2::element_rect(color="black", fill="white"),
                    strip.text = ggplot2::element_text(color = "black", size = 10))+
              ggplot2::ylab(if(output=="percentage"){"Soundscape diversity (%)"} else {"Soundscape diversity (# OSUs)"})+
              ggplot2::xlab("Time of day (h)")+
              ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                               breaks = scales::date_breaks(timeinterval),
                               expand = c(0,0))+
              ggplot2::scale_y_continuous(expand = c(0,0))+
              ggplot2::facet_wrap(~frequency, labeller = ggplot2::labeller(frequency=labels))

            if (interactive==TRUE){
              plotly::ggplotly(plot)}

            else{plot}
          }

          else{

            if (smooth==FALSE){

              labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz")
              names(labels)=seq(minfreq, maxfreq, (maxfreq/nbins))

              tmp <- freq_tod %>%
                dplyr::mutate(frequency2=frequency)

              plot= tmp %>%
                ggplot2::ggplot( ggplot2::aes(x=time_of_day, y=soundscape_div)) +
                ggplot2::geom_line( ggplot2::aes(color=frequency), size=1 )+
                ggplot2::geom_area( ggplot2::aes(fill=frequency), alpha=0.2 )+
                viridis::scale_color_viridis(discrete = TRUE) +
                viridis::scale_fill_viridis(discrete = TRUE)+
                ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                      panel.grid.minor = ggplot2::element_blank(),
                      panel.background = ggplot2::element_blank(),
                      plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                      panel.border = ggplot2::element_blank(),
                      axis.line = ggplot2::element_line(colour = "black"),
                      axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
                      axis.text.y = ggplot2::element_text(color = "black", size = 10),
                      legend.position="none",
                      strip.background = ggplot2::element_rect(color="black", fill="white"),
                      strip.text = ggplot2::element_text(color = "black", size = 10))+
                ggplot2::ylab(if(output=="percentage"){"Soundscape diversity (%)"} else {"Soundscape diversity (# OSUs)"})+
                ggplot2::xlab("Time of day (h)")+
                ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                                 breaks = scales::date_breaks(timeinterval),
                                 expand = c(0,0))+
                ggplot2::scale_y_continuous(expand = c(0,0))+
                ggplot2::facet_wrap(~frequency, labeller = ggplot2::labeller(frequency=labels))

              if (interactive==TRUE){
                plotly::ggplotly(plot)}

              else{plot}

            }

            else{errorCondition(message = "Invalid smooth argument - please consult package documentation for options")}

          }

        }

    else{errorCondition(message = "Invalid graphtype argument - consult package documentation for options")}

      }
    }
  }

  if (save==TRUE){
    ggplot2::ggsave(filename=paste0(paste0(graphtype, "_"),filename,".",device),plot=plot,device = device, path=dir, dpi="retina")

    if (interactive==TRUE){
      plotly::ggplotly(plot)}

    else{plot}
  }

  else{
    if (interactive==TRUE){
      plotly::ggplotly(plot)}

    else{plot}}
}


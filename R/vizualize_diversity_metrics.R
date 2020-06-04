# Functions for visualizing richness and diversity metrics

  #1) Plots showing soundscape richness over time

utils::globalVariables(c("time", "diversity", "frequency", "freq", "richness", "diversity_smooth", "richness_smooth"))

#' Visualize Soundscape Richness by Time of Day
#'
#' @description Produces plots showing the variation in soundscape richness by time-of-day. Richness
#' can be shown for the full frequency range, or the relative contribution of frequency-bins with
#' user-specified width.
#'
#' @param df The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param graphtype The type of plot which is produced. \cr
#' \cr
#'\strong{Options are}:\cr
#'\cr
#'\bold{\emph{'total'}}:
#'\cr
#'An area chart showing the soundscape richness by time-of-day for the entire frequency range.
#'\cr
#'\bold{\emph{'frequency'}}:
#'\cr
#'A stacked area chart showing the relative contribution of frequency bins with user-defined width to the total soundscape richness by time-of-day.
#'\cr
#'\bold{\emph{'normfreq'}}:
#'\cr
#'A percentage stacked area chart showing the normalized relative contribution of frequency bins with user-defined width to the soundscape richness by time-of-day.
#'\cr
#'\bold{\emph{'linefreq'}}:
#'\cr
#'A line chart showing the relative contribution of frequency bins with user-defined width to the soundscape richness by time-of-day.
#' @param date The first day of the recording period. Used for managing time-objects in R. \cr
#' Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected.
#' @param lon The longitude of the site at which the sound files were collected.
#' @param minfreq The lower frequency limit for which to visualize the soundscape richness, expressed as a numeric value.
#' Defaults to the lowest frequency for which data exists in the dataframe.
#' @param maxfreq The upper frequency limit for which to visualize the soundscape richness, expressed as a numeric value.
#' Defaults to the highest frequency for which data exists in the dataframe.
#' @param nbins If graphtype='frequency'/'normfreq'/'linefreq', determines the number of the frequency-bins by which to divide the frequency range to compute the
#' relative contribution of each bin to the total richness.
#' @param timeinterval A time interval for the x-axis. Options can be found in the \code{\link[scales]{date_breaks}} documentation.
#' @param smooth One of either TRUE or FALSE. If set to TRUE, applies a moving average filter for smoothing the richness by time-of-day.
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
#'
#' @return Returns a ggplot object and if save=TRUE, saves the plot in a directory of choice using a specified device and filename.
#' @export
richness_by_time=function(df, graphtype="total", date, lat, lon, minfreq="default", maxfreq="default", nbins=10, timeinterval="1 hour", smooth=TRUE, movavg=6,interactive=TRUE, save=FALSE, dir="default", filename="file", device="png"){

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

    total_tod=as.data.frame(as.numeric(soundscape_richness(df=df,
                                                           type="tod",
                                                           date=date,
                                                           lat=lat,
                                                           lon=lon,
                                                           minfreq=minfreq,
                                                           maxfreq=(maxfreq-1),
                                                           twilight="sunlight",
                                                           freqseq=FALSE ,
                                                           nbins=nbins,
                                                           output="percentage")))

    total_tod$time=as.POSIXct((strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
    colnames(total_tod)=c("richness", "time")
    total_tod$richness_smooth=pracma::movavg(total_tod$richness, movavg, type="t")

    if (smooth==TRUE){

    plot=ggplot2::ggplot(total_tod, ggplot2::aes(time, richness_smooth))+
      ggplot2::geom_area(alpha=0.25, fill="#440154FF")+
      ggplot2::geom_line(color="#440154FF", size=1)+
      ggplot2::xlab("Total soundscape richness (%)\n")+
      ggplot2::xlab("\nTime of day (h)")+
      ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                       breaks = scales::date_breaks(timeinterval),
                       expand = c(0,0))+
      ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,100))+
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
            panel.grid.minor = ggplot2::element_blank(),
            panel.background = ggplot2::element_blank(),
            plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
            panel.border = ggplot2::element_blank(),
            axis.line = ggplot2::element_line(colour = "black"),
            axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
            axis.text.y = ggplot2::element_text(color = "black", size = 10))

    if (interactive==TRUE){
      plotly::ggplotly(plot)}

    else{plot}

    }

    else{

      if (smooth==FALSE){

        plot=ggplot2::ggplot(total_tod, ggplot2::aes(time, richness))+
          ggplot2::geom_area(alpha=0.25, fill="#440154FF")+
          ggplot2::geom_line(color="#440154FF", size=1)+
          ggplot2::xlab("Total soundscape richness (%)\n")+
          ggplot2::xlab("\nTime of day (h)")+
          ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                           breaks = scales::date_breaks(timeinterval),
                           expand = c(0,0))+
          ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,100))+
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                panel.grid.minor = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                panel.border = ggplot2::element_blank(),
                axis.line = ggplot2::element_line(colour = "black"),
                axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
                axis.text.y = ggplot2::element_text(color = "black", size = 10))

        if (interactive==TRUE){
          plotly::ggplotly(plot)}

        else{plot}

      }
    }
  }

  else{

    if (graphtype=="frequency"){


      freq_tod=soundscape_richness(df=df,
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

      for (i in 1:length(freq_tod)){
        freq_tod[[i]]=(freq_tod[[i]]/nrow(df))*100
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

      colnames(freq_tod)=c("time", "frequency", "richness")
      colnames(freq_tod_smooth)=c("time", "frequency", "richness")

      freq_tod$frequency=as.factor(freq_tod$frequency)
      freq_tod_smooth$frequency=as.factor(freq_tod_smooth$frequency)

      if (smooth=="TRUE"){


        plot= ggplot2::ggplot(freq_tod_smooth, ggplot2::aes(time, richness, fill=frequency))+
          ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE))+
          ggplot2::xlab("Soundscape richness (%)")+
          ggplot2::xlab("Time of day (h)")+
          ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                           breaks = scales::date_breaks(timeinterval),
                           expand = c(0,0))+
          ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,100))+
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

            plot= ggplot2::ggplot(freq_tod, ggplot2::aes(time, richness, fill=frequency))+
              ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE))+
              ggplot2::xlab("Soundscape richness (%)")+
              ggplot2::xlab("Time of day (h)")+
              ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                               breaks = scales::date_breaks(timeinterval),
                               expand = c(0,0))+
              ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,100))+
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
        }
    }

    else{

      if (graphtype=="normfreq"){

        freq_tod=soundscape_richness(df=df,
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

        for (i in 1:length(freq_tod)){
          freq_tod[[i]]=(freq_tod[[i]]/nrow(df))*100
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

        colnames(freq_tod)=c("time", "frequency", "richness")
        colnames(freq_tod_smooth)=c("time", "frequency", "richness")

        freq_tod$frequency=as.factor(freq_tod$frequency)
        freq_tod_smooth$frequency=as.factor(freq_tod_smooth$frequency)

        if (smooth==TRUE){

          plot = freq_tod_smooth %>%
            dplyr::group_by(time) %>%
            dplyr::mutate(freq = richness / sum(richness)) %>%
            dplyr::ungroup() %>%

            ggplot2::ggplot(ggplot2::aes(x=time, y=freq, fill=frequency)) +
            ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
            ggplot2::xlab("Contribution to total richness")+
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
                                        guide=ggplot2::guide_legend(title=NULL,direction ="horizontal", nrow=1, label.position = "top"),
                                        labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz"))

          if (interactive==TRUE){
            plotly::ggplotly(plot)}

          else{plot}

        }

        else{

          if (smooth==FALSE){

            plot= freq_tod %>%
              dplyr::group_by(time) %>%
              dplyr::mutate(freq = richness / sum(richness)) %>%
              dplyr::ungroup() %>%
              ggplot2::ggplot( ggplot2::aes(x=time, y=freq, fill=frequency)) +
                ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
                ggplot2::xlab("Contribution to total richness")+
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
                                            guide=ggplot2::guide_legend(title=NULL,direction ="horizontal", nrow=1, label.position = "top"),
                                            labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz"))

            if (interactive==TRUE){
              plotly::ggplotly(plot)}

            else{plot}

          }
        }
      }

      else{

        if (graphtype=="linefreq"){

          freq_tod=soundscape_richness(df=df,
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

          for (i in 1:length(freq_tod)){
            freq_tod[[i]]=(freq_tod[[i]]/nrow(df))*100
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

          colnames(freq_tod)=c("time", "frequency", "richness")
          colnames(freq_tod_smooth)=c("time", "frequency", "richness")

          freq_tod$frequency=as.factor(freq_tod$frequency)
          freq_tod_smooth$frequency=as.factor(freq_tod_smooth$frequency)

          if (smooth==TRUE){

            labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz")
            names(labels)=seq(minfreq, maxfreq, (maxfreq/nbins))

            tmp <- freq_tod_smooth %>%
              dplyr::mutate(frequency2=frequency)

            plot= tmp %>%
              ggplot2::ggplot( ggplot2::aes(x=time, y=richness)) +
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
              ggplot2::xlab("Soundscape richness (%)")+
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
                ggplot2::ggplot( ggplot2::aes(x=time, y=richness)) +
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
                ggplot2::xlab("Soundscape richness (%)")+
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
          }
        }

    else{print("Error: invalid graphtype argument - consult package documentation for options")}

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

##

#' Visualize Soundscape Diversity by Time of Day
#'
#' @description Produces plots showing the variation in soundscape diversity by time-of-day. diversity
#' can be shown for the full frequency range, or the relative contribution of frequency-bins with
#' user-specified width.
#'
#' @param df The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param graphtype The type of plot which is produced. \cr
#' \cr
#'\strong{Options are}:\cr
#'\cr
#'\bold{\emph{'total'}}:
#'\cr
#'An area chart showing the soundscape richness by time-of-day for the entire frequency range.
#'\cr
#'\bold{\emph{'frequency'}}:
#'\cr
#'A stacked area chart showing the relative contribution of frequency bins with user-defined width to the total soundscape richness by time-of-day.
#'\cr
#'\bold{\emph{'normfreq'}}:
#'\cr
#'A percentage stacked area chart showing the normalized relative contribution of frequency bins with user-defined width to the soundscape richness by time-of-day.
#'\cr
#'\bold{\emph{'linefreq'}}:
#'\cr
#'A line chart showing the relative contribution of frequency bins with user-defined width to the soundscape richness by time-of-day.
#'\cr
#' @param date The first day of the recording period. Used for managing time-objects in R. \cr
#' Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected.
#' @param lon The longitude of the site at which the sound files were collected.
#' @param minfreq The lower frequency limit for which to visualize the soundscape diversity, expressed as a numeric value.
#' Defaults to the lowest frequency for which data exists in the dataframe.
#' @param maxfreq The upper frequency limit for which to visualize the soundscape diversity, expressed as a numeric value.
#' Defaults to the highest frequency for which data exists in the dataframe.
#' @param nbins If graphtype='frequency'/'normfreq'/'linefreq', determines the number of the frequency-bins by which to divide the frequency range to compute the
#' relative contribution of each bin to the total diversity
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
#'
#' @return Returns a ggplot object and if save=TRUE, saves the plot in a directory of choice using a specified device and filename.
#' @keywords internal
diversity_by_time=function(df, graphtype="total", date, lat, lon, minfreq="default", maxfreq="default", nbins=10, timeinterval="1 hour", smooth=TRUE, movavg=6,interactive=TRUE, save=FALSE, dir="default", filename="file", device="png"){

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

    total_tod=as.data.frame(as.numeric(soundscape_diversity(df=df,
                                                           type="tod",
                                                           date=date,
                                                           lat=lat,
                                                           lon=lon,
                                                           minfreq=minfreq,
                                                           maxfreq=(maxfreq-1),
                                                           twilight="sunlight",
                                                           freqseq=FALSE ,
                                                           nbins=nbins,
                                                           output="percentage")))

    total_tod$time=as.POSIXct((strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
    colnames(total_tod)=c("diversity", "time")
    total_tod$diversity_smooth=pracma::movavg(total_tod$diversity, movavg, type="t")

    if (smooth==TRUE){

      plot=ggplot2::ggplot(total_tod, ggplot2::aes(time, diversity_smooth))+
        ggplot2::geom_area(alpha=0.25, fill="#440154FF")+
        ggplot2::geom_line(color="#440154FF", size=1)+
        ggplot2::xlab("Total soundscape diversity (%)\n")+
        ggplot2::xlab("\nTime of day (h)")+
        ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                                  breaks = scales::date_breaks(timeinterval),
                                  expand = c(0,0))+
        ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,100))+
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                       panel.grid.minor = ggplot2::element_blank(),
                       panel.background = ggplot2::element_blank(),
                       plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                       panel.border = ggplot2::element_blank(),
                       axis.line = ggplot2::element_line(colour = "black"),
                       axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
                       axis.text.y = ggplot2::element_text(color = "black", size = 10))

      if (interactive==TRUE){
        plotly::ggplotly(plot)}

      else{plot}

    }

    else{

      if (smooth==FALSE){

        plot=ggplot2::ggplot(total_tod, ggplot2::aes(time,diversity))+
          ggplot2::geom_area(alpha=0.25, fill="#440154FF")+
          ggplot2::geom_line(color="#440154FF", size=1)+
          ggplot2::xlab("Total soundscape diversity (%)\n")+
          ggplot2::xlab("\nTime of day (h)")+
          ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                                    breaks = scales::date_breaks(timeinterval),
                                    expand = c(0,0))+
          ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,100))+
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                         panel.grid.minor = ggplot2::element_blank(),
                         panel.background = ggplot2::element_blank(),
                         plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
                         panel.border = ggplot2::element_blank(),
                         axis.line = ggplot2::element_line(colour = "black"),
                         axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -45, vjust=1.2, hjust=-0.3),
                         axis.text.y = ggplot2::element_text(color = "black", size = 10))

        if (interactive==TRUE){
          plotly::ggplotly(plot)}

        else{plot}

      }
    }
  }

  else{

    if (graphtype=="frequency"){


      freq_tod=soundscape_diversity(df=df,
                                   type="tod",
                                   date=date,
                                   lat=lat,
                                   lon=lon,
                                   minfreq=minfreq,
                                   maxfreq=maxfreq,
                                   twilight="sunlight",
                                   freqseq=TRUE ,
                                   nbins=nbins,
                                   output="raw")

      for (i in 1:length(freq_tod)){
        freq_tod[[i]]=(freq_tod[[i]]/nrow(df))*100
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

      colnames(freq_tod)=c("time", "frequency", "diversity")
      colnames(freq_tod_smooth)=c("time", "frequency", "diversity")

      freq_tod$frequency=as.factor(freq_tod$frequency)
      freq_tod_smooth$frequency=as.factor(freq_tod_smooth$frequency)

      if (smooth=="TRUE"){


        plot= ggplot2::ggplot(freq_tod_smooth, ggplot2::aes(time, diversity, fill=frequency))+
          ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE))+
          ggplot2::xlab("Soundscape diversity (%)")+
          ggplot2::xlab("Time of day (h)")+
          ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                                    breaks = scales::date_breaks(timeinterval),
                                    expand = c(0,0))+
          ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,100))+
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

          plot= ggplot2::ggplot(freq_tod, ggplot2::aes(time, diversity, fill=frequency))+
            ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE))+
            ggplot2::xlab("Soundscape diversity (%)")+
            ggplot2::xlab("Time of day (h)")+
            ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                                      breaks = scales::date_breaks(timeinterval),
                                      expand = c(0,0))+
            ggplot2::scale_y_continuous(expand = c(0,0), limits=c(0,100))+
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
      }
    }

    else{

      if (graphtype=="normfreq"){

        freq_tod=soundscape_diversity(df=df,
                                     type="tod",
                                     date=date,
                                     lat=lat,
                                     lon=lon,
                                     minfreq=minfreq,
                                     maxfreq=maxfreq,
                                     twilight="sunlight",
                                     freqseq=TRUE ,
                                     nbins=nbins,
                                     output="raw")

        for (i in 1:length(freq_tod)){
          freq_tod[[i]]=(freq_tod[[i]]/nrow(df))*100
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

        colnames(freq_tod)=c("time", "frequency", "diversity")
        colnames(freq_tod_smooth)=c("time", "frequency", "diversity")

        freq_tod$frequency=as.factor(freq_tod$frequency)
        freq_tod_smooth$frequency=as.factor(freq_tod_smooth$frequency)

        if (smooth==TRUE){

          plot = freq_tod_smooth %>%
            dplyr::group_by(time) %>%
            dplyr::mutate(freq = diversity / sum(diversity)) %>%
            dplyr::ungroup() %>%

            ggplot2::ggplot(ggplot2::aes(x=time, y=freq, fill=frequency)) +
            ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
            ggplot2::xlab("Contribution to total diversity")+
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
                                        guide=ggplot2::guide_legend(title=NULL,direction ="horizontal", nrow=1, label.position = "top"),
                                        labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz"))

          if (interactive==TRUE){
            plotly::ggplotly(plot)}

          else{plot}

        }

        else{

          if (smooth==FALSE){

            plot= freq_tod %>%
              dplyr::group_by(time) %>%
              dplyr::mutate(freq = diversity / sum(diversity)) %>%
              dplyr::ungroup() %>%
              ggplot2::ggplot( ggplot2::aes(x=time, y=freq, fill=frequency)) +
              ggplot2::geom_area(position = ggplot2::position_stack(reverse = TRUE)) +
              ggplot2::xlab("Contribution to total diversity")+
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
                                          guide=ggplot2::guide_legend(title=NULL,direction ="horizontal", nrow=1, label.position = "top"),
                                          labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz"))

            if (interactive==TRUE){
              plotly::ggplotly(plot)}

            else{plot}

          }
        }
      }

      else{

        if (graphtype=="linefreq"){

          freq_tod=soundscape_diversity(df=df,
                                       type="tod",
                                       date=date,
                                       lat=lat,
                                       lon=lon,
                                       minfreq=minfreq,
                                       maxfreq=maxfreq,
                                       twilight="sunlight",
                                       freqseq=TRUE ,
                                       nbins=nbins,
                                       output="raw")

          for (i in 1:length(freq_tod)){
            freq_tod[[i]]=(freq_tod[[i]]/nrow(df))*100
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

          colnames(freq_tod)=c("time", "frequency", "diversity")
          colnames(freq_tod_smooth)=c("time", "frequency", "diversity")

          freq_tod$frequency=as.factor(freq_tod$frequency)
          freq_tod_smooth$frequency=as.factor(freq_tod_smooth$frequency)

          if (smooth==TRUE){

            labels=paste0(seq((minfreq-minfreq),(maxfreq-(maxfreq/nbins)), (maxfreq/nbins)), "-", seq((maxfreq/nbins), maxfreq, (maxfreq/nbins)), " ", "Hz")
            names(labels)=seq(minfreq, maxfreq, (maxfreq/nbins))

            tmp <- freq_tod_smooth %>%
              dplyr::mutate(frequency2=frequency)

            plot= tmp %>%
              ggplot2::ggplot( ggplot2::aes(x=time, y=diversity)) +
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
              ggplot2::xlab("Soundscape diversity (%)")+
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
                ggplot2::ggplot( ggplot2::aes(x=time, y=diversity)) +
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
                ggplot2::xlab("Soundscape diversity (%)")+
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
          }
        }

        else{print("Error: invalid graphtype argument - consult package documentation for options")}

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

#richness_by_time(aggregate_df_list[[1]],
 #                graphtype="frequency",
  #               metadata$date[1],
   #              metadata$lat[1],
    #             metadata$long[1],
     #            minfreq=0,
      #           maxfreq=20000,
       #          freqinterval=2000,
        #         timeinterval="2 hours",
         #        smooth=TRUE,
          #       movavg=4,
           #      interactive = TRUE,
            #     save=TRUE,
             #    dir="D:/User/Programs/Test",
              #   filename = "test",
               #  device = "pdf"
                # )

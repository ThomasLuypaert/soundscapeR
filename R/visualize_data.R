# Functions for visualizing data -------------------------------------------------------

# Preparing dataframe for visualization # happens internally

#' Reformats the dataframe for subsequent visualization
#'
#' @description Lengthens the dataframe so it can be used for subsequent visualization using ggplot2.
#' Function intended for internal use by \code{\link{heatmapper}}, however can also be used manually by user.
#'
#' @param df The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param date The first day of the recording period. Used for managing time-objects in R. Format as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected.
#' @param lon The longitude of the site at which the sound files were collected.
#'
#' @return Returns a long dataframe with "frequency", "time" and "value" columns.
#' @export
#'
lengthen=function(df, date, lat, lon){
  tz=lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")
  df$frequency=as.integer(rownames(df))
  melt_df=reshape2::melt(df, id.vars="frequency")
  colnames(melt_df)=c("frequency", "time", "value")
  melt_df$frequency=as.numeric(as.character(melt_df$frequency))
  melt_df$time=as.POSIXct(strptime(paste(date, melt_df$time, sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))
  melt_df
}

##

#' Quick heatmap
#'
#' @description A quick-and-dirty interactive heatmap for rapid data checking, using the \code{\link[plsgenomics]{matrix.heatmap}} function.
#' Used for quick visual inspection of outputs for the \code{\link{merge_csv}} and \code{\link{binarize_df}} functions.
#' Useful for checking which binarization algorithm works best, and fine-tuning threshold values.
#'
#' @param df A time-frequency dataframe produced by \code{\link{merge_csv}}, \code{\link{binarize_df}} or \code{\link{aggregate_df}}.
#'
#' @return Returns a heatmap plot
#' @export

quick_heatmap=function(df){
  plsgenomics::matrix.heatmap(df, col=grDevices::hcl.colors(palette = "YlGnBu", n=20, rev=TRUE))
}

##

utils::globalVariables(c("time", "frequency", "value", "richness_smooth"))


#' Flexible Soundscape Heatmaps
#'
#' @description  Creates a soundscape heatmap to visualize the use of acoustic
#' space in the time-frequency domain for an set acoustic index. The function is highly
#' flexible, allowing the user maximal control over every aspect of the heatmap. Please
#' consult the arguments section to find out more about visualization options.
#'
#' @param df The aggregated time-frequency dataframe produced by \code{\link{aggregate_df}}.
#' @param type One of either "regular" or "polar". If set to "regular", produces a regular rectangular heatmap.
#' If set to "polar", produces a polar heatmap suitable for exploring diurnal patterns.
#' @param annotate One of either TRUE or FALSE. If set to TRUE, annotates the heatmap with sunrise and sunset times,
#' and highlights the border between the audible and ultrasonic spectrum for human hearing.
#' @param timeinterval A time interval for the x-axis. Options can be found in the \code{\link[scales]{date_breaks}} documentation.
#' @param mintime The lower time limit for the x-axis, formatted as "HH:MM:SS". Defaults to the earliest
#' time for which data exists in the dataframe.
#' @param maxtime The upper time limit for the x-axis, formatted as "HH:MM:SS".Defaults to the latest
#' time for which data exists in the dataframe.
#' @param freqinterval The frequency interval for the y-axis, expressed as a numeric value.
#' @param minfreq The lower frequency limit for the y-axis as a numeric value. Defaults to zero.
#' @param maxfreq The upper frequency limit for the y-axis as a numeric value. Defaults to the maximum frequency of the dataframe.
#' @param nbins If \code{marginplot=TRUE}, determines the number of the frequency-bins by which to divide the frequency range to compute the
#' soundscape richness, expressed as a numeric constant.
#' @param date The first day of the recording period. Used for managing time-objects in R. \cr Formatted as "YYYY-mm-dd".
#' @param lat The latitude of the site at which the sound files were collected.
#' @param lon The longitude of the site at which the sound files were collected.
#' @param twilight A character string of the twilight method to be used for sunrise and sunset annotation.
#' Options can be found in the \code{\link[photobiology]{day_night}} documentation.
#' @param labelsize_time If annotate=TRUE, can be used to alter the size of temporal spectrum annotation.
#' @param labelsize_frequency If annotate=TRUE, can be used to alter the size of frequency spectrum annotation.
#' @param labelsize_polar If type="polar", can be used to alter the size of the frequency axis labels.
#' @param palette A character string indicating the colormap option to use.
#' Four options are available: "magma" (or "A"), "inferno" (or "B"), "plasma" (or "C"),
#' "viridis" (or "D", the default option) and "cividis" (or "E"). Consult \url{https://www.rdocumentation.org/packages/viridisLite/versions/0.3.0/topics/viridis} for options.
#' @param direction Sets the order of colors in the scale. If 1, the default, the regular order is followed.
#' If -1, the order of colors is reversed.
#' @param marginplot One of either TRUE or FALSE. If set to TRUE, adds marginal plots to the x- and y-axes.
#' For the x-axis, the marginal plot displays the smoothed richness of acoustically active frequency bins for each time of day.
#' For the y-axis, the marginal plot displays the smoothed richness of acoustically active time-bins for each frequency band.
#' Note that marginal plots are not available for type="polar".
#' @param interactive One of either TRUE or FALSE. If set to TRUE, an interactive plot is produced using \code{\link[plotly]{ggplotly}}.
#' Note that interactive plots are not available for marginplot=TRUE.
#' @param save One of either TRUE or FALSE. If set to TRUE, saves the plot using \code{\link[ggplot2]{ggsave}}, and the 'dir', 'filename' and 'device'
#' arguments.
#' @param dir Path of the directory to save plot to: path and filename are combined to create the fully qualified file name.
#' Defaults to the working directory. For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param filename The file name without the extention. For more information consult \code{\link[ggplot2]{ggsave}}.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of
#' "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' Defaults to "png". For more information consult \code{\link[ggplot2]{ggsave}}.
#'
#' @return Returns a ggplot heatmap object and if save=TRUE, saves the plot in a directory of choice using a specified device and filename.
#' @export

heatmapper=function(df,type="regular", annotate=TRUE, timeinterval="1 hour", mintime="default", maxtime="default",freqinterval=1000, minfreq=0, maxfreq="default", nbins=10, date, lat, lon, twilight="sunlight", labelsize_time=4, labelsize_frequency=4, labelsize_polar=3, palette="D", direction=1, marginplot=FALSE, interactive=FALSE, save=FALSE, dir="default", filename="file", device="png"){

  tz=lutz::tz_lookup_coords(lat=lat, lon=lon, method="accurate")

  if (dir=="default"){
    dir=getwd()
  }

  else{dir=dir}

  df2=lengthen(df, paste0(date), lat = lat, lon=lon)

  if (mintime=="default"){
    mintime=min(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  } else{mintime=as.POSIXct(strptime(paste(date, mintime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  if (maxtime=="default"){
    maxtime=max(as.POSIXct(strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz)))
  } else{maxtime=as.POSIXct(strptime(paste(date, maxtime, sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))}

  if (freqinterval=="default"){
  freqinterval=(min(df2$frequency)+1)
  } else{freqinterval=freqinterval}

  if (minfreq=="default"){
    minfreq=0
    } else {minfreq=minfreq}

  if (maxfreq=="default"){
    maxfreq=max(df2$frequency)
  } else{maxfreq=maxfreq}

  day=as.POSIXct(strptime(paste(date, "00:00", sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))
  points=as.data.frame(t(as.data.frame(c(lat, lon))))
  colnames(points)=c("lat","lon")
  suntimes=photobiology::day_night(date=day, tz=lubridate::tz(day),geocode = points, twilight = twilight, unit.out = "datetime")

  sunrise=suntimes$sunrise
  sunset=suntimes$sunset

  midnight1=as.POSIXct(strptime(paste(date, "00:00", sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))
  midnight2=as.POSIXct(strptime(paste(date, "23:50", sep=" "), format= "%Y-%m-%d %H:%M", tz=tz))

  if (type=="regular"){

    if (annotate==TRUE){

      if (maxfreq>22000){
        plot= ggplot2::ggplot(df2, ggplot2::aes(time, frequency, fill=value))+
          ggplot2::geom_raster(hjust=1)+
          viridis::scale_fill_viridis(option=palette, direction = direction, guide = ggplot2::guide_legend(title.position = "top", title.vjust = 1, title.hjust = 0.5, nrow=1),breaks=seq(0, 1, 0.1))+
          ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                           breaks = scales::date_breaks(timeinterval),
                           expand = c(0,0),
                           limits = c(mintime,maxtime))+
          ggplot2::scale_y_continuous(limits = c(minfreq, (maxfreq+(maxfreq/10))), expand = c(0,0), breaks = seq(minfreq, maxfreq, freqinterval))+
          ggplot2::labs(y="Frequency (Hz) \n", x="\nTime (hour of day)")+
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                axis.text.y = ggplot2::element_text(color = "black", size = 10),
                axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = 45, hjust=1.1, vjust=1),
                axis.title.y = ggplot2::element_text(face="bold"),
                axis.title.x = ggplot2::element_text(face="bold"),
                plot.margin = grid::unit(c(1,1,1,1),"cm"),
                legend.position = "top",
                legend.direction = "horizontal",
                legend.title = ggplot2::element_text(color = "black", size = 12, face = "bold"))+
          ggplot2::geom_vline(xintercept = sunrise, linetype="dashed", color=if (direction==1){paste("white")}else{paste("black")})+
          ggplot2::geom_vline(xintercept = sunset, linetype="dashed", color=if (direction==1){paste("white")}else{paste("black")})+
          ggplot2::geom_hline(yintercept = 20000, linetype="dashed", color=if (direction==1){paste("white")}else{paste("black")})+
          ggplot2::annotate(geom="rect", xmin =min(df2$time),xmax=sunrise,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", color="white", alpha=1)+
          ggplot2::annotate(geom="rect", xmin =sunset,xmax=midnight2,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", color="white", alpha=1)+
          ggplot2::annotate(geom="rect", xmin =sunrise,xmax=sunset,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#ffcc13", color="white", alpha=1)+
          ggplot2::annotate(geom="rect", xmin =min(df2$time),xmax=sunrise,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", color="black", alpha=0.25)+
          ggplot2::annotate(geom="rect", xmin =sunset,xmax=midnight2,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", color="black", alpha=0.25)+
          ggplot2::annotate(geom="rect", xmin =sunrise,xmax=sunset,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#ffcc13", color="black", alpha=0.25)+
          ggplot2::annotate(geom="rect", xmin =midnight2,xmax=midnight2+3600,ymin=minfreq, ymax=20000,fill="white", color="white", alpha=0.5)+
          ggplot2::annotate(geom="rect", xmin =midnight2,xmax=midnight2+3600,ymin=20000, ymax=maxfreq,fill="white", color="white", alpha=0.5)+
          ggplot2::annotate("text", x = (sunrise-(as.numeric(difftime(sunrise, midnight1, units = "secs"))/2)),
                   y = (maxfreq+(maxfreq/20)), label = "NIGHTTIME", color="white", fontface=2, size=labelsize_time)+
          ggplot2::annotate("text", x = (sunset-(as.numeric(difftime(sunset, sunrise, units = "secs"))/2)),
                   y = (maxfreq+(maxfreq/20)), label = "DAYTIME", color="black", fontface=2, size=labelsize_time)+
          ggplot2::annotate("text", x = (midnight2-(as.numeric(difftime(midnight2, sunset, units = "secs"))/2)),
                   y = (maxfreq+(maxfreq/20)), label = "NIGHTTIME", color="white", fontface=2, size=labelsize_time)+
          ggplot2::geom_vline(xintercept = midnight2) +
          ggplot2::annotate("text", x = midnight2+2200,
                   y = 20000+((maxfreq-20000)/2), label = "ULTRASOUND", color="black", fontface=2, angle=-90, size=labelsize_frequency)+
          ggplot2::annotate("text", x = midnight2+2200,
                   y = (20000-minfreq)/2, label = "AUDIBLE", color="black", fontface=2, angle=-90, size=labelsize_frequency)+
          ggplot2::labs(fill="Proportion of acoustically active bins")
      }

      else{

        if (maxfreq<=22000){

          plot= ggplot2::ggplot(df2, ggplot2::aes(time, frequency, fill=value))+
            ggplot2::geom_raster(hjust=1)+
            viridis::scale_fill_viridis(option=palette, direction = direction, guide = ggplot2::guide_legend(title.position = "top", title.vjust = 1, title.hjust = 0.5, nrow=1),breaks=seq(0, 1, 0.1))+
            ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                             breaks = scales::date_breaks(timeinterval),
                             expand = c(0,0),
                             limits = c(mintime,maxtime))+
            ggplot2::scale_y_continuous(limits = c(minfreq, (maxfreq+(maxfreq/10))), expand = c(0,0), breaks = seq(minfreq, maxfreq, freqinterval))+
            ggplot2::labs(y="Frequency (Hz) \n", x="\nTime (hour of day)")+
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                  panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                  axis.text.y = ggplot2::element_text(color = "black", size = 10),
                  axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = 45, hjust=1.1,vjust=1),
                  axis.title.y = ggplot2::element_text(face="bold"),
                  axis.title.x = ggplot2::element_text(face="bold"),
                  plot.margin = grid::unit(c(1,1,1,1),"cm"),
                  legend.position = "top",
                  legend.direction = "horizontal",
                  legend.title = ggplot2::element_text(color = "black", size = 12, face = "bold"))+
            ggplot2::geom_vline(xintercept = sunrise, linetype="dashed", color=if (direction==1){paste("white")}else{paste("black")})+
            ggplot2::geom_vline(xintercept = sunset, linetype="dashed", color=if (direction==1){paste("white")}else{paste("black")})+
            ggplot2::geom_hline(yintercept = 20000, linetype="dashed", color=if (direction==1){paste("white")}else{paste("black")})+
            ggplot2::annotate(geom="rect", xmin =min(df2$time),xmax=sunrise,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", color="white", alpha=1)+
            ggplot2::annotate(geom="rect", xmin =sunset,xmax=midnight2,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", color="white", alpha=1)+
            ggplot2::annotate(geom="rect", xmin =sunrise,xmax=sunset,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#ffcc13", color="white", alpha=1)+
            ggplot2::annotate(geom="rect", xmin =min(df2$time),xmax=sunrise,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", color="black", alpha=0.25)+
            ggplot2::annotate(geom="rect", xmin =sunset,xmax=midnight2,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", color="black", alpha=0.25)+
            ggplot2::annotate(geom="rect", xmin =sunrise,xmax=sunset,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#ffcc13", color="black", alpha=0.25)+
            ggplot2::annotate(geom="rect", xmin =midnight2,xmax=midnight2+3600,ymin=minfreq, ymax=20000,fill="white", color="white", alpha=0.5)+
            ggplot2::annotate(geom="rect", xmin =midnight2,xmax=midnight2+3600,ymin=20000, ymax=maxfreq,fill="white", color="white", alpha=0.5)+
            ggplot2::annotate("text", x = (sunrise-(as.numeric(difftime(sunrise, midnight1, units = "secs"))/2)),
                     y = (maxfreq+(maxfreq/20)), label = "NIGHTTIME", color="white", fontface=2)+
            ggplot2::annotate("text", x = (sunset-(as.numeric(difftime(sunset, sunrise, units = "secs"))/2)),
                     y = (maxfreq+(maxfreq/20)), label = "DAYTIME", color="black", fontface=2)+
            ggplot2::annotate("text", x = (midnight2-(as.numeric(difftime(midnight2, sunset, units = "secs"))/2)),
                     y = (maxfreq+(maxfreq/20)), label = "NIGHTTIME", color="white", fontface=2)+
            ggplot2::geom_vline(xintercept = midnight2)+
            ggplot2::labs(fill="Proportion of acoustically active bins")
        }

        else{print("Error: invalid maxfreq argument - please consult package documentation for options")}

      }
    }

    else {

      if (annotate==FALSE){

        plot=ggplot2::ggplot(df2, ggplot2::aes(time, frequency, fill=value))+
          ggplot2::geom_raster(hjust=1)+
          viridis::scale_fill_viridis(option=palette, direction = direction, guide = ggplot2::guide_legend(title.position = "top", title.vjust = 1, title.hjust = 0.5, nrow=1),breaks=seq(0, 1, 0.1))+
          ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                           breaks = scales::date_breaks(timeinterval),
                           expand = c(0,0),
                           limits = c(mintime,maxtime))+
          ggplot2::scale_y_continuous(limits = c(minfreq,maxfreq), expand = c(0,0), breaks = seq(minfreq, maxfreq, freqinterval))+
          ggplot2::labs(y="Frequency (Hz) \n", x="\nTime (hour of day)")+
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                axis.text.y = ggplot2::element_text(color = "black", size = 10),
                axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = 45, hjust=1.1,vjust=1),
                axis.title.y = ggplot2::element_text(face="bold"),
                axis.title.x = ggplot2::element_text(face="bold"),
                panel.border = ggplot2::element_rect(colour = "black", fill=NA, size=0.5),
                plot.margin = grid::unit(c(1,1,1,1),"cm"),
                legend.position = "top",
                legend.direction = "horizontal",
                legend.title = ggplot2::element_text(color = "black", size = 12, face = "bold"))+
          ggplot2::labs(fill="Proportion of acoustically active bins")
      }

      else{print("Error: invalid annotate argument - please consult package documentation for options")}

    }
  }

  else{

    if (type=="polar"){

      if (annotate==TRUE){

        plot=ggplot2::ggplot(df2, ggplot2::aes(time, frequency, fill=value))+
          ggplot2::geom_tile()+
          viridis::scale_fill_viridis(option=palette, direction = direction, guide = ggplot2::guide_legend(title.position = "top", title.vjust = 1, title.hjust = 0.5, nrow=2),breaks=seq(0, 1, 0.1))+
          ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                           breaks = scales::date_breaks(timeinterval),
                           expand = c(0,0),
                           limits = c(mintime,maxtime))+
          ggplot2::scale_y_continuous(limits = c(minfreq,(maxfreq+(maxfreq/10))), expand = c(0,0), breaks = seq(minfreq, maxfreq, freqinterval))+
          ggplot2::labs(y="Frequency (Hz) \n", x="\nTime (hour of day)")+
          ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -0, hjust=1.1, vjust=1),
                axis.title.y = ggplot2::element_text(face="bold"),
                axis.title.x = ggplot2::element_text(face="bold"),
                panel.border = ggplot2::element_rect(colour = "white", fill=NA, size=0.5),
                plot.margin = grid::unit(c(1,1,1,1),"cm"),
                axis.text.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank(),
                legend.position = "top",
                legend.direction = "horizontal",
                legend.title = ggplot2::element_text(color = "black", size = 12, face = "bold"))+
          ggplot2::annotate(geom="segment", x=sunrise, xend=sunrise, y=minfreq, yend=maxfreq, color=if (direction==1){paste("white")}else{paste("black")})+
          ggplot2::annotate(geom="segment", x=sunset, xend=sunset, y=minfreq, yend=maxfreq, color=if (direction==1){paste("white")}else{paste("black")})+
          ggplot2::annotate(geom="segment", x=seq.POSIXt(from=min(df2$time), to=max(df2$time), by=3600), xend=seq.POSIXt(from=min(df2$time), to=max(df2$time), by=3600), y=minfreq, yend=maxfreq, color="grey", alpha=0.5, size=0.2)+
          ggplot2::coord_polar()+
          ggplot2::geom_hline(yintercept = seq(minfreq, maxfreq, freqinterval), color="grey", alpha=0.5, size=0.2)+
          ggplot2::annotate(geom="label", size=labelsize_polar, y=seq(minfreq, maxfreq, freqinterval), x=min(df2$time),label=as.character(seq(minfreq, maxfreq, freqinterval)))+
          ggplot2::annotate(geom="rect", xmin =min(df2$time),xmax=sunrise,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", alpha=1)+
          ggplot2::annotate(geom="rect", xmin =sunset,xmax=midnight2,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", alpha=1)+
          ggplot2::annotate(geom="rect", xmin =sunrise,xmax=sunset,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#ffcc13", alpha=1)+
          ggplot2::annotate(geom="rect", xmin =min(df2$time),xmax=sunrise,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", alpha=0.25)+
          ggplot2::annotate(geom="rect", xmin =sunset,xmax=midnight2,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#4C4B69", alpha=0.25)+
          ggplot2::annotate(geom="rect", xmin =sunrise,xmax=sunset,ymin=maxfreq, ymax=(maxfreq+(maxfreq/10)),fill="#ffcc13", alpha=0.25)+
          ggplot2::labs(fill="Proportion of acoustically active bins")

      }

      else{

        if (annotate==FALSE){

          plot=ggplot2::ggplot(df2, ggplot2::aes(time, frequency, fill=value))+
            ggplot2::geom_tile()+
            viridis::scale_fill_viridis(option=palette, direction = direction, guide = ggplot2::guide_legend(title.position = "top", title.vjust = 1, title.hjust = 0.5, nrow=2),breaks=seq(0, 1, 0.1))+
            ggplot2::scale_x_datetime(labels=scales::date_format("%H:%M", tz=tz),
                             breaks = scales::date_breaks(timeinterval),
                             expand = c(0,0),
                             limits = c(mintime,maxtime))+
            ggplot2::scale_y_continuous(limits = c(minfreq,maxfreq), expand = c(0,0), breaks = seq(minfreq, maxfreq, freqinterval), label=scales::unit_format(unit = "K"))+
            ggplot2::labs(y="Frequency (Hz) \n", x="\nTime (hour of day)")+
            ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank(),
                  panel.background = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                  axis.text.x = ggplot2::element_text(color = "black", size = 10, angle = -0, hjust=1.1,vjust=1),
                  axis.title.y = ggplot2::element_text(face="bold"),
                  axis.title.x = ggplot2::element_text(face="bold"),
                  panel.border = ggplot2::element_rect(colour = "white", fill=NA, size=0.5),
                  axis.text.y = ggplot2::element_blank(),
                  axis.ticks.y = ggplot2::element_blank(),
                  plot.margin = grid::unit(c(1,1,1,1),"cm"),
                  legend.position = "top",
                  legend.direction = "horizontal",
                  legend.title = ggplot2::element_text(color = "black", size = 12, face = "bold"))+
            ggplot2::coord_polar()+
            ggplot2::geom_hline(yintercept = seq(minfreq, maxfreq, freqinterval), color="grey", alpha=0.5, size=0.2)+
            ggplot2::annotate(geom="label", size=labelsize_polar, y=seq(minfreq, maxfreq, freqinterval), x=min(df2$time),label=as.character(seq(minfreq, maxfreq, freqinterval)))+
            ggplot2::labs(fill="Proportion of acoustically active bins")
        }

        else{print("Error: invalid annotate argument - please consult package documentation for options")}
      }
    }

    else{print("Error: invalid type argument - please consult package documentation for options")}
  }

  if (marginplot==FALSE & interactive==FALSE&save==FALSE){plot}

    else{

      if (marginplot==FALSE & interactive==FALSE & save==TRUE){
        ggplot2::ggsave(filename=paste0(paste0(type, "_"),"no_margin_",filename,".",device), plot=plot, device=device, path=dir, dpi = "retina")
        plot
        }

      else{

        if (marginplot==FALSE & interactive==TRUE & save==FALSE){plotly::ggplotly(plot)}

        else{

          if (marginplot==FALSE & interactive==TRUE & save==TRUE){
                print("Error: cannot save interactive plot - saving regular plot instead")
            ggplot2::ggsave(filename=paste0(paste0(type, "_"),"no_margin_",filename,".",device), plot=plot, device=device, path=dir, dpi = "retina")
                plotly::ggplotly(plot)
          }

          else{

            if (marginplot==TRUE & type=="polar"){
      print("Error: margin plots not available for type polar - adjust type to regular")
              }

            else{

              if (marginplot==TRUE & type=="regular" & interactive==TRUE){
                print("Error: interactive mode not available for margin plot - check package documentation for options")
              }

              else{

                if (marginplot==TRUE & type=="regular" & interactive==FALSE){

                  heatmap=plot+ggplot2::theme(plot.margin = grid::unit(c(0.15, 0.15, 0, 0), "cm"),legend.position = "none")

                  xdata=as.data.frame(as.numeric(soundscape_richness(df,
                                                           type="tod",
                                                           date=date,
                                                           lat = lat,
                                                           lon=lon,
                                                           minfreq = minfreq,
                                                           maxfreq = maxfreq)))

                  xdata$time=hms::as_hms(as.POSIXct((strptime(paste(date, colnames(df), sep=" "), format= "%Y-%m-%d %H:%M:%S", tz=tz))))

                  colnames(xdata)=c("richness", "time")
                  xdata$richness_smooth=pracma::movavg(xdata$richness, 6, type="t")


                  xplot=ggplot2::ggplot(xdata, ggplot2::aes(time, richness_smooth))+
                    ggplot2::geom_area(alpha=0.25, fill="#440154FF")+
                    ggplot2::geom_line(color="black", size=1.2)+
                    ggplot2::ylab("Richness (%)")+
                    ggplot2::xlab(NULL)+
                    ggplot2::scale_x_time(expand = c(0,0))+
                    ggplot2::scale_y_continuous(expand = c(0,0))+
                    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                          axis.ticks.x = ggplot2::element_blank(),
                          axis.text.x = ggplot2::element_blank(),
                          panel.border = ggplot2::element_blank(),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.background = ggplot2::element_blank(),
                          plot.margin = grid::unit(c(0, 0, 0, 0), "cm"))

                  xplot

                  ydata=as.data.frame(as.numeric(soundscape_richness(df,
                                                                     type="total",
                                                                     date=date,
                                                                     lat = lat,
                                                                     lon=lon,
                                                                     freqseq = TRUE,
                                                                     nbins = nbins,
                                                                     minfreq = minfreq,
                                                                     maxfreq = maxfreq)))

                  ydata$frequency=seq(from=minfreq, maxfreq, (maxfreq/nbins))

                  colnames(ydata)=c("richness", "frequency")

                  ydata$richness_smooth=pracma::movavg(ydata$richness, 2, type="s")

                  yplot=ggplot2::ggplot(ydata, ggplot2::aes(frequency, richness_smooth))+
                    ggplot2::geom_area(alpha=0.40, fill="#FDE725FF")+
                    ggplot2::geom_line(color="black", size=1.5)+
                    ggplot2::ylab("Richness (%)")+
                    ggplot2::xlab(NULL)+
                    ggplot2::scale_x_continuous(expand = c(0,0), limits = c(0,(maxfreq+(maxfreq/10))))+
                    ggplot2::scale_y_continuous(expand = c(0,0))+
                    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                          axis.ticks.y = ggplot2::element_blank(),
                          axis.text.y = ggplot2::element_blank(),
                          panel.border = ggplot2::element_blank(),
                          panel.grid.major = ggplot2::element_blank(),
                          panel.grid.minor = ggplot2::element_blank(),
                          panel.background = ggplot2::element_blank(),
                          plot.margin = grid::unit(c(0, 0, 0, -0), "cm"))+
                    ggplot2::coord_flip()

                  yplot

                  combined_plot=xplot+patchwork::plot_spacer()+heatmap+yplot+patchwork::plot_layout(widths = c(3, 1), heights = c(1,3))

                  if (save==TRUE){
                    ggplot2::ggsave(filename=paste0(paste0(type, "_"),"marginplot_",filename,".",device), plot=combined_plot, device=device, path=dir, dpi = "retina")
                    combined_plot
                    }

                  else{

                    if (save==FALSE){combined_plot}
                  }
                }
              }
            }
          }
        }
      }
    }
}


#heatmapper(aggregate_df_list[[1]],
 #          xinterval = "1 hour",
  #         yinterval = 1000,
   #        ymax=20000,
    #       annotate = FALSE,
     #      date=metadata$date[1],
      #     lat = metadata$lat[1],
       #    lon=metadata$lon[1],
        #   direction=1,
         #  type="regular",
        #   palette = "A",
      #     marginplot = FALSE,
    #       interactive = FALSE,
  #         twilight = "sunlight",
#           save=FALSE,
 #          device="tiff",
  #         filename = "test4",
   #        dir="D:/User/Programs/Test")


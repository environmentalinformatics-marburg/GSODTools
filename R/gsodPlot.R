#' Plot GSOD data
#' 
#' @description
#' This function aims at visualizing GSOD data, both raw/adjusted for outliers and 
#' imputed, as well as linear trends of mean, minimum and maximum daily air
#' temperature.
#' 
#' @param fls_orig See argument \code{fls}. 
#' @param fls Character vector containing filepath(s) to the imputed GSOD data 
#' set(s), usually derived from \code{\link{gfLinInt}}, \code{\link{gfJulendat}} 
#' and/or \code{\link{gfSsa}}, or \code{list} containing \code{data.frame} objects
#' holding the data, or a single \code{data.frame} in case only one GSOD station
#' shall be visualized.
#' @param stations Character. Name(s) of the station(s) corresponding to \code{fls}
#' and \code{fls_orig} that will be displayed above each facet.
#' @param prm Character, default is "TEMP". Determines which parameter to
#' visualize. If \code{type = "trends"}, this argument will be ignored, and 
#' columns "TEMP", "MIN" and "MAX" must be available in \code{fls} and 
#' \code{fls_orig}.
#' @param ... Additional arguments passed to \code{\link{element_text}}.
#' 
#' @return
#' An object of class \code{ggplot}.
#' 
#' @author
#' Florian Detsch
#' 
#' @examples
#' library(foreach)
#' library(ggplot2)
#' 
#' # Visualize trends in daily mean, minimum, and maximum air temperature
#' lst = split(
#'   eastafrica
#'   , f = paste(
#'     eastafrica$PlotId
#'     , eastafrica$Status
#'     , sep = ","
#'   )
#' )
#' 
#' cleansed_data = lst[
#'   grep(
#'     "cleansed"
#'     , names(lst)
#'   )
#' ]
#' 
#' filled_data = lst[
#'   grep(
#'     "filled"
#'     , names(lst)
#'   )
#' ]
#' 
#' gsodPlot(
#'   cleansed_data
#'   , filled_data
#'   , stations = c("Jomo Kenyatta Intl. Airport", "Kilimanjaro Intl. Airport")
#'   , type = "trends"
#' )
#'  
#' @importFrom zoo read.zoo
#' 
#' @export
gsodPlot <- function(fls_orig = NULL,
                     fls, 
                     stations, 
                     prm = "TEMP",
                     type = "trends",
                     ...) {
  
  ## Data import
  
  # Initial GSOD data
  if (!is.null(fls_orig)) {
    ta.orig <- lapply(fls_orig, function(i) {
      tmp.ts <- zoo::read.zoo(
        i
        , sep = ","
        , header = TRUE
        , index.column = 3L
        , format = "%Y-%m-%d %H:%M:%S"
        , tz = "EAT"
        , regular = TRUE
      )
      # frequency(tmp.ts) <- 365
      return(tmp.ts)
    })
    
    # Reformat and append data
    ta.orig.df <- foreach(i = ta.orig, j = stations, .combine = "rbind") %do%
      data.frame(DATE = time(i), PLOT = j, Original = as.numeric(i[, prm]))
  }
  
  # Gap-filled GSOD data
  ta.gf <- lapply(fls, function(i) {
    ta.gf.ts <- zoo::read.zoo(
      i
      , sep = ","
      , header = TRUE
      , index.column = 3L
      , format = "%Y-%m-%d %H:%M:%S"
      , tz = "EAT"
      , regular = TRUE
    )
    return(ta.gf.ts)
  })
  
  
  ## Plotting stuff
  
  # Start and end date 
  start_year <- substr(ta.orig.df$DATE[1], 1, 4)
  start_date <- as.Date(paste0(start_year, "-01-01"))
  
  end_year <- substr(ta.orig.df$DATE[nrow(ta.orig.df)], 1, 4)
  end_date <- as.Date(paste0(end_year, "-12-31"))
  
  # Plot original and gap-filled GSOD data 
  if (type == "original") {
        
    ggplot(aes(x = DATE, y = Original), data = ta.orig.df) + 
      geom_line() + 
      facet_wrap(~ PLOT, ncol = 1) + 
      # scale_x_date(limits = c(start_date, end_date), 
      #              breaks = seq(start_date, end_date, "2 years"), 
      #              labels = scales::date_format("%Y"), minor_breaks = scales::date_breaks("1 year")) + 
      xlab("Time (d)") +  
      ylab(expression("Temperature ("~degree~C~")")) + 
      theme_bw() + 
      theme(text = element_text(...), 
            legend.key = element_rect(fill = "transparent"), 
            panel.grid.major = element_line(size = 1.2), 
            panel.grid.minor = element_line(size = 1.1))
    
  } else if (type == "both") {
    # Reformat and append gap-filled station data
    ta.gf.df <- foreach(i = ta.gf, j = stations, .combine = "rbind") %do%
      data.frame(DATE = time(i), PLOT = j, Imputed = as.numeric(i[, prm]))
    
    # Merge and meltoriginal and gap-filled data
    ta.orig.gf.df <- merge(ta.orig.df, ta.gf.df, all = TRUE, by = c(1, 2))
    ta.orig.gf.df.mlt <- melt(ta.orig.gf.df, id.vars = c(1, 2))
    
    ta.orig.gf.df.mlt$variable <- factor(ta.orig.gf.df.mlt$variable, 
                                         levels = c("Imputed", "Original"))
    
    ggplot(aes(x = DATE, y = value, group = variable, colour = variable), 
           data = ta.orig.gf.df.mlt) + 
      geom_line() + 
      facet_wrap(~ PLOT, ncol = 1) + 
      # scale_x_date(limits = c(start_date, end_date), 
      #              breaks = seq(start_date, end_date, "2 years"), 
      #              labels = scales::date_format("%Y"), minor_breaks = scales::date_breaks("1 year")) + 
      scale_colour_manual("", values = c("grey65", "black"), 
                          labels = c("Original data", "Imputed data"), 
                          breaks = c("Original", "Imputed")) +
      xlab("Time (d)") +  
      ylab(expression("Temperature ("~degree~C~")")) + 
      theme_bw() + 
      theme(text = element_text(...), 
            legend.key = element_rect(fill = "transparent"), 
            panel.grid.major = element_line(size = 1.2), 
            panel.grid.minor = element_line(size = 1.1))
        
    # Plot gap-filled GSOD data only  
  } else if (type == "trends") {  
    
    # Reformat, append and melt quality-controlled data
    ta.orig.df <- melt(foreach(i = ta.orig, j = stations, .combine = "rbind") %do%
                         data.frame(DATE = time(i), PLOT = j, MEAN = as.numeric(i$TEMP), 
                                    MAX = as.numeric(i$MAX), MIN = as.numeric(i$MIN)), 
                       id.vars = c(1, 2))
    
    # Reformat, append and melt gap-filled data
    ta.gf.df <- melt(foreach(i = ta.gf, j = stations, .combine = "rbind") %do%
                       data.frame(DATE = time(i), PLOT = j, MEAN = as.numeric(i$TEMP), 
                                  MAX = as.numeric(i$MAX), MIN = as.numeric(i$MIN)), 
                     id.vars = c(1, 2))
    
    # Reorder factor levels
    ta.orig.df$variable <- factor(ta.orig.df$variable, levels = c("MIN", "MEAN", "MAX"))
    ta.gf.df$variable <- factor(ta.gf.df$variable, levels = c("MIN", "MEAN", "MAX"))
    
    ggplot(aes(x = DATE, y = value, colour = variable, linetype = variable), 
           data = ta.gf.df) + 
      geom_line(data = subset(ta.gf.df, variable == "MEAN"), colour = "grey65") +
      geom_line(aes(x = DATE, y = value, colour = variable, linetype = variable),
                data = subset(ta.orig.df, variable == "MEAN"), 
                colour = "grey35") +
      stat_smooth(size = 1.2, method = "lm", se = FALSE) + 
      facet_wrap(~ PLOT, ncol = 1) + 
      # scale_x_date(
      #   limits = c(start_date, end_date)
      #   breaks = seq(start_date, end_date, "2 years")
      #   , labels = scales::date_format("%Y")
      #   , minor_breaks = scales::date_breaks("1 year")
      # ) +
      scale_linetype_manual("Linear trends of daily", 
                            values = c("dotted", "solid", "dotted"), 
                            labels = c("Minimum", 
                                       "Mean",                                      
                                       "Maximum")) +
      scale_colour_manual("Linear trends of daily", 
                          values = c("blue", "black", "red"), 
                          labels = c("Minimum", 
                                     "Mean",                                      
                                     "Maximum")) +
      labs(colour = "Linear trends of daily", 
           linetype = "Linear trends of daily") + 
      xlab("Time (d)") +  
      ylab(expression("Temperature ("~degree~C~")")) + 
      theme_bw() + 
      theme(text = element_text(...), 
            legend.key = element_rect(fill = "transparent"), 
            panel.grid.major = element_line(size = 1.2), 
            panel.grid.minor = element_line(size = 1.1))
  }
}
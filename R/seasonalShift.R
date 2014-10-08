#' Plot seasonal shifts in GSOD temperature data
#' 
#' @description
#' This function aims at visualizing seasonal shifts in GSOD data aggregated on 
#' a monthly level. The seasonal shifts are derived from a harmonic trend model 
#' fitted to the desired start and end range of a measurement series.  
#' 
#' @param fls Character vector containing filepath(s) to the GSOD data set(s) to
#' visualize, or \code{list} containing \code{data.frame} objects
#' holding the data, or a single \code{data.frame} in case only one GSOD station
#' shall be visualized.
#' @param start Character. A vector containing the temporal range of the start
#' interval to fit a harmonic trend model to.
#' @param end Character. A vector containing the temporal range of the end
#' interval to fit a harmonic trend model to.
#' @param prm Character, default is "TEMP". Determines which parameter to 
#' visualize. 
#' @param stations Character. Name(s) of the station(s) corresponding to 
#' \code{fls} that will be displayed above each facet.
#' @param ... Additional arguments passed to \code{\link{element_text}}.
#' 
#' @return
#' An object of class \code{ggplot}.
#' 
#' @author
#' Florian Detsch
#' 
#' @examples
#' # Load outlier-adjusted (*gsod*) and gap-filled (*ssa*) data sets from Nairobi 
#' # and Kilimanjaro Airport, 1980-2000
#' data("data_nairobi_kilimanjaro")
#' 
#' # Visualize seasonal shifts in monthly averaged air temperature
#' seasonalShift(fls = list(df_gsod_nairobi, df_gsod_kilimanjaro),  
#'               start = c("1980-01-01", "1983-12-31"), 
#'               end = c("1998-01-01", "2000-12-31"), 
#'               stations = c("Jomo Kenyatta Intl.", "Kilimanjaro Intl. Airport"), 
#'               prm = "MAX")
#'          
#' @export seasonalShift
#' @aliases seasonalShift
seasonalShift <- function(fls, 
                          start = c("1973-01-01", "1977-12-31"),
                          end = c("2008-01-01", "2012-12-31"), 
                          prm = "TEMP",
                          stations,
#                           cols = NULL, 
                          ...) {
  
  if (!"Rsenal" %in% rownames(installed.packages())) {
      stop("Package 'Rsenal' is required. Please install it from GitHub \n 
           library(devtools) \n
           install_github('environmentalinformatics-marburg/Rsenal')
           library(Rsenal)")
  } else {
    stopifnot(require(Rsenal))
  }
  
  # Import data
  tmp.all <- foreach(i = fls, j = stations, .combine = "rbind") %do% {
    
    Reduce(function(...) merge(..., by = c(1, 2), sort = FALSE), 
           foreach(k = list(start), l = list(end)) %do% {
      if (is.character(i)) {
        tmp <- read.csv(i, stringsAsFactors = FALSE)
      } else {
        tmp <- i
      }
      tmp$year <- as.Date(substr(tmp$Datetime, 1, 10))
      tmp$yearmon <- as.yearmon(tmp$year)
      
      tmp.st <- subset(tmp, year >= as.Date(k[1]) & year <= as.Date(k[2]))
      tmp.st.agg <- aggregate(tmp.st[, c("TEMP", "MAX", "MIN")], 
                              by = list(tmp.st$yearmon), 
                              FUN = function(...) mean(..., na.rm = TRUE))
      
      tmp.nd <- subset(tmp, year >= as.Date(l[1]) & year <= as.Date(l[2]))
      tmp.nd.agg <- aggregate(tmp.nd[, c("TEMP", "MAX", "MIN")], 
                              by = list(tmp.nd$yearmon), 
                              FUN = function(...) mean(..., na.rm = TRUE))
      
      tst.st <- vectorHarmonics(tmp.st.agg[, prm], frq = 12, fun = mean, m = 2,
                                st = c(as.numeric(substr(k[1], 1, 4)), 01), 
                                nd = c(as.numeric(substr(k[2], 1, 4)), 12))
      tst.nd <- vectorHarmonics(tmp.nd.agg[, prm], frq = 12, fun = mean, m = 2,
                                st = c(as.numeric(substr(l[1], 1, 4)), 01), 
                                nd = c(as.numeric(substr(l[2], 1, 4)), 12))
      
      tmp.df <- data.frame("station" = j, 
                           "month" = month.abb, 
                           "st" = tst.st, 
                           "nd" = tst.nd)
      
      index <- c(grep("^st$", names(tmp.df)), grep("^nd$", names(tmp.df)))
      names(tmp.df)[index] <- 
        c(paste(substr(k, 1, 4), collapse = "-"), 
          paste(substr(l, 1, 4), collapse = "-"))
      
      return(tmp.df)
    })
    
  }
  
  # Reformat data
  tmp.all <- melt(tmp.all, id.vars = 1:2)
  # Reorder factor levels of 'month' column
  tmp.all$month <- factor(tmp.all$month, levels = month.abb)
  
  
  ## ggplot

#   if (is.list(start) & is.list(end)) {
#     
#     label.st.1 <- paste(substr(start[[1]], 1, 4), collapse = "-")
#     label.st.2 <- paste(substr(start[[2]], 1, 4), collapse = "-")
#     
#     label.nd.1 <- paste(substr(end[[1]], 1, 4), collapse = "-")
#     label.nd.2 <- paste(substr(end[[2]], 1, 4), collapse = "-")
#     
#     # Color palette
#     if (is.null(cols)) {
#       greys <- brewer.pal(9, "Greys")
#       blues <- brewer.pal(9, "Blues")
#       
#       cols <- c(greys[3], blues[3], blues[9], greys[9])
#     }
#     
#     names(cols) <- c(label.st.1, label.st.2, label.nd.2, label.nd.1)
#     
#     ggplot(aes(x = month, y = value, group = variable, colour = variable, 
#                linetype = variable), data = tmp.all) + 
#       geom_line(lwd = 2) + 
#       facet_wrap(~ station, ncol = 1, scales = "free_y") + 
#       scale_colour_manual("", 
#                           values = cols, 
#                           breaks = c(label.st.1, label.st.2, label.nd.2, label.nd.1)) + 
#       scale_linetype_manual("", values = c(2, 2, 1, 1), 
#                             breaks = c(label.st.1, label.st.2, label.nd.2, label.nd.1)) + 
#       labs(list(x = "\nMonth", y = "Temperature [°C]\n")) + 
#       theme_bw() + 
#       theme(text = element_text(size = 15), 
#             panel.grid.major = element_blank(), 
#             panel.grid.minor = element_blank(),
#             legend.key = element_rect(fill = "transparent"))
#   } else {

    label.st <- paste(substr(start, 1, 4), collapse = "-")
    label.nd <- paste(substr(end, 1, 4), collapse = "-")
    
    ggplot(aes(x = month, y = value, colour = variable, group = variable), 
           data = tmp.all) + 
      geom_line(lwd = 1) + 
      facet_wrap(~ station, ncol = 1, scales = "free_y") + 
      scale_colour_manual("", values = c("cornflowerblue", "red2"), 
                          labels = c(label.st, label.nd)) + 
      labs(list(x = "\nMonth", y = "Temperature [°C]\n")) + 
      theme_bw() + 
      theme(text = element_text(...), 
            legend.key = element_rect(fill = "transparent"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank())
  
#   }
  
}

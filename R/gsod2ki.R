#' Convert GSOD data to Julendat standard format
#' 
#' @description
#' This function converts GSOD data from native format to standard KiLi SP1 format 
#' (Julendat, see https://code.google.com/p/julendat/) and, optionally, to an 
#' object of class \code{ki.data}. 
#' 
#' @param data \code{data.frame}. Input data, e.g. from \code{\link{dlGsodStations}}. 
#' @param date_col Character or numeric, default is "YEARMODA". Indicates the 
#' date column.
#' @param prm_col Character or numeric, default is "TEMP". Indicates the 
#' parameter column(s).
#' @param time_step Character, default is "day". Indicates the time step between
#' two measurements.
#' @param timezone Character, default is "NA".
#' @param aggtime Character, default is "-999".
#' @param plot_id Character, default is "NA". 
#' @param ep_plot_id Character, default is "xxx".
#' @param station_id Character, default is "NA".
#' @param proc_level Numeric, default is -999.
#' @param qual_flag Numeric, default is "NA".
#' @param save_output Logical, default is FALSE. If TRUE, a local copy of the 
#' reformatted GSOD data will be created based on the settings provided to 
#' \code{...}
#' @param df2ki Logical, default is FALSE. If TRUE, the reformatted data is 
#' converted to an object of class \code{ki.data}.  
#' @param ... Additional arguments passed to \code{\link{write.csv}}.
#' 
#' @return
#' An object of class \code{data.frame} or, if \code{df2ki = TRUE}, an object of 
#' class \code{ki.data}.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link{as.ki.data}}
#' 
#' @examples
#' library(dplyr)
#' 
#' data(gsodstations)
#' moshi <- filter(gsodstations, STATION.NAME == "MOSHI")
#' 
#' gsod_moshi <- dlGsodStations(usaf = moshi$USAF, 
#'                              start_year = 1990, end_year = 1995, 
#'                              dsn = paste0(getwd(), "/data/moshi/"), 
#'                              unzip = TRUE)
#' 
#' # Conversion to KiLi SP1 Julendat standard format
#' jul_moshi <- gsod2ki(data = gsod_moshi, 
#'                      prm_col = c("TEMP", "MIN", "MAX"), 
#'                      df2ki = FALSE)
#' 
#' plot(jul_moshi$TEMP, type = "l", lwd = 2, 
#'      ylim = c(min(jul_moshi$MIN, na.rm = TRUE), max(jul_moshi$MAX, na.rm = TRUE)))
#' lines(jul_moshi$MIN, col = "blue", lty = 2)
#' lines(jul_moshi$MAX, col = "red", lty = 2)
#' 
#' # Conversion to `ki.data` object
#' ki_moshi <- gsod2ki(data = gsod_moshi, 
#'                     prm_col = c("TEMP", "MIN", "MAX"), 
#'                     df2ki = TRUE)
#' 
#' str(ki_moshi)
#' 
#' @export gsod2ki
#' @aliases gsod2ki
gsod2ki <- function(data,
                    date_col = "YEARMODA",
                    prm_col = "TEMP", 
                    time_step = "day",
                    timezone = "NA",
                    aggtime = "-999",
                    plot_id = "NA",
                    ep_plot_id = "xxx",
                    station_id = "NA",
                    proc_level = -999,
                    qual_flag = "NA",
                    save_output = FALSE,
                    df2ki = FALSE,
                    ...) {
  
  # Convert parameter column into continuous Date object
  st <- as.Date(paste0(substr(data[1, date_col], 1, 4), "-01-01"))
  nd <- as.Date(paste0(substr(data[nrow(data), date_col], 1, 4), "-12-31"))
  st_nd <- seq(st, nd, "day")
  
  # Subset GSOD data by relevant columns
  data.ts.prm <- data[, c(date_col, prm_col)]
  data.ts.prm[, date_col] <- as.Date(as.character(data.ts.prm[, date_col]), 
                                        format = "%Y%m%d")
  
  # Merge continuous Date object with available GSOD measurements
  data.ts <- merge(data.frame(st_nd), data.ts.prm, by = 1, all.x = TRUE)
  names(data.ts) <- names(data.ts.prm)
  
  # Merge generated data
  data.ts <- data.frame(Datetime = if (time_step == "day") {
                          paste(data.ts[, date_col], "12:00:00") 
                        } else {
                          index(data.ts)
                        },
                        Timezone = rep(timezone, nrow(data.ts)),
                        Aggregationtime = rep(aggtime, nrow(data.ts)),
                        PlotId = rep(plot_id, nrow(data.ts)),
                        EpPlotId = rep(ep_plot_id, nrow(data.ts)),
                        StationId = rep(station_id, nrow(data.ts)),
                        Processlevel = rep(proc_level, nrow(data.ts)),
                        Qualityflag = rep(qual_flag, nrow(data.ts)),
                        data.ts[, prm_col], 
                        stringsAsFactors = FALSE)
  
  # Adjust column names
  if (is.character(prm_col))
    prm_col_id <- sapply(prm_col, function(i) which(i == names(data)))

  names(data.ts)[9:ncol(data.ts)] <- names(data)[prm_col_id]
  
  # Save reformatted data (optional)
  if (save_output)
    write.csv(data.ts, ...)
  
  # Convert to `ki.data` object (optional)
  if (df2ki)
    data.ts <- as.ki.data(data.ts)
    
  return(data.ts)
}

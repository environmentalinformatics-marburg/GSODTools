#' Extract and merge downloaded GSOD data
#' 
#' @description
#' Extract and merge previously downloaded GSOD data based on a unique USAF
#' station code and a supplied temporal range.
#' 
#' @param usaf Numeric. A unique USAF station code. It can either be manually 
#' determined from \code{data(gsodstations)}, or retreived from spatial 
#' subsetting (see \code{\link{stationFromCoords}}, 
#' \code{\link{stationFromExtent}}).
#' @param dsn Character, default is the current working directory. Destination 
#' folder for data download. 
#' @param start_year Numeric. The desired year to start data extraction and 
#' concatenation. If not supplied, all files in \code{dsn} will be considered. 
#' @param end_year Numeric. The desired year to stop data extraction and
#' concatenation. If not supplied, all files in \code{dsn} will be considered. 
#' @param save_output Logical, default is FALSE. If TRUE, a local copy of the 
#' concatenated GSOD data will be created based on the settings provided to \code{...} 
#' @param rm_gz Logical, default is FALSE. If TRUE, *.gz files are removed after
#' extraction and concatenation. If \code{unzip = FALSE}, this argument is ignored. 
#' @param ... Additional arguments passed to \code{\link{write.table}}.
#' 
#' @return
#' An object of class \code{data.frame}. 
#' 
#' @author
#' Florian Detsch
#' 
#' @examples
#' library(dplyr)
#' 
#' data(gsodstations)
#' moshi <- filter(gsodstations, STATION.NAME == "MOSHI")
#' 
#' # Download data from Moshi, Tanzania, from 1990 to 1995
#' gsod_moshi <- gzGsodStations(usaf = moshi$USAF, 
#'                              start_year = 1990, end_year = 1995, 
#'                              dsn = paste0(getwd(), "/data/moshi/"), 
#'                              save_output = TRUE, 
#'                              file = paste0(getwd(), "/data/moshi/moshi_1990_1995.csv"), 
#'                              row.names = FALSE)
#' 
#' # Plot temperature data (but: time series not continuous!)                                                         
#' plot(gsod_moshi$TEMP, type = "l")
#' 
#' @export gzGsodStations
#' @aliases gzGsodStations
gzGsodStations <- function(usaf, 
                           dsn = ".",
                           start_year = NULL, 
                           end_year = NULL, 
                           save_output = FALSE,
                           rm_gz = FALSE,
                           ...) {
  
  # List available *.gz files
  fls <- list.files(dsn, pattern = paste(usaf, ".gz$", sep = ".*"), 
                    full.names = TRUE)
  
  # Subset files by supplied temporal range (in case *.gz files from previous
  # operations are present in destination folder)
  start_year <- as.numeric(start_year)
  end_year <- as.numeric(end_year)
  index <- sapply(seq(start_year, end_year), function(i) {
    grep(i, fls)
  })
  fls <- fls[index]
  
  # Loop through all available files with valid records, i.e. with at least one
  # valid measurement
  index <- sapply(fls, function(i) length(readLines(i)) > 0)
  df.all <- do.call("rbind", lapply(fls[index], function(i) {
    # Import fixed width formatted data
    df <- read.fwf(i, widths = gsodColWidth(), header = FALSE, skip = 1, 
                   fill = TRUE, na.strings = c("999.9", "9999.9", "99.99"),
                   stringsAsFactors = FALSE)
    
    # Remove redundant columns
    df <- df[, -c(seq(2, 34, 2), 37, 40, 43, 44)]
    
    # Set column names
    names(df) <- c("STN---", "WBAN", "YEARMODA", "TEMP", "NC", "DEWP", "NC", 
                   "SLP", "NC", "STP", "NC", "VISIB", "NC", "WDSP", "NC", 
                   "MXSPD", "GUST", "MAX", "MAXFLAG", "MIN", "MINFLAG", "PRCP", 
                   "PRCPFLAG", "SNDP", "FRSHTT")
    
    # Return annual data per station
    return(df)
  }))
  
  # Save (optional) and return merged annual data per station
  if (save_output)
    write.table(df.all, ...)
  
  # Optionally remove *.gz files
  if (rm_gz)
    file.remove(fls)
  
  # Return merged annual data for all stations
  return(df.all)
}
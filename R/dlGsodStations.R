#' Download (and extract) GSOD data
#' 
#' @description
#' Download (and extract) data from a GSOD station based on its unique USAF code.
#' 
#' @param usaf Numeric. A unique USAF station code. It can either be manually 
#' determined from \code{data(gsodstations)}, or retreived from spatial 
#' subsetting (see \code{\link{stationFromCoords}}, 
#' \code{\link{stationFromExtent}}).
#' @param start_year Numeric. The desired year to start data acquisition. If not 
#' supplied, download starts from the first year of measurement. 
#' @param end_year Numeric. The desired year to stop data acquisition. If not 
#' supplied, download stops at the last year of measurement. 
#' @param dsn Character, default is the current working directory. Destination 
#' folder for data download. 
#' @param unzip Logical, default is FALSE. If TRUE, *.gz files are unpacked and 
#' merged after download. 
#' @param rm_gz Logical, default is FALSE. If TRUE, *.gz files are removed after
#' extraction. If \code{unzip = FALSE}, this argument is ignored. 
#' @param ... Additional arguments passed to \code{\link{gzGsodStations}}.
#' 
#' @return
#' A vector of *.gz filenames, or an object of class \code{data.frame} if 
#' \code{unzip = TRUE}.
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
#' gsod_moshi <- dlGsodStations(usaf = moshi$USAF, 
#'                              start_year = 1990, end_year = 1995, 
#'                              dsn = paste0(getwd(), "/data/moshi/"), 
#'                              unzip = TRUE)
#' 
#' # Plot temperature data (but: time series not continuous!)                                                         
#' plot(gsod_moshi$TEMP, type = "l")
#' 
#' @export dlGsodStations
#' @aliases dlGsodStations
dlGsodStations <- function(usaf,
                           start_year = NA, 
                           end_year = NA,
                           dsn = ".", 
                           unzip = FALSE, 
                           rm_gz = FALSE,
                           ...) {
  
  # Set `rm_gz = FALSE` in case `unzip = FALSE`
  rm_gz <- ifelse(unzip, rm_gz, FALSE)
  
  # Load GSOD station list
  data(gsodstations)
  locations <- gsodstations
  
  # Extract desired station from list of GSOD stations
  dl_usaf <- locations %>% filter(USAF == usaf)

  # If not supplied, set start_year and end_year to whole temporal range of
  # available measurements
  start_measurement <- as.numeric(substr(dl_usaf$BEGIN, 1, 4))
  end_measurement <- as.numeric(substr(dl_usaf$END, 1, 4))

  if (is.na(start_year))
    start_year <- start_measurement
  if (is.na(end_year))
    end_year <- end_measurement
  
  # Else, verify user-defined temporal range and adjust start year / end year
  # if `start_year < start_measurement` / `end_year > end_measurement`
  start_year <- max(start_year, start_measurement)
  end_year <- min(end_year, end_measurement)
  
  # Throw error in case of false user input, i.e. `start_year > end_year`
  if (start_year > end_year)
    stop("Skipping GSOD station ", dl_usaf$USAF, " (", dl_usaf$STATION.NAME, 
         "): Start year is higher than end year!", sep = "")
  
  cat("Processing GSOD station ", dl_usaf$USAF, " (", 
      as.character(dl_usaf$STATION.NAME), ") ... \n", sep = "")
  
  # Download op.gz of current station per year
  fls_gz <- sapply(start_year:end_year, function(year) {
    # Basename of both URL and destfile
    dlbase <- paste0(dl_usaf$USAF, "-", dl_usaf$WBAN, "-", year, ".op.gz")
    # URL
    dlurl <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/gsod/", year, "/", 
                    dlbase)
    # Destination file
    dlfile <- paste0(dsn, "/", dlbase)
    
    # Check if destination file already exists, else proceed to download
    if (file.exists(dlfile)) {
      cat("File", dlfile, "already exists. Proceeding to next file... \n")
    } else {      
      try(download.file(dlurl, dlfile), silent = FALSE)
    }
    
    return(dlfile)
  })
    
  # Unzip downloaded files (optional)
  if (unzip) {
    
    df <- gzGsodStations(usaf = usaf, 
                         dsn = dsn, 
                         start_year = start_year, 
                         end_year = end_year, 
                         ...)
    return(df)
    
  } else {
    
    # Return downloaded .gz files
    return(fls_gz)
  }
}
#' Extract and merge downloaded GSOD data
#' 
#' @description
#' Extract and merge previously downloaded GSOD data based on a unique USAF
#' station code and a supplied temporal range.
#' 
#' @param usaf Numeric. A unique USAF station code. It can either be manually 
#' determined from built-in \code{gsodstations}, or retrieved from spatial 
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
#' \dontrun{
#' moshi <- subset(gsodstations, `STATION NAME` == "MOSHI")
#' 
#' #' # download data from moshi, tanzania, from 1990 to 1995
#' jnk = dlGsodStations(
#'   usaf = moshi$USAF
#'   , start_year = 1990
#'   , end_year = 1995
#'   , dsn = tempdir()
#' )
#' 
#' # Download data from Moshi, Tanzania, from 1990 to 1995
#' gsod_moshi <- gzGsodStations(usaf = moshi$USAF, 
#'                              start_year = 1990, end_year = 1995, 
#'                              dsn = tempdir(),
#'                              save_output = TRUE, 
#'                              file = file.path(tempdir(), "moshi_1990_1995.csv"), 
#'                              row.names = FALSE)
#' 
#' # Plot temperature data (but: time series not continuous!)
#' plot(gsod_moshi$TEMP, type = "l")
#' }
#' 
#' @importFrom stats setNames
#' @importFrom utils read.fwf
#' 
#' @export
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
  df.all = do.call(
    rbind
    , lapply(
      fls[index]
      , \(i) {
        
        # Import fixed width formatted data
        df = utils::read.fwf(
          i
          , widths = gsodColWidth()
          , skip = 1L
          , fill = TRUE
          , na.strings = c("999.9", "9999.9", "99.99", " ")
          , colClasses = gsodColClasses()
        )
        
        # Import column names
        nms = utils::read.fwf(
          i
          , widths = gsodColWidth()
          , n = 1L
        )
        
        idx = !is.na(nms)
        nms[idx] = gsub("\\s*", "", nms[idx])
        
        # Name flag columns as such
        for (i in c("MAX", "MIN", "PRCP")) {
          nms[
            which(nms == i) + 1L
          ] = paste0(i, "FLAG")
        }
        
        # Remove nameless columns
        df = df[
          , idx
        ] |> 
          # Set column names
          stats::setNames(
            nms[idx]
          ) |> 
          # Convert 'YEARMODA' to `Date`
          transform(
            YEARMODA = as.Date(
              YEARMODA
              , format = "%Y%m%d"
            )
          )
        
        # Return annual data per station
        return(df)
      }
    )
  )
  
  # Save (optional) and return merged annual data per station
  if (save_output)
    write.table(df.all, ...)
  
  # Optionally remove *.gz files
  if (rm_gz)
    file.remove(fls)
  
  # Return merged annual data for all stations
  return(df.all)
}
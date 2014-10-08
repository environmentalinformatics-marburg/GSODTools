#' Reformat list of available GSOD stations 
#' 
#' @description
#' This function is intended to reformat selected columns, i.e. elevation and
#' coordinates, from the initial list of available GSOD stations (see 
#' ftp://ftp.ncdc.noaa.gov/pub/data/gsod/ish-history.csv). Optionally, the
#' corresponding \code{data.frame} can then be converted to an object of class
#' \code{SpatialPointsDataFrame} for further processing. 
#' 
#' @param data \code{data.frame}. Usually \code{data("gsodstations")}.
#' @param elevation Logical, default is TRUE. Determines whether elevation is
#' converted from decimeters to meters.
#' @param coords Logical, default is TRUE. Determines whether coordinates are
#' converted from thousandth of a degree longitude/latitude to whole degrees.
#' @param rm_invalid_coords Logical, default is TRUE. Determines whether records
#' with invalid coordinates, i.e. latitudes > 90 / < -90 degree and longitude > 
#' 180 / < -180 degree, are removed from the data set. Note: if \code{coords = FALSE}, 
#' this argument will automatically be set to FALSE.
#' @param df2sp Logical, default is FALSE. If TRUE, the list of available GSOD
#' stations is converted from \code{data.frame} to \code{SpatialPointsDataFrame} object. 
#' @param ... Additional arguments. Currently not in use.
#' 
#' @return
#' An object of class \code{data.frame}, or an object of class 
#' \code{SpatialPointsDataFrame} if \code{df2sp = TRUE}.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{gsodDf2Sp}}
#' 
#' @examples
#' # Load data
#' data(gsodstations)
#' 
#' # Reformat elevation and coordinates, and convert to spatial object.
#' gsod_shp <- gsodReformat(data = gsodstations, 
#'                          elevation = TRUE, 
#'                          coords = TRUE, 
#'                          rm_invalid_coords = TRUE, 
#'                          df2sp = TRUE)
#'                          
#' plot(gsod_shp)                        
#' 
#' @export gsodReformat
#' @aliases gsodReformat
gsodReformat <- function(data, 
                         elevation = TRUE, 
                         coords = TRUE, 
                         rm_invalid_coords = TRUE,
                         df2sp = FALSE,
                         ...) {

  # Reformat elevation (optional)
  if (elevation)
    data$ELEV..1M. <- data$ELEV..1M. / 10
  
  # Reformat coordinates (optional)
  if (coords) {
    for (i in c("LON", "LAT")) {
      data[, i] <- data[, i] / 1000
    }
  } else {
    rm_invalid_coords <- FALSE
  }
  
  # Eliminate inconsistent coordinates (optional)
  if (rm_invalid_coords) {
    for (i in c("LON", "LAT")) {
      data[data[, i] < ifelse(i == "LON", -180, -90) & 
             !is.na(data[, i]), i] <- NA
      data[data[, i] > ifelse(i == "LON", 180, 90) & 
             !is.na(data[, i]), i] <- NA
    }
  }
  
  # Convert data frame to spatial object (optional)
  if (df2sp)
    data <- gsodDf2Sp(data = data)
  
  # Return reformatted data
  return(data)
}

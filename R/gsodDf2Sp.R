#' Convert GSOD station list to spatial object
#' 
#' @description
#' This function converts the list of available GSOD stations from \code{data.frame}
#' to \code{SpatialPointsDataFrame}. 
#' 
#' @param data \code{data.frame}. Usually built-in \code{gsodstations}.
#' @param ... Additional arguments. Currently not in use.
#' 
#' @return
#' An object of class \code{SpatialPointsDataFrame}.
#' 
#' @author
#' Florian Detsch
#' 
#' @examples
#' # Convert station list to spatial object
#' gsod_shp <- gsodDf2Sp(data = gsodstations)
#' 
#' if (interactive()) {
#'   plot(gsod_shp)
#' }
#' 
#' @importFrom sf st_as_sf
#' 
#' @export
gsodDf2Sp <- function(data, 
                      ...) {
  
  LON = LAT = NULL
  
  # Subset data by valid coordinates and convert to SpatialPointsDataFrame
  data.lonlat <- subset(data, !is.na(LON) & !is.na(LAT))
  
  sf::st_as_sf(
    data.lonlat
    , coords = c("LON", "LAT")
    , crs = 4326
  )
}
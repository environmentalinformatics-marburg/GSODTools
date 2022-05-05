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
#' # Reformat station list
#' gsod_df <- gsodReformat(gsodstations, df2sp = FALSE)
#' 
#' # Convert station list to spatial object
#' gsod_shp <- gsodDf2Sp(data = gsod_df)
#'                          
#' plot(gsod_shp)                        
#' 
#' @importFrom methods as
#' @importFrom sf st_as_sf
#' 
#' @export gsodDf2Sp
#' @aliases gsodDf2Sp
gsodDf2Sp <- function(data, 
                      ...) {

  # Subset data by valid coordinates and convert to SpatialPointsDataFrame
  data.lonlat <- subset(data, !is.na(LON) & !is.na(LAT))
  
  data.lonlat = sf::st_as_sf(
    data.lonlat
    , coords = c("LON", "LAT")
    , crs = 4326
  )
  
  return(
    methods::as(
      data.lonlat
      , Class = "Spatial"
    )
  )
}
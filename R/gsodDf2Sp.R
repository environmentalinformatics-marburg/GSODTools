#' Convert GSOD station list to spatial object
#' 
#' @description
#' This function converts the list of available GSOD stations from \code{data.frame}
#' to \code{SpatialPointsDataFrame}. 
#' 
#' @param data \code{data.frame}. Usually \code{data("gsodstations")}.
#' @param ... Additional arguments. Currently not in use.
#' 
#' @return
#' An object of class \code{SpatialPointsDataFrame}.
#' 
#' @author
#' Florian Detsch
#' 
#' @examples
#' # Load data
#' data(gsodstations)
#' 
#' # Reformat station list
#' gsod_df <- gsodReformat(gsodstations, df2sp = FALSE)
#' 
#' # Convert station list to spatial object
#' gsod_shp <- gsodDf2Sp(data = gsod_df)
#'                          
#' plot(gsod_shp)                        
#' 
#' @export gsodDf2Sp
#' @aliases gsodDf2Sp
gsodDf2Sp <- function(data, 
                      ...) {

  # Subset data by valid coordinates and convert to SpatialPointsDataFrame
  data.lonlat <- subset(data, !is.na(LON) & !is.na(LAT))
  
  coordinates(data.lonlat) <- ~ LON + LAT
  proj4string(data.lonlat) <- CRS("+init=epsg:4326")
  
  return(data.lonlat)
}
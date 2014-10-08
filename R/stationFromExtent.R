#' Identify GSOD stations within user-drawn extent
#' 
#' @description
#' This function allows the selection of a subset of GSOD stations based on a 
#' user-drawn map extent.  
#' 
#' @param mapRegion Character, default is "world". The geographic extent of the 
#' displayed map on which the intended bounding box is to be drawn. See 
#' \code{\link{mapGriddedData}} for details. Note: in case the user supplies an 
#' object of class \code{extent}, this argument will automatically be ignored.
#' @param bb Object of class \code{extent}. Usually NULL and constructed from 
#' the user-drawn bounding box. If supplied, a subset of GSOD stations will be
#' created without any further user-specified input. 
#' @param ... Additional arguments. Currently not in use.
#' 
#' @return
#' An object of class \code{SpatialPointsDataFrame}.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link{mapGriddedData}}, \code{\link{crop}}
#' 
#' @examples
#' # Predefined bounding box around Mt. Kilimanjaro region
#' kili <- extent(c(37, 37.72, -3.4, -2.84))
#' gsod_shp_kili <- stationFromExtent(bb = kili)
#' 
#' mapGriddedData(mapRegion = "africa", plotData = FALSE, borderCol = "black", 
#'                addLegend = FALSE)
#' points(gsod_shp_kili, col = "red")
#' 
#' # User-drawn bounding box in Germany
#' gsod_shp_dtl <- stationFromExtent(mapRegion = "Germany")
#' points(gsod_shp_dtl, col = "red")                          
#' 
#' @export stationFromExtent
#' @aliases stationFromExtent
stationFromExtent <- function(mapRegion = "world",
                              bb = NULL, 
                             ...) {
 
  # Manually draw bounding box if not supplied
  if (is.null(bb)) {

    # Display plain map of desired extent
    mapGriddedData(mapRegion = mapRegion, addLegend = FALSE, plotData = FALSE, 
                   borderCol = "black")
    
    # Draw extent on map
    bb <- drawExtent()
  }
  
  # Crop available stations by drawn extent and return
  stations <- gsodReformat(data = gsodstations, df2sp = TRUE)
  stations <- crop(stations, bb)
  
  return(stations)
}
  
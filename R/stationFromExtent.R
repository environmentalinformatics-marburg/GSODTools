#' Identify GSOD stations within spatial extent
#' 
#' @description
#' This function allows the selection of a subset of GSOD stations based on a 
#' given map extent.
#' 
#' @param bb Object of class \code{extent}. Usually NULL and constructed from 
#' the user-drawn bounding box. If supplied, a subset of GSOD stations will be
#' created without any further user-specified input. 
#' @param ... Currently not used.
#' 
#' @return
#' A \code{sf} object.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link[sf]{st_crop}}
#' 
#' @examples
#' # Predefined bounding box around Mt. Kilimanjaro region
#' kili <- sf::st_bbox(c(xmin = 37, xmax = 37.72, ymin = -3.4, ymax = -2.84))
#' gsod_shp_kili <- stationFromExtent(bb = kili)
#' 
#' rworldmap::mapGriddedData(mapRegion = "africa", plotData = FALSE, borderCol = "black", 
#'                addLegend = FALSE)
#' points(sf::st_coordinates(gsod_shp_kili), col = "red")
#' 
#' @importFrom rworldmap mapGriddedData
#' 
#' @export
stationFromExtent <- function(bb, 
                             ...) {
  
  # Crop available stations by drawn extent and return
  stations <- gsodReformat(data = gsodstations, df2sp = TRUE)
  
  suppressWarnings(
    sf::st_crop(stations, bb)
  )
}
  
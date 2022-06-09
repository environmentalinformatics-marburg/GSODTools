#' Identify GSOD stations in the vicinity of a given geographic location
#' 
#' @description
#' This function allows the selection of a subset of GSOD stations based on a
#' specified location and a desired buffer size. 
#' 
#' @param x Numeric. A a single number specifying the x (longitude) coordinate, 
#' or a vector of two numbers specifying the x and y (latitude) coordinate, or
#' an object of class \code{SpatialPoints}. 
#' @param y Numeric, default is NULL. A single number specifying the y 
#' coordinate. Note: if a vector of two numbers is supplied to the previous 
#' argument, this parameter will automatically be ignored. 
#' @param width Numeric, default is 50. The desired buffer width (km) that will 
#' be applied to the specified coordinate.
#' @param ... Additional arguments passed to \code{\link{round}}.
#' 
#' @return
#' A \code{sf} object.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link[gmt]{geodist}}
#' 
#' @examples
#' # Identify GSOD stations that lie within a distance of 100 km around Kibo summit
#' gsod_shp <- stationFromCoords(x = 37.359031, 
#'                               y = -3.065053, 
#'                               width = 100)
#'
#' rworldmap::mapGriddedData(mapRegion = "africa", plotData = FALSE, borderCol = "black", 
#'                addLegend = FALSE)
#' points(sf::st_coordinates(gsod_shp), col = "red", pch = 20, cex = 2)
#'  
#' # Alternatively
#' gsod_shp <- stationFromCoords(x = c(37.359031, -3.065053), 
#'                               width = 100)
#' 
#' # Alternatively
#' kibo = sf::st_as_sf(
#'   data.frame(
#'     x = 37.359031
#'     , y = -3.065053
#'   )
#'   , crs = 4326
#'   , coords = c("x", "y")
#' )
#' 
#' gsod_shp <- stationFromCoords(x = kibo, 
#'                               width = 100)
#' 
#' @export
stationFromCoords <- function(x, 
                              y = NULL, 
                              width = 50, 
                              ...) {
  
  DIST = NULL
  
  if (is.numeric(x) & length(x) > 1) {
    y <- x[2]
    x <- x[1]
  } else if (inherits(x, c("SpatialPoints", "sf"))) {
    y <- sf::st_coordinates(x)[, 2]
    x <- sf::st_coordinates(x)[, 1]
  }
  
  # Calculate distance from point of interest to supplied stations
  # TODO: what happens if there are 2+ points in input
  stations <- gsodstations
  x.to.stations <- gmt::geodist(
    Nfrom = y
    , Efrom = x
    , Nto = stations$LAT, Eto = stations$LON
  )
  # Add calculated distances to stations
  stations$DIST <- round(x.to.stations, ...)
  
  # Find stations that lie within the given buffer width and convert to spatial
  stations = stations |> 
    subset(
      DIST <= width
    ) |> 
    gsodDf2Sp()
  
  # Sort by distance and return
  stations[
    order(stations$DIST)
    ,
  ]
}
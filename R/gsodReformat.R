#' Reformat list of available GSOD stations 
#' 
#' @description
#' This function is intended to convert the initial list of available GSOD 
#' stations (see \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv}) to
#' an object of class \code{sf} for further processing. 
#' 
#' @param data \code{data.frame}. Usually built-in \code{gsodstations}.
#' @param df2sp Logical, default is `FALSE`. If `TRUE`, the list of available 
#' GSOD stations is converted to \code{sf}. 
#' @param ... Additional arguments. Currently not in use.
#' 
#' @return
#' An object of class \code{data.frame}, or a \code{sf} object if 
#' \code{df2sp = TRUE}.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{gsodDf2Sp}}
#' 
#' @examples
#' # Reformat coordinates and convert to spatial object
#' gsod_shp <- gsodReformat(data = gsodstations, 
#'                          coords = TRUE, 
#'                          rm_invalid_coords = TRUE, 
#'                          df2sp = TRUE)
#' 
#' if (interactive()) {
#'   plot(gsod_shp)
#' }
#' 
#' @export
gsodReformat <- function(data, 
                         df2sp = FALSE,
                         ...) {
  
  # Convert data frame to spatial object (optional)
  if (df2sp)
    data <- gsodDf2Sp(data = data)
  
  # Return reformatted data
  return(data)
}

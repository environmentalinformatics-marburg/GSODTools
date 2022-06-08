#' Reformat list of available GSOD stations 
#' 
#' @description
#' This function is intended to convert the initial list of available GSOD 
#' stations (see \url{ftp://ftp.ncdc.noaa.gov/pub/data/noaa/isd-history.csv}) to
#' an object of class \code{sf} for further processing. 
#' 
#' @param data \code{data.frame}. Usually built-in \code{gsodstations}.
#' @param df2sp Logical, default is \code{FALSE}. If {TRUE}, the list of 
#' available GSOD stations is converted to \code{sf}. 
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
#' \code{\link{GSODTools-deprecated}}
#' 
#' @examples
#' # Reformat coordinates and convert to spatial object
#' gsod_shp <- suppressWarnings(
#'   gsodReformat(
#'     data = gsodstations
#'     , df2sp = TRUE
#'   )
#' )
#' 
#' if (interactive()) {
#'   plot(gsod_shp)
#' }
#' 
#' @name gsodReformat-deprecated
#' @keywords internal
NULL

#' @rdname GSODTools-deprecated
#' @section \code{gsodReformat}:
#'   For \code{gsodReformat}, use \code{\link{gsodDf2Sp}} instead.
#' 
#' @export
gsodReformat = function(
  data
  , df2sp = FALSE
  , ...
) {
  
  .Deprecated(
    "gsodDf2Sp"
    , package = "GSODTools"
  )
  
  # Convert data frame to spatial object (optional)
  if (df2sp) {
    data = gsodDf2Sp(
      data = data
    )
  }
  
  # Return reformatted data
  return(data)
}

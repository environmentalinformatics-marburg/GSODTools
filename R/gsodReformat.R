#' Reformat list of available GSOD stations 
#' 
#' @description
#' This function is intended to convert the initial list of available GSOD 
#' stations (see <https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv>) to
#' an object of class `sf` for further processing. 
#' 
#' @param data `data.frame`. Usually built-in [gsodstations].
#' @param df2sp `logical`, default is `FALSE`. If `TRUE`, the list of available 
#'   GSOD stations is converted to `sf`. 
#' @param ... Currently not used.
#' 
#' @return
#' An object of class `data.frame`, or a `sf` object if `df2sp = TRUE`.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso [GSODTools-deprecated]
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
#' @section `gsodReformat`:
#'   For `gsodReformat`, use [gsodDf2Sp()] instead.
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

#' Fixed column widths of GSOD ASCII data
#' 
#' @description
#' This function returns the native column widths of GSOD ASCII data, e.g. for
#' use with \code{\link{read.fwf}}.
#' 
#' @param ... Additional arguments. Currently not in use.
#' 
#' @return
#' A numeric vector containing fixed column width of GSOD ASCII data.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link{read.fwf}}

#' @examples
#' gsodColWidth()
#' 
#' @export gsodColWidth
#' @aliases gsodColWidth
gsodColWidth <- function(...) {
  
  return(c(6, 1, 5, 2, 8, 2, 6, 1, 2, 2, 6, 1, 2, 2, 6, 1, 2, 2, 6, 1, 2, 2, 5, 
           1, 2, 2, 5, 1, 2, 2, 5, 2, 5, 2, 6, 1, 1, 6, 1, 1, 5, 1, 1, 5, 2, 6))
  
}
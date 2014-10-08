#' Convert degree Fahrenheit to degree Celsius
#' 
#' @description
#' This function converts temperature values from native degree Fahrenheit to 
#' degree Celsius.
#' 
#' @param val Numeric. A vector containing temperature values in degree Fahrenheit.
#' @param ... Additional arguments passed to \code{\link{round}}.
#' 
#' @return
#' A numeric vector containing temperature values in degree Celsius.
#' 
#' @author
#' Florian Detsch
#' 
#' @examples
#' toCelsius(val = c(68, 72, 76), digits = 1)
#' 
#' @export toCelsius
#' @aliases toCelsius
toCelsius <- function(val, 
                      ...) {
  
  val_new <- (val - 32) * 5 / 9
  val_new <- round(val_new, ...)
  
  return(val_new)
}

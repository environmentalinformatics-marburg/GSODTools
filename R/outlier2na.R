#' Remove statistical outliers from time-series values
#' 
#' @description
#' This is a wrapper function around \code{\link{tsOutliers}} that automatically
#' removes identified statistical outliers from a measurement series.
#' 
#' @param val Numeric. A vector of observed time-series values. 
#' @param lower_quantile Numeric, default is 0.2. The lower quantile limit. 
#' @param upper_quantile Numeric, default is 0.8. The upper quantile limit.
#' @param ... Additional arguments passed to \code{\link{tsOutliers}}.
#' 
#' @return
#' An outlier-cleaned numeric vector of time-series values.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link{tsOutliers}}
#' 
#' @examples
#' # Random time-series values
#' set.seed(10)
#' x <- rnorm(100, 0, 2)
#' 
#' # Replace outliers with NA
#' x2 <- outlier2na(x, lower_quantile = .35, upper_quantile = .7)
#' 
#' plot(x, type = "l", lty = 2)
#' lines(x2, col = "red")
#'
#' @export outlier2na
#' @aliases outlier2na
outlier2na <- function(val, 
                       lower_quantile = .2, 
                       upper_quantile = .8,
                       ...) {
  
  # Identify outliers
  outliers <- tsOutliers(val, index = TRUE, 
                         lower_quantile = lower_quantile, 
                         upper_quantile = upper_quantile, 
                         ...)
  # Replace identified outliers with NA
  val[outliers] <- NA
  
  return(val)
}
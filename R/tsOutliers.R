#' Identification of statistical outliers in time series
#' 
#' @description
#' This function identifies statistical outliers in a \code{ts} object based on
#' upper and lower quantile criteria. The function body is mainly taken from 
#' \href{https://stats.stackexchange.com/q/1142}{stats.stackexchange.com}.
#' 
#' @param x Numeric. A vector of observed time-series values. 
#' @param lower_quantile Numeric, default is 0.2. The lower quantile limit. 
#' @param upper_quantile Numeric, default is 0.8. The upper quantile limit.
#' @param plot  Logical, default is FALSE. If TRUE, a time-series plot including 
#' identified outliers is generated.
#' @param index Logical, default is FALSE. If TRUE, a vector holding the indices
#' of identified outliers is returned rather than the statistically obtained
#' scores for each measured value.
#' @param ... Additional arguments passed to \code{\link{ts}}.
#' 
#' @return
#' A numeric vector of scores or, if \code{index = TRUE}, a vector holding the
#' indices of identified outliers.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link{ts}}
#' 
#' @examples
#' # Random time-series values
#' set.seed(10)
#' x <- rnorm(100, 0, 2)
#' 
#' # Return indices of outliers incl. visualization
#' tsOutliers(x, lower_quantile = .35, upper_quantile = .7, 
#'            plot = TRUE, index = TRUE)
#' 
#' @importFrom graphics points
#' 
#' @export
tsOutliers <- function(x, 
                       lower_quantile = .2, 
                       upper_quantile = .8,
                       plot = FALSE,
                       index = FALSE,
                       ...) {
  
  # Convert input vector to time series
  x <- ts(x, ...)
  
  # Residuals
  if (stats::frequency(x) > 1) {
    resid <- stats::stl(x, s.window = "periodic", robust = TRUE, 
                 na.action = stats::na.exclude)$time.series[, 3]
  } else {
    tt <- 1:length(x)
    resid <- stats::residuals(stats::loess(x ~ tt, na.action = stats::na.exclude))
  }
  
  # Calculate scores
  resid.q <- stats::quantile(resid, prob = c(lower_quantile, upper_quantile), na.rm = TRUE)
  iqr <- diff(resid.q)
  limits <- resid.q + 1.5 * iqr * c(-1, 1)
  score <- abs(pmin((resid - limits[1]) / iqr, 0) + 
                 pmax((resid - limits[2]) / iqr, 0))
  
  # Optional plotting
  if (plot) {
    plot(x)
    
    x2 <- ts(rep(NA,length(x)))
    x2[score > 0 & !is.na(score)] <- x[score > 0 & !is.na(score)]
    stats::tsp(x2) <- stats::tsp(x)
    graphics::points(x2, pch = 19, col = "red")
  }
  
  # Return output
  if (index) {
    return(which(score > 0 & !is.na(score)))
  } else {
    return(score)
  }
}

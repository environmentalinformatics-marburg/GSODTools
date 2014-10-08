#' Imputation of small data gaps by linear interpolation
#' 
#' @description
#' This function fills small data gaps occurring in (eco-)climatological 
#' measurement series by simple linear interpolation.
#' 
#' @param data Object of class \code{ki.data}. See \code{\link{as.ki.data}}, 
#' \code{\link{gsod2ki}} for further information.  
#' @param prm Character, default is "TEMP". Parameter(s) to fill. 
#' @param limit Numeric, default is 5. Maximum gap length to be filled by linear
#' interpolation.
#' @param width Numeric, default is 11. See \code{\link{rollapply}} for further
#' information.
#' @param ... Additional arguments. Currently not in use.
#' 
#' @return
#' An object of class \code{ki.data}.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link{rollapply}}
#' 
#' @examples
#' library(dplyr)
#' 
#' data(gsodstations)
#' moshi <- filter(gsodstations, STATION.NAME == "MOSHI")
#' 
#' gsod_moshi <- dlGsodStations(usaf = moshi$USAF,
#'                              start_year = 1990, end_year = 1995,
#'                              dsn = paste0(getwd(), "/data/moshi/"),
#'                              unzip = TRUE)
#' 
#' # Conversion to KiLi SP1 `ki.data` object
#' ki_moshi <- gsod2ki(data = gsod_moshi,
#'                     prm_col = c("TEMP", "MIN", "MAX"),
#'                     df2ki = TRUE)
#' 
#' # Fill small gaps (n <= 3) by linear interpolation
#' ki_moshi_lf <- gfLinInt(data = ki_moshi,
#'                         prm = c("TEMP", "MIN", "MAX"),
#'                         limit = 3, width = 7)
#' 
#' plot(slot(ki_moshi_lf, "Parameter")$TEMP, type = "l", col = "red")
#' lines(slot(ki_moshi, "Parameter")$TEMP) 
#'
#' @export gfLinInt
gfLinInt <- function(data, 
                     prm = "TEMP", 
                     limit = 5, 
                     width = 11, 
                     ...) {
  
  ## Linear interpolation over small (n <= limit) measurement gaps
  
  for (i in prm) {
    # Identify lengths of measurement gaps
    pos.na <- do.call("rbind", 
                      gfGapLength(data.dep = data, 
                                  pos.na = which(is.na(data@Parameter[[i]])), 
                                  gap.limit = 999999, 
                                  end.datetime = Sys.Date(), 
                                  units = "days"))
    # Sufficiently small gaps
    pos.na <- pos.na[which(pos.na[, 3] <= limit), ]
    pos.na.small <- foreach(j = seq(nrow(pos.na)), .combine = "c") %do% {
      seq(pos.na[j, 1], pos.na[j, 2])
    }
    
    # Time series
    tmp.ts <- zoo(data@Parameter[[i]], order.by = as.Date(data@Datetime))
    # Rolling mean (window width = 11)
    tmp.ts.rm <- rollapply(data = tmp.ts, width = width, fill = list(NA, NULL, NA), 
                           partial = TRUE, function(...) mean(..., na.rm = TRUE))
    
    # Replace identified gaps by rolling mean
    tmp.ts[pos.na.small] <- tmp.ts.rm[pos.na.small]
    
    # Insert gap-filled data into referring slots
    data@Parameter[[i]] <- as.numeric(tmp.ts)
  }
  
  return(data)
}

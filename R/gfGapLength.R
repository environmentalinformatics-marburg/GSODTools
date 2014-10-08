#' Julendat function to identify gap lengths in measurement series
#' 
#' @description
#' This is a function taken from Julendat (see \link{https://code.google.com/p/julendat/})
#' to identify gap lengths in (eco-)climatological measurement series of a given
#' parameter.
#' 
#' @param data.dep Object of class \code{ki.data}. See \code{\link{as.ki.data}}, 
#' \code{\link{gsod2ki}} for further information. 
#' @param pos.na Numeric. Indices of missing data points. 
#' @param gap.limit Numeric. Maximum length of a measurement gap. All gaps 
#' exceeding this threshold will not be considered.
#' @param end.datetime Object of class \code{Date}, default is \code{Sys.Date()}.
#' Not required for GSOD data processing. 
#' @param units Character. Measurement interval, typically "days" for GSOD data. 
#' @param ... Additional arguments. Currently not in use.
#' 
#' @return
#' A \code{list} containing start, end, and length of each data gap.
#' 
#' @author
#' Florian Detsch
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
#' # Identify length per data gap
#' gfGapLength(data.dep = ki_moshi, 
#'             pos.na = which(is.na(slot(ki_moshi, "Parameter")$TEMP)), 
#'             gap.limit = 365,
#'             units = "days", 
#'             end.datetime = Sys.Date())
#'             
#' @export gfGapLength
#' @aliases gfGapLength
gfGapLength <- function(data.dep, 
                        pos.na,
                        gap.limit,
                        end.datetime = Sys.Date(),
                        units = "days", 
                        ...) {

  # Temporal space between single NA values
  pos.na.diff <- c(-99, diff(pos.na), -99)
  
  
  ## Determination of gap length
  
  # Single gaps --> starting point == endpoint
  gap.single <- unlist(lapply(seq(pos.na), function(i) {
    pos.na.diff[i] != 1 && pos.na.diff[i+1] != 1
  }))
  
  # Gap starting points
  gap.start <- unlist(lapply(seq(pos.na), function(i) {
    pos.na.diff[i] != 1 && pos.na.diff[i+1] == 1
  }))
  
  # Gap endpoints
  gap.end <- unlist(lapply(seq(pos.na) + 1, function(i) {
    pos.na.diff[i-1] == 1 && pos.na.diff[i] != 1
  }))
  
  # Concatenate starting points and endpoints
  gap <- as.data.frame(rbind(cbind(pos.na[which(gap.start)], pos.na[which(gap.end)]), 
               cbind(pos.na[which(gap.single)], pos.na[which(gap.single)])))
  gap <- gap[order(gap[,1]),]
  gap.end.set <- as.Date(end.datetime, "%Y-%m-%d")
  gap.end.act <- as.Date(paste(data.dep@Date$Year[gap[nrow(gap),2]],
                               data.dep@Date$Month[gap[nrow(gap),2]],
                               data.dep@Date$Day[gap[nrow(gap),2]], sep="-"),
                         "%Y-%m-%d")
  time.difference.hours <- difftime(gap.end.set, gap.end.act, units = units)
  if (time.difference.hours < 0.0 & units == "hours") {
    gap[nrow(gap),2] <- gap[nrow(gap),2] + (time.difference.hours) 
  } else if (units == "days") {
    gap[nrow(gap), 2] <- gap[nrow(gap), 1]
  }
  
  # Calculate gap length
  gap[,3] <- gap[,2] + 1 - gap[,1]
  
  # Reject too large gaps
  gap <- subset(gap, gap[,3] <= gap.limit)
 
  # Convert data frame to list
  if (nrow(gap) > 0) {
    gap <- lapply(1:nrow(gap), function(i) gap[i,])
  } else {
    gap <- list()
  }
    
  # Return output
  return(gap)
}


### Exemplary function call

# input.filepath <- "/media/permanent/r_mulreg/data/ki_0000cof3_000rug_201102010000_201102282355_eat_ca05_cti05_0050.dat"
# prm.dep <- "Ta_200"
# 
# source("/home/dogbert/software/development/julendat/src/julendat/rmodules/as.ki.data.R")
# ki.data.dep <- as.ki.data(input.filepath)
# 
# pos.na <- which(is.na(ki.data.dep@Parameter[[prm.dep]]))
# 
# gaps <- gfGapLength(data.dep = ki.data.dep, 
#                     pos.na = pos.na)
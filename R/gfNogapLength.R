#' Julendat function to identify lengths of continuous measurements
#' 
#' @description
#' This is a function taken from Julendat (see \link{https://code.google.com/p/julendat/})
#' to identify lengths of continuous measurements in (eco-)climatological 
#' measurement series.
#' 
#' @param gaps A \code{data.frame} or \code{list}. Usually created from 
#' \code{\link{gfGapLength}}. 
#' @param data.dep An object of class \code{ki.data}, or a filepath that can be 
#' coerced to \code{ki.data}. The data set under investigation. 
#' @param ... Additional arguments. Currently not in use.
#' 
#' @return
#' A \code{list} containing start, end, and length of each continuous measurement.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso 
#' \code{\link{gfGapLength}}
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
#' gaps <- gfGapLength(data.dep = ki_moshi, 
#'                     pos.na = which(is.na(slot(ki_moshi, "Parameter")$TEMP)), 
#'                     gap.limit = 365,
#'                     units = "days", 
#'                     end.datetime = Sys.Date())
#'                     
#' # Identify lengths of continuous measurements
#' nogaps <- gfNogapLength(gaps = gaps, 
#'                         data.dep = ki_moshi)                     
#'             
#' @export gfNogapLength
#' @aliases gfNogapLength
gfNogapLength <- function(gaps, 
                          data.dep, 
                          ...) {
  
  # If `class(gaps) == data.frame` -> convert to list
  if (is.data.frame(gaps)) {
    gaps <- lapply(seq(nrow(gaps)), function(i) {
      gaps[i, ]
    })
  }
  
  # If `class(data.dep) == "character"` (i.e. filepath) -> convert to `ki.data` object
  if (is.character(data.dep))
    data.dep <- as.ki.data(data.dep)
  
  # For each gap, calculate start, end, and range of intermediate measurement
  nogaps <- lapply(seq(gaps), function(i) {
    
    if (i == 1 & gaps[[i]][[1]] != 1) {
      tmp.start <- 1
      tmp.end <- gaps[[i]][[1]] - 1
      tmp.span <- length(seq(tmp.start, tmp.end))
    } else if (i > 1 & i < length(gaps)) {
      tmp.start <- gaps[[i-1]][[2]] + 1
      tmp.end <- gaps[[i]][[1]] - 1
      tmp.span <- length(seq(tmp.start, tmp.end))
    } else if (i == length(gaps) & gaps[[i]][[2]] != length(slot(data.dep, "Parameter")[["TEMP"]])) {
      tmp.start <- gaps[[i]][[2]] + 1
      tmp.end <- length(slot(data.dep, "Parameter")[["TEMP"]])
      tmp.span <- length(seq(tmp.start, tmp.end))
    }
    
    if (exists("tmp.start"))
      return(data.frame(start = tmp.start, end = tmp.end, span = tmp.span))
  })
  
  return(nogaps)
}
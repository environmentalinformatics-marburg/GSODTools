#' Time-series forecasting based on singular spectrum analysis (SSA)
#' 
#' @description
#' This function applies singular spectrum analysis (SSA) in order to impute 
#' missing values in a data set based on time-series forecasting. Note that the
#' SSA method requires sufficiently long series of continuous measurements, i.e.
#' with no data gaps at all. Therefore, \code{\link{gfLinInt}} and 
#' \code{\link{gfJulendat}} should be used to fill smaller and medium-sized gaps
#' before applying \code{gfSsa}.
#' 
#' @param data Object of class `ki.data` or a filepath that can be coerced to
#' \code{ki.data}. The data set under investigation. 
#' @param prm Character, default is "TEMP". Parameter(s) to fill.
#' @param reversed_forecast Logical, default is FALSE. If TRUE, the supplied 
#' measurement series is reversed prior to forecasting, i.e. values are predicted
#' into the past rather than into the future.
#' @param ... Additional arguments passed to \code{\link{round}}.
#' 
#' @return
#' An object of class \code{ki.data}.
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{gfLinInt}}, \code{\link{gfJulendat}}
#' 
#' @examples
#' library(dplyr)
#' 
#' data(gsodstations)
#' gar <- filter(gsodstations, STATION.NAME == "GARISSA")
#' 
#' gsod_gar <- dlGsodStations(usaf = gar$USAF,
#'                            start_year = 1990, end_year = 1995,
#'                            dsn = paste0(getwd(), "/data/gar/"),
#'                            unzip = TRUE)
#' 
#' # Conversion to KiLi SP1 `ki.data` object
#' ki_gar <- gsod2ki(data = gsod_gar,
#'                   prm_col = c("TEMP", "MIN", "MAX"),
#'                   df2ki = TRUE)
#' 
#' # Fill small gaps (n <= 5) with linear interpolation
#' ki_gar_linint <- gfLinInt(data = ki_gar, 
#'                           prm = c("TEMP", "MIN", "MAX"))
#' 
#' # Fill remaining gaps based on SSA
#' ki_gar_ssa <- gfSsa(data = ki_gar_linint, 
#'                     prm = c("TEMP", "MIN", "MAX"), 
#'                     reversed_forecast = FALSE, 
#'                     digits = 2)
#' 
#' plot(slot(ki_gar_ssa, "Parameter")[["TEMP"]], type = "l", col = "red")
#' lines(slot(ki_gar_linint, "Parameter")[["TEMP"]], col = "green")
#' lines(slot(ki_gar, "Parameter")[["TEMP"]])             
#' 
#' @export gfSsa
#' @aliases gfSsa
gfSsa <- function(data, 
                  prm = "TEMP", 
                  reversed_forecast = FALSE, 
                  ...) {
  
  # If `class(data) == "character"` (i.e. filepath) -> convert to `ki.data` object
  if (class(data) == "character")
    data <- as.ki.data(data)
  
  # Duplicate input data
  data.rev <- data
  
  # Loop through all columns to be gap-filled via SSA
  filled.data <- lapply(prm, function(h) {

    # Reverse time series prior to forecasting (optional)
    tmp.rev <- if (reversed_forecast) {
      rev(as.numeric(slot(data, "Parameter")[[h]]))
    } else {
      as.numeric(slot(data, "Parameter")[[h]])
    }
    
    # Convert numeric vector to 'zoo' time series
    tmp.rev.ts <- zoo(tmp.rev, order.by = as.Date(slot(data, "Datetime")))
    # Insert potentially reversed time series into referring parameter slot
    slot(data.rev, "Parameter")[[h]] <- as.numeric(tmp.rev.ts)
    
    # Identify lengths of measurement gaps
    data.rev.na <- which(is.na(slot(data.rev, "Parameter")[[h]]))
    ki.rev.na <- do.call(function(...) {
      tmp <- rbind(...)
      names(tmp) <- c("start", "end", "span")
      return(tmp)}, gfGapLength(data.dep = data.rev,
                                pos.na = data.rev.na, 
                                gap.limit = 999999, 
                                end.datetime = Sys.Date(), 
                                units = "days"))
    
    # As long as gaps exists, do the following stuff ...
    while (length(ki.rev.na) > 0) {
      
      # Identify lengths of continuous measurements
      ki.rev.nona <- do.call("rbind", gfNogapLength(gaps = ki.rev.na, 
                                                    data.dep = data.rev))
      
      # Deconstruct continuous measurement series
      tmp.ssa <- 
        ssa(slot(data.rev, "Parameter")[[h]][ki.rev.nona[1, 1]:ki.rev.nona[1, 2]], 
            L = if (ki.rev.nona[1, 3] > 365) {
              365
            } else if (ki.rev.nona[1, 3] <= 365 & ki.rev.nona[1, 3] > 182) {
              182
            } else {
              cat("Time series not sufficiently continuous to perform SSA!")
              break
            })

      # Forecast the next gap
      slot(data.rev, "Parameter")[[h]][ki.rev.na[1,1] : ki.rev.na[1,2]] <-
        forecast(tmp.ssa, groups = list(seq(nsigma(tmp.ssa))), len = ki.rev.na[1,3])$mean
      
      # Update lengths of measurement gaps
      data.rev.na <- which(is.na(slot(data.rev, "Parameter")[[h]]))
      if (length(data.rev.na) > 0) {
        ki.rev.na <- do.call(function(...) {
          tmp <- rbind(...)
          names(tmp) <- c("start", "end", "span")
          return(tmp)}, gfGapLength(data.dep = data.rev,
                                    pos.na = data.rev.na, 
                                    gap.limit = 999999, 
                                    end.datetime = Sys.Date(), 
                                    units = "days"))
      } else {
        ki.rev.na <- list()
      }
    }
    
    # Replace gappy by filled time series
    if (reversed_forecast) {
      tmp <- rev(as.numeric(slot(data.rev, "Parameter")[[h]]))
    } else {
      tmp <- as.numeric(slot(data.rev, "Parameter")[[h]])
    }
    
    return(round(tmp, ...))
  })
  
  # Insert gap-filled time series into referring slots
  for (j in prm) {
    id_prm <- grep(j, prm)
    id_ki <- grep(j, names(slot(data, "Parameter")))
    slot(data, "Parameter")[[id_ki]] <- filled.data[[id_prm]]
  }
  
  # Return gap-filled data sets
  return(data)
}
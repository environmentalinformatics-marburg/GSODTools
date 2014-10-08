#' Julendat function to convert \code{ki.data} to \code{data.frame}
#' 
#' @description
#' This function converts objects of class \code{ki.data} to "ordinary" objects
#' of class \code{data.frame} that follow KiLi SP1 standard formatting conventions. 
#' 
#' @param data.dep Object of class \code{ki.data}. 
#' @param plevel, default is "NA". Processing level. Not required for GSOD data
#' processing. 
#' @param ... Further arguments. Currently not in use. 
#' 
#' @return
#' A \code{data.frame}. 
#' 
#' @author
#' Florian Detsch
#' 
#' @seealso
#' \code{\link{gsod2ki}}, \code{\link{as.ki.data}}
#' 
#' @export gfOutputData
#' @aliases gfOutputData
gfOutputData <- function(data.dep,
                         plevel = "NA", 
                         ...) {
  
  # Define columns of output data frame
  datetime <- data.dep@Datetime
  timezone <- rep(data.dep@Timezone, length(datetime))
  aggregationtime <- rep(data.dep@Aggregationtime, length(datetime))
  plotid <- data.dep@PlotId$Shortname
  epplotid <- data.dep@EpPlotId
  stationid <- data.dep@StationId$Longname
  processlevel <- rep(plevel, length(datetime))
  qualityflag <- data.dep@Qualityflag
  parameter <- as.data.frame(data.dep@Parameter)
  
  # Return output data frame
  return(data.frame(Datetime = datetime, 
                    Timezone = timezone,
                    Aggregationtime = aggregationtime,
                    PlotId = plotid,
                    EpPlotId = epplotid,
                    StationId = stationid,
                    Processlevel = processlevel,
                    Qualityflag = qualityflag,
                    parameter, 
                    stringsAsFactors = F))
}
#' Class ki.data
#' 
#' @description 
#' An object of class \code{ki.data}, typically created through
#'   \code{\link{as.ki.data}}.
#' 
#' @exportClass ki.data
#' @name ki.data-class
NULL

methods::setClass(
  "ki.data"
  , methods::representation(
    Datetime = "POSIXct"
    , Date = "list"
    , Time = "list"
    , AggregationLevels = "list"
    , Origin = "character"
    , Season = "character"
    , Timezone = "character"
    , Aggregationtime = "character"
    , PlotId = "list"
    , EpPlotId = "character"
    , StationId = "list"
    , Processlevel = "integer"
    , Qualityflag = "character"
    , Valid = "list"
    , Parameter = "list"
    , PrmHisto = "list"
  )
)

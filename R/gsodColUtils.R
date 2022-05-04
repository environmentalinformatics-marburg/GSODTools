#' Utility Functions for Fixed-Width Columns
#' 
#' @description 
#' Utility functions to handle fixed-width columns in GSOD ASCII data.
#' 
#' @author Florian Detsch
#' 
#' @examples
#' gsodColWidth()
#' gsodColClasses()
#' 
#' @noRd
NULL

# @describeIn gsodColUtils Returns native column widths as \code{numeric}, e.g.
#   for use with \code{\link[utils]{read.fwf}}.
# 
gsodColWidth <- function() {
  c(
    6, 1 # STN---
    , 5, 2 # WBAN
    , 8, 2 # YEARMODA
    , 6, 1, 2, 2 # TEMP
    , 6, 1, 2, 2 # DEWP
    , 6, 1, 2, 2 # SLP
    , 6, 1, 2, 2 # STP
    , 5, 1, 2, 2 # VISIB
    , 5, 1, 2, 2 # WDSP
    , 5, 2 # MXSPD
    , 5, 2 # GUST
    , 6, 1, 1 # MAX
    , 6, 1, 1 # MIN
    , 5, 1, 1 # PRCP
    , 5, 2 # SNDP
    , 6 # FRSHTT
  )
}

# @describeIn gsodColUtils Returns column names as \code{character}.
# 
gsodColClasses = function() {
  c(
    c("character", "logical") # STN---
    , c("character", "logical") # WBAN
    , c("character", "logical") # YEARMODA
    , c("numeric", "logical", "integer", "logical") # TEMP
    , c("numeric", "logical", "integer", "logical") # DEWP
    , c("numeric", "logical", "integer", "logical") # SLP
    , c("numeric", "logical", "integer", "logical") # STP
    , c("numeric", "logical", "integer", "logical") # VISIB
    , c("numeric", "logical", "integer", "logical") # WDSP
    , c("numeric", "logical") # MXSPD
    , c("numeric", "logical") # GUST
    , c("numeric", "character", "logical") # MAX
    , c("numeric", "character", "logical") # MIN
    , c("numeric", "character", "logical") # PRCP
    , c("numeric", "logical") # SNDP
    , c("character") # FRSHTT
  )
}
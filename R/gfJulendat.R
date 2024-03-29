#' Imputation of medium-sized gaps by linear modeling using adjacent GSOD stations
#' 
#' @description
#' This is a wrapper function encompassing several sub-functions from the 
#' Julendat gap-filling routine (processing level "0310", see 
#' \href{https://code.google.com/archive/p/julendat/}{Julendat}). Briefly, a 
#' discontinuous measurement series from a distinct GSOD station is being filled
#' by the use of simultaneous measurements from adjacent GSOD stations. Taking 
#' the gappy data as response variable, a linear model is fitted to explain the 
#' measurements taken by the station under investigation based on surrounding 
#' stations.
#' 
#' @param files.dep Character. Path leading to the response GSOD station. 
#' Note: the file needs to be formatted according to standard KiLi SP1 format, 
#' see \code{\link{gsod2ki}}.
#' @param files.indep Character. Path(s) leading to the predictor GSOD station(s).
#' @param filepath.coords Character, default is NULL. Path to file holding 
#' coordinate information. Not required for GSOD data processing. 
#' @param quality.levels Character, default is NULL. Not required for GSOD data
#' processing.
#' @param gap.limit Numeric. Maximum length of a measurement gap to be imputed. 
#' All gaps exceeding this threshold will not be considered.
#' @param end.datetime Object of class \code{Date}, default is \code{Sys.Date()}.
#' Not required for GSOD data processing. 
#' @param units Character. Measurement interval, typically "days" for GSOD data. 
#' @param na.limit Numeric, default is 0.5. Maximum amount of missing
#' data within \code{time.window}. If a certain explanatory station exceeds this
#' threshold, it will not be considered for linear modeling.
#' @param time.window Numeric, default is 365. Window width before and after a 
#' data gap to extract data for linear modeling. 
#' @param n.plot Numeric. Number of explanatory GSOD stations to consider for 
#' linear modeling. If not supplied, all stations specified in \code{files.indep}
#' will be considered.
#' @param prm.dep Character, default is "TEMP". Determines which parameters to fill.
#' @param prm.indep Character, default is "NA". Not required for GSOD data processing.
#' @param family Object of class \code{family}, default is 
#' \code{\link[stats]{gaussian}}.
#' @param plevel Character. Determines current processing level. Not required 
#' for GSOD data processing.
#' @param ... Additional arguments. Currently not in use.
#' 
#' @return
#' An object of class \code{ki.data}.
#' 
#' @author
#' Florian Detsch
#' 
#' @examples
#' \dontrun{
#' library(GSODTools)
#'
#' # Download data sets for selected GSOD stations
#' usafs = c(
#'   "gar" = "637230"   # GARISSA
#'   , "jom" = "637400" # NAIROBI JKIA
#'   , "kia" = "637910" # KILIMANJARO INTL
#'   , "voi" = "637930" # VOI
#'   , "mom" = "638200" # MOMBASA MOI INTL
#'   , "mor" = "638660" # MOROGORO (MET)
#' )
#' 
#' shp_gsod <- 
#'   gsodstations |> 
#'   gsodDf2Sp() |> 
#'   subset(`USAF` %in% usafs) |> 
#'   gsodDf2Sp()
#' 
#' df_gsod = Map(
#'   \(usaf, plot_id) {
#'     
#'     # Download and extraction
#'     tmp_df_gsod <- dlGsodStations(usaf = usaf, 
#'                                   start_year = 1990, end_year = 1995, 
#'                                   dsn = tempdir(), 
#'                                   unzip = TRUE,
#'                                   save_output = FALSE)
#'     
#'     # Fahrenheit -> Celsius
#'     tmp_df_gsod = transform(
#'       tmp_df_gsod
#'       , TEMP = toCelsius(TEMP, digits = 1L)
#'       , MIN = toCelsius(MIN, digits = 1L)
#'       , MAX = toCelsius(MAX, digits = 1L)
#'     )
#'     
#'     # GSOD -> `ki.data`
#'     tmp_ki_gsod <- gsod2ki(tmp_df_gsod, 
#'                            prm_col = c("TEMP", "MIN", "MAX"), 
#'                            timezone = "eat",
#'                            aggtime = "diurnal",
#'                            plot_id = plot_id, 
#'                            df2ki = TRUE)
#'     
#'     # Remove outliers
#'     for (j in c("TEMP", "MIN", "MAX")) {
#'       methods::slot(tmp_ki_gsod, "Parameter")[[j]] = outlier2na(
#'         methods::slot(tmp_ki_gsod, "Parameter")[[j]]
#'         , lower_quantile = 0.2
#'         , upper_quantile = 0.8
#'       )
#'     }
#'     
#'     # Fill small gaps by linear interpolation
#'     tmp_ki_gsod <- gfLinInt(tmp_ki_gsod, 
#'                             prm = c("TEMP", "MIN", "MAX"))
#'     
#'     # Save data created so far
#'     tmp_df_gsod <- gfOutputData(tmp_ki_gsod, plevel = "NA")
#'     write.csv(tmp_df_gsod, sprintf("%s/%s.csv", tempdir(), usaf), 
#'               row.names = FALSE)
#'     
#'     return(tmp_ki_gsod)    
#'   }
#'   , methods::slot(shp_gsod, "data")$USAF
#'   , names(usafs)
#' )
#' 
#' # Fill medium-sized gaps at Kilimanjaro Intl. Airport by linear modeling
#' fls_gsod <- list.files(tempdir(), pattern = "^\\d{6}.csv$", 
#'                        recursive = TRUE, full.names = TRUE)
#' 
#' jul_gsod <- gfJulendat(files.dep = fls_gsod[3],
#'                        files.indep = fls_gsod[-3],
#'                        filepath.coords = NULL,
#'                        quality.levels = NULL,
#'                        gap.limit = 1825, 
#'                        na.limit = .9,
#'                        time.window = 913,
#'                        n.plot = 10,
#'                        prm.dep = c("TEMP", "MAX", "MIN"), 
#'                        prm.indep = c(NA, NA, NA), 
#'                        plevel = "NA", 
#'                        end.datetime = Sys.Date(), 
#'                        units = "days")
#' 
#' plot(jul_gsod$TEMP, col = "red", type = "l")
#' lines(methods::slot(df_gsod[[3]], "Parameter")$TEMP, col = "grey75")
#' }
#' 
#' @export
gfJulendat <- function(files.dep, 
                       files.indep, 
                       filepath.coords = NULL, 
                       quality.levels = NULL, 
                       gap.limit,
                       end.datetime = Sys.Date(),
                       units = "days", 
                       na.limit = 0.5,
                       time.window = 365,
                       n.plot,
                       prm.dep = "TEMP", 
                       prm.indep = "NA", 
                       family = stats::gaussian,
                       plevel,
                       ...) {
  
  # Perform imputation of missing values
  imputation.data <- gfRun(files.dep = files.dep,
                           files.indep = files.indep,
                           filepath.coords = filepath.coords, 
                           quality.levels = quality.levels,
                           gap.limit = gap.limit,
                           end.datetime = end.datetime,
                           units = units, 
                           na.limit = na.limit,
                           time.window = time.window,
                           n.plot = n.plot,
                           prm.dep = prm.dep, 
                           prm.indep = prm.indep, 
                           family = family, 
                           plevel = plevel)
  
  return(imputation.data[[2]])
}

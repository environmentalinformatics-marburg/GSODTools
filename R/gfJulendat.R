#' Imputation of medium-sized gaps by linear modeling using adjacent GSOD stations
#' 
#' @description
#' This is a wrapper function encompassing several sub-functions from the 
#' Julendat gap-filling routine (processing level "0310", see \link{https://code.google.com/p/julendat/}). 
#' Briefly, a discontinuous measurement series from a distinct GSOD station is
#' being filled by the use of simultaneous measurements from adjacent GSOD stations. 
#' Taking the gappy data as response variable, a linear model is fitted to explain 
#' the measurements taken by the station under investigation based on surrounding stations.
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
#' @param family Object of class \code{family}, default is gaussian. See 
#' \code{\link{family}}.
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
#' library(dplyr)
#' library(foreach)
#'
#' data(gsodstations)
#'
#' # Download data sets for selected GSOD stations
#' station_names <- c("GARISSA", "KILIMANJARO AIRPORT", "MOMBASA", "MOROGORO", 
#'                    "JOMO KENYATTA INTL", "VOI")
#' station_names_abb <- c("gar", "kia", "mom", "mor", "jom", "voi")
#' 
#' shp_gsod <- 
#'   gsodstations %>% 
#'   gsodReformat() %>% 
#'   filter(STATION.NAME %in% station_names) %>% 
#'   gsodDf2Sp()
#' 
#' df_gsod <- 
#'   foreach(usaf = slot(shp_gsod, "data")$USAF, plot_id = station_names_abb) %do% {
#'     
#'     # Download and extraction
#'     tmp_df_gsod <- dlGsodStations(usaf = usaf, 
#'                                   start_year = 1990, end_year = 1995, 
#'                                   dsn = paste0("data/", usaf), 
#'                                   unzip = TRUE,
#'                                   save_output = FALSE)
#'                                   
#'     # Remove obsolete "NC" columns
#'     tmp_df_gsod <- tmp_df_gsod[, -grep("^NC$", names(tmp_df_gsod))]
#'     
#'     # Fahrenheit -> Celsius
#'     tmp_df_gsod$TEMP <- tmp_df_gsod %>% select(TEMP) %>% unlist() %>% toCelsius(., digits = 1)
#'     tmp_df_gsod$MIN <- tmp_df_gsod %>% select(MIN) %>% unlist() %>% toCelsius(., digits = 1)
#'     tmp_df_gsod$MAX <- tmp_df_gsod %>% select(MAX) %>% unlist() %>% toCelsius(., digits = 1)
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
#'       slot(tmp_ki_gsod, "Parameter")[[j]] <- outlier2na(slot(tmp_ki_gsod, "Parameter")[[j]], 
#'                                                lower_quantile = .2, upper_quantile = .8)
#'     }
#'     
#'     # Fill small gaps by linear interpolation
#'     tmp_ki_gsod <- gfLinInt(tmp_ki_gsod, 
#'                             prm = c("TEMP", "MIN", "MAX"))
#'     
#'     # Save data created so far
#'     tmp_df_gsod <- gfOutputData(tmp_ki_gsod, plevel = "NA")
#'     write.csv(tmp_df_gsod, paste0("data/", usaf, "/", usaf, "_1990_1995.csv"), 
#'               row.names = FALSE)
#'     
#'     return(tmp_ki_gsod)    
#'   }
#' 
#' # Fill medium-sized gaps at Kilimanjaro Intl. Airport by linear modeling
#' fls_gsod <- list.files("data/", pattern = "^63.*1990_1995.csv$", 
#'                        recursive = TRUE, full.names = TRUE)
#' 
#' jul_gsod <- gfJulendat(files.dep = fls_gsod[2],
#'                        files.indep = fls_gsod[-2],
#'                        filepath.coords = NULL,
#'                        quality.levels = NULL,
#'                        gap.limit = 1825, 
#'                        na.limit = .9,
#'                        time.window = 913,
#'                        n.plot = 10,
#'                        prm.dep = c("TEMP", "MAX", "MIN"), 
#'                        prm.indep = c(NA, NA, NA), 
#'                        family = gaussian, 
#'                        plevel = "NA", 
#'                        end.datetime = Sys.Date(), 
#'                        units = "days")
#' 
#' plot(jul_gsod$TEMP, col = "red", type = "l")
#' lines(slot(df_gsod[[2]], "Parameter")$TEMP, col = "grey75")
#'             
#' @export gfJulendat
#' @aliases gfJulendat
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
                       family = gaussian,
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

#' Global Summary of the Day Station List
#' 
#' A data set containing the full Global Summary of the Day (GSOD) station list.
#' 
#' @seealso Integrated Surface Database Station History, May 2022. Available 
#'   online: \url{https://www.ncei.noaa.gov/pub/data/noaa/isd-history.txt} (last
#'   accessed 2022-05-05)
#' 
#' @format A \code{data.table} with 29610 rows and 11 columns:
#' \itemize{
#'   \item{"USAF": }{[\code{character}] Air Force station ID. May contain a letter in the first position.}
#'   \item{"WBAN": }{[\code{character}] NCDC WBAN number.}
#'   \item{"STATION NAME": }{[\code{character}] Station name.}
#'   \item{"CTRY": }{[\code{character}] FIPS country ID, see also \url{https://www.ncei.noaa.gov/data/global-summary-of-the-day/doc/country-list.txt}.}
#'   \item{"STATE": }{[\code{character}] State for US stations.}
#'   \item{"ICAO": }{[\code{character}] ICAO ID.}
#'   \item{"LAT,LON": }{[\code{numeric}] Latitude, longitude in thousandths of decimal degrees.}
#'   \item{"ELEV(M)": }{[\code{numeric}] Elevation in meters.}
#'   \item{"BEGIN,END": }{[\code{character}] Beginning, end of Period Of Record (YYYYMMDD). There may be reporting gaps within the P.O.R..}
#' }
#' 
"gsodstations"


#' East African Sample Data
#' 
#' A data set containing outlier-adjusted and gap-filled daily readings from the
#'   GSOD stations at Kilimanjaro Intl. Airport, Tanzania, and Jomo Kenyatta 
#'   Intl. Airport, Nairobi, Kenya between 1980 and 2000. Missing values were 
#'   imputed based on singular spectrum analysis (SSA).
#' 
#' @format A \code{data.table} with 30684 rows and 6 columns:
#' \itemize{
#'   \item{"PlotId": }{[\code{factor}] Station name (\code{"kil"}: Kilimanjaro, 
#'     \code{"jom"}: Nairobi)}
#'   \item{"Status": }{[\code{factor}] Preprocess level (\code{"cleansed"}: 
#'     outliers removed, \code{"filled"}: gaps filled via SSA)}
#'   \item{"Datetime": }{[\code{POSIXct}] Time in Eastern Africa Time}
#'   \item{"TEMP,MIN,MAX": }{[\code{numeric}] Mean, minimum, maximum air 
#'     temperature in degC}
#' }
#' 
"eastafrica"
#' Global Summary of the Day Station List
#' 
#' A data set containing the full Global Summary of the Day (GSOD) station list.
#' 
#' @seealso Integrated Surface Database Station History, May 2022. Available 
#'   online: <https://www.ncei.noaa.gov/pub/data/noaa/isd-history.txt> (last
#'   accessed 2023-12-12).
#' 
#' @format A `data.table` with 29640 rows and 11 columns:
#' * `"USAF"`: \[`character`\] Air Force station ID. May contain a letter in the
#'   first position.
#' * `"WBAN"`: \[`character`\] NCDC WBAN number.
#' * `"STATION NAME"`: \[`character`\] Station name.
#' * `"CTRY"`: \[`character`\] FIPS country ID, see also 
#'   <https://www.ncei.noaa.gov/data/global-summary-of-the-day/doc/country-list.txt>.
#' * `"STATE"`: \[`character`\] State for US stations.
#' * `"ICAO"`: \[`character`\] ICAO ID.
#' * `"LAT, LON"`: \[`numeric`\] Latitude, longitude in thousandths of decimal 
#'   degrees.
#' * `"ELEV(M)"`: \[`numeric`\] Elevation in meters.
#' * `"BEGIN, END"`: \[`character`\] Beginning, end of Period Of Record (POR; 
#'   YYYYMMDD). There may be reporting gaps within the POR.
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
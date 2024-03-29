% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dlGsodStations.R
\name{dlGsodStations}
\alias{dlGsodStations}
\title{Download (and extract) GSOD data}
\usage{
dlGsodStations(
  usaf,
  start_year = NA,
  end_year = NA,
  dsn = ".",
  unzip = FALSE,
  rm_gz = FALSE,
  ...
)
}
\arguments{
\item{usaf}{Numeric. A unique USAF station code. It can either be manually 
determined from built-in \code{gsodstations}, or retrieved from spatial 
subsetting (see \code{\link{stationFromCoords}}, 
\code{\link{stationFromExtent}}).}

\item{start_year}{Numeric. The desired year to start data acquisition. If not 
supplied, download starts from the first year of measurement.}

\item{end_year}{Numeric. The desired year to stop data acquisition. If not 
supplied, download stops at the last year of measurement.}

\item{dsn}{Character, default is the current working directory. Destination 
folder for data download.}

\item{unzip}{Logical, default is FALSE. If TRUE, *.gz files are unpacked and 
merged after download.}

\item{rm_gz}{Logical, default is FALSE. If TRUE, *.gz files are removed after
extraction. If \code{unzip = FALSE}, this argument is ignored.}

\item{...}{Additional arguments passed to \code{\link{gzGsodStations}}.}
}
\value{
A vector of *.gz filenames, or an object of class \code{data.frame} if 
\code{unzip = TRUE}.
}
\description{
Download (and extract) data from a GSOD station based on its unique USAF code.
}
\examples{
\dontrun{
moshi = subset(
  gsodstations
  , `STATION NAME` == "MOSHI"
)

# download data from moshi, tanzania, from 1990 to 1995
gsod_moshi = dlGsodStations(
  usaf = moshi$USAF
  , start_year = 1990
  , end_year = 1995
  , dsn = tempdir()
  , unzip = TRUE
)

# plot temperature data
plot(
  TEMP ~ YEARMODA
  , data = gsod_moshi
  , type = "l"
)
}

}
\author{
Florian Detsch
}

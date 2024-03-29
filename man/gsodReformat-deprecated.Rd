% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsodReformat.R
\name{gsodReformat-deprecated}
\alias{gsodReformat-deprecated}
\title{Reformat list of available GSOD stations}
\arguments{
\item{data}{\code{data.frame}. Usually built-in \link{gsodstations}.}

\item{df2sp}{\code{logical}, default is \code{FALSE}. If \code{TRUE}, the list of available
GSOD stations is converted to \code{sf}.}

\item{...}{Currently not used.}
}
\value{
An object of class \code{data.frame}, or a \code{sf} object if \code{df2sp = TRUE}.
}
\description{
This function is intended to convert the initial list of available GSOD
stations (see \url{https://www.ncei.noaa.gov/pub/data/noaa/isd-history.csv}) to
an object of class \code{sf} for further processing.
}
\examples{
# Reformat coordinates and convert to spatial object
gsod_shp <- suppressWarnings(
  gsodReformat(
    data = gsodstations
    , df2sp = TRUE
  )
)

if (interactive()) {
  plot(gsod_shp)
}

}
\seealso{
\link{GSODTools-deprecated}
}
\author{
Florian Detsch
}
\keyword{internal}

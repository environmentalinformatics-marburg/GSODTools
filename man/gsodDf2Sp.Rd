% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gsodDf2Sp.R
\name{gsodDf2Sp}
\alias{gsodDf2Sp}
\title{Convert GSOD station list to spatial object}
\usage{
gsodDf2Sp(data, ...)
}
\arguments{
\item{data}{\code{data.frame}. Usually built-in \code{gsodstations}.}

\item{...}{Additional arguments. Currently not in use.}
}
\value{
An object of class \code{SpatialPointsDataFrame}.
}
\description{
This function converts the list of available GSOD stations from \code{data.frame}
to \code{SpatialPointsDataFrame}.
}
\examples{
# Convert station list to spatial object
gsod_shp <- gsodDf2Sp(data = gsodstations)

if (interactive()) {
  plot(gsod_shp)
}

}
\author{
Florian Detsch
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gfGapLength.R
\name{gfGapLength}
\alias{gfGapLength}
\title{Julendat function to identify gap lengths in measurement series}
\usage{
gfGapLength(
  data.dep,
  pos.na,
  gap.limit,
  end.datetime = Sys.Date(),
  units = "days",
  ...
)
}
\arguments{
\item{data.dep}{Object of class \code{ki.data}. See \code{\link{as.ki.data}}, 
\code{\link{gsod2ki}} for further information.}

\item{pos.na}{Numeric. Indices of missing data points.}

\item{gap.limit}{Numeric. Maximum length of a measurement gap. All gaps 
exceeding this threshold will not be considered.}

\item{end.datetime}{Object of class \code{Date}, default is \code{Sys.Date()}.
Not required for GSOD data processing.}

\item{units}{Character. Measurement interval, typically "days" for GSOD data.}

\item{...}{Additional arguments. Currently not in use.}
}
\value{
A \code{list} containing start, end, and length of each data gap.
}
\description{
This is a function taken from 
\href{https://code.google.com/archive/p/julendat/}{Julendat} to identify 
gap lengths in (eco-)climatological measurement series of a given parameter.
}
\examples{
\dontrun{
moshi <- subset(gsodstations, `STATION NAME` == "MOSHI")

gsod_moshi <- dlGsodStations(usaf = moshi$USAF,
                             start_year = 1990, end_year = 1995,
                             dsn = tempdir(),
                             unzip = TRUE)

# Conversion to KiLi SP1 `ki.data` object
ki_moshi <- gsod2ki(data = gsod_moshi,
                    prm_col = c("TEMP", "MIN", "MAX"),
                    df2ki = TRUE)

# Identify length per data gap
gfGapLength(data.dep = ki_moshi, 
            pos.na = which(is.na(methods::slot(ki_moshi, "Parameter")$TEMP)), 
            gap.limit = 365,
            units = "days", 
            end.datetime = Sys.Date())
}

}
\author{
Florian Detsch
}

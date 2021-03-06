% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/make_gsod_set.R
\name{make_GSOD_set}
\alias{make_GSOD_set}
\title{Make a data set of GSOD data suitable for interpolation}
\usage{
make_GSOD_set(years, dsn = NULL)
}
\arguments{
\item{years}{A numeric vector of years of \acronym{GSOD} data to get for
interpolation.  Defaults to current year.}

\item{dsn}{Optional. A file path where resulting \pkg{fst} files are to be
saved on local disk. If unspecified a tidy data frame is returned in the
\R session.}
}
\value{
A \code{base::list()} of \code{base::data.frame} objects containing
\acronym{GSOD} data suitable for interpolation using \code{interpolate_gsod()}
and optionally data files written to disk in \code{fst::fst()} format.
}
\description{
This function can be wrapped in an \code{base::lapply()} function to
retrieve and save multiple years of \acronym{GSOD} data for interpolation,
though it may be used to retrieve GSOD data and interpolate files on-the-fly.
See \code{utils::vignette("glint")} for more details and examples.
}
\details{
This function will get \acronym{GSOD} data using
\code{GSODR::get_GSOD()} and save a CSV file containing only the
following fields to use in interpolating a global surface between 60 and -60
degrees latitude.
\describe{
\item{STNID}{A unique station id number that can be used to identify the
station and link with \pkg{GSODR} data for station metadata.}
\item{LONGITUDE}{Longitude in decimal degrees.}
\item{LATITUDE}{Latitude in decimal degrees.}
\item{ELEVATION}{Elevation in metres.}
\item{YEAR}{The year (YYYY).}
\item{YDAY}{Sequential day of year.}
\item{TEMP}{Mean daily temperature in degrees C to tenths. Missing = NA.}
\item{MAX}{Maximum temperature reported during the day in Celsius to tenths.
Missing = NA;}
\item{MIN}{Minimum temperature reported during the day in Celsius to tenths.
Missing = NA;}
\item{RH}{Mean daily relative humidity. Missing = NA.}
}
}
\examples{
\donttest{
# Get one year of GSOD data
#gsod_1998 <- make_GSOD_set(years = 1998)
}

\donttest{
# Get multiple years of GSOD data and save to disk
#years <- as.list(seq(from = 1983, to = 2017, by = 1))
#lapply(X = years, FUN = make_GSOD_set, dsn = "~/Data/GSOD")
}
}
\author{
Adam H. Sparks, \email{adamhsparks@gmail.com}
}

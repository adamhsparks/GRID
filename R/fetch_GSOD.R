
#' Fetch GSOD Data and Subset Fields for Interpolation
#'
#' This function can be wrapped in an \code{\link[base]{lapply}} function to
#' retrieve and save multiple years of GSOD data for interpolation, though it
#' may be used to retrieve GSOD data and interpolate files on-the-fly. See
#' \code{vignette("GRID")} for more details and examples.
#'
#' @details This function will fetch GSOD data using
#' \code{\link[GSODR]{get_GSOD}} and save a CSV file containing only the
#' following fields to use in interpolating a global surface between 60 and -60
#' degrees latitude.
#' \describe{
#' \item{STNID}{A unique station id number that can be us ed to identify the
#' station and link with GSODR data for station metadata.}
#' \item{LON}{Longitude in decimal degrees.}
#' \item{LAT}{Latitude in decimal degrees.}
#' \item{ELEV_M_SRTM_90m}{Elevation in metres corrected for possible errors,
#' derived from the CGIAR-CSI SRTM 90m database (Jarvis et al. 2008).}
#' \item{YEAR}{The year (YYYY).}
#' \item{YDAY}{Sequential day of year.}
#' \item{TEMP}{Mean daily temperature in degrees C to tenths. Missing = NA.}
#' \item{MAX}{Maximum temperature reported during the day in Celsius to tenths.
#'  Missing = NA;}
#' \item{MIN}{Minimum temperature reported during the day in Celsius to tenths.
#'  Missing = NA;}
#' \item{RH}{Mean daily relative humidity.}
#' }
#'
#' @param years A numeric vector of years of GSOD data to fetch for
#' interpolation. Defaults to current year.
#' @param dsn Optional. A filepath where resulting CSV files are to be saved on
#' local disk. If unspecified a tidy data frame is returned in the R session.
#'
#' @references Jarvis, A., Reuter, H. I., Nelson, A., Guevara, E. (2008)
#' Hole-filled SRTM for the globe Version 4, available from the CGIAR-CSI SRTM
#' 90m Database (http://srtm.csi.cgiar.org)
#'
#' @return List of data frames
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Fetch one year of GSOD data
#' gsod_1998 <- fetch_GSOD(years = 1998)
#'
#' # Fetch multiple years of GSOD data and save to disk
#' years <- as.list(seq(from = 1983, to = 2017, by = 1))
#' lapply(X = years, FUN = fetch_GSOD, dsn = "~/Data/GSOD")
#' }

fetch_GSOD <- function(years = NULL, dsn = NULL) {
  # check user inputs, see internal_functions.R for these functions
  year_list <- .validate_year(years)
  dsn <- .validate_dsn(dsn)

  # fetch GSOD data from NCEI server
  weather <- GSODR::get_GSOD(years = year_list,
                             max_missing = 5,
                             agroclimatology = TRUE)

  # select only the fields that are necessary for or to be interpolated
  # this saves more than 1/2 the space of the full original data in storage
  weather <-
    weather[, c("STNID",
                "LON",
                "LAT",
                "ELEV_M_SRTM_90m",
                "YEAR",
                "YDAY",
                "TEMP",
                "MAX",
                "MIN",
                "RH")]

  # remove any stations lacking elevation data,
  # they cannot be used in calculations
  weather <- weather[!is.na(weather$ELEV_M_SRTM_90m),]

  # create a list of data frames to return
  weather <- split(weather, weather$YEAR)

  # if dsn is specifed write data frames to files, see internal_funtions.R
  # for .write_gsod()
  if (!is.null(dsn)) {
    lapply(X =  weather, FUN = .write_GSOD, dsn = dsn)
  }

  return(weather)
}

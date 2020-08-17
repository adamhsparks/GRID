
#' Make a Data Set of \acronym{GSOD} Data Suitable for Interpolation
#'
#' This function can be wrapped in an `base::lapply()` function to
#' retrieve and save multiple years of \acronym{GSOD} data for interpolation,
#' though it may be used to retrieve GSOD data and interpolate files on-the-fly.
#' See `utils::vignette("glint")` for more details and examples.
#'
#' @details This function will get \acronym{GSOD} data using
#' `GSODR::get_GSOD()` and save a CSV file containing only the
#' following fields to use in interpolating a global surface between 60 and -60
#' degrees latitude.
#' \describe{
#' \item{STNID}{A unique station id number that can be used to identify the
#' station and link with \pkg{GSODR} data for station metadata.}
#' \item{LON}{Longitude in decimal degrees.}
#' \item{LAT}{Latitude in decimal degrees.}
#' \item{ELEV_M_SRTM_90m}{Elevation in metres corrected for possible errors,
#' derived from the \acronym{CGIAR-CSI} \acronym{SRTM} 90 m database (Jarvis et
#' al. 2008).}
#' \item{YEAR}{The year (YYYY).}
#' \item{YDAY}{Sequential day of year.}
#' \item{TEMP}{Mean daily temperature in degrees C to tenths. Missing = NA.}
#' \item{MAX}{Maximum temperature reported during the day in Celsius to tenths.
#'  Missing = NA;}
#' \item{MIN}{Minimum temperature reported during the day in Celsius to tenths.
#'  Missing = NA;}
#' \item{RH}{Mean daily relative humidity. Missing = NA.}
#' }
#'
#' @param years A numeric vector of years of \acronym{GSOD} data to get for
#' interpolation.  Defaults to current year.
#' @param dsn Optional. A filepath where resulting \pkg{fst} files are to be
#' saved on local disk. If unspecified a tidy data frame is returned in the
#' \R session.
#'
#' @references Jarvis, A., Reuter, H. I., Nelson, A., Guevara, E. (2008)
#' Hole-filled SRTM for the globe Version 4, available from the CGIAR-CSI SRTM
#' 90m Database (<http://srtm.csi.cgiar.org>)
#'
#' @return A `base::list()` of `base::data.frame` objects containing
#' \acronym{GSOD} data suitable for interpolation using `interpolate_gsod()`
#' and optionally data files written to disk in `fst::fst()` format.
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#'
#' @examples
#' \donttest{
#' # Get one year of GSOD data
#' #gsod_1998 <- make_GSOD_set(years = 1998)
#' }
#'
#' \donttest{
#' # Get multiple years of GSOD data and save to disk
#' #years <- as.list(seq(from = 1983, to = 2017, by = 1))
#' #lapply(X = years, FUN = make_GSOD_set, dsn = "~/Data/GSOD")
#' }
#' @export make_GSOD_set

make_GSOD_set <- function(years, dsn = NULL) {
  # check user inputs, see internal_functions.R for these functions
  year_list <- .validate_year(years)

  if (!is.null(dsn)) {
    dsn <- .validate_dsn(dsn)
  }
  # get GSOD data from NCEI server
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
  weather <- weather[!is.na(weather$ELEV_M_SRTM_90m), ]

  # create a list of data frames to return
  weather <- split(weather, weather$YEAR)

  # if dsn is specified write data frames to files, see internal_functions.R
  # for .write_gsod()
  if (!is.null(dsn)) {
    future.apply::future_lapply(X =  weather,
                                FUN = .write_GSOD,
                                dsn = dsn)
  }
  return(weather)
}

#' Write a Compressed 'fst' File to Disk
#'
#' Writes compressed fst files of weather data to disk.
#' @param weather A data set from the 'NCEI' 'GSOD' 'FTP' server that is slimmed
#' down to only variables necessary for interpolation and Temp, Max, Min and RH
#' to save disk space
#' @param dsn User supplied value of directory to write files to
#' @noRd
.write_GSOD <- function(weather, dsn) {
  # create YEAR object for naming object out
  YEAR <- weather$YEAR[1]

  # create file name
  fname <- paste0("GSOD_", YEAR)

  # write a compressed 'fst' file to disk in the specified location
  fst::write_fst(weather, path = file.path(dsn, fname), 100)

  gsod_files <-
    list.files(tempdir(), pattern = ".gz$|.tar$", full.names = TRUE)
  on.exit(unlink(gsod_files, recursive = TRUE))
}

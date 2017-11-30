
#' Fetch GSOD Data Using GSODR, Subset Fields for Interpolation and Save CSV Files by Year
#'
#' This function is designed to be wrapped in an \code{\link[base]{lapply}}
#' function to retrieve multiple years of GSOD data for interpolation, though a
#' single year may be used.
#'
#' @details This function will fetch GSOD data using
#' \code{\link[GSODR]{get_gsod}} and save a CSV file containing only the
#' following fields to use in interpolating a global surface between 60˚and -60˚
#' latitude.
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
#' @param year_list A numeric vector of years of GSOD data to fetch for
#' interpolation. Defaults to current year.
#' @param dsn A filepath where resulting CSV files are to be saved. Defaults
#' to user's "home" directory.
#'
#' @references Jarvis, A., Reuter, H. I., Nelson, A., Guevara, E. (2008)
#' Hole-filled SRTM for the globe Version 4, available from the CGIAR-CSI SRTM
#' 90m Database (http://srtm.csi.cgiar.org)
#'
#' @return CSV file of GSOD data in compressed .bz2 format
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Fetch one year of GSOD data
#' fetch_GSOD(year_list = 1998, dsn = "~/GSOD")
#'
#' # Fetch multiple years of GSOD data
#' years <- as.list(seq(from = 1983, to = 2017, by = 1))
#'
#' lapply(X = years, FUN = fetch_gsod, dsn = "~/GSOD")
#' }

fetch_gsod <- function(year_list = NULL, dsn = NULL) {

  # check year list, if not specified default to current year
  if (is.null(year_list)) {
    message("\nYou have not specified any years to fetch, defaulting to ",
            format(Sys.Date(), "%Y"), ".\n")
    year_list <- format(Sys.Date(), "%Y")
  }

  # check if the dsn exists
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

  # create YEAR object for naming file out
  YEAR <- weather$YEAR[1]

  # create file name
  fname <- paste0("GSOD_", YEAR, ".bz2")

  # write a compressed CSV file to disk in the specified location
  readr::write_csv(weather, path = file.path(dsn, fname), na = "NA")

  # clean up and free up RAM/swap
  rm(weather)
  gc()
}

#' @noRd
.validate_dsn <- function(dsn) {
  if (is.null(dsn)) {
    dsn <- getwd()
  } else {
    if (substr(dsn, nchar(dsn) - 1, nchar(dsn)) == "//") {
      dsn <- substr(dsn, 1, nchar(dsn) - 2)
    } else if (substr(dsn, nchar(dsn), nchar(dsn)) == "/" |
               substr(dsn, nchar(dsn), nchar(dsn)) == "\\") {
      dsn <- substr(dsn, 1, nchar(dsn) - 1)
    }
    if (!dir.exists(dsn) & !dir.exists(dsn)) {
      stop("\nThis dsn does not exist: ", dsn, ".\n")
    }
  }
}

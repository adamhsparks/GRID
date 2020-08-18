
#' Interpolate GSOD data to a daily gridded surface
#'
#' This function is designed to be wrapped in an `base::lapply()`
#' function to process multiple years of \acronym{GSOD} data for interpolation,
#' though a single year may be used. Output is written to a `fst::fst` file.
#'
#' @param x A `base::list()` of data frames or `fst` files of \acronym{GSOD}
#'  data created by `make_GSOD_set()`.
#' @param dem Digital elevation model that has been fetched and processed using
#' `glint::make_DEM()`.
#' @param dsn Optional. A file path where resulting \pkg{fst} files are to be
#' saved on local disk. If unspecified a tidy data frame is returned in the
#' \R session.
#' @param fname Optional. A file name for the data to be saved as a `fst::fst()`
#' compressed file object on local disk.
#' @param vars Weather variables to interpolate. Possible values are
#' `TEMP`, `MAX`, `MIN` and `RH`. Defaults to `TEMP`.
#' @param dates Optional. A vector of dates to optionally filter and
#'  interpolate. If left `NULL`, the default, all days of the specified year(s)
#'  are interpolated and returned. Values should be entered as a complete
#'  date in ISO 8601 format, *e.g.* `"2017-07-15` or a vector, *e.g.*
#'  `c("2017-07-15", "2017-05-01", "2017-02-15")`.
#'
#' @return
#' A `terra::SpatRaster()` of daily interpolated weather variables and
#' optionally writes values to a `fst::fst()` compressed file.
#'
#' @author \email{adamhsparks@@gmail.com}
#'
#' @examples
#' \donttest{
#' # Get and aggregate the raster digital elevation model
#' #dem <- make_DEM()
#'
#' # Create a list of GSOD files
#' #files <- list.files("~/Data/GSOD", full.names = TRUE)
#'
#' # Run the function for MAX and MIN temperature using parallel processing
#' #future::plan("multisession")
#' #GRID <- lapply(X = files, FUN = interpolate_GSOD, dem = dem,
#' # dsn = "~/Cache/GTiff", vars = c("MAX", "MIN"))
#'
#' # Run the function for MAX and MIN temperature using a single core
#' #future::plan("sequential")
#' #GRID <- lapply(X = files, FUN = interpolate_GSOD, dem = dem, vars = "MAX")
#' }
#' @export interpolate_GSOD

interpolate_GSOD <- function(x,
                             dem,
                             dsn = NULL,
                             fname,
                             vars = NULL,
                             dates = NULL) {
  # validate user inputs, see `internal_functions.R` or below for these

  vars <- .validate_vars(vars)
  dates <- .validate_dates(dates)

  # if x is a list of files then validate and import
  if (is.null(x)) {
    stop("You must supply a list of GSOD data files or objects for interpolation")
  }

  # if x is not a set of in-memory objects, import the fst files
  if (is.character(x)) {
    x <-
      fst::read_fst(x, as.data.table = TRUE)
  }

  # CRAN note avoidance
  MAX <- MIN <- RH <- NULL

  if (!is.null(dates)) {
    x <- subset(x, YDAY %in% dates)
  }

  # If we have a full data.frame of all days.
  # Create a list of data frames by YDAY
  if (unique(x$YDAY > 1)) {
    GSOD <- split(x, as.factor(x$YDAY))
  }

  # Apply function for each `wvar` that is specified
  if ("TEMP" %in% vars) {
    TEMP <- .create_stack(
      GSOD = GSOD,
      wvar = "TEMP",
      dem = dem,
      dsn = dsn
    )
  } else {
    TEMP <- NULL
  }

  if ("MAX" %in% vars) {
    MAX <- .create_stack(
      GSOD = GSOD,
      wvar = "MAX",
      dem = dem,
      dsn = dsn
    )
  } else {
    MAX <- NULL
  }

  if ("MIN" %in% vars) {
    MIN <- .create_stack(
      GSOD = GSOD,
      wvar = "MIN",
      dem = dem,
      dsn = dsn
    )
  } else {
    MIN <- NULL
  }

  if ("RH" %in% vars) {
    RH <- .create_stack(
      GSOD = GSOD,
      wvar = "RH",
      dem = dem,
      dsn = dsn
    )

  } else {
    RH <- NULL
  }

  # remove any null vars
  out <- list(TEMP, MAX, MIN, RH)
  out <- out[unlist(lapply(out, length) != 0)]

  # create final stack of vars
  out <- c(out)

  # assign GSOD_YYYY to list objects before returning list
  out <- stats::setNames(out,
                         paste0(substr(x,
                                       nchar(x) - 13 + 1,
                                       nchar(x) - 5 + 1),
                                "_",
                                names(out)))

  return(out)
}

#' Create a terra SpatRaster object of weather variables
#'
#' Creates raster stacks of weather variables
#'
#' @param GSOD A list of GSOD dataframes `split` by day.
#' @param wvar Weather variable to interpolate.
#' @param dem Digital elevation model that has been fetched and processed using
#' `make_DEM()`.
#' @param dsn Optional. Directory where resulting GeoTIFF files are to be saved.
#' if not otherwise specified.
#'
#' @noRd
.create_stack <- function(GSOD, wvar, dem, dsn) {
  Y <- future.apply::future_lapply(
    X = GSOD,
    FUN = .interpolate_raster,
    wvar = wvar,
    dem = dem,
    dsn = dsn
  )
  Y <- .stack_lists(X = Y, wvar = wvar)
  return(Y)
}

#' Create a stack from lists of terra SpatRaster class objects
#'
#' Called from `.create_stack()` at the end of the function to create a raster
#' stack of layers from lists resulting from using `future_lapply()`
#'
#' @param X A list of interpolated weather variable surfaces.
#' @param wvar Interpolated weather variable.
#' @noRd
.stack_lists <- function(X, wvar) {
  X <- c(X[seq_along(X)])
  X <- stats::setNames(X,
                       paste0(wvar, "_", 1:terra::nlyr(X)))
}

#' @noRd
.validate_x <- function(x) {
  if (is.null(x)) {
    stop("You must supply a list of GSOD data files or objects for interpolation")
  } else if (typeof(x[[1]]) == "character") {
    x <- x
  }
}

#' @noRd
.validate_dates <- function(dates) {
  if (!is.null(dates)) {
    # I stole this from my own nasapower package #
    dates <- as.list(dates)

    # check dates as entered by user
    date_format <- function(x) {
      tryCatch(
        # try to parse the date format using lubridate
        x <- lubridate::parse_date_time(x,
                                        c(
                                          "Ymd",
                                          "dmY",
                                          "mdY",
                                          "BdY",
                                          "Bdy",
                                          "bdY",
                                          "bdy"
                                        )),
        warning = function(c) {
          stop(call. = FALSE,
               "\n",
               x,
               " is not a valid entry for date. Enter as YYYY-MM-DD.\n")
        }
      )
      as.Date(x)
      x <- format(x, "%j")
      return(x)
    }

    # apply function to reformat/check dates
    dates <- lapply(X = dates, FUN = date_format)
    dates <- unlist(lapply(dates, as.numeric))
  }
  # end stealing
}

#' Create an Interpolated Surface of a Weather Variable
#'
#' Called from `.create_stack()`, does the heavy lifting of checking for
#' outliers and then interpolating the data
#'
#' @param GSOD A list of GSOD dataframes `split` by day.
#' @param wvar Weather variable to interpolate.
#' @param dem Digital elevation model that has been fetched and processed using
#' `make_DEM()`.
#' @param dsn Optional. A filepath where resulting \pkg{fst} files are to be
#' saved on local disk. If unspecified a tidy data frame is returned in the
#' \R session.
#' @noRd
.interpolate_raster <- function(GSOD, wvar, dsn, fname, dem) {
  # create data frame for individual weather vars for interpolation
  y <-
    data.frame(GSOD["LON"], GSOD["LAT"], GSOD["ELEV_M_SRTM_90m"],
               GSOD[wvar])

  # remove any NA values from the data, the interpolation will not work with
  # any NAs present for any field.
  y <- stats::na.omit(y)

  # remove outliers
  bxs <- grDevices::boxplot.stats(y[, 4])
  y <- y[!y[, 4] %in% bxs$out, ]

  # create interpolation data set
  y_vals <- y[, 4]
  names(y_vals) <- NULL

  # create thin plate spline object
  tps_y <-
    fields::Tps(y[, c("LON", "LAT", "ELEV_M_SRTM_90m")],
                y_vals, lon.lat = TRUE)

  # interpolate thin plate spline object
  tps_pred <- terra::interpolate(dem, tps_y, xyOnly = FALSE)

  # if a dsn is provided write to database as numeric values, else return
  # in-memory
  if (!is.null(dsn)) {
    # convert the raster object to point values
    tps_pred <- terra::as.points(tps_pred)
    fst::write_fst(tps_pred, path = file.path(dsn, fname), 100)
  }
  return(tps_pred)
}

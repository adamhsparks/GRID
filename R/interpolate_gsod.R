
#' Interpolate \acronym{GSOD} Data to a Gridded Surface
#'
#' This function is designed to be wrapped in an `base::lapply()`
#' function to process multiple years of \acronym{GSOD} data for interpolation,
#' though a single year may be used. Output is written to a MonetDB database.
#'
#' @param file_list A `base::list()` of data frames or `fst` files of
#' \acronym{GSOD} data created by `make_GSOD_set()`.
#' @param dem Digital elevation model that has been fetched and processed using
#' `make_DEM()`.
#' @param dsn A DBIConnection object, as returned by dbConnect().
#' @param db_name A character string specifying the unquoted DBMS table name.
#' @param vars Weather variables to interpolate. Possible values are
#' `TEMP`, `MAX`, `MIN` and `RH`. Defaults to `TEMP`.
#'
#' @return
#' A `raster::stack()` of daily interpolated weather variables and writes
#' values to a MonetDB database.
#'
#' @author \email{adamhsparks@@gmail.com}
#'
#' @examples
#' \donttest{
#' # Get and aggregate the raster digital elevation model
#' dem <- make_DEM()
#'
#' # Create a list of GSOD files
#' files <- list.files("~/Data/GSOD", full.names = TRUE)
#'
#' # Run the function for MAX and MIN temperature using parallel processing
#' future::plan("multisession")
#' GRID <- lapply(X = files, FUN = interpolate_GSOD, dem = dem,
#' 		  dsn = "~/Cache/GTiff", vars = c("MAX", "MIN"))
#'
#' # Run the function for MAX and MIN temperature using a single core
#' future::plan("sequential")
#' GRID <- lapply(X = files, FUN = interpolate_GSOD, dem = dem, vars = "MAX")
#' }
#' @export interpolate_GSOD

interpolate_GSOD <- function(file_list = NULL,
                             dem = NULL,
                             dsn = NULL,
                             vars = NULL) {
  # validate user inputs, see `internal_functions.R` for these
  dsn <- .validate_dsn(dsn)
  vars <- .validate_vars(vars)
  file_list <- .validate_files(file_list)

  # CRAN note avoidance
  MAX <- MIN <- RH <- NULL

  # Import GSOD data
  GSOD <-
    fst::read_fst(file_list)

  # Create a list of data frames by YDAY
  GSOD <- split(GSOD, as.factor(GSOD$YDAY))

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
  out <- raster::stack(out)

  # assign GSOD_YYYY to list objects before returning list
  out <- stats::setNames(out,
                         paste0(substr(file_list,
                                       nchar(file_list) - 13 + 1,
                                       nchar(file_list) - 5 + 1),
                                "_",
                                names(out)))

  return(out)
}

#' Create a Raster Stack Object of Weather Variables
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

#' Create a Stack From Lists of Raster Objects
#'
#' Called from `.create_stack()` at the end of the function to create a raster
#' stack of layers from lists resulting from using `future_lapply()`
#'
#' @param X A list of interpolated weather variable surfaces.
#' @param wvar Interpolated weather variable.
#' @noRd
.stack_lists <- function(X, wvar) {
  X <- raster::stack(X[seq_along(X)])
  X <- stats::setNames(X,
                       paste0(wvar, "_", 1:raster::nlayers(X)))
}

#' @noRd
.validate_files <- function(file_list) {
  if (is.null(file_list)) {
    stop("You must supply a list of GSOD data files for interpolation")
  } else if (typeof(file_list[[1]]) == "character") {
    file_list <- file_list
  }
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
#' @param dsn A DBIConnection object, as returned by dbConnect().
#' @param db_name A character string specifying the unquoted DBMS table name. 
#' @noRd
.interpolate_raster <- function(GSOD, wvar, dsn, db_name, dem) {
  # create data frame for individual weather vars for interpolation
  y <-
    data.frame(GSOD["LON"], GSOD["LAT"], GSOD["ELEV_M_SRTM_90m"],
               GSOD[wvar])

  # remove any NA values from the data, the interpolation will not work with
  # any NAs present for any field.
  y <- stats::na.omit(y)

  # remove outliers
  bxs <- grDevices::boxplot.stats(y[, 4])
  y <- y[!y[, 4] %in% bxs$out,]

  # create interpolation data set
  y_vals <- y[, 4]
  names(y_vals) <- NULL

  # create thin plate spline object
  tps_y <-
    fields::Tps(y[, c("LON", "LAT", "ELEV_M_SRTM_90m")],
                y_vals, lon.lat = TRUE)

  # interpolate thin plate spline object
  tps_pred <- raster::interpolate(dem, tps_y, xyOnly = FALSE)

  # if a dsn is provided write to database, else return in memory
  if (!is.null(dsn)) {
	  # convert the raster object to 
	  tps_pred <- raster::rasterToPoints(tps_pred)
	  DBI::dbWriteTable(conn = dsn, name = db_name, value = tps_pred)
  }
  return(tps_pred)
}

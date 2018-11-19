#' Add %notin% Function
#'
#' Negates `%in%`` for easier matching.
#'
#' @param x A character string to match.
#' @param table A table containing values to match `x` against.
#'
#' @return A function to use for checking if something is not in a table
#'
#' @noRd
`%notin%` <- function(x, table) {
  # Same as !(x %in% table)
  match(x, table, nomatch = 0L) == 0L
}

#' Create a Raster Stack Object of Weather Variables
#'
#' Creates raster stacks of weather variables
#'
#' @param GSOD A list of GSOD dataframes `split` by day
#' @param wvar Weather variable to interpolate
#' @param dem Digital elevation model that has been fetched and processed using
#' `get_DEM()`.
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

#' Create an Interpolated Surface of a Weather Variable
#'
#' Called from `.create_stack()`, does the heavy lifting of checking for
#' outliers and then interpolating the data
#'
#' @param GSOD A list of GSOD dataframes `split` by day
#' @param wvar Weather variable to interpolate
#' @param dem Digital elevation model that has been fetched and processed using
#' `get_DEM()`.
#' @param dsn Optional. Directory where resulting GeoTIFF files are to be saved.
#'
#' @noRd
.interpolate_raster <- function(GSOD, wvar, dsn, dem) {
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

  # if a dsn is provided write to local disk, else return in memory
  if (!is.null(dsn)) {
    # write to disk with "YYYY_YDAY.tiff" as the name
    raster::writeRaster(
      tps_pred,
      filename = paste0(dsn,
                        "/",
                        wvar,
                        "_",
                        GSOD[1, 5],
                        "_", GSOD[1, 6],
                        ".tiff"),
      format = "GTiff",
      dataType = "INT2S",
      options = c("COMPRESS=LZW", "TFW=YES"),
      overwrite = TRUE
    )
  }
  return(tps_pred)
}

#' Create a Stack From Lists of Raster Objects
#'
#' Called from `.create_stack()` at the end of the function to create a raster
#' stack of layers from lists resulting from using `future_lapply()`
#'
#' @param X A list of interpolated weather variable surfaces
#' @param wvar Interpolated weather variable
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

#' Check User Entered DSN Value
#'
#' Verifies that the dsn argument entered by the user is valid and exists for
#' writing files out
#' @param dsn User supplied value of directory to write files to
#'
#' @noRd
.validate_dsn <- function(dsn) {
  if (substr(dsn, nchar(dsn) - 1, nchar(dsn)) == "//") {
    dsn <- substr(dsn, 1, nchar(dsn) - 2)
  } else if (substr(dsn, nchar(dsn), nchar(dsn)) == "/" |
             substr(dsn, nchar(dsn), nchar(dsn)) == "\\") {
    dsn <- substr(dsn, 1, nchar(dsn) - 1)
  } else if (!dir.exists(dsn) & !dir.exists(dsn)) {
    stop("\nThis dsn does not exist: ", dsn, ".\n")
  } else {
    dsn <- dsn
  }
}

#' Validate Max Missing Argument
#'
#' Validates argument for max missing to make sure it is positive as entered by
#' the user
#' @param missing User supplied value for the maximum number of missing values
#' allowed
#' @noRd
.validate_max_missing <- function(missing) {
  if (!is.null(missing)) {
    if (is.na(missing) | missing < 1) {
      stop("\nThe 'max_missing' parameter must be a positive",
           "value larger than 1\n")
    }
  }
}

#' Validate User Entered Spatial Resolution
#'
#' Validates user supplied values for the resolution to which the DEM should be
#' aggregated.
#' @param resolution User supplied value for the desired resolution
#' @noRd
.validate_resolution <- function(resolution) {
  if (any(resolution %notin% c(NULL, 0.25, 0.5, 1))) {
    stop("The resolution you have specified is not valid.\n",
         "It should be one of: 1, 0.5 or 0.25.")
  } else if (is.null(resolution)) {
    agg <- 6
  } else {
    agg <- dplyr::case_when(resolution == 0.25 ~ 3,
                            resolution == 0.5 ~ 6,
                            resolution == 1 ~ 12)
  }
}
#' Validate User Entered Weather Variables
#'
#' Validates user supplied weather variables to ensure that they are all valid.
#' Defaults to TEMP if not supplied.
#'
#' @param vars Users supplied value(s) for weather variables to interpolate
#'
#' @noRd
.validate_vars <- function(vars) {
  if (is.null(vars)) {
    vars <- "TEMP"
  } else {
    vars <- toupper(vars)

    if (any(vars %notin% c("TEMP", "RH", "MAX", "MIN"))) {
      stop("One or more of your weather variable(s) are\n",
           "not valid for interpolation.\n")
    }
  }
  return(vars)
}

#' Validate User Entered Year Variable
#'
#' Validates user supplied year argument. If not specified defaults to current.
#' @param years User supplied value(s) for years of weather to interpolate
#'
#' @noRd
# check year list, if not specified default to current year
.validate_year <- function(years) {
  if (is.null(years)) {
    message(
      "\nYou have not specified any years to get, defaulting to ",
      format(Sys.Date(), "%Y"),
      ".\n"
    )
    years <- as.numeric(format(Sys.Date(), "%Y"))
  } else {
    years <- years
  }
}

#' Write a Compressed CSV File to Disk
#'
#' Writes compressed CSV files of weather data to disk.
#' @param weather A data set from the NCEI GSOD FTP server that is slimmed
#' down to only variables necessary for interpolation and Temp, Max, Min and RH
#' to save disk space
#' @param dsn User supplied value of directory to write files to
#' @noRd
.write_GSOD <- function(weather, dsn) {
  # create YEAR object for naming object out
  YEAR <- weather$YEAR[1]

  # create file name
  fname <- paste0("GSOD_", YEAR, ".bz2")

  # write a compressed CSV file to disk in the specified location
  readr::write_csv(weather, path = file.path(dsn, fname), na = "NA")
}

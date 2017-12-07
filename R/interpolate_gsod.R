

#' Interpolate GSOD Data to a Gridded Surface
#'
#' This function is designed to be wrapped in an \code{\link[base]{lapply}}
#' function to process multiple years of GSOD data for interpolation, though a
#' single year may be used.
#'
#' @param GSOD A \code{\link[base]{list}} of data frames or CSV files of GSOD
#' data created by \link{fetch_GSOD}.
#' @param dem Digital elevation model that has been fetched and processed using
#' \code{\link{fetch_DEM}}.
#' @param dsn Optional. Directory where resulting GeoTIFF files are to be saved.
#' @param vars Weather variables to interpolate. Possible values are,
#' \code{TEMP}, \code{MAX}, \code{MIN}, \code{RH}. Defaults to \code{TEMP}.
#'
#' @return
#' \code{List} of \code{\link[raster]{stack}} objects by year and variable
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a list of GSOD files
#' file_list <- list.files("~/Data/GSOD", full.names = TRUE)
#'
#' # Fetch and aggregate the raster digital elevation model
#' dem <- fetch_DEM()
#'
#' # Run the function for MAX and MIN temperature
#' GRID <- pblapply(X = file_list, FUN = interpolate_GSOD,
#'                      dem = dem, vars = c("MAX", "MIN"))
#' }
#'

interpolate_GSOD <- function(GSOD = NULL,
                             dem = NULL,
                             dsn = NULL,
                             vars = NULL) {
  # validate user inputs, see `internal_functions.R` for these
  dsn <- .validate_dsn(dsn)
  vars <- .check_vars(vars)
  GSOD <- .check_GSOD(GSOD)

  # Import GSOD data
  GSOD <-
    readr::read_csv(GSOD, col_types = "cdddcddddd", progress = FALSE)

  # Create a list of data frames by YDAY
  GSOD <- split(GSOD, as.factor(GSOD$YDAY))

  # Apply function for each var that is specified
  if ("TEMP" %in% vars) {
    TEMP <- .create_stack(
      GSOD = GSOD,
      var = "TEMP",
      dem = dem,
      dsn = dsn
    )
  } else {
    TEMP <- NULL
  }

  if ("MAX" %in% vars) {
    MAX <- .create_stack(
      GSOD = GSOD,
      var = "MAX",
      dem = dem,
      dsn = dsn
    )
  } else {
    MAX <- NULL
  }

  if ("MIN" %in% vars) {
    MIN <- .create_stack(
      GSOD = GSOD,
      var = "MIN",
      dem = dem,
      dsn = dsn
    )
  } else {
    MIN <- NULL
  }

  if ("RH" %in% vars) {
    RH <- .create_stack(
      GSOD = GSOD,
      var = "RH",
      dem = dem,
      dsn = dsn
    )
  } else {
    RH <- NULL
  }

  # create a list of the raster stacks, name it, remove NULL items
  out <- list(c(TEMP, MAX, MIN, RH))
  return(out)
}

#' @noRd
.create_stack <- function(GSOD, var, dem, dsn) {
  weather <-
    lapply(
      X = GSOD,
      FUN = .interpolate_raster,
      var = var,
      dem = dem,
      dsn = dsn
    )
  weather <- raster::stack(weather[seq_along(weather)])
  weather <-
    setNames(weather, paste0(var, "_", 1:raster::nlayers(weather)))
  return(weather)
}

#' @noRd
.interpolate_raster <- function(GSOD, var, dsn, dem) {
  # create data frame for individual weather vars for interpolation
  y <-
    data.frame(GSOD["LON"], GSOD["LAT"], GSOD["ELEV_M_SRTM_90m"], GSOD[var])

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
  tps_pred <- raster::interpolate(dem, tps_y, xyOnly = FALSE)

  # if a dsn is provided write to local disk, else return in memory
  if (!is.null(dsn)) {
    # write to disk with "YYYY_YDAY.tiff" as the name
    raster::writeRaster(
      tps_pred,
      filename = paste0(dsn,
                        "/",
                        var,
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


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
#' @param cores Number of cores to use for parallel processing. Defaults to 1 on
#' Windows OS or if not specified.
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
#' # Run the function for MAX and MIN temperature on a *nix system using 2 cores
#'
#' GRID <- lapply(X = file_list, FUN = interpolate_GSOD, dem = dem,
#' dsn = "~/Cache/GTiff", vars = c("MAX", "MIN"), cores = 2)
#'
#' # Run the function for MAX and MIN temperature using 1 core, will work on Win
#'
#' GRID <- lapply(X = file_list, FUN = interpolate_GSOD, dem = dem,
#' dsn = "~/Cache/GTiff", vars = c("MAX", "MIN"))
#' }
#'

interpolate_GSOD <- function(GSOD = NULL,
                             dem = NULL,
                             dsn = NULL,
                             vars = NULL,
                             cores = NULL) {
  # validate user inputs, see `internal_functions.R` for these
  dsn <- .validate_dsn(dsn)
  vars <- .validate_vars(vars)
  GSOD <- .validate_GSOD(GSOD)
  cores <- .validate_cores(cores)

  # Import GSOD data
  GSOD <-
    readr::read_csv(GSOD, col_types = "cdddcddddd", progress = FALSE)

  # Create a list of data frames by YDAY
  GSOD <- split(GSOD, as.factor(GSOD$YDAY))

  # Apply function for each wvar that is specified
  if ("TEMP" %in% vars) {
    TEMP <- .create_stack(
      GSOD = GSOD,
      wvar = "TEMP",
      dem = dem,
      dsn = dsn,
      cores = cores,
      year = GSOD[1, 5]
    )
    TEMP <- .stack_lists(X = TEMP, wvar = "TEMP")
  } else {
    TEMP <- NULL
  }

  if ("MAX" %in% vars) {
    MAX <- .create_stack(
      GSOD = GSOD,
      wvar = "MAX",
      dem = dem,
      dsn = dsn,
      cores = cores,
      year = GSOD[1, 5]
    )
    MAX <- .stack_lists(X = MAX, wvar = "MAX")
  } else {
    MAX <- NULL
  }

  if ("MIN" %in% vars) {
    MIN <- .create_stack(
      GSOD = GSOD,
      wvar = "MIN",
      dem = dem,
      dsn = dsn,
      cores = cores,
      year = GSOD[1, 5]
    )
    MIN <- .stack_lists(X = MIN, wvar = "MIN")
  } else {
    MIN <- NULL
  }

  if ("RH" %in% vars) {
    RH <- .create_stack(
      GSOD = GSOD,
      wvar = "RH",
      dem = dem,
      dsn = dsn,
      cores = cores,
      year = GSOD[1, 5]
    )
    RH <- .stack_lists(X = RH, wvar = "RH")
  } else {
    RH <- NULL
  }

  # create a list of the raster stacks, name it
  out <- list(c(TEMP = TEMP, MAX = MAX, MIN = MIN, RH = RH))
  return(out)
}

#' @noRd
.create_stack <- function(GSOD, wvar, dem, dsn, cores, year) {
  parallel::mclapply(
    X = GSOD,
    FUN = .interpolate_raster,
    wvar = wvar,
    dem = dem,
    dsn = dsn,
    mc.cores = cores,
    mc.preschedule = FALSE
  )
}

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
  names(tps_pred) <- paste0(year,
                            "_",
                            wvar,
                            "_",
                            GSOD[1, 5],
                            "_", GSOD[1, 6])
  return(tps_pred)
}

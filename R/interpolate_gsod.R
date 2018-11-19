
#' Interpolate \acronym{GSOD} Data to a Gridded Surface
#'
#' This function is designed to be wrapped in an `[base::lapply()]`
#' function to process multiple years of \acronym{GSOD} data for interpolation,
#' though a single year may be used.
#'
#' @param file_list A `[base::list()]` of data frames or `CSV` files of
#' GSOD data created by `make_GSOD_set()`.
#' @param dem Digital elevation model that has been fetched and processed using
#' `get_DEM()`.
#' @param dsn Optional. Directory where resulting GeoTIFF files are to be saved.
#' @param vars Weather variables to interpolate. Possible values are
#' `TEMP`, `MAX`, `MIN` and `RH`. Defaults to `TEMP`.
#'
#' @return
#' A `[raster::stack()]` of daily interpolated weather variables.
#'
#' @author \email{adamhsparks@@gmail.com}
#'
#' @examples
#' \donttest{
#' # Get and aggregate the raster digital elevation model
#' dem <- get_DEM()
#'
#' # Create a list of GSOD files
#' files <- list.files("~/Data/GSOD", full.names = TRUE)
#'
#' # Run the function for MAX and MIN temperature on a *nix system using parallel
#' # processing
#' future::plan(multisession)
#' GRID <- lapply(X = files, FUN = interpolate_GSOD, dem = dem,
#' 		  dsn = "~/Cache/GTiff", vars = c("MAX", "MIN"))
#'
#' # Run the function for MAX and MIN temperature using a single core
#' future::plan(sequential)
#' GRID <- lapply(X = files, FUN = interpolate_GSOD, dem = dem, vars = "MAX")
#' }
#'
#' @export interpolate_GSOD

interpolate_GSOD <- function(file_list = NULL,
                             dem = NULL,
                             dsn = NULL,
                             vars = NULL) {
  # validate user inputs, see `internal_functions.R` for these
  dsn <- .validate_dsn(dsn)
  vars <- .validate_vars(vars)
  file_list <- .validate_files(file_list)

  # Import GSOD data
  GSOD <-
    data.table::fread(file_list)

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

  if ("TMAX" %in% vars) {
    MAX <- .create_stack(
      GSOD = GSOD,
      wvar = "MAX",
      dem = dem,
      dsn = dsn
    )
  } else {
    MAX <- NULL
  }

  if ("TMIN" %in% vars) {
    MIN <- .create_stack(
      GSOD = GSOD,
      wvar = "MIN",
      dem = dem,
      dsn = dsn
    )
  } else {
    MIN <- NULL
  }

  if ("RHUM" %in% vars) {
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
  out <- list(TEMP, TMAX, TMIN, RHUM)
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

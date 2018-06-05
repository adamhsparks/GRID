
#' Interpolate GSOD Data to a Gridded Surface
#'
#' This function is designed to be wrapped in an \code{\link[base]{lapply}}
#' function to process multiple years of GSOD data for interpolation, though a
#' single year may be used.
#'
#' @param file_list A \code{\link[base]{list}} of data frames or CSV files of
#' GSOD data created by \link{get_GSOD}.
#' @param dem Digital elevation model that has been fetched and processed using
#' \code{\link{get_DEM}}.
#' @param dsn Optional. Directory where resulting GeoTIFF files are to be saved.
#' @param vars Weather variables to interpolate. Possible values are,
#' \code{TEMP}, \code{MAX}, \code{MIN}, \code{RH}. Defaults to \code{TEMP}.
#' @param cores Number of cores to use for parallel processing. Defaults to 1 on
#' Windows OS or if not specified.
#'
#' @return
#' A \code{\link[raster]{stack}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Get and aggregate the raster digital elevation model
#' dem <- get_DEM()
#'
#' # Create a list of GSOD files
#' files <- list.files("~/Data/GSOD", full.names = TRUE)
#'
#' # Run the function for MAX and MIN temperature on a *nix system using 2 cores
#' GRID <- lapply(X = files, FUN = interpolate_GSOD, dem = dem,
#' dsn = "~/Cache/GTiff", vars = c("MAX", "MIN"), cores = 2)
#'
#' # Run the function for MAX and MIN temperature using 1 core, will work on Win
#' GRID <- lapply(X = files, FUN = interpolate_GSOD, dem = dem, vars = "MAX")
#' }
#'

interpolate_GSOD <- function(file_list = NULL,
                             dem = NULL,
                             dsn = NULL,
                             vars = NULL,
                             cores = NULL) {
  # validate user inputs, see `internal_functions.R` for these
  dsn <- .validate_dsn(dsn)
  vars <- .validate_vars(vars)
  file_list <- .validate_files(file_list)
  cores <- .validate_cores(cores)

  # Import GSOD data
  GSOD <-
    readr::read_csv(file_list, col_types = "cdddcddddd", progress = FALSE)

  # Create a list of data frames by YDAY
  GSOD <- split(GSOD, as.factor(GSOD$YDAY))

  # Apply function for each wvar that is specified
  if ("TEMP" %in% vars) {
    TEMP <- .create_stack(
      GSOD = GSOD,
      wvar = "TEMP",
      dem = dem,
      dsn = dsn,
      cores = cores
    )
  } else {
    TEMP <- NULL
  }

  if ("MAX" %in% vars) {
    MAX <- .create_stack(
      GSOD = GSOD,
      wvar = "MAX",
      dem = dem,
      dsn = dsn,
      cores = cores
    )
  } else {
    MAX <- NULL
  }

  if ("MIN" %in% vars) {
    MIN <- .create_stack(
      GSOD = GSOD,
      wvar = "MIN",
      dem = dem,
      dsn = dsn,
      cores = cores
    )
  } else {
    MIN <- NULL
  }

  if ("RH" %in% vars) {
    RH <- .create_stack(
      GSOD = GSOD,
      wvar = "RH",
      dem = dem,
      dsn = dsn,
      cores = cores
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

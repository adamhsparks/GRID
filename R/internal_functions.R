
`%notin%` <- Negate("%in%")

#' @noRd
.create_stack <- function(GSOD, wvar, dem, dsn, cores) {
  Y <- parallel::mclapply(
    X = GSOD,
    FUN = .interpolate_raster,
    wvar = wvar,
    dem = dem,
    dsn = dsn,
    mc.cores = cores,
    mc.preschedule = FALSE
  )
  Y <- .stack_lists(X = Y, wvar = wvar)
  return(Y)
}

#' @noRd
.interpolate_raster <- function(GSOD, wvar, dsn, dem, year) {
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

#' @noRd
# check OS and set cores to 1 if NULL, Windows OS or unknown
# make sure that number of cores specifed less than number available
.validate_cores <- function(cores) {
  if (is.null(cores)) {
    cores <- 1
  }

  if (tolower(.Platform$OS.type) == "windows") {
    cores <- 1
  } else if (Sys.info()["sysname"] == "Darwin") {
    if (cores <= parallel::detectCores()) {
      cores <- cores
    } else {
      stop("You specified more cores than available for processing.")
    }
  } else if (.Platform$OS.type == "unix") {
    if (cores <= parallel::detectCores()) {
      cores <- cores
    } else {
      stop("You specified more cores than available for processing.")
    }
  }
}

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

#' @noRd
.validate_max_missing <- function(missing) {
  if (!is.null(missing)) {
    if (is.na(missing) | missing < 1) {
      stop("\nThe 'max_missing' parameter must be a positive",
           "value larger than 1\n")
    }
  }
}

#' @noRd
.validate_resolution <- function(resolution) {
  if (any(resolution %notin% c(NULL, 0.25, 0.5, 1))) {
    stop("The resolution you have specified is not valid.\n",
         "It should be one of: 1, 0.5 or 0.25.")
  } else if (is.null(resolution)) {
    agg <- 12
  } else {
    agg <- dplyr::case_when(resolution == 0.25 ~ 3,
                            resolution == 0.5 ~ 6,
                            resolution == 1 ~ 12)
  }
}

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

#' @noRd
.write_GSOD <- function(weather, dsn) {
  # create YEAR object for naming object out
  YEAR <- weather$YEAR[1]

  # create file name
  fname <- paste0("GSOD_", YEAR, ".bz2")

  # write a compressed CSV file to disk in the specified location
  readr::write_csv(weather, path = file.path(dsn, fname), na = "NA")
}

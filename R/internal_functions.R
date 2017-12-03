`%notin%` <- Negate("%in%")

#' @noRd
.check_gsod <- function(gsod) {
  if (is.null(gsod)) {
    stop("You must supply a list of GSOD data files for interpolation")
  } else if (typeof(gsod[[1]]) == "character") {
    gsod <- gsod
  } else {
    # import the file for interpolation
    gsod <- readr::read_csv(gsod, col_types = "cdddcddddd")
  }

}

#' @noRd
.check_vars <- function(vars) {
  if (is.null(vars)) {
    vars <- "TEMP"
  } else {
    vars <- toupper(vars)

    if (any(vars %notin% c("TEMP", "RH", "MAX", "MIN"))) {
      stop("One or more of your weather variable(s) are\n",
           "not valid for interpolation.\n")
    }
  }
}

#' @noRd
# check year list, if not specified default to current year
.check_year <- function(years) {
  if (is.null(years)) {
    message(
      "\nYou have not specified any years to fetch, defaulting to ",
      format(Sys.Date(), "%Y"),
      ".\n"
    )
    years <- as.numeric(format(Sys.Date(), "%Y"))
  } else {
    years <- years
  }
}

#' @noRd
.validate_dsn <- function(dsn) {
  if (!is.null(dsn)) {
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
  if (any(resolution %notin% c(NULL, .25, .5, 1))) {
    stop("The resolution you have specified is not valid.\n",
         "It should be one of: 1, .5 or .25.")
  } else if (is.null(resolution)) {
    agg <- 12
  } else {
    agg <- dplyr::case_when(resolution == .25 ~ 3,
                            resolution == .5 ~ 6,
                            resolution == 1 ~ 12)
  }
}

#' @noRd
write_gsod <- function(weather, dsn) {
  # create YEAR object for naming object out
  YEAR <- weather$YEAR[1]

  # create file name
  fname <- paste0("GSOD_", YEAR, ".bz2")

  # write a compressed CSV file to disk in the specified location
  readr::write_csv(weather, path = file.path(dsn, fname), na = "NA")
}

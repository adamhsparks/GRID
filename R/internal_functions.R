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
    stop(
      "The resolution you have specified is not valid.\n",
      "It should be one of: 1, 0.5 or 0.25."
    )
  } else if (is.null(resolution)) {
    agg <- 6
  } else {
    resolution <- as.character(resolution)
    agg <- switch(resolution,
                  "0.25" = 3,
                  "0.5" = 6,
                  "1" = 12)
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

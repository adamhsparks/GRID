`%notin%` <- Negate("%in%")

#' @noRd
.validate_dsn <- function(dsn) {
  if (is.null(dsn)) {
    dsn <- path.expand("~")
  } else  if (substr(dsn, nchar(dsn) - 1, nchar(dsn)) == "//") {
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
# check year list, if not specified default to current year
.check_year <- function(year_list) {
  if (is.null(year_list)) {
    message(
      "\nYou have not specified any years to fetch, defaulting to ",
      format(Sys.Date(), "%Y"),
      ".\n"
    )
    year_list <- format(Sys.Date(), "%Y")
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
.check_bz2 <- function(bz2_file) {
  if (is.null(bz2_file)) {
    stop("You must supply a list of GSOD data files for interpolation")
  }
}

#' @noRd
# check OS and set cores to 1 if NULL, Windows OS or unknown
# make sure that number of cores specifed less than number available
.validate_cores <- function(cores) {
  if (is.null(cores)) {
    cores <- 1
  }
  if (.Platform$OS.type == "windows") {
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

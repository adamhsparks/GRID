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


`%notin%` <- Negate("%in%")

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

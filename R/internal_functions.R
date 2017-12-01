#' @noRd
.validate_dsn <- function(dsn) {
  if (is.null(dsn)) {
    dsn <- getwd()
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

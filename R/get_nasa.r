
#' Download NASA/POWER temperature and relative humidity variables and write to local disk
#'
#' @param x Longitude in decimal degrees for cell to download
#' @param y Latitude in decimal degrees for cell to download
#' @param vars Weather variables to download, Temperature, Minimum Temperature,
#' Maximum Temperature, Relative Humidity
#' @param stdate Starting date for download, defaults to 01/01/1983
#' @param endate End date for download, defaults to current date
#' @param dsn Local file path to write file out to
#' @param filename The filename for resulting file(s) to be written to disk
#'
#' @examples
#' \dontrun{
#' get_nasa(lon = -179.5, lat = 89.5)
#' }
#'
#' @author Jorrel Khalil S. Aunario, jaunario@gmail.com

# Author:
# Date :  22 February 2011
# Version 0.0.1
# Licence GPL v3

get_nasa <-
  function(lon,
           lat,
           vars = c("T2M",
                    "T2MN",
                    "T2MX",
                    "RH2M"),
           stdate = "1983-1-1",
           endate = Sys.Date(),
           dsn = NULL,
           filename = NULL) {
    if (!require("curl")) {
      install.packages("curl", repos = "http://cran.rstudio.com/")
      library("devtools")
    }

    src <- ""
    if (length(lon) != 1 | length(lat) != 1) {
      message("Warning: Either `lon`` or `lat`` has length > 1. Using first\n",
              "only.\n",
              appendLF = TRUE)
      lon <- lon[1]
      lat <- lat[1]
    }

    result <- tibble::tibble(lon, lat)

    # check if downloaded file can be saved to disk
    .validate_fileout(dsn, filename)

    cell <-
      raster::cellFromXY(raster::raster(), raster::t(c(lon, lat)))
    result["stn"] <- as.character(cell)

    stdate <- as.Date(stdate)
    endate <- as.Date(endate)

    fname <-
      paste(paste(
        "nasa",
        sprintf("%06i", cell),
        sprintf("%05.1f", lon),
        sprintf("%05.1f", lat),
        format(stdate, "%Y.%m.%d"),
        format(endate, "%Y.%m.%d"),
        sep = "_"
      ),
      ".txt",
      sep = "")
    dlurl <-
      paste(
        "https://power.larc.nasa.gov/cgi-bin/cgiwrap/solar/agro.cgi?email=agroclim%2540larc.nasa.gov&amp;step=1&amp;lat=",
        lat,
        "&lon=",
        lon,
        "&ms=",
        lubridate::month(stdate),
        "&ds=",
        lubridate::day(stdate),
        "&ys=",
        lubridate::year(stdate),
        "&me=",
        lubridate::month(endate),
        "&de=",
        lubridate::day(endate),
        "&ye=",
        lubridate::year(endate),
        "&p=",
        paste(vars, collapse = "&p=", sep = ""),
        "&submit=Submit",
        sep = ""
      )

    message("Reading ", appendLF = FALSE)




    if (!file.exists(paste(dsn, fname, sep = "/"))) {
      message(dlurl, appendLF = TRUE)
      dlines <- unlist(strsplit(curl::curl_fetch_memory(url = dlurl), "\n"))
      if (!is.null(dsn))
        writeLines(dlines, paste(dsn, fname, sep = "/"))
      src <- dlurl
    } else if (rm.existing |
               file.info(paste(dsn, fname, sep = "/"))$size == 0) {
      message(dlurl, appendLF = TRUE)
      file.remove(paste(dsn, fname, sep = "/"))
      dlines <- unlist(strsplit(curl::curl(url = dlurl), "\n"))
      writeLines(dlines, paste(dsn, fname, sep = "/"))
      src <- dlurl
    } else {
      message(paste(dsn, fname, sep = "/"), appendLF = TRUE)
      dlines <- readLines(paste(dsn, fname, sep = "/"))
      src <- paste(dsn, fname, sep = "/")
    }

    if (class(dlines) == "try-error") {
      msg <- as.character(dlines)
    } else {
      # Check download integrity
      stline <-
        grep(paste(format(stdate, "%Y"), format(as.numeric(
          format(stdate, "%j")
        ), width = 3)), dlines)
      endline <-
        grep(paste(format(endate, "%Y"), format(as.numeric(
          format(endate, "%j")
        ), width = 3)), dlines)

      if (length(stline) != 1 | length(endline) != 1) {
        msg <-
          paste(
            "Incomplete or no data found on file. If file",
            fname,
            "is on disk, remove the file then rerun this program."
          )
      } else if (length(unlist(strsplit(dlines[endline], "[[:space:]]+"))) !=
                 (length(vars) + 2)) {
        msg <-
          paste(
            "Incomplete download detected. If file",
            fname,
            "is on disk, remove the file then rerun this program."
          )
      } else {
        msg <- paste("Read from", src)
        if (proceedwrite)
          writeLines(dlines, paste(dsn, fname, sep = "/"))
        alt <-
          as.numeric(unlist(strsplit(dlines[grep("Elevation", dlines)], "="))[2])
        dlines <- dlines[stline:endline]
        dvector <- unlist(strsplit(dlines, "[[:space:]]+"))
        dvector[dvector == "-"] <- NA
        nasadata <-
          as.data.frame(matrix(
            as.numeric(dvector),
            ncol = (length(vars) + 2),
            byrow = TRUE
          ))
        colnames(nasadata) <- c("yr", "doy", vars)

        date <-
          format(as.Date(paste(nasadata$yr, nasadata$doy), "%Y %j"), "%Y-%m-%d")
        nasadata <-
          cbind(date, nasadata[, -(1:2)], stringsAsFactors = FALSE)

        result["alt"] <- alt
        result["w"] <- nasadata
        rm(dlines, dvector, nasadata)
        gc(verbose = FALSE)
      }
    }
    message(msg)
    result["rmk"] <- msg
    return(result)
  }

#' @noRd
.validate_fileout <- function(dsn, filename) {
  if (!is.null(filename)) {
    stop("\nYou need to specify a filename.\n")
  }

  if (is.null(dsn)) {
    dsn <- getwd()
  } else {
    dsn <- trimws(dsn)
  }

  if (substr(dsn, nchar(dsn) - 1, nchar(dsn)) == "//") {
    p <- substr(dsn, 1, nchar(dsn) - 2)
  } else if (substr(dsn, nchar(dsn), nchar(dsn)) == "/" |
             substr(dsn, nchar(dsn), nchar(dsn)) == "\\") {
    p <- substr(dsn, 1, nchar(dsn) - 1)
  } else {
    p <- dsn
  }
  if (!file.exists(p) & !file.exists(dsn)) {
    stop("\nFile dsn does not exist: ", dsn, ".\n")
  }

  if (substr(dsn, nchar(dsn), nchar(dsn)) != "/" &
      substr(dsn, nchar(dsn), nchar(dsn)) != "\\") {
    dsn <- paste0(dsn, "/")
  }
  if (is.null(filename)) {
    filename_out <- "NASA-POWER"
  } else {
    filename_out <- filename
  }
  outfile <- paste0(dsn, filename_out)
  return(outfile)
}

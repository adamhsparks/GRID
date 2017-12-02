
#' Interpolate GSOD Data to a Gridded Surface
#'
#' This function is designed to be wrapped in an \code{\link[base]{lapply}}
#' function to process multiple years of GSOD data for interpolation, though a
#' single year may be used.
#'
#' @param gsod A \code{\link[base]{list}} of data frames or CSV files of GSOD
#' data created by \link{fetch_gsod}.
#' @param dem Path to a raster readable digital elevation model file that has
#' been fetched and processed using \code{\link{fetch_DEM}}.
#' @param dsn Directory where resulting GeoTIFF files are to be saved. Defaults
#' to user's "home" directory.
#' @param vars Weather variables to interpolate. Possible values are,
#' \code{TEMP}, \code{MAX}, \code{MIN}, \code{RH}. Defaults to \code{TEMP}.
#'
#' @return
#' GeoTIFF files of interpolated GSOD data at the spatial resolution of the
#' supplied digital elevation model.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a list of GSOD files
#' gsod_list <- list.files("~/Data/gsod", full.names = TRUE)
#'
#' # Load the raster digital elevation model
#' dem <- raster::raster("~/Data/SRTM/SRTM_1deg.grd")
#'
#' # Run the function for MAX and MIN temperature
#' lapply(X = gsod_list, FUN = interp, dem = dem, vars = c("MAX", "MIN"))
#' }
#'
#' @importFrom rlang .data

interpolate_gsod <- function(gsod = NULL,
                             dem = NULL,
                             dsn = NULL,
                             vars = NULL) {

  # validate user inputs, see `internal_functions.R` for these
  dsn <- .validate_dsn(dsn)
  vars <- .check_vars(vars)
  gsod <- .check_gsod(gsod)

  for (i in unique(gsod$YDAY)) {
    # subset the dataframe to just the date of interest
    x <- dplyr::filter(gsod, .data$YDAY == i)

    for (j in vars) {
      # create object with x, y and weather var
      y <- data.frame(x["LON"], x["LAT"], x["ELEV_M_SRTM_90m"], x[j])

      # remove any NA values from the data, the interpolation will not work with
      # any NAs present for any field.
      y <- stats::na.omit(y)

      # remove outliers
      bxs <- grDevices::boxplot.stats(y[, 4])
      y <- y[!y[, 4] %in% bxs$out, ]

      # create correction dataset
      y_vals <- y[, 4]
      names(y_vals) <- NULL

      # create thin plate spline object
      tps_y <- fields::Tps(y[, c("LON", "LAT", "ELEV_M_SRTM_90m")],
                           y_vals, lon.lat = TRUE)

      # interpolate thin plate spline object and write to disk
      tps_pred <- raster::interpolate(dem, tps_y, xyOnly = FALSE)

      raster::writeRaster(
        tps_pred,
        filename = paste0(dsn, "/", j, "_", x[1, 5], "_", i, ".tiff"),
        format = "GTiff",
        dataType = "INT2S",
        options = c("COMPRESS=LZW", "TFW=YES"),
        overwrite = TRUE
      )
      gc()
    }
  }
}

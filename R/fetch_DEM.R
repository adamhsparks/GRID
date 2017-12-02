
#' Fetch and Aggregate a Digital Elevation Model for Use in Interpolating GSOD Data
#'
#' @param resolution Resolution to aggregate the digital elevation model to in
#' arc-degrees, e.g. 1 = 1 arc degree, .25 = one quarter arc degree. Valid
#' options are 1, .5 and .25 degrees.
#' @param dsn Directory where resulting CSV files are to be saved. Defaults
#' to user's "home" directory.
#'
#' @return A digital elevation model cropped to -60/60 degrees latitude and
#' aggregated by the requested factor
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Fetch DEM and aggregate to 1 arc degree
#' fetch_DEM(dsn = "~/Data/DEM")
#' }
#'
fetch_DEM <- function(resolution = NULL, dsn = NULL) {

  dsn <- .validate_dsn(dsn)

  if (resolution %notin% c(NULL, .25, .5, 1)) {
    stop("The resolution you have specified is not valid.\n",
         "It should be one of: 1, .5 or .25.")
  }

  if (is.null(resolution)) {
    agg <- 6
    } else {
      dplyr::case_when(
        resolution == .25 | agg == 3,
        resolution == .5 | agg == 6,
        resolution == 1 | agg == 12
      )
  }

  # set up workspace
  tf.zip <- tempfile()

  utils::download.file(
    "http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/alt_5m_bil.zip",
    destfile = tf.zip,
    mode = "wb"
  )
  utils::unzip(tf.zip, exdir = tempdir()) # unzip downloaded file
  z <-
    raster::raster(paste0(tempdir(), "/alt.bil"))
  raster::dataType(z) <- "INT2S"

  # aggregate the SRTM data
  z <- raster::aggregate(z, fact = agg)

  z[z == -9999] <- NA # set -9999 to NA

  # crop SRTM data at -60/60 for agroclimatology only
  z <- raster::crop(z,
            c(
              xmin = -180,
              xmax = 180,
              ymin = -60,
              ymax = 60
            )
  )

  raster::writeRaster(
    z,
    filename = paste0(dsn, "/", "SRTM_DEM_", resolution, ".tiff"),
    format = "GTiff",
    dataType = "INT2S",
    options = c("COMPRESS=LZW", "TFW=YES"),
    overwrite = TRUE
  )
}

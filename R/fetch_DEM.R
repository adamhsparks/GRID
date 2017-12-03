
#' Fetch and Aggregate a Digital Elevation Model for Use in Interpolating GSOD Data
#'
#' @param resolution Resolution to aggregate the digital elevation model to in
#' arc-degrees, e.g. 1 = 1 arc degree, .25 = one quarter arc degree. Valid
#' options are 1, .5 and .25 degrees.
#' @param dsn Optional. Directory where resulting DEM file is to be saved. If
#' unspecified a spatial object is returned in the R session.
#'
#' @return A digital elevation model cropped to -60/60 degrees latitude and
#' aggregated by the requested factor
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Fetch DEM and aggregate to 1 arc degree and save to local disk
#' fetch_DEM(dsn = "~/Data/DEM")
#' }
#'
fetch_DEM <- function(resolution = NULL, dsn = NULL) {

  dsn <- .validate_dsn(dsn)
  agg <- .validate_resolution(resolution)

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

  # crop SRTM data at -60/60 for agroclimatology only
  z <- raster::crop(z,
                    c(
                      xmin = -180,
                      xmax = 180,
                      ymin = -60,
                      ymax = 60
                    )
  )

  # aggregate the SRTM data
  z <- raster::aggregate(z, fact = agg)

  z[z == -9999] <- NA # set -9999 to NA

  if (!is.null(dsn)) {
  raster::writeRaster(
    z,
    filename = paste0(dsn, "/", "SRTM_DEM_", resolution, ".tiff"),
    format = "GTiff",
    dataType = "INT2S",
    options = c("COMPRESS=LZW", "TFW=YES"),
    overwrite = TRUE
  )
  }
}

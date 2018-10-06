
#' Get and Aggregate a Digital Elevation Model for Use in Interpolating \acronym{GSOD} Data
#'
#' Fetches a digital elevation model (DEM) from WorldClim data, crops and
#' aggregates to a larger spatial resolution and crops at -60/60 degrees
#' latitude for use with other weather data that provide rainfall, \emph{e.g.}
#' \acronym{NASA} \acronym{POWER} at 0.5 arc degrees or \acronym{TRMM} at 0.25
#' arc degrees. Mainly for use in crop modelling exercises.
#'
#' @param resolution Resolution to aggregate the digital elevation model to in
#' arc-degrees, e.g. 1 = 1 arc degree, 0.5 = one half arc degree, 0.25 = one
#' quarter arc degree. Valid options are `1`, `0.5` and `0.25`.
#' @param dsn Optional. Directory where resulting \acronym{DEM} file is to be
#' saved. If unspecified a spatial object is returned in the R session. If a
#' \acronym{DEM} exists, it will be overwritten with the new one of the same
#' resolution. If a new resolution is specified, a new file will be created.
#'
#' @return A `[raster::raster()]` object of a digital elevation model cropped to
#' -60/60 degrees latitude and aggregated by the requested factor.
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#'
#' @examples
#'
#' \dontrun{
#' # Get DEM and aggregate to 0.5 arc degree, saving it to a local "~/Data/DEM"
#' directory.
#'
#' DEM <- make_DEM(dsn = "~/Data/DEM")
#' }
#' @export make_DEM

make_DEM <- function(resolution = NULL, dsn = NULL) {

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

  # set -9999 to NA
  z[z == -9999] <- NA

  # if dsn is specified, write a raster file to disk
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

  return(z)

  # clean up
  unlink(tempfile())
}

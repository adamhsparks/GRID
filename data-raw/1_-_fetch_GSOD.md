Fetch GSOD Data
================

Fetch and Import GSOD Weather Data
==================================

Using the `get_GSOD()` function from *GSODR*, download and reformat Global Surface Summary of the Day (GSOD) weather data from the US National Centers for Environmental Information (NCEI) for the years 1980 to 2016 between latitudes -60 and 60 only by using the `agroclimatology` argument in `get_GSOD()`. This process will take several hours depending on Internet connection and processor speed to download and clean the data. Data are then subset to include only the fields of interest, a station id value, longitude, latitude, elevation, date, temperature and relative humidity data. The resulting comma separated values (CSV) files vary in size from a few hundred megabytes to more than half a gigabyte. However, by saving them as .bz2 files, it saves several gigabytes of storage space. Note that `fname` below ends in a .bz2 file extension so `readr::write_csv` automatically compresses the files.

``` r
get_write <- function(year_list, dsn) {
  weather <- GSODR::get_GSOD(years = year_list,
                             max_missing = 5,
                             agroclimatology = TRUE)
  
  weather <- weather[, c(3, 8:9, 11, 14:19, 33, 35, 48)]

  fname <- paste0("GSOD_", weather[1, "YEAR"], ".bz2")

  readr::write_csv(weather, path = file.path(dsn, fname), na = "NA")

  # clean up and free up RAM/swap
  rm(weather)
  gc()
}

# generate a vector of "years" from 1983 to 2016
year_list <- as.list(seq(from = 1983, to = 2016, by = 1))

# lapply to apply the get_write() function to the list of years
# download (get) and create (write) CSV files of the GSOD weather data
lapply(X = year_list, FUN = get_write, dsn = "~/Data/GSOD")
```

------------------------------------------------------------------------

Appendices
==========

R Session Information
---------------------

    ## Session info -------------------------------------------------------------

    ##  setting  value                       
    ##  version  R version 3.4.2 (2017-09-28)
    ##  system   x86_64, linux-gnu           
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_AU.UTF-8                 
    ##  tz       Zulu                        
    ##  date     2017-11-27

    ## Packages -----------------------------------------------------------------

    ##  package     * version    date       source                         
    ##  assertthat    0.2.0      2017-04-11 CRAN (R 3.4.2)                 
    ##  backports     1.1.1      2017-09-25 CRAN (R 3.4.2)                 
    ##  base        * 3.4.2      2017-10-28 local                          
    ##  bindr         0.1        2016-11-13 CRAN (R 3.4.2)                 
    ##  bindrcpp      0.2        2017-06-17 CRAN (R 3.4.2)                 
    ##  compiler      3.4.2      2017-10-28 local                          
    ##  datasets    * 3.4.2      2017-10-28 local                          
    ##  devtools      1.13.4     2017-11-09 CRAN (R 3.4.2)                 
    ##  digest        0.6.12     2017-01-27 CRAN (R 3.4.2)                 
    ##  dplyr         0.7.4      2017-09-28 CRAN (R 3.4.2)                 
    ##  evaluate      0.10.1     2017-06-24 CRAN (R 3.4.2)                 
    ##  glue          1.2.0      2017-10-29 CRAN (R 3.4.2)                 
    ##  graphics    * 3.4.2      2017-10-28 local                          
    ##  grDevices   * 3.4.2      2017-10-28 local                          
    ##  GSODR         1.1.0.9000 2017-11-27 Github (ropensci/GSODR@70d1ebb)
    ##  hms           0.3        2016-11-22 CRAN (R 3.4.2)                 
    ##  htmltools     0.3.6      2017-04-28 CRAN (R 3.4.2)                 
    ##  knitr         1.17       2017-08-10 CRAN (R 3.4.2)                 
    ##  magrittr      1.5        2014-11-22 CRAN (R 3.4.2)                 
    ##  memoise       1.1.0      2017-04-21 CRAN (R 3.4.2)                 
    ##  methods     * 3.4.2      2017-10-28 local                          
    ##  pkgconfig     2.0.1      2017-03-21 CRAN (R 3.4.2)                 
    ##  purrr         0.2.4      2017-10-18 CRAN (R 3.4.2)                 
    ##  R.methodsS3   1.7.1      2016-02-16 CRAN (R 3.4.2)                 
    ##  R.oo          1.21.0     2016-11-01 CRAN (R 3.4.2)                 
    ##  R.utils       2.6.0      2017-11-05 CRAN (R 3.4.2)                 
    ##  R6            2.2.2      2017-06-17 CRAN (R 3.4.2)                 
    ##  Rcpp          0.12.13    2017-09-28 CRAN (R 3.4.2)                 
    ##  readr         1.1.1      2017-05-16 CRAN (R 3.4.2)                 
    ##  rlang         0.1.4      2017-11-05 CRAN (R 3.4.2)                 
    ##  rmarkdown     1.8        2017-11-17 CRAN (R 3.4.2)                 
    ##  rprojroot     1.2        2017-01-16 CRAN (R 3.4.2)                 
    ##  stats       * 3.4.2      2017-10-28 local                          
    ##  stringi       1.1.6      2017-11-17 cran (@1.1.6)                  
    ##  stringr       1.2.0      2017-02-18 CRAN (R 3.4.2)                 
    ##  tibble        1.3.4      2017-08-22 CRAN (R 3.4.2)                 
    ##  tools         3.4.2      2017-10-28 local                          
    ##  utils       * 3.4.2      2017-10-28 local                          
    ##  withr         2.1.0      2017-11-01 CRAN (R 3.4.2)                 
    ##  yaml          2.1.14     2016-11-12 CRAN (R 3.4.2)

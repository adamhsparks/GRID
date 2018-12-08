
library(glint)

# set up session
future::plan("multisession")
years <- as.list(seq(from = 1983, to = 2015, by = 1))
ifelse(!dir.exists(file.path("/mnt", "GSOD")), dir.create(file.path("/mnt", "GSOD")), FALSE)

# fetch GSOD data and store in "/mnt/GSOD" as `fst` files
lapply(X = years, FUN = make_GSOD_set, dsn = "/mnt/GSOD")

# create DEM files
DEM <- make_DEM(resolution = 0.5, dsn = "/mnt/DEM")

# interpolate the temperature surfaces and save in Monetdb
GRID <- lapply(X = file_list, FUN = interpolate_GSOD, dem = DEM,
               dsn = "/mnt/Cache/GTiff", vars = c("MAX", "MIN"))


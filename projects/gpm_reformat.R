#' Reformat gpm_imergm files
#' 
#' By Mijael Rodrigo Vargas Godoy
#' Aug 15, 2022
#' 
#' The script reformats GPM IMERGM v06 from .HDF5 into .nc files.
#' NOTE: The working directory must be the directory containing the .HDF5 files

library(parallel)
library(data.table)
library(raster)
library(dplyr)
library(terra)
library(ncdf4)
file_names <- list.files(".", full.names = TRUE)
no_cores <- detectCores()
if (no_cores >= 24){
  no_cores <- 23
} else if(no_cores <= 1 | is.na(no_cores)){
  no_cores <- 1
} else {
  no_cores <- no_cores -1
}
cluster <- makeCluster(no_cores, type = "PSOCK")
clusterEvalQ(cluster, library("hdf5r"))
clusterEvalQ(cluster, library("raster"))
clusterEvalQ(cluster, library("ncdf4"))
parLapply(cluster, file_names, function(dummie_layer){
  layer_name <- sub(".*3IMERG.", "", dummie_layer)
  layer_name <- substr(layer_name, 1, 8)
  dummie_file <- H5File$new(dummie_layer, mode="r+")    
  dummie_table <- dummie_file[["Grid/precipitation"]]
  dummie_table <- dummie_table[1:1800, 1:3600, 1]
  dummie_table[dummie_table < 0] <- NA
  dummie_file$close_all()
  dummie_table <- dummie_table * lubridate::days_in_month(as.Date(layer_name, format = "%Y%m%d")) * 24
  dummie_table <- raster(dummie_table, xmn = -180, xmx = 180, ymn = -90, ymx = 90, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84")
  dummie_table <- flip(dummie_table, direction = "y")
  ##
  ncfname <- paste0("./gpm-imergm_", layer_name, ".nc")
  lon <- as.array(seq(-179.95, 179.95, 0.1))
  nlon <- 3600
  lat <- as.array(seq(89.95, -89.95, -0.1))
  nlat <- 1800
  time <- difftime(as.Date(layer_name, format = "%Y%m%d"), "1900-01-01", units = "hours")
  time <- as.array(as.double(time))
  nt <- 1
  tunits <- "hours since 1900-01-01 00:00:00.0"
  londim <- ncdim_def("lon", "degrees_east", as.double(lon))
  latdim <- ncdim_def("lat", "degrees_north", as.double(lat))
  timedim <- ncdim_def("time", tunits, as.double(time))
  fillvalue <- -9999
  dlname <- "Total precipitation"
  pre_def <- ncvar_def("tp", "mm", list(londim, latdim, timedim), fillvalue, dlname, prec = "single")
  ncout <- nc_create(ncfname, pre_def, force_v4 = TRUE) 
  ncvar_put(ncout, pre_def, array(getValues(dummie_table), dim = c(nlon, nlat, nt)))
  ncatt_put(ncout, "lon", "axis", "X")
  ncatt_put(ncout, "lat", "axis", "Y")
  ncatt_put(ncout, "time", "axis", "T")
  nc_close(ncout)
})
stopCluster(cluster)


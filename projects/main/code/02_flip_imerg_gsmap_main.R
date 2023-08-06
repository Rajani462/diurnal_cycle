
library(raster)

### GSMaP-----------------------------------------------
# Read the original file
original_file <- brick("~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_2015_20_grid_025.nc")

# Transpose and flip the raster
transposed_flipped <- flip(t(original_file), direction = "x")
file_datetime <- as.POSIXct(getZ(original_file), origin = "1970-01-01", format = "%Y-%m-%d %H:%M:%S")
transposed_flipped <- setZ(transposed_flipped, file_datetime, 'date')

# Save the transposed and flipped raster to a new file
raster::writeRaster(transposed_flipped, filename = "./projects/kenya_example/data/gsmap_hour_kenya_2015_20_fliptrans.nc", format = "CDF", overwrite=TRUE)

# Verify the result by reading the new file
result <- brick("gsmap_tf_2015_20.nc")
getZ(result)

persiann_precip_dt <- as.data.frame(result, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann"))

#print(result)

plot(result)


### IMERG-----------------------------------------------

library(raster)

# Read the original file
original_file <- brick("./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025.nc")

# Transpose and flip the raster
transposed_flipped <- flip(t(original_file), direction = "x")
file_datetime <- as.POSIXct(getZ(original_file), origin = "1970-01-01", format = "%Y-%m-%d %H:%M:%S")
transposed_flipped <- setZ(transposed_flipped, file_datetime, 'date')

# Save the transposed and flipped raster to a new file
raster::writeRaster(transposed_flipped, filename = "./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025_fliptrans.nc", format = "CDF", overwrite=TRUE)

# Verify the result by reading the new file
result <- brick("gsmap_tf_2015_20.nc")
getZ(result)

persiann_precip_dt <- as.data.frame(result, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann"))

#print(result)

plot(result)




############################################################################




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


# hourly_frequency --------------------------------------------------------

library(raster)

### GSMaP-----------------------------------------------
# Read the original file
original_file <- brick("~/shared/data_downloads/input_data/seasonal/hourly_character/hour_freq_gsmap_hour_60ns_2015_20_grid_025_djf_.nc")

plot(original_file[[1]])

#no need as it was alsredy in required format

############################################################################

# hourly_intensity --------------------------------------------------------

library(raster)

### GSMaP-----------------------------------------------
# Read the original file
original_file <- brick("~/shared/data_downloads/input_data/seasonal/hourly_character/hourly_int_gsmap_tp_mm_60ns_2015_20_025_hourly_jja.nc")
#original_file <- brick("~/shared/data_downloads/input_data/seasonal/hourly_character/hourly_int_gsmap_tp_mm_60ns_2015_20_025_hourly_djf.nc")

plot(original_file[[1]])

# Transpose and flip the raster
transposed_flipped <- flip(t(original_file), direction = "x")
file_datetime <- as.POSIXct(getZ(original_file), origin = "1970-01-01", format = "%Y-%m-%d %H:%M:%S")
transposed_flipped <- setZ(transposed_flipped, file_datetime, 'date')

plot(transposed_flipped[[1]])
# Save the transposed and flipped raster to a new file
raster::writeRaster(transposed_flipped, filename = "~/shared/data_downloads/input_data/seasonal/hourly_character/hourly_int_gsmap_tp_mm_60ns_2015_20_025_hourly_jja_fliptrans.nc", format = "CDF", overwrite=TRUE)
#raster::writeRaster(transposed_flipped, filename = "~/shared/data_downloads/input_data/seasonal/hourly_character/hourly_int_gsmap_tp_mm_60ns_2015_20_025_hourly_djf_fliptrans.nc", format = "CDF", overwrite=TRUE)


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
original_file <- brick("~/shared/data_downloads/input_data/seasonal/hourly_character/hourly_int_imerg_tp_mm_60ns_2001_20_025_hourly_jja.nc")
#original_file <- brick("~/shared/data_downloads/input_data/seasonal/hourly_character/hourly_int_imerg_tp_mm_60ns_2001_20_025_hourly_djf.nc")
plot(original_file[[1]])

# Transpose and flip the raster
transposed_flipped <- flip(t(original_file), direction = "x")
file_datetime <- as.POSIXct(getZ(original_file), origin = "1970-01-01", format = "%Y-%m-%d %H:%M:%S")
transposed_flipped <- setZ(transposed_flipped, file_datetime, 'date')

plot(transposed_flipped[[1]])
# Save the transposed and flipped raster to a new file
raster::writeRaster(transposed_flipped, filename = "~/shared/data_downloads/input_data/seasonal/hourly_character/hourly_int_imerg_tp_mm_60ns_2001_20_025_hourly_jja_fliptrans.nc", format = "CDF", overwrite=TRUE)
#raster::writeRaster(transposed_flipped, filename = "~/shared/data_downloads/input_data/seasonal/hourly_character/hourly_int_imerg_tp_mm_60ns_2001_20_025_hourly_djf_fliptrans.nc", format = "CDF", overwrite=TRUE)

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


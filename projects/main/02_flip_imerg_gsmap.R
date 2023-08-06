library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)


#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

### GSMaP-----------------------------------------------
# Read the original file
original_file <- brick("./projects/main/data/hourly_mean_gsmap_2001_15.nc")

plot(original_file[[1]])

# Transpose and flip the raster
transposed_flipped <- flip(t(original_file), direction = "x")
file_datetime <- as.POSIXct(getZ(original_file), origin = "1970-01-01", format = "%Y-%m-%d %H:%M:%S")
transposed_flipped <- setZ(transposed_flipped, file_datetime, 'date')

plot(transposed_flipped[[1]])

# Save the transposed and flipped raster to a new file
raster::writeRaster(transposed_flipped, filename = "./projects/main/data/hourly_mean_gsmap_glob_2015_20_fliptrans.nc", format = "CDF", overwrite=TRUE)

# Verify the result by reading the new file
result <- brick("./projects/main/data/hourly_mean_gsmap_glob_2015_20_fliptrans.nc")
getZ(result)

persiann_precip_dt <- as.data.frame(result, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann"))


summary(persiann_precip_dt)
#print(result)

plot(result)


### IMERG-----------------------------------------------

library(raster)

# Read the original file
original_file <- brick("./projects/main/data/hourly_mean_imergf_glob_2001_20.nc")

plot(original_file[[1]])

# Transpose and flip the raster
transposed_flipped <- flip(t(original_file), direction = "x")
plot(transposed_flipped[[1]])
file_datetime <- as.POSIXct(getZ(original_file), origin = "1970-01-01", format = "%Y-%m-%d %H:%M:%S")
transposed_flipped <- setZ(transposed_flipped, file_datetime, 'date')

# Save the transposed and flipped raster to a new file
raster::writeRaster(transposed_flipped, filename = "./projects/main/data/hourly_mean_imergf_glob_2001_20_fliptrans.nc", format = "CDF", overwrite=TRUE)

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

# frequency ---------------------------------------------------------------

original_file <- brick("./projects/main/data/gsmap_freq_0.1_2001_15.nc")

plot(original_file[[1]])

# Transpose and flip the raster
transposed_flipped <- flip(t(original_file), direction = "x")
file_datetime <- as.POSIXct(getZ(original_file), origin = "1970-01-01", format = "%Y-%m-%d %H:%M:%S")
transposed_flipped <- setZ(transposed_flipped, file_datetime, 'date')

plot(transposed_flipped[[1]])

# Save the transposed and flipped raster to a new file
raster::writeRaster(transposed_flipped, filename = "./projects/main/data/hourly_mean_gsmap_glob_2015_20_fliptrans.nc", format = "CDF", overwrite=TRUE)

# Verify the result by reading the new file
result <- brick("./projects/main/data/hourly_mean_gsmap_glob_2015_20_fliptrans.nc")
getZ(result)

persiann_precip_dt <- as.data.frame(result, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann"))


summary(persiann_precip_dt)
#print(result)

plot(result)


### IMERG-----------------------------------------------

library(raster)

# Read the original file
original_file <- brick("./projects/main/data/imerg_freq_0.1_2001_20.nc")

plot(original_file[[1]])

# Transpose and flip the raster
transposed_flipped <- flip(t(original_file), direction = "x")
plot(transposed_flipped[[1]])
file_datetime <- as.POSIXct(getZ(original_file), origin = "1970-01-01", format = "%Y-%m-%d %H:%M:%S")
transposed_flipped <- setZ(transposed_flipped, file_datetime, 'date')

# Save the transposed and flipped raster to a new file
raster::writeRaster(transposed_flipped, filename = "./projects/main/data/hourly_mean_imergf_glob_2001_20_fliptrans.nc", format = "CDF", overwrite=TRUE)

# Verify the result by reading the new file
result <- brick("gsmap_tf_2015_20.nc")
getZ(result)

persiann_precip_dt <- as.data.frame(result, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann"))

#print(result)


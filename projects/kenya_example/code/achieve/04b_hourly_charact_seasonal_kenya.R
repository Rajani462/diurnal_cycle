
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(reshape)
#library(terra)
library(ncdf4)
library(sf)
library(rgdal)
library(parallel)
library(doParallel)
library(foreach)

##read the datasets
cmorph <- brick("./projects/kenya_example/data/cmorph_hour_kenya_2001_20.nc")
imerg <- brick("./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025.nc")
persiann <- brick("./projects/kenya_example/data/persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc")
imerg <- brick("./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025.nc")
gsmap <- brick("./projects/kenya_example/data/gsmap_hour_kenya_2015_20.nc")


# cmorph ------------------------------------------------------------------

library(raster)

library(raster)

# Create a vector of season labels
season_labels <- c("JF", "MAM", "JJAS", "OND")

# Create an empty list to store the intensity results
intensity_list <- vector("list", length(season_labels))

# Loop over each season
for (i in 1:length(season_labels)) {
  season <- season_labels[i]
  
  # Subset the data for the current season
  season_data <- cmorph[[grep(season, names(cmorph))]]
  
  # Calculate the total precipitation for each hour of the day
  total_precip <- calc(season_data, sum)
  
  # Calculate the number of precipitation hours for each hour of the day
  precip_hours <- calc(season_data, function(x) sum(x > 0.1))
  
  # Calculate the average precipitation intensity for each hour of the day
  intensity <- total_precip / precip_hours
  
  # Store the intensity results in the list
  intensity_list[[i]] <- intensity
}

# Plot the intensity for each hour of each season

library(raster)
library(doParallel)
library(foreach)

# Set the number of cores to use
num_cores <- 4

# Create a vector of season labels
season_labels <- c("JF", "MAM", "JJAS", "OND")

# Create an empty list to store the intensity results
intensity_list <- vector("list", length(season_labels))

# Initialize parallel processing with the specified number of cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Loop over each season in parallel
intensity_list <- foreach(i = 1:length(season_labels), .packages = "raster") %dopar% {
  season <- season_labels[i]
  
  # Subset the data for the current season
  season_data <- cmorph[[grep(season, names(cmorph))]]
  
  # Calculate the total precipitation for each hour of the day
  total_precip <- calc(season_data, sum)
  
  # Calculate the number of precipitation hours for each hour of the day
  precip_hours <- calc(season_data, function(x) sum(x > 0.1))
  
  # Calculate the average precipitation intensity for each hour of the day
  intensity <- total_precip / precip_hours
  
  # Return the intensity result for the current season
  intensity
}

# Stop parallel processing
stopCluster(cl)

# Plot the intensity for each hour of each season
for (i in 1:length(season_labels)) {
  season <- season_labels[i]
  intensity <- intensity_list[[i]]
  
  for (hour in 1:24) {
    plot(intensity[[hour]])
    # Add code for saving or displaying the plots as desired
  }
}



### Intensity

# Convert the time names to POSIXct format and extract the seasonal indices
indices <- format(as.POSIXct(names(cmorph), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")
indices <- as.numeric(indices)

# Create a vector of season labels corresponding to the indices
season_labels <- c("JF", "MAM", "JJAS", "OND")
season <- season_labels[(indices %in% c(1, 2))] # Modify the indices accordingly for the seasons

# Calculate the total precipitation for each season and hour of the day
total_precip <- stackApply(cmorph, c(season_labels, "hour"), fun = sum)


cmorph_totprecip_dt <- as.data.frame(total_precip, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_tot") %>% 
  `[`(, name := factor("cmorph"))

# Calculate the number of precipitation hours for each season and hour of the day
precip_hours <- stackApply(cmorph, c(season, "hour"), fun = function(x, na.rm = TRUE) sum(x > 0.1))

# Calculate the average precipitation intensity for each season and hour of the day
cmorph_intensity <- total_precip / precip_hours

# Plot the intensity for the first hour of the first season
plot(cmorph_intensity[["JF", 1]])




# Convert the time names to POSIXct format and extract the seasonal indices
indices <- format(as.POSIXct(names(cmorph), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")
indices <- as.numeric(indices)

# Create a vector of season labels corresponding to the indices
season_labels <- c("JF", "MAM", "JJAS", "OND")
season <- season_labels[(indices %in% c(1, 2))] # Modify the indices accordingly for the seasons

# Calculate the total precipitation for each season
total_precip <- stackApply(cmorph, season, fun = sum)

# Calculate the number of precipitation hours for each season
precip_hours <- stackApply(cmorph, season, fun = function(x, na.rm = TRUE) sum(x > 0.1))

# Calculate the average precipitation intensity for each season
cmorph_intensity <- total_precip / precip_hours

# Plot the first season's intensity
plot(cmorph_intensity[[1]])



indices <- format(as.POSIXct(names(cmorph), format = "X%Y.%m.%d.%H.%M.%S"), format = "%Y-%m")
indices <- as.yearmon(indices)


indices <- format(as.POSIXct(names(cmorph), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

total_precip <- stackApply(cmorph, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours <- stackApply(cmorph, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

cmorph_intensity <- total_precip / precip_hours

plot(intensity[[1]])

cmorph_totprecip_dt <- as.data.frame(total_precip, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_tot") %>% 
  `[`(, name := factor("cmorph"))


# cdo_totprcp <- brick("./projects/kenya_example/data/hourly_charact/trial_hourly_sum_cmorph_kenya_2001_20.nc")
# cdo_hour_prcp <- brick("./projects/kenya_example/data/hourly_charact/tril_persian_precip_hours_hourly.nc")
# 
# cdo_totprecip_dt <- as.data.frame(cdo_totprcp, xy = TRUE) %>%
#   as.data.table() %>%
#   data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_tot") %>% 
#   `[`(, name := factor("cmorph"))
# 
# cmorph_prehours_dt <- as.data.frame(precip_hours, xy = TRUE) %>%
#   as.data.table() %>%
#   data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_hours") %>% 
#   `[`(, name := factor("cmorph"))
# 
# cdo_prehours_dt <- as.data.frame(cdo_hour_prcp, xy = TRUE) %>%
#   as.data.table() %>%
#   data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_hours") %>% 
#   `[`(, name := factor("cmorph"))
# 
# summary(cdo_prehours_dt)
# summary(persiann_hour_dt)
# 
cmorph_int_dt <- as.data.frame(cmorph_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>%
  `[`(, name := factor("cmorph"))


saveRDS(cmorph_int_dt, "./projects/kenya_example/data/hourly_charact/int_cmorph_hourly.RDS")

# cmorph_sum_dt <- as.data.frame(cmorph_mean, xy = TRUE) %>%
#   as.data.table() %>%
#   melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_sum") %>%
#   `[`(, name := factor("cmorph"))
# 
# mean_int_cmorph = mean(intensity, na.rm=FALSE)
# plot(mean_int_cmorph)

### Frequency
# Calculate the frequency (total precipitation hours divided by total available hours)

# Create a function to calculate precipitation hours

# precip_hours_fun <- function(x) {
#   sum(x > 0.1, na.rm = TRUE)
# }


# Calculate the total available hours
total_available_hours <- length(indices)

cmorph_frequency <- (precip_hours / total_available_hours) * 100

cmorph_freq_dt <- as.data.frame(cmorph_frequency, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>%
  `[`(, name := factor("cmorph"))

saveRDS(cmorph_freq_dt, "./projects/kenya_example/data/hourly_charact/freq_cmorph_hourly.RDS")


# plot(frequency[[1]])
# 
# mean_freq_cmorph = mean(frequency, na.rm=FALSE)
# plot(mean_freq_cmorph)



# persiann ----------------------------------------------------------------

### Intensity
pers_time <- getZ(persiann)
posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00")
names(persiann) <- posixct_time

indices <- format(as.POSIXct(names(persiann), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

total_precip <- stackApply(persiann, indices, fun = sum)

persiann_precip_dt <- as.data.frame(total_precip, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann"))

# Calculate the precipitation hours using stackApply()
#precip_hours <- stackApply(persiann, indices, fun = function(x, na.rm =TRUE) sum(x > 0))
precip_hours <- stackApply(persiann, indices, fun = function(x, na.rm = TRUE) {
  sum(x > 0.1, na.rm = TRUE)
})

persiann_hour_dt <- as.data.frame(precip_hours, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))

summary(persiann_hour_dt)

persiann_intensity <- total_precip / precip_hours

plot(persiann_intensity[[1]])

persiann_int_dt <- as.data.frame(persiann_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))

summary(persiann_int_dt)

saveRDS(persiann_int_dt, "./projects/kenya_example/data/hourly_charact/int_persiann_hourly.RDS")

### Frequency

# Calculate the total available hours
total_available_hours <- length(indices)

persiann_frequency <- (precip_hours / total_available_hours) * 100

persiann_freq_dt <- as.data.frame(persiann_frequency, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  `[`(, name := factor("persiann"))

saveRDS(persiann_freq_dt, "./projects/kenya_example/data/hourly_charact/freq_persiann_hourly.RDS")


# imerg -------------------------------------------------------------------

### Intensity

indices <- format(as.POSIXct(names(imerg), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

total_precip <- stackApply(imerg, indices, fun = sum)

# imerg_precip_dt <- as.data.frame(total_precip, xy = TRUE) %>%
#   as.data.table() %>%
#   melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>%
#   `[`(, name := factor("imerg"))

# Calculate the precipitation hours using stackApply()
precip_hours <- stackApply(imerg, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

# imerg_hour_dt <- as.data.frame(precip_hours, xy = TRUE) %>%
#   as.data.table() %>%
#   melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>%
#   `[`(, name := factor("imerg"))


imerg_intensity <- total_precip / precip_hours

plot(imerg_intensity[[1]])
imerg_intensity_t <- t(imerg_intensity)
imerg_intensity_flipx = flip(imerg_intensity_t, direction = 1) 
plot(imerg_intensity_flipx[[1]])

imerg_int_dt <- as.data.frame(imerg_intensity_flipx, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>% 
  `[`(, name := factor("imerg"))

summary(imerg_int_dt)

saveRDS(imerg_int_dt, "./projects/kenya_example/data/hourly_charact/int_imerg_hourly.RDS")

### Frequency

# Calculate the total available hours
total_available_hours <- length(indices)

imerg_frequency <- (precip_hours / total_available_hours) * 100

imerg_frequency_t <- t(imerg_frequency)
imerg_frequency_flipx = flip(imerg_frequency_t, direction = 1) 
plot(imerg_frequency_flipx[[1]])

imerg_freq_dt <- as.data.frame(imerg_frequency_flipx, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg"))

saveRDS(imerg_freq_dt, "./projects/kenya_example/data/hourly_charact/freq_imerg_hourly.RDS")


# imerg --------------------------------------------------------------------

### Intensity

indices <- format(as.POSIXct(names(imerg), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

total_precip <- stackApply(imerg, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours <- stackApply(imerg, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))
imerg_intensity <- total_precip / precip_hours

plot(imerg_intensity[[1]])

imerg_int_dt <- as.data.frame(imerg_intensity, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>% 
  `[`(, name := factor("imerg"))

imerg_int_dt <- imerg_int_dt[, .(x = y, y = x, date = variable, prec_int = value, name)]
#setnames(imerg_int_dt, c("variable", "value"), c("date", "prec_int"))

summary(imerg_int_dt)

saveRDS(imerg_int_dt, "./projects/kenya_example/data/hourly_charact/int_imerg_hourly.RDS")

### Frequency

# Calculate the total available hours
total_available_hours <- length(indices)
imerg_frequency <- (precip_hours / total_available_hours) * 100

imerg_freq_dt <- as.data.frame(imerg_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg"))

#setnames(imerg_freq_dt, c("variable", "value"), c("date", "prec_freq"))
imerg_freq_dt <- imerg_freq_dt[, .(x = y, y = x, date = variable, prec_freq = value, name)]

saveRDS(imerg_freq_dt, "./projects/kenya_example/data/hourly_charact/freq_imerg_hourly.RDS")

# gsmap --------------------------------------------------------------------

### Intensity

indices <- format(as.POSIXct(names(gsmap), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

total_precip <- stackApply(gsmap, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours <- stackApply(gsmap, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))
gsmap_intensity <- total_precip / precip_hours

plot(gsmap_intensity[[1]])
gsmap_intensity_t <- t(gsmap_intensity)
gsmap_intensity_flipx <- flip(gsmap_intensity_t, direction = 1)


gsmap_int_dt <- as.data.frame(gsmap_intensity_flipx, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>% 
  `[`(, name := factor("gsmap"))

#gsmap_int_dt <- gsmap_int_dt[, .(x = y, y = x, date = variable, prec_int = value, name)]
#setnames(gsmap_int_dt, c("variable", "value"), c("date", "prec_int"))

summary(gsmap_int_dt)

gsmap_int_dt[prec_int == "Inf"] # R gives Inf when something divided by 0 (It is possible thatprecipitation less than 0.1 mm/hr are accumulated to total precipitation amount, but the number of precipitation hours are yet zero according to the condition of precip > 0.1 mm /hr)

# gsmap_int_dt[!is.finite(prec_int), prec_int := NA]
# summary(gsmap_int_dt)

saveRDS(gsmap_int_dt, "./projects/kenya_example/data/hourly_charact/int_gsmap_hourly.RDS")

### Frequency

# Calculate the total available hours
total_available_hours <- length(indices)
gsmap_frequency <- (precip_hours / total_available_hours) * 100

gsmap_frequency_t <- t(gsmap_frequency)
gsmap_frequency_flipx <- flip(gsmap_frequency_t, direction = 1)

gsmap_freq_dt <- as.data.frame(gsmap_frequency_flipx, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("gsmap"))

#setnames(gsmap_freq_dt, c("variable", "value"), c("date", "prec_freq"))
#gsmap_freq_dt <- gsmap_freq_dt[, .(x = y, y = x, date = variable, prec_freq = value, name)]

saveRDS(gsmap_freq_dt, "./projects/kenya_example/data/hourly_charact/freq_gsmap_hourly.RDS")

#######################################################

# UTC to Local Solar Time  ------------------------------------------------

11+(100/15)

#########example of amplitude

# Example time series of precipitation data
# Example time series of precipitation data
precipitation <- c(0.5, 1.2, 1.8, 1.4, 0.9, 0.3, 0.1, 0.6)  # Replace with your own data

# Calculate daily mean precipitation
daily_mean <- tapply(precipitation, as.Date(seq_along(precipitation), origin = "2023-05-01"), mean)

# Split daily mean into separate days
daily_values <- split(daily_mean, as.Date(names(daily_mean)))

# Calculate the amplitude of the diurnal cycle
amplitude <- sapply(daily_values, function(x) max(x) - min(x))

# Print the amplitude values
print(amplitude)

####

# parellised version ------------------------------------------------------
library(doParallel)
library(foreach)

# Assuming you have three datasets: cmorph_2001_20, dataset_2, and dataset_3

# List of dataset file paths
dataset_paths <- c(
  "./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025.nc", 
  "./projects/kenya_example/data/cmorph_hour_kenya_2001_20.nc",
  "./projects/kenya_example/data/imerg_hour_kenya_2001_20_regrid.nc",
  "./projects/kenya_example/data/persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc"
)

# Load the datasets into a list
dataset <- lapply(dataset_paths, brick)

num_cores <- detectCores() - 20  # Change this to the desired number of cores (minimum 40)

# Register the parallel backend
#cl <- makeCluster(num_cores)
registerDoParallel(num_cores)

# Loop over each dataset in parallel
results <- foreach(dataset = datasets, .combine = list) %dopar% {
  # Perform analysis for each dataset
  indices <- format(as.POSIXct(names(dataset), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
  indices <- as.numeric(indices)
  
  hourly_mean <- foreach(i = indices, .combine = stack) %dopar% {
    subset <- dataset[[i]]
    mean(subset)
  }
  
  stack(hourly_mean)
}

# Stop the parallel backend
stopCluster(cl)


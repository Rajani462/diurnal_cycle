
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
era5 <- brick("./projects/kenya_example/data/era5_hour_kenya_2001_20_regrid.nc")
gsmap <- brick("./projects/kenya_example/data/gsmap_hour_kenya_2015_grid_025.nc")


# Define a custom function to assign NA to values less than 0 and greater than 1000
assign_na <- function(x) {
  x[x < 0 | x > 1000] <- NA
  return(x)
}

# Use the calc() function to apply the custom function to each cell of the brick
persiann_corrcted <- calc(persiann, assign_na)

plot(persiann_corrcted[[5:8]])

persiann <- clamp(persiann, lower=0, upper=700, useValues=FALSE)
persiann[persiann < 0] <- NA

# mean
cmorph_mean = mean(cmorph, na.rm=TRUE)
imerg_mean = mean(imerg, na.rm=TRUE)
persiann_mean = mean(cmorph, na.rm=TRUE)
imerg_mean = mean(cmorph, na.rm=TRUE)

persiann <- brick("~/rajani/diurnal_cycle/projects/kenya_example/data/trial_persian_2001_01_01.nc")
trl_mean <- mean(dat, na.rm = TRUE)

trl_dt <- as.data.frame(trl_mean, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("cmorph"))

persiann_dt <- as.data.frame(persiann, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("persiann"))
summary(persiann_dt)


persiann_dt[, prec_mean > 0.1, by = .(date, x, y)]

plot(mean_cmorph_2001_20)

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(cmorph), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

hourly_mean<- stackApply(cmorph, indices, fun = mean)
cmorph_dt <- as.data.frame(hourly_mean, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("cmorph"))

cdo_cmorph_dt <- as.data.frame(mean_ken, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("cmorph"))


### Intensity

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


cdo_totprcp <- brick("./projects/kenya_example/data/hourly_charact/trial_hourly_sum_cmorph_kenya_2001_20.nc")
cdo_hour_prcp <- brick("./projects/kenya_example/data/hourly_charact/tril_persian_precip_hours_hourly.nc")

cdo_totprecip_dt <- as.data.frame(cdo_totprcp, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_tot") %>% 
  `[`(, name := factor("cmorph"))

cmorph_prehours_dt <- as.data.frame(precip_hours, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_hours") %>% 
  `[`(, name := factor("cmorph"))

cdo_prehours_dt <- as.data.frame(cdo_hour_prcp, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_hours") %>% 
  `[`(, name := factor("cmorph"))

summary(cdo_prehours_dt)
summary(persiann_hour_dt)

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

cmorph_freq_dt <- as.data.frame(frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>%
  `[`(, name := factor("cmorph"))

saveRDS(cmorph_freq_dt, "./projects/kenya_example/data/hourly_charact/freq_cmorph_hourly.RDS")


plot(frequency[[1]])

mean_freq_cmorph = mean(frequency, na.rm=FALSE)
plot(mean_freq_cmorph)



# persiann ----------------------------------------------------------------

### Intensity
pers_time <- getZ(persiann)
posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00") #We use (1*3600) in the formula because time values are stored as seconds in R. Since there are 3,600 seconds in one hour
names(persiann) <- posixct_time

persiann[[172805]]

# persiann2 <- brick("./projects/kenya_example/data/persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc")
# pers_time2 <- getZ(persiann2)
# posixct_time <- as.POSIXct(pers_time2, origin = "2001-01-01", tz = "UTC")
# names(persiann2) <- posixct_time
# 
# pers_time <- getZ(dataset)
# posixct_time <- as.POSIXct(pers_time, origin = "2001-01-01", tz = "UTC")
# names(dataset) <- posixct_time
# persiann2[[172805]]

indices <- format(as.POSIXct(names(persiann), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

total_precip <- stackApply(persiann, indices, fun = sum)

persiann_precip_dt <- as.data.frame(total_precip, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann"))

# Calculate the precipitation hours using stackApply()
precip_hours_meth1 <- stackApply(persiann, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))
precip_hours_meth2 <- stackApply(persiann, indices, fun = function(x, na.rm = TRUE) sum(x > 0.1, na.rm = TRUE))

precip_hours <- stackApply(persiann, indices, fun = function(x, na.rm = TRUE) {
  sum(x > 0.1, na.rm = TRUE)
})

persiann_hour_dt <- as.data.frame(precip_hours, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))

summary(persiann_hour_dt)

persiann_intensity_meth1 <- total_precip / precip_hours_meth1
persiann_intensity_meth2 <- total_precip / precip_hours_meth2

persiann_intensity <- total_precip / precip_hours

plot(persiann_intensity[[1]])

persiann_int_dt_meth1 <- as.data.frame(persiann_intensity_meth1, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))

persiann_int_dt_meth2 <- as.data.frame(persiann_intensity_meth2, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))


persiann_int_dt <- as.data.frame(persiann_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))

summary(persiann_int_dt)

saveRDS(persiann_int_dt, "./projects/kenya_example/data/hourly_charact/int_persiann_hourly.RDS")

### Frequency

# Calculate the total available hours
total_available_hours <- length(indices)

ncell(persiann)
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


precip_hours <- stackApply(imerg, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))
imerg_intensity <- total_precip / precip_hours

plot(imerg_intensity[[1]])

imerg_int_dt <- as.data.frame(imerg_intensity, xy = TRUE) %>%
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

imerg_freq_dt <- as.data.frame(imerg_frequency, xy = TRUE) %>%
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

gsmap_int_dt <- as.data.frame(gsmap_intensity, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  as.data.table() %>% 
  `[`(, name := factor("gsmap"))

gsmap_int_dt <- gsmap_int_dt[, .(x = y, y = x, date = variable, prec_int = value, name)]
#setnames(gsmap_int_dt, c("variable", "value"), c("date", "prec_int"))

summary(gsmap_int_dt)

gsmap_int_dt[prec_int == "Inf"] # R gives Inf when something divided by 0 (It is possible that precipitation less than 0.1 mm/hr are accumulated to total precipitation amount, but the number of precipitation hours are yet zero according to the condition of precip > 0.1 mm /hr)

gsmap_int_dt[!is.finite(prec_int), prec_int := NA]
summary(gsmap_int_dt)

saveRDS(gsmap_int_dt, "./projects/kenya_example/data/hourly_charact/int_gsmap_hourly.RDS")

### Frequency

# Calculate the total available hours
total_available_hours <- length(indices)
gsmap_frequency <- (precip_hours / total_available_hours) * 100

gsmap_freq_dt <- as.data.frame(gsmap_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("gsmap"))

#setnames(gsmap_freq_dt, c("variable", "value"), c("date", "prec_freq"))
gsmap_freq_dt <- gsmap_freq_dt[, .(x = y, y = x, date = variable, prec_freq = value, name)]

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


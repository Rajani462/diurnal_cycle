
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(terra)
library(ncdf4)
library(sf)
library(rgdal)
library(parallel)
library(doParallel)
library(foreach)

##read the datasets
#persi <- brick("E:/PERSIANN/hourly/rkpradhan462gin12t/PERSIANN_2023-01-05062607am_200101.nc")
mean_ken <- brick("./projects/kenya_example/data/hourly_charact/hourly_mean_cmorph_kenya_2001_20.nc")

cmorph_2001_20 <- brick("./projects/kenya_example/data/cmorph_hour_kenya_2001_20.nc")
#mean, frequncy, intensity

mean_cmorph_2001_20 = mean(cmorph_2001_20, na.rm=FALSE)
(sum(era_ocn2, na.rm=FALSE) / nlayers(era_ocn2))
rain_hours <- calc(mean_era, fun = function(x) bitwAnd(x,32L))
mean_freq <- no of rain hours/ total hours
mean_inten


#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(cmorph_2001_20), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

hourly_mean<- stackApply(cmorph_2001_20, indices, fun = mean)
#names(MonthNDVI) <- month.abb

cmorph_dt <- as.data.frame(hourly_mean, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("cmorph"))

cdo_cmorph_dt <- as.data.frame(mean_ken, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("cmorph"))


# Intensity

total_precip <- stackApply(cmorph_2001_20, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours <- stackApply(cmorph_2001_20, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))
# precip_hours <- stackApply(cmorph_2001_20, indices, fun = function(x) {
#   n <- length(x)
#   sum(x > 0.1, na.rm = TRUE) / sum(!is.na(x))
# })
# Calculate the intensity (total precipitation divided by precipitation hours)
intensity <- total_precip / precip_hours

plot(intensity[[1]])

cmorph_prehours_dt <- as.data.frame(precip_hours, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_hours") %>%
  `[`(, name := factor("cmorph"))

cmorph_inte_dt <- as.data.frame(intensity, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>%
  `[`(, name := factor("cmorph"))


cmorph_sum_dt <- as.data.frame(total_precip, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_sum") %>%
  `[`(, name := factor("cmorph"))


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


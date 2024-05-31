library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(lubridate)
#library(terra)
library(ncdf4)
library(sf)
library(sp)
library(hms)
library(forcats)
library(parallel)
library("RColorBrewer")

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('source/graphics.R')


# read the datasets -------------------------------------------------------


gsmap_hourly <- raster("~/shared/data/obs/precip/raw/gsmap-v8_tp_mm_global60s60n_200101_202012_025_hourly.nc")
gsmap_hourly2 <- raster("~/shared/data/obs/precip/raw/gsmap-v8_tp_mm_global_199801_202306_025_hourly.nc")



cdo  -b 32 -P 50 -fldmean ~/shared/data/obs/precip/raw/gsmap-v8_tp_mm_global60s60n_200101_202012_025_hourly.nc ~/rajani/diurnal_cycle/projects/main/data/gsmap_60ns_hourly_mean_timeseries_2001_20.nc

#lets see how the data prepared by Mijael looks like
cdo  -b 32 -P 50 -fldmean ~/shared/data/obs/precip/raw/gsmap-v8_tp_mm_global_199801_202306_025_hourly.nc ~/rajani/diurnal_cycle/projects/main/data/gsmap_glob_hourly_mean_timeseries_1998_2023.nc

#lets see how the data prepared by Mijael looks like (for 60ns)
cdo  -b 32 -P 50 -sellonlatbox,-180,180,-60,60 -fldmean ~/shared/data/obs/precip/raw/gsmap-v8_tp_mm_global_199801_202306_025_hourly.nc ~/rajani/diurnal_cycle/projects/main/data/gsmap_60ns_hourly_mean_timeseries_1998_2023.nc



#gsmap_hourly <- raster("~/rajani/diurnal_cycle/projects/main/data/gsmap_60ns_hourly_mean_timeseries_2001_20.nc")
ncfile <- nc_open("~/rajani/diurnal_cycle/projects/main/data/gsmap_60ns_hourly_mean_timeseries_2001_20.nc")

# Get variable names
print(ncfile)

# Extract a specific variable (e.g., precipitation)
var_name <- "precip" # replace with the actual variable name
prec <- ncvar_get(ncfile, var_name)

# Get longitude and latitude
time <- ncvar_get(ncfile, "time")
time_units <- ncatt_get(ncfile, "time", "units")$value
time_origin <- sub(".*since ", "", time_units)
time_converted <- as.POSIXct(time * 3600, origin = time_origin, tz = "UTC")


# Combine into a data frame
gsmap_hourly <- data.table(date = time_converted, prec = prec)


ggplot(gsmap_hourly, aes(date, prec)) + 
  geom_line()

gsmap_hourly[, `:=`(day = day(date), month = month(date), year = year(date))]

gsmap_daily <- gsmap_hourly[, .(prec = sum(prec)), by  = .(day, month, year)]
gsmap_daily[, date := as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")]

ggplot(gsmap_daily, aes(date, prec)) + 
  geom_line()

ggsave("./projects/main/results/xx_gsmap_daily_mean_60ns_2001_20.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)

gsmap_monthly <- gsmap_daily[, .(prec = sum(prec)), by  = .(month, year)]
gsmap_monthly[, date := as.Date(paste(year, month, sep = "-"), format = "%Y-%m-%d")]

ggplot(gsmap_daily, aes(month, prec)) + 
  geom_line()

#annual

gsmap_annual <- gsmap_daily[, .(prec = sum(prec)), by  = .(year)]

ggplot(gsmap_annual, aes(year, prec)) + 
  geom_line() + 
  geom_point()

ggsave("./projects/main/results/xx_gsmap_annual_mean_60ns_2001_20.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)

#########################################################

ncfile2 <- nc_open("~/rajani/diurnal_cycle/projects/main/data/gsmap_60ns_hourly_mean_timeseries_1998_2023.nc")


# Extract a specific variable (e.g., precipitation)
var_name <- "tp" # replace with the actual variable name
prec <- ncvar_get(ncfile2, var_name)


time_var <- ncvar_get(ncfile2, "time")
# Get the time units attribute
time_units <- ncatt_get(ncfile2, "time", "units")$value
ref_time <- sub("seconds since ", "", time_units)
# Convert the time variable to POSIXct format
time_posix2 <- as.POSIXct(time_var, origin = ref_time, tz = "UTC")

# Combine into a data frame
gsmap_hourly2 <- data.table(date = time_posix2, prec = prec)
#str(gsmap_dt)

ggplot(gsmap_hourly2, aes(date, prec)) + 
  geom_line()

gsmap_hourly2[, `:=`(day = day(date), month = month(date), year = year(date))]

gsmap_daily2 <- gsmap_hourly2[, .(prec = sum(prec)), by  = .(day, month, year)]
gsmap_daily2[, date := as.Date(paste(year, month, day, sep = "-"), format = "%Y-%m-%d")]

ggplot(gsmap_daily2, aes(date, prec)) + 
  geom_line()

ggsave("./projects/main/results/xx_gsmap_daily_mean_60ns_1998_2023.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)

gsmap_monthly2 <- gsmap_daily2[, .(prec = sum(prec)), by  = .(month, year)]
gsmap_monthly2[, date := as.Date(paste(year, month, sep = "-"), format = "%Y-%m-%d")]

ggplot(gsmap_daily2, aes(month, prec)) + 
  geom_line()

#annual

gsmap_annual2 <- gsmap_daily2[, .(prec = sum(prec)), by  = .(year)]

ggplot(gsmap_annual2, aes(year, prec)) + 
  geom_line() + 
  geom_point()

ggsave("./projects/main/results/xx_gsmap_annual_mean_60ns_1998_2023.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)


#####
#compare both the dataset

gsmap_annual2[, `:=`(name = "gsmap_aft")]
gsmap_annual[, `:=`(name = "gsmap_bef")]

gsmap_merg <- rbind(gsmap_annual, gsmap_annual2)

ggplot(gsmap_merg, aes(year, prec, col = name)) + 
  geom_line() + 
  geom_point()

ggsave("./projects/main/results/xx_gsmap_annual_mean_60ns_bef_aft_compa.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)



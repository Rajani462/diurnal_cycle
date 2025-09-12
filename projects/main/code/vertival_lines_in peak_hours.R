

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
#library(reshape)
#library(terra)
library(ncdf4)
library(sf)
library(hms)
library(forcats)
library(parallel)
library(foreach)
library(doParallel)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


## read the data sets -------------------------------

#dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_mean_thres_list.RDS")
data_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")

# data_dt <- rbindlist(data_list)
# #data_dt[, `:=`(time_lst = NULL, tmz_offset = NULL)]
# levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5", "ERA5nsft")
# levels(data_dt$location) <- c("Land", "Ocean")

## Pre-process ----------------------------------------------


### Estimate the peak hour of data.tables -------------------------------------------

system.time(peak_hour_dt <- data_dt[, .SD[which.max(prec_mean)], by = .(lat, lon, name)])
# user  system elapsed 
# 488.337   2.325 362.277 

# Your original function definition

find_peak_hour2 <- function(dt) {
  library(data.table)
  library(dplyr)
  return(dt %>% 
           group_by(lat, lon) %>% 
           slice_max(prec_mean, n = 1))
}



# Using parLapply to apply the function to each element of the list
num_cores <- detectCores()-58  # Adjust as needed
cl <- makeCluster(num_cores)
system.time(peak_hour_list <- parLapply(cl, data_list, find_peak_hour2))
# user  system elapsed 
# 6.115   1.386  85.901

# Stop the cluster
stopCluster(cl)

# Combining the results into a single data table
peak_hour_dt <- rbindlist(peak_hour_list)
#write.csv(peak_hour_dt[name == "era5"], "peak_jour_dt_era5new.csv")
View(peak_hour_dt[name == "era5"])
peak_hour_dt[, `:=`(peah_hour_lst = lubridate:: hour(time_utc) +  (tmz_offset))]

peak_hour_dt[, `:=`(peak_hour_lst2 = convert_UTC_to_solartime(peak_hour_dt$time_utc, peak_hour_dt$lon, time.type = c("mean solar")))]
peak_hour_dt[, `:=`(peak_hour_lst3 = as.POSIXct(paste(1970, format(time_lst, "%m-%d %H:%M:%S")), format = "%Y %m-%d %H:%M:%S")]

peak_hour_dt$peak_hour_lst2 <- as.POSIXct(paste("1970-01-01", times, format(peak_hour_dt$time_lst, "%m-%d %H:%M:%S")), format = "%Y %m-%d %H:%M:%S")

peak_hour_dt$peak_hour_lst2 <- as.POSIXct(paste("1970-01-01", hour(peak_hour_dt$time_lst)), format = "%Y-%m-%d %H:%M:%S")


peak_hour_dt <- peak_hour_dt[peah_hour_lst > 23, peah_hour_lst := (-35 + peah_hour_lst)]
peak_hour_dt <- peak_hour_dt[peah_hour_lst < 0, peah_hour_lst := (12 - peah_hour_lst)]

summary(peak_hour_dt)


# Extract times from the original data
times <- format(peak_hour_dt$time_lst, format = "%H:%M:%S")

# Create new POSIXct object with "1970-01-01" date and the times from the original data
new_datetime <- as.POSIXct(paste("1970-01-01", times), format = "%Y-%m-%d %H:%M:%S")

# Replace the time component in peak_hour_dt
peak_hour_dt$peak_hour_lst2 <- new_datetime

summary(peak_hour_dt)


era <- peak_hour_dt[name == "era5nsft" & lon > -8.125 & lon < 7.875 & lat > -1.625 & lat < 1.625]
View(era)

summary(era)
era[lon == 7.125]
era[lon == 7.375]
era[lon == 7.625]

era[lon == -7.875]
era[lon == -7.625]
era[lon == -7.375]
era[lon == -7.125]
era[lon == -6.875]

library(forcats)
ggplot(peak_hour_dt[name == "era5nsft" & lon > -8.125 & lon < 7.875 & lat > -1.625 & lat < 1.625]) + 
  geom_tile(aes(lon, lat, fill = factor(hour(time_utc)))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_wrap(~name, ncol = 2) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/06a_extratrial_peak_hour_mean.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)

######################################
# try with frequency ------------------------------------------------------

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
#library(reshape)
#library(terra)
library(ncdf4)
library(sf)
library(hms)
library(forcats)
library(parallel)
library(foreach)
library(doParallel)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


## read the data sets -------------------------------

#dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_mean_thres_list.RDS")
data_list <- readRDS("./projects/main/data/freq_peak_hour_dt_2001_20.RDS")

peak_hour_dt <- data_list$era5

#ggplot(peak_hour_dt[name == "era5nsft" & lon > -8.125 & lon < 7.875 & lat > -1.625 & lat < 1.625]) + 
ggplot(peak_hour_dt) + 
  geom_tile(aes(lon, lat, fill = factor(hour(time_utc)))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_wrap(~name, ncol = 2) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/xxx_extratrial_peak_hour_freq_UTC.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)
write.csv(peak_hour_dt, "~/rajani/diurnal_cycle/projects/main/code/peak_hor.csv")

ggplot(peak_hour_dt) + 
  geom_tile(aes(lon, lat, fill = factor(hour(time_lst)))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_wrap(~name, ncol = 2) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/xxx_extratrial_peak_hour_freq_LST.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)

write.csv(peak_hour_dt, "./projects/main/results/paek_hour_freq_era.csv")


ggplot(peak_hour_dt[lon > -8.125 & lon < 7.875 & lat > -1.625 & lat < 1.625]) + 
  geom_tile(aes(lon, lat, fill = factor(hour(time_utc)))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_wrap(~name, ncol = 2) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/xxx_extratrial_peak_hour_freq_UTC.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)

write.csv(peak_hour_dt[lon > -8.125 & lon < 7.875 & lat > -1.625 & lat < 1.625], "./projects/main/results/paek_hour_freq_era_tropic.csv")

ggplot(peak_hour_dt[lon > -179 & lon < -100 & lat > -30 & lat < 30]) + 
  geom_tile(aes(lon, lat, fill = factor(hour(time_lst)))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_wrap(~name, ncol = 2) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/xxx_extratrial_peak_hour_freq_LST_tropic2.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)

ggplot(peak_hour_dt[lon > -179 & lon < -100 & lat > -30 & lat < 30]) + 
  geom_tile(aes(lon, lat, fill = factor(hour(time_utc)))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_wrap(~name, ncol = 2) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/xxx_extratrial_peak_hour_freq_UTC_tropic2.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)
write.csv(peak_hour_dt[lon > -179 & lon < -100 & lat > -30 & lat < 30], "./projects/main/results/paek_hour_freq_era_tropic2.csv")


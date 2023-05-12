
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

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


## read the data sets -------------------------------

dataset_names <- c("imerg", "cmorph", "persiann", "era5", "gsmap")
intensity_list <- list()
frequency_list <- list()

for (dataset_name in dataset_names) {
  intensity_file <- file.path("./projects/kenya_example/data/output", paste0(dataset_name, "_intensity.RDS"))
  frequency_file <- file.path("./projects/kenya_example/data/output", paste0(dataset_name, "_frequency.RDS"))
  
  intensity_list[[dataset_name]] <- readRDS(intensity_file)
  frequency_list[[dataset_name]] <- readRDS(frequency_file)
}


## Pre-process -----------------------------------------------

int <- rbindlist(intensity_list)
freq <- rbindlist(frequency_list)

int <- int[, .(x, y, date, value = prec_int, name, variable)]
freq <- freq[, .(x, y, date, value = prec_freq, name, variable)]

int_freq <- rbind(int, freq)


# change the time to LST 

int_freq$date <- substr(int_freq$date, 7, 8) %>% paste0(":00:00")
int_freq <- int_freq[, .(lat = y, lon = x, time_utc = as_hms(date), value, name, variable)]
int_freq[, `:=`(tmz_offset = round((lon / 15)))]
int_freq$time_utc <- as.POSIXct(int_freq$time_utc)
int_freq[,  `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]


## plot ----------------------------------------

### intensity

peak_hour <- int_freq[, .SD[which.max(value)], by = .(lat, lon, name, variable)]

peak_hour_int <- peak_hour[variable == "intensity", .(lat, lon, peak_hour = hour(time_lst), value, name, variable)]

ggplot(peak_hour_int) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_int$lon), max(peak_hour_int$lon)), 
                  ylim = c(min(peak_hour_int$lat), max(peak_hour_int$lat))) + 
  facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of Intensity") + 
  theme_small


ggsave("./projects/kenya_example/results/05b_peak_int.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)


### frequency

peak_hour_freq <- peak_hour[variable == "frequency", .(lat, lon, peak_hour = hour(time_lst), value, name, variable)]
levels(peak_hour_freq$name) <- c("IMERG", "CMORPH", "PERSIANN", "ERA5", "GSMaP")


ggplot(peak_hour_freq) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_freq$lon), max(peak_hour_freq$lon)), 
                  ylim = c(min(peak_hour_freq$lat), max(peak_hour_freq$lat))) + 
  facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of freqensity") + 
  theme_small


ggsave("./projects/kenya_example/results/05b_peak_freq.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)



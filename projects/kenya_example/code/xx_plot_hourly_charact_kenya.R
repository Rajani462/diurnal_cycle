
#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(reshape)
#library(terra)
library(ncdf4)
library(sf)
library(hms)

##read the datasets

hour_mean <- readRDS("./projects/kenya_example/data/hourly_charact/all_hourly_mean.RDS")

cmorph_int <- readRDS("./projects/kenya_example/data/hourly_charact/int_cmorph_hourly.RDS")
era5_int <- readRDS("./projects/kenya_example/data/hourly_charact/int_era5_hourly.RDS")
pers_int <- readRDS("./projects/kenya_example/data/hourly_charact/int_persiann_hourly.RDS")
imerg_int <- readRDS("./projects/kenya_example/data/hourly_charact/int_imerg_hourly.RDS")
gsmap_int <- readRDS("./projects/kenya_example/data/hourly_charact/int_gsmap_hourly.RDS")

cmorph_freq <- readRDS("./projects/kenya_example/data/hourly_charact/freq_cmorph_hourly.RDS")
era5_freq <- readRDS("./projects/kenya_example/data/hourly_charact/freq_era5_hourly.RDS")
pers_freq <- readRDS("./projects/kenya_example/data/hourly_charact/freq_persiann_hourly.RDS")
imerg_freq <- readRDS("./projects/kenya_example/data/hourly_charact/freq_imerg_hourly.RDS")
gsmap_freq <- readRDS("./projects/kenya_example/data/hourly_charact/freq_gsmap_hourly.RDS")

int_hourly <- rbind(imerg_int, cmorph_int, pers_int, era5_int, gsmap_int)
freq_hourly <- rbind(imerg_freq, cmorph_freq, pers_freq, era5_freq, gsmap_freq)

# change the time to LST 

int_hourly$date <- substr(int_hourly$date, 7, 8) %>% paste0(":00:00")
int_hourly <- int_hourly[, .(lat = y, lon = x, time_utc = as_hms(date), prec_int, name)]
int_hourly[, `:=`(tmz_offset = round((lon / 15)))]
int_hourly$time_utc <- as.POSIXct(int_hourly$time_utc)
int_hourly[,  `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]

freq_hourly$date <- substr(freq_hourly$date, 7, 8) %>% paste0(":00:00")
freq_hourly <- freq_hourly[, .(lat = y, lon = x, time_utc = as_hms(date), prec_freq, name)]
freq_hourly[, `:=`(tmz_offset = round((lon / 15)))]
freq_hourly$time_utc <- as.POSIXct(freq_hourly$time_utc)
freq_hourly[,  `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]


## plot

int_hourly_spat_lst <- int_hourly[, .(prec_int = mean(prec_int, na.rm = TRUE)), by = .(hour(time_lst), name)]
levels(int_hourly_spat_lst$name) <- c("IMERG", "CMORPH", "PERSIANN", "ERA5", "GSMaP")

ggplot(int_hourly_spat_lst, aes(hour, prec_int, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Time", y = "Intensity (mm/hr)", fill = "") + 
  theme_small + 
  theme(legend.title = element_blank())

ggsave("./projects/kenya_example/results/diurnal_intensity_lst.png",
       width = 8.5, height = 5.9, units = "in", dpi = 600)

## Frequency
freq_hourly_spat_lst <- freq_hourly[, .(prec_freq = mean(prec_freq, na.rm = TRUE)), by = .(hour(time_lst), name)]
levels(freq_hourly_spat_lst$name) <- c("IMERG", "CMORPH", "PERSIANN", "ERA5", "GSMaP")

ggplot(freq_hourly_spat_lst, aes(hour, prec_freq, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Time", y = "Frequency (%)", fill = "") + 
  theme_small + 
  theme(legend.title = element_blank())

ggsave("./projects/kenya_example/results/diurnal_frequency_lst.png",
       width = 8.5, height = 5.9, units = "in", dpi = 600)



# spatial mean plots -------------------------------------------------------

### mean

levels(hour_mean$name) <- c("IMERG", "CMORPH", "PERSIANN", "ERA5", "GSMaP")

summary(hour_mean)

ggplot(hour_mean) + 
  geom_raster(aes(lon, lat, fill = prec_mean)) +
  #scale_fill_viridis(direction = -1) + 
  scale_fill_binned(type = "viridis", direction = -1, 
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.5), show.limits = TRUE) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(all_dt$lon), max(all_dt$lon)), 
                  ylim = c(min(all_dt$lat), max(all_dt$lat))) + 
  facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean\nPrecipitation\n (mm/hr)") +
  theme_small +
  theme(legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/spatial_hourly_mean.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)


### intensity

int_hourly_mean <- int_hourly[, .(prec_int = round(mean(prec_int, na.rm = TRUE), 2)), by = .(lat, lon, name)]
levels(int_hourly_mean$name) <- c("IMERG", "CMORPH", "PERSIANN", "ERA5", "GSMaP")

summary(int_hourly_mean)

ggplot(int_hourly_mean) + 
  geom_tile(aes(lon, lat, fill = prec_int)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(int_hourly_mean$lon), max(int_hourly_mean$lon)), 
                  ylim = c(min(int_hourly_mean$lat), max(int_hourly_mean$lat))) + 
  
  scale_fill_binned(type = "viridis", direction = -1, 
                    breaks = c(0.5, 1.0, 1.5, 2.0, 2.5, 3), show.limits = TRUE) +
  #coord_sf(ylim = c(-29, 29)) + 
  facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Intensity\n (mm/hr)") +
  theme_small +
  theme(legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/spatial_hourly_mean_intensity.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)


### frequency

freq_hourly_mean <- freq_hourly[, .(prec_freq = round(mean(prec_freq, na.rm = TRUE), 2)), by = .(lat, lon, name)]
levels(freq_hourly_mean$name) <- c("IMERG", "CMORPH", "PERSIANN", "ERA5", "GSMaP")

summary(freq_hourly_mean)

ggplot(freq_hourly_mean) + 
  geom_raster(aes(lon, lat, fill = prec_freq)) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(freq_hourly_mean$lon), max(freq_hourly_mean$lon)), 
                  ylim = c(min(freq_hourly_mean$lat), max(freq_hourly_mean$lat))) + 
  #scale_fill_viridis(direction = -1) + 
  scale_fill_binned(type = "viridis", direction = -1, 
                    breaks = c(0.3, 0.5, 0.7, 0.9, 1.1, 1.3), show.limits = TRUE) + 
  facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Frequency\n (%)") + 
  theme_small +
  theme(legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/spatial_hourly_mean_frequency.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)

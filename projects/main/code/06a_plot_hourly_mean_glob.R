
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

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


## read the data sets -------------------------------

#dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_mean_thres_list.RDS")
data_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")

data_dt <- rbindlist(data_list)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$season) <- c("JJA", "DJF")
levels(data_dt$location) <- c("Land", "Ocean")

## Pre-process ----------------------------------------------

### spatial mean plot --------------------------------------

spat_mean_dt <- data_dt[, .(mean_value = round(mean(prec_mean, na.rm = TRUE), 2)), by = .(lat, lon, name)]

summary(spat_mean_dt)

ggplot(spat_mean_dt) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.5, 1, 1.5, 2), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_dt$lon), max(spat_mean_dt$lon)), 
                  ylim = c(min(spat_mean_dt$lat), max(spat_mean_dt$lat))) + 
  facet_wrap(~name, ncol = 2) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean\n  \n (mm/hr)") + 
  #facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")) + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(1.0, 'cm'))

ggsave("./projects/main/results/06a_spat_mean_threshold_0.1.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)



### 24hr diurnal cycle line plot ---------------------------------------------------------------------

## for glob

mean_24h_glob <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name)]

ggplot(mean_24h_glob, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  #facet_wrap(~location) + 
  labs(x ="Hour (LST)", y = "Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06a_24hlineplot_mean_glob.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

## for glob seasons

mean_24h_glob_seas <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, season)]

ggplot(mean_24h_glob_seas, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~season) + 
  labs(x ="Hour (LST)", y = "Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06a_24hlineplot_mean_glob_seas.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

##for land and ocean
mean_24h_landocn <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, location)]

ggplot(mean_24h_landocn, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~location) + 
  labs(x ="Hour (LST)", y = "Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06a_24hlineplot_mean_landocn.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


##for seasons land and ocean
mean_list_landocn_seas <- lapply(mean_list, function(dt) {
  dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, location, season)]
})

mean_24h_landocn_seas <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, location, season)]

ggplot(mean_24h_landocn_seas, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_grid(location~season) + 
  labs(x ="Hour (LST)", y = "Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06a_24hlineplot_mean_landocn_seas.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


### Estimate the peak hour of data.tables -------------------------------------------

system.time(peak_hour_dt <- data_dt[, .SD[which.max(prec_mean)], by = .(lat, lon, name)])
# user  system elapsed 
# 488.337   2.325 362.277 

peak_hour_dt[, `:=`(peak_hour = hour(time_lst))]

library(forcats)
ggplot(peak_hour_dt) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
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

ggsave("./projects/main/results/06a_plot_spat_peak_hour_mean_threshold_0.1.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)


### seasonal

system.time(peak_hour_seas_dt <- data_dt[, .SD[which.max(prec_mean)], by = .(lat, lon, name, season)])
# user  system elapsed 
# 865.650   2.789 729.145 

peak_hour_seas_dt[, `:=`(peak_hour = hour(time_lst))]

library(forcats)
ggplot(peak_hour_seas_dt) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_seas_dt$lon), max(peak_hour_seas_dt$lon)), 
                  ylim = c(min(peak_hour_seas_dt$lat), max(peak_hour_seas_dt$lat))) + 
  facet_grid(name~season) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/06a_plot_spat_peak_hour_mean_threshold_0.1_season.png", width = 10.5, height = 7.9, 
       units = "in", dpi = 600)

###############################################################
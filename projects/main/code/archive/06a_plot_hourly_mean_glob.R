
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
mean_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")


## Pre-process ----------------------------------------------

### spatial mean plot --------------------------------------

spat_mean_list <- lapply(mean_list, function(dt) {
  dt[, .(mean_value = round(mean(prec_mean, na.rm = TRUE), 2)), by = .(lat, lon, name)]
})

# Combine the results into a single data.table
spat_mean_dt <- rbindlist(spat_mean_list)
levels(spat_mean_dt$name)

levels(spat_mean_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

# mean precipitation
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

mean_list_glob <- lapply(mean_list, function(dt) {
  dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name)]
})

mean_24h_glob <- rbindlist(mean_list_glob)
levels(mean_24h_glob$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

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

mean_list_glob_seas <- lapply(mean_list, function(dt) {
  dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, season)]
})

mean_24h_glob_seas <- rbindlist(mean_list_glob_seas)
levels(mean_24h_glob_seas$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(mean_24h_glob_seas$season) <- c("JJA", "DJF")

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
mean_list_landocn <- lapply(mean_list, function(dt) {
  dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, location)]
})

mean_24h_landocn <- rbindlist(mean_list_landocn)
levels(mean_24h_landocn$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(mean_24h_landocn$location) <- c("Land", "Ocean")

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

mean_24h_landocn_seas <- rbindlist(mean_list_landocn_seas)
levels(mean_24h_landocn_seas$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(mean_24h_landocn_seas$location) <- c("Land", "Ocean")
levels(mean_24h_landocn_seas$season) <- c("JJA", "DJF")

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




ggplot(mean_mean_24h, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~name,  scales = "free_y") + 
  labs(x ="Hour (LST)", y = " Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06a_faceted_24hlineplot_mean_threshold_0.1.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)



### Estimate the peak hour of data.tables -------------------------------------------

num_cores <- detectCores() - 59
cl = makeCluster(num_cores)
clusterEvalQ(cl, {
  library(data.table)
})
# Parallel processing using parLapply
# user  system elapsed 
# 4.243   0.980 134.126 #with 4 cores (after restart)
#14.769   5.094 133.338 #with 4 cores
#4.124   1.121  74.012 #with 5 cores
#4.147   1.024  73.412 #with 5 cores (after restart)
#4.260   1.070  73.996 #with 12 cores
# 4.255   1.172 202.681 #with 2 cores

system.time(peak_hour_list <- parLapplyLB(cl, mean_list, function(dt) {
  # Modify the code to use the data.table "int_mean"
  peak_hour <- dt[, .SD[which.max(prec_mean)], keyby = .(lat, lon, name)]
  peak_hour <- peak_hour[, .(lat, lon, peak_hour = hour(time_lst), name)]
  return(peak_hour)
}))

# Stop the cluster
stopCluster(cl)

#merge the list to data.table
peak_hour_dt <- rbindlist(peak_hour_list)

# Check the new order of levels
levels(peak_hour_dt$name)
# Change the order of levels 
levels(peak_hour_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")


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

#################################################################################

# land and ocean ----------------------------------------------------------

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
#library(ggh4x)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


## read the data sets -------------------------------

#dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_freq_thres_list.RDS")
mean_land_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_land_2001_20.rds")
mean_ocn_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_ocn_2001_20.rds")

mean_list <- c(mean_land_list, mean_ocn_list)

### 24hr diurnal cycle line plot -------------------------------------------

mean_mean_list <- lapply(mean_list, function(dt) {
  dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, type)]
})

mean_mean_24h <- rbindlist(mean_mean_list)

levels(mean_mean_24h$name)
levels(mean_mean_24h$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(mean_mean_24h$type) <- c("Land", "Ocean")


str(mean_mean_24h)

## mean, frequency and frequency together

ggplot(mean_mean_24h, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "Mean (mm/hr)") + 
  facet_wrap(~type) + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06a_24hlineplot_mean_land_ocn_threshold_0.1.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


### spatial mean plot --------------------------------------

spat_mean_list <- lapply(freq_list, function(dt) {
  dt[, .(mean_value = round(mean(prec_mean, na.rm = TRUE), 2)), by = .(lat, lon, name, type)]
})

# Combine the results into a single data.table
spat_mean_dt <- rbindlist(spat_mean_list)
levels(spat_mean_dt$name)

levels(spat_mean_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(spat_mean_dt$type) <- c("Land", "Ocean")


# mean precipitation
summary(spat_mean_dt)

ggplot(spat_mean_dt) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(5, 10, 15, 20, 25, 30, 40, 50, 60, 70), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_dt$lon), max(spat_mean_dt$lon)), 
                  ylim = c(min(spat_mean_dt$lat), max(spat_mean_dt$lat))) + 
  facet_grid(type~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean\n  \n (%)") + 
  #facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")) + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(1.0, 'cm'))


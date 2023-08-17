
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

#dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_int_thres_list.RDS")
data_list <- readRDS("./projects/main/data/hourly_int_all_datasets_LST_glob_2001_20_seasonal.rds")
data_list1 <- readRDS("./projects/main/data/hourly_int_thres_0.5_all_datasets_LST_glob_2001_20_seasonal.rds")


merged_list <- lapply(data_list, function(dataset) merge(dataset, rbindlist(data_list1), 
                                                         by = c("lat", "lon", "time_utc", "name", "season", 
                                                                "location", "tmz_offset", "time_lst")))

saveRDS(merged_list, "./projects/main/data/hourly_int_thres_0.1_0.5_all_datasets_LST_glob_2001_20_seasonal.rds")

#restart and read the dataset again to save memory

data_dt <-  readRDS("./projects/main/data/hourly_int_thres_0.1_0.5_all_datasets_LST_glob_2001_20_seasonal.rds")


data_dt <- rbindlist(data_dt)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$season) <- c("JJA", "DJF")
levels(data_dt$location) <- c("Land", "Ocean")

## Pre-process ----------------------------------------------

### spatial mean plot --------------------------------------

spat_mean_dt <- data_dt[, .('0.1' = round(mean(prec_int, na.rm = TRUE), 2), 
                            '0.5' = round(mean(prec_int_0.5, na.rm = TRUE), 2)), by = .(lat, lon, name, season)]

summary(spat_mean_dt)

spat_mean_dt <- melt(spat_mean_dt, c("lat", "lon", "name", "season"), variable.name = "threshold")

ggplot(spat_mean_dt) + 
  geom_raster(aes(lon, lat, fill = value)) +
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(0.1, 0.3, 0.6, 0.9, 1.2, 1.5, 2, 2.5, 3, 4, 5, 7), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_dt$lon), max(spat_mean_dt$lon)), 
                  ylim = c(min(spat_mean_dt$lat), max(spat_mean_dt$lat))) + 
  facet_grid(season~threshold~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Intensity (mm/hr)") + 
  #facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")) + 
  theme_small + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "horizontal", legend.position = "bottom", legend.key.width = unit(2.6, "cm"),
        legend.key.height = unit(0.35, 'cm'), legend.text = element_text(size = 10, hjust = 0.5)) + 
  guides(fill = guide_colorsteps(title.position = "top", title.hjust =0.5))

ggsave("./projects/main/results/06C_spat_int_thres_0.1_0.5_seasonal.png", width = 11.5, height = 5.3, 
       units = "in", dpi = 600)



### 24hr diurnal cycle line plot ---------------------------------------------------------------------

## for glob

mean_24h_glob <- data_dt[, .('0.1' = mean(prec_int, na.rm = TRUE), 
                             '0.5' = mean(prec_int_0.5, na.rm = TRUE)), by = .(hour(time_lst), name, season)]

mean_24h_glob <- melt(mean_24h_glob, c("hour", "name", "season"), variable = "threshold")

ggplot(mean_24h_glob, aes(hour, value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(season~threshold) + 
  labs(x ="Hour (LST)", y = "Intensity (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06c_24hlineplot_int_thres_0.1_0.5_glob_seasonal.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)



##for seasons land and ocean
# mean_list_landocn_seas <- lapply(mean_list, function(dt) {
#   dt[, .(mean_value = mean(prec_int, na.rm = TRUE)), by = .(hour(time_lst), name, location, season)]
# })

mean_24h_landocn_seas <- data_dt[, .('0.1' = mean(prec_int, na.rm = TRUE), 
                                     '0.5' = mean(prec_int_0.5, na.rm = TRUE)), by = .(hour(time_lst), name, location, season)]

mean_24h_landocn_seas <- melt(mean_24h_landocn_seas, c("hour", "name", "location", "season"), variable = "threshold")


ggplot(mean_24h_landocn_seas, aes(hour, value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_grid(location~season~threshold) + 
  labs(x ="Hour (LST)", y = "Intensity (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06c_24hlineplot_int_thres_0.1_0.5_landocn_seasonal.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


### Estimate the peak hour of data.tables -------------------------------------------

system.time(peak_hour_dt <- data_dt[, .SD[which.max(prec_int_0.5)], by = .(lat, lon, name, season)])
# user  system elapsed 
# 488.337   2.325 362.277 
saveRDS(peak_hour_dt, "./projects/main/data/freq_thres_0.5_peak_hour_dt_2001_20_seasonal.RDS")

peak_hour_dt <- readRDS("./projects/main/data/freq_thres_0.5_peak_hour_dt_2001_20_seasonal.RDS")
peak_hour_dt[, `:=`(peak_hour = hour(time_lst))]
peak_hour_dt[, `:=`(time_lst = NULL)]
# peak_hour_dt[peak_hour  == "1" | peak_hour  == "2" | peak_hour  == "3", peak_hour2 := '1-3']
# peak_hour_dt[peak_hour  == "4" | peak_hour  == "5" | peak_hour  == "6", peak_hour2 := '4-6']
# peak_hour_dt[peak_hour  == "7" | peak_hour  == "8" | peak_hour  == "9", peak_hour2 := '7-9']
# peak_hour_dt[peak_hour  == "10" | peak_hour  == "11" | peak_hour  == "12", peak_hour2 := '10-12']
# peak_hour_dt[peak_hour  == "13" | peak_hour  == "14" | peak_hour  == "15", peak_hour2 := '13-15']
# peak_hour_dt[peak_hour  == "16" | peak_hour  == "17" | peak_hour  == "18", peak_hour2 := '16-18']
# peak_hour_dt[peak_hour  == "19" | peak_hour  == "20" | peak_hour  == "21", peak_hour2 := '19-21']
# peak_hour_dt[peak_hour  == "22" | peak_hour  == "23" | peak_hour  == "0", peak_hour2 := '22-00']

peak_hour_dt[peak_hour  == "15" | peak_hour  == "16" | peak_hour  == "17" | peak_hour  == "18", peak_hour2 := '15-18']
peak_hour_dt[peak_hour  == "19" | peak_hour  == "20" | peak_hour  == "21" | peak_hour  == "22", peak_hour2 := '19-22']
peak_hour_dt[peak_hour  == "23" | peak_hour  == "0" | peak_hour  == "1" | peak_hour  == "2", peak_hour2 := '23-02']
peak_hour_dt[peak_hour  == "3" | peak_hour  == "4" | peak_hour  == "5" | peak_hour  == "6", peak_hour2 := '03-06']
peak_hour_dt[peak_hour  == "7" | peak_hour  == "8" | peak_hour  == "9" | peak_hour  == "10", peak_hour2 := '07-10']
peak_hour_dt[peak_hour  == "11" | peak_hour  == "12" | peak_hour  == "13" | peak_hour  == "14", peak_hour2 := '11-14']

library(forcats)
ggplot(peak_hour_dt[season == "JJA"]) + 
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

ggsave("./projects/main/results/06c_plot_spat_peak_hour_int_thres_0.5_seasonal_JJA.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)

ggplot(peak_hour_dt[season == "DJF"]) + 
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

ggsave("./projects/main/results/06c_plot_spat_peak_hour_int_thres_0.5_seasonal_DJF.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)

ggplot(peak_hour_dt[season == "JJA"]) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour2))) + 
  # scale_fill_manual(values = c("#EBB582", "#FFCC66", "#F0810F", "#CC3333",
  #                                       "#ACBD78",  "#739F3D", "#99CC00", "#009999")) + 
  scale_fill_manual(values = c("#FFCC66", "#9999CC", "#33CCCC", "#99CC00", "#F0810F", "#CC3333")) +
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

ggsave("./projects/main/results/06c_plot_spat_peak_hour_int_thres_0.5_seasonal_JJA_clasfy.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)

ggplot(peak_hour_dt[season == "DJF"]) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour2))) + 
  # scale_fill_manual(values = c("#EBB582", "#FFCC66", "#F0810F", "#CC3333",
  #                                       "#ACBD78",  "#739F3D", "#99CC00", "#009999")) + 
  scale_fill_manual(values = c("#FFCC66", "#9999CC", "#33CCCC", "#99CC00", "#F0810F", "#CC3333")) +
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

ggsave("./projects/main/results/06c_plot_spat_peak_hour_int_thres_0.5_seasonal_DJF_clasfy.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)
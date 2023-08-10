
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

#dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_freq_thres_list.RDS")
data_list <- readRDS("./projects/main/data/hourly_freq_all_datasets_LST_glob_2001_20.rds")
data_list1 <- readRDS("./projects/main/data/hourly_freq_thres_0.5_all_datasets_LST_glob_2001_20.rds")

data_list1_dt <- rbindlist(data_list1)


merged_list <- lapply(data_list, function(dataset) merge(dataset, rbindlist(data_list1), 
                                                         by = c("lat", "lon", "time_utc", "name", "season", 
                                                                "location", "tmz_offset", "time_lst")))

saveRDS(merged_list, "./projects/main/data/hourly_freq_thres_0.1_0.5_all_datasets_LST_glob_2001_20.rds")
data_dt <-  readRDS("./projects/main/data/hourly_freq_thres_0.1_0.5_all_datasets_LST_glob_2001_20.rds")


data_dt <- rbindlist(data_dt)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$season) <- c("JJA", "DJF")
levels(data_dt$location) <- c("Land", "Ocean")

## Pre-process ----------------------------------------------

### spatial mean plot --------------------------------------

spat_mean_dt <- data_dt[, .(freq_0.1 = round(mean(prec_freq, na.rm = TRUE), 2), 
                            freq_0.5 = round(mean(prec_freq_0.5, na.rm = TRUE), 2)), by = .(lat, lon, name)]

summary(spat_mean_dt)

ggplot(spat_mean_dt) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 50), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_dt$lon), max(spat_mean_dt$lon)), 
                  ylim = c(min(spat_mean_dt$lat), max(spat_mean_dt$lat))) + 
  facet_wrap(~name, ncol = 2) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Frequency\n  \n (%)") + 
  #facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")) + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(1.0, 'cm'))

ggsave("./projects/main/results/06b_spat_freq_thres_0.5.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)



### 24hr diurnal cycle line plot ---------------------------------------------------------------------

## for glob

mean_24h_glob <- data_dt[, .(mean_value_0.1 = mean(prec_freq, na.rm = TRUE), 
                             mean_value_0.5 = mean(prec_freq_0.5, na.rm = TRUE)), by = .(hour(time_lst), name)]

mean_24h_glob <- melt(mean_24h_glob, c("hour", "name"), variable = "threshold", value = "mean_value")

ggplot(mean_24h_glob, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~threshold) + 
  labs(x ="Hour (LST)", y = "Frequency (%)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06b_24hlineplot_freq_thres_0.1_0.5_glob.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

## for glob seasons

mean_24h_glob_seas <- data_dt[, .('0.1' = mean(prec_freq, na.rm = TRUE), 
                                  '0.5' = mean(prec_freq_0.5, na.rm = TRUE)), by = .(hour(time_lst), name, season)]

mean_24h_glob_seas <- melt(mean_24h_glob_seas, c("hour", "name", "season"), variable = "threshold", value = "mean_value")

ggplot(mean_24h_glob_seas, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_grid(season~threshold) + 
  labs(x ="Hour (LST)", y = "Frequency (%)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06b_24hlineplot_freq_thres_0.1_0.5_glob_seas.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

##for land and ocean
mean_24h_landocn <- data_dt[, .('0.1' = mean(prec_freq, na.rm = TRUE), 
                                '0.5' = mean(prec_freq_0.5, na.rm = TRUE)), by = .(hour(time_lst), name, location)]

mean_24h_landocn <- melt(mean_24h_landocn, c("hour", "name", "location"), variable = "threshold", value = "mean_value")

ggplot(mean_24h_landocn, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_grid(threshold~location, scales = "free_y") + 
  labs(x ="Hour (LST)", y = "Frequency (%)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06b_24hlineplot_freq_thres_0.1_0.5_landocn_diff_y_scale.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


##for seasons land and ocean
# mean_list_landocn_seas <- lapply(mean_list, function(dt) {
#   dt[, .(mean_value = mean(prec_freq, na.rm = TRUE)), by = .(hour(time_lst), name, location, season)]
# })

mean_24h_landocn_seas <- data_dt[, .('0.1' = mean(prec_freq, na.rm = TRUE), 
                                     '0.5' = mean(prec_freq_0.5, na.rm = TRUE)), by = .(hour(time_lst), name, location, season)]

mean_24h_landocn_seas <- melt(mean_24h_landocn_seas, c("hour", "name", "location", "season"), variable = "threshold", value = "mean_value")


ggplot(mean_24h_landocn_seas, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_grid(threshold~location~season) + 
  labs(x ="Hour (LST)", y = "Frequency (%)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06b_24hlineplot_freq_thres_0.1_0.5_landocn_seas.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


### Estimate the peak hour of data.tables -------------------------------------------

system.time(peak_hour_dt <- data_dt[, .SD[which.max(prec_freq_0.5)], by = .(lat, lon, name)])
# user  system elapsed 
# 488.337   2.325 362.277 
saveRDS(peak_hour_dt, "./projects/main/data/freq_thres_0.5_peak_hour_dt_2001_20.RDS")

peak_hour_dt <- readRDS("./projects/main/data/freq_thres_0.5_peak_hour_dt_2001_20.RDS")
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

ggsave("./projects/main/results/06b_plot_spat_peak_hour_freq_thres_0.5.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)

ggplot(peak_hour_dt) + 
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

ggsave("./projects/main/results/06b_plot_spat_peak_hour_freq_thres_0.5_2.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)


### seasonal

system.time(peak_hour_seas_dt <- data_dt[, .SD[which.max(prec_freq_0.5)], by = .(lat, lon, name, season)])
# user  system elapsed 
# 865.650   2.789 729.145 

saveRDS(peak_hour_seas_dt, "./projects/main/data/freq_thres_0.5_peak_hour_seas_dt_2001_20.RDS")
#peak_hour_seas_dt <- readRDS("./projects/main/data/freq_thres_0.5_peak_hour_seas_dt_2001_20.RDS")

peak_hour_seas_dt[, `:=`(peak_hour = hour(time_lst))]

peak_hour_seas_dt[peak_hour  == "15" | peak_hour  == "16" | peak_hour  == "17" | peak_hour  == "18", peak_hour2 := '15-18']
peak_hour_seas_dt[peak_hour  == "19" | peak_hour  == "20" | peak_hour  == "21" | peak_hour  == "22", peak_hour2 := '19-22']
peak_hour_seas_dt[peak_hour  == "23" | peak_hour  == "0" | peak_hour  == "1" | peak_hour  == "2", peak_hour2 := '23-02']
peak_hour_seas_dt[peak_hour  == "3" | peak_hour  == "4" | peak_hour  == "5" | peak_hour  == "6", peak_hour2 := '03-06']
peak_hour_seas_dt[peak_hour  == "7" | peak_hour  == "8" | peak_hour  == "9" | peak_hour  == "10", peak_hour2 := '07-10']
peak_hour_seas_dt[peak_hour  == "11" | peak_hour  == "12" | peak_hour  == "13" | peak_hour  == "14", peak_hour2 := '11-14']



library(forcats)
ggplot(peak_hour_seas_dt) + 
  geom_raster(aes(lon, lat, fill = factor(time_lst))) +
  scale_fill_manual(values = rainbow(48)) +
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

ggsave("./projects/main/results/06b_plot_spat_peak_hour_freq_thres_0.5_season.png", width = 10.5, height = 7.9, 
       units = "in", dpi = 600)



ggplot(peak_hour_seas_dt) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour2))) +
  scale_fill_manual(values = c("#FFCC66", "#9999CC", "#33CCCC", "#99CC00", "#F0810F", "#CC3333")) + 
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

ggsave("./projects/main/results/06b_plot_spat_peak_hour_freq_thres_0.5_season_2.png", width = 10.5, height = 7.9, 
       units = "in", dpi = 600)
###############################################################
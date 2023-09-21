
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
source('./source/graphics.R')


## read the data sets -------------------------------

# #dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_freq_thres_list.RDS")
# data_list <- readRDS("./projects/main/data/hourly_freq_all_datasets_LST_glob_2001_20_seasonal.rds")
# data_list1 <- readRDS("./projects/main/data/hourly_freq_thres_0.5_all_datasets_LST_glob_2001_20_seasonal.rds")
# 

merged_list <- lapply(data_list, function(dataset) merge(dataset, rbindlist(data_list1), 
                                                         by = c("lat", "lon", "time_utc", "name", "season", 
                                                                "location", "tmz_offset", "time_lst")))

saveRDS(merged_list, "./projects/main/data/hourly_freq_thres_0.1_0.5_all_datasets_LST_glob_2001_20_seasonal.rds")

#restart and read the dataset again to save memory

data_list <-  readRDS("./projects/main/data/hourly_freq_thres_0.1_0.5_all_datasets_LST_glob_2001_20_seasonal.rds")

desired_order <- c("imerg", "gsmap", "cmorph", "persiann", "era5")

# Use reorder to reorder the list elements
data_list <- data_list[desired_order]

## Pre-process ----------------------------------------------
### spatial mean plot in robinson projection ----------------------------------

mean_data_list <- lapply(data_list, function(df) df[, .(prec_freq = round(mean(prec_freq, na.rm = FALSE), 2)), by = .(lat, lon, threshold, season, name)])

#extracted_data_list <- lapply(mean_data_list, function(df) df[, c("lon", "lat", "prec_freq_0.1", "prec_freq_0.5")])

# Use lapply to create a list of rasters
raster_list <- lapply(extracted_data_list, create_raster)
#raster_brick <- brick(raster_list)

# Assuming your list is named raster_list
new_names <- c("imerg_jja", "imerg_djf", "gsmap_jja", "gsmap_djf", "cmorph_jja", "cmorph_djf", "persiann_jja", "persiann_djf", "era5_jja", "era5_djf")
names(raster_list) <- new_names

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#rastlist_robin <- projectRaster(raster_brick, crs = PROJ)

rastlist_robin <- lapply(raster_list, function(raster_object) {
  return(projectRaster(raster_object, crs = PROJ))
})

# Convert spatial data to data frame 
rast_robin_sp <- lapply(rastlist_robin, function(raster_object) {
  return(as(raster_object, "SpatialPixelsDataFrame"))
})  

rast_robin_df <- lapply(rast_robin_sp, function(raster_object) {
  return(as.data.frame(raster_object) %>% as.data.table())
})

# Get the list names
list_names <- names(rast_robin_df)

# Iterate through the list and add the 'name' column
for (i in seq_along(rast_robin_df)) {
  rast_robin_df[[i]]$season <- substr(list_names[i], nchar(list_names[i]) - 2, nchar(list_names[i]))
  rast_robin_df[[i]]$name <- sub(paste0("_", rast_robin_df[[i]]$season, "$"), "", list_names[i])
}

rast_robin_dt <- rbindlist(rast_robin_df)

to_plot <- melt(rast_robin_dt, c("x", "y", "name", "season"), variable.name = "threshold")
to_plot <- to_plot[, .(x, y, name = factor(name), season = factor(season), threshold, value = round(value, 2))]

#Define the desired order of levels
dataset_desired_order <- c("imerg", "gsmap", "cmorph", "persiann", "era5")
season_desired_order <- c("jja", "djf")

# Reorder the levels of the "name" column
to_plot$name <- factor(to_plot$name, levels = dataset_desired_order)
to_plot$season <- factor(to_plot$season, levels = season_desired_order)

levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot$season) <- c("JJA", "DJF")
levels(to_plot$threshold) <- c("0.1 (mm/hr)", "0.5 (mm/hr)")

to_plot[value < 0] #soame negative values generated in the projectraster()
to_plot[value < 0, value := 0]

ggplot() + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot, aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_grid(season~threshold~name) + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1,
                    breaks = c(1, 5, 10, 15, 20, 25, 30, 40, 50, 60), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Frequency (%)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_small +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25,"cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center") +
  theme(strip.background = element_blank(), panel.border=element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  guides(fill=guide_coloursteps(title.position="top"))


ggsave("./projects/main/results/06b_spat_freq_thres_0.1_0.5_seasonal_robin.png", width = 11.5, height = 5.3, 
       units = "in", dpi = 600)

### spatial mean plot --------------------------------------


spat_mean_dt <- data_list[, .(prec_freq = round(mean(prec_freq, na.rm = TRUE), 2), 
                            '0.2' = round(mean(prec_freq_0.2, na.rm = TRUE), 2), 
                            '0.5' = round(mean(prec_freq_0.5, na.rm = TRUE), 2)), by = .(lat, lon, name, season)]

summary(spat_mean_dt)
spat_mean_dt <- rbindlist(mean_data_list)
summary(spat_mean_dt)

spat_mean_dt <- melt(spat_mean_dt, c("lat", "lon", "name", "season"), variable.name = "threshold")

ggplot(spat_mean_dt) + 
  geom_raster(aes(lon, lat, fill = prec_freq)) +
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(1, 5, 10, 15, 20, 25, 30, 35, 40, 50, 60), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_dt$lon), max(spat_mean_dt$lon)), 
                  ylim = c(min(spat_mean_dt$lat), max(spat_mean_dt$lat))) + 
  facet_grid(season~threshold~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Frequency\n  \n (%)") + 
  #facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")) + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(1.0, 'cm'))

ggsave("./projects/main/results/06b_spat_freq_thres_0.1_0.5_seasonal.png", width = 11.5, height = 5.3, 
       units = "in", dpi = 600)



### 24hr diurnal cycle line plot ---------------------------------------------------------------------


data_dt <- rbindlist(data_list)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$season) <- c("JJA", "DJF")
levels(data_dt$location) <- c("Land", "Ocean")

## for glob

mean_24h_glob_list <- lapply(data_list, function(df) df[, .(prec_freq = round(mean(prec_freq, na.rm = TRUE), 2)), by = .(hour(time_lst), threshold, season, name)])

mean_24h_glob_dt <- rbindlist(mean_24h_glob_list)

to_plot <- mean_24h_glob_dt

levels(to_plot$threshold) <- c("0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot$season) <- c("JJA", "DJF")
#levels(mean_24h_glob_dt$location) <- c("Land", "Ocean")

ggplot(to_plot, aes(hour, prec_freq, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_grid(season~threshold) + 
  labs(x ="Hour (LST)", y = "Frequency (%)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06b_24hlineplot_freq_thres_0.1_0.5_glob_seasonal.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


    ##for seasons land and ocean and globe

mean_24h_landocn <- lapply(data_list, function(df) df[, .(prec_freq = round(mean(prec_freq, na.rm = TRUE), 2)), by = .(hour(time_lst), threshold, season, name, location)])

mean_24h_glob_2 <- mean_24h_glob_dt[, .(hour, threshold, name, location = factor("Global"), season, prec_freq)]


to_plot <- rbind(mean_24h_glob_2, rbindlist(mean_24h_landocn))

levels(to_plot$threshold) <- c("0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot$season) <- c("JJA", "DJF")
levels(to_plot$location) <- c("Global", "Land", "Ocean")

ggplot(to_plot, aes(hour, prec_freq, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_grid(season~threshold~location) + 
  labs(x ="Hour (LST)", y = "Frequency (%)") + 
  theme_small + 
  theme(legend.title = element_blank(), legend.position = "right", legend.direction = "vertical", strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06b_24hlineplot_freq_thres_0.1_0.5_landocnglob_seasonal.png",
       width = 9.9, height = 5.8, units = "in", dpi = 600)


### Estimate the peak hour of data.tables -------------------------------------------

system.time(peak_hour_dt <- data_dt[, .SD[which.max(prec_freq_0.5)], by = .(lat, lon, name, season)])
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

ggsave("./projects/main/results/06b_plot_spat_peak_hour_freq_thres_0.5_seasonal_JJA.png", width = 10.5, height = 6.9, 
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

ggsave("./projects/main/results/06b_plot_spat_peak_hour_freq_thres_0.5_seasonal_DJF.png", width = 10.5, height = 6.9, 
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

ggsave("./projects/main/results/06b_plot_spat_peak_hour_freq_thres_0.5_seasonal_JJA_clasfy.png", width = 10.5, height = 6.9, 
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

ggsave("./projects/main/results/06b_plot_spat_peak_hour_freq_thres_0.5_seasonal_DJF_clasfy.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)
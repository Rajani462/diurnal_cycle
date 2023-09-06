
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(RColorBrewer)
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

data_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20_seasonal.rds")

data_dt <- rbindlist(data_list)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$season) <- c("JJA", "DJF")
levels(data_dt$location) <- c("Land", "Ocean")

## Pre-process ----------------------------------------------

### spatial mean plot --------------------------------------

### spatial mean plot --------------------------------------
mean_data_list <- lapply(data_list, function(df) df[, .(mean_value = round(mean(prec_mean, na.rm = FALSE), 2)), by = .(lat, lon, season, name)])

extracted_data_list <- lapply(mean_data_list, function(df) df[, c("lon", "lat", "mean_value")])

# Use lapply to create a list of rasters
raster_list <- lapply(extracted_data_list, create_raster)
raster_brick <- brick(raster_list)

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_brick, crs = PROJ)

# Convert spatial data to data frame
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame")
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()

to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
to_plot <- to_plot[, .(x, y, value = round(value, 2), name)]

library(tidyr)
to_plot <- separate(to_plot, col = name, into = c("name", "season"), sep = "\\.") %>% 
  as.data.table() 

str(to_plot)

to_plot <- to_plot[, .(x, y, value, name = factor(name), season= factor(season))]

to_plot$season <- factor(to_plot$season, levels = c("1", "2"), labels = c("JJA", "DJF"))

#Define the desired order of levels
desired_order <- c("imerg", "gsmap", "cmorph", "persiann", "era5")

# Reorder the levels of the "name" column
to_plot$name <- factor(to_plot$name, levels = desired_order)

levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
summary(to_plot)

ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot, aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_grid(season~name) + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1,
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.5, 1, 1.5, 2), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Mean (mm/hr)") + 
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


ggsave("./projects/main/results/06a_spat_mean_seasonal.png", width = 11.5, height = 5.3, 
       units = "in", dpi = 600)


### 24hr diurnal cycle line plot ---------------------------------------------------------------------

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

ggsave("./projects/main/results/06a_24hlineplot_mean_glob_seasonal.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


##for seasons land and ocean

mean_24h_landocn_seas <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, location, season)]

ggplot(mean_24h_landocn_seas, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_grid(location~season) + 
  labs(x ="Hour (LST)", y = "Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06a_24hlineplot_mean_landocn_seasonal.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


## for land, ocean and Global

mean_24h_landocn <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, location, season)]
mean_24h_glob_seas2 <- mean_24h_glob_seas[, .(hour, name, location = factor("Global"), season, mean_value)]

land_ocn_glob_seas <- rbind(mean_24h_glob_seas2, mean_24h_landocn)
# levels(land_ocn_glob$location) <- c("Global", "Ocean", "Land")

ggplot(land_ocn_glob_seas, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_grid(location~season) + 
  labs(x ="Hour (LST)", y = "Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), legend.position = "bottom", strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06a_24hlineplot_mean_landocnglob_seasonal.png",
       width = 8.9, height = 4.6, units = "in", dpi = 600)


### Estimate the peak hour of data.tables -------------------------------------------

#system.time(peak_hour_dt <- data_dt[, .SD[which.max(prec_mean)], by = .(lat, lon, name, season)])
# user  system elapsed 
# 823.215   3.262 694.131 

system.time(peak_hour_list <- lapply(data_list, function(df) {
  df[, .SD[which.max(prec_mean)], by = .(lat, lon, name, season)]
}))

saveRDS(peak_hour_dt_list, "./projects/main/data/mean_peak_hour_dt_2001_20_seasonal.RDS")

peak_hour_dt_list <- readRDS("./projects/main/data/mean_peak_hour_dt_2001_20_seasonal.RDS")

peak_hour_dt <- rbindlist(peak_hour_dt_list)

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
  facet_grid(season~name) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/06a_plot_spat_peak_hour_mean_seasonal.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)

ggplot(peak_hour_dt) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour2))) + 
  # scale_fill_manual(values = c("#EBB582", "#FFCC66", "#F0810F", "#CC3333",
  #                                       "#ACBD78",  "#739F3D", "#99CC00", "#009999")) + 
  scale_fill_manual(values = c("#FFCC66", "#9999CC", "#33CCCC", "#99CC00", "#F0810F", "#CC3333")) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_grid(season~name) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/06a_plot_spat_peak_hour_mean_seasonal_clasfy.png", width = 10.5, height = 6.9, 
       units = "in", dpi = 600)

####################################################

# in Robinson projection --------------------------------------------------


system.time(peak_hour_list <- lapply(data_list, function(df) {
  df[, .SD[which.max(prec_mean)], by = .(lat, lon, name, season)]
}))

saveRDS(peak_hour_list, "./projects/main/data/mean_peak_hour_dt_2001_20_seasonal.RDS")

extracted_data_list <- lapply(peak_hour_list, function(df) {
  df[, c("lon", "lat", "time_lst")][, time_lst := (hour(time_lst))]
})

# Use lapply to create a list of rasters
raster_list <- lapply(extracted_data_list, create_raster)
raster_brick <- brick(raster_list)

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_brick, crs = PROJ, method="ngb")

# Convert spatial data to data frame
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame")
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()

summary(rast_robin_df)

to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
#to_plot <- to_plot[, .(x, y, value = round(value, 2), name)]

library(tidyr)
to_plot <- separate(to_plot, col = name, into = c("name", "season"), sep = "\\.") %>% 
  as.data.table() 

str(to_plot)

to_plot <- to_plot[, .(x, y, value, name = factor(name), season= factor(season))]

to_plot$season <- factor(to_plot$season, levels = c("1", "2"), labels = c("JJA", "DJF"))

#Define the desired order of levels
desired_order <- c("imerg", "gsmap", "cmorph", "persiann", "era5")

# Reorder the levels of the "name" column
to_plot$name <- factor(to_plot$name, levels = desired_order)

levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
summary(to_plot)

ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot, aes(x = x, y = y, fill = value), alpha = 1) + 
  #scale_fill_manual(values = rainbow(24)) + 
  
  scale_fill_stepsn(colours = brewer.pal(8,"Spectral"),
                    breaks = c(3, 6, 9, 12, 15, 18, 21), show.limits = TRUE) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) + 
  facet_grid(season~name) + 
  theme_small +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.width = unit(1.9, "cm"),
        legend.key.height = unit(0.5, "cm"), 
        legend.spacing = unit(0.1,"cm"),
        legend.text = element_text(size = 8), 
        legend.title = element_text(hjust = 0.5, size = 8),
        legend.justification = "center") +
  theme(strip.background = element_blank(), panel.border=element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  # guides(fill = guide_legend(nrow = 1, label.position = "bottom", title.position="top"))
  guides(fill=guide_coloursteps(direction = "horizontal", title.position="top", label.position = "bottom")) 


ggsave("./projects/main/results/06a_plot_spat_peak_hour_mean_seasonal_robin.png", width = 11.5, height = 5.3, 
       units = "in", dpi = 600)

####################################################################################
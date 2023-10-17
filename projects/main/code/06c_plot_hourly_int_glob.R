
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

#dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_int_thres_list.RDS")
data_list <- readRDS("./projects/main/data/hourly_int_all_datasets_LST_glob_2001_20.rds")

lapply(data_list, summary)

data_dt <- rbindlist(data_list)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$location) <- c("Land", "Ocean")

## Pre-process ----------------------------------------------

### spatial mean plot --------------------------------------
mean_data_list <- lapply(data_list, function(df) df[lat >= -58.875 & lat <= 58.875, .(mean_value = round(mean(prec_int, na.rm = TRUE), 2)), by = .(lat, lon, name)])

extracted_data_list <- lapply(mean_data_list, function(df) df[, c("lon", "lat", "mean_value")])

lapply(extracted_data_list, summary)

# Use lapply to create a list of rasters
raster_list <- lapply(extracted_data_list, create_raster)
raster_brick <- brick(raster_list)


PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_brick, crs = PROJ, method = "ngb")

# Convert spatial data to data frame
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame")
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()

to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
to_plot <- to_plot[, .(x, y, value = round(value, 2), name)]

levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
summary(to_plot)
to_plot[name == "PERSIANN" & value > 100]
to_plot[value > 20]
to_plot[value > 15]
to_plot[value > 11]
to_plot[value > 7]

ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot[value < 11], aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1,
                    breaks = c(0.3, 0.6, 0.9, 1.2, 1.5, 2, 2.5, 3, 4, 5, 7), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Intensity (mm/hr)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_small +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(3.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.35,"cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center") +
  theme(strip.background = element_blank(), panel.border=element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  guides(fill=guide_coloursteps(title.position="top"))

ggsave("./projects/main/results/06c_spat_int.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)


### 24hr diurnal cycle line plot ---------------------------------------------------------------------

## for glob

mean_24h_glob <- data_dt[, .(mean_value = mean(prec_int, na.rm = TRUE)), by = .(hour(time_lst), name)]

ggplot(mean_24h_glob, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  #facet_wrap(~location) + 
  labs(x ="Hour (LST)", y = "Frequency (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06c_24hlineplot_int_glob.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


##for land and ocean
mean_24h_landocn <- data_dt[, .(mean_value = mean(prec_int, na.rm = TRUE)), by = .(hour(time_lst), name, location)]

ggplot(mean_24h_landocn, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~location) + 
  labs(x ="Hour (LST)", y = "Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06c_24hlineplot_int_landocn.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


## for land, ocean and global

mean_24h_landocn <- data_dt[, .(mean_value = mean(prec_int, na.rm = TRUE)), by = .(hour(time_lst), name, location)]
mean_24h_glob2 <- mean_24h_glob[, .(hour, name, location = factor("Global"), mean_value)]

land_ocn_glob <- rbind(mean_24h_glob2, mean_24h_landocn)
# levels(land_ocn_glob$location) <- c("Global", "Ocean", "Land")

ggplot(land_ocn_glob, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~location) + 
  labs(x ="Hour (LST)", y = "Intensity (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), legend.position = "bottom", strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06c_24hlineplot_int_landocnglob.png",
       width = 9.6, height = 4.3, units = "in", dpi = 600)

ggplot(land_ocn_glob, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~location, ncol = 1) + 
  labs(x ="Hour (LST)", y = "Intensity (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), legend.position = "right", legend.direction = "vertical", strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06c_24hlineplot_int_landocnglob_2.png",
       width = 7.6, height = 5.3, units = "in", dpi = 600)

### Estimate the peak hour of data.tables -------------------------------------------

extracted_data_list <- lapply(data_list, function(df) df[, c("lon", "lat", "prec_int", "time_lst", "name")])
str(extracted_data_list)
lapply(extracted_data_list, summary)


# system.time(peak_hour_list <- lapply(extracted_data_list, function(df) {
#   df[, .SD[which(prec_int == max(prec_int, na.rm = FALSE))], by = .(lat, lon)]
# }))

system.time(peak_hour_list <- lapply(extracted_data_list, function(df) {
  df[, .SD[which.max(prec_int)], by = .(lon, lat, name)]
}))
# user  system elapsed 
# 412.915   4.548 342.563 

lapply(peak_hour_list, summary)

saveRDS(peak_hour_list, "./projects/main/data/int_peak_hour_dt_2001_20.RDS")

#######################################

peak_hour_list <- readRDS("./projects/main/data/int_peak_hour_dt_2001_20.RDS")

peak_hour_list <- lapply(peak_hour_list, function(df) {
  df[, c("lon", "lat", "time_lst", "name")][, time_lst := (hour(time_lst))]
})


imerg <- peak_hour_list$imerg

#Filling missing pixels (lat, lon) with NA's
peak_hour_list$gsmap <- rbind(peak_hour_list$gsmap, imerg)
peak_hour_list$gsmap <- unique(peak_hour_list$gsmap, by = c("lat", "lon"))
peak_hour_list$gsmap <- peak_hour_list$gsmap[name == "imerg", time_lst := NA]
peak_hour_list$gsmap[is.na(peak_hour_list$gsmap$time_lsts), "name"] <- "gsmap"

peak_hour_list$persiann <- rbind(peak_hour_list$persiann, imerg)
peak_hour_list$persiann <- unique(peak_hour_list$persiann, by = c("lat", "lon"))
peak_hour_list$persiann <- peak_hour_list$persiann[name == "imerg", time_lst := NA]
peak_hour_list$persiann[is.na(peak_hour_list$persiann$time_lsts), "name"] <- "persiann"

peak_hour_list$cmorph <- rbind(peak_hour_list$cmorph, imerg)
peak_hour_list$cmorph <- unique(peak_hour_list$cmorph, by = c("lat", "lon"))
peak_hour_list$cmorph <- peak_hour_list$cmorph[name == "imerg", time_lsts := NA]
peak_hour_list$cmorph[is.na(peak_hour_list$cmorph$time_lsts), "name"] <- "cmorph"

extracted_data_list <- lapply(peak_hour_list, function(df) df[, c("lon", "lat", "time_lst")])

# Use lapply to create a list of rasters
raster_list <- lapply(extracted_data_list, create_raster)
raster_brick <- brick(raster_list)

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_brick, crs = PROJ, method="ngb")

# Convert spatial data to data frame
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame")
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()

to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
to_plot2 <- to_plot[, .(x, y, peak_hour = value), name]

levels(to_plot2$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")


# library(RColorBrewer)
# my_colors <- c("red2", "sandybrown", "yellow2", "palegreen1", "lightseagreen","steelblue4", "sienna2", "red3")


ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot2, aes(x = x, y = y, fill = peak_hour), alpha = 1) + 
  scale_fill_manual(values = rainbow(24)) + scale_fill_gradientn(colours = c("blue", "red", "yellow", "green", "blue"),  
  breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) +
  # scale_fill_stepsn(colours = my_colors,
  #                   breaks = c(3, 6, 9, 12, 15, 18, 21), show.limits = TRUE) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
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
  guides(fill=guide_colourbar(direction = "horizontal", title.position="top", label.position = "bottom")) 


ggsave("./projects/main/results/06c_plot_spat_peak_hour_int.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)


###############################################################
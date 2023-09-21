
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(RColorBrewer)
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

data_list <- readRDS("./projects/main/data/hourly_int_all_datasets_LST_glob_2001_20_seasonal.rds")

## Pre-process ----------------------------------------------

### spatial mean plot --------------------------------------


mean_data_list <- lapply(data_list, function(df) df[, .(mean_value = round(mean(prec_int, na.rm = TRUE), 2)), by = .(lat, lon, season, name)])

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
hist(to_plot$value)
to_plot[value < 0]
to_plot[value >= 50]

ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot[value > 0], aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_grid(season~name) + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1,
                    breaks = c(0.3, 0.6, 0.9, 1.2, 1.5, 2, 2.5, 3, 4, 5, 7, 10), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Intensity (mm/hr)") + 
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


ggsave("./projects/main/results/06c_spat_int_seasonal.png", width = 11.5, height = 5.3, 
       units = "in", dpi = 600)



### 24hr diurnal cycle line plot ---------------------------------------------------------------------

## Pre-process ----------------------------------------------
data_dt <- rbindlist(data_list)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$season) <- c("JJA", "DJF")
levels(data_dt$location) <- c("Land", "Ocean")

## for glob seasons

mean_24h_glob_seas <- data_dt[, .(mean_value = mean(prec_int, na.rm = TRUE)), by = .(hour(time_lst), name, season)]

ggplot(mean_24h_glob_seas, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~season) + 
  labs(x ="Hour (LST)", y = "Intensity (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06c_24hlineplot_int_glob_seasonal.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


##for seasons land and ocean

mean_24h_landocn_seas <- data_dt[, .(mean_value = mean(prec_int, na.rm = TRUE)), by = .(hour(time_lst), name, location, season)]

ggplot(mean_24h_landocn_seas, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_grid(location~season) + 
  labs(x ="Hour (LST)", y = "Intensity (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06c_24hlineplot_int_landocn_seasonal.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

## for land, ocean and Global

mean_24h_landocn <- data_dt[, .(mean_value = mean(prec_int, na.rm = TRUE)), by = .(hour(time_lst), name, location, season)]
mean_24h_glob_seas2 <- mean_24h_glob_seas[, .(hour, name, location = factor("Global"), season, mean_value)]

land_ocn_glob_seas <- rbind(mean_24h_glob_seas2, mean_24h_landocn)
# levels(land_ocn_glob$location) <- c("Global", "Ocean", "Land")

ggplot(land_ocn_glob_seas, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_grid(location~season) + 
  labs(x ="Hour (LST)", y = "Intensity (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), legend.position = "bottom", strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06c_24hlineplot_int_landocnglob_seasonal.png",
       width = 8.9, height = 4.6, units = "in", dpi = 600)


# ### Estimate the peak hour of data.tables -------------------------------------------

system.time(peak_hour_list <- lapply(data_list, function(df) {
  df[, .SD[which.max(prec_int)], by = .(lat, lon, name, season)]
}))

saveRDS(peak_hour_list, "./projects/main/data/int_peak_hour_dt_2001_20_seasonal.RDS")

####
# in Robinson projection -------------------------------------------------

peak_hour_list <- readRDS("./projects/main/data/int_peak_hour_dt_2001_20_seasonal.RDS")

data_list <- lapply(peak_hour_list, function(df) {
  df[, c("lon", "lat", "time_lst", "name", "season")][, time_lst := (hour(time_lst))]
})

names(data_list) <- c("imerg_jja", "imerg_djf", "gsmap_jja", "gsmap_djf", 
                                "cmorph_jja", "cmorph_djf", "persiann_jja", "persiann_djf", 
                                "era5_jja", "era5_djf")


#Filling missing pixels (lat, lon) with NA'
imerg <- data_list$imerg_jja

data_list$persiann_jja <- rbind(data_list$persiann_jja, imerg)
data_list$persiann_jja <- unique(data_list$persiann_jja, by = c("lat", "lon"))
data_list$persiann_jja <- data_list$persiann_jja[name == "imerg", time_lst := NA]
data_list$persiann_jja[is.na(data_list$persiann_jja$time_lsts), "name"] <- "persiann"

#imerg <- data_list$imerg_jja

data_list$persiann_djf <- rbind(data_list$persiann_djf, imerg)
data_list$persiann_djf <- unique(data_list$persiann_djf, by = c("lat", "lon"))
data_list$persiann_djf <- data_list$persiann_djf[name == "imerg", time_lst := NA]
data_list$persiann_djf[is.na(data_list$persiann_djf$time_lsts), "name"] <- "persiann"

extracted_data_list <- lapply(data_list, function(df) df[, c("lon", "lat", "time_lst")])

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

to_plot <- separate(to_plot, col = name, into = c("name", "season"), sep = "_") %>% 
  as.data.table() 

str(to_plot)
to_plot <- to_plot[, .(x, y, value, name = factor(name), season= factor(season))]
str(to_plot)


levels(to_plot$name) 
#Define the desired order of levels
desired_order <- c("imerg", "gsmap", "cmorph", "persiann", "era5")
desired_order_season <- c("jja", "djf")

# Reorder the levels of the "name" column
to_plot$name <- factor(to_plot$name, levels = desired_order)
to_plot$season <- factor(to_plot$season, levels = desired_order_season)

levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot$season) <- c("JJA", "DJF")
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


ggsave("./projects/main/results/06c_plot_spat_peak_hour_int_seasonal_robin.png", width = 11.5, height = 5.3, 
       units = "in", dpi = 600)

################################################
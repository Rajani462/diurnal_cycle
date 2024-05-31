
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(ncdf4)
library(sf)
library(sp)
library(hms)
library(forcats)
library(parallel)
library(RColorBrewer)

#source('./source/libs.R')
source('source/themes.R')
source('source/palettes.R')
source('source/graphics.R')


## read the data sets -------------------------------
data_list <- readRDS("~/rajani/diurnal_cycle/projects/main/reproduce/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")

## Pre-process ----------------------------------------------
mean_data_list <- lapply(data_list, function(df) df[, .(mean_value = round(mean(prec_mean, na.rm = TRUE), 2)), by = .(lat, lon, name)])
extracted_data_list <- lapply(mean_data_list, function(df) df[, c("lon", "lat", "mean_value")])
raster_list <- lapply(extracted_data_list, create_raster) # Use lapply to create a list of rasters
raster_brick <- brick(raster_list)
PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_brick, crs = PROJ)
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame") # Convert spatial data to data frame
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()
to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
to_plot <- to_plot[, .(x, y, value = round(value, 2), name)]
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

### spatial mean plot --------------------------------------
ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot, aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1,
                    breaks = c(0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Amount (mm/hr)") + 
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
        legend.justification = "center", 
        panel.grid = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), panel.spacing = unit(0, "lines")) + 
  guides(fill=guide_coloursteps(title.position="top"))

ggsave("~/rajani/diurnal_cycle/projects/main/reproduce/results/01_fig_spat_mean.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)

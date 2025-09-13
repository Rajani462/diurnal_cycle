
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
peak_hour_list <- readRDS("./projects/main/reproduce/data/int_peak_hour_dt_2001_20.RDS")

## pre-process
peak_hour_list <- lapply(peak_hour_list, function(df) {
  df[, c("lon", "lat", "time_utc", "time_lst", "name")][, time_lst := (hour(time_lst))]
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

# Function to convert UTC to local solar time and extract fractional hour
library(lubridate)
convert_to_lst <- function(dt) {
  dt[, time_lst := time_utc + dhours(lon / 15)]
  dt[, hour_lst := hour(time_lst) + minute(time_lst)/60 + second(time_lst)/3600]
  return(dt)
}

# Apply to each list element
extracted_data_list2 <- lapply(peak_hour_list, convert_to_lst)
extracted_data_list <- lapply(extracted_data_list2, function(df) df[, c("lon", "lat", "hour_lst")])

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

## plot
ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot2, aes(x = x, y = y, fill = peak_hour), alpha = 1) + 
  scale_fill_gradientn(colours = c("#e66101", "#ffffbf","#0571b0", "#4dac26", "#e66101"),  
                       breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_small +
  theme(plot.title = element_text(hjust = 0.3, size = 10, face = "bold"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.width = unit(1.9, "cm"),
        legend.key.height = unit(0.5, "cm"), 
        legend.spacing = unit(0.1,"cm"),
        legend.text = element_text(size = 10), 
        legend.title = element_text(hjust = 0.5, size = 10),
        legend.justification = "center", panel.grid = element_blank(),
        strip.background = element_blank(),
        panel.background = element_blank(),
        panel.border=element_blank(),
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) + 
  # guides(fill = guide_legend(nrow = 1, label.position = "bottom", title.position="top"))
  guides(fill=guide_colourbar(direction = "horizontal", title.position="top", label.position = "bottom")) 

ggsave("~/rajani/diurnal_cycle/projects/main/reproduce/results/07_fig_spat_peak_hour_freq.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)

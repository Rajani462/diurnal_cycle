library(gridExtra)
library(ggpubr)
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
#library(reshape)
#library(terra)
library(ncdf4)
library(sf)
library(sp)
library(hms)
library(forcats)
library(parallel)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('source/graphics.R')


## read the data sets -------------------------------

#dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_mean_thres_list.RDS")
data_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")


## Pre-process ----------------------------------------------
### spatial mean plot --------------------------------------
mean_data_list <- lapply(data_list, function(df) df[, .(mean_value = round(mean(prec_mean, na.rm = TRUE), 2)), by = .(lat, lon, name)])

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

levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
summary(to_plot)

hist(to_plot$value)
mean_plot <- ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot, aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1,
                    breaks = c(0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), show.limits = TRUE) + 
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


data_list <-  readRDS("./projects/main/data/hourly_mean_thres_0.1_0.5_all_datasets_LST_glob_2001_20.rds")


zonmean_data_list <- lapply(data_list, function(df) df[, .('no_threshold' = mean(prec_mean, na.rm = TRUE), 
                                                           '0.1' = mean(prec_mean_0.1, na.rm = TRUE), 
                                                           '0.2' = mean(prec_mean_0.2, na.rm = TRUE),
                                                           '0.5' = mean(prec_mean_0.5, na.rm = TRUE)), by = .(lat, name)])

zonmean_data <- rbindlist(zonmean_data_list)
to_plot <- melt(zonmean_data,  c("lat", "name"), variable.name = "threshold")


levels(to_plot$threshold) <- c("no threshold", "0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

#zonal_data_comb[precip > 0.4]
summary(to_plot)

##flpped x-y axix----

zon_mean <- ggplot(to_plot[threshold == "no threshold"], aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Mean (mm/hr)", col = " ") + 
  theme_small + 
  #facet_wrap(~threshold) + 
  coord_flip() + 
  scale_x_reverse(breaks = seq(-60, 60, by = 10), expand = c(0, 0)) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), legend.position = "right", legend.direction = "vertical") + 
  theme(legend.position = c(0.7, 0.2))


p <- ggarrange(mean_plot, zon_mean, nrow = 1, widths = c(2.8, 1), align = c("h"),
               labels = c("a)", "b)"), font.label=list(family = font, face = "plain", color = "#222222", size=12))


ggsave("./projects/main/results/08a_mean.png", p, width = 10.5, height = 5.1, 
       units = "in", dpi = 600)



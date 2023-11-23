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


int_plot <- ggplot() +
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
        legend.key.width = unit(3, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.35,"cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center") +
  theme(strip.background = element_blank(), panel.border=element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  guides(fill=guide_coloursteps(title.position="top"))


data_list <-  readRDS("./projects/main/data/hourly_int_thres_0.1_0.5_all_datasets_LST_glob_2001_20.rds")


zonmean_data_list <- lapply(data_list, function(df) df[, .('0.1' = mean(prec_int, na.rm = TRUE), 
                                                           '0.2' = mean(prec_int_0.2, na.rm = TRUE),
                                                           '0.5' = mean(prec_int_0.5, na.rm = TRUE)), by = .(lat, name)])

zonmean_data <- rbindlist(zonmean_data_list)
to_plot <- melt(zonmean_data,  c("lat", "name"), variable.name = "threshold")


levels(to_plot$threshold) <- c("0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

to_plot[value > 3]
summary(to_plot)
summary(to_plot[lat >= -58.875 & lat <= 58.875])

ggplot(to_plot[lat >= -58.875 & lat <= 58.875], aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Intensity (mm/hr)", col = " ") + 
  theme_generic + 
  facet_wrap(~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07b_zonal_int_lineplot_thres_0.1_0.5_glob.png",
       width = 9.0, height = 5.2, units = "in", dpi = 600)

##flpped x-y axix----

zon_int <- ggplot(to_plot[threshold == "0.1 (mm/hr)" & lat >= -58.875 & lat <= 58.875], aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Intensity (mm/hr)", col = " ") + 
  theme_small + 
  #facet_wrap(~threshold) + 
  coord_flip() + 
  scale_x_reverse(breaks = seq(-60, 60, by = 10), expand = c(0, 0)) +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), legend.position = "right", legend.direction = "vertical") + 
  theme(legend.position = c(0.8, 0.19))


p <- ggarrange(int_plot, zon_int, nrow = 1, widths = c(2.2, 1), align = c("h"),
                  labels = c("a)", "b)"), font.label=list(family = font, face = "plain", color = "#222222", size=12))

ggsave("./projects/main/results/08c_int.png", p, width = 10.5, height = 5.1, 
       units = "in", dpi = 600)



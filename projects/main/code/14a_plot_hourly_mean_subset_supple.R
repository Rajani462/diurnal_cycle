
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

peak_hour_list <- readRDS("./projects/main/data/mean_peak_hour_dt_2001_20.RDS")

#extracted_data_list <- lapply(peak_hour_list, function(df) df[, c("lon", "lat", "time_lst", "name")])


extracted_data_list <- lapply(peak_hour_list, function(df) {
  df[, c("lon", "lat", "time_lst", "name")][, time_lst := (hour(time_lst))]
})


# Define bounding box coordinates for each region
# (min_lon, max_lon, min_lat, max_lat)
india_bbox <- c(68, 120, 20, 40)   # Northern India
#china_bbox <- c(100, 125, 20, 45) # Eastern China
usa_bbox <- c(-105, -85, 25, 50)  # Great Plains of USA
amazon_bbox <- c(-78, -50, -40, -5) # Amazon region

# Create a function to filter data within a given bounding box
filter_bbox <- function(data, bbox) {
  filtered_data <- data[lon >= bbox[1] & lon <= bbox[2] & lat >= bbox[3] & lat <= bbox[4]]
  return(filtered_data)
}

# Extract data for each region
india_data <- lapply(extracted_data_list, function(x) filter_bbox(x, india_bbox))
#china_data <- lapply(extracted_data_list, function(x) filter_bbox(x, china_bbox))
usa_data <- lapply(extracted_data_list, function(x) filter_bbox(x, usa_bbox))
amazon_data <- lapply(extracted_data_list, function(x) filter_bbox(x, amazon_bbox))


india_dt <- rbindlist(india_data)
summary(india_dt)


ggplot(india_dt, aes(lon, lat, fill = time_lst)) + 
  geom_tile() + 
  coord_fixed(ratio = 1) + 
  scale_fill_gradientn(colours = c("wheat2", "firebrick", "gold3", "forestgreen", "wheat2"), 
                       breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  theme_generic + 
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.width = unit(1.9, "cm"),
        legend.key.height = unit(0.5, "cm"), 
        legend.spacing = unit(0.1,"cm"),
        legend.text = element_text(size = 8), 
        legend.title = element_text(hjust = 0.5, size = 8),
        legend.justification = "center", 
        panel.grid = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(), 
        #axis.ticks = element_blank(), 
        panel.spacing = unit(0, "lines")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) + 
  guides(fill=guide_colourbar(direction = "horizontal", title.position="top", label.position = "bottom"))


#USA greta plain

usa_dt <- rbindlist(usa_data)
summary(usa_dt)


ggplot(usa_dt, aes(lon, lat, fill = time_lst)) + 
  geom_tile() + 
  coord_fixed(ratio = 1) + 
  scale_fill_gradientn(colours = c("wheat2", "firebrick", "gold3", "forestgreen", "wheat2"), 
                       breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  theme_generic + 
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.width = unit(1.9, "cm"),
        legend.key.height = unit(0.5, "cm"), 
        legend.spacing = unit(0.1,"cm"),
        legend.text = element_text(size = 8), 
        legend.title = element_text(hjust = 0.5, size = 8),
        legend.justification = "center", 
        panel.grid = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(), 
        #axis.ticks = element_blank(), 
        panel.spacing = unit(0, "lines")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) + 
  guides(fill=guide_colourbar(direction = "horizontal", title.position="top", label.position = "bottom"))



#Amazon region

amazon_dt <- rbindlist(amazon_data)
summary(amazon_dt)


ggplot(amazon_dt, aes(lon, lat, fill = time_lst)) + 
  geom_tile() + 
  coord_fixed(ratio = 1) + 
  scale_fill_gradientn(colours = c("wheat2", "firebrick", "gold3", "forestgreen", "wheat2"), 
                       breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  theme_generic + 
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.width = unit(1.9, "cm"),
        legend.key.height = unit(0.5, "cm"), 
        legend.spacing = unit(0.1,"cm"),
        legend.text = element_text(size = 8), 
        legend.title = element_text(hjust = 0.5, size = 8),
        legend.justification = "center", 
        panel.grid = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        # axis.text.x = element_blank(),
        # axis.text.y = element_blank(), 
        #axis.ticks = element_blank(), 
        panel.spacing = unit(0, "lines")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) + 
  guides(fill=guide_colourbar(direction = "horizontal", title.position="top", label.position = "bottom"))


# ggsave("./projects/main/results/14a_supplot_mount_asia_spat_peak_hour_mean.png", width = 10.5, height = 5.1, 
#        units = "in", dpi = 600)


# Use lapply to create a list of rasters
india_data_list <- lapply(india_data, function(df) {
  df[, c("lon", "lat", "time_lst")]
})


raster_list <- lapply(india_data_list, create_raster)
raster_brick <- brick(raster_list)

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_brick, crs = PROJ, method="ngb")

# Convert spatial data to data frame
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame")
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()

to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
peak_hour_dt <- to_plot[, .(x, y, peak_hour = value), name]

levels(peak_hour_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

summary(peak_hour_dt)


# Define x and y limits based on summary statistics
x_min <- 5932101
x_max <- 11130401
y_min <- 2153702
y_max <- 4263002

ggplot() +
  # geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
  #              colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = peak_hour_dt, aes(x = x, y = y, fill = peak_hour), alpha = 1) + 
  scale_fill_gradientn(colours = c("#e66101", "#ffffbf","#0571b0", "#4dac26", "#e66101"),  breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  # geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
  #              colour = "black", fill = "transparent", size = 0.25, 
  #              xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  theme_generic +  # Remove axis labels and ticks
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.width = unit(1.9, "cm"),
        legend.key.height = unit(0.5, "cm"), 
        legend.spacing = unit(0.1,"cm"),
        legend.text = element_text(size = 8), 
        legend.title = element_text(hjust = 0.5, size = 8),
        legend.justification = "center", 
        panel.grid = element_blank(),
        strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), panel.spacing = unit(0.1, "lines")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) + 
  guides(fill=guide_colourbar(direction = "horizontal", title.position="top", label.position = "bottom")) + 
  xlim(x_min, x_max) +  # Limit x-axis
  ylim(y_min, y_max)    # Limit y-axis


ggsave("./projects/main/results/14a_supplot_mount_asia_spat_peak_hour_mean_robinsn.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)



# USA great palin ---------------------------------------------------------

# Use lapply to create a list of rasters
usa_data_list <- lapply(usa_data, function(df) {
  df[, c("lon", "lat", "time_lst")]
})


raster_list <- lapply(usa_data_list, create_raster)
raster_brick <- brick(raster_list)

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_brick, crs = PROJ, method="ngb")

# Convert spatial data to data frame
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame")
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()

to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
usa_peak_hour_dt <- to_plot[, .(x, y, peak_hour = value), name]

levels(usa_peak_hour_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

summary(usa_peak_hour_dt)


# Define x and y limits based on summary statistics
x_min <- min(usa_peak_hour_dt$x)
x_max <- max(usa_peak_hour_dt$x)
y_min <- min(usa_peak_hour_dt$y)
y_max <- max(usa_peak_hour_dt$y)

ggplot() +
  # geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
  #              colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = usa_peak_hour_dt, aes(x = x, y = y, fill = peak_hour), alpha = 1) + 
  scale_fill_gradientn(colours = c("#e66101", "#ffffbf","#0571b0", "#4dac26", "#e66101"),  breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  facet_wrap(~name, ncol = 5) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  # geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
  #              colour = "black", fill = "transparent", size = 0.25, 
  #              xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  theme_generic +  # Remove axis labels and ticks
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.width = unit(1.9, "cm"),
        legend.key.height = unit(0.5, "cm"), 
        legend.spacing = unit(0.1,"cm"),
        legend.text = element_text(size = 8), 
        legend.title = element_text(hjust = 0.5, size = 8),
        legend.justification = "center", 
        panel.grid = element_blank(),
        strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), panel.spacing = unit(0.1, "lines")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) + 
  guides(fill=guide_colourbar(direction = "horizontal", title.position="top", label.position = "bottom")) + 
  xlim(x_min, x_max) +  # Limit x-axis
  ylim(y_min, y_max)    # Limit y-axis


ggsave("./projects/main/results/14a_supplot_greatpalin_usa_spat_peak_hour_mean_robinsn.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)


# Amazon region ---------------------------------------------------------

# Use lapply to create a list of rasters
amazon_data_list <- lapply(amazon_data, function(df) {
  df[, c("lon", "lat", "time_lst")]
})


raster_list <- lapply(amazon_data_list, create_raster)
raster_brick <- brick(raster_list)

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_brick, crs = PROJ, method="ngb")

# Convert spatial data to data frame
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame")
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()

to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
amazon_peak_hour_dt <- to_plot[, .(x, y, peak_hour = value), name]

levels(amazon_peak_hour_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

summary(amazon_peak_hour_dt)


# Define x and y limits based on summary statistics
x_min <- min(amazon_peak_hour_dt$x)
x_max <- max(amazon_peak_hour_dt$x)
y_min <- min(amazon_peak_hour_dt$y)
y_max <- max(amazon_peak_hour_dt$y)

amz_plot <- ggplot() +
  # geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
  #              colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = amazon_peak_hour_dt, aes(x = x, y = y, fill = peak_hour), alpha = 1) + 
  scale_fill_gradientn(colours = c("#e66101", "#ffffbf","#0571b0", "#4dac26", "#e66101"),  breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  facet_wrap(~name, ncol = 5) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  # geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
  #              colour = "black", fill = "transparent", size = 0.25, 
  #              xlim = c(x_min, x_max), ylim = c(y_min, y_max)) +
  theme_generic +  # Remove axis labels and ticks
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.width = unit(1.9, "cm"),
        legend.key.height = unit(0.5, "cm"), 
        legend.spacing = unit(0.1,"cm"),
        legend.text = element_text(size = 8), 
        legend.title = element_text(hjust = 0.5, size = 8),
        legend.justification = "center", 
        panel.grid = element_blank(),
        strip.background = element_blank(),
        #panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), panel.spacing = unit(0.1, "lines")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) + 
  guides(fill=guide_colourbar(direction = "horizontal", title.position="top", label.position = "bottom")) + 
  xlim(x_min, x_max) +  # Limit x-axis
  ylim(y_min, y_max) +
  theme(legend.position = "none")


ggsave("./projects/main/results/14a_supplot_amazon_spat_peak_hour_mean_robinsn.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)

#################################################

# highlights athe above orgraphic preci regions ---------------------------

# peak_hour_list <- readRDS("./projects/main/data/mean_peak_hour_dt_2001_20.RDS")

extracted_data_list <- lapply(peak_hour_list, function(df) df[, c("lon", "lat", "time_lst")])


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

to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
peak_hour_dt <- to_plot[, .(x, y, peak_hour = value), name]

levels(peak_hour_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")


him_x_min <- 5932101
him_x_max <- 11130401
him_y_min <- 2153702
him_y_max <- 4263002

usa_x_min <- min(usa_peak_hour_dt$x)
usa_x_max <- max(usa_peak_hour_dt$x)
usa_y_min <- min(usa_peak_hour_dt$y)
usa_y_max <- max(usa_peak_hour_dt$y)

amz_x_min <- min(amazon_peak_hour_dt$x)
amz_x_max <- max(amazon_peak_hour_dt$x)
amz_y_min <- min(amazon_peak_hour_dt$y)
amz_y_max <- max(amazon_peak_hour_dt$y)


main_plot <- ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = peak_hour_dt[name == "IMERG"], aes(x = x, y = y, fill = peak_hour), alpha = 1) + 
  #scale_color_manual(values = rainbow(24)) + 
  #scale_color_manual(colours = c("red", "blue")) + 
  scale_fill_gradientn(colours = c("#e66101", "#ffffbf","#0571b0", "#4dac26", "#e66101"), 
                       breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  
  # scale_fill_stepsn(colours = (pals::kovesi.cyclic_mygbm_30_95_c78_s25),
  #                   breaks = c(3, 6, 9, 12, 15, 18, 21), show.limits = TRUE) + 
  # scale_fill_viridis(option = "magma", direction = 1,
  #                      breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  #facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) + 
  geom_rect(aes(xmin = him_x_min, xmax = him_x_max, ymin = him_y_min, ymax = him_y_max), 
            colour = "#f00505", fill = "transparent", size = 0.5) + 
  geom_rect(aes(xmin = amz_x_min, xmax = amz_x_max, ymin = amz_y_min, ymax = amz_y_max), 
            colour = "#f00505", fill = "transparent", size = 0.5) + 
  geom_rect(aes(xmin = usa_x_min, xmax = usa_x_max, ymin = usa_y_min, ymax = usa_y_max), 
            colour = "#f00505", fill = "transparent", size = 0.5) +
  theme_generic +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom", legend.direction = "horizontal",
        legend.key.width = unit(1.9, "cm"),
        legend.key.height = unit(0.5, "cm"), 
        legend.spacing = unit(0.1,"cm"),
        legend.text = element_text(size = 10), 
        legend.title = element_text(hjust = 0.5, size = 10),
        legend.justification = "center", 
        panel.grid = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), panel.spacing = unit(0, "lines")) +
  guides(fill = guide_colorbar(barwidth = 10, barheight = 0.5)) + 
  guides(fill=guide_colourbar(direction = "horizontal", title.position="top", label.position = "bottom"))

main_plot
ggsave("./projects/main/results/14a_plot_orographic_precip_mean.png", width = 7.5, height = 5.1, 
       units = "in", dpi = 600)















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
library("RColorBrewer")

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('source/graphics.R')


# read the datasets -------------------------------------------------------


peak_hour_list <- readRDS("./projects/main/data/mean_peak_hour_dt_2001_20.RDS")
extracted_data_list <- lapply(peak_hour_list, function(df) df[, c("lon", "lat", "time_lst", "name", "location")])

data_comb <- rbindlist(extracted_data_list)
data_comb$time_lst <- hour(data_comb$time_lst)
data_imerg <- data_comb[name == "imerg"]
result <- merge(data_comb, data_imerg, by = c("lat", "lon"))
setnames(result, "time_lst.y", "time_lst_imerg")
result[, `:=`(time_lst = time_lst.x)]

# Convert hours to datetime objects
# library(lubridate)
# common_date <- as.Date("2024-03-21")  # Choose any common date
# result$time_lst.x <- make_datetime(year(common_date), month(common_date), day(common_date), hour = result$time_lst.x)
# result[, `:=`(time_lst_imerg2 = time_lst_imerg)]
# result$time_lst_imerg <- make_datetime(year(common_date), month(common_date), day(common_date), hour = result$time_lst_imerg)
# 
# # Calculate time difference
# result$time_diff_lst <- difftime(result$time_lst.x, result$time_lst_imerg, units = "hours")
# 
# 
# 
# # Calculate time difference
# result$time_diff_lst <- difftime(result$time_lst_imerg, result$time_lst.x, units = "hours")



result[, time_diff_lst := time_lst.x - time_lst_imerg]
#View(result)

summary(result)
setnames(result, c("name.x", "location.x"), c("name", "location"))

<<<<<<< HEAD
##########histogram---------------------
=======
>>>>>>> 46dff6d04cacfa664d131384b2ba6441bd1dc628

to_plot <- result[, .(lat, lon, name, location, time_diff_lst)]

to_plot_glob <- to_plot[, .(lat, lon, name, location = factor(" global"), time_diff_lst)]

to_plot_glob_land_ocn <- rbind(to_plot_glob, to_plot)
#to_plot_glob_land_ocn <- to_plot_glob_land_ocn[name != "imerg"]

levels(to_plot_glob_land_ocn$name)
levels(to_plot_glob_land_ocn$location)
levels(to_plot_glob_land_ocn$name) <- c("imerg", "GSMaP - IMERG", "CMORPH - IMERG", "PERSIANN - IMERG", "ERA5 - IMERG")
levels(to_plot_glob_land_ocn$location) <- c("Global", "Land", "Ocean")


ggplot(to_plot_glob_land_ocn[name != "imerg"]) + 
  geom_histogram(aes(x = time_diff_lst), color = "black", fill = "white") +  # Specify color and fill aesthetics
  facet_grid(location~name) + 
  labs(x = "Difference in diurnal peak (h)", y = "Count") + 
  theme_small
  

#realtive plot
ggplot(to_plot_glob_land_ocn[name != "imerg"]) + 
  geom_histogram(aes(x = time_diff_lst, y = ..density..), color = "black", fill = "white") +  # Specify color and fill aesthetics
  facet_grid(location ~ name) + 
  labs(x = "Difference in diurnal peak (h)", y = "Density") + 
  theme_generic + 
  theme(strip.background = element_blank())

ggsave("./projects/main/results/11d_plot_hist_peak_hour_diff_mean.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)

ggplot(to_plot_glob_land_ocn[name != "imerg"]) + 
  geom_freqpoly(aes(x = time_diff_lst, color = name)) +  # Plot density line
  facet_grid(location ~ name) + 
  labs(x = "Difference in diurnal peak (h)", y = "Count") + 
  theme_small

ggplot(to_plot_glob_land_ocn[name != "imerg"]) + 
  geom_freqpoly(aes(x = time_diff_lst, y = ..density..), color = "black", fill = "white") +  # Specify color and fill aesthetics
  facet_grid(location ~ name) + 
  labs(x = "Difference in diurnal peak (h)", y = "Count") + 
  theme_small


<<<<<<< HEAD
# spatial plot ------------------------------------------------------------


=======
>>>>>>> 46dff6d04cacfa664d131384b2ba6441bd1dc628
result[time_diff_lst > 15]
peak_hour_list <- split(result, by = "name")

extracted_data_list <- lapply(peak_hour_list, function(df) df[, c("lon", "lat", "time_diff_lst", "name")])


extracted_data_list <- lapply(peak_hour_list, function(df) {
  df[, c("lon", "lat", "time_diff_lst")]
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

levels(peak_hour_dt$name)
levels(peak_hour_dt$name) <- c("IMERG", "GSMaP - IMERG", "CMORPH - IMERG", "PERSIANN - IMERG", "ERA5 - IMERG")


library(RColorBrewer)
summary(peak_hour_dt)

peak_hour_dt[peak_hour > 5]
#my_colors <- c("red2", "sandybrown", "yellow2", "palegreen1", "lightseagreen","steelblue2", "sienna2", "red3")
hist(peak_hour_dt$peak_hour)


ggplot(peak_hour_dt[name != "IMERG"]) + 
  geom_histogram(aes(x = peak_hour), color = "black", fill = "white") +  # Specify color and fill aesthetics
  facet_wrap(~name, ncol = 2) + 
  theme_small


ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = peak_hour_dt[name != "IMERG"], aes(x = x, y = y, fill = peak_hour), alpha = 1) + 
<<<<<<< HEAD
  scale_fill_gradientn(colors = c("#543005", "#8c510a", "#bf812d", "#FFFFFF", "#80cdc1", "#01665e", "#003c30"),
                       breaks=c(-8, -6, -4,-2, 0, 2, 4, 6, 8),
                       limits = c(-23, 23)) +
=======
  #scale_fill_gradientn(colours = c("blue", "red", "yellow", "green", "blue")) + 
  #scale_fill_gradient2(low = "#2D708EFF", mid = "white", high = "#FFA07AFF", midpoint = 0) + 
  #scale_fill_gradient2(low = "#2D708EFF", mid = "white", high = "#FFA07AFF", midpoint = 0, 
                       #breaks = seq(-23, 23, by = 3)) + 
  scale_fill_gradientn(colors = c("#543005", "#8c510a", "#bf812d", "#e0e0e0", "#80cdc1", "#01665e", "#003c30"),
                       breaks=c(-12, -10, -8, -6, -4,-2, 0, 2, 4, 6, 8, 10, 12),
                       limits = c(-23, 23)) +  
>>>>>>> 46dff6d04cacfa664d131384b2ba6441bd1dc628
                       #n.breaks = 5,
                       #breaks = custom_breaks,
                       # guide = guide_colorbar(barwidth = 10, barheight = 0.5,
                       #                        title.position = "top")) + 
  facet_wrap(~name, ncol = 2) + 
  labs(x = NULL, y = NULL, fill = "Difference in diurnal peak (h)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
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
        panel.background = element_blank(),
        panel.border=element_blank(),
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), panel.spacing = unit(0, "lines")) +  # Remove y-axis labels
  guides(fill=guide_coloursteps(title.position="top"))


ggsave("./projects/main/results/11d_plot_spat_peak_hour_diff_mean.png", width = 7.1, height = 5.1, 
       units = "in", dpi = 600)


<<<<<<< HEAD


=======
>>>>>>> 46dff6d04cacfa664d131384b2ba6441bd1dc628
ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = peak_hour_dt[name != "IMERG"], aes(x = x, y = y, fill = peak_hour), alpha = 1) + 
  #scale_fill_gradientn(colours = c("blue", "red", "yellow", "green", "blue")) + 
  #scale_fill_gradient2(low = "#2D708EFF", mid = "white", high = "#FFA07AFF", midpoint = 0) + 
  #scale_fill_gradient2(low = "#2D708EFF", mid = "white", high = "#FFA07AFF", midpoint = 0, 
  #breaks = seq(-23, 23, by = 3)) + 
  scale_fill_gradientn(colors = c("#543005", "#8c510a", "#bf812d", "#e0e0e0", "#80cdc1", "#01665e", "#003c30"),
                       breaks=c(-20, -16, -12, -8, -4, 0, 4,  8, 12, 16, 20),
                       limits = c(-23, 23), 
                       #n.breaks = 5,
                       #breaks = custom_breaks,
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5,
                                              title.position = "top")) + 
  facet_wrap(~name, ncol = 2) + 
  labs(x = NULL, y = NULL, fill = "Difference in diurnal peak (h)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
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
        panel.border=element_blank(),
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),panel.spacing = unit(0, "lines")) +  # Remove y-axis labels
  guides(fill=guide_coloursteps(title.position="top"))

ggsave("./projects/main/results/11d_plot_spat_peak_hour_diff_mean2.png", width = 7.1, height = 5.1, 
       units = "in", dpi = 600)
<<<<<<< HEAD

###########################################################################################################

library(ggplot2)
library(gtable)
library(grid)

gradient_OrBu <- colorRampPalette(c('#49411D', '#463F2C', '#9E9278','#f5f5f5', '#9CAABA', '#65758C', '#001f4b'), 
                                  interpolate = "spline", space = "rgb")(100)


ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = peak_hour_dt[name != "IMERG"], aes(x = x, y = y, fill = peak_hour), alpha = 1) + 
  #scale_fill_gradientn(colours = c("blue", "red", "yellow", "green", "blue")) + 
  scale_fill_gradientn(
    colours = gradient_OrBu,
    name = "Difference in diurnal peak (h)", 
    breaks=c(-8, -6, -4,-2, 0, 2, 4, 6, 8),
    limits = c(-10, 10), oob = scales::oob_squish
  ) + 
  facet_wrap(~name, ncol = 2) + 
  labs(x = NULL, y = NULL, fill = "Difference in diurnal peak (h)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
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
        panel.background = element_blank(),
        panel.border=element_blank(),
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), panel.spacing = unit(0, "lines")) +  # Remove y-axis labels
  guides(fill=guide_coloursteps(title.position="top"))

ggsave("./projects/main/results/11d_plot_spat_peak_hour_diff_mean3.png", width = 7.1, height = 5.1, 
       units = "in", dpi = 600)
=======
>>>>>>> 46dff6d04cacfa664d131384b2ba6441bd1dc628

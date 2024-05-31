
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
library(RColorBrewer)

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
ggplot() +
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

ggsave("./projects/main/results/06a_spat_mean.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)


### 24hr diurnal cycle line plot ---------------------------------------------------------------------

data_dt <- rbindlist(data_list)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$location) <- c("Land", "Ocean")
## for glob

mean_24h_glob <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name)]

ggplot(mean_24h_glob, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  #facet_wrap(~location) + 
  labs(x ="Hour (LST)", y = "Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06a_24hlineplot_mean_glob.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


## for land, ocean and Global

mean_24h_landocn <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, location)]
mean_24h_glob2 <- mean_24h_glob[, .(hour, name, location = factor("Global"), mean_value)]

land_ocn_glob <- rbind(mean_24h_glob2, mean_24h_landocn)
# levels(land_ocn_glob$location) <- c("Global", "Ocean", "Land")

ggplot(land_ocn_glob, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~location) + 
  labs(x ="Hour (LST)", y = "Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), legend.position = "bottom", strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/06a_24hlineplot_mean_landocnglob.png",
       width = 10.6, height = 4.2, units = "in", dpi = 600)


ggplot(land_ocn_glob, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~location) + 
  labs(x = "Hour (LST)", y = "Mean (mm/hr)") + 
  theme_generic + 
  theme(legend.title = element_blank(), legend.position = "bottom", strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) +
  scale_y_continuous(expand = expansion(add = c(0, 0)), limits = c(0, NA))


ggsave("./projects/main/results/06a_24hlineplot_mean_landocnglob_scale0.png",
       width = 10.6, height = 4.2, units = "in", dpi = 600)

### Estimate the peak hour of data.tables -------------------------------------------

#system.time(peak_hour_dt <- data_dt[, .SD[which.max(prec_mean)], by = .(lat, lon, name)])
# user  system elapsed 
# 488.337   2.325 362.277 
system.time(peak_hour_list <- lapply(data_list, function(df) {
  df[, .SD[which.max(prec_mean)], by = .(lat, lon, name)]
}))
# user  system elapsed 
# 412.915   4.548 342.563 

saveRDS(peak_hour_list, "./projects/main/data/mean_peak_hour_dt_2001_20.RDS")

###################################

peak_hour_list <- readRDS("./projects/main/data/mean_peak_hour_dt_2001_20.RDS")

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


ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = peak_hour_dt, aes(x = x, y = y, fill = peak_hour), alpha = 1) + 
  #scale_color_manual(values = rainbow(24)) + 
  #scale_color_manual(colours = c("red", "blue")) + 
  scale_fill_gradientn(colours = c("#e66101", "#ffffbf","#0571b0", "#4dac26", "#e66101"), 
                       breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  
  # scale_fill_stepsn(colours = (pals::kovesi.cyclic_mygbm_30_95_c78_s25),
  #                   breaks = c(3, 6, 9, 12, 15, 18, 21), show.limits = TRUE) + 
  # scale_fill_viridis(option = "magma", direction = 1,
  #                      breaks = c(0, 3, 6, 9, 12, 15, 18, 21, 23)) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "Peak hour (LST)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_generic +
  theme(plot.title = element_text(hjust = 0.3, size = 10, face = "bold"),
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


ggsave("./projects/main/results/06a_plot_spat_peak_hour_mean.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)


###############################################################

# variance or range among the estimates

mean_data_list


# Create a data.table from your list of data.tables
merged_dt <- rbindlist(mean_data_list, idcol = "dataset")

# Calculate the standard deviation and range for each time, latitude, and longitude
result_dt <- merged_dt[, .(Std_Deviation = sd(mean_value), range = diff(range(mean_value))), 
                       by = .(lat, lon)]

result_dt
summary(result_dt)

extracted_data_list <- result_dt[, c("lon", "lat", "range")]

# Use lapply to create a list of rasters
raster_list <- create_raster(extracted_data_list)

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_list, crs = PROJ)

# Convert spatial data to data frame
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame")
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()

to_plot <- melt(rast_robin_df, c("x", "y"))
to_plot <- to_plot[, .(x, y, value = round(value, 2))]

summary(to_plot)

hist(to_plot$value)
ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot, aes(x = x, y = y, fill = value), alpha = 1) + 
  #facet_wrap(~name, ncol = 3) + 
  # scale_fill_binned(type = "viridis", option = "B", direction = -1,
  #                   breaks = c(0.02, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6), show.limits = TRUE) + 
  scale_fill_viridis_c(option = "C", direction = -1,
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

ggsave("./projects/main/results/06a_range_mean.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)




library(fst)
write_fst(dat_lst_list, "./projects/main/data/trial_2001_20.rds")
#extras------------------
#box plot
to_plot <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(lat, lon, name, location)]

ggplot(to_plot, aes(name, mean_value, fill = location)) + 
  geom_boxplot() + 
  labs(x = "", y = "Mean (mm/hr)", fill = "") + 
  theme_small 


to_plot_hourly <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(lat, lon, hour(time_lst), name, location)]

ggplot(to_plot_hourly, aes(factor(hour), mean_value, fill = name)) + 
  geom_boxplot() + 
  labs(x = "", y = "Mean (mm/hr)", fill = "") + 
  facet_wrap(~location, nrow = 2) + 
  theme_small



dummie <- raster::raster("~/shared/data_review/rkp_hourly_precip/gsmap_tp_mm_global60s60n_200101_202012_025_hourly.nc")
pRecipe::plot_map(dummie)
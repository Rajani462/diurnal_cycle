
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

data_comb <- rbindlist(mean_data_list)

ens_mean <- data_comb[, .(ens_mean = mean(mean_value, na.rm = TRUE)), by = .(lat, lon)]

result <- merge(data_comb, ens_mean, by = c("lat", "lon"))

# Subtract mean_value from ens_mean
result[, difference := mean_value - ens_mean]
result
result[,  `:=`(mean_value = NULL, ens_mean = NULL)]

long_result <- melt(result, id.vars = c("lat", "lon", "name"))
long_result[,  `:=`(variable = NULL)]
head(long_result, 10)

ens_mean[, name := factor("ens_mean")]
ens_mean <- ens_mean[, .(lat, lon, name, value = ens_mean)]
merged_data <- rbind(ens_mean, long_result)
setnames(merged_data, "value", "mean_value")
mean_data_list <- split(merged_data, by = "name")

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

levels(to_plot$name) <- c("Ensemble mean", "IMERG - Ensemble mean", "GSMaP - Ensemble mean", 
                          "CMORPH - Ensemble mean", "PERSIANN - Ensemble mean", "ERA5 - Ensemble mean")
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
        strip.background = element_blank(),
        panel.border=element_blank(),
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.text.y = element_blank()) +  # Remove y-axis labels
  guides(fill=guide_coloursteps(title.position="top"))


summary(to_plot[name != "Ensemble mean"])

library("RColorBrewer")
ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  geom_tile(data = to_plot[name != "Ensemble mean"], aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_gradientn(colors = c("#543005", "#8c510a", "#bf812d", "#e0e0e0", "#80cdc1", "#01665e", "#003c30"),
                       breaks=c(-0.1, -0.05, 0, 0.05, 0.1),
                       limits = c(-0.2, 0.2), 
                       #n.breaks = 5,
                       #breaks = custom_breaks,
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5,
                                              title.position = "top")) +
  labs(x = NULL, y = NULL, fill = "Amount (mm/hr)") + 
  coord_fixed(ratio = 1) + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) + 
  theme_generic +
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
        axis.ticks = element_blank(), panel.spacing = unit(0, "lines")) +  # Remove y-axis labels
  guides(fill=guide_coloursteps(title.position="top"))


ggsave("./projects/main/results/11a_ens_mean_diff_spat_mean.png", width = 10.5, height = 5.1,
       units = "in", dpi = 600)


###############################################################

# extras ------------------------------------------------------------------



ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  geom_tile(data = to_plot, aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_gradientn(colors = c("#543005", "#8c510a", "#bf812d", "#e0e0e0", "#80cdc1", "#01665e", "#003c30"),
                       breaks=c(-0.3, -0.1, -0.05, 0, 0.05, 0.1, 0.3),
                       limits = c(-0.5, 0.5), 
                       #n.breaks = 5,
                       #breaks = custom_breaks,
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5,
                                              title.position = "top")) +
  labs(x = NULL, y = NULL, fill = "Amount (mm/hr)") + 
  coord_fixed(ratio = 1) + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) + 
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25,"cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center",
        strip.background = element_blank(),
        panel.border=element_blank(),
        axis.text.x = element_blank(),  # Remove x-axis labels
        axis.text.y = element_blank()) +  # Remove y-axis labels
  guides(fill=guide_coloursteps(title.position="top"))



#########################################################################################################

ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot[name != "Ensemble mean"], aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1,
                    breaks = c(-0.5, -0.2, 0, 0.02, 0.05, 0.1, 0.5), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Amount (mm/hr)") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_small

ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot[name != "Ensemble mean"], aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_manual(name="Amount (mm/hr)",
                    values=c("#543005", "#8c510a", "#bf812d", "#dfc27d",
                                      "#e0e0e0",
                                      "#80cdc1", "#35978f", "#01665e", "#003c30"),
                                      guide=guide_legend(reverse=TRUE))
    

# Define breaks to bin the continuous values
ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot[name != "Ensemble mean"], aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_gradient2(name = "Amount (mm/hr)",
                       low = "#543005", mid = "white", high = "#003c30",
                       midpoint = 0,
                       breaks = c(-1, -0.5, -0.2, 0, 0.02, 0.05, 0.1, 0.5),
                       na.value = "transparent",
                       guide = guide_colorbar(reverse = TRUE))




library(ggplot2)
library(viridis)

# Define custom breaks for the color scale
custom_breaks <- c(-0.5, -0.2, -0.1, -0.02, 0, 0.05, 0.1, 0.2)

ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  geom_tile(data = to_plot[name != "Ensemble mean"], aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0, na.value = "white",
                       breaks = custom_breaks,
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5,
                                              title.position = "top")) +
  labs(x = NULL, y = NULL, fill = "Amount (mm/hr)") + 
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25,"cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center",
        strip.background = element_blank(),
        panel.border=element_blank()) +
  guides(fill=guide_coloursteps(title.position="top"))

library(ggplot2)

custom_breaks <- c(-0.5, -0.1, -0.05, -0.02, 0, 0.02, 0.05, 0.1, 0.5)

ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  geom_tile(data = to_plot[name != "Ensemble mean"], aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_gradientn(colors = c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#e0e0e0", "#80cdc1", "#35978f", "#01665e", "#003c30"),
                       breaks = custom_breaks,
                       trans = "log",
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5,
                                              title.position = "top")) +
  labs(x = NULL, y = NULL, fill = "Amount (mm/hr)") + 
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25,"cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center",
        strip.background = element_blank(),
        panel.border=element_blank()) +
  guides(fill=guide_coloursteps(title.position="top"))

# Define custom breaks
# Define custom breaks
custom_breaks <- c(-0.1, -0.02, -0.01, 0, 0.01, 0.03, 0.1)

# Plot with updated breaks
midpoint <- quantile(to_plot[name != "Ensemble mean"]$value, 0.5)

ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  geom_tile(data = to_plot[name != "Ensemble mean"], aes(x = x, y = y, fill = value), alpha = 1) + 
  facet_wrap(~name, ncol = 3) + 
  scale_fill_gradientn(colors = c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#e0e0e0", "#80cdc1", "#35978f", "#01665e", "#003c30"),
                       breaks = custom_breaks,
                       midpoint = midpoint,
                       guide = guide_colorbar(barwidth = 10, barheight = 0.5,
                                              title.position = "top")) +
  labs(x = NULL, y = NULL, fill = "Amount (mm/hr)") + 
  coord_fixed(ratio = 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25,"cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center",
        strip.background = element_blank(),
        panel.border=element_blank()) +
  guides(fill=guide_coloursteps(title.position="top"))

# Plot with updated breaks and colors

custom_breaks <- c(-0.1, -0.05, 0, 0.05, 0.1)


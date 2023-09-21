
# clustring plots --------------------------------------------------------------

# for number of cluster K = 4 ---------------------------------------------

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
# library(cluster)
# library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/graphics.R')
#library(factoextra)


# read the dataset --------------------------------------------------------

processed_data_list <- readRDS("./projects/main/data/cluster_3_all_dat_freq_parellel_setseed123.RDS")


#plot--

to_plot_list <- lapply(names(processed_data_list), function(name) {
  data_dt <- processed_data_list[[name]]
  to_plot <- data_dt[, .(mean_value = mean(value, na.rm = TRUE), 
                         q25 = quantile(value, 0.25, na.rm = TRUE), 
                         q75 = quantile(value, 0.75, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
  to_plot$dataset_name <- factor(name)
  return(to_plot)
})


to_plot_all <- rbindlist(to_plot_list)
#to_plot_all_2 <- data.table::melt(to_plot_all, c("hour", "clusters", "dataset_name"))

levels(to_plot_all$dataset_name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~dataset_name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Frequency (%)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/05b_all_data_freq_cluster_3_line_plot.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Frequency (%)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/05b_all_data_freq_cluster_3_uncert_line_plot.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


### spatial plot-----

to_plot_spat_list <- lapply(names(processed_data_list), function(name) {
  data_dt <- processed_data_list[[name]]
  to_plot <- data_dt[, .(mean_value = mean(value, na.rm = TRUE)), by = .(lat, lon, clusters = as.factor(clusters))]
  to_plot$dataset_name <- factor(name)
  return(to_plot)
})

to_plot_spat_all <- rbindlist(to_plot_spat_list)
levels(to_plot_spat_all$dataset_name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

ggplot(to_plot_spat_all) + 
  geom_raster(aes(lon, lat, fill = clusters)) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "") + 
  facet_wrap(~dataset_name, ncol = 3) + 
  coord_cartesian(xlim = c(min(to_plot_spat_all$lon), max(to_plot_spat_all$lon)), 
                  ylim = c(min(to_plot_spat_all$lat), max(to_plot_spat_all$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.15))

ggsave("./projects/main/results/cluster/05b_all_data_freq_cluster_3_spat_plot.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)



################################################################################

to_plot_list <- lapply(names(processed_data_list), function(name) {
  data_dt <- processed_data_list[[name]]
  to_plot <- data_dt[, .(mean_value = mean(value, na.rm = TRUE), 
                         q25 = quantile(value, 0.25, na.rm = TRUE), 
                         q75 = quantile(value, 0.75, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
  to_plot$dataset_name <- factor(name)
  return(to_plot)
})

dat <- rbindlist(to_plot_list)

# Setnames for consistency
setnames(dat, "dataset_name", "name")

# Define the conditions and update the cluster_name column
dat[name %in% c("imerg", "gsmap", "era5") & clusters == 3, cluster_name := "Afternoon peak"]
dat[name %in% c("cmorph", "persiann") & clusters == 1, cluster_name := "Afternoon peak"]

dat[name %in% c("gsmap", "era5") & clusters == 1, cluster_name := "Early-morning peak"]
dat[name == "cmorph" & clusters == 2, cluster_name := "Early-morning peak"]

dat[name %in% c("imerg", "era5") & clusters == 2, cluster_name := "Midnight peak"]
dat[name == "persiann" & clusters == 3, cluster_name := "Midnight peak"]
dat[name == "cmorph" & clusters == 3, cluster_name := "Midnight peak"]


dat[name %in% c("gsmap", "persiann") & clusters == 2, cluster_name := "Midday peak"]
dat[name == "imerg" & clusters == 1, cluster_name := "Midday peak"]


# Convert cluster_name to a factor
dat[, cluster_name := factor(cluster_name)]

levels(dat$cluster_name)

to_plot_all <- dat
levels(to_plot_all$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

ggplot(to_plot_all, aes(hour, mean_value, col = cluster_name, group = cluster_name)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Frequency (%)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/05b_all_data_freq_cluster_3_line_plot_named.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)



#spatial named---------------

to_plot_list <- lapply(names(processed_data_list), function(name) {
  data_dt <- processed_data_list[[name]]
  to_plot <- data_dt[, .(mean_value = mean(value, na.rm = TRUE)), by = .(lat, lon, clusters = as.factor(clusters))]
  to_plot$dataset_name <- factor(name)
  return(to_plot)
})


dat <- rbindlist(to_plot_list)

setnames(dat, "dataset_name", "name")

# Define the conditions and update the cluster_name column
dat[name %in% c("imerg", "gsmap", "era5") & clusters == 3, cluster_name := "Afternoon peak"]
dat[name %in% c("cmorph", "persiann") & clusters == 1, cluster_name := "Afternoon peak"]

dat[name %in% c("gsmap", "era5") & clusters == 1, cluster_name := "Early-morning peak"]
dat[name == "cmorph" & clusters == 2, cluster_name := "Early-morning peak"]

dat[name %in% c("imerg", "era5") & clusters == 2, cluster_name := "Midnight peak"]
dat[name == "persiann" & clusters == 3, cluster_name := "Midnight peak"]
dat[name == "cmorph" & clusters == 3, cluster_name := "Midnight peak"]


dat[name %in% c("gsmap", "persiann") & clusters == 2, cluster_name := "Midday peak"]
dat[name == "imerg" & clusters == 1, cluster_name := "Midday peak"]

# Convert cluster_name to a factor
dat[, cluster_name := factor(cluster_name)]


levels(dat$cluster_name)

to_plot_all <- dat
levels(to_plot_all$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

ggplot(to_plot_all) + 
  geom_raster(aes(lon, lat, fill = cluster_name)) + 
  scale_fill_manual(values = line_colors) + 
  borders(colour = "black") + 
  labs(x ="", y = "") + 
  facet_wrap(~name, ncol = 3) + 
  coord_cartesian(xlim = c(min(to_plot_all$lon), max(to_plot_all$lon)), 
                  ylim = c(min(to_plot_all$lat), max(to_plot_all$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.15))

ggsave("./projects/main/results/cluster/all_data_freq_cluster_3_spat_plot_named.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)


# spatial nmaed in robinson projection--------------------------------------------------------

data_list_org <- split(to_plot_all, by = 'name') 
data_list <- lapply(data_list_org, function(df) df[, c("lon", "lat", "clusters", "name")])

imerg <- data_list$IMERG


# Assuming your list is named "data_list"
data_list$PERSIANN <- rbind(data_list$PERSIANN, imerg)
data_list$PERSIANN <- unique(data_list$PERSIANN, by = c("lat", "lon"))
data_list$PERSIANN <- data_list$PERSIANN[name == "IMERG", clusters := NA]
data_list$PERSIANN[is.na(data_list$PERSIANN$clusters), "name"] <- "PERSIANN"

data_list$CMORPH <- rbind(data_list$CMORPH, imerg)
data_list$CMORPH <- unique(data_list$CMORPH, by = c("lat", "lon"))
data_list$CMORPH <- data_list$CMORPH[name == "IMERG", clusters := NA]
data_list$CMORPH[is.na(data_list$CMORPH$clusters), "name"] <- "CMORPH"

extracted_data_list <- lapply(data_list, function(df) df[, c("lon", "lat", "clusters")])
  
# Use lapply to create a list of rasters
raster_list <- lapply(extracted_data_list, create_raster)
raster_brick <- brick(raster_list)

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_brick, crs = PROJ, method="ngb")

# Convert spatial data to data frame
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame")
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()

to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
to_plot2 <- to_plot[, .(x, y, clusters = value), name]

# Define the conditions and update the cluster_name column
to_plot2[name %in% c("IMERG", "GSMaP", "ERA5") & clusters == 3, cluster_name := "Afternoon peak"]
to_plot2[name %in% c("CMORPH", "PERSIANN") & clusters == 1, cluster_name := "Afternoon peak"]

to_plot2[name %in% c("GSMaP", "ERA5") & clusters == 1, cluster_name := "Early-morning peak"]
to_plot2[name == "CMORPH" & clusters == 2, cluster_name := "Early-morning peak"]

to_plot2[name %in% c("IMERG", "ERA5") & clusters == 2, cluster_name := "Midnight peak"]
to_plot2[name == "PERSIANN" & clusters == 3, cluster_name := "Midnight peak"]
to_plot2[name == "CMORPH" & clusters == 3, cluster_name := "Midnight peak"]


to_plot2[name %in% c("GSMaP", "PERSIANN") & clusters == 2, cluster_name := "Midday peak"]
to_plot2[name == "IMERG" & clusters == 1, cluster_name := "Midday peak"]


# Convert cluster_name to a factor
to_plot2[, cluster_name := factor(cluster_name)]

levels(to_plot2$cluster_name)
levels(to_plot2$name)

#to_plot_all <- dat
#levels(to_plot_all$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot2, aes(x = x, y = y, fill = cluster_name), alpha = 1) + 
  scale_fill_manual(values = line_colors, breaks = c("Afternoon peak", "Early-morning peak", "Midday peak", 
                                                     "Midnight peak")) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_small +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold")) + 
  theme(strip.background = element_blank(), panel.border=element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + theme( legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.80, 0.20))


ggsave("./projects/main/results/cluster/05b_all_data_freq_cluster_3_spat_plot_named_robin.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)

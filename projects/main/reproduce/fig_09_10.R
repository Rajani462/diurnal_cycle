
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
library(gridExtra)
#library(ggh4x)
# library(cluster)
# library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/graphics.R')
#library(factoextra)


# read the dataset --------------------------------------------------------

processed_data_list <- readRDS("~/rajani/diurnal_cycle/projects/main/reproduce/data/cluster_4_all_dat_parellel_setseed123.RDS")

##plot

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
dat[name %in% c("cmorph") & clusters == 1, cluster_name := "Afternoon peak"]
dat[name %in% c("imerg", "era5") & clusters == 3, cluster_name := "Afternoon peak"]
dat[name == "persiann" & clusters == 4, cluster_name := "Afternoon peak"]

dat[name %in% c("cmorph", "persiann") & clusters == 3, cluster_name := "Early-morning peak"]
dat[name == "imerg" & clusters == 4, cluster_name := "Early-morning peak"]
dat[name == "era5" & clusters == 2, cluster_name := "Early-morning peak"]
dat[name == "persiann" & clusters == 1, cluster_name := "Early-morning peak"]
dat[name %in% c("gsmap") & clusters == 1, cluster_name := "Early-morning peak"]

dat[name %in% c("imerg", "gsmap", "cmorph") & clusters == 2, cluster_name := "Late-morning peak"]
dat[name %in% c("gsmap") & clusters == 2, cluster_name := "Afternoon peak"]
dat[name == "era5" & clusters == 4, cluster_name := "Late-morning peak"]
dat[name %in% c("gsmap") & clusters == 3, cluster_name := "Late-morning peak"]

dat[name == "imerg" & clusters == 1, cluster_name := "Mid-night peak"]
dat[name == "cmorph" & clusters == 4, cluster_name := "Mid-night peak"]
dat[name == "persiann" & clusters == 3, cluster_name := "Mid-night peak"]

dat[name == "persiann" & clusters == 2, cluster_name := "Early-afternoon peak"]
dat[name == "era5" & clusters == 1, cluster_name := "Early-afternoon peak"]
dat[name == "gsmap" & clusters == 4, cluster_name := "Early-afternoon peak"]

# Convert cluster_name to a factor
dat[, cluster_name := factor(cluster_name)]

levels(dat$cluster_name)

to_plot_all <- dat
levels(to_plot_all$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

line_plot_nmaed <- ggplot(to_plot_all, aes(hour, mean_value, col = cluster_name, group = cluster_name)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

line_plot_nmaed

ggsave("~/rajani/diurnal_cycle/projects/main/reproduce/results/09_fig_all_data_cluster_4_line_plot_named.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


################################################################################

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
dat[name %in% c("cmorph") & clusters == 1, cluster_name := "Afternoon peak"]
dat[name %in% c("imerg", "era5") & clusters == 3, cluster_name := "Afternoon peak"]
dat[name == "persiann" & clusters == 4, cluster_name := "Afternoon peak"]

dat[name %in% c("cmorph", "persiann") & clusters == 3, cluster_name := "Early-morning peak"]
dat[name == "imerg" & clusters == 4, cluster_name := "Early-morning peak"]
dat[name == "era5" & clusters == 2, cluster_name := "Early-morning peak"]
dat[name == "persiann" & clusters == 1, cluster_name := "Early-morning peak"]
dat[name %in% c("gsmap") & clusters == 1, cluster_name := "Early-morning peak"]

dat[name %in% c("imerg", "gsmap", "cmorph") & clusters == 2, cluster_name := "Late-morning peak"]
dat[name %in% c("gsmap") & clusters == 2, cluster_name := "Afternoon peak"]
dat[name == "era5" & clusters == 4, cluster_name := "Late-morning peak"]
dat[name %in% c("gsmap") & clusters == 3, cluster_name := "Late-morning peak"]

dat[name == "imerg" & clusters == 1, cluster_name := "Mid-night peak"]
dat[name == "cmorph" & clusters == 4, cluster_name := "Mid-night peak"]
dat[name == "persiann" & clusters == 3, cluster_name := "Mid-night peak"]

dat[name == "persiann" & clusters == 2, cluster_name := "Early-afternoon peak"]
dat[name == "era5" & clusters == 1, cluster_name := "Early-afternoon peak"]
dat[name == "gsmap" & clusters == 4, cluster_name := "Early-afternoon peak"]

# Convert cluster_name to a factor
dat[, cluster_name := factor(cluster_name)]


levels(dat$cluster_name)

to_plot_all <- dat
levels(to_plot_all$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")


# spatial named in robinson projection--------------------------------------------------------

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

to_plot2[name %in% c("CMORPH") & clusters == 1, cluster_name := "Afternoon peak"]
to_plot2[name %in% c("IMERG", "ERA5") & clusters == 3, cluster_name := "Afternoon peak"]
to_plot2[name == "PERSIANN" & clusters == 4, cluster_name := "Afternoon peak"]

to_plot2[name %in% c("CMORPH", "PERSIANN") & clusters == 3, cluster_name := "Early-morning peak"]
to_plot2[name == "IMERG" & clusters == 4, cluster_name := "Early-morning peak"]
to_plot2[name == "ERA5" & clusters == 2, cluster_name := "Early-morning peak"]
to_plot2[name == "PERSIANN" & clusters == 1, cluster_name := "Early-morning peak"]
to_plot2[name %in% c("GSMaP") & clusters == 1, cluster_name := "Early-morning peak"]

to_plot2[name %in% c("IMERG", "GSMaP", "CMORPH") & clusters == 2, cluster_name := "Late-morning peak"]
to_plot2[name %in% c("GSMaP") & clusters == 2, cluster_name := "Afternoon peak"]
to_plot2[name == "ERA5" & clusters == 4, cluster_name := "Late-morning peak"]
to_plot2[name %in% c("GSMaP") & clusters == 3, cluster_name := "Late-morning peak"]

to_plot2[name == "IMERG" & clusters == 1, cluster_name := "Mid-night peak"]
to_plot2[name == "CMORPH" & clusters == 4, cluster_name := "Mid-night peak"]
to_plot2[name == "PERSIANN" & clusters == 3, cluster_name := "Mid-night peak"]

to_plot2[name == "PERSIANN" & clusters == 2, cluster_name := "Early-afternoon peak"]
to_plot2[name == "ERA5" & clusters == 1, cluster_name := "Early-afternoon peak"]
to_plot2[name == "GSMaP" & clusters == 4, cluster_name := "Early-afternoon peak"]


# Convert cluster_name to a factor
to_plot2[, cluster_name := factor(cluster_name)]

levels(to_plot2$cluster_name)

to_plot_all <- dat
levels(to_plot_all$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot_all$cluster_name)

spat_plot <- ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  coord_fixed(ratio = 1) +
  geom_tile(data = to_plot2, aes(x = x, y = y, fill = cluster_name), alpha = 1) + 
  scale_fill_manual(values = line_colors, breaks = c("Afternoon peak", "Early-afternoon peak", "Early-morning peak", "Late-morning peak", 
                                                      "Mid-night peak")) + 
  facet_wrap(~name, ncol = 3) + 
  labs(x = NULL, y = NULL, fill = "") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_generic +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold")) + 
  theme(panel.grid = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(), panel.spacing = unit(0, "lines"), 
        legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.80, 0.20))

spat_plot

ggsave("~/rajani/diurnal_cycle/projects/main/reproduce/results/10_fig_all_data_cluster_4_spat_plot_named_robin.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)



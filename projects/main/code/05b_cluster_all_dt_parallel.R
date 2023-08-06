
# clustring ---------------------------------------------------------------

library(raster)
library(data.table)
library(factoextra)
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



# read the dataset --------------------------------------------------------

processed_data_list <- readRDS("./projects/main/data/cluster_all_dat_parellel.RDS")


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
  facet_wrap(~dataset_name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))
  
ggsave("./projects/main/results/cluster/all_data_cluster_line_plot.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


ggplot(to_plot_all, aes(hour, mean_value, ymin=q25, ymax=q75, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  geom_ribbon(alpha = 0.5) + 
  facet_grid(dataset_name~clusters) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/all_data_cluster_uncert_line_plot.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


### spatial plot-----

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]

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

ggsave("./projects/main/results/cluster/all_data_cluster_spat_plot.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)


to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
to_plot <- processed_data_list$cmorph[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

to_plot <- processed_data_list$persiann[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")


###########################################################################################

#for set seed (123)-----------------------------------------


processed_data_list <- readRDS("./projects/main/data/cluster_all_dat_parellel_setseed123.RDS")


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
  facet_wrap(~dataset_name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/all_data_cluster_line_setseed_plot.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


ggplot(to_plot_all, aes(hour, mean_value, ymin=q25, ymax=q75, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  geom_ribbon(alpha = 0.5) + 
  facet_grid(dataset_name~clusters) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/all_data_cluster_uncert_setseed_line_plot.png", width = 9.5, height = 5.3, 
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

ggsave("./projects/main/results/cluster/all_data_cluster_spat_setseed_plot.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)


to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
to_plot <- processed_data_list$cmorph[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

to_plot <- processed_data_list$persiann[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")









# for mean precip --------------------------------------------------------------------
dat_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")


cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001.rds")
mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
mean_dat <- dat_list$cmorph[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)

mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]
summary(mean_dat)

mean_dat[, `:=`(value = NULL)]

mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

mean_dat_wide_comp <- mean_dat_wide[complete.cases(mean_dat_wide), ]

k2 <- kmeans(mean_dat_wide_comp, centers = 6, nstart = 50)



mean_dat <- dat_list$cmorph[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]

mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]

mean_dat_comp2 <- mean_dat_comp[, .(lat, lon, hour, value, normalized_value = NULL)]
mean_dat_comp2_wide <- dcast(mean_dat_comp2, lat + lon ~ hour)

mean_dat_comp2_wide$clusters <- k2$cluster


mean_dt_k_long <- melt(mean_dat_comp2_wide, c("lat", 'lon', 'clusters'), variable.name = "hour")

############################################

library(data.table)
library(cluster)

process_cmorph_data <- function(data_dt, k_value = 6, nstart_value = 50) {
  # Process the data
  mean_dat <- data_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
  mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]
  
  mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]
  
  mean_dat_comp2 <- mean_dat_comp[, .(lat, lon, hour, normalized_value)]
  mean_dat_comp2_wide <- dcast(mean_dat_comp2, lat + lon ~ hour)
  
  # Perform kmeans clustering
  k2 <- kmeans(mean_dat_comp2_wide[, -c("lat", "lon")], centers = k_value, nstart = nstart_value)
  
  #join the cluster to original data
  mean_dat_comp[, `:=`(normalized_value = NULL)]
  mean_dat_wide_org <- dcast(mean_dat_comp, lat + lon ~ hour)
  mean_dat_wide_org$clusters <- k2$cluster
  # 
  mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")
  # 
  return(mean_dt_k_long)
}

# Usage example:

gsmap_result <- process_cmorph_data(dat_list$gsmap, k_value = 6, nstart_value = 50)

imerg_result <- process_cmorph_data(dat_list$imergf, k_value = 6, nstart_value = 50)

cmorph_result <- process_cmorph_data(dat_list$cmorph, k_value = 6, nstart_value = 50)


mean_dat <- dat_list$cmorph[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]

mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]

mean_dat_comp[, `:=`(normalized_value = NULL)]

mean_dat_wide_org <- dcast(mean_dat_comp, lat + lon ~ hour)
summary(mean_dat_wide_org)

#join the clusters
mean_dat_wide_org$clusters <- cmorph_result$cluster

mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, list(mean_value = mean(value, na.rm = TRUE)), by = list(hour, clusters = as.factor(clusters))]
to_plot <- cmorph_result[, list(mean_value = mean(value, na.rm = TRUE)), by = list(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/cluster/gsmap_clusters_mean_cl6_2001_20_n50.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  theme(axis.text.x=element_text(color=c("black","transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black")))

ggsave("./projects/main/results/cluster/gsmap_clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
       units = "in", dpi = 600)

to_plot_spat <- gsmap_result[, .(mean_value = mean(value, na.rm = TRUE)), 
                             by = .(lat, lon, clusters = as.factor(clusters))]
ggplot(to_plot_spat) + 
  geom_raster(aes(lon, lat, fill = clusters)) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  #labs(x ="Longitude", y = "Latitude") + 
  coord_cartesian(xlim = c(min(to_plot_spat$lon), max(to_plot_spat$lon)), 
                  ylim = c(min(to_plot_spat$lat), max(to_plot_spat$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/cluster/gsmap__clusters_mean_spat_cl6_2001_20_n50.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)

ggplot(to_plot_spat[clusters == "3"]) + 
  geom_raster(aes(lon, lat, fill = clusters)) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  #labs(x ="Longitude", y = "Latitude") + 
  coord_cartesian(xlim = c(min(to_plot_spat$lon), max(to_plot_spat$lon)), 
                  ylim = c(min(to_plot_spat$lat), max(to_plot_spat$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(to_plot_spat[clusters == "3" | clusters == "4"]) + 
  geom_raster(aes(lon, lat, fill = clusters)) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  #labs(x ="Longitude", y = "Latitude") + 
  coord_cartesian(xlim = c(min(to_plot_spat$lon), max(to_plot_spat$lon)), 
                  ylim = c(min(to_plot_spat$lat), max(to_plot_spat$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

##############################

#mean_dat[, `:=`(value = NULL)]
mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]

mean_dat_comp2 <- mean_dat_comp[, .(lat, lon, hour, normalized_value, value = NULL)]
mean_dat_comp2_wide <- dcast(mean_dat_comp2, lat + lon ~ hour)

mean_dat_comp2_wide$clusters <- k_means

summary(mean_dat_comp2_wide)

mean_dt_k_long <- melt(mean_dat_comp2_wide, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/02e_cmorph_clusters_mean_cl6_2001_20_n50.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  theme(axis.text.x=element_text(color=c("black","transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black")))

ggsave("./projects/main/results/02e_cmorph_clusters_mean_line_cl6_2001_20_n50.png", width = 10.5, height = 5.3, 
       units = "in", dpi = 600)

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]
ggplot(to_plot_spat) + 
  geom_raster(aes(lon, lat, fill = clusters)) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  #labs(x ="Longitude", y = "Latitude") + 
  coord_cartesian(xlim = c(min(to_plot_spat$lon), max(to_plot_spat$lon)), 
                  ylim = c(min(to_plot_spat$lat), max(to_plot_spat$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/02e_cmorph_clusters_mean_spat_cl6_2001_20_n50.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)

########################################################################


# for number of cluster K = 5 ---------------------------------------------

library(raster)
library(data.table)
library(factoextra)
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



# read the dataset --------------------------------------------------------

processed_data_list <- readRDS("./projects/main/data/cluster_")


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
  facet_wrap(~dataset_name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/all_data_cluster_4_line_plot.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/all_data_cluster_5_uncert_line_plot.png", width = 9.5, height = 5.3, 
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

ggsave("./projects/main/results/cluster/all_data_cluster_5_spat_plot.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)


to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
to_plot <- processed_data_list$cmorph[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

to_plot <- processed_data_list$persiann[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")



########################################################################


# for number of cluster K = 4 ---------------------------------------------

library(raster)
library(data.table)
library(factoextra)
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



# read the dataset --------------------------------------------------------

processed_data_list <- readRDS("./projects/main/data/cluster_4_all_dat_parellel_setseed123.RDS")


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
  facet_wrap(~dataset_name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/all_data_cluster_4_line_plot.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/all_data_cluster_4_uncert_line_plot.png", width = 9.5, height = 5.3, 
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

ggsave("./projects/main/results/cluster/all_data_cluster_4_spat_plot.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)


to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
to_plot <- processed_data_list$cmorph[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

to_plot <- processed_data_list$persiann[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")


# clustring ---------------------------------------------------------------

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
library(cluster)
install.packages("factoextra")
library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


# for mean precip --------------------------------------------------------------------

#cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001.rds")
cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001_20.rds")

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)

mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide, centers = 10, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
# summary(mean_dat)
# 
# mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
# summary(mean_dat)
# mean_dat_cop <- mean_dat
#mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide_org$clusters <- k_means

summary(mean_dat_wide_org)

mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/clusters_mean_cmorph_2001_20_n50.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
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

ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
       units = "in", dpi = 600)

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]

ggplot(to_plot_spat[clusters == "5" | clusters == "10"]) + 
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

ggsave("./projects/main/results/clusters_mean_spat_cmorph_2001_20_n50.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)


###########################################################################################

#standardise the data by hour, but the scale is true----------------------------------------------------
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
library(cluster)
#library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

# for mean precip --------------------------------------------------------------------

#cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001.rds")
cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001_20.rds")

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)

mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(hour)]
summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide, centers = 10, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
#mean_dat <- mean_dat[, .(lat, lon, hour = factor(hour), value = normalized_value)]

# summary(mean_dat)
# 
# mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
# summary(mean_dat)
# mean_dat_cop <- mean_dat
#mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide_org$clusters <- k_means

summary(mean_dat_wide_org)

mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

# ggsave("./projects/main/results/clusters_mean_cmorph_2001_20_n50.png", width = 9.5, height = 5.3, 
#        units = "in", dpi = 600)

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
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

# ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
#        units = "in", dpi = 600)

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

# ggsave("./projects/main/results/clusters_mean_spat_cmorph_2001_20_n50.png", width = 9.9, height = 4.6, 
#        units = "in", dpi = 600)


########################################################################

#standardise the data by lat, lon, but the scale is false----------------------------------------------------

#cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001.rds")
cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001_20.rds")

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)

mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(lat, lon)]
summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide, centers = 10, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
# summary(mean_dat)
# 
# mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
# summary(mean_dat)
# mean_dat_cop <- mean_dat
#mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide_org$clusters <- k_means

summary(mean_dat_wide_org)

mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
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

# ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
#        units = "in", dpi = 600)

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


ggplot(to_plot_spat[clusters == "9"]) + 
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

#######################################################################

########################################################################

#standardise the data by lat, lon, but the scale is True----------------------------------------------------

#cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001.rds")
cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001_20.rds")

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)

mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]
summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

mean_dat_wide2 <- mean_dat_wide[complete.cases(mean_dat_wide), ]

k2 <- kmeans(mean_dat_wide2, centers = 10, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
# summary(mean_dat)
# 
# mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
# summary(mean_dat)
# mean_dat_cop <- mean_dat
#mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

#mean_dat_wide_org$clusters <- k_means
mean_dat_wide_org$clusters <- rep(k_means, length.out = nrow(mean_dat_wide_org))


summary(mean_dat_wide_org)

mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
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


to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]

ggplot(to_plot_spat[clusters == "3" | clusters == "7"]) + 
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

###############################################################################


# normalise the values using min/max method and by .(lat, lon)-------------------------------


library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
library(cluster)
#library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001_20.rds")

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)

mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := (value - min(value)) / (max(value) - min(value)), by = .(lat, lon)]

summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

mean_dat_wide2 <- mean_dat_wide[complete.cases(mean_dat_wide), ]
#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide2, centers = 10, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

#mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
mean_dat <- mean_dat[, .(lat, lon, hour = factor(hour), value = normalized_value)]

# summary(mean_dat)
# 
# mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
# summary(mean_dat)
# mean_dat_cop <- mean_dat
#mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

#mean_dat_wide_org$clusters <- k_means

mean_dat_wide_org$clusters <- rep(k_means, length.out = nrow(mean_dat_wide_org))


summary(mean_dat_wide_org)

mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
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

# ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
#        units = "in", dpi = 600)

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]

ggplot(to_plot_spat[clusters == "5" | clusters == "8"]) + 
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


ggplot(to_plot_spat[clusters == "10"]) + 
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



#######################################################################

# normalise the values using min/max method and hour -------------------------------

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
library(cluster)
#library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')




cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001_20.rds")

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)

mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := (value - min(value)) / (max(value) - min(value)), by = .(hour)]

summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

mean_dat_wide2 <- mean_dat_wide[complete.cases(mean_dat_wide), ]
#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide2, centers = 10, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

mean_dat <- mean_dat[, .(lat, lon, hour = factor(hour), value = normalized_value)]
# summary(mean_dat)
# 
# mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
# summary(mean_dat)
# mean_dat_cop <- mean_dat
#mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

#mean_dat_wide_org$clusters <- k_means

mean_dat_wide_org$clusters <- rep(k_means, length.out = nrow(mean_dat_wide_org))


summary(mean_dat_wide_org)

mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

to_plot[clusters == "8"]


ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")


ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
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

# ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
#        units = "in", dpi = 600)

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]

ggplot(to_plot_spat[clusters == "5" | clusters == "10"]) + 
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

to_plot_spat[clusters == "8"]

ggplot(to_plot_spat[clusters == "8"]) + 
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


#####################################

#Extrcat soth american western coastal pixels------------------
#check if the bi-modal peaks are true over there

filtered_mean_dat <- mean_dat[between(lat, -50, -40) & between(lon, -76, -75.5)]

to_plot_spat <- filtered_mean_dat[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon)]

ggplot(to_plot_spat) + 
  geom_raster(aes(lon, lat, fill = mean_value)) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  #labs(x ="Longitude", y = "Latitude") + 
  coord_cartesian(xlim = c(min(to_plot_spat$lon), max(to_plot_spat$lon)), 
                  ylim = c(min(to_plot_spat$lat), max(to_plot_spat$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")


to_plot_temp <- filtered_mean_dat[, .(mean_value = mean(value, na.rm = TRUE)), 
                                  by = .(hour)]

ggplot(to_plot_temp, aes(hour, mean_value)) + 
  geom_point() + 
  geom_line()


unique(filtered_mean_dat, by = c("lat", "lon")) 

ggplot(filtered_mean_dat, aes(hour, value,group = interaction(lat, lon))) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~lon+lat, scales = "free_y", nrow = 8) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL")

ggplot(filtered_mean_dat, aes(hour, value,group = interaction(lat, lon))) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~lon+lat, nrow = 8) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL")


# Calculate mean_value by grouping on lat, lon, and hour
filtered_mean_dat[, mean_value := mean(value), by = .(lat, lon, hour)]

# Plot line plots for each lat, lon
ggplot(filtered_mean_dat, aes(x = hour, y = mean_value, group = interaction(lat, lon), color = interaction(lat, lon))) + 
  geom_point() + 
  geom_line() + 
  labs(x = "Hour (LST)", y = "Mean Precipitation (mm/hr)") + 
  theme_minimal() + 
  theme(legend.position = "top") +
  scale_color_manual(values = rainbow(length(unique(filtered_mean_dat[, interaction(lat, lon)]))))



filtered_mean_dat2 <- mean_dat[between(lat, -42, -40) & between(lon, -76, -75.5)]

ggplot(filtered_mean_dat2, aes(hour, value, group = interaction(lon, lat))) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~lon+lat, scales = "free_y", nrow = 4) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL")



to_plot_temp2 <- filtered_mean_dat2[, .(mean_value = mean(value, na.rm = TRUE)), 
                                  by = .(hour)]

ggplot(to_plot_temp2, aes(hour, mean_value, group = 1)) + 
  geom_point() + 
  geom_line(linewidth = 1.2)



###############################################################

# normalise the values using min/max method and by .(lat, lon) and with K = 8 -------------------------------


library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
library(cluster)
#library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001_20.rds")

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)

mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := (value - min(value)) / (max(value) - min(value)), by = .(lat, lon)]

summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

mean_dat_wide2 <- mean_dat_wide[complete.cases(mean_dat_wide), ]
#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide2, centers = 8, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

#mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
mean_dat <- mean_dat[, .(lat, lon, hour = factor(hour), value = normalized_value)]

# summary(mean_dat)
# 
# mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
# summary(mean_dat)
# mean_dat_cop <- mean_dat
#mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

#mean_dat_wide_org$clusters <- k_means

mean_dat_wide_org$clusters <- rep(k_means, length.out = nrow(mean_dat_wide_org))


summary(mean_dat_wide_org)

mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
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

# ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
#        units = "in", dpi = 600)

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]

ggplot(to_plot_spat[clusters == "6" | clusters == "8"]) + 
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


#####################################################################

# normalise the values using min/max method and by .(lat, lon) and with K = 15 -------------------------------

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
library(cluster)
#library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001_20.rds")

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)

mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := (value - min(value)) / (max(value) - min(value)), by = .(lat, lon)]

summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

mean_dat_wide2 <- mean_dat_wide[complete.cases(mean_dat_wide), ]
#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide2, centers = 20, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

k_means = k2$cluster

mean_dat_cop

mean_dat_wide_3 <- dcast(mean_dat_cop, lat + lon ~ hour)
mean_dat_wide_3_complet <- mean_dat_wide_3[complete.cases(mean_dat_wide_3), ]

mean_dat_wide_3_complet$clusters <- k_means

summary(mean_dat_wide_3_complet)

mean_dt_k_long <- melt(mean_dat_wide_3_complet, c("lat", 'lon', 'clusters'), variable.name = "hour")


#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
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

# ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
#        units = "in", dpi = 600)

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]

ggplot(to_plot_spat[clusters == "2" | clusters == "9" | clusters == "13"]) + 
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


ggplot(to_plot_spat[clusters == "1" | clusters == "3" | clusters == "6" | clusters == "11"]) + 
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

ggplot(to_plot_spat[clusters == "4"]) + 
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

#############################################################################################


# Frequency of precipitation ----------------------------------------------

# normalise the values using min/max method and by .(lat, lon) and with K = 15 -------------------------------

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
library(cluster)
install.packages("factoextra")
library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


all_dt <- readRDS("./projects/main/data/hourly_freq_all_datasets_LST_glob_2001_20.rds")
cmorph_dt <- all_dt$cmorph

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_freq)]
summary(mean_dat)

#mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := (value - min(value)) / (max(value) - min(value)), by = .(lat, lon)]

summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

mean_dat_wide2 <- mean_dat_wide[complete.cases(mean_dat_wide), ]
#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#Dertemines and visualize the optimal number of clusters using different methods: within cluster sums of squares, average silhouette and gap statistics.
#note: it takes for a while to complete
#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide2, centers = 6, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

mean_dat_cop

mean_dat_wide_3 <- dcast(mean_dat_cop, lat + lon ~ hour)
mean_dat_wide_3_complet <- mean_dat_wide_3[complete.cases(mean_dat_wide_3), ]

mean_dat_wide_3_complet$clusters <- k_means

summary(mean_dat_wide_3_complet)

mean_dt_k_long <- melt(mean_dat_wide_3_complet, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, ncol = 3) + 
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

# ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
#        units = "in", dpi = 600)

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]

ggplot(to_plot_spat[clusters == "2" | clusters == "3" | clusters == "13" | clusters == "14"]) + 
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


ggplot(to_plot_spat[clusters == "8" | clusters == "15"]) + 
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

ggplot(to_plot_spat[clusters == "11"]) + 
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


#############################

# normalise the values using min/max method and by .(lat, lon) and with K = 6 but plot the percentage values -------------------------------

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
library(cluster)
install.packages("factoextra")
library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


all_dt <- readRDS("./projects/main/data/hourly_freq_all_datasets_LST_glob_2001_20.rds")
cmorph_dt <- all_dt$cmorph

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_freq)]
summary(mean_dat)

#mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := (value - min(value)) / (max(value) - min(value)), by = .(lat, lon)]

summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(normalized_value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

mean_dat_wide2 <- mean_dat_wide[complete.cases(mean_dat_wide), ]
#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#Dertemines and visualize the optimal number of clusters using different methods: within cluster sums of squares, average silhouette and gap statistics.
#note: it takes for a while to complete
#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide2, centers = 6, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

#mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
mean_dat <- mean_dat[, .(lat, lon, hour = factor(hour), value)]

# summary(mean_dat)
# 
# mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
# summary(mean_dat)
# mean_dat_cop <- mean_dat
#mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

#mean_dat_wide_org$clusters <- k_means

mean_dat_wide_org$clusters <- rep(k_means, length.out = nrow(mean_dat_wide_org))


summary(mean_dat_wide_org)

mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, ncol = 3) + 
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

# ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
#        units = "in", dpi = 600)

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]

ggplot(to_plot_spat[clusters == "2" | clusters == "3" | clusters == "13" | clusters == "14"]) + 
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


ggplot(to_plot_spat[clusters == "8" | clusters == "15"]) + 
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

ggplot(to_plot_spat[clusters == "11"]) + 
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


###############################################################################


# Intensity ---------------------------------------------------------------


# normalise the values using min/max method and by .(lat, lon) and with K = 6/10  -------------------------------

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
library(cluster)
install.packages("factoextra")
library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


all_dt <- readRDS("./projects/main/data/hourly_int_all_datasets_LST_glob_2001_20.rds")
cmorph_dt <- all_dt$cmorph

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_int)]
summary(mean_dat)

#mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := (value - min(value)) / (max(value) - min(value)), by = .(lat, lon)]

summary(mean_dat)
mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

mean_dat_wide2 <- mean_dat_wide[complete.cases(mean_dat_wide), ]
#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#Dertemines and visualize the optimal number of clusters using different methods: within cluster sums of squares, average silhouette and gap statistics.
#note: it takes for a while to complete
#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide2, centers = 6, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

mean_dat_cop

mean_dat_wide_3 <- dcast(mean_dat_cop, lat + lon ~ hour)
mean_dat_wide_3_complet <- mean_dat_wide_3[complete.cases(mean_dat_wide_3), ]

mean_dat_wide_3_complet$clusters <- k_means

summary(mean_dat_wide_3_complet)

mean_dt_k_long <- melt(mean_dat_wide_3_complet, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, ncol = 3) + 
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

# ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
#        units = "in", dpi = 600)

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


####################################################

# normalise the values using scale (sd, and mean) method and by .(lat, lon) and with K = 6/10  -------------------------------

library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)
library(cluster)
install.packages("factoextra")
library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


all_dt <- readRDS("./projects/main/data/hourly_int_all_datasets_LST_glob_2001_20.rds")
cmorph_dt <- all_dt$cmorph

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_int)]
summary(mean_dat)

#mean_dat[lat == "59.875" & lon == "-179.875"]
#mean_dat <- mean_dat[complete.cases(mean_dat), ]

mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]
summary(mean_dat)

mean_dat_cop <- mean_dat
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

mean_dat_wide2 <- mean_dat_wide[complete.cases(mean_dat_wide), ]
#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

#Dertemines and visualize the optimal number of clusters using different methods: within cluster sums of squares, average silhouette and gap statistics.
#note: it takes for a while to complete
#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide2, centers = 10, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

mean_dat_cop

mean_dat_wide_3 <- dcast(mean_dat_cop, lat + lon ~ hour)
mean_dat_wide_3_complet <- mean_dat_wide_3[complete.cases(mean_dat_wide_3), ]

mean_dat_wide_3_complet$clusters <- k_means

summary(mean_dat_wide_3_complet)

mean_dt_k_long <- melt(mean_dat_wide_3_complet, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

library(ggplot2)
ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
  #facet_wrap(~clusters, ncol = 3) + 
  facet_wrap(~clusters, ncol = 3) + 
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

# ggsave("./projects/main/results/clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
#        units = "in", dpi = 600)

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


############################################################################################

# cluster (K = 10) all data in parellel with standardisation --------------------------------------------

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(raster)
#library(factoextra)
library(ggplot2)
library(dplyr)
library(sf)
library(hms)
library(forcats)


#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

######################################

# Function to process each dataset
cluster_data <- function(data_dt, k_value = 10, nstart_value = 50) {
  # Set the random seed for reproducibility
  set.seed(123)  # Change the seed value as desired
  # Process the data
  mean_dat <- data_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
  mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]
  
  mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]
  
  mean_dat_comp2 <- mean_dat_comp[, .(lat, lon, hour, normalized_value)]
  mean_dat_comp2_wide <- dcast(mean_dat_comp2, lat + lon ~ hour)
  
  # Perform kmeans clustering
  k2 <- kmeans(mean_dat_comp2_wide[, -c("lat", "lon")], centers = k_value, nstart = nstart_value)
  
  #join the cluster to original data
  #mean_dat_comp[, `:=`(normalized_value = NULL)]
  mean_dat_comp[, `:=`(value = NULL)] #insted consider the normalised value and plot
  mean_dat_wide_org <- dcast(mean_dat_comp, lat + lon ~ hour)
  mean_dat_wide_org$clusters <- k2$cluster
  # 
  mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")
  # 
  return(mean_dt_k_long)
}


# Function to process datasets in parallel
process_datasets_parallel <- function(data_list, k_value = 10, nstart_value = 50) {
  num_cores <- detectCores() - 50  # Specify the number of cores to use (adjust if needed)
  cl <- makeCluster(num_cores)
  clusterExport(cl, c("cluster_data"))
  
  processed_data_list <- mclapply(data_list, cluster_data, k_value = k_value, nstart_value = nstart_value)
  stopCluster(cl)
  
  # Assign names to the processed_data_list
  names(processed_data_list) <- names(data_list)
  
  return(processed_data_list)
}

########################

dat_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")

# Process all datasets in parallel
processed_data_list <- process_datasets_parallel(dat_list)

lapply(processed_data_list, summary)
#save the results as a list in .RDS format

#saveRDS(processed_data_list, "./projects/main/data/cluster_K10_all_dat_parellel.RDS")
saveRDS(processed_data_list, "./projects/main/data/cluster_K10_standardised_all_dat_parellel_setseed123.RDS")


####plot
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

# Assuming you have a vector of 10 distinct colors, replace line_colors with your actual vector
line_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f")

summary(to_plot_all)

ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~dataset_name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

#ggsave("./projects/main/results/cluster/05b_all_data_cluster_4_line_plot.png", width = 9.5, height = 5.3, 
#units = "in", dpi = 600)



ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  #geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Standradised Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggsave("./projects/main/results/cluster/10_all_data_cluster_10_stndised_line_plot.png", width = 9.5, height = 5.3, 
units = "in", dpi = 600)


ggplot(to_plot_all[dataset_name == "CMORPH"], aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  #geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_wrap(~clusters) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))


#spatial plot

cmorph <- processed_data_list$cmorph
cmorph[lat == "-59.875" & lon == "-179.375"]

cmorph_uniq <- unique(cmorph, by = c("lat", "lon"))

ggplot(cmorph_uniq[clusters == "5" | clusters == "2"]) + 
  geom_raster(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "CMORPH") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/cluster/10_all_data_cluster_10_stndised_CMORPH_bimodal.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)

ggplot(cmorph_uniq[clusters == "6" | clusters == "10"]) + 
  geom_raster(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "CMORPH") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/cluster/10_all_data_cluster_10_stndised_CMORPH_midnight.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)

ggplot(cmorph_uniq[clusters == "7" | clusters == "9" | clusters == "3"]) + 
  geom_raster(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "CMORPH") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/cluster/10_all_data_cluster_10_stndised_CMORPH_afternoon.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)

ggplot(cmorph_uniq[clusters == "1" | clusters == "4"| clusters == "8"]) + 
  geom_raster(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "CMORPH") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/cluster/10_all_data_cluster_10_stndised_CMORPH_earlymorning.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)



#PERSIANN
persiann <- processed_data_list$persiann %>% unique(, by = c("lat", "lon"))

ggplot(persiann) + 
  geom_raster(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "PERSIANN") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(persiann[clusters == "10"]) + 
  geom_raster(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "PERSIANN") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(persiann[clusters == "7"]) + 
  geom_raster(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "PERSIANN") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")


ggplot(persiann[clusters == "4" | clusters == "6" |clusters == "8"]) + 
  geom_raster(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "PERSIANN") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")



ggplot(persiann[clusters == "5"]) + 
  geom_raster(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "PERSIANN") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")



#GSMaP
gsmap <- processed_data_list$gsmap %>% unique(, by = c("lat", "lon"))

ggplot(gsmap) + 
  geom_raster(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "gsmap") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggplot(gsmap[clusters == "3"]) + 
  geom_tile(aes(lon, lat, fill = factor(clusters))) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="", y = "", fill = "gsmap") + 
  coord_cartesian(xlim = c(min(cmorph_uniq$lon), max(cmorph_uniq$lon)), 
                  ylim = c(min(cmorph_uniq$lat), max(cmorph_uniq$lat))) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

















to_plot_spat_list <- lapply(names(processed_data_list), function(name) {
  data_dt <- processed_data_list[[name]]
  to_plot <- data_dt[, .(mean_value = mean(value, na.rm = TRUE), 
                        by = .(lat, lon, clusters = as.factor(clusters))]
  to_plot$dataset_name <- factor(name)
  return(to_plot)
})


to_plot_all <- rbindlist(to_plot_list)



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










##############################################################################################

# cluster all data in parellel --------------------------------------------

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(raster)
#library(factoextra)
library(ggplot2)
library(dplyr)
library(sf)
library(hms)
library(forcats)


#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

######################################

# Function to process each dataset
cluster_data <- function(data_dt, k_value = 10, nstart_value = 50) {
  # Set the random seed for reproducibility
  set.seed(123)  # Change the seed value as desired
  # Process the data
  mean_dat <- data_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
  mean_dat[, normalized_value := (value - min(value)) / (max(value) - min(value)), by = .(lat, lon)]
  
  mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]
  
  mean_dat_comp2 <- mean_dat_comp[, .(lat, lon, hour, normalized_value)]
  mean_dat_comp2_wide <- dcast(mean_dat_comp2, lat + lon ~ hour)
  
  # Perform kmeans clustering
  k2 <- kmeans(mean_dat_comp2_wide[, -c("lat", "lon")], centers = k_value, nstart = nstart_value)
  
  #join the cluster to original data
  #mean_dat_comp[, `:=`(normalized_value = NULL)]
  mean_dat_comp[, `:=`(value = NULL)] #insted consider the normalised value and plot
  mean_dat_wide_org <- dcast(mean_dat_comp, lat + lon ~ hour)
  mean_dat_wide_org$clusters <- k2$cluster
  # 
  mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")
  # 
  return(mean_dt_k_long)
}


# Function to process datasets in parallel
process_datasets_parallel <- function(data_list, k_value = 10, nstart_value = 50) {
  num_cores <- detectCores() - 50  # Specify the number of cores to use (adjust if needed)
  cl <- makeCluster(num_cores)
  clusterExport(cl, c("cluster_data"))
  
  processed_data_list <- mclapply(data_list, cluster_data, k_value = k_value, nstart_value = nstart_value)
  stopCluster(cl)
  
  # Assign names to the processed_data_list
  names(processed_data_list) <- names(data_list)
  
  return(processed_data_list)
}

########################

dat_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")

# Process all datasets in parallel
processed_data_list <- process_datasets_parallel(dat_list)

lapply(processed_data_list, summary)
#save the results as a list in .RDS format

#saveRDS(processed_data_list, "./projects/main/data/cluster_K10_all_dat_parellel.RDS")
saveRDS(processed_data_list, "./projects/main/data/cluster_K10_nromalised_all_dat_parellel_setseed123.RDS")


####plot
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

# Assuming you have a vector of 10 distinct colors, replace line_colors with your actual vector
line_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f")

summary(to_plot_all)

ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~dataset_name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

#ggsave("./projects/main/results/cluster/05b_all_data_cluster_4_line_plot.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

#ggsave("./projects/main/results/cluster/05b_all_data_cluster_4_uncert_line_plot.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)

ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  #geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))


ggplot(to_plot_all[dataset_name == "IMERG"], aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  #geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name, scales = "free_y") + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

ggplot(to_plot_all[dataset_name == "CMORPH"], aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  #geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_wrap(~clusters) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))


######################################################################


# for frequency -----------------------------------------------------------

# cluster all data in parellel --------------------------------------------

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(raster)
#library(factoextra)
library(ggplot2)
library(dplyr)
library(sf)
library(hms)
library(forcats)


#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

######################################

# Function to process each dataset
cluster_data <- function(data_dt, k_value = 10, nstart_value = 50) {
  # Set the random seed for reproducibility
  set.seed(123)  # Change the seed value as desired
  # Process the data
  mean_dat <- data_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_freq)]
  mean_dat[, normalized_value := (value - min(value)) / (max(value) - min(value)), by = .(lat, lon)]
  
  mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]
  
  mean_dat_comp2 <- mean_dat_comp[, .(lat, lon, hour, normalized_value)]
  mean_dat_comp2_wide <- dcast(mean_dat_comp2, lat + lon ~ hour)
  
  # Perform kmeans clustering
  k2 <- kmeans(mean_dat_comp2_wide[, -c("lat", "lon")], centers = k_value, nstart = nstart_value)
  
  #join the cluster to original data
  #mean_dat_comp[, `:=`(normalized_value = NULL)]
  mean_dat_comp[, `:=`(value = NULL)] #insted consider the normalised value and plot
  mean_dat_wide_org <- dcast(mean_dat_comp, lat + lon ~ hour)
  mean_dat_wide_org$clusters <- k2$cluster
  # 
  mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")
  # 
  return(mean_dt_k_long)
}


# Function to process datasets in parallel
process_datasets_parallel <- function(data_list, k_value = 10, nstart_value = 50) {
  num_cores <- detectCores() - 50  # Specify the number of cores to use (adjust if needed)
  cl <- makeCluster(num_cores)
  clusterExport(cl, c("cluster_data"))
  
  processed_data_list <- mclapply(data_list, cluster_data, k_value = k_value, nstart_value = nstart_value)
  stopCluster(cl)
  
  # Assign names to the processed_data_list
  names(processed_data_list) <- names(data_list)
  
  return(processed_data_list)
}

########################

dat_list <- readRDS("./projects/main/data/hourly_freq_all_datasets_LST_glob_2001_20.rds")

# Process all datasets in parallel
processed_data_list <- process_datasets_parallel(dat_list)

lapply(processed_data_list, summary)
#save the results as a list in .RDS format

#saveRDS(processed_data_list, "./projects/main/data/cluster_K10_all_dat_parellel.RDS")
saveRDS(processed_data_list, "./projects/main/data/cluster_K10_freq_nromalised_all_dat_parellel_setseed123.RDS")


####

processed_data_list <- readRDS("./projects/main/data/cluster_K10_freq_nromalised_all_dat_parellel_setseed123.RDS")


####plot
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

# Assuming you have a vector of 10 distinct colors, replace line_colors with your actual vector
line_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f")

summary(to_plot_all)

ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~dataset_name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

#ggsave("./projects/main/results/cluster/05b_all_data_cluster_4_line_plot.png", width = 9.5, height = 5.3, 
units = "in", dpi = 600)


ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

#ggsave("./projects/main/results/cluster/05b_all_data_cluster_4_uncert_line_plot.png", width = 9.5, height = 5.3, 
units = "in", dpi = 600)

ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  #geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))


ggplot(to_plot_all[dataset_name == "CMORPH"], aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  #geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_wrap(~clusters) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))


#############################################################


# For Intensity -----------------------------------------------------------

# cluster all data in parellel --------------------------------------------

library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(raster)
#library(factoextra)
library(ggplot2)
library(dplyr)
library(sf)
library(hms)
library(forcats)


#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

######################################

# Function to process each dataset
cluster_data <- function(data_dt, k_value = 10, nstart_value = 50) {
  # Set the random seed for reproducibility
  set.seed(123)  # Change the seed value as desired
  # Process the data
  mean_dat <- data_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_int)]
  mean_dat[, normalized_value := (value - min(value)) / (max(value) - min(value)), by = .(lat, lon)]
  
  mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]
  
  mean_dat_comp2 <- mean_dat_comp[, .(lat, lon, hour, normalized_value)]
  mean_dat_comp2_wide <- dcast(mean_dat_comp2, lat + lon ~ hour)
  
  # Perform kmeans clustering
  k2 <- kmeans(mean_dat_comp2_wide[, -c("lat", "lon")], centers = k_value, nstart = nstart_value)
  
  #join the cluster to original data
  #mean_dat_comp[, `:=`(normalized_value = NULL)]
  mean_dat_comp[, `:=`(value = NULL)] #insted consider the normalised value and plot
  mean_dat_wide_org <- dcast(mean_dat_comp, lat + lon ~ hour)
  mean_dat_wide_org$clusters <- k2$cluster
  # 
  mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")
  # 
  return(mean_dt_k_long)
}


# Function to process datasets in parallel
process_datasets_parallel <- function(data_list, k_value = 10, nstart_value = 50) {
  num_cores <- detectCores() - 50  # Specify the number of cores to use (adjust if needed)
  cl <- makeCluster(num_cores)
  clusterExport(cl, c("cluster_data"))
  
  processed_data_list <- mclapply(data_list, cluster_data, k_value = k_value, nstart_value = nstart_value)
  stopCluster(cl)
  
  # Assign names to the processed_data_list
  names(processed_data_list) <- names(data_list)
  
  return(processed_data_list)
}

########################

dat_list <- readRDS("./projects/main/data/hourly_int_all_datasets_LST_glob_2001_20.rds")

# Process all datasets in parallel
processed_data_list <- process_datasets_parallel(dat_list)

lapply(processed_data_list, summary)
#save the results as a list in .RDS format

#saveRDS(processed_data_list, "./projects/main/data/cluster_K10_all_dat_parellel.RDS")
saveRDS(processed_data_list, "./projects/main/data/cluster_K10_int_nromalised_all_dat_parellel_setseed123.RDS")


####

processed_data_list <- readRDS("./projects/main/data/cluster_K10_freq_nromalised_all_dat_parellel_setseed123.RDS")


####plot
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

# Assuming you have a vector of 10 distinct colors, replace line_colors with your actual vector
line_colors <- c("#1f78b4", "#33a02c", "#e31a1c", "#ff7f00", "#6a3d9a", "#b15928", "#a6cee3", "#b2df8a", "#fb9a99", "#fdbf6f")

summary(to_plot_all)

ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~dataset_name, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  theme(legend.direction = "vertical", legend.position = "right") + 
  theme(legend.position = c(0.8, 0.2)) + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

#ggsave("./projects/main/results/cluster/05b_all_data_cluster_4_line_plot.png", width = 9.5, height = 5.3, 
units = "in", dpi = 600)


ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))

#ggsave("./projects/main/results/cluster/05b_all_data_cluster_4_uncert_line_plot.png", width = 9.5, height = 5.3, 
units = "in", dpi = 600)

ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  #geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))


#########################################################################################

# manually merge the similar clusters and plot ----------------------------


library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(raster)
#library(factoextra)
library(ggplot2)
library(dplyr)
library(sf)
library(hms)
library(forcats)


#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

#read the datasets----

processed_data_list <- readRDS("./projects/main/data/cluster_K10_nromalised_all_dat_parellel_setseed123.RDS")
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

to_plot_all

##

ggplot(to_plot_all, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point(size = 0.5) + 
  geom_line() + 
  #geom_ribbon(aes(ymin=q25, ymax=q75,), fill = "grey70", alpha = 0.5) + 
  facet_grid(clusters~dataset_name) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black')) + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.position = "NULL") + 
  scale_x_discrete(labels = function(x) ifelse(as.numeric(x) %% 4 == 0, x, ""))



##estimate th epeak hours
peak_hour_list <- to_plot_all[, .SD[which.max(mean_value)], by = .(clusters, dataset_name)]




##spatial plots
trl <- processed_data_list$imerg
trl

trl_uniq <- unique(trl, by = c("lat", "lon"))

ggplot(trl_uniq, aes(lon, lat, fill = factor(clusters))) + 
  geom_raster()




all_data <- rbindlist(processed_data_list)

# Find maximum value for each lat, lon, and cluster
max_values <- all_data[, .(max_value = max(value)), by = .(lat, lon, clusters)]

max_values <- all_data[, .(max_value = max(value), 
                           max_hour = hour[which.max(value)]), 
                       by = .(lat, lon, clusters)]


library(data.table)

# Define a function to find maximum value and corresponding hour for each lat, lon, and cluster
find_max_hour <- function(dt) {
  # Find maximum value and corresponding hour for each lat, lon, and cluster
  max_values <- dt[, .(max_value = max(value), 
                       max_hour = hour[which.max(value)]), 
                   by = .(lat, lon, clusters)]
  
  return(max_values)
}

# Apply the function to each data table in the list
max_values_list <- lapply(processed_data_list, find_max_hour)

# View the result
max_values_list


max_values_imerg <- max_values_list$imerg









# Assuming to_plot_all is your data.table
to_plot_all[, hour := as.numeric(hour)]
#to_plot_all[, modified_hour := (hour + 3) %% 24]
to_plot_all[, modified_hour := (hour + 2) %% 24]



# Now you can use modified_hour for clustering
merged_clusters <- to_plot_all[, .(mean_value = mean(mean_value)), by = .(clusters, modified_hour, dataset_name)]

# Identify similar clusters and assign a common label
merged_clusters[, merged_cluster := rleid(mean_value), by = .(modified_hour, dataset_name)]

# Merge the original data with the merged_clusters
to_plot_all[, clusters := as.character(clusters)]
# Assuming 'mean_value' is the common column
merged_data <- merge(to_plot_all, merged_clusters[, .(clusters, modified_hour, dataset_name, mean_value, merged_cluster)], 
                     by = c("clusters", "modified_hour", "dataset_name", "mean_value"))

# Drop unnecessary columns
merged_data[, c("hour", "mean_value", "q25", "q75") := NULL]

# Rename the columns if needed
colnames(merged_data) <- c("modified_hour", "clusters", "q25", "q75", "dataset_name", "merged_cluster")

# Display the merged data
print(merged_data)
 
View(merged_data[dataset_name == "IMERG" & clusters == 8])
View(merged_data[dataset_name == "IMERG"])

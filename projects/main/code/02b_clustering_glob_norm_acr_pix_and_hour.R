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
library(cluster)
library(factoextra)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


# for mean precip --------------------------------------------------------------------

#cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001.rds")
cmorph_dt <- readRDS("./projects/main/data/hourly_mean_cmorph_glob_LST_2001_20.rds")

mean_dat <- cmorph_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)

mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE)]
summary(mean_dat)

mean_dat[, normalized_value2 := scale(normalized_value, center = TRUE, scale = FALSE), by = .(hour)]

#mean_dat[, `:=`(value = NULL)]
mean_dat[, `:=`(value = NULL, normalized_value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]
summary(mean_dat_wide)

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

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/02b_clusters_mean_cmorph_2001_20_n50.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line() + 
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

ggsave("./projects/main/results/02b_clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
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

ggsave("./projects/main/results/02b_clusters_mean_spat_cmorph_2001_20_n50.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)



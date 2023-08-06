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
#persiann_dt <- readRDS("./projects/main/data/hourly_mean_persiannf_glob_LST_2001_20.rds")
all_dt <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")

cmorph_dt <- all_dt$cmorph

mean_dat <- all_dt$cmorph[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
summary(mean_dat)


mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]
summary(mean_dat)

mean_dat[, `:=`(value = NULL)]

mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]
summary(mean_dat_wide)

mean_dat_wide_comp <- mean_dat_wide[complete.cases(mean_dat_wide), ]
summary(mean_dat_wide_comp)


cw.gap<-clusGap(mean_dat_wide_comp,FUN=kmeans,K.max=10,B=500,d.power=2)
fviz_gap_stat(cw.gap,
              maxSE=list(method='Tibs2001SEmax',SE.factor=1))

#fviz_nbclust(mean_dat_wide, kmeans, method = "wss")
###parellise
library(doParallel)
library(foreach)

n_clusters <- 20
wss <- numeric(n_clusters)

set.seed(123)

# Register parallel backend with desired number of cores
cores <- detectCores() - 30
cl <- makeCluster(cores)
registerDoParallel(cl)

# Perform parallel computation
wss <- foreach(i = 1:n_clusters, .combine = "c") %dopar% {
  km.out <- kmeans(mean_dat_wide, centers = i, nstart = 20)
  km.out$tot.withinss
}

# Stop the parallel backend
stopCluster(cl)

# Produce a scree plot
wss_df <- data.frame(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)) + 
  scale_y_log10() + 
  xlab('Number of clusters')
scree_plot

scree_plot +
  geom_hline(
    yintercept = wss, 
    linetype = 'dashed', 
    #col = c(rep('#000000',4),'#FF0000', rep('#000000', 5))
  )


# Use map_dbl to run many models with varying value of k
sil_width <- map_dbl(2:10,  function(k){
  model <- pam(x = scaled_data, k = k)
  model$silinfo$avg.width
})

#### Generate a data frame containing both k and sil_width-----

n_clusters <- 10
wss <- numeric(n_clusters)

set.seed(123)

# Register parallel backend with desired number of cores
cores <- detectCores() - 30
cl <- makeCluster(cores)
registerDoParallel(cl)

# Perform parallel computation
wss <- foreach(i = 1:n_clusters, .combine = "c", .packages = c("cluster")) %dopar% {
  km.out <- clusGap(mean_dat_wide, FUN = kmeans, k = i, nstart = 20, K.max = 10, B = 50)
  km.out
}

# Stop the parallel backend
stopCluster(cl)

# Produce a scree plot
wss_df <- data.frame(clusters = 2:n_clusters, wss = wss)

ggplot(wss_df, aes(x = clusters, y = wss)) +
  geom_line() + geom_point()+
  scale_x_continuous(breaks = 1:10)

#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide_comp, centers = 6, nstart = 50)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

# mean_dat <- persiann_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
# mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]

# 
# mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
# summary(mean_dat)
# mean_dat_cop <- mean_dat
#mean_dat[, `:=`(value = NULL)]

mean_dat <- all_dt$cmorph[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]

mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]

mean_dat_comp2 <- mean_dat_comp[, .(lat, lon, hour, value)]
mean_dat_comp2_wide <- dcast(mean_dat_comp2, lat + lon ~ hour)

mean_dat_comp2_wide$clusters <- k_means

summary(mean_dat_comp2_wide)

saveRDS(mean_dat_comp2_wide, "./projects/main/results/cluster/cmorph_clusters_6.RDS")

#############

mean_dat_comp2_wide <- readRDS("./projects/main/results/cluster/cmorph_clusters_6.RDS")

mean_dt_k_long <- melt(mean_dat_comp2_wide, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "Clusters") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/main/results/cluster/cmorph_mean_cmorph_2001_20_n50.png", width = 9.5, height = 5.3, 
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

ggsave("./projects/main/results/cluster/cmorph_clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
       units = "in", dpi = 600)


to_plot <- mean_dt_k_long[, .(mean = mean(value, na.rm = TRUE), 
                              q25 = quantile(value, 0.25, na.rm = TRUE), q50 = quantile(value, 0.50, na.rm = TRUE), 
                              q75 = quantile(value, 0.75, na.rm = TRUE), q90 = quantile(value, 0.90, na.rm = TRUE)), 
                          by = .(hour, clusters = as.factor(clusters))]


to_plot2 <- data.table::melt(to_plot, c("hour", 'clusters'))


ggplot(to_plot2, aes(hour, value, col = factor(variable), group = factor(variable))) + 
  geom_point() + 
  geom_line() + 
  facet_wrap(~clusters, scales = "free_y", ncol = 3) + 
  #facet_wrap(~clusters, ncol = 3) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)", fill = "") + 
  theme_small + 
  #scale_x_continuous(breaks=seq(0, 23, 3)) + 
  theme(legend.title=element_blank()) + 
  theme(axis.text.x=element_text(color=c("black","transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black", 
                                                "transparent","transparent","black")))

ggsave("./projects/main/results/cluster/cmorph_clusters_mean_quantile_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
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

ggsave("./projects/main/results/cluster/cmorph_clusters_mean_spat_cmorph_2001_20_n50.png", width = 9.9, height = 4.6, 
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

ggsave("./projects/main/results/02d_clusters_mean_cmorph_2001_20_n50.png", width = 9.5, height = 5.3, 
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

ggsave("./projects/main/results/02d_clusters_mean_line_cmorph_2001_20_n50.png", width = 10.5, height = 5.3, 
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

ggsave("./projects/main/results/02d_clusters_mean_spat_cmorph_2001_20_n50.png", width = 9.9, height = 4.6, 
       units = "in", dpi = 600)

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

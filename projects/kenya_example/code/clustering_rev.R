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


## read the data sets -------------------------------

#dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_freq_thres_list.RDS")
dat_thres_list_new <- readRDS("./projects/kenya_example/data/output/diurnal_int_freq_thres_list_new.RDS")


## Pre-process -----------------------------------------------

dat_list <- unlist(dat_thres_list_new, recursive=F) %>% unlist(, recursive=F)

# change the time to LST 
# converting the time from utc to LST for a list of data.tables
dat_lst_list <- lapply(dat_list, function(dt) {
  dt$date <- substr(dt$date, 7, 8) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), value, name, variable, threshold)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})

#############################################################


# mean --------------------------------------------------------------------

mean <- dat_lst_list$mean

mean_dat <- mean[, .(lat, lon, hour = factor(hour(time_lst)), value)]
summary(mean_dat)

mean_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
mean_dat[, `:=`(value = NULL)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

#mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = FALSE)

fviz_nbclust(mean_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)
set.seed(123)
k2 <- kmeans(mean_dat_wide, centers = 5, nstart = 1500)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

mean_dat_wide_org$clusters <- k_means

mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/kenya_example/results/clusters_mean_kenya.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)



to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]
ggplot(to_plot_spat) + 
  geom_raster(aes(lon, lat, fill = clusters)) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="Longitude", y = "Latitude") + 
  coord_cartesian(xlim = c(min(to_plot_spat$lon), max(to_plot_spat$lon)), 
                  ylim = c(min(to_plot_spat$lat), max(to_plot_spat$lat))) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/kenya_example/results/clusters_mean_spat_kenya.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


mean_dt_k_long[clusters == "1"]

#verify
peak_night <- mean_dt_k_long[clusters == "1"]

ggplot(subset(peak_night), aes(hour, value)) + 
  geom_point(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small

ggplot(subset(peak_night, lat %in% c(-1.375, -1.125)), aes(hour, value)) + 
  geom_point(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small


peak_night <- mean_dt_k_long[clusters == "4"]
ggplot(subset(peak_night), aes(hour, value)) + 
  geom_point(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small


# frequency ---------------------------------------------------------------

freq <- dat_lst_list$frequency

freq_dat <- freq[, .(lat, lon, hour = factor(hour(time_lst)), value)]

freq_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
summary(freq_dat)

freq_dat[, `:=`(value = NULL)]
freq_dat_wide_org <- dcast(freq_dat, lat + lon ~ hour)

freq_dat_wide <- freq_dat_wide_org[, !c("lat", "lon")]

#freq_dat_wide_scaled <- scale(freq_dat_wide, center = TRUE, scale = FALSE)

fviz_nbclust(freq_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(freq_dat_wide_scaled, 5)

k2 <- kmeans(freq_dat_wide, centers = 5, nstart = 1500)
str(k2)
k2

# contains the vector of freqegers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

freq_dat_wide_org$clusters <- k_means

freq_dt_k_long <- melt(freq_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- freq_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/kenya_example/results/clusters_freq_kenya.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)



to_plot_spat <- freq_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                              by = .(lat, lon, clusters = as.factor(clusters))]
ggplot(to_plot_spat) + 
  geom_raster(aes(lon, lat, fill = clusters)) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="Longitude", y = "Latitude") + 
  coord_cartesian(xlim = c(min(to_plot_spat$lon), max(to_plot_spat$lon)), 
                  ylim = c(min(to_plot_spat$lat), max(to_plot_spat$lat))) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/kenya_example/results/clusters_freq_spat_kenya.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


mean_dt_k_long[clusters == "1"]

#verify
peak_night <- mean_dt_k_long[clusters == "1"]

ggplot(subset(peak_night), aes(hour, value)) + 
  geom_pofreq(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small

ggplot(subset(peak_night, lat %in% c(-1.375, -1.125)), aes(hour, value)) + 
  geom_pofreq(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small


peak_night <- mean_dt_k_long[clusters == "4"]
ggplot(subset(peak_night), aes(hour, value)) + 
  geom_point(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small


# intensity ---------------------------------------------------------------

int <- dat_lst_list$intensity

int_dat <- int[, .(lat, lon, hour = factor(hour(time_lst)), value)]
summary(int_dat)

int_dat[, normalized_value := scale(value, center = TRUE, scale = FALSE), by = .(hour)]
int_dat[, `:=`(value = NULL)]
int_dat_wide_org <- dcast(int_dat, lat + lon ~ hour)

int_dat_wide <- int_dat_wide_org[, !c("lat", "lon")]

#int_dat_wide_scaled <- scale(int_dat_wide, center = TRUE, scale = FALSE)

fviz_nbclust(int_dat_wide, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(int_dat_wide_scaled, 5)

k2 <- kmeans(int_dat_wide, centers = 5, nstart = 1500)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

int_dat_wide_org$clusters <- k_means

int_dt_k_long <- melt(int_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- int_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "Precipitation (mm/hr)") + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/kenya_example/results/clusters_int_kenya.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)



to_plot_spat <- int_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), 
                               by = .(lat, lon, clusters = as.factor(clusters))]
ggplot(to_plot_spat) + 
  geom_raster(aes(lon, lat, fill = clusters)) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  labs(x ="Longitude", y = "Latitude") + 
  coord_cartesian(xlim = c(min(to_plot_spat$lon), max(to_plot_spat$lon)), 
                  ylim = c(min(to_plot_spat$lat), max(to_plot_spat$lat))) + 
  theme_generic + 
  theme( legend.direction = "vertical", legend.position = "right")

ggsave("./projects/kenya_example/results/clusters_int_spat_kenya.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


mean_dt_k_long[clusters == "1"]

#verify
peak_night <- mean_dt_k_long[clusters == "1"]

ggplot(subset(peak_night), aes(hour, value)) + 
  geom_point(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small

ggplot(subset(peak_night, lat %in% c(-1.375, -1.125)), aes(hour, value)) + 
  geom_point(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small


peak_night <- mean_dt_k_long[clusters == "4"]
ggplot(subset(peak_night), aes(hour, value)) + 
  geom_point(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small

##################################################

# Decide how many clusters to look at

##Elbow method-------------------------------------
n_clusters <- 10

# Initialize total within sum of squares error: wss
wss <- numeric(n_clusters)

set.seed(123)

# Look over 1 to n possible clusters
for (i in 1:n_clusters) {
  # Fit the model: km.out
  km.out <- kmeans(mean_dat_wide, centers = i, nstart = 20)
  # Save the within cluster sum of squares
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
wss_df <- tibble(clusters = 1:n_clusters, wss = wss)

scree_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')
scree_plot


###parellise
library(doParallel)
library(foreach)

n_clusters <- 10
wss <- numeric(n_clusters)

set.seed(123)

# Register parallel backend with desired number of cores
cores <- detectCores()
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
ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
  geom_point(size = 4)+
  geom_line() +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 10)) +
  xlab('Number of clusters')


#########another method-------------------------------------------

gap.stat <- clusGap(mean_dat_wide, FUNcluster = kmeans, K.max = 15)
gap.stat

#plot
fviz_gap_stat(gap.stat)


trl <- clusGap(mean_dat_wide, FUNcluster = kmeans, K.max = 15, B = 100,
        do_parallel = TRUE, parallel::mclapply, option(mc.cores = 20))


#parallise the above ---------------------

n_clusters <- 11
wss <- numeric(n_clusters)

set.seed(123)

# Register parallel backend with desired number of cores
cores <- detectCores() - 30
cl <- makeCluster(cores)
registerDoParallel(cl)

# Perform parallel computation

mean_dat_wide  # Your dataset here
K_values <- 1:15  # Range of K values to evaluate


results <- foreach(K = K_values, .combine = "c", .packages = c("cluster")) %dopar% {
  clusGap(mean_dat_wide, FUNcluster = kmeans, K.max = 10)
}


stopCluster(cl)

#plot
fviz_gap_stat(results$Tab)



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


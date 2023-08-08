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

#dat_list <- lapply(dat_thres_list, rbindlist)
#dat_list <- dat_thres_list
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

mean <- dat_lst_list$mean
# summary(mean)
# 
# write.csv(mean, "./mean.csv")


mean_dat <- mean[, .(lat, lon, hour = factor(hour(time_lst)), value)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

# as_tibble(mean_dat_wide)
# pivot<-cbind(offers,mean_dat_wide[-1])


mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]

mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = TRUE)


# wssplot <- function(data, nc=15, seed=1234){
#   wss <- (nrow(data)-1)*sum(apply(data,2,var))
#   for (i in 2:nc){
#     set.seed(seed)
#     wss[i] <- sum(kmeans(data, centers=i)$withinss)}
#   plot(1:nc, wss, type="b", xlab="Number of Clusters",
#        ylab="Within groups sum of squares")}
# 
# wssplot(mean_dat_wide)
# wssplot(mean_dat_wide_scaled)


fviz_nbclust(mean_dat_wide_scaled, kmeans, method = "wss")


#k_mean algo
#k2 <- pam(mean_dat_wide_scaled, 5)

k2 <- kmeans(mean_dat_wide, centers = 5, nstart = 2000)
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

ggsave("./projects/kenya_example/results/clusters_kenya.png", width = 9.5, height = 5.3, 
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

ggsave("./projects/kenya_example/results/clusters_spat_kenya.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


mean_dt_k_long[clusters == "1"]

#verify
peak_night <- mean_dt_k_long[clusters == "2"]
ggplot(subset(peak_night, lat %in% c(-1.375, -1.125)), aes(hour, value)) + 
  geom_point(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small


peak_night <- mean_dt_k_long[clusters == "3"]
ggplot(subset(peak_night, lon %in% c(38.125, 40.875)), aes(hour, value)) + 
  geom_point(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small


peak_night_mean <- peak_night[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]
ggplot(peak_night_mean, aes(hour, mean_value)) + 
  geom_point() + 
  #facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small

vef_dat <- mean_dat[lat == "0.125" & lon == "33.875"]

vef_to_plot <- vef_dat[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour)]

ggplot(vef_to_plot, aes(hour, mean_value)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "")


# for cluster 1
clust_1 <- mean_dt_k_long[clusters == "1"]
ggplot(clust_1[1:30], aes(hour, value)) + 
  geom_point(size = 0.85, col = "red") + 
  facet_wrap(~lat + lon) + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small

# by max and min hour -----------------------------------------------------

# Estimate the peak hour of data.tables is called "data_list" -----------------

peak_hour_list <- lapply(dat_lst_list, function(dt) {
  # Modify the code to use the data.table "int_freq"
  peak_hour <- dt[, .SD[which.max(value)], by = .(lat, lon, name, variable, threshold)]
  peak_hour <- peak_hour[, .(lat, lon, peak_hour = hour(time_lst),name, variable, threshold)]
  return(peak_hour)
})

min_hour_list <- lapply(dat_lst_list, function(dt) {
  # Modify the code to use the data.table "int_freq"
  min_hour <- dt[, .SD[which.min(value)], by = .(lat, lon, name, variable, threshold)]
  min_hour <- min_hour[, .(lat, lon, min_hour = hour(time_lst),name, variable, threshold)]
  return(min_hour)
})
## peak hour of intensity and frequency plot -----------------------------------

peak_hour_dt <- rbindlist(peak_hour_list)
min_hour_dt <- rbindlist(min_hour_list)

peak_hour_mean <- peak_hour_dt[variable == "mean" & name == "imerg" & threshold == "0.1"]
min_hour_mean <- min_hour_dt[variable == "mean" & name == "imerg" & threshold == "0.1"]

##############

mean_dat <- mean[, .(lat, lon, hour = factor(hour(time_lst)), value)]
mean_dat_wide_org <- dcast(mean_dat, lat + lon ~ hour)

mean_dat_wide_org$peak_hour <- peak_hour_mean$peak_hour
mean_dat_wide_org$min_hour <- min_hour_mean$min_hour

#mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]
mean_dat_wide <- mean_dat_wide_org[, c("peak_hour")]

mean_dat_wide_scaled <- scale(mean_dat_wide, center = TRUE, scale = TRUE)


fviz_nbclust(mean_dat_wide_scaled, kmeans, method = "wss")


#k_mean algo
k2 <- kmeans(mean_dat_wide_scaled, centers = 4, nstart = 200)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

mean_dat_wide_org$clusters <- k_means

mean_dat_wide_org_clust <- mean_dat_wide_org[, !c("peak_hour", "min_hour")]

mean_dt_k_long <- melt(mean_dat_wide_org_clust, c("lat", 'lon', 'clusters'), variable.name = "hour")

#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(to_plot, aes(hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(lat, lon, clusters = as.factor(clusters))]
ggplot(to_plot_spat) + 
  geom_raster(aes(lon, lat, fill = clusters)) + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  coord_cartesian(xlim = c(min(to_plot_spat$lon), max(to_plot_spat$lon)), 
                  ylim = c(min(to_plot_spat$lat), max(to_plot_spat$lat))) + 
  theme_small



# only for peak hour ------------------------------------------------------

peak_dat <- peak_hour_mean[, .(lat, lon, peak_hour)]
peak_dat_wide_org <- dcast(peak_dat, lat + lon ~ peak_hour)

mean_dat_wide_org$peak_hour <- peak_hour_mean$peak_hour
mean_dat_wide_org$min_hour <- min_hour_mean$min_hour

#mean_dat_wide <- mean_dat_wide_org[, !c("lat", "lon")]
mean_dat_wide <- mean_dat_wide_org[, c("peak_hour")]

mean_dat_wide_scaled <- scale(peak_dat, center = TRUE, scale = TRUE)


fviz_nbclust(mean_dat_wide_scaled, kmeans, method = "wss")


#k_mean algo
k2 <- kmeans(mean_dat_wide_scaled, centers = 4, nstart = 200)
str(k2)
k2

# contains the vector of integers indicating the assignment of each observation to a particular cluster
k_means = k2$cluster

peak_dat$clusters <- k_means


#plot--
to_plot <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour, clusters = as.factor(clusters))]

ggplot(peak_dat, aes(peak_hour, mean_value, col = clusters, group = clusters)) + 
  geom_point() + 
  geom_line(linewidth = 1.2) + 
  labs(x ="Hour (LST)", y = "") + 
  theme_small

to_plot_spat <- mean_dt_k_long[, .(mean_value = mean(value, na.rm = TRUE)), by = .(lat, lon, clusters = as.factor(clusters))]
ggplot(peak_dat) + 
  geom_raster(aes(lon, lat, fill = clusters)) + 
  scale_fill_binned(type = "viridis") + 
  #scale_fill_manual(values = rainbow(24)) + 
  borders(colour = "black") + 
  coord_cartesian(xlim = c(min(peak_dat$lon), max(peak_dat$lon)), 
                  ylim = c(min(peak_dat$lat), max(peak_dat$lat))) + 
  theme_small



library(data.table)
library(dplyr)
library(foreach)
library(doParallel)
library(raster)
library(factoextra)
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
cluster_data <- function(data_dt, k_value = 4, nstart_value = 50) {
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
  mean_dat_comp[, `:=`(normalized_value = NULL)]
  mean_dat_wide_org <- dcast(mean_dat_comp, lat + lon ~ hour)
  mean_dat_wide_org$clusters <- k2$cluster
  # 
  mean_dt_k_long <- melt(mean_dat_wide_org, c("lat", 'lon', 'clusters'), variable.name = "hour")
  # 
  return(mean_dt_k_long)
}


# Function to process datasets in parallel
process_datasets_parallel <- function(data_list, k_value = 4, nstart_value = 50) {
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

#save the results as a list in .RDS format

#saveRDS(processed_data_list, "./projects/main/data/cluster_all_dat_parellel.RDS")
saveRDS(processed_data_list, "./projects/main/data/cluster_4_all_dat_parellel_setseed123.RDS")


#######################################


mean_dat <- dat_list$cmorph[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]

mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]

mean_dat_comp2 <- mean_dat_comp[, .(lat, lon, hour, value, normalized_value = NULL)]
mean_dat_comp2_wide <- dcast(mean_dat_comp2, lat + lon ~ hour)

mean_dat_comp2_wide$clusters <- processed_data_list$cmorph$cluster #k2$cluster


mean_dt_k_long <- melt(mean_dat_comp2_wide, c("lat", 'lon', 'clusters'), variable.name = "hour")

#######################truy the plots-------

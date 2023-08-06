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
process_dataset <- function(data_dt, k_value = 6, nstart_value = 50) {
  # Read the data from RDS file
  #cmorph_dt <- readRDS(rds_file_path)
  
  # Process the data
  mean_dat <- data_dt[, .(lat, lon, hour = factor(hour(time_lst)), value = prec_mean)]
  mean_dat[, normalized_value := scale(value, center = TRUE, scale = TRUE), by = .(lat, lon)]
  
  mean_dat_comp <- mean_dat[complete.cases(mean_dat), ]
  
  mean_dat_comp2 <- mean_dat_comp[, .(lat, lon, hour, value, normalized_value = NULL)]
  mean_dat_comp2_wide <- dcast(mean_dat_comp2, lat + lon ~ hour)
  
  # Perform kmeans clustering
  k2 <- kmeans(mean_dat_comp2_wide[, -c("lat", "lon")], centers = k_value, nstart = nstart_value)
  
  mean_dat_comp2_wide$clusters <- k2$cluster
  
  mean_dt_k_long <- melt(mean_dat_comp2_wide, c("lat", 'lon', 'clusters'), variable.name = "hour")
  
  return(mean_dt_k_long)
}

##########################################


# Assuming 'dat_list' contains the data in the format you provided
# dat_list <- list(imergf = data.table(...), gsmap = data.table(...), ...)
dat_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")


# Set the number of cores to use for parallel processing
num_cores <- detectCores() - 50  # You can adjust the number of cores as needed

# Register the parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Export the 'process_cmorph_data' function to each worker
clusterExport(cl, "process_cmorph_data")

# # Function to process each dataset in parallel
# process_datasets_parallel <- function(data_list, k_value = 6, nstart_value = 50) {
#   result_list <- foreach(data_dt = data_list, .packages = c("data.table", "dplyr", "cluster")) %dopar% { 
#     # Load required packages within the parallel environment
#     library(data.table)
#     library(dplyr)
#     library(cluster)
#     # Process each dataset using the 'process_cmorph_data' function
#     process_cmorph_data(data_dt, k_value = k_value, nstart_value = nstart_value)
#   }
#   return(result_list)
# }
# 

# Function to process datasets in parallel
process_datasets_parallel <- function(data_list, k_value = 6, nstart_value = 50) {
  num_cores <- detectCores() - 50  # Specify the number of cores to use (adjust if needed)
  cl <- makeCluster(num_cores)
  clusterExport(cl, c("process_dataset"))
  
  processed_data_list <- mclapply(data_list, process_dataset, k_value = k_value, nstart_value = nstart_value)
  stopCluster(cl)
  
  # Assign names to the processed_data_list
  names(processed_data_list) <- names(data_list)
  
  return(processed_data_list)
}

# Process all datasets in parallel
processed_data_list <- process_datasets_parallel(dat_list)

saveRDS(processed_data_list, "./projects/main/data/cluster_all_dat_parellel.RDS")

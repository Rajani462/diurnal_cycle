# Load required libraries
library(raster)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)

# Define the file paths for multiple datasets
file_paths <- c(
  "./projects/main/data/hourly_int_imerg_glob_0.1_2001_20_fliptrans.nc",
  "./projects/main/data/hourly_int_gsmap_glob_0.1_2015_20_fliptrans.nc", 
  "./projects/main/data/hourly_int_cmorph_glob_0.1_2001_20.nc", 
  "./projects/main/data/hourly_int_persiann_glob_0.1_2001_20.nc", 
  "./projects/main/data/hourly_int_era5_glob_0.1_2001_20.nc"
  # Add more file paths for additional datasets as needed
)

# Set the number of cores to use for parallel processing
num_cores <- detectCores() - 50

# Register the parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to extract the dataset name from the file path
extract_dataset_name <- function(file_path) {
  # Extract the dataset name after 'hourly_mean_'
  start_index <- regexpr("hourly_int_", file_path) + nchar("hourly_int_")
  end_index <- regexpr("_glob", file_path, fixed = TRUE)
  dataset_name <- substr(file_path, start_index, end_index - 1)
  return(dataset_name)
}

# Function to process each dataset

process_dataset <- function(file_path) {
  dataset_name <- extract_dataset_name(file_path)
  
  if (dataset_name == "persiann") {
    # Additional preprocessing specific to "persiann" dataset
    persiann <- brick(file_path)
    pers_time <- getZ(persiann)
    posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00")
    names(persiann) <- posixct_time
    
    # Continue with the main processing steps
    dataset_dt <- as.data.frame(persiann, xy = TRUE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>%
      `[`(, name := factor(dataset_name))
  } else {
    # Perform the main processing steps similar to other datasets
    dataset <- brick(file_path)
    dataset_dt <- as.data.frame(dataset, xy = TRUE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>%
      `[`(, name := factor(dataset_name))
  }
  
  return(dataset_dt)
}


results <- foreach(file_path = file_paths, .combine = "c", .packages = c("raster", "dplyr", "data.table")) %dopar% {
  dataset_name <- extract_dataset_name(file_path)
  list(dataset_name = process_dataset(file_path))
}

# Stop the parallel backend
stopCluster(cl)

# Save the results as a list in a single RDS file

#output_list <- rbindlist(results, use.names = TRUE, fill = TRUE)

output_list <- list()
for (i in seq_along(results)) {
  output_list[[extract_dataset_name(file_paths[i])]] <- results[[i]]
}

#saveRDS(output_list, "./projects/main/data/hourly_int_all_datasets_2001_20.rds")


#####################################

library(hms)
# converting the time from utc to LST for a list of data.tables

dat_lst_list <- lapply(output_list, function(dt) {
  dt$date <- substr(dt$date, 13, 14) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_int, name)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})

saveRDS(dat_lst_list, "./projects/main/data/hourly_int_all_datasets_LST_glob_0.1_2001_20.rds")

##################################################################################################

#For Ocean --------------------------------------------------------------------------
library(raster)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)
library(hms)

# Define the file paths for multiple datasets
file_paths <- c(
  "./projects/main/data/hourly_mean_imerg_ocn_2001_20.nc",
  "./projects/main/data/hourly_mean_gsmap_ocn_2015_20.nc", 
  "./projects/main/data/hourly_mean_cmorph_ocn_2001_20.nc", 
  "./projects/main/data/hourly_mean_persiann_ocn_2001_20.nc", 
  "./projects/main/data/hourly_mean_era5_ocn_2001_20.nc"
  # Add more file paths for additional datasets as needed
)

# Set the number of cores to use for parallel processing
num_cores <- detectCores() - 50

# Register the parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to extract the dataset name from the file path
extract_dataset_name <- function(file_path) {
  # Extract the dataset name after 'hourly_mean_'
  start_index <- regexpr("hourly_mean_", file_path) + nchar("hourly_mean_")
  end_index <- regexpr("_ocn", file_path, fixed = TRUE)
  dataset_name <- substr(file_path, start_index, end_index - 1)
  return(dataset_name)
}


# Function to process each dataset
process_dataset <- function(file_path) {
  dataset_name <- extract_dataset_name(file_path)
  
  if (dataset_name == "persiann") {
    # Additional preprocessing specific to "persiann" dataset
    persiann <- brick(file_path)
    pers_time <- getZ(persiann)
    posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00")
    names(persiann) <- posixct_time
    
    # Continue with the main processing steps
    dataset_dt <- as.data.frame(persiann, xy = TRUE, na.rm = TRUE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
      `[`(, name := factor(dataset_name)) %>%
      `[`(, type := factor("ocn"))
  } else {
    # Perform the main processing steps similar to other datasets
    dataset <- brick(file_path)
    dataset_dt <- as.data.frame(dataset, xy = TRUE, na.rm = TRUE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
      `[`(, name := factor(dataset_name)) %>%
      `[`(, type := factor("ocn"))
  }
  
  return(dataset_dt)
}


# Use foreach to process multiple datasets in parallel
results <- foreach(file_path = file_paths, .combine = "c", .packages = c("raster", "dplyr", "data.table")) %dopar% {
  dataset_name <- extract_dataset_name(file_path)
  list(dataset_name = process_dataset(file_path))
}

# Stop the parallel backend
stopCluster(cl)

# Save the results as a list in a single RDS file

#output_list <- rbindlist(results, use.names = TRUE, fill = TRUE)

output_list <- list()
for (i in seq_along(results)) {
  output_list[[extract_dataset_name(file_paths[i])]] <- results[[i]]
}


# converting the time from utc to LST for a list of data.tables

dat_lst_list <- lapply(output_list, function(dt) {
  dt$date <- substr(dt$date, 13, 14) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_mean, name, type)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})

saveRDS(dat_lst_list, "./projects/main/data/hourly_mean_all_datasets_LST_ocn_2001_20.rds")


#For Land --------------------------------------------------------------------------
library(raster)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)
library(hms)

# Define the file paths for multiple datasets
file_paths <- c(
  "./projects/main/data/hourly_mean_imerg_land_2001_20.nc",
  "./projects/main/data/hourly_mean_gsmap_land_2015_20.nc", 
  "./projects/main/data/hourly_mean_cmorph_land_2001_20.nc", 
  "./projects/main/data/hourly_mean_persiann_land_2001_20.nc", 
  "./projects/main/data/hourly_mean_era5_land_2001_20.nc"
  # Add more file paths for additional datasets as needed
)

# Set the number of cores to use for parallel processing
num_cores <- detectCores() - 50

# Register the parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to extract the dataset name from the file path
extract_dataset_name <- function(file_path) {
  # Extract the dataset name after 'hourly_mean_'
  start_index <- regexpr("hourly_mean_", file_path) + nchar("hourly_mean_")
  end_index <- regexpr("_land", file_path, fixed = TRUE)
  dataset_name <- substr(file_path, start_index, end_index - 1)
  return(dataset_name)
}


# Function to process each dataset
process_dataset <- function(file_path) {
  dataset_name <- extract_dataset_name(file_path)
  
  if (dataset_name == "persiann") {
    # Additional preprocessing specific to "persiann" dataset
    persiann <- brick(file_path)
    pers_time <- getZ(persiann)
    posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00")
    names(persiann) <- posixct_time
    
    # Continue with the main processing steps
    dataset_dt <- as.data.frame(persiann, xy = TRUE, na.rm = TRUE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
      `[`(, name := factor(dataset_name)) %>%
      `[`(, type := factor("land"))
  } else {
    # Perform the main processing steps similar to other datasets
    dataset <- brick(file_path)
    dataset_dt <- as.data.frame(dataset, xy = TRUE, na.rm = TRUE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
      `[`(, name := factor(dataset_name)) %>%
      `[`(, type := factor("land"))
  }
  
  return(dataset_dt)
}


# Use foreach to process multiple datasets in parallel
results <- foreach(file_path = file_paths, .combine = "c", .packages = c("raster", "dplyr", "data.table")) %dopar% {
  dataset_name <- extract_dataset_name(file_path)
  list(dataset_name = process_dataset(file_path))
}

# Stop the parallel backend
stopCluster(cl)

# Save the results as a list in a single RDS file

#output_list <- rbindlist(results, use.names = TRUE, fill = TRUE)

output_list <- list()
for (i in seq_along(results)) {
  output_list[[extract_dataset_name(file_paths[i])]] <- results[[i]]
}


# converting the time from utc to LST for a list of data.tables

dat_lst_list <- lapply(output_list, function(dt) {
  dt$date <- substr(dt$date, 13, 14) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_mean, name, type)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})

saveRDS(dat_lst_list, "./projects/main/data/hourly_mean_all_datasets_LST_land_2001_20.rds")



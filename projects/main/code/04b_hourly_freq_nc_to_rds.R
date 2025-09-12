
# for global without season ----------------------------------------------------------

library(raster)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)
library(hms)

datasets <- c("imerg", "imergv07", "gsmap", "cmorph", "persiann", "era5")
file_paths <- sprintf("~/shared/data_projects/diurnal_precip/processed/hourly_freq_%s.nc", datasets)


# Set the number of cores to use for parallel processing
num_cores <- detectCores() - 50

# Register the parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to extract the dataset name from the file path
extract_dataset_name <- function(file_path) {
  # Extract the dataset name after 'hourly_freq_'
  start_index <- regexpr("hourly_freq_", file_path) + nchar("hourly_freq_")
  end_index <- regexpr(".nc", file_path, fixed = TRUE)
  dataset_name <- substr(file_path, start_index, end_index - 1)
  return(dataset_name)
}


# Function to process each dataset
process_dataset <- function(file_path) { 
  
  dataset_name <- extract_dataset_name(file_path)
  
  dataset <- brick(file_path)
  
  if (dataset_name == "persiann") {
    pers_time <- getZ(dataset)
    posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00")
    names(dataset) <- posixct_time
  }
  
  dataset_dt <- as.data.frame(dataset, xy = TRUE, na.rm = TRUE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>%
    `[`(, name := factor(dataset_name))
  
  return(dataset_dt)
}


# Use foreach to process multiple datasets in parallel
results <- foreach(file_path = file_paths, .packages = c("raster", "dplyr", "data.table")) %dopar% {
  process_dataset(file_path)
}

# Stop the parallel backend
stopCluster(cl)

names(results) <- extract_dataset_name(file_paths)

#read the mask datasets (topo = 1 ocean and 0 for land) to identify land nd ocen pixels
mask_raster <- brick("~/rajani/diurnal_cycle/mask_land.nc", varname="topo")
mask_table <- as.data.frame(mask_raster, xy = TRUE) %>% as.data.table()

mask_table[, `:=`(location = factor("land"))]
mask_table[layer  == "1", `:=`(location = factor("ocean"))]

summary(mask_table)


# Apply the merge operation to each dataset in the output_list
merged_output_list <- lapply(results, function(dataset) merge(dataset, mask_table, by = c("x", "y")))


# converting the time from utc to LST for a list of data.tables

library(hms) 

dat_lst_list <- lapply(merged_output_list, function(dt) {
  dt$date <- substr(dt$date, 13, 14) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_freq, name, location)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})

saveRDS(dat_lst_list, "./projects/main/data/hourly_freq_all_datasets_LST_glob_2001_20.rds")

#################################



# for seasonal ------------------------------------------------------------

# Load required libraries
library(raster)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)

# Define the file paths for multiple datasets
# Vector of dataset names and seasons
datasets <- c("imerg", "imergv07", "gsmap", "cmorph", "persiann", "era5") # Add more datasets if needed
seasons <- c("jja", "djf")

# Initialize an empty vector to store file paths
file_paths <- character(0)

# Generate file paths using nested loops and paste0

for (dataset in datasets) {
  for (season in seasons) {
    file_name <- paste0("hourly_freq_", dataset, "_tp_mm_60ns_", ifelse(dataset == "gsmap", "2015_20", "2001_20"), "_025_hourly_", season)
    # if (dataset %in% c("imerg", "gsmap")) {
    #   file_name <- paste0(file_name, "_fliptrans")
    # }
    file_path <- paste0("~/shared/data_downloads/input_data/seasonal/hourly_character/", file_name, ".nc")
    file_paths <- c(file_paths, file_path)
  }
}


# Print the generated file paths
print(file_paths)

# Set the number of cores to use for parallel processing
num_cores <- detectCores() - 53

# Register the parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to extract the dataset name from the file path

extract_dataset_name <- function(file_path) {
  start_index <- regexpr("hourly_freq_", file_path) + nchar("hourly_freq_")
  end_index <- regexpr("_tp", file_path, fixed = TRUE)
  dataset_name <- substr(file_path, start_index, end_index - 1)
  return(dataset_name)
}



# Function to extract the season from the file path
extract_season <- function(file_path) {
  if (grepl("jja", file_path, ignore.case = TRUE)) {
    return("jja")
  } else if (grepl("djf", file_path, ignore.case = TRUE)) {
    return("djf")
  } else {
    return("unknown")
  }
}


# Function to process each dataset
process_dataset <- function(file_path) {
  dataset_name <- extract_dataset_name(file_path)
  season <- extract_season(file_path)
  
  dataset <- brick(file_path)
  
  if (dataset_name == "persiann") {
    pers_time <- getZ(dataset)
    posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00")
    names(dataset) <- posixct_time
  }
  
  dataset_dt <- as.data.frame(dataset, xy = TRUE, na.rm = TRUE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>%
    `[`(, name := factor(dataset_name)) %>%
    `[`(, season := factor(season))
  
  return(dataset_dt)
}



# Use foreach to process multiple datasets in parallel
results <- foreach(file_path = file_paths, .packages = c("raster", "dplyr", "data.table")) %dopar% {
  process_dataset(file_path)
}

# Stop the parallel backend
stopCluster(cl)

# Print the results

names(results) <- extract_dataset_name(file_paths)

#read the mask datasets (topo = 1 ocean and 0 for land) to identify land nd ocen pixels
mask_raster <- brick("~/rajani/diurnal_cycle/mask_land.nc", varname="topo")
mask_table <- as.data.frame(mask_raster, xy = TRUE) %>% as.data.table()

mask_table[, `:=`(location = factor("land"))]
mask_table[layer  == "1", `:=`(location = factor("ocean"))]

summary(mask_table)


# Apply the merge operation to each dataset in the output_list
merged_output_list <- lapply(results, function(dataset) merge(dataset, mask_table, by = c("x", "y")))


# converting the time from utc to LST for a list of data.tables

library(hms) 

dat_lst_list <- lapply(merged_output_list, function(dt) {
  dt$date <- substr(dt$date, 13, 14) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_freq, name, season, location)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})

saveRDS(dat_lst_list, "./projects/main/data/hourly_freq_all_datasets_LST_glob_2001_20_seasonal.rds")


##################################################################################################


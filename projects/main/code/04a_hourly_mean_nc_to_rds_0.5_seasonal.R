# for seasonal (JJA and DJF) ----------------------------------------------

# Load required libraries
library(raster)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)

# Define the file paths for multiple datasets
# Vector of dataset names and seasons
datasets <- c("imerg", "gsmap", "cmorph", "persiann", "era5") # Add more datasets if needed
seasons <- c("jja", "djf")

# Initialize an empty vector to store file paths
file_paths <- character(0)

# Generate file paths using nested loops and paste0

for (dataset in datasets) {
  for (season in seasons) {
    file_name <- paste0("hourly_mean_", dataset, "_", season)
    # if (dataset %in% c("imerg", "gsmap")) {
    #   file_name <- paste0(file_name, "_fliptrans")
    # }
    file_path <- paste0("~/shared/data_projects/diurnal_precip/processed/", file_name, ".nc")
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
  # Extract the part of the file name that comes after "hourly_mean_"
  start_index <- regexpr("hourly_mean_", file_path) + nchar("hourly_mean_")
  sub_path <- substr(file_path, start_index, nchar(file_path))
  
  # Extract the dataset name, which is everything before the first underscore
  end_index <- regexpr("_", sub_path, fixed = TRUE)
  dataset_name <- substr(sub_path, 1, end_index - 1)
  
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
  
  dataset_dt <- as.data.frame(dataset, xy = TRUE, na.rm = FALSE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
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

names(results) <- extract_dataset_name(file_paths)


#read the mask datasets (topo = 1 ocean and 0 for land) to identify land nd ocen pixels
mask_raster <- brick("~/rajani/diurnal_cycle/mask_land.nc", varname="topo")
mask_table <- as.data.frame(mask_raster, xy = TRUE) %>% as.data.table()

mask_table[, `:=`(location = factor("land"))]
mask_table[layer  == "1", `:=`(location = factor("ocean"))]

summary(mask_table)


# Apply the merge operation to each dataset in the output_list
merged_list <- lapply(results, function(dataset) merge(dataset, mask_table, by = c("x", "y")))


# converting the time from utc to LST for a list of data.tables

library(hms) 

dat_lst_list <- lapply(merged_list, function(dt) {
  dt$date <- substr(dt$date, 13, 14) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_mean, name, season, location)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})

saveRDS(dat_lst_list, "./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20_seasonal.rds")


################################################################################

# for seasonal (JJA and DJF) and threshold of 0.5 mm/hr ----------------------------------------------


# Load required libraries
library(raster)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)

# Define the file paths for multiple datasets
# Vector of dataset names and seasons
datasets <- c("imerg", "gsmap", "cmorph", "persiann", "era5") # Add more datasets if needed
seasons <- c("jja", "djf")

# Initialize an empty vector to store file paths
file_paths <- character(0)

# Generate file paths using nested loops and paste0

for (dataset in datasets) {
  for (season in seasons) {
    file_name <- paste0("hourly_mean_", dataset, "_", season, "_", "0.5")
    # if (dataset %in% c("imerg", "gsmap")) {
    #   file_name <- paste0(file_name, "_fliptrans")
    # }
    file_path <- paste0("~/shared/data_projects/diurnal_precip/processed/", file_name, ".nc")
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
  # Extract the part of the file name that comes after "hourly_mean_"
  start_index <- regexpr("hourly_mean_", file_path) + nchar("hourly_mean_")
  sub_path <- substr(file_path, start_index, nchar(file_path))
  
  # Extract the dataset name, which is everything before the first underscore
  end_index <- regexpr("_", sub_path, fixed = TRUE)
  dataset_name <- substr(sub_path, 1, end_index - 1)
  
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
  
  dataset_dt <- as.data.frame(dataset, xy = TRUE, na.rm = FALSE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean_0.5") %>%
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

names(results) <- extract_dataset_name(file_paths)


#read the mask datasets (topo = 1 ocean and 0 for land) to identify land nd ocen pixels
mask_raster <- brick("~/rajani/diurnal_cycle/mask_land.nc", varname="topo")
mask_table <- as.data.frame(mask_raster, xy = TRUE) %>% as.data.table()

mask_table[, `:=`(location = factor("land"))]
mask_table[layer  == "1", `:=`(location = factor("ocean"))]

summary(mask_table)


# Apply the merge operation to each dataset in the output_list
merged_list <- lapply(results, function(dataset) merge(dataset, mask_table, by = c("x", "y")))


# converting the time from utc to LST for a list of data.tables

library(hms) 

dat_lst_list <- lapply(merged_list, function(dt) {
  dt$date <- substr(dt$date, 13, 14) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_mean_0.5, name, season, location)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})

saveRDS(dat_lst_list, "./projects/main/data/hourly_mean_thres_0.5_all_datasets_LST_glob_2001_20_seasonal.rds")


################################################################################


persi_jja <- results$persiann[season == "jja"]
summary(persi_jja)

lapply(results, summary)

pers_jja <- brick("~/shared/data_projects/diurnal_precip/hourly_mean_persiann_60ns_2001_2020_regrid.nc")

dataset_dt <- as.data.frame(pers_jja, xy = TRUE, na.rm = FALSE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean_0.5")

summary(dataset_dt)

dt <- dataset_dt[, .(x, y, date, value = round(prec_mean_0.5, 2))]
summary(dt)

rows_with_na <- dataset_dt[is.na(prec_mean_0.5)]
View(rows_with_na)


pers_org <- brick("~/shared/data_projects/diurnal_precip/processed/hourly_mean_persiann.nc")

pers_org_dt <- as.data.frame(pers_org, xy = TRUE, na.rm = FALSE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean_0.5")

summary(pers_org_dt)

rows_with_na <- pers_org_dt[is.na(prec_mean_0.5)]
View(rows_with_na)




# compute the mean 
cdo -b 32 -P 50 -dhourmean ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid.nc ~/shared/data_projects/diurnal_precip/hourly_mean_persiann_60ns_2001_2020_regrid.nc


########

jja <- brick("~/shared/data_projects/diurnal_precip/processed/hourly_mean_persiann_jja.nc")
djf <- brick("~/shared/data_projects/diurnal_precip/processed/hourly_mean_persiann_djf.nc")

jja_dt <- as.data.frame(jja, xy = TRUE, na.rm = FALSE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec")

djf_dt <- as.data.frame(djf, xy = TRUE, na.rm = FALSE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec")

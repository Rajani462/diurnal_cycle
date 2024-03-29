
# for thresholds 0.2_0.5mm/hr -----------------------------------------------------------

library(raster)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)

# Define the file paths for multiple datasets
# Vector of dataset names and seasons
datasets <- c("imerg", "gsmap", "cmorph", "persiann", "era5") # Add more datasets if needed
thresholds <- c("0.2", "0.5")  # Add more thresholds if needed

# Initialize an empty vector to store file paths
file_paths <- character(0)

# Generate file paths using nested loops and paste0
for (dataset in datasets) {
  for (threshold in thresholds) {
    file_name <- paste0("hourly_int_", dataset, "_", threshold)
    file_path <- paste0("~/shared/data_projects/diurnal_precip/processed/", file_name, ".nc")
    file_paths <- c(file_paths, file_path)
  }
}

# Rest of your code remains the same


# Print the generated file paths
print(file_paths)

# Set the number of cores to use for parallel processing
num_cores <- detectCores() - 53

# Register the parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to extract the dataset name and threshold from the file path
extract_dataset_name <- function(file_path) {
  start_index <- regexpr("hourly_int_", file_path) + nchar("hourly_int_")
  end_index <- regexpr(".nc", file_path, perl = TRUE)
  dataset_name <- substr(file_path, start_index, end_index - 1)
  return(dataset_name)
}

extract_dataset_name(file_paths)

# Function to process each dataset
process_dataset <- function(file_path) {
  dataset_name <- extract_dataset_name(file_path)
  
  dataset <- brick(file_path)
  
  if (grepl("imerg_(0\\.2|0\\.5)", dataset_name)) {
    transposed_flipped <- flip(t(dataset), direction = "x")
    file_datetime <- as.POSIXct(getZ(dataset), origin = "1970-01-01", format = "%Y-%m-%d %H:%M:%S")
    dataset <- setZ(transposed_flipped, file_datetime, 'date')
    names(dataset) <- file_datetime
    # } else if (dataset_name == "gsmap") {
    #   transposed_flipped <- flip(t(dataset), direction = "x")
    #   file_datetime <- as.POSIXct(getZ(dataset), origin = "1970-01-01", format = "%Y-%m-%d %H:%M:%S")
    #   dataset <- setZ(transposed_flipped, file_datetime, 'date')
    #   names(dataset) <- file_datetime
  } else if (grepl("persiann_(0\\.2|0\\.5)", dataset_name)) {
    pers_time <- getZ(dataset)
    posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00")
    names(dataset) <- posixct_time
  }
  
  # Determine the column name based on the dataset name
  col_name <- ifelse(grepl("_0\\.2$", dataset_name), "prec_int_0.2", 
                     ifelse(grepl("_0\\.5$", dataset_name), "prec_int_0.5", NA))
  
  dataset_dt <- as.data.frame(dataset, xy = TRUE, na.rm = FALSE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = col_name) %>%
    `[`(, name := factor(gsub("_0\\.2|_0\\.5", "", dataset_name)))
  
  return(dataset_dt)
}


# Use foreach to process multiple datasets in parallel
results <- foreach(file_path = file_paths, .packages = c("raster", "dplyr", "data.table")) %dopar% {
  process_dataset(file_path)
}

# Stop the parallel backend
stopCluster(cl)
names <- extract_dataset_name(file_paths)
names(results) <- sub("_.*$", "", names)

# Assuming your list is named 'your_list'
# First, create a list of data frames excluding those with the same name
unique_names <- unique(names(results))
mean_data_list <- lapply(unique_names, function(name) {
  dfs <- results[names(results) == name]
  # Combine the data frames into one by row binding
  do.call(cbind, dfs)
})


extracted_data_list <- lapply(mean_data_list, function(df) {
  # Select the first 5 columns and the 9th column
  df[, c(1:4, 9:10), with = FALSE]
})

new_col_names <- c("x", "y", "date", "prec_int_0.2", "prec_int_0.5", "name")

# Assuming extracted_data_list is your list of data frames
# Use lapply to rename columns in each data frame
extracted_data_list <- lapply(extracted_data_list, function(df) {
  # Set the column names to the desired names
  setnames(df, old = names(df), new = new_col_names)
  return(df)
})


#read the mask datasets (topo = 1 ocean and 0 for land) to identify land nd ocen pixels
mask_raster <- brick("~/rajani/diurnal_cycle/mask_land.nc", varname="topo")
mask_table <- as.data.frame(mask_raster, xy = TRUE) %>% as.data.table()

mask_table[, `:=`(location = factor("land"))]
mask_table[layer  == "1", `:=`(location = factor("ocean"))]

summary(mask_table)

# Apply the merge operation to each dataset in the output_list
merged_list <- lapply(extracted_data_list, function(dataset) merge(dataset, mask_table, by = c("x", "y")))

# converting the time from utc to LST for a list of data.tables
library(hms) 

dat_lst_list <- lapply(merged_list, function(dt) {
  dt$date <- substr(dt$date, 13, 14) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_int_0.2, prec_int_0.5, name, location)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})

saveRDS(dat_lst_list, "./projects/main/data/hourly_int_thres_0.2_0.5_all_datasets_LST_glob_2001_20.rds")

##########

## merge 0.1 mm/hrwith 0.2 mm/hr and 0.5 mm/hr intensity thresholds
data_list <- readRDS("./projects/main/data/hourly_int_all_datasets_LST_glob_2001_20.rds")

merged_list <- lapply(data_list, function(dataset) merge(dataset, rbindlist(dat_lst_list),
                                                         by = c("lat", "lon", "time_utc", "name",
                                                                "location", "tmz_offset", "time_lst")))

saveRDS(merged_list, "./projects/main/data/hourly_int_thres_0.1_0.5_all_datasets_LST_glob_2001_20.rds")

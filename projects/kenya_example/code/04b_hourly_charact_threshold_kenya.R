
# estimate the mean diurnal cycle of mean precipitation, intensity and frequency

library(raster)
library(doParallel)
library(foreach)
library(data.table)
library(tools)
library(magrittr)

# Set the number of cores to use
num_cores <- 40

# Create a named vector of file paths for each dataset
file_paths <- c(
  cmorph = "./projects/kenya_example/data/cmorph_hour_kenya_2001_20.nc",
  imerg = "./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025_fliptrans.nc",
  persiann = "./projects/kenya_example/data/persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc",
  era5 = "./projects/kenya_example/data/era5_hour_kenya_2001_20_regrid.nc",
  gsmap = "./projects/kenya_example/data/gsmap_hour_kenya_2015_20_fliptrans.nc"
)

# Initialize parallel processing with the specified number of cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Create empty lists to store mean, intensity, and frequency data tables for each dataset
mean_list <- foreach(dataset_name = names(file_paths), file_path = file_paths, .packages = c("raster", "data.table", "magrittr")) %dopar% {
  # Read the dataset
  dataset <- brick(file_path)
  
  # Pre-processing for persiann dataset
  if (dataset_name == "persiann") {
    pers_time <- getZ(dataset)
    posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00")
    names(dataset) <- posixct_time
  }
  
  # Calculate the indices
  indices <- format(as.POSIXct(names(dataset), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
  indices <- as.numeric(indices)
  
  unique_dates <- unique(as.Date(names(dataset), format = "X%Y.%m.%d.%H.%M.%S"))
  
  ### 24 hourly mean diurnal precipitation 
  hourly_mean <- stackApply(dataset, indices, fun = mean)
  mean_dt <- as.data.frame(hourly_mean, xy = TRUE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
    `[`(, name := factor(dataset_name)) %>%
    `[`(, variable := "mean")
  
  mean_dt[!is.finite(value), value := NA]
  
  # Calculate the total precipitation for each hour of the day
  total_precip <- stackApply(dataset, indices, fun = sum)
  
  # Calculate the number of precipitation hours for each hour of the day
  precip_hours <- stackApply(dataset, indices, fun = function(x, na.rm = TRUE) sum(x > 0.1, na.rm = TRUE))
  
  # Calculate the intensity
  intensity <- total_precip / precip_hours
  
  # Create a data table with the intensity values
  intensity_dt <- as.data.frame(intensity, xy = TRUE) %>%
    as.data.table() %>%
    melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
    as.data.table() %>% 
    `[`(, name := factor(dataset_name)) %>%
    `[`(, variable := "intensity")
  
  intensity_dt[!is.finite(value), value := NA]
  
  # Calculate the frequency
  total_available_hours <- length(unique_dates)
  frequency <- (precip_hours / total_available_hours) * 100
  
  # Create a data table with the frequency values
  frequency_dt <- as.data.frame(frequency, xy = TRUE) %>%
    as.data.table() %>%
    melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
    as.data.table() %>% 
    `[`(, name := factor(dataset_name)) %>%
    `[`(, variable := "frequency")
  
  frequency_dt[!is.finite(value), value := NA]
  
  # Return a list of the mean, intensity, and frequency data tables
  list(mean = mean_dt, intensity = intensity_dt, frequency = frequency_dt)
}

output_list <- list(
  mean = lapply(mean_list, function(x) x$mean),
  intensity = lapply(mean_list, function(x) x$intensity),
  frequency = lapply(mean_list, function(x) x$frequency)
)


# Save the output list as a single .RDS file
saveRDS(output_list, file = "./projects/kenya_example/data/output/diurnal_mean_int_freq_list.RDS")

# Stop parallel processing
stopCluster(cl)

############################################

### verify the outputs with original script (04a_hourly_charact_kenya.R)

dat_list <- readRDS("./projects/kenya_example/data/output/diurnal_mean_int_freq_list.RDS")

cmorph_int <- readRDS("./projects/kenya_example/data/hourly_charact/int_cmorph_hourly.RDS")
pers_int <- readRDS("./projects/kenya_example/data/hourly_charact/int_persiann_hourly.RDS")
imerg_int <- readRDS("./projects/kenya_example/data/hourly_charact/int_imerg_hourly.RDS")
gsmap_int <- readRDS("./projects/kenya_example/data/hourly_charact/int_gsmap_hourly.RDS")
era5_int <- readRDS("./projects/kenya_example/data/hourly_charact/int_era5_hourly.RDS")

cmorph_freq <- readRDS("./projects/kenya_example/data/hourly_charact/freq_cmorph_hourly.RDS")
pers_freq <- readRDS("./projects/kenya_example/data/hourly_charact/freq_persiann_hourly.RDS")
imerg_freq <- readRDS("./projects/kenya_example/data/hourly_charact/freq_imerg_hourly.RDS")
gsmap_freq <- readRDS("./projects/kenya_example/data/hourly_charact/freq_gsmap_hourly.RDS")
era5_freq <- readRDS("./projects/kenya_example/data/hourly_charact/freq_era5_hourly.RDS")


##########################################


# check the intensity and frequency with different threshold ------------------------------------------------
# same as above but for precip > 0.1, 0.5, and 1 mm/hr

library(raster)
library(doParallel)
library(foreach)
library(data.table)
library(magrittr)

# Set the number of cores to use
num_cores <- 40 #for R large (128GB)

# Create a named vector of file paths for each dataset
file_paths <- c(
  cmorph = "./projects/kenya_example/data/cmorph_hour_kenya_2001_20.nc",
  imerg = "./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025_fliptrans.nc",
  persiann = "./projects/kenya_example/data/persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc",
  era5 = "./projects/kenya_example/data/era5_hour_kenya_2001_20_regrid.nc",
  gsmap = "./projects/kenya_example/data/gsmap_hour_kenya_2015_20_fliptrans.nc"
)

# Set the thresholds
thresholds <- c(0.1, 0.5, 1.0)

# Initialize parallel processing with the specified number of cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Loop over each dataset and threshold in parallel
result_list <- foreach(dataset_name = names(file_paths), file_path = file_paths, .packages = c("raster", "data.table", "magrittr")) %:%
  foreach(threshold = thresholds, .combine = "c") %dopar% {
    # Read the dataset
    dataset <- brick(file_path)
    
    # Pre-processing for persiann dataset
    if (dataset_name == "persiann") {
      pers_time <- getZ(dataset)
      posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00")
      names(dataset) <- posixct_time
    }
    
    # Calculate the indices
    indices <- format(as.POSIXct(names(dataset), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
    indices <- as.numeric(indices)
    
    # Calculate the total precipitation for each hour of the day
    total_precip <- stackApply(dataset, indices, fun = sum)
    
    # Calculate the number of precipitation hours for each hour of the day
    precip_hours <- stackApply(dataset, indices, fun = function(x, na.rm = TRUE) sum(x > threshold, na.rm = TRUE))
    
    # Calculate the intensity
    intensity <- total_precip / precip_hours
    
    # Create a data table with the intensity values
    intensity_dt <- as.data.frame(intensity, xy = TRUE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
      `[`(, name := factor(dataset_name)) %>%
      `[`(, variable := "intensity") %>%
      `[`(, threshold := factor(threshold))
    
    intensity_dt[!is.finite(value), value := NA]
    
    # Calculate the frequency
    total_available_hours <- length(indices)
    frequency <- (precip_hours / total_available_hours) * 100
    
    # Create a data table with the frequency values
    frequency_dt <- as.data.frame(frequency, xy = TRUE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
      `[`(, name := factor(dataset_name)) %>%
      `[`(, variable := "frequency") %>%
      `[`(, threshold := factor(threshold))
    
    frequency_dt[!is.finite(value), value := NA]
    
    # Return the intensity and frequency data tables as a named list
    list(intensity_dt = intensity_dt, frequency_dt = frequency_dt)
  } -> result_list

# Stop the parallel processing and close the cluster
stopCluster(cl)


# Save the data tables as a single .RDS file
saveRDS(result_list, file = "./projects/kenya_example/data/output/diurnal_int_freq_thres_list.RDS")

#################################################


# Extras ------------------------------------------------------------------


dat <- readRDS("./projects/kenya_example/data/output/diurnal_int_freq_thres_list.RDS")


# # Unpack the result_list into separate intensity and frequency data tables
# intensity_list <- lapply(result_list, function(result) result$intensity_dt)
# frequency_list <- lapply(result_list, function(result) result$frequency_dt)
# 
# # Create a list to store the intensity and frequency data tables
# data_tables <- list(intensity_list = intensity_list, frequency_list = frequency_list)


##############################


# Diurnal intensity and frequency for seasonal ----------------------------

########### produces multiple files JJA1, JJA2, JJA3..etc for PERSIANN

########################################################################################

library(raster)
library(doParallel)
library(foreach)
library(data.table)
library(tools)
library(magrittr)

# Set the number of cores to use
num_cores <- 40

# Create a named vector of file paths for each dataset
file_paths <- c(
  cmorph = "./projects/kenya_example/data/cmorph_hour_kenya_2001_20.nc",
  imerg = "./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025_fliptrans.nc",
  persiann = "./projects/kenya_example/data/persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc",
  era5 = "./projects/kenya_example/data/era5_hour_kenya_2001_20_regrid.nc",
  gsmap = "./projects/kenya_example/data/gsmap_hour_kenya_2015_20_fliptrans.nc"
)

# Create a named vector of seasons
seasons <- c(
  JF = c(1, 2),
  MAM = c(3, 4, 5),
  JJAS = c(6, 7, 8, 9),
  OND = c(10, 11, 12)
)

# Initialize parallel processing with the specified number of cores
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Create empty lists to store intensity and frequency data tables
intensity_list <- list()
frequency_list <- list()

# Loop over each dataset and season in parallel
foreach(dataset_name = names(file_paths), file_path = file_paths, .packages = c("raster", "data.table", "magrittr")) %:%
  foreach(season_name = names(seasons), season_months = seasons, .combine = 'c') %dopar% {
    # Read the dataset
    dataset <- brick(file_path)
    
    # Pre-processing for persiann dataset
    if (dataset_name == "persiann") {
      pers_time <- getZ(dataset)
      posixct_time <- as.POSIXct(pers_time, origin = "2001-01-01", tz = "UTC")
      names(dataset) <- posixct_time
    }
    
    # Subset the dataset for the current season
    subset_dataset <- subset(dataset, which(getZ(dataset) %in% season_months))
    
    # Calculate the indices
    indices <- format(as.POSIXct(names(subset_dataset), format = "X%Y.%m.%d.%H.%M.%S"), format = "%Y-%m-%d %H:%M:%S")
    indices <- as.POSIXct(indices)
    
    # Calculate the total precipitation for each hour of the day
    total_precip <- stackApply(subset_dataset, indices, fun = sum)
    
    # Calculate the number of precipitation hours for each hour of the day
    precip_hours <- stackApply(subset_dataset, indices, fun = function(x, na.rm = TRUE) sum(x > 0.1, na.rm = TRUE))
    
    # Calculate the intensity
    intensity <- total_precip / precip_hours
    
    # Create a data table with the intensity values
    intensity_dt <- as.data.frame(intensity, xy = TRUE) %>%
      as.data.table() %>%
      melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
      as.data.table() %>% 
      `[`(, name := factor(dataset_name)) %>%
      `[`(, variable := "intensity") %>%
      `[`(, season := factor(season_name))
    
    # Append the intensity data table to the intensity list
    intensity_list[[paste(dataset_name, season_name, sep = "_")]] <- intensity_dt
    
    # Calculate the frequency
    total_available_hours <- length(indices)
    frequency <- (precip_hours / total_available_hours) * 100
    
    # Create a data table with the frequency values
    frequency_dt <- as.data.frame(frequency, xy = TRUE) %>%
      as.data.table() %>%
      melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
      as.data.table() %>% 
      `[`(, name := factor(dataset_name)) %>%
      `[`(, variable := "frequency") %>%
      `[`(, season := factor(season_name))
    
    # Append the frequency data table to the frequency list
    frequency_list[[paste(dataset_name, season_name, sep = "_")]] <- frequency_dt
    
    # Save the intensity and frequency data tables as separate .RDS files
    intensity_filename <- file.path("./projects/kenya_example/data/output", paste0(dataset_name, "_", season_name, "_intensity.RDS"))
    frequency_filename <- file.path("./projects/kenya_example/data/output", paste0(dataset_name, "_", season_name, "_frequency.RDS"))
    
    # Save intensity data table
    saveRDS(intensity_dt, file = intensity_filename)
    
    # Save frequency data table
    saveRDS(frequency_dt, file = frequency_filename)
    
    # Print the file paths to check if the files are saved correctly
    print(paste("Intensity file saved:", intensity_filename))
    print(paste("Frequency file saved:", frequency_filename))
  }

# Stop parallel processing
stopCluster(cl)

######################################################


cmorph_int <- readRDS("./projects/kenya_example/data/output/cmorph_intensity_all_seasons.RDS")
summary(ond)
jf <- readRDS("./projects/kenya_example/data/output/cmorph_JF_frequency.RDS")
summary(jf)
mam <- readRDS("./projects/kenya_example/data/output/cmorph_MAM_frequency.RDS")
summary(mam)
jjas <- readRDS("./projects/kenya_example/data/output/cmorph_JJAS_frequency.RDS")
summary(jjas)

################################










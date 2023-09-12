library(raster)
library(data.table)
library(ggplot2)
library(dplyr)
library(sf)
library(hms)
library(forcats)
library(ncdf4)
library(data.table)


#########################################
# Define the directory containing the NetCDF files
# Set the data directory
data_dir <- "~/shared/data_projects/diurnal_precip/processed"

# List of NetCDF files in the directory that match the pattern
nc_files <- list.files(path = data_dir, pattern = "^lat_mean_(imerg|gsmap|cmorph|persiann|era5|)\\.nc$", full.names = TRUE)

# Initialize an empty list to store the data tables
data_dt_list <- list()

# Define the desired order of datasets
desired_order <- c("imerg", "gsmap", "cmorph", "persiann", "era5")

# Iterate through each NetCDF file
for (nc_file in nc_files) {
  # Extract the dataset name from the file name
  dataset_name <- sub(".*/lat_mean_([^\\.]+)\\.nc", "\\1", nc_file)
  
  # Check if the dataset is in the desired order
  if (dataset_name %in% desired_order) {
    # Open the NetCDF file
    nc_data <- nc_open(nc_file)
    
    # Read the variable data (Replace "precip" with the actual variable name)
    variable_array <- ncvar_get(nc_data, "precip")
    
    # Extract latitude values from the NetCDF file
    latitude_values <- ncvar_get(nc_data, "lat")
    
    # Create a data.table with columns: date, precip, lat, and name
    data_dt <- data.table(
      precip = as.vector(variable_array),
      lat = latitude_values,
      name = as.factor(dataset_name)  # Add the dataset name as a factor
    )
    
    # Append the data table to the list
    data_dt_list[[dataset_name]] <- data_dt
    
    # Sort the data tables in the list by latitude values
    data_dt_list <- lapply(data_dt_list, function(dt) dt[order(lat)])
    
    # Close the NetCDF file
    nc_close(nc_data)
  }
}

# Reorder the data tables based on the desired order
reordered_dt_list <- lapply(desired_order, function(name) data_dt_list[[name]])

# Combine the data tables into a single data table
combined_dt <- rbindlist(reordered_dt_list)

# Print the resulting combined data table
print(combined_dt)

saveRDS(combined_dt, "./projects/main/data/zonal_lat_mean_2001_20.rds")

###################

# for threshold of 0.5 mm/hr-----
library(raster)
library(data.table)
library(ggplot2)
library(dplyr)
library(sf)
library(hms)
library(forcats)
library(ncdf4)
library(data.table)

data_dir <- "~/shared/data_projects/diurnal_precip/processed"

# List of NetCDF files in the directory that match the pattern
nc_files <- list.files(path = data_dir, pattern = "^lat_mean_(imerg|gsmap|cmorph|persiann|era5|)_0.5\\.nc$", full.names = TRUE)

# Initialize an empty list to store the data tables
data_dt_list <- list()

# Define the desired order of datasets
desired_order <- c("imerg", "gsmap", "cmorph", "persiann", "era5")

# Iterate through each NetCDF file
for (nc_file in nc_files) {
  # Extract the dataset name from the file name
  dataset_name <- sub(".*/lat_mean_([^\\.]+)_0.5\\.nc", "\\1", nc_file)
  
  # Check if the dataset is in the desired order
  if (dataset_name %in% desired_order) {
    # Open the NetCDF file
    nc_data <- nc_open(nc_file)
    
    # Read the variable data (Replace "precip" with the actual variable name)
    variable_array <- ncvar_get(nc_data, "precip")
    
    # Extract latitude values from the NetCDF file
    latitude_values <- ncvar_get(nc_data, "lat")
    
    # Create a data.table with columns: date, precip, lat, and name
    data_dt <- data.table(
      precip = as.vector(variable_array),
      lat = latitude_values,
      name = as.factor(dataset_name)  # Add the dataset name as a factor
    )
    
    # Append the data table to the list
    data_dt_list[[dataset_name]] <- data_dt
    
    # Sort the data tables in the list by latitude values
    data_dt_list <- lapply(data_dt_list, function(dt) dt[order(lat)])
    
    # Close the NetCDF file
    nc_close(nc_data)
  }
}

# Reorder the data tables based on the desired order
reordered_dt_list <- lapply(desired_order, function(name) data_dt_list[[name]])

# Combine the data tables into a single data table
combined_dt <- rbindlist(reordered_dt_list)
setnames(combined_dt, "precip", "precip_0.5")
# Print the resulting combined data table
print(combined_dt)

saveRDS(combined_dt, "./projects/main/data/zonal_lat_mean_0.5_2001_20.rds")


# seasonal ----------------------------------------------------------------

# Define the directory containing the NetCDF files
# Set the data directory
data_dir <- "~/shared/data_projects/diurnal_precip/processed"

# List of NetCDF files in the directory that match the pattern
nc_files <- list.files(path = data_dir, pattern = "^lat_mean_.*(_djf|_jja)_0.5\\.nc$", full.names = TRUE)

# Initialize an empty list to store the data tables
data_dt_list <- list()

# Iterate through each NetCDF file
for (nc_file in nc_files) { 
  dataset_name <- sub(".*/lat_mean_([^\\.]+)_0.5\\.nc", "\\1", nc_file)
  
  # Open the NetCDF file
  nc_data <- nc_open(nc_file)
  
  # Read the variable data (Replace "precip" with the actual variable name)
  variable_array <- ncvar_get(nc_data, "count_value_above_0_5")
  
  # Extract latitude values from the NetCDF file
  latitude_values <- ncvar_get(nc_data, "lat")
  
  # Create a data.table with columns: precip, lat, name
  data_dt <- data.table(
    precip = as.vector(variable_array),
    lat = latitude_values,
    name = as.factor(dataset_name)  # Add the dataset name as a factor
  )
  
  # Append the data table to the list
  data_dt_list[[dataset_name]] <- data_dt
  
  # Sort the data tables in the list by latitude values
  data_dt_list <- lapply(data_dt_list, function(dt) dt[order(lat)])
  
  # Close the NetCDF file
  nc_close(nc_data)
}

# Combine the data tables into a single data table
combined_dt <- rbindlist(data_dt_list)

# Print the resulting combined data table
print(combined_dt)

# Split the 'name' column into 'name' and 'season' based on the underscore
combined_dt[, c("name", "season") := tstrsplit(name, "_")]
setnames(combined_dt, "precip", "precip_0.5")
# Print the resulting combined data table
print(combined_dt)

saveRDS(combined_dt, "./projects/main/data/zonal_lat_mean_seasonal_0.5_2001_20.rds")


##############################################################################

#extras
#for hourly mean-------------------------------------------------------------

# Define the directory containing the NetCDF files
# Set the data directory
data_dir <- "~/shared/data_projects/diurnal_precip/processed"

# List of NetCDF files in the directory that match the pattern
nc_files <- list.files(path = data_dir, pattern = "^lat_mean_hourly_(imerg|gsmap|cmorph|persiann|era5|)\\.nc$", full.names = TRUE)

# Initialize an empty list to store the data tables
data_dt_list <- list()

# Define the desired order of datasets
desired_order <- c("imerg", "gsmap", "cmorph", "persiann", "era5")

# Iterate through each NetCDF file
for (nc_file in nc_files) {
  # Extract the dataset name from the file name
  dataset_name <- sub(".*/lat_mean_hourly_([^\\.]+)\\.nc", "\\1", nc_file)
  
  # Check if the dataset is in the desired order
  if (dataset_name %in% desired_order) {
    # Open the NetCDF file
    nc_data <- nc_open(nc_file)
    
    # Read the variable data (Replace "precip" with the actual variable name)
    variable_array <- ncvar_get(nc_data, "precip")
    
    # Extract latitude values from the NetCDF file
    latitude_values <- ncvar_get(nc_data, "lat")
    
    # Create a data.table with columns: date, precip, lat, and name
    data_dt <- data.table(
      precip = as.vector(variable_array),
      lat = latitude_values,
      name = as.factor(dataset_name)  # Add the dataset name as a factor
    )
    
    # Append the data table to the list
    data_dt_list[[dataset_name]] <- data_dt
    
    # Sort the data tables in the list by latitude values
    data_dt_list <- lapply(data_dt_list, function(dt) dt[order(lat)])
    
    # Close the NetCDF file
    nc_close(nc_data)
  }
}

# Reorder the data tables based on the desired order
reordered_dt_list <- lapply(desired_order, function(name) data_dt_list[[name]])

# Combine the data tables into a single data table
combined_dt <- rbindlist(reordered_dt_list)

# Print the resulting combined data table
print(combined_dt)

saveRDS(combined_dt, "./projects/main/data/zonal_lat_mean_2001_20.rds")


################################################################################
#extras--------------------------------------
library(data.table)
library(ncdf4)

# Define the directory containing the NetCDF files
data_dir <- "~/shared/data_projects/diurnal_precip/processed"

# List of NetCDF files in the directory that match the pattern
nc_files <- list.files(path = data_dir, pattern = "^lat_mean_hourly_(imerg|gsmap|cmorph|era5|)\\.nc$", full.names = TRUE)

# Initialize an empty list to store the data tables
data_dt_list <- list()

# Define the desired order of datasets
desired_order <- c("imerg", "gsmap", "cmorph", "persiann", "era5")

# Iterate through each NetCDF file
for (nc_file in nc_files) {
  # Extract the dataset name from the file name
  dataset_name <- sub(".*/lat_mean_hourly_([^\\.]+)\\.nc", "\\1", nc_file)
  
  # Check if the dataset is in the desired order
  if (dataset_name %in% desired_order) {
    # Open the NetCDF file
    nc_data <- nc_open(nc_file)
    
    # Read the variable data (Replace "precip" with the actual variable name)
    variable_array <- ncvar_get(nc_data, "precip")
    
    # Extract latitude values from the NetCDF file
    latitude_values <- ncvar_get(nc_data, "lat")
    
    # Extract time values from the NetCDF file
    time_values <- ncvar_get(nc_data, "time")
    
    # Calculate hourly timestamps starting from the reference time
    reference_time <- as.POSIXct("2001-01-01 00:00:00", tz = "UTC")
    timestamps <- reference_time + as.difftime(time_values, units = "hours")
    
    # Create a data.table with columns: date, precip, lat, and name
    data_dt <- data.table(
      date = timestamps,
      precip = as.vector(variable_array),
      lat = latitude_values,
      name = as.factor(dataset_name)  # Add the dataset name as a factor
    )
    
    # Append the data table to the list
    data_dt_list[[dataset_name]] <- data_dt
    
    # Sort the data tables in the list by latitude values
    data_dt_list <- lapply(data_dt_list, function(dt) dt[order(lat)])
    
    # Close the NetCDF file
    nc_close(nc_data)
  }
}

# Reorder the data tables based on the desired order
reordered_dt_list <- lapply(desired_order, function(name) data_dt_list[[name]])

# Combine the data tables into a single data table
combined_dt <- rbindlist(reordered_dt_list)
combined_dt <- combined_dt[, .(hour = hour(date), precip, lat, name)]
# Print the resulting combined data table
print(combined_dt)

#saveRDS(combined_dt, "./projects/main/data/zonal_lat_mean_2001_20.rds")
ggplot(combined_dt, aes(lat, precip), size = 0.5) + 
  geom_line() + 
  #scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Precipitation (mm/hour)", col = " ") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggplot(combined_dt[lat >= 50.875], aes(hour, precip), size = 0.5) + 
  geom_line() + 
  facet_wrap(~lat, scales = "free_y") + 
  theme(strip.background = element_blank(), panel.border=element_blank())

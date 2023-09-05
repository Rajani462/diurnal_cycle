library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
library(ncdf4)
library(data.table)
#library(ggh4x)


#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

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

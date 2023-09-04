library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
#library(ggh4x)


#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

#########################################
# Load the ncdf4 package
# Load the required package

library(ncdf4)
library(data.table)
library(terra)

# Specify the path to your NetCDF file
nc_file <- "~/shared/data_projects/diurnal_precip/processed/lat_mean_imerg.nc"
a <- rast("~/shared/data_projects/diurnal_precip/processed/lat_mean_imerg.nc")

as.data.frame(a, xy = TRUE)
# Open the NetCDF file
nc_data <- nc_open(nc_file)

# Read the variable data
variable_array <- ncvar_get(nc_data, "precip")  # Replace "precip" with the actual variable name

# Extract time values from the NetCDF file
time_values <- ncvar_get(nc_data, "time")

obsdatadates = (nc_data$dim$time$vals)
posixct_time <- as.POSIXct(time_values * 3600, origin = "2001-01-01 00:00:00")


# Convert time values to POSIXct format
base_time <- as.POSIXct("2015-01-01 00:00:00")
time_dates <- base_time + as.difftime(time_values, units = "hours")
time_dates <- base_time + as.difftime(time_values, units = "secs")  # Use "secs" as units

# Extract latitude values from the NetCDF file
latitude_values <- ncvar_get(nc_data, "lat")

# Create a data.table with columns: date, precip, and lat
data_dt <- data.table(
  date = rep(time_dates, each = length(latitude_values)),
  precip = as.vector(variable_array),
  lat = rep(latitude_values, times = length(time_dates))
)

# Close the NetCDF file
nc_close(nc_data)

# Print the first few rows of the data.table
print(data_dt)

ggplot(data_dt, aes(lat, precip)) + 
  #geom_point(size = 0.85) + 
  geom_line() + 
  #facet_wrap(~location) + 
  labs(x ="Latitude", y = "Mean (mm/hr)") + 
  #theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))


########################################################################

library(ncdf4)
library(data.table)

# Define the directory containing the NetCDF files
data_dir <- "~/shared/data_projects/diurnal_precip/processed"

# List of NetCDF files in the directory that match the pattern
nc_files <- list.files(path = data_dir, pattern = "^lat_mean_(imerg|gsmap|cmorph|gsmap|era5|persiann)\\.nc$", full.names = TRUE)


# Initialize an empty list to store the data tables
data_dt_list <- list()

# Iterate through each NetCDF file
for (nc_file in nc_files) {
  # Open the NetCDF file
  nc_data <- nc_open(nc_file)
  
  # Read the variable data
  variable_array <- ncvar_get(nc_data, "precip")  # Replace "precip" with the actual variable name
  
  # Extract time values from the NetCDF file
  dataset_name <- tools::file_path_sans_ext(basename(nc_file))  # Extract dataset name
  
  # Check if the dataset is "persiann" and use "datetime" variable if so
  if (dataset_name == "lat_mean_persiann") {
    time_values <- ncvar_get(nc_data, "datetime")
  } else {
    time_values <- ncvar_get(nc_data, "time")
  }
  
  # Convert time values to POSIXct format
  base_time <- as.POSIXct("2015-01-01 00:00:00")
  time_dates <- base_time + as.difftime(time_values, units = "hours")
  
  # Extract latitude values from the NetCDF file
  latitude_values <- ncvar_get(nc_data, "lat")
  
  # Create a data.table with columns: date, precip, lat, and name
  data_dt <- data.table(
    date = rep(time_dates, each = length(latitude_values)),
    precip = as.vector(variable_array),
    lat = rep(latitude_values, times = length(time_dates)),
    name = as.factor(dataset_name)  # Add the dataset name as a factor
  )
  
  # Append the data table to the list
  data_dt_list[[dataset_name]] <- data_dt
  
  # Close the NetCDF file
  nc_close(nc_data)
}


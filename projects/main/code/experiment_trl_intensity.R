library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
#library(reshape)
#library(terra)
library(ncdf4)
library(sf)
library(hms)
library(forcats)
library(parallel)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

##################################

#to verify the gsmap values between -30 to -60 NS 
cdo -P 40 sellonlatbox,-180,180,-40,-60 ~/shared/data_downloads/input_data/gsmap_tp_mm_60ns_2001_15_025_hourly.nc ~/shared/data_downloads/input_data/2015_20/trl/gsmap_tp_mm_40_60ns_2015_20_025_hourly.nc

--------------------------------------------------------------------------------

#verifying the intensity

cdo -P 40 sellonlatbox,20,40,20,40 ~/shared/data_downloads/input_data/gsmap_tp_mm_60ns_2001_15_025_hourly.nc ~/shared/data_downloads/input_data/africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc

afr <- brick("~/shared/data_downloads/input_data/2015_20/trl/africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc")


plot(afr[[1]])

cdo -P 43 -div -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? precip : 0)' gsmap_tp_mm_60ns_2001_15_025_hourly.nc -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' gsmap_tp_mm_60ns_2001_15_025_hourly.nc hourly_int_gsmap_tp_mm_60ns_2015_20_025.nc


cdo -P 43 -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? precip : 0)' africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc africa_gsmap_total_precip.nc

cdo -P 43 -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc africa_gsmap_count_precip.nc

#R

tot_precip <- brick("~/shared/data_downloads/input_data/2015_20/trl/africa_gsmap_total_precip.nc")
plot(tot_precip[[1]])

tot_precip_dt <- as.data.frame(tot_precip, xy = TRUE, na.rm = FALSE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "tot_prec_0.5")

tot_precip_dt
summary(tot_precip_dt)

tot_precip_dt[complete.cases(tot_precip_dt), ]

tot_precip_dt[x == "20.125" & y == "39.875"]
tot_precip_dt[tot_prec_0.5 == "0"]
tot_precip_dt[x == "22.125" & y == "30.875"]

count_precip <- brick("~/shared/data_downloads/input_data/2015_20/trl/africa_gsmap_count_precip.nc")
plot(count_precip[[1]])

count_precip_dt <- as.data.frame(count_precip, xy = TRUE, na.rm = FALSE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "count_precip_0.5")

count_precip_dt
summary(count_precip_dt)

count_precip_dt[complete.cases(count_precip_dt), ]
count_precip_dt[x == "20.125" & y == "39.875"]
count_precip_dt[tot_prec_0.5 == "0"]
count_precip_dt[x == "22.125" & y == "30.875"]






# Load required libraries
library(raster)
library(data.table)
library(lubridate)
library(dplyr)
library(foreach)
library(doParallel)

# Define the file paths for multiple datasets
# Vector of dataset names and seasons
datasets <- c("imergf", "gsmap", "cmorph", "persiann", "era5") # Add more datasets if needed


# Initialize an empty vector to store file paths
file_paths <- "~/shared/data_downloads/input_data/2015_20/trl/hourly_int_gsmap_tp_mm_60ns_2015_20_025.nc"



# Print the generated file paths
print(file_paths)

# Set the number of cores to use for parallel processing
num_cores <- detectCores() - 53

# Register the parallel backend using doParallel
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Function to extract the dataset name from the file path
extract_dataset_name <- function(file_path) {
  start_index <- regexpr("hourly_int_", file_path) + nchar("hourly_int_")
  end_index <- regexpr("_0.5", file_path, fixed = TRUE)
  dataset_name <- substr(file_path, start_index, end_index - 1)
  return(dataset_name)
}

extract_dataset_name(file_paths)



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
    dataset_dt <- as.data.frame(persiann, xy = TRUE, na.rm = FALSE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int_0.5") %>%
      `[`(, name := factor(dataset_name))
  } else {
    # Perform the main processing steps similar to other datasets
    dataset <- brick(file_path)
    dataset_dt <- as.data.frame(dataset, xy = TRUE, na.rm = FALSE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int_0.5") %>%
      `[`(, name := factor(dataset_name))
  }
  
  return(dataset_dt)
}


# Use foreach to process multiple datasets in parallel
results <- foreach(file_path = file_paths, .packages = c("raster", "dplyr", "data.table")) %dopar% {
  process_dataset(file_path)
}

# Stop the parallel backend
stopCluster(cl)

library(hms)

dt <- rbindlist(results)

dt$date <- substr(dt$date, 13, 14) %>% paste0(":00:00")
dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_int_0.5, name)]
dt[, `:=`(tmz_offset = round((lon / 15)))]
dt$time_utc <- as.POSIXct(dt$time_utc)
dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  

dt[, `:=`(name = NULL, tmz_offset = NULL)]

data_dt <- dt[, .(lat, lon, prec_int_0.5, time_lst = hour(time_lst))]

spat_int_dt <- data_dt[, .(mean_value = round(mean(prec_int_0.5, na.rm = TRUE), 2)), by = .(lat, lon)]

summary(spat_int_dt)


ggplot(spat_int_dt) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(0.3, 0.6, 0.9, 1.2, 1.5, 2, 2.5, 3, 4, 5, 7), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_int_dt$lon), max(spat_int_dt$lon)), 
                  ylim = c(min(spat_int_dt$lat), max(spat_int_dt$lat))) + 
  #facet_wrap(~name, ncol = 2) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Intensity\n  \n (mm/hr)") + 
  #facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")) + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(1.0, 'cm'))



################################################################

# compare the intensity estimated by both methods -------------------------
cdo -P 43 -div -dhoursum -mul africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc method1_hourly_int_africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc
cdo -P 43 -div -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? precip : 0)' africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc method2_hourly_int_africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc

method_1_intensity <- brick("~/shared/data_downloads/input_data/2015_20/trl/method1_hourly_int_africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc")
method_2_intensity <- brick("~/shared/data_downloads/input_data/2015_20/trl/method2_hourly_int_africa_gsmap_tp_mm_60ns_2001_15_025_hourly.nc")


method_1 <- as.data.frame(method_1_intensity, xy = TRUE, na.rm = FALSE) %>%  #when it is TRUE, it remove all the 24 time steps if there is a NA in any single time steps
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "tot_prec_0.5")

method_1
summary(method_1)
method_1[!complete.cases(method_1), ]
method_1[x == "38.125" & y == "36.875"]

method_2 <- as.data.frame(method_2_intensity, xy = TRUE, na.rm = FALSE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "tot_prec_0.5")

method_2
summary(method_2)

#conclusion: both methods produce exactly the same results (same NA's)
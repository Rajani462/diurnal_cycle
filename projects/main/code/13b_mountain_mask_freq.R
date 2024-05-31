library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
library(hms)
library(forcats)
library(raster)
library(data.table)
library(doParallel)
library(magrittr)
library(sf)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

# ...

data_list <- readRDS("./projects/main/data/hourly_freq_all_datasets_LST_glob_2001_20.rds")
filtered_data_list <- lapply(data_list, function(dt) dt[location == "land"])

# data_dt <- rbindlist(data_list)
# data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
# #levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
# 
# shp <- read_sf("./projects/main/data/shapefile/ne_50m_geography_regions_polys.shp")
# #plot(shp)
# 
# # Filter only the range/mountain features (assuming 'featurecla' is the column indicating the feature type)
# mountain_shp <- shp[shp$featurecla == "Range/mtn", ]
# 
# # Convert lat and lon to spatial points
# 
# data_sp <- st_as_sf(data_dt, coords = c("lon", "lat"), crs = st_crs(mountain_shp))
# # Add lat and lon columns to the sf object
# data_sp <- cbind(data_sp, data_dt[, c("lat", "lon"), with = FALSE])
# 
# # Perform spatial join to retain only points within mountain regions
# data_mountain <- st_join(data_sp, mountain_shp, join = st_within)
# 
# # Convert back to data.table if needed
# data_mountain_dt <- as.data.table(data_mountain)
# data_mountain_dt
# 
# unique(data_mountain_dt$featurecla)
# data_mountain_dt[featurecla == "Range/mtn"]


####################

library(sf)
library(data.table)

# Assuming data_list is your list of 5 datasets and shp is your shapefile

# Read the shapefile
shp <- read_sf("./projects/main/data/shapefile/ne_50m_geography_regions_polys.shp")

# Filter only the range/mountain features
mountain_shp <- shp[shp$featurecla == "Range/mtn", ]

# Function to process each dataset
process_dataset <- function(dt, mountain_shp) {
  # Convert lat and lon to spatial points
  cmorph_sp <- sf::st_as_sf(dt, coords = c("lon", "lat"), crs = sf::st_crs(mountain_shp))
  
  # Add lat and lon columns to the sf object
  #cmorph_sp <- cbind(cmorph_sp, dt[, c("lat", "lon"), with = FALSE])
  
  # Perform spatial join to retain only points within mountain regions
  data_mountain <- sf::st_join(cmorph_sp, mountain_shp, join = sf::st_within)
  
  # Convert back to data.table if needed
  data.table::setDT(data_mountain)
  
  return(data_mountain)
}


# Create a cluster with 5 cores
cl <- makeCluster(5)

# Parallel processing using parLapply
masked_data_list <- parLapply(cl, filtered_data_list, process_dataset, mountain_shp = mountain_shp)

# Stop the cluster
stopCluster(cl)


split_geometry <- function(dataset) {
  # Extract lat and lon from geometry
  coordinates <- st_coordinates(dataset$geometry)
  dataset$lat <- coordinates[, 2]
  dataset$lon <- coordinates[, 1]
  # Remove geometry column
  dataset$geometry <- NULL
  return(dataset)
}

# Apply the function to each dataset in masked_data_list using lapply
masked_data_list2 <- lapply(masked_data_list, split_geometry)

saveRDS(masked_data_list2, "./projects/main/data/mountain_freq_hour_all_dt_2001_20.RDS")

############

# plot --------------------------------------------------------------------

mount_data_dt <- rbindlist(masked_data_list2)
mount_data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(mount_data_dt$name.x) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

## for land, ocean and Global
mean_24h_mount <- mount_data_dt[, .(mean_value = mean(prec_freq, na.rm = TRUE)), by = .(hour(time_lst), name.x, name.y, region)]

unique(mount_data_dt$featurecla)
unique(mount_data_dt$name.y)
unique(mount_data_dt$region)

north_amric <- mean_24h_mount[region == "North America"]
south_amric <- mean_24h_mount[region == "South America"]
asia <- mean_24h_mount[region == "Asia"]
south_afric <- mean_24h_mount[region == "South Africa"]
europe <- mean_24h_mount[region == "Europe"]
africa <- mean_24h_mount[region == "Africa"]

mount_data_dt[name.y == "ROCKY MOUNTAINS"]

unique(north_amric$name.y)

ggplot(north_amric[name.x == "IMERG"], aes(hour, mean_value, col = name.y, group = name.y)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~name.y, scales = "free_y")

ggplot(north_amric, aes(hour, mean_value, col = name.x, group = name.x)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~name.y, scales = "free_y") + 
  labs(x ="Hour (LST)", y = "freq (mm/hr)", fill = "") + 
  theme_generic

ggsave("./projects/main/results/13b_mount_na_24hlineplot_freq.png",
       width = 10.6, height = 5.2, units = "in", dpi = 600)


ggplot(south_amric, aes(hour, mean_value, col = name.x, group = name.x)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~name.y, scales = "free_y") + 
  labs(x ="Hour (LST)", y = "freq (mm/hr)", fill = "") + 
  theme_generic

ggsave("./projects/main/results/13b_mount_sa_24hlineplot_freq.png",
       width = 10.6, height = 5.2, units = "in", dpi = 600)

### Europe

ggplot(europe, aes(hour, mean_value, col = name.x, group = name.x)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~name.y, scales = "free_y") + 
  labs(x ="Hour (LST)", y = "freq (mm/hr)", fill = "") + 
  theme_generic

ggsave("./projects/main/results/13b_mount_europ_24hlineplot_freq.png",
       width = 10.6, height = 5.2, units = "in", dpi = 600)


### Aisa

ggplot(asia, aes(hour, mean_value, col = name.x, group = name.x)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~name.y, scales = "free_y") + 
  labs(x ="Hour (LST)", y = "freq (mm/hr)", fill = "") + 
  theme_small

ggsave("./projects/main/results/13b_mount_asia_24hlineplot_freq.png",
       width = 10.6, height = 5.2, units = "in", dpi = 600)


### Africa

ggplot(africa, aes(hour, mean_value, col = name.x, group = name.x)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~name.y, scales = "free_y") + 
  labs(x ="Hour (LST)", y = "freq (mm/hr)", fill = "") + 
  theme_small

ggsave("./projects/main/results/13b_mount_africa_24hlineplot_freq.png",
       width = 10.6, height = 5.2, units = "in", dpi = 600)


#################################
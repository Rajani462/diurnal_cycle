#################################
# try with the cmorph datasets
library(terra)
library(data.table)
library()

nc_file <- rast("C:/Users/rkpra/Downloads/cmorph_hour_JF_kenya_2001_20.nc")
names(nc_file) <- time(nc_file)
threshold <- 0.1
precipitation_mask <- nc_file > threshold

# Assuming precipitation_mask is your binary mask
# clumps <- raster::clump(precipitation_mask)
# freq(clumps)
prec_system <- patches(precipitation_mask, zeroAsNA=TRUE)

prec_dt2 <- as.data.table(as.data.frame(prec_system, xy = TRUE, na.rm = FALSE))

#head(prec_dt2)

prec_dt3 <- melt(prec_dt2, id.vars = c('x', 'y'), variable.name = "time")

#summary(prec_dt3)

#prec_feat <- prec_dt3[, .(mean_size = mean(value, na.rm = TRUE)), by = .(x, y)]

#prec_dt4 <- prec_dt3[, .N, by = .(x, y, time)]

#prec_dt3[complete.cases(prec_dt3), ]

prec_dt3[, count := .N, by = .(value, time)]

#prec_syst_mean <- prec_dt3[, .(mean_size = median(count, na.rm = TRUE)), by = .(x, y)]
prec_syst_mean <- prec_dt3[complete.cases(prec_dt3), 
                           .(mean_size = median(count, na.rm = TRUE)), 
                           by = .(x, y)]
summary(prec_syst_mean)


#plot
library(ggplot2)

ggplot(prec_syst_mean, aes(x, y, fill = mean_size)) +
  geom_raster() +
  scale_fill_gradient(low = "blue", high = "red")  # You can adjust colors as needed


#freq

library(ggplot2)

# Assuming 'count' is the column representing the frequency of occurrences
prec_syst_count <- prec_dt3[complete.cases(prec_dt3), .(count = .N), by = .(x, y)]

ggplot(prec_syst_count, aes(x, y, fill = count)) +
  geom_raster(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red")  # Customize colors as needed

#######################################################################################################

nc_file <- rast("~/rajani/diurnal_cycle/projects/kenya_example/data/output/precip_system/tmp_regions_label.nc")



prec_dt2 <- as.data.table(as.data.frame(nc_file[[1]], xy = TRUE, na.rm = FALSE))

prec_dt2[cmorph_1 > 0]



########################################################################

library(terra)
library(data.table)
library(doParallel)
library(raster)

# Register parallel backend with the desired number of cores/workers
cores <- detectCores() - 10
cl <- makeCluster(cores)
registerDoParallel(cl)

nc_file <- raster("~/rajani/diurnal_cycle/projects/kenya_example/data/cmorph_hour_JF_kenya_2001_20.nc")
#names(nc_file) <- time(nc_file)
threshold <- 1
precipitation_mask <- nc_file > threshold

# Function to compute patches in parallel
compute_patches <- function(mask) {
  raster::clump(mask, zeroAsNA = TRUE)
}

# Use foreach to parallelize the computation
prec_system <- foreach(i = 1:nlayers(precipitation_mask), .combine = rbind) %dopar% {
  compute_patches(precipitation_mask[[i]])
}

# Stop the parallel backend
stopCluster(cl)

prec_dt2 <- as.data.table(as.data.frame(prec_system, xy = TRUE, na.rm = FALSE))

prec_dt2[complete.cases(prec_dt2), ]


###########################################

# for the entire globe 60 NS ----------------------------------------------


library(terra)
library(data.table)
library(doParallel)
library(raster)

# Register parallel backend with the desired number of cores/workers
cores <- detectCores() - 10
cl <- makeCluster(cores)
registerDoParallel(cl)

nc_file <- brick("~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/cmorph_tp_mm_60ns_2001_jan_025_hourly_rotated.nc")
#names(nc_file) <- time(nc_file)
threshold <- 1
precipitation_mask <- nc_file > threshold

# Function to compute patches in parallel
compute_patches <- function(mask) {
  raster::clump(mask, zeroAsNA = TRUE)
}

# Use foreach to parallelize the computation
prec_system <- foreach(i = 1:nlayers(precipitation_mask), .combine = rbind) %dopar% {
  compute_patches(precipitation_mask[[i]])
}

# Stop the parallel backend
stopCluster(cl)

prec_dt2 <- as.data.table(as.data.frame(prec_system, xy = TRUE, na.rm = FALSE))

prec_dt2[complete.cases(prec_dt2), ]




#########################

library(terra)
library(data.table)
install.packages("futurize")
library(futurize)
install.packages("uture.apply")
library(future.apply)

# Register parallel backend with the desired number of cores/workers
plan(multisession, workers = detectCores())

nc_file <- rast("C:/Users/rkpra/Downloads/cmorph_hour_JF_kenya_2001_20.nc")
names(nc_file) <- time(nc_file)
threshold <- 1

# Split the data into chunks (adjust the chunk size based on your data size)
chunk_size <- 10
chunks <- split(nc_file, rep(1:chunk_size, length.out = nlayers(nc_file)))

# Function to compute the mask for a chunk
compute_mask <- function(chunk) {
  return(chunk > threshold)
}

# Use future.apply::future_lapply to apply the function in parallel
future_masks <- future.apply::future_lapply(chunks, compute_mask)

# Combine the results
precipitation_mask <- do.call(c, future_masks)

# Unregister the parallel backend
plan(NULL)

# Assuming precipitation_mask is your binary mask
prec_system <- patches(precipitation_mask, zeroAsNA = TRUE)

prec_dt2 <- as.data.table(as.data.frame(prec_system, xy = TRUE, na.rm = FALSE))


#############################


library(raster)
library(data.table)
library(doParallel)

# Register parallel backend with the desired number of cores/workers
cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

nc_file <- brick("~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/cmorph_tp_mm_60ns_2001_jan_025_hourly_rotated.nc")
#nc_file <- brick("~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/cmorph_tp_mm_60ns_2001_025_hourly_rotated.nc")

#names(nc_file) <- time(nc_file)
threshold <- 0.1

# Function to compute patches in parallel
compute_patches <- function(mask) {
  raster::clump(mask, zeroAsNA = TRUE)
}

# Use foreach to parallelize the computation (takes around 1 - 2 min)
prec_system_list <- foreach(i = 1:nlayers(nc_file), .combine = 'c') %dopar% {
  compute_patches(nc_file[[i]] > threshold)
}


time_temp = getZ(nc_file)
names(prec_system_list) <- time_temp


subset_raste <- prec_system_list[1:2]
subset_raste_list <- lapply(prec_system_list, function(x) as.data.table(as.data.frame(x, xy = TRUE, long = TRUE, na.rm = FALSE)))


prec_system_list <- lapply(prec_system_list, function(x) as.data.table(as.data.frame(x, xy = TRUE, na.rm = FALSE)))

#str(prec_system_list)

# Convert each RasterLayer in the list to data.table
prec_system_list <- lapply(prec_system_list, function(x) as.data.table(as.data.frame(x, xy = TRUE, na.rm = FALSE)))

# Assuming your list is named 'your_list'
# for (i in seq_along(prec_system_list)) {
#   prec_system_list[[i]][, date := as.POSIXct(names(prec_system_list)[i])]
# }

prec_system_list <- lapply(names(prec_system_list), function(date) {
  prec_system_list[[date]][, date := as.POSIXct(date)]
})

your_list <- lapply(prec_system_list, function(dt) {
  # Melt the data.table
  #dt_melted <- melt(dt, id.vars = c('x', 'y', 'clumps'), variable.name = "date")
  
  # Count occurrences and create 'count' column
  dt[, count := .N, by = .(clumps)]
  return(dt)
})


wide_list <- lapply(your_list, function(dt) dcast(dt, x + y ~ date, value.var = c("clumps", "count")))


subset_rasters <- your_list[2:4]

lapply(your_list[1], summary)
summary()


apply(array(unlist(subset_rasters), dim = c(dim(subset_rasters[[1]]), length(subset_rasters))), c(1, 2, 3, 4, 5), mean)


# Stop the parallel backend
stopCluster(cl)





subset_rasters <- raster_list[[c(2, 3)]]


# Assuming your list is named 'your_list'
your_list <- lapply(prec_system_list, function(dt) {
  # Melt the data.table
  #dt_melted <- melt(dt, id.vars = c('x', 'y', 'clumps'), variable.name = "date")
  
  # Count occurrences and create 'count' column
  dt[, count := .N, by = .(clumps)]
  
  return(dt)
})

# Check the result
print(your_list)

last_list <- your_list[[1,2]]

summary(last_list)


library(data.table)

# Assuming your list is named 'your_list'
result_list <- lapply(your_list, function(dt) {
  # Filter out rows with NAs
  dt_filtered <- dt[complete.cases(dt)]
  
  # Calculate mean and create a data.table
  dt_result <- dt_filtered[, .(mean_count = mean(count, na.rm = TRUE)), by = .(x, y)]
  
  return(dt_result)
})



mean_values <- rowMeans(do.call(cbind, lapply(your_list, function(dt) dt[, .(x, y, clumps, count)])))



# Combine the results into a single data.table
final_result <- rbindlist(result_list)

# Check the final result
print(final_result)



# Combine the list of data.tables into a single data.table
prec_dt2 <- rbindlist(prec_system_list)

prec_dt2[complete.cases(prec_dt2), ]

saveRDS(prec_dt2, "~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/prec_system_2001_jan.RDS")
saveRDS(prec_system_list, "~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/prec_system_2001.RDS")

########

#Reaerd the data and plot

prec_dt2 <- readRDS("~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/prec_system_2001_jan.RDS")

prec_dt3 <- melt(prec_dt2, id.vars = c('x', 'y'), variable.name = "time")

prec_dt3[, count := .N, by = .(value, time)]

#prec_syst_mean <- prec_dt3[, .(mean_size = median(count, na.rm = TRUE)), by = .(x, y)]
prec_syst_mean <- prec_dt3[complete.cases(prec_dt3), 
                           .(mean_size = median(count, na.rm = TRUE)), 
                           by = .(x, y)]

saveRDS(prec_syst_mean, "~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/prec_syst_mean_2001_jan.RDS")

summary(prec_syst_mean)


#plot----------------------------------------------------------

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

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/graphics.R')

# ggplot(prec_syst_mean, aes(x, y, fill = mean_size)) +
#   geom_raster() +
#   scale_fill_gradient(low = "blue", high = "red")  # You can adjust colors as needed


extracted_data_list <- readRDS("~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/prec_syst_mean_2001_jan.RDS") %>% list

# Use lapply to create a list of rasters
raster_list <- lapply(extracted_data_list, create_raster)
raster_brick <- brick(raster_list)

PROJ <- "+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
rastlist_robin <- projectRaster(raster_brick, crs = PROJ, method="ngb")

# Convert spatial data to data frame
rast_robin_sp <- as(rastlist_robin, "SpatialPixelsDataFrame")
rast_robin_df <- as.data.frame(rast_robin_sp) %>% as.data.table()

# to_plot <- melt(rast_robin_df, c("x", "y"), variable.name = "name")
# to_plot2 <- to_plot[, .(x, y, peak_hour = value), name]


summary(rast_robin_df)


ggplot() +
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "white", size = 0.25) +
  geom_polygon(data = NE_box_rob, aes(x = long, y = lat), colour = "black", fill = "transparent", size = 0.25) +
  geom_path(data = NE_graticules_rob, aes(long, lat, group = group), linetype = "dotted", color = "grey50", size = 0.25) +
  # geom_text(data = lbl.Y.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2, hjust = 1.5) +
  # geom_text(data = lbl.X.prj[c(FALSE, FALSE, FALSE, TRUE), ], aes(x = X.prj, y = Y.prj, label = lbl), color = "black", size = 2.2) +
  coord_fixed(ratio = 1) +
  geom_tile(data = rast_robin_df, aes(x = x, y = y, fill = mean_size), alpha = 1) + 
  #facet_wrap(~name, ncol = 3) + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1,
                    breaks = c(1000, 10000, 15000, 25000, 50000, 100000), show.limits = TRUE) + 
  labs(x = NULL, y = NULL, fill = "Number of grids") + 
  geom_polygon(data = NE_countries_rob, aes(long, lat, group = group),
               colour = "black", fill = "transparent", size = 0.25) +
  theme_small +
  theme(plot.title = element_text(hjust = 0.3, size = 8, face = "bold"),
        legend.position = "bottom",
        legend.key.width = unit(2.8, "cm"),
        legend.key.height = unit(0.4, "cm"), 
        legend.spacing = unit(0.25,"cm"),
        legend.text = element_text(size = 12), 
        legend.title = element_text(hjust = 0.5, size = 12),
        legend.justification = "center") +
  theme(strip.background = element_blank(), panel.border=element_blank()) + 
  scale_x_discrete(breaks = NULL) + 
  scale_y_discrete(breaks = NULL) + 
  guides(fill=guide_coloursteps(title.position="top"))

ggsave("~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/cmorph_precip_syst_size_jan_2001_0.1mm.png", width = 10.5, height = 5.1, 
       units = "in", dpi = 600)


##########################

prec_dt2 <- readRDS("~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/prec_system_2001.RDS")

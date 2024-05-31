

library(raster)
library(data.table)
library(doParallel)

# Register parallel backend with the desired number of cores/workers
cores <- detectCores()
cl <- makeCluster(cores)
registerDoParallel(cl)

nc_file <- brick("~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/cmorph_tp_mm_60ns_2001_jan_025_hourly_rotated.nc")
#nc_file <- brick("~/rajani/diurnal_cycle/projects/main/data/subset_data_2001/cmorph_tp_mm_60ns_2001_025_hourly_rotated.nc")
#nc_file <- brick("~/rajani/diurnal_cycle/projects/kenya_example/data/cmorph_hour_JF_kenya_2001_20.nc")

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

# Convert each RasterLayer in the list to data.table
#prec_system_list <- lapply(prec_system_list, function(x) data.table::as.data.table(as.data.frame(x, xy = TRUE, optional = TRUE, na.rm = FALSE)))
# prec_system_list <- parLapply(cl, prec_system_list, function(x) data.table::as.data.table(as.data.frame(x, xy = TRUE, na.rm = FALSE)))

format(object.size(prec_system_list),units="Gb")

clusterEvalQ(cl, {
  library(data.table)
  library(raster)
})

# Parallelize the lapply operation
prec_system_list <- parLapply(cl, prec_system_list, function(x) {
  as.data.table(as.data.frame(x, xy = TRUE, na.rm = FALSE))
})
# Assuming your list is named 'your_list'
# for (i in seq_along(prec_system_list)) {
#   prec_system_list[[i]][, date := as.POSIXct(names(prec_system_list)[i])]
# }

format(object.size(prec_system_list),units="Mb")

prec_system_list <- lapply(names(prec_system_list), function(date) {
  prec_system_list[[date]][, date := as.POSIXct(date)]
})



prec_system_list <- parLapply(cl, prec_system_list, function(dt) {
  # Melt the data.table
  #dt_melted <- melt(dt, id.vars = c('x', 'y', 'clumps'), variable.name = "date")
  
  # Count occurrences and create 'count' column
  dt[, count := .N, by = .(clumps)]
  return(dt)
})

stopCluster(cl)


# Set up a parallel cluster
cl <- makeCluster(detectCores() - 6)

# Export the required object to the cluster
clusterExport(cl, c("prec_system_list"))

# Apply the operation in parallel
system.time(final_list <- parLapply(cl, 1:691200, function(i) data.table::rbindlist(lapply(prec_system_list, function(df) df[i]))))

# Clean up the cluster
stopCluster(cl)


# prec_system_list <- lapply(prec_system_list, function(dt) {
#   dt[, id := 1:.N]
#   setkey(dt, "id")
#   return(dt)
# })

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

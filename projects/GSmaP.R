
source('./source/libs.R')
source('./source/functions.R')

# library(parallel)
# library(dplyr)
# library(data.table)

#output_dir <- "./projects/kenya_example/code/achieve/"
output_dir <- "~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/2020_nc/"


file_name <- list.files("~/shared/data_downloads/GSMAP/ftp_downloads/2020", pattern = "*.h5", full.names = TRUE) %>% as.list()

no_cores <- detectCores() - 14
cluster <-  parallel::makeCluster(no_cores, type = "PSOCK")
clusterExport(cluster, c("convertH5toNC", "output_dir"))

length(file_name)

# parLapply(cluster, file_name[1:2000], function(x) convertH5toNC(x, output_dir)) # don't run >3000 files:gives error (R_large124GB)
# parLapply(cluster, file_name[2001:4000], function(x) convertH5toNC(x, output_dir)) # don't run >3000 files:gives error (R_large124GB)
# parLapply(cluster, file_name[4001:6000], function(x) convertH5toNC(x, output_dir)) # don't run >3000 files:gives error (R_large124GB)
# parLapply(cluster, file_name[6001:8000], function(x) convertH5toNC(x, output_dir)) # don't run >3000 files:gives error (memory)
parLapply(cluster, file_name[8001:length(file_name)], function(x) convertH5toNC(x, output_dir)) # don't run >3000 files:gives error (memory)

# parLapply(cluster, file_name[8001:8784], function(x) convertH5toNC(x, output_dir)) # don't run >3000 files:gives error (memory) 

parallel::stopCluster(cluster)

##################################

library(raster)

da <- raster("~/shared/data_downloads/GSMAP/ftp_downloads/GPMMRG_MAP_1403010000_H_L3S_MCH_04B.nc")
plot(da)

da_dt <- as.data.frame(da, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("gsmap"))

summary(da_dt)

library(ggplot2)
library(viridis)

ggplot(da_dt, aes(y, x, fill = prec_mean)) + 
  geom_tile() + 
  scale_fill_binned(type = "viridis", direction = -1, 
                    breaks = c(0.01, 0.05, 0.1, 0.3, 0.7, 0.9, 1.0, 2, 10), show.limits = TRUE)


gsm <- raster("~/shared/data_downloads/GSMAP/ftp_downloads/2015/2015_nc/gsmap_hour_2015.nc")
gsm_regrd <- brick("~/shared/data_downloads/GSMAP/ftp_downloads/2015/2015_nc/gsmap_hour_2015_grid_025.nc")

gsm_ken <- raster("./projects/kenya_example/data/gsmap_hour_kenya_2015_grid_025.nc")

########################

trl <- brick("./projects/kenya_example/data/gsmap_hour_kenya_2015_20.nc")

trl <- brick("~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_2015_20_grid_025.nc")

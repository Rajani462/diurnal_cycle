
library(ncdf4)
library(data.table)
library(raster)
library(rgdal)
library(dplyr)
#library(terra)
library(ggplot2)
library(viridis)
#library(sf)
#library(doParallel)

source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')

# import the datasets -----------------------------------------------------

# mean hourly precipitation (1 time steps or 1 layer)
imerg <- rast("~/rajani/diurnal_cycle/projects/kenya_example/data/mean_imerg_kenya_2001_20.nc")
era5 <- raster("~/rajani/diurnal_cycle/projects/kenya_example/data/mean_era5_kenya_2001_20.nc")
cmorph <- raster("~/rajani/diurnal_cycle/projects/kenya_example/data/mean_cmorph_kenya_2001_20.nc")
pers <- raster("./projects/kenya_example/data/mean_persiann_kenya_2001_20.nc")
gsmap <- raster("./projects/kenya_example/data/mean_gsmap_kenya_2015_20.nc")


imerg_t <- raster("~/rajani/diurnal_cycle/projects/kenya_example/data/outfile.nc")


## pre-process (correct the IMERG and GSMaP) ---------------

#GSMaP
plot(gsmap)
gsmap_t <- t(gsmap)
plot(gsmap_t)

gsmap_flipx <- flip(gsmap_t, direction = 1) # flip in x direction
plot(gsmap_flipx) #now the plots looks similar to panoply

gsmap_cor2 <- flip(t(gsmap), direction = "x") 
plot(gsmap_cor2)

#imerg

plot(imerg)
imerg_cor <- flip(t(imerg), direction = "x") 
plot(imerg_cor2)


# convert to data.table ---------------------------------------------------

imerg_mean_dt <- as.data.frame(imerg_cor, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("imerg"))

#imerg_mean_dt <- imerg_mean_dt[, .(x = y, y = x, date, prec_mean, name)]

cmorph_mean_dt <- as.data.frame(cmorph, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("cmorph"))

era5_mean_dt <- as.data.frame(era5, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("era5"))

persiann_mean_dt <- as.data.frame(pers, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("persiann"))

gsmap_mean_dt <- as.data.frame(gsmap.flipy, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("gsmap"))

gsmap_mean_dt <- gsmap_mean_dt[, .(x = y, y = x, date, prec_mean, name)]


# merge all
all_dt <- rbind(imerg_mean_dt, cmorph_mean_dt, persiann_mean_dt, era5_mean_dt, gsmap_mean_dt)

all_dt <- all_dt[, .(lon = x, lat = y, prec_mean =round(prec_mean, 2), name)]

saveRDS(all_dt, "./projects/kenya_example/data/hourly_charact/all_hourly_mean.RDS")

#############


# Verification plot -------------------------------------------------------


levels(all_dt$name) <- c("IMERG", "CMORPH", "PERSIANN", "ERA5", "GSMaP")

summary(all_dt)

ggplot(all_dt) + 
  geom_raster(aes(lon, lat, fill = prec_mean)) +
  #scale_fill_viridis(direction = -1) + 
  scale_fill_binned(type = "viridis", direction = -1, 
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.5), show.limits = TRUE) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(all_dt$lon), max(all_dt$lon)), 
                  ylim = c(min(all_dt$lat), max(all_dt$lat))) + 
  
  facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Precipitation\n (mm/hr)") +
  theme_small +
  theme(legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

# ggsave("./projects/kenya_example/results/spatial_hourly_mean.png", width = 8.5, height = 5.3, 
#        units = "in", dpi = 600)



# pers_hou <- brick("./projects/kenya_example/data/hourly_charact/hourly_mean_persiann_kenya_2001_20.nc")
# trl_persiann_dt <- as.data.frame(pers_hou, xy = TRUE) %>%
#   as.data.table() %>%
#   melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
#   `[`(, name := factor("persiann"))



# hourly means (24 time steps) -----------------------------------------------------


imerg_hour <- brick("~/rajani/diurnal_cycle/projects/kenya_example/data/hourly_charact/hourly_mean_imerg_kenya_2001_20.nc")
era_hour <- brick("~/rajani/diurnal_cycle/projects/kenya_example/data/hourly_charact/hourly_mean_era5_kenya_2001_20.nc")
cmorph_hour <- brick("~/rajani/diurnal_cycle/projects/kenya_example/data/hourly_charact/hourly_mean_cmorph_kenya_2001_20.nc")
persiann_hour <- brick("~/rajani/diurnal_cycle/projects/kenya_example/data/hourly_charact/hourly_mean_persiann_kenya_2001_20.nc")
gsmap_hour <- brick("~/rajani/diurnal_cycle/projects/kenya_example/data/hourly_charact/hourly_mean_gsmap_kenya_2015_20.nc")

# Pre-process-------------------------------------

#GSMaP
gsmap_t <- t(gsmap_hour)
gsmap_hour_flipx <- flip(gsmap_t, direction = 1) # flip in x direction
#plot(gsmap.flipx) #now the plots looks similar to panoply

#imerg
imerg_hour_t <- t(imerg_hour)
#plot(x)
imerg_hour_flipx = flip(imerg_hour_t, direction = 1) 
# imerg_hour_flipxy = flip(imerg_hour_flipy, direction = 1)
# plot(imerg_hour_flipxy[[1]]) #looks similar to panoply


names(persiann_hour) <- names(cmorph_hour)
names(imerg_hour_flipx) <- names(imerg_hour)
names(gsmap_hour_flipx) <- names(gsmap_hour)

#convert the above data to data,table format and save ---------------

imerg_dt <- as.data.frame(imerg_hour_flipx, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("imerg"))

#imerg_dt <- imerg_dt[, .(x = y, y = x, date, prec_mean, name)]

cmorph_dt <- as.data.frame(cmorph_hour, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  as.data.table() %>%
  `[`(, name := factor("cmorph"))

era5_dt <- as.data.frame(era_hour, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("era5"))

persiann_dt <- as.data.frame(persiann_hour, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("persiann"))

gsmap_dt <- as.data.frame(gsmap_hour_flipx, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("gsmap"))

#gsmap_dt <- gsmap_dt[, .(x = y, y = x, date, prec_mean, name)]

# merge all
all_dt <- rbind(imerg_dt, cmorph_dt, persiann_dt, era5_dt, gsmap_dt)
#remove X from date
all_dt$date <- substr(all_dt$date, 2, nchar(as.character(all_dt$date)))

# Convert factor time format to R time series
all_dt$date <- as.POSIXct(all_dt$date, format = "%Y.%m.%d.%H.%M.%S")
all_dt <- all_dt[, .(lon = x, lat  = y, datetime = date, prec_mean, name)]
all_dt <- tidyr::separate(all_dt, datetime, c("date", "time"), sep = " ") %>% 
  as.data.table() 

spat_avg <- all_dt[, .(prec_mean = mean(prec_mean, na.rm = TRUE)), by = .(time, name)]
library(hms)
spat_avg$time <- as_hms(spat_avg$time)
levels(spat_avg$name) <- c("IMERG", "CMORPH", "PERSIANN", "ERA5", "GSMaP")
ggplot(spat_avg, aes(time, prec_mean, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Time", y = "Precipittion (mm/hr)", fill = "") + 
  theme_small +  
  theme(legend.title = element_blank())

ggsave("./projects/kenya_example/results/diurnal_precipi.png",
       width = 8.5, height = 5.9, units = "in", dpi = 600)

## convert the time from UTC to LST ----------------------------

# formula by Watters et.,al,: LST=UTC+ (λ/15∘h−1)

str(all_dt)

all_dt_lst <- all_dt[, .(lat, lon, time_utc = as_hms(time), 
                         tmz_offset = round((lon / 15)), prec_mean, name)]

all_dt_lst$time_utc <- as.POSIXct(all_dt_lst$time_utc)
all_dt_lst[,  `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]

saveRDS(all_dt_lst, "./projects/kenya_example/data/hourly_charact/all_24_hourly_mean.RDS")

spat_avg_lst <- all_dt_lst[, .(prec_mean = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name)]
# library(hms)
# spat_avg$time <- as_hms(spat_avg$time)

levels(spat_avg_lst$name) <- c("IMERG", "CMORPH", "PERSIANN", "ERA5", "GSMaP")

ggplot(spat_avg_lst, aes(hour, prec_mean, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Time", y = "Precipittion (mm/hr)", fill = "") + 
  theme_generic + 
  theme(legend.title = element_blank(), legend.position = "right", legend.direction = "vertical")


ggsave("./projects/kenya_example/results/diurnal_precipi_lst.png",
       width = 8.5, height = 5.9, units = "in", dpi = 600)

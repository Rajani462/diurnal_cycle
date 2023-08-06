
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

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')



dat_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")


# # converting the time from utc to LST for a list of data.tables
# 
# dat_lst_list <- lapply(dat_list, function(dt) {
#   dt$date <- substr(dt$date, 13, 14) %>% paste0(":00:00")
#   dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_mean, name)]
#   dt[, `:=`(tmz_offset = round((lon / 15)))]
#   dt$time_utc <- as.POSIXct(dt$time_utc)
#   dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
#   return(dt)
# })
# 
# saveRDS(dat_lst_list, "./projects/main/data/hourly_mean_all_datasets_glob_LST_2001_20.rds")
# 

# Estimate the peak hour of data.tables is called "data_list"
peak_hour_list <- lapply(dat_list, function(dt) {
  # Modify the code to use the data.table "int_freq"
  peak_hour <- dt[, .SD[which.max(prec_mean)], by = .(lat, lon, name)]
  peak_hour <- peak_hour[, .(lat, lon, peak_hour = hour(time_lst),name)]
  return(peak_hour)
})


#merge the list to data.table
peak_hour_dt <- rbindlist(peak_hour_list)

# Check the new order of levels
levels(peak_hour_dt$name)
# Change the order of levels 
levels(peak_hour_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "ERA5")


#library(forcats)
ggplot(peak_hour_dt) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_wrap(~name, ncol = 2) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))


ggsave("./projects/main/results/xxxx_plot_spat_peak_hour_mean.png", width = 10.5, height = 5.3, 
       units = "in", dpi = 600)



### spatial mean plot --------------------------------------

spat_mean_list <- lapply(dat_list, function(dt) {
  dt[, .(mean_value = round(mean(prec_mean, na.rm = TRUE), 2)), by = .(lat, lon, name)]
})

spat_mean_dt <- rbindlist(spat_mean_list)
summary(spat_mean_dt)

levels(spat_mean_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "ERA5")

ggplot(spat_mean_dt) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.5, 1, 2), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_dt$lon), max(spat_mean_dt$lon)), 
                  ylim = c(min(spat_mean_dt$lat), max(spat_mean_dt$lat))) + 
  facet_wrap(~name, ncol = 2) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean\n precipitation \n (mm/hr)") + 
  #facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")) + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/xxxx_plot_mean.png", width = 10.5, height = 5.3, 
       units = "in", dpi = 600)

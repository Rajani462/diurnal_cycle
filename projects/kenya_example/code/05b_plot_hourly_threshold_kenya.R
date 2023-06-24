
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

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


## read the data sets -------------------------------

dat_thres_list <- readRDS("./projects/kenya_example/data/output/diurnal_int_freq_thres_list.RDS")


## Pre-process -----------------------------------------------

#dat_list <- lapply(dat_thres_list, rbindlist)
dat_list <- dat_thres_list

# change the time to LST 
# converting the time from utc to LST for a list of data.tables
dat_lst_list <- lapply(dat_list, function(dt) {
  dt$date <- substr(dt$date, 7, 8) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), value, name, variable, threshold)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})


### spatial mean plot --------------------------------------

spat_mean_list <- lapply(dat_lst_list, function(dt) {
  dt[, .(mean_value = round(mean(value, na.rm = TRUE), 2)), by = .(lat, lon, name, variable, threshold)]
})

# Combine the results into a single data.table
spat_mean_dt <- rbindlist(spat_mean_list)
levels(spat_mean_dt$name)

# Change the order of levels in the 'name' column
# spat_mean_dt$name <- fct_recode(spat_mean_dt$name, CMORPH = "cmorph", IMERG = "imerg", PERSIANN = "persiann", 
#            ERA5 = "era5", GSMaP = "gsmap")

levels(spat_mean_dt$name) <- c("CMORPH", "IMERG", "PERSIANN", "ERA5", "GSMaP")
levels(spat_mean_dt$threshold) <- c("0.1 (mm/hr)", "0.5 (mm/hr)", "1 (mm/hr)")

# mean precipitation
summary(spat_mean_dt[variable == "mean"])

ggplot(spat_mean_dt[variable == "mean"]) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", direction = -1, 
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.5), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_dt$lon), max(spat_mean_dt$lon)), 
                  ylim = c(min(spat_mean_dt$lat), max(spat_mean_dt$lat))) + 
  facet_grid(threshold~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean\n precipitation \n (mm/hr)") + 
  theme_small + 
  theme(legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm')) + 
  facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5"))


ggsave("./projects/kenya_example/results/05b_mean_threshold.png", width = 8.8, height = 6.3,, 
       units = "in", dpi = 600)


# mean intensity
summary(spat_mean_dt[variable == "intensity"])

ggplot(spat_mean_dt[variable == "intensity"]) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", direction = -1, 
                    breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_dt$lon), max(spat_mean_dt$lon)), 
                  ylim = c(min(spat_mean_dt$lat), max(spat_mean_dt$lat))) + 
  facet_grid(threshold~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean \n intensity (mm/hr)") + 
  theme_small + 
  theme(legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm')) + 
  facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5"))


ggsave("./projects/kenya_example/results/05b_intensity_threshold.png", width = 8.8, height = 6.3,, 
       units = "in", dpi = 600)

spat_mean_dt[variable == "intensity" & mean_value >= 7]

#mean frequency
summary(spat_mean_dt[variable == "frequency"])
spat_mean_dt[variable == "frequency" & mean_value >= 60]

ggplot(spat_mean_dt[variable == "frequency"]) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", direction = -1, 
                    breaks = c(2, 5, 10, 15, 20, 30, 40, 50), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_dt$lon), max(spat_mean_dt$lon)), 
                  ylim = c(min(spat_mean_dt$lat), max(spat_mean_dt$lat))) + 
  facet_grid(threshold~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean \n frequency (%)") + 
  theme_small + 
  theme(legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm')) + 
  facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5"))


ggsave("./projects/kenya_example/results/05b_frequency_threshold.png", width = 8.8, height = 6.3,, 
       units = "in", dpi = 600)



# Estimate the peak hour of data.tables is called "data_list" -----------------
peak_hour_list <- lapply(dat_lst_list, function(dt) {
  # Modify the code to use the data.table "int_freq"
  peak_hour <- dt[, .SD[which.max(value)], by = .(lat, lon, name, variable, threshold)]
  peak_hour <- peak_hour[, .(lat, lon, peak_hour = hour(time_lst),name, variable, threshold)]
  return(peak_hour)
})

## peak hour of intensity and frequency plot -----------------------------------

peak_hour_dt <- rbindlist(peak_hour_list)
levels(peak_hour_dt$name) <- c("CMORPH", "IMERG", "PERSIANN", "ERA5", "GSMaP")
# peak_hour_dt$name <- ordered(peak_hour_dt$name, levels = c("CMORPH", "IMERG", "PERSIANN", "ERA5", "GSMaP"))

### mean 

ggplot(peak_hour_dt[variable  == "mean"]) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_grid(threshold~name) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of\n mean") + 
  theme_small + 
  facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5"))


ggsave("./projects/kenya_example/results/05b_peak_int_threshold.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)


### intensity 

ggplot(peak_hour_dt[variable  == "intensity"]) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_grid(threshold~name) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of\n intensity") + 
  theme_small + 
  facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5"))


ggsave("./projects/kenya_example/results/05b_peak_int_threshold.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)


### frequency

ggplot(peak_hour_dt[variable  == "frequency"]) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_grid(threshold~name) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of frequency") + 
  theme_small + 
  facet_grid(threshold~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5"))


ggsave("./projects/kenya_example/results/05b_peak_freq_threshold.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)


### 24hr diurnal cycle line plot -------------------------------------------

mean_int_freq_list <- lapply(dat_lst_list, function(dt) {
  dt[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour(time_lst), name, variable = as.factor(variable), threshold)]
})

mean_int_freq_24h <- rbindlist(mean_int_freq_list)
levels(mean_int_freq_24h$name) <- c("CMORPH", "IMERG", "PERSIANN", "ERA5", "GSMaP")
levels(mean_int_freq_24h$variable) <- c("Mean (mm/hr)", "Intensity (mm/hr)", "Frequency (%)")
levels(mean_int_freq_24h$threshold) <- c("0.1 (mm/hr)", "0.5 (mm/hr)", "1 (mm/hr)")


str(mean_int_freq_24h)

## mean, intensity and frequency together

ggplot(mean_int_freq_24h, aes(hour, mean_value, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "") + 
  facet_grid(variable~threshold, scales = "free_y") +
  # ggh4x::facet_grid2(variable~threshold, scales = "free_y", independent = "y") + 
  theme_small + 
  theme(legend.title = element_blank(), legend.direction = "horizontal", 
        legend.position = "bottom")

ggsave("./projects/kenya_example/results/05b_diurnal_mean_freq_inten_LST_threshold.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

#same as above butchange y axix to see the differences
library(ggh4x)
ggplot(mean_int_freq_24h, aes(hour, mean_value, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Time", y = "Mean intensity (mm/hr)           Mean frequency (%)", fill = "") + 
  #facet_grid(~threshold, scales = "free_y") + 
  ggh4x::facet_grid2(variable~threshold, scales = "free_y", independent = "y") + 
  theme_small + 
  theme(legend.title = element_blank(), legend.direction = "horizontal", 
        legend.position = "bottom")

ggsave("./projects/kenya_example/results/05b_vers2_diurnal_freq_inten_LST_threshold.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


#Separate plots for both intensity and frequency

ggplot(mean_int_freq_24h[variable == "intensity"], aes(hour, mean_value, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Time", y = "Mean intensity (mm/hr)", fill = "") + 
  facet_grid(~threshold, scales = "free_y") +
  # ggh4x::facet_grid2(variable~threshold, scales = "free_y", independent = "y") + 
  theme_small + 
  theme(legend.title = element_blank(), legend.direction = "horizontal", 
        legend.position = "bottom")

ggsave("./projects/kenya_example/results/05b_diurnal_inten_LST_threshold.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


ggplot(mean_int_freq_24h[variable == "frequency"], aes(hour, mean_value, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Time", y = "Mean frequency (%)", fill = "") + 
  facet_grid(~threshold, scales = "free_y") +
  # ggh4x::facet_grid2(variable~threshold, scales = "free_y", independent = "y") + 
  theme_small + 
  theme(legend.title = element_blank(), legend.direction = "horizontal", 
        legend.position = "bottom")

ggsave("./projects/kenya_example/results/05b_diurnal_freqn_LST_threshold.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)



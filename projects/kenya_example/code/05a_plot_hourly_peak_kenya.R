
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


## read the data sets -------------------------------


## read the data sets -------------------------------

dat_list_list <- readRDS("./projects/kenya_example/data/output/diurnal_mean_int_freq_list.RDS")


## Pre-process -----------------------------------------------

dat_list <- lapply(dat_list_list, rbindlist)

# change the time to LST 
# converting the time from utc to LST for a list of data.tables
dat_lst_list <- lapply(dat_list, function(dt) {
  dt$date <- substr(dt$date, 7, 8) %>% paste0(":00:00")
  dt <- dt[, .(lat = y, lon = x, time_utc = as_hms(date), value, name, variable)]
  dt[, `:=`(tmz_offset = round((lon / 15)))]
  dt$time_utc <- as.POSIXct(dt$time_utc)
  dt[, `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]
  return(dt)
})

# Estimate the peak hour of data.tables is called "data_list"
peak_hour_list <- lapply(dat_lst_list, function(dt) {
  # Modify the code to use the data.table "int_freq"
  peak_hour <- dt[, .SD[which.max(value)], by = .(lat, lon, name, variable)]
  peak_hour <- peak_hour[, .(lat, lon, peak_hour = hour(time_lst),name, variable = factor(variable))]
  return(peak_hour)
})


#merge the list to data.table
peak_hour_dt <- rbindlist(peak_hour_list)

# Check the new order of levels
levels(peak_hour_dt$name)
# Change the order of levels
peak_hour_dt$name <- reorder(peak_hour_dt$name, match(peak_hour_dt$name, c("imerg", "gsmap", "cmorph", "persiann", "era5")))
# Change names to specific format
levels(peak_hour_dt$name) <- ifelse(levels(peak_hour_dt$name) == "gsmap", "GSMaP", toupper(levels(peak_hour_dt$name)))

# Check the new order of levels
levels(peak_hour_dt$name)
levels(peak_hour_dt$variable) <- c("Mean", "Intensity", "Frequency")
# Define the desired order of levels
new_order <- c("Mean", "Frequency", "Intensity")
# Change the order of levels
peak_hour_dt$variable <- factor(peak_hour_dt$variable, levels = new_order)


library(forcats)
ggplot(peak_hour_dt) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
                  ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
  facet_grid(variable~name) +  
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))



ggsave("./projects/kenya_example/results/05a_spat_peak_mean_freq_int.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


# ### intensity
# 
# library(forcats)
# ggplot(peak_hour_dt[variable  == "intensity"]) + 
#   geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
#   scale_fill_manual(values = rainbow(24)) +
#   borders(colour = "black") +
#   coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
#                   ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
#   facet_grid(~name) +  
#   scale_x_continuous(expand = c(0, 0)) + 
#   labs(x = "", y = "", fill = "Peak hour of\n intensity") + 
#   theme_small + 
#   facet_grid(~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5"))
# 
# 
# ggsave("./projects/kenya_example/results/05b_peak_int.png", width = 8.5, height = 5.3, 
#        units = "in", dpi = 600)
# 
# 
# ### frequency
# 
# ggplot(peak_hour_dt[variable  == "frequency"]) + 
#   geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
#   scale_fill_manual(values = rainbow(24)) +
#   borders(colour = "black") +
#   coord_cartesian(xlim = c(min(peak_hour_dt$lon), max(peak_hour_dt$lon)), 
#                   ylim = c(min(peak_hour_dt$lat), max(peak_hour_dt$lat))) + 
#   facet_grid(~name) +  
#   scale_x_continuous(expand = c(0, 0)) + 
#   labs(x = "", y = "", fill = "Peak hour of frequency") + 
#   theme_small + 
#   facet_grid(~fct_relevel(name,  "IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5"))
# 
# 
# ggsave("./projects/kenya_example/results/05b_peak_freq.png", width = 8.5, height = 5.3, 
#        units = "in", dpi = 600)


### 24hr diurnal cycle line plot -------------------------------------------

mean_int_freq_list <- lapply(dat_lst_list, function(dt) {
  dt[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour(time_lst), name, variable = factor(variable))]
})

mean_int_freq_24h <- rbindlist(mean_int_freq_list)

mean_int_freq_24h$name <- reorder(mean_int_freq_24h$name, match(mean_int_freq_24h$name, c("imerg", "gsmap", "cmorph", "persiann", "era5")))
levels(mean_int_freq_24h$name) <- ifelse(levels(mean_int_freq_24h$name) == "gsmap", "GSMaP", toupper(levels(peak_hour_dt$name)))

# Check the new order of levels
levels(mean_int_freq_24h$name)

#Separate plots for mean, intensity and frequency

#Mean
ggplot(mean_int_freq_24h[variable == "mean"], aes(hour, mean_value, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "Mean precipitation (mm/hr)", fill = "") + 
  theme_generic + 
  theme(legend.title = element_blank(), legend.direction = "horizontal", 
        legend.position = "bottom")

ggsave("./projects/kenya_example/results/05a_lineplot_mean_LST.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


#Intensity
ggplot(mean_int_freq_24h[variable == "intensity"], aes(hour, mean_value, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "Mean intensity (mm/hr)", fill = "") + 
  theme_generic + 
  theme(legend.title = element_blank(), legend.direction = "horizontal", 
        legend.position = "bottom")

ggsave("./projects/kenya_example/results/05a_lineplot_intens_LST.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

# Frequency
ggplot(mean_int_freq_24h[variable == "frequency"], aes(hour, mean_value, col = name, group = name)) + 
  geom_point() + 
  geom_line() + 
  labs(x ="Hour (LST)", y = "Mean frequency (%)", fill = "") + 
  theme_generic + 
  theme(legend.title = element_blank(), legend.direction = "horizontal", 
        legend.position = "bottom")

ggsave("./projects/kenya_example/results/05a_lineplot_freq_LST.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)


## spatial plot of diurnal mean, intens and freq ----------------------------------------

spat_mean_int_freq_list <- lapply(dat_lst_list, function(dt) {
  dt[, .(mean_value = mean(value, na.rm = TRUE)), by = .(lat, lon, name, variable = factor(variable))]
})

spat_mean_int_freq <- rbindlist(spat_mean_int_freq_list)

spat_mean_int_freq$name <- reorder(spat_mean_int_freq$name, match(spat_mean_int_freq$name, c("imerg", "gsmap", "cmorph", "persiann", "era5")))
# Change names to specific format
levels(spat_mean_int_freq$name) <- ifelse(levels(spat_mean_int_freq$name) == "gsmap", "GSMaP", toupper(levels(spat_mean_int_freq$name)))


### mean

summary(spat_mean_int_freq[variable == "mean"])

ggplot(spat_mean_int_freq[variable  == "mean"]) + 
  geom_raster(aes(lon, lat, fill = round(mean_value, 2))) +
  #scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_int_freq$lon), max(spat_mean_int_freq$lon)), 
                  ylim = c(min(spat_mean_int_freq$lat), max(spat_mean_int_freq$lat))) + 
  scale_fill_binned(type = "viridis", option="B", direction = -1, 
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 0.6), show.limits = TRUE) + 
  facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean (mm/hr)") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/05a_spat_mean.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)

### intensity

summary(spat_mean_int_freq[variable == "intensity"])

ggplot(spat_mean_int_freq[variable  == "intensity"]) + 
  geom_raster(aes(lon, lat, fill = round(mean_value, 2))) +
  #scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_int_freq$lon), max(spat_mean_int_freq$lon)), 
                  ylim = c(min(spat_mean_int_freq$lat), max(spat_mean_int_freq$lat))) + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(0.5, 0.7, 1.0, 1.5, 2.0, 2.5, 3), show.limits = TRUE) + 
  facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Intensity\n (mm/hr)") + 
  theme_generic +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/05a_spat_intens.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)

### frequency

summary(spat_mean_int_freq[variable == "frequency"])

ggplot(spat_mean_int_freq[variable  == "frequency"]) + 
  geom_raster(aes(lon, lat, fill = round(mean_value, 2))) +
  #scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_mean_int_freq$lon), max(spat_mean_int_freq$lon)), 
                  ylim = c(min(spat_mean_int_freq$lat), max(spat_mean_int_freq$lat))) + 
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(2, 4, 6, 8, 10, 15, 20, 30, 40, 50), show.limits = TRUE) + 
  facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Frequency\n (%)") + 
  theme_generic +
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/05a_spat_freq.png", width = 8.5, height = 5.3, 
       units = "in", dpi = 600)




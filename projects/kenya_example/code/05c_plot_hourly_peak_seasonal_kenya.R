
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
#library(reshape)
#library(terra)
#library(ncdf4)
library(sf)
library(hms)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')


#### read the data sets -------------------------------

dataset_names <- c("imerg_allseason", "gsmap_allseason", "cmorph_allseason", "persiann_allseason", 
                   "era5_allseason")

intensity_list <- list()
frequency_list <- list()
mean_list <- list()

for (dataset_name in dataset_names) {
  mean_file <- file.path("./projects/kenya_example/data/output", paste0(dataset_name, "_mean_dt.RDS"))
  intensity_file <- file.path("./projects/kenya_example/data/output", paste0(dataset_name, "_int_dt.RDS"))
  frequency_file <- file.path("./projects/kenya_example/data/output", paste0(dataset_name, "_freq_dt.RDS"))
  
  mean_list[[dataset_name]] <- readRDS(mean_file) %>% rbindlist()
  intensity_list[[dataset_name]] <- readRDS(intensity_file) %>% rbindlist()
  frequency_list[[dataset_name]] <- readRDS(frequency_file) %>% rbindlist()
}


#### Pre-process -----------------------------------------------

mean_pr <- rbindlist(mean_list)
int <- rbindlist(intensity_list)
freq <- rbindlist(frequency_list)

mean_pr <- mean_pr[, .(x, y, date, value = prec_mean, name, season, variable = factor("mean"))]
int <- int[, .(x, y, date, value = prec_int, name, season, variable = factor("intensity"))]
freq <- freq[, .(x, y, date, value = prec_freq, name, season, variable = factor("frequency"))]

mean_int_freq <- rbind(mean_pr, int, freq)


# change the time to LST 

mean_int_freq$date <- substr(mean_int_freq$date, 2, 3) %>% paste0(":00:00")
mean_int_freq <- mean_int_freq[, .(lat = y, lon = x, time_utc = as_hms(date), value, name, season, variable)]
mean_int_freq[, `:=`(tmz_offset = round((lon / 15)))]
mean_int_freq$time_utc <- as.POSIXct(mean_int_freq$time_utc)
mean_int_freq[,  `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]


#### plot ----------------------------------------

levels(mean_int_freq$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(mean_int_freq$season) <- c("JF", "MAM", "JJAS", "OND")
levels(mean_int_freq$variable) <- c("Mean", "Intensity", "Frequency")

### 24hr diurnal cycle line plot

mean_int_freq_24h <- mean_int_freq[, .(mean_value = mean(value, na.rm = TRUE)), by = .(hour(time_lst), name, variable, season)]

ggplot(mean_int_freq_24h, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.8) + 
  geom_line() + 
  labs(x ="Time", y = "", fill = "") + 
  facet_grid(variable~season, scales = "free") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/kenya_example/results/05c_lineplot_mean_int_freq_seasonal_LST.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

# #mean
# ggplot(mean_int_freq_24h[variable == "mean"], aes(hour, mean_value, col = name, group = name)) + 
#   geom_point() + 
#   geom_line() + 
#   labs(x ="Time", y = "Mean precipitation (mm/hr)", fill = "") + 
#   facet_grid(~season) + 
#   theme_small + 
#   theme(legend.title = element_blank(), legend.direction = "horizontal", 
#         legend.position = "bottom")
# 
# ggsave("./projects/kenya_example/results/05c_diurnal_mean_LST.png",
#        width = 8.9, height = 5.6, units = "in", dpi = 600)
# 
# 
# ## Intensity
# ggplot(mean_int_freq_24h[variable == "intensity"], aes(hour, mean_value, col = name, group = name)) + 
#   geom_point() + 
#   geom_line() + 
#   labs(x ="Time", y = "Mean intensity (mm/hr)", fill = "") + 
#   facet_grid(~season) + 
#   theme_small + 
#   theme(legend.title = element_blank(), legend.direction = "horizontal", 
#         legend.position = "bottom")
# 
# ggsave("./projects/kenya_example/results/05c_diurnal_intensity_LST.png",
#        width = 8.9, height = 5.6, units = "in", dpi = 600)
# 
# 
# ## Frequency
# ggplot(mean_int_freq_24h[variable == "frequency"], aes(hour, mean_value, col = name, group = name)) + 
#   geom_point() + 
#   geom_line() + 
#   labs(x ="Time", y = "Mean frequency (%)", fill = "") + 
#   facet_grid(~season) + 
#   theme_small + 
#   theme(legend.title = element_blank(), legend.direction = "horizontal", 
#         legend.position = "bottom")
# 
# ggsave("./projects/kenya_example/results/05c_diurnal_frequency_LST.png",
#        width = 8.9, height = 5.6, units = "in", dpi = 600)

### spatial mean plot

spat_seas <- mean_int_freq[, .(mean_value = round(mean(value, na.rm = TRUE), 2)), by = .(lat, lon, name, variable, season)]

# levels(spat_seas$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
# levels(spat_seas$season) <- c("JF", "MAM", "JJAS", "OND")


## mean precipitation

summary(spat_seas[variable == "mean_value"])

ggplot(spat_seas[variable == "Mean"]) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.5, 1), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_seas$lon), max(spat_seas$lon)), 
                  ylim = c(min(spat_seas$lat), max(spat_seas$lat))) + 
  facet_grid(season~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean\n precipitation \n (mm/hr)") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/05c_spat_mean_seasonal.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


## mean intensity

summary(spat_seas[variable == "Intensity"])

ggplot(spat_seas[variable == "Intensity"]) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", option = "B",  direction = -1, 
                    breaks = c(0.5, 1, 2, 3, 4, 5, 6, 10), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_seas$lon), max(spat_seas$lon)), 
                  ylim = c(min(spat_seas$lat), max(spat_seas$lat))) + 
  facet_grid(season~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean \n intensity (mm/hr)") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/05c_sapt_intens_seasonal.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)

spat_seas[variable == "intensity" & mean_value >= 8]

## mean frequency

summary(spat_seas[variable == "Frequency"])
spat_seas[variable == "frequency" & mean_value >= 50]

ggplot(spat_seas[variable == "Frequency"]) + 
  geom_raster(aes(lon, lat, fill = mean_value)) +
  scale_fill_binned(type = "viridis", option = "B", direction = -1, 
                    breaks = c(2, 5, 10, 15, 20, 30, 40, 50, 60), show.limits = TRUE) + 
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(spat_seas$lon), max(spat_seas$lon)), 
                  ylim = c(min(spat_seas$lat), max(spat_seas$lat))) + 
  facet_grid(season~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Mean \n frequency (%)") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/05c_spat_frequency_seasonal.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


#### peak hour of mean, intensity, and frequency at seasonal ----------------------

peak_hour <- mean_int_freq[, .SD[which.max(value)], by = .(lat, lon, name, variable, season)]

#levels(peak_hour$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
#levels(peak_hour$season) <- c("JF", "MAM", "JJAS", "OND")

## peak hour of mean

peak_hour_mean <- peak_hour[variable == "Mean", .(lat, lon, peak_hour = hour(time_lst), value, name, variable, season)]

ggplot(peak_hour_mean) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_mean$lon), max(peak_hour_mean$lon)), 
                  ylim = c(min(peak_hour_mean$lat), max(peak_hour_mean$lat))) + 
  facet_grid(season~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of\n mean") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/05c_spat_peak_mean_seasonal.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


## peak hour of intensity

peak_hour_int <- peak_hour[variable == "Intensity", .(lat, lon, peak_hour = hour(time_lst), value, name, variable, season)]

ggplot(peak_hour_int) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_int$lon), max(peak_hour_int$lon)), 
                  ylim = c(min(peak_hour_int$lat), max(peak_hour_int$lat))) + 
  facet_grid(season~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of\n intensity") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/kenya_example/results/05c_spat_peak_int_seasonal.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)


## peak hour of frequency

peak_hour_freq <- peak_hour[variable == "Frequency", .(lat, lon, peak_hour = hour(time_lst), value, name, variable, season)]

ggplot(peak_hour_freq) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_hour_freq$lon), max(peak_hour_freq$lon)), 
                  ylim = c(min(peak_hour_freq$lat), max(peak_hour_freq$lat))) + 
  #facet_wrap(~name, ncol = 3) + 
  facet_grid(season~name) + 
  scale_x_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of\n frequency") + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))


ggsave("./projects/kenya_example/results/05c_spat_peak_freq_seasonal.png", width = 9.5, height = 5.3, 
       units = "in", dpi = 600)




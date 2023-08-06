
# estimate the mean diurnal cycle of mean precipitation, intensity and frequency

library(raster)
library(doParallel)
library(foreach)
library(data.table)
library(tools)
library(magrittr)
library(dplyr)
#library(reshape)
library(hms)
library(ggplot2)

source('./source/themes.R')
source('./source/palettes.R')

dataset  <- brick("./projects/main/data/hourly_mean_cmorph_glob_2001_20.nc")

cmorph_dt <- as.data.frame(dataset, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("cmorph"))

cmorph_dt
summary(cmorph_dt)

saveRDS(cmorph_dt, "./projects/main/data/hourly_mean_cmorph_glob_2001_20.rds")

# converting the time from utc to LST for a list of data.tables
cmorph_dt$date <- substr(cmorph_dt$date, 13, 14) %>% paste0(":00:00")
cmorph_dt <- cmorph_dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_mean, name)]
cmorph_dt[, `:=`(tmz_offset = round((lon / 15)))]
cmorph_dt$time_utc <- as.POSIXct(cmorph_dt$time_utc)
cmorph_dt[,  `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]

summary(cmorph_dt)
saveRDS(cmorph_dt, "./projects/main/data/hourly_mean_cmorph_glob_LST_2001_20.rds")

## plot ----------------------------------------


cmorph_dt[lat == "-59.875" & lon == "178.875"]

cmorph_dt_mean <- cmorph_dt[, .(prec_mean_mean = mean(prec_mean)), by = .(lat, lon)]
summary(cmorph_dt_mean)

ggplot(cmorph_dt_mean) + 
  geom_raster(aes(lon, lat, fill = round(prec_mean_mean, 2))) +
  #scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(cmorph_dt_mean$lon), max(cmorph_dt_mean$lon)), 
                  ylim = c(min(cmorph_dt_mean$lat), max(cmorph_dt_mean$lat))) + 
  scale_fill_binned(type = "viridis", option="B", direction = -1, 
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 0.6), show.limits = TRUE) + 
  labs(x = "", y = "", fill = "Mean \n(mm/hr)") + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/cmorph_mean_prec_2001_20.png",  width = 9.9, height = 4.6, 
       units = "in", dpi = 600)


#diurnal variation --------------------------------------------------

cmorph_dt[lat == "-59.875" & lon == "178.875"]

cmorph_dt_mean_time <- cmorph_dt[, .(value = mean(prec_mean, na.rm = TRUE)), by = .(time_lst)]
summary(cmorph_dt_mean)

ggplot(cmorph_dt_mean_time, aes(time_lst, value)) + 
  geom_point(size = 0.8) + 
  geom_line() + 
  labs(x ="Time", y = "", fill = "") + 
  #facet_grid(variable~season, scales = "free") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/kenya_example/results/05c_lineplot_mean_int_freq_seasonal_LST.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

# Peak hour ---------------------------------------------------------------

cmorph_dt[, `:=`(tmz_offset = NULL, time_utc = NULL)]
peak_prec_hour <- cmorph_dt[, .SD[which.max(prec_mean)], by = .(lat, lon, name)]
peak_prec_hour <- peak_prec_hour[, .(lat, lon, peak_hour = hour(time_lst), prec_mean, name)]

#levels(peak_prec_hour$name) <- c("persiann", "CMORPH", "PERSIANN", "ERA5", "GSMaP")

ggplot(peak_prec_hour) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_prec_hour$lon), max(peak_prec_hour$lon)), 
                  ylim = c(min(peak_prec_hour$lat), max(peak_prec_hour$lat))) + 
  #facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of\n precipitation") + 
  theme_small


ggsave("./projects/main/results/cmorph_peak_hour_mean_2001_20.png", width = 9.9, height = 4.1, 
       units = "in", dpi = 600)


#####################


# for imerg ---------------------------------------------------------------

dataset  <- brick("./projects/main/data/hourly_mean_imergf_glob_2001_20_fliptrans.nc")

imergf_dt <- as.data.frame(dataset, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("imergf"))

imergf_dt
summary(imergf_dt)

saveRDS(imergf_dt, "./projects/main/data/hourly_mean_imergf_glob_2001_20.rds")

# converting the time from utc to LST for a list of data.tables
imergf_dt$date <- substr(imergf_dt$date, 13, 14) %>% paste0(":00:00")
imergf_dt <- imergf_dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_mean, name)]
imergf_dt[, `:=`(tmz_offset = round((lon / 15)))]
imergf_dt$time_utc <- as.POSIXct(imergf_dt$time_utc)
imergf_dt[,  `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]

summary(imergf_dt)
saveRDS(imergf_dt, "./projects/main/data/hourly_mean_imergf_glob_LST_2001_20.rds")

## plot ----------------------------------------


imergf_dt[lat == "-59.875" & lon == "178.875"]

imergf_dt_mean <- imergf_dt[, .(prec_mean_mean = mean(prec_mean)), by = .(lat, lon)]
summary(imergf_dt_mean)

ggplot(imergf_dt_mean) + 
  geom_raster(aes(lon, lat, fill = round(prec_mean_mean, 2))) +
  #scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(imergf_dt_mean$lon), max(imergf_dt_mean$lon)), 
                  ylim = c(min(imergf_dt_mean$lat), max(imergf_dt_mean$lat))) + 
  scale_fill_binned(type = "viridis", option="B", direction = -1, 
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 0.6), show.limits = TRUE) + 
  labs(x = "", y = "", fill = "Mean \n(mm/hr)") + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/imergf_mean_prec_2001_20.png",  width = 9.9, height = 4.6, 
       units = "in", dpi = 600)


#diurnal variation --------------------------------------------------

imergf_dt[lat == "-59.875" & lon == "178.875"]

imergf_dt_mean_time <- imergf_dt[, .(value = mean(prec_mean, na.rm = TRUE)), by = .(time_lst)]
summary(imergf_dt_mean)

ggplot(imergf_dt_mean_time, aes(time_lst, value)) + 
  geom_point(size = 0.8) + 
  geom_line() + 
  labs(x ="Time", y = "", fill = "") + 
  #facet_grid(variable~season, scales = "free") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/kenya_example/results/05c_lineplot_mean_int_freq_seasonal_LST.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

# Peak hour ---------------------------------------------------------------

imergf_dt[, `:=`(tmz_offset = NULL, time_utc = NULL)]
peak_prec_hour <- imergf_dt[, .SD[which.max(prec_mean)], by = .(lat, lon, name)]
peak_prec_hour <- peak_prec_hour[, .(lat, lon, peak_hour = hour(time_lst), prec_mean, name)]

#levels(peak_prec_hour$name) <- c("imergf", "imergf", "imergf", "ERA5", "GSMaP")

ggplot(peak_prec_hour) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_prec_hour$lon), max(peak_prec_hour$lon)), 
                  ylim = c(min(peak_prec_hour$lat), max(peak_prec_hour$lat))) + 
  #facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of\n precipitation") + 
  theme_small


ggsave("./projects/main/results/imergf_peak_hour_mean_2001_20.png", width = 9.9, height = 4.1, 
       units = "in", dpi = 600)


#####################


# for persiann ---------------------------------------------------------------

persiann  <- brick("./projects/main/data/hourly_mean_persiann_glob_2001_20.nc")

pers_time <- getZ(persiann)
posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00") #We use (1*3600) in the formula because time values are stored as seconds in R. Since there are 3,600 seconds in one hour
names(persiann) <- posixct_time

persiann_dt <- as.data.frame(persiann, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>%
  `[`(, name := factor("persiann"))

persiann_dt
summary(persiann_dt)

saveRDS(persiann_dt, "./projects/main/data/hourly_mean_persiann_glob_2001_20.rds")

# converting the time from utc to LST for a list of data.tables
persiann_dt$date <- substr(persiann_dt$date, 13, 14) %>% paste0(":00:00")
persiann_dt <- persiann_dt[, .(lat = y, lon = x, time_utc = as_hms(date), prec_mean, name)]
persiann_dt[, `:=`(tmz_offset = round((lon / 15)))]
persiann_dt$time_utc <- as.POSIXct(persiann_dt$time_utc)
persiann_dt[,  `:=`(time_lst = time_utc + lubridate::hours(tmz_offset))]

summary(persiann_dt)
saveRDS(persiann_dt, "./projects/main/data/hourly_mean_persiann_glob_LST_2001_20.rds")

## plot ----------------------------------------


persiann_dt[lat == "-59.875" & lon == "178.875"]

persiann_dt_mean <- persiann_dt[, .(prec_mean_mean = mean(prec_mean)), by = .(lat, lon)]
summary(persiann_dt_mean)

ggplot(persiann_dt_mean) + 
  geom_raster(aes(lon, lat, fill = round(prec_mean_mean, 2))) +
  #scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(persiann_dt_mean$lon), max(persiann_dt_mean$lon)), 
                  ylim = c(min(persiann_dt_mean$lat), max(persiann_dt_mean$lat))) + 
  scale_fill_binned(type = "viridis", option="B", direction = -1, 
                    breaks = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.2, 0.4, 0.6), show.limits = TRUE) + 
  labs(x = "", y = "", fill = "Mean \n(mm/hr)") + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  theme_generic + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), 
        legend.direction = "vertical", legend.position = "right", legend.key.width = unit(0.5, "cm"),
        legend.key.height = unit(0.9, 'cm'))

ggsave("./projects/main/results/persiann_mean_prec_2001_20.png",  width = 9.9, height = 4.6, 
       units = "in", dpi = 600)


#diurnal variation --------------------------------------------------

persiann_dt[lat == "-59.875" & lon == "178.875"]

persiann_dt_mean_time <- persiann_dt[, .(value = mean(prec_mean, na.rm = TRUE)), by = .(time_lst)]
summary(persiann_dt_mean)

ggplot(persiann_dt_mean_time, aes(time_lst, value)) + 
  geom_point(size = 0.8) + 
  geom_line() + 
  labs(x ="Time", y = "", fill = "") + 
  #facet_grid(variable~season, scales = "free") + 
  theme_generic + 
  theme(legend.title = element_blank(), strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/kenya_example/results/05c_lineplot_mean_int_freq_seasonal_LST.png",
       width = 8.9, height = 5.6, units = "in", dpi = 600)

# Peak hour ---------------------------------------------------------------

persiann_dt[, `:=`(tmz_offset = NULL, time_utc = NULL)]
peak_prec_hour <- persiann_dt[, .SD[which.max(prec_mean)], by = .(lat, lon, name)]
peak_prec_hour <- peak_prec_hour[, .(lat, lon, peak_hour = hour(time_lst), prec_mean, name)]

#levels(peak_prec_hour$name) <- c("persiann", "persiann", "PERSIANN", "ERA5", "GSMaP")

ggplot(peak_prec_hour) + 
  geom_raster(aes(lon, lat, fill = factor(peak_hour))) +
  scale_fill_manual(values = rainbow(24)) +
  borders(colour = "black") +
  coord_cartesian(xlim = c(min(peak_prec_hour$lon), max(peak_prec_hour$lon)), 
                  ylim = c(min(peak_prec_hour$lat), max(peak_prec_hour$lat))) + 
  #facet_wrap(~name, ncol = 3) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) + 
  labs(x = "", y = "", fill = "Peak hour of\n precipitation") + 
  theme_small


ggsave("./projects/main/results/persiann_peak_hour_mean_2001_20.png", width = 9.9, height = 4.1, 
       units = "in", dpi = 600)




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


#########################################


data_list <-  readRDS("./projects/main/data/hourly_mean_thres_0.1_0.5_all_datasets_LST_glob_2001_20.rds")


zonmean_data_list <- lapply(data_list, function(df) df[, .('no_threshold' = mean(prec_mean, na.rm = TRUE), 
                                                           '0.1' = mean(prec_mean_0.1, na.rm = TRUE), 
                                                           '0.2' = mean(prec_mean_0.2, na.rm = TRUE),
                                                           '0.5' = mean(prec_mean_0.5, na.rm = TRUE)), by = .(lat, name)])

zonmean_data <- rbindlist(zonmean_data_list)
to_plot <- melt(zonmean_data,  c("lat", "name"), variable.name = "threshold")


levels(to_plot$threshold) <- c("no threshold", "0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

#zonal_data_comb[precip > 0.4]
summary(to_plot)

ggplot(to_plot, aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Mean (mm/hr)", col = " ") + 
  theme_generic + 
  facet_wrap(~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07b_zonal_mean_lineplot_thres_0.1_0.5_glob.png",
       width = 9.0, height = 5.2, units = "in", dpi = 600)

##flpped x-y axix----

ggplot(to_plot[threshold == "0.1 (mm/hr)"], aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Mean (mm/hr)", col = " ") + 
  theme_generic + 
  #facet_wrap(~threshold) + 
  coord_flip() + 
  scale_x_continuous(breaks = seq(-60, 60, by=10), expand = c(0, 0)) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), legend.position = "right", legend.direction = "vertical") + 
  theme(legend.position = c(0.8, 0.2))


ggsave("./projects/main/results/07b_zonal_mean_lineplot_flip.png",
       width = 4.5, height = 8.8, units = "in", dpi = 600)

### for land ocean and globe-------

zonmean_data_list_landocn <- lapply(data_list, function(df) df[, .('no_threshold' = mean(prec_mean, na.rm = TRUE), 
                                                                   '0.1' = mean(prec_mean_0.1, na.rm = TRUE), 
                                                                   '0.2' = mean(prec_mean_0.2, na.rm = TRUE),
                                                                   '0.5' = mean(prec_mean_0.5, na.rm = TRUE)), by = .(lat, name, location)])

zonmean_data_landocn <- rbindlist(zonmean_data_list_landocn)
to_plot_landocn <- melt(zonmean_data_landocn,  c("lat", "name", "location"), variable.name = "threshold")
to_plot_landocn <- to_plot_landocn[, .(lat, name, threshold, value, location)]
levels(to_plot_landocn$threshold) <- c("no threshold", "0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot_landocn$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
to_plot_glob <- to_plot[, `:=`(location = factor("global"))]

to_plot_landocnglob <- rbind(to_plot_glob, to_plot_landocn)
levels(to_plot_landocnglob$location) <- c("Global", "Land", "Ocean")


ggplot(to_plot_landocnglob, aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Mean (mm/hr)", col = " ") + 
  theme_generic + 
  facet_grid(location~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07b_zonal_mean_lineplot_thres_0.1_0.5_landocnglob.png",
       width = 9.0, height = 4.8, units = "in", dpi = 600)


### seasonal -------------------------------

data_list <- readRDS("./projects/main/data/hourly_mean_all_datasets_LST_glob_2001_20_seasonal.rds")

zonmean_data_list_seas <- lapply(data_list, function(df) df[, .('prec_mean' = mean(prec_mean, na.rm = TRUE)), by = .(lat, name, season)])

to_plot_seas <- rbindlist(zonmean_data_list_seas)

levels(to_plot_seas$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot_seas$season) <- c("JJA", "DJF")
  
summary(to_plot_seas)
to_plot_seas[prec_mean > 0.4]

ggplot(to_plot_seas[lat >= -59.125 & lat <= 59.125], aes(lat, prec_mean, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Mean (mm/hr)", col = " ") + 
  theme_generic + 
  facet_wrap(~season) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07b_zonal_mean_lineplot_glob_seasonal.png",
       width = 9.0, height = 5.2, units = "in", dpi = 600)


#### for global, land and ocean

zonmean_data_list_seas <- lapply(data_list, function(df) df[, .('prec_mean' = mean(prec_mean, na.rm = TRUE)), by = .(lat, name, season, location)])

to_plot_seas_landocn <- rbindlist(zonmean_data_list_seas)

levels(to_plot_seas_landocn$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot_seas_landocn$season) <- c("JJA", "DJF")

to_plot_seas_glob <- to_plot_seas[, `:=`(location = factor("global"))]

to_plot_seas_landocnglob <- rbind(to_plot_seas_glob, to_plot_seas_landocn)
levels(to_plot_seas_landocnglob$location) <- c("Global", "Land", "Ocean")


summary(to_plot_seas_landocnglob)
to_plot_seas_landocnglob[prec_mean > 0.4]
to_plot_seas_landocnglob[lat >= -59.125 & lat <= 59.125]

ggplot(to_plot_seas_landocnglob[lat >= -59.125 & lat <= 59.125], aes(lat, prec_mean, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Mean (mm/hr)", col = " ") + 
  theme_generic + 
  facet_grid(location~season) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07b_zonal_mean_lineplot_globlandocn_seasonal.png",
       width = 9.0, height = 5.2, units = "in", dpi = 600)


### seasonal for different thresholds-------------

data_list <- readRDS( "./projects/main/data/hourly_mean_thres_0.5_all_datasets_LST_glob_2001_20_seasonal.rds")

zonmean_data_list_seas <- lapply(data_list, function(df) df[, .('prec_mean' = mean(prec_mean, na.rm = TRUE)), by = .(lat, name, season, threshold, location)])

to_plot_seas <- rbindlist(zonmean_data_list_seas)

levels(to_plot_seas$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot_seas$season) <- c("JJA", "DJF")
levels(to_plot_seas$location) <- c("Land", "Ocean")
levels(to_plot_seas$threshold)

summary(to_plot_seas)
#to_plot_seas[prec_mean > 0.4]

ggplot(to_plot_seas[lat >= -59.125 & lat <= 59.125], aes(lat, prec_mean, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Mean (mm/hr)", col = " ") + 
  theme_generic + 
  facet_grid(location~season~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07b_zonal_mean_lineplot_thres_0.1_0.5_landocn_seasonal.png",
       width = 9.0, height = 5.2, units = "in", dpi = 600)

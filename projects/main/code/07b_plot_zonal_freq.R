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


data_list <-  readRDS("./projects/main/data/hourly_freq_thres_0.1_0.5_all_datasets_LST_glob_2001_20.rds")

data_list$imerg <- NULL
zonmean_data_list <- lapply(data_list, function(df) df[, .('0.1' = round(mean(prec_freq, na.rm = TRUE), 2), 
                                                        '0.2' = round(mean(prec_freq_0.2, na.rm = TRUE), 2),
                                                        '0.5' = round(mean(prec_freq_0.5, na.rm = TRUE), 2)), by = .(lat, name)])

zonmean_data <- rbindlist(zonmean_data_list)
to_plot <- melt(zonmean_data,  c("lat", "name"), variable.name = "threshold")


levels(to_plot$threshold) <- c("0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

#zonal_data_comb[precip > 0.4]

ggplot(to_plot, aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Frequency (%)", col = " ") + 
  theme_generic + 
  facet_wrap(~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07b_zonal_freq_lineplot_thres_0.1_0.5_glob_updated.png",
       width = 9.0, height = 5.2, units = "in", dpi = 600)

##flpped x-y axix----

ggplot(to_plot[threshold == "0.1 (mm/hr)"], aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Frequency (%)", col = " ") + 
  theme_generic + 
  #facet_wrap(~threshold) + 
  coord_flip() + 
  scale_x_continuous(breaks = seq(-60, 60, by=10), expand = c(0, 0)) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'), legend.position = "right", legend.direction = "vertical") + 
  theme(legend.position = c(0.8, 0.2))


ggsave("./projects/main/results/07b_zonal_freq_lineplot_flip_updated.png",
       width = 4.5, height = 8.8, units = "in", dpi = 600)

### for land ocean and globe-------

zonmean_data_list_landocn <- lapply(data_list, function(df) df[, .('0.1' = round(mean(prec_freq, na.rm = TRUE), 2), 
                                                           '0.2' = round(mean(prec_freq_0.2, na.rm = TRUE), 2),
                                                           '0.5' = round(mean(prec_freq_0.5, na.rm = TRUE), 2)), by = .(lat, name, location)])

zonmean_data_landocn <- rbindlist(zonmean_data_list_landocn)
to_plot_landocn <- melt(zonmean_data_landocn,  c("lat", "name", "location"), variable.name = "threshold")
to_plot_landocn <- to_plot_landocn[, .(lat, name, threshold, value, location)]
levels(to_plot_landocn$threshold) <- c("0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot_landocn$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
to_plot_glob <- to_plot[, `:=`(location = factor("global"))]

to_plot_landocnglob <- rbind(to_plot_glob, to_plot_landocn)
levels(to_plot_landocnglob$location) <- c("Global", "Land", "Ocean")


ggplot(to_plot_landocnglob, aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Frequency (%)", col = " ") + 
  theme_generic + 
  facet_grid(location~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07b_zonal_freq_lineplot_thres_0.1_0.5_landocnglob_updated.png",
       width = 9.0, height = 4.8, units = "in", dpi = 600)


#######################################################################


# for seasonal -------------------------------------------------

data_list <-  readRDS("./projects/main/data/hourly_freq_thres_0.1_0.5_all_datasets_LST_glob_2001_20_seasonal.rds")


zonmean_data_list <- lapply(data_list, function(df) df[, .(prec_freq = round(mean(prec_freq, na.rm = TRUE), 2)), by = .(lat, name, threshold, season)])

zonmean_data <- rbindlist(zonmean_data_list)
to_plot <- zonmean_data


#levels(to_plot$threshold) <- c("0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
to_plot$name <- factor(to_plot$name, levels = c("imerg", "gsmap", "cmorph", "persiann", "era5"))
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot$season) <- c("JJA", "DJF")

#zonal_data_comb[precip > 0.4]

ggplot(to_plot, aes(lat, prec_freq, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Frequency (%)", col = " ") + 
  theme_generic + 
  facet_grid(season~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07b_zonal_freq_lineplot_thres_0.1_0.5_glob_seasonal.png",
       width = 9.0, height = 5.2, units = "in", dpi = 600)


### for land ocean and globe------------------------------------------------------

zonmean_data_list_landocn <- lapply(data_list, function(df) df[, .(prec_freq = round(mean(prec_freq, na.rm = TRUE), 2)), by = .(lat, name, threshold, season, location)])

zonmean_data_landocn <- rbindlist(zonmean_data_list_landocn)
#to_plot_landocn <- melt(zonmean_data_landocn,  c("lat", "name", "location"), variable.name = "threshold")
to_plot_landocn <- zonmean_data_landocn[, .(lat, name, threshold, season, prec_freq, location)]
#levels(to_plot_landocn$threshold) <- c("0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
to_plot_landocn$name <- factor(to_plot_landocn$name, levels = c("imerg", "gsmap", "cmorph", "persiann", "era5"))
levels(to_plot_landocn$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot_landocn$season) <- c("JJA", "DJF")
to_plot_glob <- to_plot[, `:=`(location = factor("global"))]

to_plot_landocnglob <- rbind(to_plot_glob, to_plot_landocn)
levels(to_plot_landocnglob$location) <- c("Global", "Land", "Ocean")
levels(to_plot_landocnglob$season)

ggplot(to_plot_landocnglob, aes(lat, prec_freq, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Frequency (%)", col = " ") + 
  theme_small + 
  #facet_grid(threshold~season~location) + 
  facet_grid(location~season~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07b_zonal_freq_lineplot_thres_0.1_0.5_landocnglob_seasonal.png",
       width = 9.0, height = 4.8, units = "in", dpi = 600)


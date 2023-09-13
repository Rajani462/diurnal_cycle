
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(sf)
#library(reshape)
#library(terra)
library(ncdf4)
library(hms)
library(forcats)
library(parallel)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/graphics.R')

#################################################

zonal_data <- readRDS("./projects/main/data/zonal_lat_mean_2001_20.rds")
zonal_data_thres <- readRDS("./projects/main/data/zonal_lat_mean_0.1_0.5_2001_20.rds")


zonal_data[, `:=`(threshold = factor('no threshold'))]
zonal_data_comb <- rbind(zonal_data, zonal_data_thres)

levels(zonal_data_comb$threshold) <- c("no threshold", "0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(zonal_data_comb$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

zonal_data_comb[precip > 0.4]

ggplot(zonal_data_comb[precip < 0.4], aes(lat, precip, col = name), size = 0.5) + 
    geom_line() + 
    scale_color_manual(values = line_colors) + 
    labs(x = "Latitude", y = "Precipitation (mm/hour)", col = " ") + 
    theme_generic + 
  facet_wrap(~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07_zonal_mean_lineplot_thres_0.1_0.5.png",
      width = 9.0, height = 5.2, units = "in", dpi = 600)

ggplot(zonal_data_comb[precip < 0.4 & threshold == "no threshold"], aes(lat, precip, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Precipitation (mm/hour)", col = " ") + 
  theme_generic + 
  #facet_wrap(~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07_zonal_mean_lineplot.png",
       width = 9.0, height = 4.8, units = "in", dpi = 600)


# seasonal ----------------------------------------------------------------

zonal_data_seas <- readRDS("./projects/main/data/zonal_lat_mean_seasonal_2001_20.rds")
zonal_data_seas_0.5 <- readRDS("./projects/main/data/zonal_lat_mean_seasonal_0.1_0.5_2001_20.rds")

zonal_data_seas[, `:=`(threshold = factor('no threshold'))]

zonal_data_seas_comb <- rbind(zonal_data_seas, zonal_data_seas_0.5)

levels(zonal_data_seas_comb$threshold) <- c("no threshold", "0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
# Modify the 'name' column to be a factor
zonal_data_seas_comb[, name := factor(name, levels = c("imerg", "gsmap", "cmorph", "persiann", "era5"))]
zonal_data_seas_comb[, season := factor(season, levels = c("jja", "djf"))]

levels(zonal_data_seas_comb$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(zonal_data_seas_comb$season) <- c("JJA", "DJF")

zonal_data_seas_comb[precip > 0.4]

###### plot

ggplot(zonal_data_seas_comb[precip < 0.4], aes(lat, precip, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Precipitation (mm/hour)", col = " ") + 
  theme_small + 
  facet_grid(threshold~season) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07_zonal_mean_seasonal_lineplot.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)

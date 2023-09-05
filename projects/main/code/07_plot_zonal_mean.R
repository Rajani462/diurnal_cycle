
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

#################################################

zonal_data <- readRDS("./projects/main/data/zonal_lat_mean_2001_20.rds")

levels(zonal_data$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

ggplot(zonal_data, aes(lat, precip, col = name), size = 0.5) + 
    geom_line() + 
    scale_color_manual(values = line_colors) + 
    labs(x = "Latitude", y = "Precipitation (mm/hour)", col = " ") + 
    theme_generic

ggsave("./projects/main/results/07_zonal_mean_lineplot.png",
      width = 7.2, height = 5.5, units = "in", dpi = 600)

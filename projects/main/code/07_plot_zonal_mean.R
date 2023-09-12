
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
zonal_data_0.5 <- readRDS("./projects/main/data/zonal_lat_mean_0.5_2001_20.rds")

zonal_data_comb <- cbind(zonal_data, zonal_data_0.5)
zonal_data_comb <- zonal_data_comb[, .(lat, precip, precip_0.5, name)]

to_plot <- melt(zonal_data_comb, c("lat", "name"), variable.name = "threshold")
levels(to_plot$threshold) <- c("0.1 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

to_plot[value > 0.4]

ggplot(to_plot[value < 0.4], aes(lat, value, col = name), size = 0.5) + 
    geom_line() + 
    scale_color_manual(values = line_colors) + 
    labs(x = "Latitude", y = "Precipitation (mm/hour)", col = " ") + 
    theme_generic + 
  facet_wrap(~threshold) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))

ggsave("./projects/main/results/07_zonal_mean_lineplot.png",
      width = 7.2, height = 5.5, units = "in", dpi = 600)



# seasonal ----------------------------------------------------------------

zonal_data_seas <- readRDS("./projects/main/data/zonal_lat_mean_seasonal_2001_20.rds")
zonal_data_seas_0.5 <- readRDS("./projects/main/data/zonal_lat_mean_seasonal_0.5_2001_20.rds")

zonal_data_comb_seas <- cbind(zonal_data_seas, zonal_data_seas_0.5)
zonal_data_comb_seas <- zonal_data_comb_seas[, .(lat, precip, precip_0.5, name, season)]

to_plot_seas <- melt(zonal_data_comb_seas, c("lat", "name", "season"), variable.name = "threshold")
levels(to_plot_seas$threshold) <- c("0.1 (mm/hr)", "0.5 (mm/hr)")
#levels(to_plot_seas$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")


# Modify the 'name' column to be a factor
to_plot_seas[, name := factor(name, levels = c("imerg", "gsmap", "cmorph", "persiann", "era5"))]
to_plot_seas[, season := factor(season, levels = c("jja", "djf"))]

# Print the resulting combined data table
print(zonal_data_seas)

levels(to_plot_seas$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(to_plot_seas$season) <- c("JJA", "DJF")

###### plot

ggplot(to_plot_seas[value < 0.4], aes(lat, value, col = name)) + 
  #geom_point(size = 0.85) + 
  geom_line() + 
  facet_wrap(~season) + 
  scale_color_manual(values = line_colors) + 
  labs(x = "Latitude", y = "Precipitation (mm/hour)", col = " ") + 
  theme_generic + 
  facet_grid(threshold~season) + 
  theme(strip.background = element_rect(fill = "white"),
        strip.text = element_text(colour = 'Black'))
  

ggsave("./projects/main/results/07_zonal_mean_seasonal_lineplot.png",
       width = 7.2, height = 5.5, units = "in", dpi = 600)

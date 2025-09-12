
library(gridExtra)
library(ggpubr)
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(ncdf4)
library(sf)
library(sp)
library(hms)
library(forcats)
library(parallel)

source('./source/themes.R')
source('./source/palettes.R')
source('source/graphics.R')

#### read the data sets -------------------------------

### Mean------------------------------------------
data_list <-  readRDS("./projects/main/data/hourly_mean_thres_0.1_0.5_all_datasets_LST_glob_2001_20.rds")
data_list$imerg <- NULL
zonmean_data_list <- lapply(data_list, function(df) df[, .('no_threshold' = mean(prec_mean, na.rm = TRUE), 
                                                           '0.1' = mean(prec_mean_0.1, na.rm = TRUE), 
                                                           '0.2' = mean(prec_mean_0.2, na.rm = TRUE),
                                                           '0.5' = mean(prec_mean_0.5, na.rm = TRUE)), by = .(lat, name)])
zonmean_data <- rbindlist(zonmean_data_list)
to_plot <- melt(zonmean_data,  c("lat", "name"), variable.name = "threshold")
levels(to_plot$threshold) <- c("no threshold", "0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

line_colors2 <- c("#D35C37", "#BF9A77", "#ACBD78", "#4D648D", "#E69F00")
##plot
zon_mean <- ggplot(to_plot[threshold == "no threshold"], aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors2) + 
  labs(x = "Latitude", y = "Amount (mm/hr)", col = " ") + 
  theme_small + 
  coord_flip() + 
  scale_x_continuous(breaks = seq(-60, 60, by = 10), expand = c(0, 0)) + 
  theme(strip.background = element_rect(fill = "white"), 
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12), 
        legend.text = element_text(size = 10),
        strip.text = element_text(colour = 'Black'), legend.position = "right", legend.direction = "vertical") + 
  theme(legend.position = c(0.75, 0.2), legend.margin = margin(0, 0, 0, 0),
        legend.spacing.x = unit(0, "mm"),
        legend.spacing.y = unit(0, "mm"))


### Frequency------------------------------------------
data_list <-  readRDS("./projects/main/data/hourly_freq_thres_0.1_0.5_all_datasets_LST_glob_2001_20.rds")
data_list$imerg <- NULL
zonmean_data_list <- lapply(data_list, function(df) df[, .('0.1' = round(mean(prec_freq, na.rm = TRUE), 2), 
                                                           '0.2' = round(mean(prec_freq_0.2, na.rm = TRUE), 2),
                                                           '0.5' = round(mean(prec_freq_0.5, na.rm = TRUE), 2)), by = .(lat, name)])
zonmean_data <- rbindlist(zonmean_data_list)
to_plot <- melt(zonmean_data,  c("lat", "name"), variable.name = "threshold")
levels(to_plot$threshold) <- c("0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

## plot
zon_freq <- ggplot(to_plot[threshold == "0.1 (mm/hr)"], aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors2) + 
  labs(x = "", y = "Frequency (%)", col = " ") + 
  theme_small + 
  #facet_wrap(~threshold) + 
  coord_flip() + 
  scale_x_reverse() + 
  scale_x_continuous(breaks = seq(-60, 60, by = 10), expand = c(0, 0)) +  
  theme(strip.background = element_rect(fill = "white"), 
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12), 
        legend.text = element_text(size = 10),
        strip.text = element_text(colour = 'Black'), legend.position = "none")


### Intensity------------------------------------------
data_list <-  readRDS("./projects/main/data/hourly_int_thres_0.1_0.5_all_datasets_LST_glob_2001_20.rds")
data_list$imerg <- NULL
zonmean_data_list <- lapply(data_list, function(df) df[, .('0.1' = mean(prec_int, na.rm = TRUE), 
                                                           '0.2' = mean(prec_int_0.2, na.rm = TRUE),
                                                           '0.5' = mean(prec_int_0.5, na.rm = TRUE)), by = .(lat, name)])
zonmean_data <- rbindlist(zonmean_data_list)
to_plot <- melt(zonmean_data,  c("lat", "name"), variable.name = "threshold")
levels(to_plot$threshold) <- c("0.1 (mm/hr)", "0.2 (mm/hr)", "0.5 (mm/hr)")
levels(to_plot$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")

## plot
zon_int <- ggplot(to_plot[threshold == "0.1 (mm/hr)" & lat >= -58.875 & lat <= 58.875], aes(lat, value, col = name), size = 0.5) + 
  geom_line() + 
  scale_color_manual(values = line_colors2) + 
  labs(x = "", y = "Intensity (mm/hr)", col = " ") + 
  theme_small + 
  #facet_wrap(~threshold) + 
  coord_flip() + 
  scale_x_continuous(breaks = seq(-60, 60, by = 10), expand = c(0, 0)) + 
  theme(strip.background = element_rect(fill = "white"), 
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.y = element_text(size = 12), 
        legend.text = element_text(size = 10), 
        strip.text = element_text(colour = 'Black'), legend.position = "none")



p <- ggarrange(zon_mean, zon_freq, zon_int, nrow = 1,  align = "h",
               labels = c("a)", "b)", "c)"), hjust = -3.4, font.label=list(family = font, 
                                                                           face = "plain", 
                                                                           color = "#222222", 
                                                                           size=12))
p
ggsave("~/rajani/diurnal_cycle/projects/main/reproduce/results/04_zonal_mean_freq_int_lineplot.png",
       width = 9.0, height = 5.2, units = "in", dpi = 600)

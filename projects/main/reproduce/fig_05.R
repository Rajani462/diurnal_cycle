
library(raster)
library(data.table)
library(ggplot2)
library(viridis)
library(dplyr)
library(ggpubr)
library(ncdf4)
library(sf)
library(hms)
library(forcats)
library(parallel)

#source('./source/libs.R')
source('./source/themes.R')
source('./source/palettes.R')
source('./source/graphics.R')

### mean ---------------------------------------------------------------
## read the data sets -------------------------------
data_list <- readRDS("~/rajani/diurnal_cycle/projects/main/reproduce/data/hourly_mean_all_datasets_LST_glob_2001_20.rds")

data_dt <- rbindlist(data_list)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$location) <- c("Land", "Ocean")

### 24hr diurnal cycle line plot ---------------------------------------------------------------------
mean_24h_glob <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name)]
mean_24h_landocn <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, location)]
mean_24h_landocn <- data_dt[, .(mean_value = mean(prec_mean, na.rm = TRUE)), by = .(hour(time_lst), name, location)]
mean_24h_glob2 <- mean_24h_glob[, .(hour, name, location = factor("Global"), mean_value)]
land_ocn_glob_mean <- rbind(mean_24h_glob2, mean_24h_landocn)

mean <- ggplot(land_ocn_glob_mean, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~location) + 
  labs(x ="", y = "Amount (mm/hr)") + 
  theme_small + 
  theme(axis.text.x=element_blank(),legend.title = element_blank(), legend.position = "bottom", strip.background = element_blank(),
        strip.text = element_text(colour = 'Black'))


### frequency ---------------------------------------------------------------
## read the data sets -------------------------------
data_list <- readRDS("~/rajani/diurnal_cycle/projects/main/reproduce/data/hourly_freq_all_datasets_LST_glob_2001_20.rds")

data_dt <- rbindlist(data_list)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$location) <- c("Land", "Ocean")

### 24hr diurnal cycle line plot ---------------------------------------------------------------------
mean_24h_glob <- data_dt[, .(mean_value = mean(prec_freq, na.rm = TRUE)), by = .(hour(time_lst), name)]
mean_24h_landocn <- data_dt[, .(mean_value = mean(prec_freq, na.rm = TRUE)), by = .(hour(time_lst), name, location)]
mean_24h_glob2 <- mean_24h_glob[, .(hour, name, location = factor("Global"), mean_value)]
land_ocn_glob_freq <- rbind(mean_24h_glob2, mean_24h_landocn)

freq <- ggplot(land_ocn_glob_freq, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~location) + 
  labs(x ="", y = "Frequency (%)") + 
  theme_small + 
  theme(axis.text.x=element_blank(), legend.title = element_blank(), legend.position = "bottom", strip.background = element_blank(),
        strip.text = element_blank())


### intensity ---------------------------------------------------------------
## read the data sets -------------------------------
data_list <- readRDS("~/rajani/diurnal_cycle/projects/main/reproduce/data/hourly_int_all_datasets_LST_glob_2001_20.rds")

data_dt <- rbindlist(data_list)
data_dt[, `:=`(time_utc = NULL, tmz_offset = NULL)]
levels(data_dt$name) <- c("IMERG", "GSMaP", "CMORPH", "PERSIANN", "ERA5")
levels(data_dt$location) <- c("Land", "Ocean")

### 24hr diurnal cycle line plot ---------------------------------------------------------------------
mean_24h_glob <- data_dt[, .(mean_value = mean(prec_int, na.rm = TRUE)), by = .(hour(time_lst), name)]
mean_24h_landocn <- data_dt[, .(mean_value = mean(prec_int, na.rm = TRUE)), by = .(hour(time_lst), name, location)]
mean_24h_glob2 <- mean_24h_glob[, .(hour, name, location = factor("Global"), mean_value)]
land_ocn_glob <- rbind(mean_24h_glob2, mean_24h_landocn)

int <- ggplot(land_ocn_glob, aes(hour, mean_value, col = name, group = name)) + 
  geom_point(size = 0.85) + 
  geom_line() + 
  scale_color_manual(values = line_colors) + 
  facet_wrap(~location) + 
  labs(x ="Hour (LST)", y = "Intensity (mm/hr)") + 
  theme_small + 
  theme(legend.title = element_blank(), legend.position = "bottom", strip.background = element_blank(),
        strip.text = element_blank())


land_ocn_glob_int <- land_ocn_glob[, `:=`(varaible = "Intensity (mm/hr)")]

# merge all into one ------------------------------------------------------
land_ocn_glob_mean[, `:=`(varaible = "Amount (mm/hr)")]
land_ocn_glob_freq[, `:=`(varaible = "Frequency (%)")]
land_ocn_glob_int <- land_ocn_glob[, `:=`(varaible = "Intensity (mm/hr)")]

# to_plot <- rbind(land_ocn_glob_mean,land_ocn_glob_freq, land_ocn_glob_int)

# Adjust margins and spacing between plots
plot1 <- mean + theme(plot.margin = margin(0.1, 0.1, -0.25, 0.1, unit = "cm")) #top, right, bottm, left
plot2 <- freq + theme(plot.margin = margin(-0.15, 0.1, -0.15, 0.1, unit = "cm"))
plot3 <- int + theme(plot.margin = margin(-0.25, 0.1, 0.1, 0.1, unit = "cm"))

# Arrange plots
ggarrange(plot1, plot2, plot3, nrow = 3, common.legend = TRUE, legend = "bottom", 
          align = "v", heights = c(0.9, 0.74, 0.93),
          labels = c("a)", "b)", "c)"), hjust = -0.5, vjust = 0.85, 
          font.label = list(family = font, face = "plain", color = "#222222", size = 10))

# Save the arranged plots with increased right margin
ggsave("~/rajani/diurnal_cycle/projects/main/reproduce/results/05_fig_24hlineplot_mean_freq_int_landocnglob.png",
       width = 7.6, height = 5.3, units = "in", dpi = 600)


###############################################
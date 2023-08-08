library(raster)
library(data.table)
library(dplyr)
library(ggplot2)
library(MASS)
library(scales)

gsmap <- brick("./projects/kenya_example/data/gsmap_hour_kenya_2015_grid_025.nc")
plot(da)

gsmap_dt <- as.data.frame(gsmap, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "rf") %>%
  `[`(, name := factor("gsmap"))

gsmap_dt <- gsmap_dt[, .(lat = x, lon = y, date = date, rf = round(rf, 2), name)]
summary(gsmap_dt)

gsmap_freq <- gsmap_dt[, .(freq = .N), by = .(rf, lat, lon)]

gsmap_freq_ken <- gsmap_freq[, .(rf = mean(rf, na.rm = TRUE), 
                                 freq = mean(freq, na.rm = TRUE)), by = .(lat, lon)]

ggplot(gsmap_freq_ken, aes(rf, freq)) + 
  geom_line() + 
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) + 
  labs(x = "Precipitation intensities (mm/hr)", y = "freq") + 
  #facet_wrap(~ocn) + 
  #scale_fill_discrete(name = "", labels = c("Buoys", "IMERG")) + 
  #scale_fill_manual(values = c("#4682B4", "#FF8247"), name = "", labels = c("Buoys", "IMERG")) + 
  #theme(legend.position = c(0.07, 0.90)) + 
  scale_x_log10() + 
  theme_bw() 
  # theme(axis.text.x = element_text(angle = 30, hjust = 0.8, vjust = 0.9)) + 
  # theme(strip.background = element_rect(fill = "white"),
  #       strip.text = element_text(colour = 'Black'))






library(ggplot2)
library(viridis)

ggplot(da_dt, aes(y, x, fill = prec_mean)) + 
  geom_tile() + 
  scale_fill_binned(type = "viridis", direction = -1, 
                    breaks = c(0.01, 0.05, 0.1, 0.3, 0.7, 0.9, 1.0, 2, 10), show.limits = TRUE)


gsm <- raster("~/shared/data_downloads/GSMAP/ftp_downloads/2015/2015_nc/gsmap_hour_2015.nc")
gsm_regrd <- brick("~/shared/data_downloads/GSMAP/ftp_downloads/2015/2015_nc/gsmap_hour_2015_grid_025.nc")

gsm_ken <- raster("./projects/kenya_example/data/gsmap_hour_kenya_2015_grid_025.nc")

########################

trl <- brick("./projects/kenya_example/data/trial_era5_hour_kenya_2001_01.nc")


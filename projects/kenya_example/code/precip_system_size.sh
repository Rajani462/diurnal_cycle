#################################



# Create a binary mask where precipitation is greater than the threshold
cdo gec,1 ~/rajani/diurnal_cycle/projects/kenya_example/data/cmorph_hour_JF_kenya_2001_20.nc ~/rajani/diurnal_cycle/projects/kenya_example/data/output/precip_system/tmp_mask.nc

# Multiply the original data by the mask, setting non-matching values to missing
cdo mulc,1 -gec,1 ~/rajani/diurnal_cycle/projects/kenya_example/data/cmorph_hour_JF_kenya_2001_20.nc ~/rajani/diurnal_cycle/projects/kenya_example/data/output/precip_system/tmp_regions.nc

# Replace missing values with zeros
cdo mulc,1 tmp_regions.nc output_regions.nc







# Label connected components and output as NetCDF
cdo mul -gec,0.1 ~/rajani/diurnal_cycle/projects/kenya_example/data/cmorph_hour_JF_kenya_2001_20.nc tmp_mask.nc tmp_regions_label.nc


# Identify connected components manually and assign unique labels
cdo mul ~/rajani/diurnal_cycle/projects/kenya_example/data/cmorph_hour_JF_kenya_2001_20.nc -gec,1 -add -mul -gec,1 -shift -1 tmp_mask.nc -mul -gec,1 -shift 1 tmp_mask.nc tmp_regions3.nc



# try with the cmorph datasets
library(terra)
library(data.table)

nc_file <- rast("C:/Users/rkpra/Downloads/cmorph_hour_JF_kenya_2001_20.nc")
names(nc_file) <- time(nc_file)
threshold <- 0.1
precipitation_mask <- nc_file > threshold

# Assuming precipitation_mask is your binary mask
# clumps <- raster::clump(precipitation_mask)
# freq(clumps)
prec_system <- patches(precipitation_mask, zeroAsNA=TRUE)
# prec_system[[1]]
# freq(prec_system[[1]])
# freq(prec_system[[1:3]])
# # Convert clumps to polygons
# #polygons <- raster::rasterToPolygons(clumps, dissolve=TRUE)
# 
# # Plot the original raster
# plot(prec_system, 1:2)
# plot(precipitation_mask, 1)
# plot(nc_file, 1)
# 
# prec_syst2 <- prec_system[1]
# # Plot the identified regions
# prec_dt <- as.data.table(as.data.frame(prec_system[[1:2]], xy = TRUE, na.rm = FALSE))
prec_dt2 <- as.data.table(as.data.frame(prec_system, xy = TRUE, na.rm = FALSE))

#head(prec_dt2)

prec_dt3 <- melt(prec_dt2, id.vars = c('x', 'y'), variable.name = "time")

#summary(prec_dt3)

#prec_feat <- prec_dt3[, .(mean_size = mean(value, na.rm = TRUE)), by = .(x, y)]

#prec_dt4 <- prec_dt3[, .N, by = .(x, y, time)]

#prec_dt3[complete.cases(prec_dt3), ]

prec_dt3[, count := .N, by = .(value, time)]

#prec_syst_mean <- prec_dt3[, .(mean_size = median(count, na.rm = TRUE)), by = .(x, y)]
prec_syst_mean <- prec_dt3[complete.cases(prec_dt3), 
                           .(mean_size = median(count, na.rm = TRUE)), 
                           by = .(x, y)]
summary(prec_syst_mean)


#plot
library(ggplot2)

ggplot(prec_syst_mean, aes(x, y, fill = mean_size)) +
  geom_raster() +
  scale_fill_gradient(low = "blue", high = "red")  # You can adjust colors as needed


#freq

library(ggplot2)

# Assuming 'count' is the column representing the frequency of occurrences
prec_syst_count <- prec_dt3[complete.cases(prec_dt3), .(count = .N), by = .(x, y)]

ggplot(prec_syst_count, aes(x, y, fill = count)) +
  geom_raster(stat = "identity") +
  scale_fill_gradient(low = "blue", high = "red")  # Customize colors as needed

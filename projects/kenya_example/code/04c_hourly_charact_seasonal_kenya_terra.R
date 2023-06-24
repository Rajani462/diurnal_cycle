
# diurnal cycle of frequency and intensity at seasonal scale

library(terra)
library(data.table)
library(dplyr)


cmorph <-  rast("./projects/kenya_example/data/cmorph_hour_kenya_2001_20.nc") 
#imerg <-  rast("./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025_fliptrans.nc") 

# for cmorph 

dte <- format(as.POSIXct(time(cmorph), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")

m <- as.numeric(dte)
jf <- (m >= 1 & m <= 2)
mam <- (m >= 3 & m <= 5)
jjas <- (m >= 6 & m <= 9)
ond <- (m >= 10 & m <= 12)

cmorph_jf <- cmorph[[jf]]
cmorph_mam <- cmorph[[mam]]
cmorph_jjas <- cmorph[[jjas]]
cmorph_ond <- cmorph[[ond]]


#### for JF
#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(cmorph_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

cmorph_hourly_mean_jf <- tapp(cmorph_jf, indices, fun = mean)
cmorph_jf_mean_dt <- as.data.frame(cmorph_hourly_mean_jf, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("jf"))

cmorph_jf_mean_dt[!is.finite(prec_mean), prec_mean := NA]

### Intensity
total_precip_jf <- tapp(cmorph_jf, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jf <- tapp(cmorph_jf, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

cmorph_jf_intensity <- total_precip_jf / precip_hours_jf

#plot(cmorph_jf_intensity[[1]])

cmorph_jf_int_dt <- as.data.frame(cmorph_jf_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("jf"))


#cmorph_jf_intensity_dt[prec_int == Inf]
cmorph_jf_int_dt[!is.finite(prec_int), prec_int := NA]
#summary(cmorph_jf_int_dt)

### Frequency
# Calculate the total available hours
total_available_hours_jf <- length(indices)
cmorph_jf_frequency <- (precip_hours_jf / total_available_hours_jf) * 100

cmorph_jf_freq_dt <- as.data.frame(cmorph_jf_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("jf"))

#summary(cmorph_jf_freq_dt)
cmorph_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]


### for MAM

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(cmorph_mam), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation 
cmorph_hourly_mean_mam <- tapp(cmorph_mam, indices, fun = mean)
cmorph_mam_mean_dt <- as.data.frame(cmorph_hourly_mean_mam, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("mam"))

cmorph_mam_mean_dt[!is.finite(prec_mean), prec_mean := NA]

### Intensity
total_precip_mam <- tapp(cmorph_mam, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_mam <- tapp(cmorph_mam, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

cmorph_mam_intensity <- total_precip_mam / precip_hours_mam

#plot(cmorph_mam_intensity[[1]])

cmorph_mam_int_dt <- as.data.frame(cmorph_mam_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("mam"))

#summary(cmorph_mam_int_dt)
#cmorph_mam_intensity_dt[prec_int == Inf]
cmorph_mam_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_mam <- length(indices)
cmorph_mam_frequency <- (precip_hours_mam / total_available_hours_mam) * 100

cmorph_mam_freq_dt <- as.data.frame(cmorph_mam_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("cmorph")) %>%
  `[`(, season := factor("mam"))

#summary(cmorph_mam_freq_dt)
cmorph_mam_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(cmorph_mam_freq_dt)

### for JJAS

indices <- format(as.POSIXct(time(cmorph_jjas), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation
cmorph_hourly_mean_jjas <- tapp(cmorph_jjas, indices, fun = mean)
cmorph_jjas_mean_dt <- as.data.frame(cmorph_hourly_mean_jjas, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("jjas"))

cmorph_jjas_mean_dt[!is.finite(prec_mean), prec_mean := NA]

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(cmorph_jjas), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_jjas <- tapp(cmorph_jjas, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jjas <- tapp(cmorph_jjas, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

cmorph_jjas_intensity <- total_precip_jjas / precip_hours_jjas

#plot(cmorph_jjas_intensity[[1]])

cmorph_jjas_int_dt <- as.data.frame(cmorph_jjas_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("jjas"))

#summary(cmorph_jjas_int_dt)
#cmorph_jjas_intensity_dt[prec_int == Inf]
cmorph_jjas_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_jjas <- length(indices)
cmorph_jjas_frequency <- (precip_hours_jjas / total_available_hours_jjas) * 100

cmorph_jjas_freq_dt <- as.data.frame(cmorph_jjas_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("cmorph")) %>%
  `[`(, season := factor("jjas"))

#summary(cmorph_jjas_freq_dt)
cmorph_jjas_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(cmorph_jjas_freq_dt)


### for OND

indices <- format(as.POSIXct(time(cmorph_ond), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)


### 24 hourly mean diurnal precipitation
cmorph_hourly_mean_ond <- tapp(cmorph_ond, indices, fun = mean)
cmorph_ond_mean_dt <- as.data.frame(cmorph_hourly_mean_ond, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("ond"))

cmorph_ond_mean_dt[!is.finite(prec_mean), prec_mean := NA]

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(cmorph_ond), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_ond <- tapp(cmorph_ond, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_ond <- tapp(cmorph_ond, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

cmorph_ond_intensity <- total_precip_ond / precip_hours_ond

#plot(cmorph_ond_intensity[[1]])

cmorph_ond_int_dt <- as.data.frame(cmorph_ond_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("ond"))

#summary(cmorph_ond_int_dt)
#cmorph_ond_intensity_dt[prec_int == Inf]
cmorph_ond_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_ond <- length(indices)
cmorph_ond_frequency <- (precip_hours_ond / total_available_hours_ond) * 100

cmorph_ond_freq_dt <- as.data.frame(cmorph_ond_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("cmorph")) %>%
  `[`(, season := factor("ond"))

#summary(cmorph_ond_freq_dt)
cmorph_ond_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(cmorph_ond_freq_dt)


### combine everything to a list

cmorph_allseason_mean_dt <- list(cmorph_jf_mean_dt, cmorph_mam_mean_dt, cmorph_jjas_mean_dt, 
                                 cmorph_ond_mean_dt)

cmorph_allseason_freq_dt <- list(cmorph_jf_freq_dt, cmorph_mam_freq_dt, cmorph_jjas_freq_dt, 
                                 cmorph_ond_freq_dt)

cmorph_allseason_int_dt <- list(cmorph_jf_int_dt, cmorph_mam_int_dt, cmorph_jjas_int_dt, 
                                 cmorph_ond_int_dt)


### save as .RDS

saveRDS(cmorph_allseason_mean_dt, "./projects/kenya_example/data/output/cmorph_allseason_mean_dt.RDS")
saveRDS(cmorph_allseason_int_dt, "./projects/kenya_example/data/output/cmorph_allseason_int_dt.RDS")
saveRDS(cmorph_allseason_freq_dt, "./projects/kenya_example/data/output/cmorph_allseason_freq_dt.RDS")

##################################################################

# Try with IMERG----------------------------------------------------------------- 
library(raster)
library(data.table)
library(dplyr)

imerg <-  brick("./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025_fliptrans.nc") 

dte <- format(as.POSIXct(names(imerg), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")

m <- as.numeric(dte)


imerg_jf <- subset(imerg, which(m %in% c(1, 2)))
imerg_mam <- subset(imerg, which(m %in% c(3, 4, 5)))
imerg_jjas <- subset(imerg, which(m %in% c(6, 7, 8, 9)))
imerg_ond <- subset(imerg, which(m %in% c(10, 11, 12)))


### for JF
#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(imerg_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

imerg_hourly_mean_jf <- stackApply(imerg_jf, indices, fun = mean)
imerg_jf_mean_dt <- as.data.frame(imerg_hourly_mean_jf, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("jf"))

imerg_jf_mean_dt[!is.finite(prec_mean), prec_mean := NA]
imerg_jf_mean_dt$date <- gsub("index_(\\d+)", "X\\1", imerg_jf_mean_dt$date)

### Intensity
total_precip_jf <- stackApply(imerg_jf, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jf <- stackApply(imerg_jf, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_jf_intensity <- total_precip_jf / precip_hours_jf

#plot(imerg_jf_intensity[[1]])

imerg_jf_int_dt <- as.data.frame(imerg_jf_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("jf"))

imerg_jf_int_dt$date <- gsub("index_(\\d+)", "X\\1", imerg_jf_int_dt$date)


#imerg_jf_intensity_dt[prec_int == Inf]
imerg_jf_int_dt[!is.finite(prec_int), prec_int := NA]
#summary(imerg_jf_int_dt)

### Frequency
# Calculate the total available hours
total_available_hours_jf <- length(indices)
imerg_jf_frequency <- (precip_hours_jf / total_available_hours_jf) * 100

imerg_jf_freq_dt <- as.data.frame(imerg_jf_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("jf"))

imerg_jf_freq_dt$date <- gsub("index_(\\d+)", "x\\1", imerg_jf_freq_dt$date)

#summary(imerg_jf_freq_dt)
imerg_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]


### for MAM

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(imerg_mam), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

imerg_hourly_mean_mam <- stackApply(imerg_mam, indices, fun = mean)
imerg_mam_mean_dt <- as.data.frame(imerg_hourly_mean_mam, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("mam"))

imerg_mam_mean_dt[!is.finite(prec_mean), prec_mean := NA]
imerg_mam_mean_dt$date <- gsub("index_(\\d+)", "X\\1", imerg_mam_mean_dt$date)

### Intensity
total_precip_mam <- stackApply(imerg_mam, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_mam <- stackApply(imerg_mam, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_mam_intensity <- total_precip_mam / precip_hours_mam

#plot(imerg_mam_intensity[[1]])

imerg_mam_int_dt <- as.data.frame(imerg_mam_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("mam"))

imerg_mam_int_dt$date <- gsub("index_(\\d+)", "x\\1", imerg_mam_int_dt$date)

#summary(imerg_mam_int_dt)
#imerg_mam_intensity_dt[prec_int == Inf]
imerg_mam_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_mam <- length(indices)
imerg_mam_frequency <- (precip_hours_mam / total_available_hours_mam) * 100

imerg_mam_freq_dt <- as.data.frame(imerg_mam_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg")) %>%
  `[`(, season := factor("mam"))

imerg_mam_freq_dt$date <- gsub("index_(\\d+)", "x\\1", imerg_mam_freq_dt$date)

#summary(imerg_mam_freq_dt)
imerg_mam_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(imerg_mam_freq_dt)

### for JJAS

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(imerg_jjas), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

imerg_hourly_mean_jjas <- stackApply(imerg_jjas, indices, fun = mean)
imerg_jjas_mean_dt <- as.data.frame(imerg_hourly_mean_jjas, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("jjas"))

imerg_jjas_mean_dt[!is.finite(prec_mean), prec_mean := NA]
imerg_jjas_mean_dt$date <- gsub("index_(\\d+)", "X\\1", imerg_jjas_mean_dt$date)

### Intensity
total_precip_jjas <- stackApply(imerg_jjas, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jjas <- stackApply(imerg_jjas, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_jjas_intensity <- total_precip_jjas / precip_hours_jjas

#plot(imerg_jjas_intensity[[1]])

imerg_jjas_int_dt <- as.data.frame(imerg_jjas_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("jjas"))

imerg_jjas_int_dt$date <- gsub("index_(\\d+)", "x\\1", imerg_jjas_int_dt$date)
#summary(imerg_jjas_int_dt)
#imerg_jjas_intensity_dt[prec_int == Inf]
imerg_jjas_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_jjas <- length(indices)
imerg_jjas_frequency <- (precip_hours_jjas / total_available_hours_jjas) * 100

imerg_jjas_freq_dt <- as.data.frame(imerg_jjas_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg")) %>%
  `[`(, season := factor("jjas"))

imerg_jjas_freq_dt$date <- gsub("index_(\\d+)", "x\\1", imerg_jjas_freq_dt$date)
#summary(imerg_jjas_freq_dt)
imerg_jjas_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(imerg_jjas_freq_dt)


### for OND

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(imerg_ond), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

imerg_hourly_mean_ond <- stackApply(imerg_ond, indices, fun = mean)
imerg_ond_mean_dt <- as.data.frame(imerg_hourly_mean_ond, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("ond"))

imerg_ond_mean_dt[!is.finite(prec_mean), prec_mean := NA]
imerg_ond_mean_dt$date <- gsub("index_(\\d+)", "X\\1", imerg_ond_mean_dt$date)

### Intensity
total_precip_ond <- stackApply(imerg_ond, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_ond <- stackApply(imerg_ond, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_ond_intensity <- total_precip_ond / precip_hours_ond

#plot(imerg_ond_intensity[[1]])

imerg_ond_int_dt <- as.data.frame(imerg_ond_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("ond"))

imerg_ond_int_dt$date <- gsub("index_(\\d+)", "x\\1", imerg_ond_int_dt$date)
#summary(imerg_ond_int_dt)
#imerg_ond_intensity_dt[prec_int == Inf]
imerg_ond_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_ond <- length(indices)
imerg_ond_frequency <- (precip_hours_ond / total_available_hours_ond) * 100

imerg_ond_freq_dt <- as.data.frame(imerg_ond_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg")) %>%
  `[`(, season := factor("ond"))

imerg_ond_freq_dt$date <- gsub("index_(\\d+)", "x\\1", imerg_ond_freq_dt$date)
#summary(imerg_ond_freq_dt)
imerg_ond_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(imerg_ond_freq_dt)


### comine everything to a lsit

imerg_allseason_mean_dt <- list(imerg_jf_mean_dt, imerg_mam_mean_dt, imerg_jjas_mean_dt, 
                                imerg_ond_mean_dt)

imerg_allseason_freq_dt <- list(imerg_jf_freq_dt, imerg_mam_freq_dt, imerg_jjas_freq_dt, 
                                 imerg_ond_freq_dt)

imerg_allseason_int_dt <- list(imerg_jf_int_dt, imerg_mam_int_dt, imerg_jjas_int_dt, 
                                imerg_ond_int_dt)


### save as .RDS

saveRDS(imerg_allseason_mean_dt, "./projects/kenya_example/data/output/imerg_allseason_mean_dt.RDS")
saveRDS(imerg_allseason_int_dt, "./projects/kenya_example/data/output/imerg_allseason_int_dt.RDS")
saveRDS(imerg_allseason_freq_dt, "./projects/kenya_example/data/output/imerg_allseason_freq_dt.RDS")


#############################################################################

# try with GSMAP ------------------------------------------

library(raster)
library(data.table)
library(dplyr)

gsmap <-  brick("./projects/kenya_example/data/gsmap_hour_kenya_2015_20_fliptrans.nc") 

dte <- format(as.POSIXct(names(gsmap), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")

m <- as.numeric(dte)


gsmap_jf <- subset(gsmap, which(m %in% c(1, 2)))
gsmap_mam <- subset(gsmap, which(m %in% c(3, 4, 5)))
gsmap_jjas <- subset(gsmap, which(m %in% c(6, 7, 8, 9)))
gsmap_ond <- subset(gsmap, which(m %in% c(10, 11, 12)))


### for JF
#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(gsmap_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

gsmap_hourly_mean_jf <- stackApply(gsmap_jf, indices, fun = mean)
gsmap_jf_mean_dt <- as.data.frame(gsmap_hourly_mean_jf, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("gsmap"))%>%
  `[`(, season := factor("jf"))

gsmap_jf_mean_dt[!is.finite(prec_mean), prec_mean := NA]
gsmap_jf_mean_dt$date <- gsub("index_(\\d+)", "X\\1", gsmap_jf_mean_dt$date)

### Intensity
total_precip_jf <- stackApply(gsmap_jf, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jf <- stackApply(gsmap_jf, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

gsmap_jf_intensity <- total_precip_jf / precip_hours_jf

#plot(gsmap_jf_intensity[[1]])

gsmap_jf_int_dt <- as.data.frame(gsmap_jf_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("gsmap"))%>%
  `[`(, season := factor("jf"))

gsmap_jf_int_dt$date <- gsub("index_(\\d+)", "X\\1", gsmap_jf_int_dt$date)


#gsmap_jf_intensity_dt[prec_int == Inf]
gsmap_jf_int_dt[!is.finite(prec_int), prec_int := NA]
#summary(gsmap_jf_int_dt)

### Frequency
# Calculate the total available hours
total_available_hours_jf <- length(indices)
gsmap_jf_frequency <- (precip_hours_jf / total_available_hours_jf) * 100

gsmap_jf_freq_dt <- as.data.frame(gsmap_jf_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("gsmap"))%>%
  `[`(, season := factor("jf"))

gsmap_jf_freq_dt$date <- gsub("index_(\\d+)", "x\\1", gsmap_jf_freq_dt$date)

#summary(gsmap_jf_freq_dt)
gsmap_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]


### for MAM

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(gsmap_mam), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

gsmap_hourly_mean_mam <- stackApply(gsmap_mam, indices, fun = mean)
gsmap_mam_mean_dt <- as.data.frame(gsmap_hourly_mean_mam, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("gsmap"))%>%
  `[`(, season := factor("mam"))

gsmap_mam_mean_dt[!is.finite(prec_mean), prec_mean := NA]
gsmap_mam_mean_dt$date <- gsub("index_(\\d+)", "X\\1", gsmap_mam_mean_dt$date)

### Intensity
total_precip_mam <- stackApply(gsmap_mam, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_mam <- stackApply(gsmap_mam, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

gsmap_mam_intensity <- total_precip_mam / precip_hours_mam

#plot(gsmap_mam_intensity[[1]])

gsmap_mam_int_dt <- as.data.frame(gsmap_mam_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("gsmap"))%>%
  `[`(, season := factor("mam"))

gsmap_mam_int_dt$date <- gsub("index_(\\d+)", "x\\1", gsmap_mam_int_dt$date)

#summary(gsmap_mam_int_dt)
#gsmap_mam_intensity_dt[prec_int == Inf]
gsmap_mam_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_mam <- length(indices)
gsmap_mam_frequency <- (precip_hours_mam / total_available_hours_mam) * 100

gsmap_mam_freq_dt <- as.data.frame(gsmap_mam_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("gsmap")) %>%
  `[`(, season := factor("mam"))

gsmap_mam_freq_dt$date <- gsub("index_(\\d+)", "x\\1", gsmap_mam_freq_dt$date)

#summary(gsmap_mam_freq_dt)
gsmap_mam_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(gsmap_mam_freq_dt)

### for JJAS

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(gsmap_jjas), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

gsmap_hourly_mean_jjas <- stackApply(gsmap_jjas, indices, fun = mean)
gsmap_jjas_mean_dt <- as.data.frame(gsmap_hourly_mean_jjas, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("gsmap"))%>%
  `[`(, season := factor("jjas"))

gsmap_jjas_mean_dt[!is.finite(prec_mean), prec_mean := NA]
gsmap_jjas_mean_dt$date <- gsub("index_(\\d+)", "X\\1", gsmap_jjas_mean_dt$date)

### Intensity
total_precip_jjas <- stackApply(gsmap_jjas, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jjas <- stackApply(gsmap_jjas, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

gsmap_jjas_intensity <- total_precip_jjas / precip_hours_jjas

#plot(gsmap_jjas_intensity[[1]])

gsmap_jjas_int_dt <- as.data.frame(gsmap_jjas_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("gsmap"))%>%
  `[`(, season := factor("jjas"))

gsmap_jjas_int_dt$date <- gsub("index_(\\d+)", "x\\1", gsmap_jjas_int_dt$date)
#summary(gsmap_jjas_int_dt)
#gsmap_jjas_intensity_dt[prec_int == Inf]
gsmap_jjas_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_jjas <- length(indices)
gsmap_jjas_frequency <- (precip_hours_jjas / total_available_hours_jjas) * 100

gsmap_jjas_freq_dt <- as.data.frame(gsmap_jjas_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("gsmap")) %>%
  `[`(, season := factor("jjas"))

gsmap_jjas_freq_dt$date <- gsub("index_(\\d+)", "x\\1", gsmap_jjas_freq_dt$date)
#summary(gsmap_jjas_freq_dt)
gsmap_jjas_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(gsmap_jjas_freq_dt)


### for OND

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(gsmap_ond), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

gsmap_hourly_mean_ond <- stackApply(gsmap_ond, indices, fun = mean)
gsmap_ond_mean_dt <- as.data.frame(gsmap_hourly_mean_ond, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("gsmap"))%>%
  `[`(, season := factor("ond"))

gsmap_ond_mean_dt[!is.finite(prec_mean), prec_mean := NA]
gsmap_ond_mean_dt$date <- gsub("index_(\\d+)", "X\\1", gsmap_ond_mean_dt$date)

### Intensity
total_precip_ond <- stackApply(gsmap_ond, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_ond <- stackApply(gsmap_ond, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

gsmap_ond_intensity <- total_precip_ond / precip_hours_ond

#plot(gsmap_ond_intensity[[1]])

gsmap_ond_int_dt <- as.data.frame(gsmap_ond_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("gsmap"))%>%
  `[`(, season := factor("ond"))

gsmap_ond_int_dt$date <- gsub("index_(\\d+)", "x\\1", gsmap_ond_int_dt$date)
#summary(gsmap_ond_int_dt)
#gsmap_ond_intensity_dt[prec_int == Inf]
gsmap_ond_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_ond <- length(indices)
gsmap_ond_frequency <- (precip_hours_ond / total_available_hours_ond) * 100

gsmap_ond_freq_dt <- as.data.frame(gsmap_ond_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("gsmap")) %>%
  `[`(, season := factor("ond"))

gsmap_ond_freq_dt$date <- gsub("index_(\\d+)", "x\\1", gsmap_ond_freq_dt$date)
#summary(gsmap_ond_freq_dt)
gsmap_ond_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(gsmap_ond_freq_dt)


### comine everything to a lsit

gsmap_allseason_mean_dt <- list(gsmap_jf_mean_dt, gsmap_mam_mean_dt, gsmap_jjas_mean_dt, 
                                gsmap_ond_mean_dt)

gsmap_allseason_freq_dt <- list(gsmap_jf_freq_dt, gsmap_mam_freq_dt, gsmap_jjas_freq_dt, 
                                gsmap_ond_freq_dt)

gsmap_allseason_int_dt <- list(gsmap_jf_int_dt, gsmap_mam_int_dt, gsmap_jjas_int_dt, 
                               gsmap_ond_int_dt)


### save as .RDS

saveRDS(gsmap_allseason_mean_dt, "./projects/kenya_example/data/output/gsmap_allseason_mean_dt.RDS")
saveRDS(gsmap_allseason_int_dt, "./projects/kenya_example/data/output/gsmap_allseason_int_dt.RDS")
saveRDS(gsmap_allseason_freq_dt, "./projects/kenya_example/data/output/gsmap_allseason_freq_dt.RDS")
###############################

# try with ERA5 ---------------------------------------

library(terra)
library(data.table)
library(dplyr)


era5 <-  rast("./projects/kenya_example/data/era5_hour_kenya_2001_20_regrid.nc") 

# for era5 

dte <- format(as.POSIXct(time(era5), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")

m <- as.numeric(dte)
jf <- (m >= 1 & m <= 2)
mam <- (m >= 3 & m <= 5)
jjas <- (m >= 6 & m <= 9)
ond <- (m >= 10 & m <= 12)

era5_jf <- era5[[jf]]
era5_mam <- era5[[mam]]
era5_jjas <- era5[[jjas]]
era5_ond <- era5[[ond]]


#### for JF
#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(era5_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

era5_hourly_mean_jf <- tapp(era5_jf, indices, fun = mean)
era5_jf_mean_dt <- as.data.frame(era5_hourly_mean_jf, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("era5"))%>%
  `[`(, season := factor("jf"))

era5_jf_mean_dt[!is.finite(prec_mean), prec_mean := NA]

### Intensity
total_precip_jf <- tapp(era5_jf, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jf <- tapp(era5_jf, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

era5_jf_intensity <- total_precip_jf / precip_hours_jf

#plot(era5_jf_intensity[[1]])

era5_jf_int_dt <- as.data.frame(era5_jf_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("era5"))%>%
  `[`(, season := factor("jf"))


#era5_jf_intensity_dt[prec_int == Inf]
era5_jf_int_dt[!is.finite(prec_int), prec_int := NA]
#summary(era5_jf_int_dt)

### Frequency
# Calculate the total available hours
total_available_hours_jf <- length(indices)
era5_jf_frequency <- (precip_hours_jf / total_available_hours_jf) * 100

era5_jf_freq_dt <- as.data.frame(era5_jf_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("era5"))%>%
  `[`(, season := factor("jf"))

#summary(era5_jf_freq_dt)
era5_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]


### for MAM

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(era5_mam), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation 
era5_hourly_mean_mam <- tapp(era5_mam, indices, fun = mean)
era5_mam_mean_dt <- as.data.frame(era5_hourly_mean_mam, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("era5"))%>%
  `[`(, season := factor("mam"))

era5_mam_mean_dt[!is.finite(prec_mean), prec_mean := NA]

### Intensity
total_precip_mam <- tapp(era5_mam, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_mam <- tapp(era5_mam, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

era5_mam_intensity <- total_precip_mam / precip_hours_mam

#plot(era5_mam_intensity[[1]])

era5_mam_int_dt <- as.data.frame(era5_mam_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("era5"))%>%
  `[`(, season := factor("mam"))

#summary(era5_mam_int_dt)
#era5_mam_intensity_dt[prec_int == Inf]
era5_mam_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_mam <- length(indices)
era5_mam_frequency <- (precip_hours_mam / total_available_hours_mam) * 100

era5_mam_freq_dt <- as.data.frame(era5_mam_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("era5")) %>%
  `[`(, season := factor("mam"))

#summary(era5_mam_freq_dt)
era5_mam_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(era5_mam_freq_dt)

### for JJAS

indices <- format(as.POSIXct(time(era5_jjas), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation
era5_hourly_mean_jjas <- tapp(era5_jjas, indices, fun = mean)
era5_jjas_mean_dt <- as.data.frame(era5_hourly_mean_jjas, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("era5"))%>%
  `[`(, season := factor("jjas"))

era5_jjas_mean_dt[!is.finite(prec_mean), prec_mean := NA]

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(era5_jjas), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_jjas <- tapp(era5_jjas, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jjas <- tapp(era5_jjas, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

era5_jjas_intensity <- total_precip_jjas / precip_hours_jjas

#plot(era5_jjas_intensity[[1]])

era5_jjas_int_dt <- as.data.frame(era5_jjas_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("era5"))%>%
  `[`(, season := factor("jjas"))

#summary(era5_jjas_int_dt)
#era5_jjas_intensity_dt[prec_int == Inf]
era5_jjas_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_jjas <- length(indices)
era5_jjas_frequency <- (precip_hours_jjas / total_available_hours_jjas) * 100

era5_jjas_freq_dt <- as.data.frame(era5_jjas_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("era5")) %>%
  `[`(, season := factor("jjas"))

#summary(era5_jjas_freq_dt)
era5_jjas_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(era5_jjas_freq_dt)


### for OND

indices <- format(as.POSIXct(time(era5_ond), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)


### 24 hourly mean diurnal precipitation
era5_hourly_mean_ond <- tapp(era5_ond, indices, fun = mean)
era5_ond_mean_dt <- as.data.frame(era5_hourly_mean_ond, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("era5"))%>%
  `[`(, season := factor("ond"))

era5_ond_mean_dt[!is.finite(prec_mean), prec_mean := NA]

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(era5_ond), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_ond <- tapp(era5_ond, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_ond <- tapp(era5_ond, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

era5_ond_intensity <- total_precip_ond / precip_hours_ond

#plot(era5_ond_intensity[[1]])

era5_ond_int_dt <- as.data.frame(era5_ond_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("era5"))%>%
  `[`(, season := factor("ond"))

#summary(era5_ond_int_dt)
#era5_ond_intensity_dt[prec_int == Inf]
era5_ond_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_ond <- length(indices)
era5_ond_frequency <- (precip_hours_ond / total_available_hours_ond) * 100

era5_ond_freq_dt <- as.data.frame(era5_ond_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("era5")) %>%
  `[`(, season := factor("ond"))

#summary(era5_ond_freq_dt)
era5_ond_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(era5_ond_freq_dt)


### combine everything to a list

era5_allseason_mean_dt <- list(era5_jf_mean_dt, era5_mam_mean_dt, era5_jjas_mean_dt, 
                                 era5_ond_mean_dt)

era5_allseason_freq_dt <- list(era5_jf_freq_dt, era5_mam_freq_dt, era5_jjas_freq_dt, 
                                 era5_ond_freq_dt)

era5_allseason_int_dt <- list(era5_jf_int_dt, era5_mam_int_dt, era5_jjas_int_dt, 
                                era5_ond_int_dt)


### save as .RDS

saveRDS(era5_allseason_mean_dt, "./projects/kenya_example/data/output/era5_allseason_mean_dt.RDS")
saveRDS(era5_allseason_int_dt, "./projects/kenya_example/data/output/era5_allseason_int_dt.RDS")
saveRDS(era5_allseason_freq_dt, "./projects/kenya_example/data/output/era5_allseason_freq_dt.RDS")


# try with PERSIANN -----------------------------------------------------------------------------

# diurnal cycle of frequency and intensity at seasonal scale

library(terra)
library(data.table)
library(dplyr)

persiann <-  rast("./projects/kenya_example/data/persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc") 


# time(persiann) <- seq(from = as.POSIXct("2001-01-01:00:00:00"), by = "1 hour", length.out = nlyr(persiann))
# persiann[[172805]]

pers_time <- names(persiann)
pers_time <- as.numeric(gsub("[^0-9.]", "", pers_time))
posixct_time <- as.POSIXct(pers_time*3600, origin = "2001-01-01 00:00:00") #3600 second per hour
#names(persiann) <- posixct_time
time(persiann)<- posixct_time

persiann[[172805]]

dte <- format(as.POSIXct(time(persiann), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")
m <- as.numeric(dte)

jf <- (m >= 1 & m <= 2)
mam <- (m >= 3 & m <= 5)
jjas <- (m >= 6 & m <= 9)
ond <- (m >= 10 & m <= 12)

persiann_jf <- persiann[[jf]]
persiann_mam <- persiann[[mam]]
persiann_jjas <- persiann[[jjas]]
persiann_ond <- persiann[[ond]]


### for JF
#get the date from the names of the layers and extract the month 
indices <- format(as.POSIXct(time(persiann_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation
persiann_hourly_mean_jf <- tapp(persiann_jf, indices, fun = mean, na.rm = TRUE)
persiann_jf_mean_dt <- as.data.frame(persiann_hourly_mean_jf, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("jf"))

persiann_jf_mean_dt[!is.finite(prec_mean), prec_mean := NA]

### Intensity
total_precip_jf <- tapp(persiann_jf, indices, fun = sum, na.rm = TRUE)

# Calculate the precipitation hours using stackApply()
precip_hours_jf <- tapp(persiann_jf, indices, fun = function(x, na.rm = TRUE) {
  sum(x > 0.1, na.rm = TRUE)
})

persiann_jf_intensity <- total_precip_jf / precip_hours_jf

#plot(persiann_jf_intensity[[1]])

persiann_jf_int_dt <- as.data.frame(persiann_jf_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("jf"))


#persiann_jf_intensity_dt[prec_int == Inf]
persiann_jf_int_dt[!is.finite(prec_int), prec_int := NA]
#summary(persiann_jf_int_dt)

### Frequency
# Calculate the total available hours
total_available_hours_jf <- length(indices)
persiann_jf_frequency <- (precip_hours_jf / total_available_hours_jf) * 100

persiann_jf_freq_dt <- as.data.frame(persiann_jf_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("jf"))

#summary(persiann_jf_freq_dt)
persiann_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]


### for MAM

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(persiann_mam), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation
persiann_hourly_mean_mam <- tapp(persiann_mam, indices, fun = mean, na.rm = TRUE)
persiann_mam_mean_dt <- as.data.frame(persiann_hourly_mean_mam, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("mam"))

persiann_mam_mean_dt[!is.finite(prec_mean), prec_mean := NA]

### Intensity
total_precip_mam <- tapp(persiann_mam, indices, fun = sum, na.rm = TRUE)

# Calculate the precipitation hours using stackApply()
precip_hours_mam <- tapp(persiann_mam, indices, fun = function(x, na.rm = TRUE) {
  sum(x > 0.1, na.rm = TRUE)
})
persiann_mam_intensity <- total_precip_mam / precip_hours_mam

#plot(persiann_mam_intensity[[1]])

persiann_mam_int_dt <- as.data.frame(persiann_mam_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("mam"))

#summary(persiann_mam_int_dt)
#persiann_mam_intensity_dt[prec_int == Inf]
persiann_mam_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_mam <- length(indices)
persiann_mam_frequency <- (precip_hours_mam / total_available_hours_mam) * 100

persiann_mam_freq_dt <- as.data.frame(persiann_mam_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann")) %>%
  `[`(, season := factor("mam"))

#summary(persiann_mam_freq_dt)
persiann_mam_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(persiann_mam_freq_dt)

### for JJAS

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(persiann_jjas), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation
persiann_hourly_mean_jjas <- tapp(persiann_jjas, indices, fun = mean, na.rm = TRUE)
persiann_jjas_mean_dt <- as.data.frame(persiann_hourly_mean_jjas, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("jjas"))

persiann_jjas_mean_dt[!is.finite(prec_mean), prec_mean := NA]

### Intensity
total_precip_jjas <- tapp(persiann_jjas, indices, fun = sum, na.rm = TRUE)

# Calculate the precipitation hours using stackApply()
precip_hours_jjas <- tapp(persiann_jjas, indices, fun = function(x, na.rm = TRUE) {
  sum(x > 0.1, na.rm = TRUE)
})
persiann_jjas_intensity <- total_precip_jjas / precip_hours_jjas

#plot(persiann_jjas_intensity[[1]])

persiann_jjas_int_dt <- as.data.frame(persiann_jjas_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("jjas"))

#summary(persiann_jjas_int_dt)
#persiann_jjas_intensity_dt[prec_int == Inf]
persiann_jjas_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_jjas <- length(indices)
persiann_jjas_frequency <- (precip_hours_jjas / total_available_hours_jjas) * 100

persiann_jjas_freq_dt <- as.data.frame(persiann_jjas_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann")) %>%
  `[`(, season := factor("jjas"))

#summary(persiann_jjas_freq_dt)
persiann_jjas_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(persiann_jjas_freq_dt)


### for OND

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(persiann_ond), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices) 

### 24 hourly mean diurnal precipitation
persiann_hourly_mean_ond <- tapp(persiann_ond, indices, fun = mean, na.rm = TRUE)
persiann_ond_mean_dt <- as.data.frame(persiann_hourly_mean_ond, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("ond"))

persiann_ond_mean_dt[!is.finite(prec_mean), prec_mean := NA]

### Intensity
total_precip_ond <- tapp(persiann_ond, indices, fun = sum, na.rm = TRUE)

# Calculate the precipitation hours using stackApply()
precip_hours_ond <- tapp(persiann_ond, indices, fun = function(x, na.rm = TRUE) {
  sum(x > 0.1, na.rm = TRUE)
})
persiann_ond_intensity <- total_precip_ond / precip_hours_ond

#plot(persiann_ond_intensity[[1]])

persiann_ond_int_dt <- as.data.frame(persiann_ond_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("ond"))

#summary(persiann_ond_int_dt)
#persiann_ond_intensity_dt[prec_int == Inf]
persiann_ond_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_ond <- length(indices)
persiann_ond_frequency <- (precip_hours_ond / total_available_hours_ond) * 100

persiann_ond_freq_dt <- as.data.frame(persiann_ond_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann")) %>%
  `[`(, season := factor("ond"))

#summary(persiann_ond_freq_dt)
persiann_ond_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(persiann_ond_freq_dt)


### comine everything to a lsit

persiann_allseason_mean_dt <- list(persiann_jf_mean_dt, persiann_mam_mean_dt, persiann_jjas_mean_dt, 
                               persiann_ond_mean_dt)

persiann_allseason_freq_dt <- list(persiann_jf_freq_dt, persiann_mam_freq_dt, persiann_jjas_freq_dt, 
                                 persiann_ond_freq_dt)

persiann_allseason_int_dt <- list(persiann_jf_int_dt, persiann_mam_int_dt, persiann_jjas_int_dt, 
                                persiann_ond_int_dt)


### save as .RDS

saveRDS(persiann_allseason_mean_dt, "./projects/kenya_example/data/output/persiann_allseason_mean_dt.RDS")
saveRDS(persiann_allseason_int_dt, "./projects/kenya_example/data/output/persiann_allseason_int_dt.RDS")
saveRDS(persiann_allseason_freq_dt, "./projects/kenya_example/data/output/persiann_allseason_freq_dt.RDS")


#############################################################################################
#############################################################################################

# create a function of the same -------------------------------------------

library(terra)
library(data.table)
library(dplyr)


estimate_diurnal <- function(datasetname, output_dir) {
  dataset <- rast(datasetname)
  datasetname <- deparse(substitute(datasetname))  # Get the actual object name
  
  if (datasetname == "persiann") {
    time(dataset) <- seq(from = as.POSIXct("2001-01-01:00:00:00"), by = "1 hour", length.out = nlyr(dataset))
  }
  
  dte <- format(as.POSIXct(time(dataset), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")
  m <- as.numeric(dte)
  jf <- (m >= 1 & m <= 2)
  mam <- (m >= 3 & m <= 5)
  jjas <- (m >= 6 & m <= 9)
  ond <- (m >= 10 & m <= 12)
  
  dataset_jf <- dataset[[jf]]
  dataset_mam <- dataset[[mam]]
  dataset_jjas <- dataset[[jjas]]
  dataset_ond <- dataset[[ond]]
  
  #For JF
  indices <- format(as.POSIXct(time(dataset_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
  indices <- as.numeric(indices)
  
  total_precip_jf <- tapp(dataset_jf, indices, fun = sum)
  precip_hours_jf <- tapp(dataset_jf, indices, fun = function(x, na.rm = TRUE) sum(x > 0.1))
  dataset_jf_intensity <- total_precip_jf / precip_hours_jf
  
  dataset_jf_int_dt <- as.data.frame(dataset_jf_intensity, xy = TRUE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
    `[`(, name := factor(datasetname)) %>%
    `[`(, season := factor("jf"))
  
  dataset_jf_int_dt[!is.finite(prec_int), prec_int := NA]
  
  total_available_hours_jf <- length(indices)
  dataset_jf_frequency <- (precip_hours_jf / total_available_hours_jf) * 100
  
  dataset_jf_freq_dt <- as.data.frame(dataset_jf_frequency, xy = TRUE) %>%
    as.data.table() %>%
    melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
    as.data.table() %>%
    `[`(, name := factor(paste(datasetname, "_jf", sep = ""))) %>%
    `[`(, season := factor("jf"))
  
  dataset_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]
  
  # Perform similar operations for other seasons (MAM, JJAS, and OND)

  #For MAM
  indices <- format(as.POSIXct(time(dataset_mam), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
  indices <- as.numeric(indices)
  
  total_precip_mam <- tapp(dataset_mam, indices, fun = sum)
  precip_hours_mam <- tapp(dataset_mam, indices, fun = function(x, na.rm = TRUE) sum(x > 0.1))
  dataset_mam_intensity <- total_precip_mam / precip_hours_mam
  
  dataset_mam_int_dt <- as.data.frame(dataset_mam_intensity, xy = TRUE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
    `[`(, name := factor(datasetname)) %>%
    `[`(, season := factor("mam"))
  
  dataset_mam_int_dt[!is.finite(prec_int), prec_int := NA]
  
  total_available_hours_mam <- length(indices)
  dataset_mam_frequency <- (precip_hours_mam / total_available_hours_mam) * 100
  
  dataset_mam_freq_dt <- as.data.frame(dataset_mam_frequency, xy = TRUE) %>%
    as.data.table() %>%
    melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
    as.data.table() %>%
    `[`(, name := factor(paste(datasetname, "_mam", sep = ""))) %>%
    `[`(, season := factor("mam"))
  
  dataset_mam_freq_dt[!is.finite(prec_freq), prec_freq := NA]
  
  # for JJAS 
  
  indices <- format(as.POSIXct(time(dataset_jjas), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
  indices <- as.numeric(indices)
  
  total_precip_jjas <- tapp(dataset_jjas, indices, fun = sum)
  precip_hours_jjas <- tapp(dataset_jjas, indices, fun = function(x, na.rm = TRUE) sum(x > 0.1))
  dataset_jjas_intensity <- total_precip_jjas / precip_hours_jjas
  
  dataset_jjas_int_dt <- as.data.frame(dataset_jjas_intensity, xy = TRUE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
    `[`(, name := factor(datasetname)) %>%
    `[`(, season := factor("jjas"))
  
  dataset_jjas_int_dt[!is.finite(prec_int), prec_int := NA]
  
  total_available_hours_jjas <- length(indices)
  dataset_jjas_frequency <- (precip_hours_jjas / total_available_hours_jjas) * 100
  
  dataset_jjas_freq_dt <- as.data.frame(dataset_jjas_frequency, xy = TRUE) %>%
    as.data.table() %>%
    melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
    as.data.table() %>%
    `[`(, name := factor(paste(datasetname, "_jjas", sep = ""))) %>%
    `[`(, season := factor("jjas"))
  
  dataset_jjas_freq_dt[!is.finite(prec_freq), prec_freq := NA] 
  
  #For OND
  indices <- format(as.POSIXct(time(dataset_ond), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
  indices <- as.numeric(indices)
  
  total_precip_ond <- tapp(dataset_ond, indices, fun = sum)
  precip_hours_ond <- tapp(dataset_ond, indices, fun = function(x, na.rm = TRUE) sum(x > 0.1))
  dataset_ond_intensity <- total_precip_ond / precip_hours_ond
  
  dataset_ond_int_dt <- as.data.frame(dataset_ond_intensity, xy = TRUE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
    `[`(, name := factor(datasetname)) %>%
    `[`(, season := factor("ond"))
  
  dataset_ond_int_dt[!is.finite(prec_int), prec_int := NA]
  
  total_available_hours_ond <- length(indices)
  dataset_ond_frequency <- (precip_hours_ond / total_available_hours_ond) * 100
  
  dataset_ond_freq_dt <- as.data.frame(dataset_ond_frequency, xy = TRUE) %>%
    as.data.table() %>%
    melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
    as.data.table() %>%
    `[`(, name := factor(paste(datasetname, "_ond", sep = ""))) %>%
    `[`(, season := factor("ond"))
  
  dataset_ond_freq_dt[!is.finite(prec_freq), prec_freq := NA]
  
  dataset_allseason_freq_dt <- list(dataset_jf_freq_dt, dataset_mam_freq_dt, dataset_jjas_freq_dt, dataset_ond_freq_dt)
  dataset_allseason_int_dt <- list(dataset_jf_int_dt, dataset_mam_int_dt, dataset_jjas_int_dt, dataset_ond_int_dt)
  
  # Save as .RDS
  saveRDS(dataset_allseason_int_dt, file.path(output_dir, paste(datasetname, "_allseason_int_dt.RDS", sep = "")))
  saveRDS(dataset_allseason_freq_dt, file.path(output_dir, paste(datasetname, "_allseason_freq_dt.RDS", sep = "")))
}



######parallel version--------------

library(terra)
library(data.table)
library(dplyr)
library(foreach)
library(doParallel)

estimate_diurnal_parallel <- function(datasetname, output_dir) {
  dataset <- rast(datasetname)
  
  if (datasetname == "persiann") {
    time(dataset) <- seq(from = as.POSIXct("2001-01-01:00:00:00"), by = "1 hour", length.out = nlyr(dataset))
  }
  
  dte <- format(as.POSIXct(time(dataset), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")
  m <- as.numeric(dte)
  jf <- (m >= 1 & m <= 2)
  mam <- (m >= 3 & m <= 5)
  jjas <- (m >= 6 & m <= 9)
  ond <- (m >= 10 & m <= 12)
  
  dataset_jf <- dataset[[jf]]
  dataset_mam <- dataset[[mam]]
  dataset_jjas <- dataset[[jjas]]
  dataset_ond <- dataset[[ond]]
  
  indices <- format(as.POSIXct(time(dataset_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
  indices <- as.numeric(indices)
  
  total_precip_jf <- tapp(dataset_jf, indices, fun = sum)
  precip_hours_jf <- tapp(dataset_jf, indices, fun = function(x, na.rm = TRUE) sum(x > 0.1))
  dataset_jf_intensity <- total_precip_jf / precip_hours_jf
  
  dataset_jf_int_dt <- as.data.frame(dataset_jf_intensity, xy = TRUE) %>%
    as.data.table() %>%
    data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
    `[`(, name := factor(datasetname)) %>%
    `[`(, season := factor("jf"))
  
  dataset_jf_int_dt[!is.finite(prec_int), prec_int := NA]
  
  total_available_hours_jf <- length(indices)
  dataset_jf_frequency <- (precip_hours_jf / total_available_hours_jf) * 100
  
  dataset_jf_freq_dt <- as.data.frame(dataset_jf_frequency, xy = TRUE) %>%
    as.data.table() %>%
    melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
    as.data.table() %>%
    `[`(, name := factor(paste(datasetname, "_jf", sep = ""))) %>%
    `[`(, season := factor("jf"))
  
  dataset_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]
  
  # Create a cluster object and register it
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  # Define the seasons and datasets
  seasons <- c("jf", "mam", "jjas", "ond")
  datasets <- list(dataset_jf, dataset_mam, dataset_jjas, dataset_ond)
  
  # Perform the calculations in parallel
  dataset_allseason_freq_dt <- foreach(i = 1:length(seasons), .combine = rbind) %dopar% {
    season <- seasons[i]
    current_dataset <- datasets[[i]]
    
    indices <- format(as.POSIXct(time(current_dataset), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
    indices <- as.numeric(indices)
    
    total_precip <- tapp(current_dataset, indices, fun = sum)
    precip_hours <- tapp(current_dataset, indices, fun = function(x, na.rm = TRUE) sum(x > 0.1))
    dataset_intensity <- total_precip / precip_hours
    
    dataset_int_dt <- as.data.frame(dataset_intensity, xy = TRUE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
      `[`(, name := factor(datasetname)) %>%
      `[`(, season := factor(season))
    
    dataset_int_dt[!is.finite(prec_int), prec_int := NA]
    
    total_available_hours <- length(indices)
    dataset_frequency <- (precip_hours / total_available_hours) * 100
    
    dataset_freq_dt <- as.data.frame(dataset_frequency, xy = TRUE) %>%
      as.data.table() %>%
      melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
      as.data.table() %>%
      `[`(, name := factor(paste(datasetname, "_", season, sep = ""))) %>%
      `[`(, season := factor(season))
    
    dataset_freq_dt[!is.finite(prec_freq), prec_freq := NA]
    
    return(dataset_freq_dt)
  }
  
  # Stop the cluster and unregister it
  stopCluster(cl)
  registerDoSEQ()
  
  dataset_allseason_int_dt <- foreach(i = 1:length(seasons), .combine = rbind) %dopar% {
    season <- seasons[i]
    current_dataset <- datasets[[i]]
    
    indices <- format(as.POSIXct(time(current_dataset), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
    indices <- as.numeric(indices)
    
    total_precip <- tapp(current_dataset, indices, fun = sum)
    precip_hours <- tapp(current_dataset, indices, fun = function(x, na.rm = TRUE) sum(x > 0.1))
    dataset_intensity <- total_precip / precip_hours
    
    dataset_int_dt <- as.data.frame(dataset_intensity, xy = TRUE) %>%
      as.data.table() %>%
      data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
      `[`(, name := factor(datasetname)) %>%
      `[`(, season := factor(season))
    
    dataset_int_dt[!is.finite(prec_int), prec_int := NA]
    
    return(dataset_int_dt)
  }
  
  # Save as .RDS
  saveRDS(dataset_allseason_int_dt, file.path(output_dir, paste(datasetname, "_allseason_int_dt.RDS", sep = "")))
  saveRDS(dataset_allseason_freq_dt, file.path(output_dir, paste(datasetname, "_allseason_freq_dt.RDS", sep = "")))
}


###############################################################################

# here I just want confirm the results produced with seasonalextraction of data with the seasonal extraction of data from cdo

library(terra)
library(data.table)
library(dplyr)


cmorph_jf <- rast("./projects/kenya_example/data/cmorph_hour_JF_kenya_2001_20.nc")

indices <- format(as.POSIXct(time(cmorph_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation

cmorph_hourly_mean_jf <- tapp(cmorph_jf, indices, fun = mean)
cmorph_jf_mean_dt <- as.data.frame(cmorph_hourly_mean_jf, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("jf"))

cmorph_jf_mean_dt[!is.finite(prec_mean), prec_mean := NA]

### Intensity
total_precip_jf <- tapp(cmorph_jf, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jf <- tapp(cmorph_jf, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

cmorph_jf_intensity <- total_precip_jf / precip_hours_jf

#plot(cmorph_jf_intensity[[1]])

cmorph_jf_int_dt <- as.data.frame(cmorph_jf_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("jf"))


#cmorph_jf_intensity_dt[prec_int == Inf]
cmorph_jf_int_dt[!is.finite(prec_int), prec_int := NA]
#summary(cmorph_jf_int_dt)

### Frequency
# Calculate the total available hours
total_available_hours_jf <- length(indices)
cmorph_jf_frequency <- (precip_hours_jf / total_available_hours_jf) * 100

cmorph_jf_freq_dt <- as.data.frame(cmorph_jf_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("cmorph"))%>%
  `[`(, season := factor("jf"))

#summary(cmorph_jf_freq_dt)
cmorph_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]

#validate outputs

cmorph_allseason_mean_dt <- readRDS("./projects/kenya_example/data/output/cmorph_allseason_mean_dt.RDS")
cmorph_allseason_mean_dt[[1]]
cmorph_jf_mean_dt

cmorph_allseason_int_dt <- readRDS("./projects/kenya_example/data/output/cmorph_allseason_int_dt.RDS")
cmorph_allseason_int_dt[[1]]
cmorph_jf_int_dt

cmorph_allseason_freq_dt <- readRDS("./projects/kenya_example/data/output/cmorph_allseason_freq_dt.RDS")
cmorph_allseason_freq_dt[[1]]
cmorph_jf_freq_dt

#################### with Persiann----------------

library(raster)

persiann_jf <- brick("./projects/kenya_example/data/persiann_hour_JF_kenya_2001_20_regrid_misrmv_negt.nc")

pers_time <- getZ(persiann_jf)
posixct_time <- as.POSIXct(pers_time * 3600, origin = "2001-01-01 00:00:00")
names(persiann_jf) <- posixct_time

persiann_jf[[28261]]

indices <- format(as.POSIXct(names(persiann_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### 24 hourly mean diurnal precipitation
persiann_hourly_mean_jf <- stackApply(persiann_jf, indices, fun = mean, na.rm = TRUE)
persiann_jf_mean_dt <- as.data.frame(persiann_hourly_mean_jf, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("jf"))

persiann_jf_mean_dt[!is.finite(prec_mean), prec_mean := NA]

### Intensity
total_precip_jf <- stackApply(persiann_jf, indices, fun = sum, na.rm = TRUE)

# Calculate the precipitation hours using stackApply()
precip_hours_jf <- stackApply(persiann_jf, indices, fun = function(x, na.rm = TRUE) {
  sum(x > 0.1, na.rm = TRUE)
})

persiann_jf_intensity <- total_precip_jf / precip_hours_jf

#plot(persiann_jf_intensity[[1]])

persiann_jf_int_dt <- as.data.frame(persiann_jf_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("jf"))


#persiann_jf_intensity_dt[prec_int == Inf]
persiann_jf_int_dt[!is.finite(prec_int), prec_int := NA]
#summary(persiann_jf_int_dt)

### Frequency
# Calculate the total available hours
total_available_hours_jf <- length(indices)
persiann_jf_frequency <- (precip_hours_jf / total_available_hours_jf) * 100

persiann_jf_freq_dt <- as.data.frame(persiann_jf_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("jf"))

#summary(persiann_jf_freq_dt)
persiann_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]

#validate outputs

cdo_jf_mean <- brick("./projects/kenya_example/data/hourly_charact/hourly_mean_persiann_JF_kenya_2001_20.nc")
cdo_jf_mean_dt <- as.data.frame(cdo_jf_mean, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_mean") %>% 
  as.data.table() %>%
  `[`(, name := factor("persiann"))%>%
  `[`(, season := factor("jf"))

cdo_jf_mean_dt
persiann_jf_mean_dt
persiann_allseason_mean_dt <- readRDS("./projects/kenya_example/data/output/persiann_allseason_mean_dt.RDS")
persiann_allseason_mean_dt[[1]]


persiann_allseason_int_dt <- readRDS("./projects/kenya_example/data/output/persiann_allseason_int_dt.RDS")
persiann_allseason_int_dt[[1]]
persiann_jf_int_dt

persiann_allseason_freq_dt <- readRDS("./projects/kenya_example/data/output/persiann_allseason_freq_dt.RDS")
persiann_allseason_freq_dt[[1]]
persiann_jf_freq_dt






# diurnal cycle of frequency and intensity at seasonal scale

library(data.table)
library(dplyr)


cmorph <-  brick("./projects/kenya_example/data/cmorph_hour_kenya_2001_20.nc") 
#imerg <-  rast("./projects/kenya_example/data/imerg_f_hour_kenya_2001_20_grid_025_fliptrans.nc") 

# for cmorph 

dte <- format(as.POSIXct(names(cmorph), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")

m <- as.numeric(dte)
# jf <- (m >= 1 & m <= 2)
# mam <- (m >= 3 & m <= 5)
# jjas <- (m >= 6 & m <= 9)
# ond <- (m >= 10 & m <= 12)

cmorph_jf <- raster::subset(cmorph, which(m %in% c(1, 2)))
cmorph_mam <- raster::subset(cmorph, which(m %in% c(3, 4, 5)))
cmorph_jjas <- raster::subset(cmorph, which(m %in% c(6, 7, 8, 9)))
cmorph_ond <- raster::subset(cmorph, which(m %in% c(10, 11, 12)))



### for JF
#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(names(cmorph_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_jf <- stackApply(cmorph_jf, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jf <- stackApply(cmorph_jf, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

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
  `[`(, name := factor("cmorph_jf"))%>%
  `[`(, season := factor("jf"))

#summary(cmorph_jf_freq_dt)
cmorph_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]


### for MAM

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(cmorph_mam), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

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
cmorph_mam_frequency <- (precip_hours_mam / total_available_hours) * 100

cmorph_mam_freq_dt <- as.data.frame(cmorph_mam_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("cmorph_mam")) %>%
  `[`(, season := factor("mam"))

#summary(cmorph_mam_freq_dt)
cmorph_mam_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(cmorph_mam_freq_dt)

### for JJAS

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
cmorph_jjas_frequency <- (precip_hours_jjas / total_available_hours) * 100

cmorph_jjas_freq_dt <- as.data.frame(cmorph_jjas_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("cmorph_jjas")) %>%
  `[`(, season := factor("jjas"))

#summary(cmorph_jjas_freq_dt)
cmorph_jjas_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(cmorph_jjas_freq_dt)


### for OND

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
cmorph_ond_frequency <- (precip_hours_ond / total_available_hours) * 100

cmorph_ond_freq_dt <- as.data.frame(cmorph_ond_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("cmorph_ond")) %>%
  `[`(, season := factor("ond"))

#summary(cmorph_ond_freq_dt)
cmorph_ond_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(cmorph_ond_freq_dt)


### comine everything to a lsit

cmorph_allseason_freq_dt <- list(cmorph_jf_freq_dt, cmorph_mam_freq_dt, cmorph_jjas_freq_dt, 
                                 cmorph_ond_freq_dt)


cmorph_allseason_int_dt <- list(cmorph_jf_int_dt, cmorph_mam_int_dt, cmorph_jjas_int_dt, 
                                 cmorph_ond_int_dt)


### save as .RDS

saveRDS(cmorph_allseason_int_dt, "./projects/kenya_example/data/output/rast_cmorph_allseason_int_dt.RDS")
saveRDS(cmorph_allseason_freq_dt, "./projects/kenya_example/data/output/rast_cmorph_allseason_freq_dt.RDS")

##################################################################

# Try with IMERG----------------------------------------------------------------- 

dte <- format(as.POSIXct(time(imerg), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")

m <- as.numeric(dte)
jf <- (m >= 1 & m <= 2)
mam <- (m >= 3 & m <= 5)
jjas <- (m >= 6 & m <= 9)
ond <- (m >= 10 & m <= 12)

imerg_jf <- imerg[[jf]]
imerg_mam <- imerg[[mam]]
imerg_jjas <- imerg[[jjas]]
imerg_ond <- imerg[[ond]]


### for JF
#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(imerg_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_jf <- tapp(imerg_jf, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jf <- tapp(imerg_jf, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_jf_intensity <- total_precip_jf / precip_hours_jf

#plot(imerg_jf_intensity[[1]])

imerg_jf_int_dt <- as.data.frame(imerg_jf_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("jf"))


#imerg_jf_intensity_dt[prec_int == Inf]
imerg_jf_int_dt[!is.finite(prec_int), prec_int := NA]
#summary(imerg_jf_int_dt)

### Frequency
# Calculate the total available hours
total_available_hours_jf <- length(indices)
imerg_jf_frequency <- (precip_hours_jf / total_available_hours) * 100

imerg_jf_freq_dt <- as.data.frame(imerg_jf_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg_jf"))%>%
  `[`(, season := factor("jf"))

#summary(imerg_jf_freq_dt)
imerg_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]


### for MAM

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(imerg_mam), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_mam <- tapp(imerg_mam, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_mam <- tapp(imerg_mam, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_mam_intensity <- total_precip_mam / precip_hours_mam

#plot(imerg_mam_intensity[[1]])

imerg_mam_int_dt <- as.data.frame(imerg_mam_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("mam"))

#summary(imerg_mam_int_dt)
#imerg_mam_intensity_dt[prec_int == Inf]
imerg_mam_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_mam <- length(indices)
imerg_mam_frequency <- (precip_hours_mam / total_available_hours) * 100

imerg_mam_freq_dt <- as.data.frame(imerg_mam_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg_mam")) %>%
  `[`(, season := factor("mam"))

#summary(imerg_mam_freq_dt)
imerg_mam_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(imerg_mam_freq_dt)

### for JJAS

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(imerg_jjas), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_jjas <- tapp(imerg_jjas, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jjas <- tapp(imerg_jjas, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_jjas_intensity <- total_precip_jjas / precip_hours_jjas

#plot(imerg_jjas_intensity[[1]])

imerg_jjas_int_dt <- as.data.frame(imerg_jjas_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("jjas"))

#summary(imerg_jjas_int_dt)
#imerg_jjas_intensity_dt[prec_int == Inf]
imerg_jjas_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_jjas <- length(indices)
imerg_jjas_frequency <- (precip_hours_jjas / total_available_hours) * 100

imerg_jjas_freq_dt <- as.data.frame(imerg_jjas_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg_jjas")) %>%
  `[`(, season := factor("jjas"))

#summary(imerg_jjas_freq_dt)
imerg_jjas_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(imerg_jjas_freq_dt)


### for OND

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(imerg_ond), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_ond <- tapp(imerg_ond, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_ond <- tapp(imerg_ond, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_ond_intensity <- total_precip_ond / precip_hours_ond

#plot(imerg_ond_intensity[[1]])

imerg_ond_int_dt <- as.data.frame(imerg_ond_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("ond"))

#summary(imerg_ond_int_dt)
#imerg_ond_intensity_dt[prec_int == Inf]
imerg_ond_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_ond <- length(indices)
imerg_ond_frequency <- (precip_hours_ond / total_available_hours) * 100

imerg_ond_freq_dt <- as.data.frame(imerg_ond_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg_ond")) %>%
  `[`(, season := factor("ond"))

#summary(imerg_ond_freq_dt)
imerg_ond_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(imerg_ond_freq_dt)


### comine everything to a lsit

imerg_allseason_freq_dt <- list(imerg_jf_freq_dt, imerg_mam_freq_dt, imerg_jjas_freq_dt, 
                                 imerg_ond_freq_dt)


imerg_allseason_int_dt <- list(imerg_jf_int_dt, imerg_mam_int_dt, imerg_jjas_int_dt, 
                                imerg_ond_int_dt)


### save as .RDS

saveRDS(imerg_allseason_int_dt, "./projects/kenya_example/data/output/imerg_allseason_int_dt.RDS")
saveRDS(imerg_allseason_freq_dt, "./projects/kenya_example/data/output/imerg_allseason_freq_dt.RDS")


#############################################################################

# try with PERSIANN -----------------------------------------------------------------------------

# diurnal cycle of frequency and intensity at seasonal scale

library(terra)
library(data.table)
library(dplyr)

persiann <-  rast("./projects/kenya_example/data/persiann_hour_kenya_2001_20.nc") 


time(persiann) <- seq(from = as.POSIXct("2001-01-01:00:00:00"), by = "1 hour", length.out = nlyr(persiann))

dte <- format(as.POSIXct(time(persiann), format = "X%Y.%m.%d.%H.%M.%S"), format = "%m")
m <- as.numeric(dte)

jf <- (m >= 1 & m <= 2)
mam <- (m >= 3 & m <= 5)
jjas <- (m >= 6 & m <= 9)
ond <- (m >= 10 & m <= 12)

imerg_jf <- imerg[[jf]]
imerg_mam <- imerg[[mam]]
imerg_jjas <- imerg[[jjas]]
imerg_ond <- imerg[[ond]]


### for JF
#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(imerg_jf), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_jf <- tapp(imerg_jf, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jf <- tapp(imerg_jf, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_jf_intensity <- total_precip_jf / precip_hours_jf

#plot(imerg_jf_intensity[[1]])

imerg_jf_int_dt <- as.data.frame(imerg_jf_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("jf"))


#imerg_jf_intensity_dt[prec_int == Inf]
imerg_jf_int_dt[!is.finite(prec_int), prec_int := NA]
#summary(imerg_jf_int_dt)

### Frequency
# Calculate the total available hours
total_available_hours_jf <- length(indices)
imerg_jf_frequency <- (precip_hours_jf / total_available_hours) * 100

imerg_jf_freq_dt <- as.data.frame(imerg_jf_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg_jf"))%>%
  `[`(, season := factor("jf"))

#summary(imerg_jf_freq_dt)
imerg_jf_freq_dt[!is.finite(prec_freq), prec_freq := NA]


### for MAM

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(imerg_mam), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_mam <- tapp(imerg_mam, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_mam <- tapp(imerg_mam, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_mam_intensity <- total_precip_mam / precip_hours_mam

#plot(imerg_mam_intensity[[1]])

imerg_mam_int_dt <- as.data.frame(imerg_mam_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("mam"))

#summary(imerg_mam_int_dt)
#imerg_mam_intensity_dt[prec_int == Inf]
imerg_mam_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_mam <- length(indices)
imerg_mam_frequency <- (precip_hours_mam / total_available_hours) * 100

imerg_mam_freq_dt <- as.data.frame(imerg_mam_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg_mam")) %>%
  `[`(, season := factor("mam"))

#summary(imerg_mam_freq_dt)
imerg_mam_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(imerg_mam_freq_dt)

### for JJAS

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(imerg_jjas), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_jjas <- tapp(imerg_jjas, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_jjas <- tapp(imerg_jjas, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_jjas_intensity <- total_precip_jjas / precip_hours_jjas

#plot(imerg_jjas_intensity[[1]])

imerg_jjas_int_dt <- as.data.frame(imerg_jjas_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("jjas"))

#summary(imerg_jjas_int_dt)
#imerg_jjas_intensity_dt[prec_int == Inf]
imerg_jjas_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_jjas <- length(indices)
imerg_jjas_frequency <- (precip_hours_jjas / total_available_hours) * 100

imerg_jjas_freq_dt <- as.data.frame(imerg_jjas_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg_jjas")) %>%
  `[`(, season := factor("jjas"))

#summary(imerg_jjas_freq_dt)
imerg_jjas_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(imerg_jjas_freq_dt)


### for OND

#get the date from the names of the layers and extract the month
indices <- format(as.POSIXct(time(imerg_ond), format = "X%Y.%m.%d.%H.%M.%S"), format = "%H")
indices <- as.numeric(indices)

### Intensity
total_precip_ond <- tapp(imerg_ond, indices, fun = sum)

# Calculate the precipitation hours using stackApply()
precip_hours_ond <- tapp(imerg_ond, indices, fun = function(x, na.rm =TRUE) sum(x > 0.1))

imerg_ond_intensity <- total_precip_ond / precip_hours_ond

#plot(imerg_ond_intensity[[1]])

imerg_ond_int_dt <- as.data.frame(imerg_ond_intensity, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_int") %>% 
  `[`(, name := factor("imerg"))%>%
  `[`(, season := factor("ond"))

#summary(imerg_ond_int_dt)
#imerg_ond_intensity_dt[prec_int == Inf]
imerg_ond_int_dt[!is.finite(prec_int), prec_int := NA]

### Frequency
# Calculate the total available hours
total_available_hours_ond <- length(indices)
imerg_ond_frequency <- (precip_hours_ond / total_available_hours) * 100

imerg_ond_freq_dt <- as.data.frame(imerg_ond_frequency, xy = TRUE) %>%
  as.data.table() %>%
  melt(., id.vars = c("x", "y"), variable.name = "date", value.name = "prec_freq") %>% 
  as.data.table() %>%
  `[`(, name := factor("imerg_ond")) %>%
  `[`(, season := factor("ond"))

#summary(imerg_ond_freq_dt)
imerg_ond_freq_dt[!is.finite(prec_freq), prec_freq := NA]
#summary(imerg_ond_freq_dt)


### comine everything to a lsit

imerg_allseason_freq_dt <- list(imerg_jf_freq_dt, imerg_mam_freq_dt, imerg_jjas_freq_dt, 
                                 imerg_ond_freq_dt)


imerg_allseason_int_dt <- list(imerg_jf_int_dt, imerg_mam_int_dt, imerg_jjas_int_dt, 
                                imerg_ond_int_dt)


### save as .RDS

saveRDS(imerg_allseason_int_dt, "./projects/kenya_example/data/output/imerg_allseason_int_dt.RDS")
saveRDS(imerg_allseason_freq_dt, "./projects/kenya_example/data/output/imerg_allseason_freq_dt.RDS")


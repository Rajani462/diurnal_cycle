
dat <- readRDS("./projects/kenya_example/data/output/diurnal_int_freq_thres_list.RDS")

dat[2]

int_cmo <-as.data.table(mean_int_freq_list[27])

summary(int_cmo)


cdo -seldate,2001-01-01T00:00:00,2001-01-02T23:59:59 cmorph_hour_kenya_2001_20.nc trial_cmorph_2001_jan_25day.nc


cdo -dhoursum -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' trial_cmorph_2001_jan_25day.nc trl_count_value_above_0.1.nc

# Step 2: Create a mask with valid values (1 for valid, 0 for missing)
cdo -dhoursum -expr,'valid_mask = cmorph >= 0' trial_cmorph_2001_jan_25day.nc trl_valid_mask.nc 

# Step 3: calculate the frequency in percentage
cdo -mulc,100 -div trl_count_value_above_0.1.nc trl_valid_mask.nc trl_freq_percentage.nc

#try to chain

cdo -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' trial_cmorph_2001_jan_25day.nc -dhoursum -expr,'valid_mask = cmorph >= 0' trial_cmorph_2001_jan_25day.nc trl_chain_freq_percentage.nc 


cdo_totdays <- brick("./projects/kenya_example/data/trl_chain_freq_percentage.nc")
cdo_totdays_dt <- as.data.frame(cdo_totdays, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
  `[`(, name := factor("cmorph"))

cdo_totdays_dt
cdo_totdays_dt[value > 1]

# cdo gec trial_cmorph_2001_jan_25day.nc 0.1 trl_wet_hours_mask.nc
# cdo timmean trl_wet_hours_mask.nc trl_frequency_wet_hours.nc


cdo_res <- brick("./projects/kenya_example/data/trl_count_value_above_0.1.nc")
#wet_mask <- brick("./projects/kenya_example/data/trl")

cdo_res_dt <- as.data.frame(cdo_res, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
  `[`(, name := factor("cmorph"))

cdo_res_dt[value >= 2]

cmo_day <- brick("./projects/kenya_example/data/trial_cmorph_2001_jan_25day.nc")
cmo_day_dt <- as.data.frame(cmo_day, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
  `[`(, name := factor("cmorph"))


cmo_day_dt
summary(cmo_day_dt)

cmo_day_dt[value >= 0.1]

# Assuming your data.table is named cmo_day_dt
cmo_day_dt$date <- substr(cmo_day_dt$date, 13, 14) %>% paste0(":00:00")

a <- cmo_day_dt[, .(count_value_above_0.1 = sum(value >= 0.1)), by = .(x, y, date)]
summary(cmo_day_dt)

a[count_value_above_0.1 >= 2]

cmo_day_dt[x == "35.875" & y == "-4.625"]


# intensity ---------------------------------------------------------------
cd projects/kenya_example/data/
#total precipitation/total no of rainy hours (rf >= 0.1mm)

#step 1: total no of rainy hours
cdo -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' persiann_hour_kenya_2001_20.nc trl_count_value_above_0.1.nc

# Step 2: Create a mask with valid values (1 for valid, 0 for missing)
cdo -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' persiann_hour_kenya_2001_20.nc trl_mask_above_0.1.nc

#step 3: Multiply the mask with original precip values
cdo -b 64 -dhoursum -mul persiann_hour_kenya_2001_20.nc trl_mask_above_0.1.nc trial_mask_cmorph_2001_jan_25day.nc


cdo -b 64 -dhoursum -setvrange,0.1,500 -expr,'precipitation_thresholded = precip' persiann_hour_kenya_2001_20.nc precipitation_greater_than_0.1.nc


#step 4: divide the total precip/total no of precip hours
cdo div trial_mask_cmorph_2001_jan_25day.nc trl_count_value_above_0.1.nc trl_intensiy.nc

### chain everything in one line


cdo -P 50 -div -dhoursum -mul persiann_hour_kenya_2001_20.nc -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' persiann_hour_kenya_2001_20.nc -dhoursum -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' persiann_hour_kenya_2001_20.nc trial_mask_cmorph_2001_jan_25day.nc
cdo -P 50 -div -dhoursum -setvrange,0.1,500 -expr,'precipitation_thresholded = precip' persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc trial_mask_cmorph_2001_jan_25day.nc

### verify in R
library(raster)
library(dplyr)
library(data.table)

cmo <- brick("./projects/kenya_example/data/trial_mask_cmorph_2001_jan_25day.nc")

cmo_day_dt <- as.data.frame(cmo, xy = TRUE) %>%
  as.data.table() %>%
  data.table::melt(., id.vars = c("x", "y"), variable.name = "date") %>% 
  `[`(, name := factor("cmorph"))


cmo_day_dt
summary(cmo_day_dt)


###verify
dat <- readRDS("./projects/kenya_example/data/output/diurnal_int_freq_thres_list.RDS")

dat[2] #cmorph
dat[20] #persiann

int_cmo <-rbindlist(dat[20])
summary(int_cmo)


cd ~/shared/data_downloads/input_data/seasonal

#Estimate the hourly mean precipitation from the entire time series, the output will be 
#24time steps or 24 raster layers

# code taken from: https://code.mpimet.mpg.de/boards/2/topics/6653

cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc hourly_mean_era5_glob_2001_20.nc #[2512.92s 244MB]
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_60ns_2001_20_grid025.nc hourly_mean_imerg_glob_2001_20.nc
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc hourly_mean_cmorph_glob_2001_20.nc
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc hourly_mean_persiann_glob_2001_20.nc
#Processed 119442816000 values from 1 variable over 172805 timesteps [2665.61s 430MB]
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_60ns_2015_20_grid_025.nc hourly_mean_gsmap_glob_2015_20.nc #[1971.30s 254MB]


### for seasonal JJA
cd ~/shared/data_downloads/input_data

cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/input_data/seasonal/era5_tp_mm_hourly_60ns_2001_2020_regrid_jja.nc hourly_mean_era5_glob_2001_20.nc #[2512.92s 244MB]
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_60ns_2001_20_grid025.nc hourly_mean_imergf_glob_2001_20.nc
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc hourly_mean_cmorph_glob_2001_20.nc
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc hourly_mean_persiann_glob_2001_20.nc
#Processed 119442816000 values from 1 variable over 172805 timesteps [2665.61s 430MB]
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_60ns_2015_20_grid_025.nc hourly_mean_gsmap_2001_15.nc #[1971.30s 254MB]

#computing dhourmean directly form the mainfile without extracting season separately
cd ~/shared/data_downloads/input_data/
cdo -b 32 -P 40 dhourmean -select,season=JJA ~/shared/data_downloads/input_data/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc seasonal/hourly_mean_era5_glob_2001_20_jja.nc #[3617.66s 224MB]
cdo -b 32 -P 40 dhourmean -select,season=DJF ~/shared/data_downloads/input_data/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc seasonal/hourly_mean_era5_glob_2001_20_djf.nc

cdo -b 32 -P 50 dhourmean -select,season=JJA ~/shared/data_downloads/input_data/imerg_f_hour_60ns_2001_20_grid025.nc seasonal/hourly_mean_imerg_glob_2001_20_jja.nc #[3562.26s 224MB]
cdo -b 32 -P 50 dhourmean -select,season=DJF ~/shared/data_downloads/input_data/imerg_f_hour_60ns_2001_20_grid025.nc seasonal/hourly_mean_imerg_glob_2001_20_djf.nc

cdo -b 32 -P 50 dhourmean -select,season=JJA ~/shared/data_downloads/input_data/cmorph_hour_2001_20_rotated.nc seasonal/hourly_mean_cmorph_glob_2001_20_jja.nc
cdo -b 32 -P 50 dhourmean -select,season=DJF ~/shared/data_downloads/input_data/cmorph_hour_2001_20_rotated.nc seasonal/hourly_mean_cmorph_glob_2001_20_djf.nc

cdo -b 32 -P 20 dhourmean -select,season=JJA ~/shared/data_downloads/input_data/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc seasonal/hourly_mean_persiann_glob_2001_20_jja.nc
cdo -b 32 -P 20 dhourmean -select,season=DJF ~/shared/data_downloads/input_data/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc seasonal/hourly_mean_persiann_glob_2001_20_djf.nc

cdo -b 32 -P 20 dhourmean -select,season=JJA ~/shared/data_downloads/input_data/gsmap_hour_60ns_2015_20_grid_025.nc seasonal/hourly_mean_gsmap_glob_2015_20_jja.nc
cdo -b 32 -P 20 dhourmean -select,season=DJF ~/shared/data_downloads/input_data/gsmap_hour_60ns_2015_20_grid_025.nc seasonal/hourly_mean_gsmap_glob_2015_20_djf.nc


#computing frequency directly form the mainfile without extracting season separately
cd ~/shared/data_downloads/input_data/




cdo -b 32 -copy infile outfile
# intensity

cdo -b F64 dhoursum cmorph_hour_kenya_2001_20.nc hourly_charact/trial_hourly_sum_cmorph_kenya_2001_20.nc
cdo -b F64 timsum -ifthen -gtc,0.1 cmorph_hour_kenya_2001_20.nc -settime,00:00:00 -selhour,0/23 cmorph_hour_kenya_2001_20.nc trial_cmorph_precip_hours_hourly.nc

cdo -b F64 -L mergetime $(for hour in {0..23}; do echo "-timsum -gtc,0.1 -selhour,$hour cmorph_hour_kenya_2001_20.nc"; done) tril_trial_precip_hours_hourly.nc
cdo -b F64 -L mergetime $(for hour in {0..23}; do echo "-timsum -gtc,0.1 -selhour,$hour persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc"; done) hourly_charact/tril_persian_precip_hours_hourly.nc



#cdo timmean persiann_ocean_hourly_2015_025_grid.nc mean_persiann.nc

## precipitation occurance (freq): ratio of rainfall counts to total (zero and positive rain rate) measurements

cdo -seldate,2001-01-01T00:00:00,2001-01-01T23:59:59 persiann_hour_kenya_2001_20_regrid.nc trial_persian_2001_jan_25day.nc
cdo dhourmean trial_persian_2001_jan_24.nc trial_hourmean_persian_2001_jan_24.nc


cdo -seldate,2001-01-01T00:00:00,2001-01-25T23:59:59 persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc trial_persian_2001_01_01.nc


#cdo setmissval,">500" trial_persian_2001_jan.nc trial_persian_2001_jan_missremov.nc
#cdo setctomiss,>500 trial_persian_2001_jan.nc trial_persian_2001_jan_missremov.nc

cdo setrtomiss,5.00e+02,9.9692e+37 trial_persian_2001_jan.nc trial_persian_2001_jan_missremov.nc
cdo dhourmean trial_persian_2001_jan_missremov.nc trial_hourmean_persian_2001_jan_missremov.nc

#cdo setctomiss,9.9692e+36 trial_persian_2001_jan.nc trial_persian_2001_jan_missremov.nc


#cdo setrtomiss,1e35,1e36 -gtc,1e35 trial_persian_2001_jan.nc trial_jan_masked_precipitation.nc
#cdo ifthenelse -gec,1e35 trial_persian_2001_jan.nc -setrtomiss,_FillValue trail_2001jan_masked_precipitation.nc


#But if -99 should be uses as missing value than:
cdo setctomiss,-99 persiann_hour_kenya_2001_20_regrid.nc trail_persian_missing.nc
cdo dhourmean trial_persian_2001_jan_missremov.nc trial_hourmean_persian_2001_jan_missremov.nc
cdo dhourmean trial_jan_masked_precipitation.nc trial_hourmean_jan_masked_precipitation.nc

##########
#copy the 60 NSpre-proceeded input file to one common folder
cp ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_60ns_2001_20_grid025.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_60ns_2015_20_grid_025.nc ~/shared/data_downloads/input_data/


#########estimate the frequency -----------------------
#step 1: count the number of days >= 0.1 mm for each hour of the day
#[4049.80s 510MB]
cdo -P 50 -dhoursum -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' ~/shared/data_downloads/input_data/cmorph_hour_2001_20_rotated.nc ~/rajani/diurnal_cycle/projects/main/data/cmorph_count_value_above_0.1.nc

# Step 2: Create a mask with valid values (1 for valid, 0 for missing)
cdo -P 50 -dhoursum -expr,'valid_mask = cmorph >= 0' ~/shared/data_downloads/input_data/cmorph_hour_2001_20_rotated.nc ~/rajani/diurnal_cycle/projects/main/data/cmorph_count_value_above_0.1.nc

# Step 3: calculate the frequency in percentage
cdo -mulc,100 -div trl_count_value_above_0.1.nc trl_valid_mask.nc trl_freq_percentage.nc

#chain everything into one-----(without creating intermediate files)
#cd ~/rajani/diurnal_cycle/projects/main/data
cd ~/shared/data_downloads/input_data
#[2604.97s 951MB]43min
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' cmorph_hour_2001_20_rotated.nc -dhoursum -expr,'valid_mask = cmorph >= 0' cmorph_hour_2001_20_rotated.nc ~/rajani/diurnal_cycle/projects/main/data/cmorph_freq_0.1_2001_20.nc
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precipitationCal >= 0.1 ? 1 : 0)' imerg_f_hour_60ns_2001_20_grid025.nc -dhoursum -expr,'valid_mask = precipitationCal >= 0' imerg_f_hour_60ns_2001_20_grid025.nc ~/rajani/diurnal_cycle/projects/main/data/imerg_freq_0.1_2001_20.nc
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (tp >= 0.1 ? 1 : 0)' era5_tp_mm_hourly_60ns_2001_2020_regrid.nc -dhoursum -expr,'valid_mask = tp >= 0' era5_tp_mm_hourly_60ns_2001_2020_regrid.nc ~/rajani/diurnal_cycle/projects/main/data/era5_freq_0.1_2001_20.nc
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (tp >= 0.1 ? 1 : 0)' gsmap_hour_60ns_2015_20_grid_025.nc -dhoursum -expr,'valid_mask = tp >= 0' gsmap_hour_60ns_2015_20_grid_025.nc ~/rajani/diurnal_cycle/projects/main/data/gsmap_freq_0.1_2001_15.nc
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc -dhoursum -expr,'valid_mask = precip >= 0' persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc ~/rajani/diurnal_cycle/projects/main/data/persiann_freq_0.1_2001_20.nc


#########estimate the Intensity -----------------------

cd projects/kenya_example/data/ #tried and verified the results for kenya
#total precipitation/total no of rainy hours (rf >= 0.1mm)

#step 1: total no of rainy hours
cdo -dhoursum -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' cmorph_hour_kenya_2001_20.nc trl_count_value_above_0.1.nc

# Step 2: Create a mask with valid values (1 for valid, 0 for missing)
cdo -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' cmorph_hour_kenya_2001_20.nc trl_mask_above_0.1.nc

#step 3: Multiply the mask with original precip values
cdo -b 64 -dhoursum -mul cmorph_hour_kenya_2001_20.nc trl_mask_above_0.1.nc trial_mask_cmorph_2001_jan_25day.nc

#step 4: divide the total precip/total no of precip hours
cdo div trial_mask_cmorph_2001_jan_25day.nc trl_count_value_above_0.1.nc trl_intensiy.nc

### chain everything in one line

cdo -P 50 -div -dhoursum -mul cmorph_hour_kenya_2001_20.nc -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' cmorph_hour_kenya_2001_20.nc -dhoursum -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' cmorph_hour_kenya_2001_20.nc trial_mask_cmorph_2001_jan_25day.nc


#for global datasets
#cd ~/rajani/diurnal_cycle/projects/main/data
cd ~/shared/data_downloads/input_data
#[2604.97s 951MB]43min
cdo -P 50 -div -dhoursum -mul cmorph_hour_2001_20_rotated.nc -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' cmorph_hour_2001_20_rotated.nc -dhoursum -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' cmorph_hour_2001_20_rotated.nc ~/rajani/diurnal_cycle/projects/main/data/hourly_int_cmorph_glob_0.1_2001_20.nc
cdo -P 50 -div -dhoursum -mul imerg_f_hour_60ns_2001_20_grid025.nc -expr,'count_value_above_0_1 = (precipitationCal >= 0.1 ? 1 : 0)' imerg_f_hour_60ns_2001_20_grid025.nc -dhoursum -expr,'count_value_above_0_1 = (precipitationCal >= 0.1 ? 1 : 0)' imerg_f_hour_60ns_2001_20_grid025.nc ~/rajani/diurnal_cycle/projects/main/data/hourly_int_imerg_glob_0.1_2001_20.nc #[7056.63s 1017MB]
cdo -P 50 -div -dhoursum -mul era5_tp_mm_hourly_60ns_2001_2020_regrid.nc -expr,'count_value_above_0_1 = (tp >= 0.1 ? 1 : 0)' era5_tp_mm_hourly_60ns_2001_2020_regrid.nc -dhoursum -expr,'count_value_above_0_1 = (tp >= 0.1 ? 1 : 0)' era5_tp_mm_hourly_60ns_2001_2020_regrid.nc ~/rajani/diurnal_cycle/projects/main/data/hourly_int_era5_glob_0.1_2001_20.nc
cdo -P 50 -div -dhoursum -mul gsmap_hour_60ns_2015_20_grid_025.nc -expr,'count_value_above_0_1 = (tp >= 0.1 ? 1 : 0)' gsmap_hour_60ns_2015_20_grid_025.nc -dhoursum -expr,'count_value_above_0_1 = (tp >= 0.1 ? 1 : 0)' gsmap_hour_60ns_2015_20_grid_025.nc ~/rajani/diurnal_cycle/projects/main/data/hourly_int_gsmap_glob_0.1_2015_20.nc
cdo -P 50 -div -dhoursum -mul persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc ~/rajani/diurnal_cycle/projects/main/data/hourly_int_persiann_glob_0.1_2001_20.nc

### tha above code create NA (0 * 0 = NA) so use another method (but the following method also cretes same results & NA's)
cd ~/shared/data_downloads/input_data

# [13168.77s 943MB]
cdo -P 50 -div -dhoursum -setvrange,0.1,500 -expr,'precipitation_thresholded = cmorph' cmorph_hour_2001_20_rotated.nc -dhoursum -expr,'count_value_above_0_1 = (cmorph >= 0.1 ? 1 : 0)' cmorph_hour_2001_20_rotated.nc ~/rajani/diurnal_cycle/projects/main/data/hourly_int_cmorph_glob_0.1_2001_20.nc
cdo -P 50 -div -dhoursum -setvrange,0.1,500 -expr,'precipitation_thresholded = precipitationCal' imerg_f_hour_60ns_2001_20_grid025.nc -dhoursum -expr,'count_value_above_0_1 = (precipitationCal >= 0.1 ? 1 : 0)' imerg_f_hour_60ns_2001_20_grid025.nc ~/rajani/diurnal_cycle/projects/main/data/hourly_int_imerg_glob_0.1_2001_20.nc
cdo -P 50 -div -dhoursum -setvrange,0.1,500 -expr,'precipitation_thresholded = tp' era5_tp_mm_hourly_60ns_2001_2020_regrid.nc -dhoursum -expr,'count_value_above_0_1 = (tp >= 0.1 ? 1 : 0)' era5_tp_mm_hourly_60ns_2001_2020_regrid.nc ~/rajani/diurnal_cycle/projects/main/data/hourly_int_era5_glob_0.1_2001_20.nc
cdo -P 50 -div -dhoursum -setvrange,0.1,500 -expr,'precipitation_thresholded = tp' gsmap_hour_60ns_2015_20_grid_025.nc -dhoursum -expr,'count_value_above_0_1 = (tp >= 0.1 ? 1 : 0)' gsmap_hour_60ns_2015_20_grid_025.nc ~/rajani/diurnal_cycle/projects/main/data/hourly_int_gsmap_glob_0.1_2015_20.nc
cdo -P 50 -div -dhoursum -setvrange,0.1,500 -expr,'precipitation_thresholded = precip' persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc ~/rajani/diurnal_cycle/projects/main/data/hourly_int_persiann_glob_0.1_2001_20.nc

###############################################

#Croping land and ocean 

#create a mask by assigning grid cells of ocean areas to missing value.
#Get the topography dataset on a 0.25x0.25 degree grid:
cdo -f nc -remapbil,~/rajani/diurnal_cycle/projects/main/data/hourly/hourly_freq_cmorph_glob_0.1_2001_20.nc -topo topo_ref.nc

# mask the ocean by setting the topo values between -8000 and 0 m to missing value and the topo data on land areas to 1.
cdo -expr,'topo = ((topo>=0.0)) ? 1.0 : topo/0.0' topo_ref.nc mask_ocean.nc

#mask our input dataset with the mask file from above by multiplying them simply.
cdo -mul mask_ocean.nc ~/rajani/diurnal_cycle/projects/main/data/hourly/hourly_freq_cmorph_glob_0.1_2001_20.nc hourly_freq_cmorph_ocn_0.1_2001_20.nc

#chain evrything into one step
#### For frequency (diurnal freq)---------
cd ~/rajani/diurnal_cycle/projects/main/data/hourly
### Mask ocean area (keep land) in one command line:
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_freq_cmorph_glob_0.1_2001_20.nc -topo hourly_freq_cmorph_glob_0.1_2001_20.nc hourly_freq_cmorph_land_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_freq_imerg_glob_0.1_2001_20.nc -topo hourly_freq_imerg_glob_0.1_2001_20.nc hourly_freq_imerg_land_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_freq_gsmap_glob_0.1_2015_20.nc -topo hourly_freq_gsmap_glob_0.1_2015_20.nc hourly_freq_gsmap_land_0.1_2015_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_freq_persiann_glob_0.1_2001_20.nc -topo hourly_freq_persiann_glob_0.1_2001_20.nc hourly_freq_persiann_land_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_freq_era5_glob_0.1_2001_20.nc -topo hourly_freq_era5_glob_0.1_2001_20.nc hourly_freq_era5_land_0.1_2001_20.nc

### Mask land area (keep ocen) in one command line:
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_freq_cmorph_glob_0.1_2001_20.nc -topo hourly_freq_cmorph_glob_0.1_2001_20.nc hourly_freq_cmorph_ocn_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_freq_imerg_glob_0.1_2001_20.nc -topo hourly_freq_imerg_glob_0.1_2001_20.nc hourly_freq_imerg_ocn_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_freq_gsmap_glob_0.1_2015_20.nc -topo hourly_freq_gsmap_glob_0.1_2015_20.nc hourly_freq_gsmap_ocn_0.1_2015_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_freq_persiann_glob_0.1_2001_20.nc -topo hourly_freq_persiann_glob_0.1_2001_20.nc hourly_freq_persiann_ocn_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_freq_era5_glob_0.1_2001_20.nc -topo hourly_freq_era5_glob_0.1_2001_20.nc hourly_freq_era5_ocn_0.1_2001_20.nc

#### For mean (diurnal mean precip)---------
cd ~/rajani/diurnal_cycle/projects/main/data
### Mask ocean area (keep land) in one command line:
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_mean_cmorph_glob_2001_20.nc -topo hourly_mean_cmorph_glob_2001_20.nc hourly_mean_cmorph_land_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_mean_imerg_glob_2001_20.nc -topo hourly_mean_imerg_glob_2001_20.nc hourly_mean_imerg_land_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_mean_gsmap_glob_2015_20.nc -topo hourly_mean_gsmap_glob_2015_20.nc hourly_mean_gsmap_land_2015_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_mean_persiann_glob_2001_20.nc -topo hourly_mean_persiann_glob_2001_20.nc hourly_mean_persiann_land_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_mean_era5_glob_2001_20.nc -topo hourly_mean_era5_glob_2001_20.nc hourly_mean_era5_land_2001_20.nc

### Mask land area (keep ocen) in one command line:
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_mean_cmorph_glob_2001_20.nc -topo hourly_mean_cmorph_glob_2001_20.nc hourly_mean_cmorph_ocn_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_mean_imerg_glob_2001_20.nc -topo hourly_mean_imerg_glob_2001_20.nc hourly_mean_imerg_ocn_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_mean_gsmap_glob_2015_20.nc -topo hourly_mean_gsmap_glob_2015_20.nc hourly_mean_gsmap_ocn_2015_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_mean_persiann_glob_2001_20.nc -topo hourly_mean_persiann_glob_2001_20.nc hourly_mean_persiann_ocn_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_mean_era5_glob_2001_20.nc -topo hourly_mean_era5_glob_2001_20.nc hourly_mean_era5_ocn_2001_20.nc


#### For Intensity (diurnal intensity)---------
cd ~/rajani/diurnal_cycle/projects/main/data
### Mask ocean area (keep land) in one command line:
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_int_cmorph_glob_0.1_2001_20.nc -topo hourly_int_cmorph_glob_0.1_2001_20.nc hourly_int_cmorph_land_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_int_imerg_glob_0.1_2001_20.nc -topo hourly_int_imerg_glob_0.1_2001_20.nc hourly_int_imerg_land_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_int_gsmap_glob_0.1_2015_20.nc -topo hourly_int_gsmap_glob_0.1_2015_20.nc hourly_int_gsmap_land_0.1_2015_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_int_persiann_glob_0.1_2001_20.nc -topo hourly_int_persiann_glob_0.1_2001_20.nc hourly_int_persiann_land_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo>0.0))?1.0:topo/0.0' -remapbil,hourly_int_era5_glob_0.1_2001_20.nc -topo hourly_int_era5_glob_0.1_2001_20.nc hourly_int_era5_land_0.1_2001_20.nc

### Mask land area (keep ocen) in one command line:
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_int_cmorph_glob_0.1_2001_20.nc -topo hourly_int_cmorph_glob_0.1_2001_20.nc hourly_int_cmorph_ocn_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_int_imerg_glob_0.1_2001_20.nc -topo hourly_int_imerg_glob_0.1_2001_20.nc hourly_int_imerg_ocn_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_int_gsmap_glob_0.1_2015_20.nc -topo hourly_int_gsmap_glob_0.1_2015_20.nc hourly_int_gsmap_ocn_0.1_2015_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_int_persiann_glob_0.1_2001_20.nc -topo hourly_int_persiann_glob_0.1_2001_20.nc hourly_int_persiann_ocn_0.1_2001_20.nc
cdo -f nc -mul -expr,'topo=((topo<0.0))?1.0:topo/0.0' -remapbil,hourly_int_era5_glob_0.1_2001_20.nc -topo hourly_int_era5_glob_0.1_2001_20.nc hourly_int_era5_ocn_0.1_2001_20.nc

#################################################################################################################


#!/bin/bash

for input_file in ~/shared/data_downloads/input_data/seasonal/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Define the output file path
    output_file="${file_name}_freq_0.1_2001_20.nc"
    
    # Run the cdo command
    cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' "$input_file" -dhoursum -expr,'valid_mask = precip >= 0' "$input_file" "$output_file"
done


### diurnal frequency--------------------------------------------------

#!/bin/bash

for input_file in ~/shared/data_downloads/input_data/seasonal/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Define the output file path
    output_file="hour_freq_${file_name}_.nc"
    
    # Run the cdo command
    cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' "$input_file" -dhoursum -expr,'valid_mask = precip >= 0' "$input_file" "$output_file"
done


### diurnal intensity----------------------------------------------------

#!/bin/bash

for input_file in ~/shared/data_downloads/input_data/seasonal/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Define the output file path
    output_file="hour_int_${file_name}.nc"
    
    # Run the cdo command
    cdo -P 40 -div -dhoursum -mul "$input_file" -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' "$input_file" -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' "$input_file" "$output_file"
done


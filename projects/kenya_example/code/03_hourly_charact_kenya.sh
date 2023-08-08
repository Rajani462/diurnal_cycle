
cd ~/rajani/diurnal_cycle/projects/kenya_example/data/

#estiamte the mean precipitation for 20 yeras (mm/hr) (output: 1 time step/1layer)
cdo timmean imerg_f_hour_kenya_2001_20_grid_025.nc mean_imerg_kenya_2001_20.nc
cdo timmean era5_hour_kenya_2001_20_regrid.nc mean_era5_kenya_2001_20.nc
cdo timmean cmorph_hour_kenya_2001_20.nc mean_cmorph_kenya_2001_20.nc
cdo timmean persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc mean_persiann_kenya_2001_20.nc
cdo timmean gsmap_hour_kenya_2015_20.nc mean_gsmap_kenya_2015_20.nc

#estimate the standard deviation among the datasets

cdo ensstd mean_imerg_kenya_2001_20.nc mean_era5_kenya_2001_20.nc mean_cmorph_kenya_2001_20.nc mean_persiann_kenya_2001_20.nc mean_gsmap_kenya_2015.nc hourly_charact/standard_dev_meanhour_kenya.nc


#Estimate the hourly mean precipitation from the entire time series, the output will be 
#24time steps or 24 raster layers

# code taken from: https://code.mpimet.mpg.de/boards/2/topics/6653

cd ~/rajani/diurnal_cycle/projects/kenya_example/data/
 
cdo dhourmean imerg_f_hour_kenya_2001_20_grid_025.nc hourly_charact/hourly_mean_imerg_kenya_2001_20.nc
cdo dhourmean era5_hour_kenya_2001_20_regrid.nc hourly_charact/hourly_mean_era5_kenya_2001_20.nc
cdo dhourmean cmorph_hour_kenya_2001_20.nc hourly_charact/hourly_mean_cmorph_kenya_2001_20.nc
cdo dhourmean persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc hourly_charact/hourly_mean_persiann_kenya_2001_20.nc
cdo dhourmean gsmap_hour_kenya_2015_20.nc hourly_charact/hourly_mean_gsmap_kenya_2015_20.nc


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



#####################################################################################

# dhourmean for different threshold
cd ~/rajani/diurnal_cycle/projects/kenya_example/data/


cdo gec,0.2 gsmap_hour_kenya_2015_20.nc trl_mask.nc
cdo mul gsmap_hour_kenya_2015_20.nc trl_mask.nc trl_masked_input_file.nc
cdo dhourmean trl_masked_input_file.nc gsmap_hourmean_precip_greater_than_0.2mm.nc



cdo -dhourmean -expr,'tp_gt_0_2mm = (tp > 0.2) * tp' gsmap_hour_kenya_2015_20.nc trl2_gsmap_hourmean_precip_greater_than_0.2mm.nc

#cdo -dhourmean -mul gsmap_hour_kenya_2015_20.nc <(cdo gec,0.2 gsmap_hour_kenya_2015_20.nc) trl_gsmap_hourmean_precip_greater_than_0.2mm.nc

#cdo -dhourmean -mul gsmap_hour_kenya_2015_20.nc <(cdo gec,0.2 gsmap_hour_kenya_2015_20.nc trl_mask.nc) trl2_gsmap_hourmean_precip_greater_than_0.2mm.nc

#cdo -dhourmean -mul gsmap_hour_kenya_2015_20.nc gec,0.2 gsmap_hour_kenya_2015_20.nc hourly_charact/gsmap_hourmean_precip_greater_than_0.2mm.nc

#cdo -dhourmean -ifthen -gec,0.2 gsmap_hour_kenya_2015_20.nc gsmap_hour_kenya_2015_20.nc trl_gsmap_hourmean_precip_greater_than_0.2mm.nc


cdo -dhourmean -mul gsmap_hour_kenya_2015_20.nc -gec,0.2 temp_file.nc
mv temp_file.nc hourly_charact/gsmap_hourmean_precip_greater_than_0.2mm.nc




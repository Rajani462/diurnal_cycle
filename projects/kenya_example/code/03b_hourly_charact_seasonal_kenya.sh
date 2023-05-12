
#Estimate the mean hourly precipitation from each datasets
# Average of annual precipitation in mm/hr (evarage hourly precip)

cd ~/rajani/diurnal_cycle/projects/kenya_example/data/
 
#extract seasonal data (try with JF)


cdo select,season=JF cmorph_hour_kenya_2001_20.nc cmorph_hour_JF_kenya_2001_20.nc
cdo select,season=JF persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc persiann_hour_JF_kenya_2001_20_regrid_misrmv_negt.nc


cdo dhourmean persiann_hour_JF_kenya_2001_20_regrid_misrmv_negt.nc hourly_charact/hourly_mean_persiann_JF_kenya_2001_20.nc

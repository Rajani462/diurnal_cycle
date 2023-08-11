

##########-------------------------------------------------------
#copy the 60 NSpre-proceeded input file to one common folder
cp ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_60ns_2001_20_grid025.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_60ns_2015_20_grid_025.nc ~/shared/data_downloads/input_data/


#######"-------------------------------------------------------"
#chnage the variable name to a common name (i.e., "precip")

cd ~/shared/data_downloads/input_data

cdo -b 32 -P 50 chname,cmorph,precip cmorph_tp_mm_60ns_2001_20_025_hourly_rotated.nc cmorph_tp_mm_60ns_2001_20_025_hourly_rotated2.nc
cdo -b 32 -P 50 chname,tp,precip era5_tp_mm_60ns_2001_20_025_hourly_regrid.nc era5_tp_mm_60ns_2001_20_025_hourly_regrid2.nc
cdo -b 32 -P 50 chname,precipitationCal,precip imergf_tp_mm_60ns_2001_20_025_hourly.nc imergf_tp_mm_60ns_2001_20_025_hourly2.nc





cd ~/rajani/diurnal_cycle/projects/main/data/

#estiamte the mean precipitation for 20 yeras (mm/hr) (output: 1 time step/1layer)
cdo timmean imerg_f_hour_kenya_2001_20_grid_025.nc mean_imerg_kenya_2001_20.nc
cdo timmean era5_hour_kenya_2001_20_regrid.nc mean_era5_kenya_2001_20.nc
cdo timmean cmorph_hour_kenya_2001_20.nc mean_cmorph_kenya_2001_20.nc
cdo timmean persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc mean_persiann_kenya_2001_20.nc
cdo timmean gsmap_hour_kenya_2015_20.nc mean_gsmap_kenya_2015_20.nc

#estimate the standard deviation among the datasets

cdo ensstd mean_imerg_kenya_2001_20.nc mean_era5_kenya_2001_20.nc mean_cmorph_kenya_2001_20.nc mean_persiann_kenya_2001_20.nc mean_gsmap_kenya_2015.nc hourly_charact/standard_dev_meanhour_kenya.nc



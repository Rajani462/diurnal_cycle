
#Estimate the mean hourly precipitation from each datasets
# Average of annual precipitation in mm/hr (evarage hourly precip)

cd ~/rajani/diurnal_cycle/projects/kenya_example/data/
 
cdo timmean imerg_f_hour_kenya_2001_20_grid_025.nc mean_imerg_hour_kenya_2001_20.nc
cdo timmean era5_hour_kenya_2001_21_regrid.nc mean_era5_hour_kenya_2001_21.nc
cdo timmean cmorph_hour_kenya_2015_20.nc mean_cmorph_hour_kenya_2015_20.nc
#cdo timmean persiann_ocean_hourly_2015_025_grid.nc mean_persiann.nc


# correct the ERA5 grid as as it's extent is different from the cmorph and imerg
#cdo remapcon,mean_cmorph_hour_kenya_2015_20.nc mean_era5_hour_kenya_2001_21.nc mean_era5_hour_kenya_2001_21_regrid.nc



#just etract 2 days of data
cdo -seldate,2001-05-01T00:00:00,2001-05-01T23:59:59 imerg_f_hour_kenya_2001_20_grid_025.nc trl_imerg_f_hour_kenya_2001_may_grid_025.nc

cdo transxy trl_imerg_f_hour_kenya_2001_may_grid_025.nc trl_trans_imerg_f_hour_kenya_2001_may_grid_025.nc

cdo griddes ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_200101.nc

cdo setgrid,grid_ithaca_modf.txt ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_200101.nc temp.nc

cdo griddes temp.nc

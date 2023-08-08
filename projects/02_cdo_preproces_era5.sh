
# correct the ERA5 grid as as it's extent is different from the cmorph and imerg
#cdo remapcon,mean_cmorph_hour_kenya_2015_20.nc mean_era5_hour_kenya_2001_21.nc mean_era5_hour_kenya_2001_21_regrid.nc

#[18252.94s 175MB]
cdo -P 50 sellonlatbox,-180,180,-60,60 -seldate,2001-01-01T00:00:00,2020-12-31T23:59:59 ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_2001_2021.nc ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020.nc

cdo -P 50 remapcon,~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020.nc ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc


# correct the ERA5 grid as as it's extent is different from the cmorph and imerg
#cdo remapcon,mean_cmorph_hour_kenya_2015_20.nc mean_era5_hour_kenya_2001_21.nc mean_era5_hour_kenya_2001_21_regrid.nc

#sellonlatbox,33.5,42,-5,5 -seldate,2001-01-01T00:00:00,2020-12-31T23:59:59 /home/rstudio/shared/data_downloads/ERA5/era5_tp_mm_hourly_2001_2021.nc era5_hour_kenya_2001_20.nc


#########for PERSIANN #[27633.12s 239MB]

cdo -P 50 -seldate,2001-01-01T00:00:00,2020-12-31T23:59:59 ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hour_2001_20.nc ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020.nc

## correct the PERSIANN grid to be similar with the cmorph and imerg
#[26553.29s 363MB]

cdo -P 50 remapcon,~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020.nc ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid.nc

## there are are few large values like and have to assign them missing before doing any analysis
#setting values between the range to miss [26553.29s 363MB]
cdo -P 50 setrtomiss,5.00e+02,inf ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid.nc ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid_misrmv.nc

#Processed 119442816000 values from 1 variable over 172805 timesteps [24967.97s 238MB]


cdo setrtomiss,-500,-0.0000000001 ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv.nc ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc
#Processed 119442816000 values from 1 variable over 172805 timesteps [25014.94s 230MB]
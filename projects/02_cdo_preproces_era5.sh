
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


###########################################################################################
##ERA5 without shifting 1 hour back

cd ~/shared/data_downloads/ERA5

cdo -b F32 -f nc2 -P 40 -mulc,1000 -selyear,2014/2020 adaptor.mars.internal-1665666192.2496276-1964-2-11cbe8c6-8fb2-40c3-974a-7d47fd9ceb2e.nc era5_tp_mm_hourly_2014_2020.nc

cdo -b F32 -f nc2  -P 40 -mulc,1000 adaptor.mars.internal-1665748822.224174-19510-10-fc223fbc-eff1-4323-a9a9-47b325dcda00.nc era5_tp_mm_hourly_2007_2013.nc

cdo -b F32 -f nc2  -P 40 -mulc,1000 adaptor.mars.internal-1665750425.9622443-31349-5-a4a5b123-96e0-4e5e-b07a-6ab5608eb68e.nc era5_tp_mm_hourly_2001_2006.nc

cdo -b F32 -f nc2  -P 40 -chname,tp,precip -remapcon,~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc -sellonlatbox,-180,180,-60,60 -mergetime era5_tp_mm_hourly_2001_2006.nc era5_tp_mm_hourly_2007_2013.nc era5_tp_mm_hourly_2014_2020.nc era5_tp_mm_hourly_2001_2020.nc
#[19726.23s 287MB]

#dhourmean, freq, and intencity

cdo -b F32 -P 40 dhourmean era5_tp_mm_hourly_60ns_025_2001_2020.nc hourly_mean_era5_2001_20.nc #[5655.40s 126MB]
cdo -b F32 -P 40 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' era5_tp_mm_hourly_60ns_025_2001_2020.nc -dhoursum -expr,'valid_mask = precip >= 0' era5_tp_mm_hourly_60ns_025_2001_2020.nc hourly_freq_era5_2001_20.nc
cdo -b F32 -P 40 -div -dhoursum -mul era5_tp_mm_hourly_60ns_025_2001_2020.nc -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' era5_tp_mm_hourly_60ns_025_2001_2020.nc -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' era5_tp_mm_hourly_60ns_025_2001_2020.nc hourly_int_era5_2001_20.nc

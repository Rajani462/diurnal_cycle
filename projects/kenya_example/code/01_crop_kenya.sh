
# Before running this file please run the cdo codes in:
#~/rajani/~/rajani/diurnal_cycle/projects/01_, 02_.. etc.


cdo sellonlatbox,33.5,42,-5,5 /home/rstudio/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_2001_2020.nc imerg_f_hour_kenya_2001_20.nc
#cdo sellonlatbox,33.5,42,-5,5 /home/rstudio/shared/data_downloads/ERA5/era5_tp_mm_hourly_2001_2021.nc era5_hour_kenya_2001_21.nc
cdo sellonlatbox,33.5,42,-5,5 -seldate,2001-01-01T00:00:00,2020-12-31T23:59:59 /home/rstudio/shared/data_downloads/ERA5/era5_tp_mm_hourly_2001_2021.nc era5_hour_kenya_2001_20.nc
cdo sellonlatbox,33.5,42,-5,5 /home/rstudio/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc ~/rajani/diurnal_cycle/projects/kenya_example/data/cmorph_hour_kenya_2001_20.nc
#cdo sellonlatbox,33.5,42,-5,5 /home/rstudio/shared/data_downloads/PERSIANN/PERSIAN_CDR/cmorph_hour_2001_20_rotated.nc ~/rajani/diurnal_cycle/projects/kenya_example/data/cmorph_hour_kenya_2001_20.nc
cdo sellonlatbox,33.5,42,-5,5 -seldate,2001-01-01T00:00:00,2020-12-31T23:59:59 /home/rstudio/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hour_2001_20.nc ~/rajani/diurnal_cycle/projects/kenya_example/data/persiann_hour_kenya_2001_20.nc

cdo sellonlatbox,33.5,42,-5,5 ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_2015_20_grid_025.nc ~/rajani/diurnal_cycle/projects/kenya_example/data/gsmap_hour_kenya_2015_20.nc


#crop by time if not performed on the above step 
cdo selyear,2001/2020 era5_hour_kenya_2001_21.nc era5_hour_kenya_2001_20.nc


cdo sellonlatbox,33.5,42,-5,5 ~/shared/data_downloads/GSMAP/trial/output.nc ~/rajani/diurnal_cycle/projects/kenya_example/data/gsmap.nc


#/home/rstudio/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2015_20.nc

#regrid imer from 0.1 to 0.25
cdo gridboxmean,2,2 imerg_f_hour_kenya_2001_20.nc imerg_f_hour_kenya_2001_20_grid02.nc

cdo -sellonlatbox,33.5,42,-5,5 ~/shared/data/obs/precip/raw/gpm-imerg_tp_mm_global_200006_202012_025_yearly.nc ref_kenya_imerg.nc
cdo remapcon,ref_kenya_imerg.nc imerg_f_hour_kenya_2001_20_grid02.nc imerg_f_hour_kenya_2001_20_grid_025.nc

#regrid gsmap from 0.1 to 0.25
cdo gridboxmean,2,2 gsmap_hour_kenya_2015.nc gsmap_hour_kenya_2015_grid02.nc
cdo remapcon,ref_kenya_imerg.nc gsmap_hour_kenya_2015_grid02.nc gsmap_hour_kenya_2015_grid_025.nc


## correct the ERA5 grid as as it's extent is different from the cmorph and imerg

cdo remapcon,cmorph_hour_kenya_2001_20.nc era5_hour_kenya_2001_20.nc era5_hour_kenya_2001_20_regrid.nc
cdo remapcon,cmorph_hour_kenya_2001_20.nc persiann_hour_kenya_2001_20.nc persiann_hour_kenya_2001_20_regrid.nc

## there are are few large values like and have to assign them missing before doing any analysis
#setting values between the range to miss
cdo setrtomiss,5.00e+02,9.9692e+37 persiann_hour_kenya_2001_20_regrid.nc persiann_hour_kenya_2001_20_regrid_misrmv.nc
cdo setrtomiss,-500,-0.0000000001 persiann_hour_kenya_2001_20_regrid_misrmv.nc persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc

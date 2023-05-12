
# Before running this file please run the cdo codes in:
#~/rajani/~/rajani/diurnal_cycle/projects/01_, 02_.. etc.



cdo sellonlatbox,33.5,42,-5,5 /home/rstudio/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_2001_2020.nc imerg_f_hour_kenya_2001_20.nc
cdo sellonlatbox,33.5,42,-5,5 /home/rstudio/shared/data_downloads/ERA5/era5_tp_mm_hourly_2001_2021.nc era5_hour_kenya_2001_21.nc
cdo sellonlatbox,33.5,42,-5,5 /home/rstudio/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2015_20_rotated.nc ~/rajani/diurnal_cycle/projects/kenya_example/data/cmorph_hour_kenya_2015_20.nc


/home/rstudio/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2015_20.nc

#regrid imer from 0.1 to 0.25
cdo gridboxmean,2,2 imerg_f_hour_kenya_2001_20.nc imerg_f_hour_kenya_2001_20_grid02.nc

cdo -sellonlatbox,33.5,42,-5,5 ~/shared/data/obs/precip/raw/gpm-imerg_tp_mm_global_200006_202012_025_yearly.nc ref_kenya_imerg.nc
cdo remapcon,ref_kenya_imerg.nc imerg_f_hour_kenya_2001_20_grid02.nc imerg_f_hour_kenya_2001_20_grid_025.nc


## correct the ERA5 grid as as it's extent is different from the cmorph and imerg

cdo remapcon,mean_cmorph_hour_kenya_2015_20.nc era5_hour_kenya_2001_21.nc era5_hour_kenya_2001_21_regrid.nc



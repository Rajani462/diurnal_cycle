

#timeout_duration=3600

cdo sellonlatbox,33.5,42,-5,5 /home/rstudio/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_2001_2020.nc imerg_f_hour_kenya_2001_20.nc

#regrid imer from 0.1 to 0.25
cdo -P 50 gridboxmean,2,2 ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_2001_2020.nc  ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_kenya_2001_20_grid02.nc

#regrid gsmap from 0.1 to 0.25 #time took [18678.52s 561MB]
cdo -P 50 remapcon,~/shared/data/obs/precip/raw/gpm-imerg_tp_mm_global_200006_202012_025_yearly.nc  ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_kenya_2001_20_grid02.nc  ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_2001_20_grid025.nc


#extract the 60NS region #[10414.72s 241MB]
cdo -P 50 sellonlatbox,-180,180,-60,60 ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_2001_20_grid025.nc ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_60ns_2001_20_grid025.nc





#cdo -P 50 sellonlatbox,-180,180,-60,60 -seldate,2001-01-01,2002-01-01 ~/shared/data/obs/precip/raw/gpm-imerg_tp_mm_global_200006_202012_025_yearly.nc ~/shared/data_downloads/IMERG_F_hourly/ref_imerg_glob_60ns_025_yearly.nc
#cdo -P 50 remapcon,~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020.nc ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc

#GSMAP

#extract the 60NS region #[3013.22s 152MB]
cdo -P 50 sellonlatbox,-180,180,-60,60 ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_2015_20_grid_025.nc ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_60ns_2015_20_grid_025.nc


#!/bin/bash
# Set the timeout duration in seconds (e.g., 3600 for 1 hour)
timeout_duration=3600


for year in {2015..2020}; do
    for month in {01..12}; do
        folder_path=~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/${year}_nc
        output_file=~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_${year}${month}.nc
        timeout "${timeout_duration}" cdo mergetime ${folder_path}/GPMMRG_MAP_$(printf "%02d" $((year % 100)))${month}*.nc ${output_file}
    done
done

# chmod +x 01_cdo_merge_gsmap.sh
# ./01_cdo_merge_gsmap.sh

# ###merge alll the nc file to one
cdo mergetime ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/*.nc ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_2015_20.nc
#cdo mergetime ~/shared/data_downloads/GSMAP/ftp_downloads/2016/2016_nc/*.nc ~/shared/data_downloads/GSMAP/ftp_downloads/2016/2016_nc/gsmap_hour_2016.nc
# 
# ### convert from 0.1 to 0.25 spatial resolution
# 
cdo gridboxmean,2,2 ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_2015_20.nc ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_2015_20_grid02.nc
cdo remapcon,~/shared/data/obs/precip/raw/gpm-imerg_tp_mm_global_200006_202012_025_yearly.nc ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_2015_20_grid02.nc ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_2015_20_grid_025.nc





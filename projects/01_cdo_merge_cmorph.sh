#!/bin/bash
# Set the timeout duration in seconds (e.g., 3600 for 1 hour)
timeout_duration=3600

for year in {2001..2021} ; do # year 2022 is incomplete
    for month in {01..12}; do 
        timeout "${timeout_duration}" cdo mergetime ~/shared/data_downloads/CMORPH_hourly_0.25degre/CMORPH_V1.0_ADJ_0.25deg-HLY_${year}${month}*.nc ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_${year}${month}.nc;
    done 
done



###merge alll the nc file to one
cdo mergetime ~/shared/data_downloads/cmorph_merged_hour/*.nc ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2015_20.nc

#rotate the files from extent: 0, 360, -60, 60  (xmin, xmax, ymin, ymax) -180 to 180
cdo sellonlatbox,-180,180,-60,60 ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2015_20.nc /home/rstudio/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2015_20_rotated.nc


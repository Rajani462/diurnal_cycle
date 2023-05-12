#!/bin/bash
# Set the timeout duration in seconds (e.g., 3600 for 1 hour)
timeout_duration=3600

for year in {2002..2020} ; do 
    for month in {01..12}; do 
        timeout "${timeout_duration}" cdo mergetime ~/shared/data_downloads/IMERGHH_F/3IMERG.${year}${month}*.nc4 ~/shared/data_downloads/IMERGHH_F_merged/imerg_f_hh_${year}${month}.nc;
    done 
done

#for year in {2001..2001} ; do 
#    for month in {09..12}; do 
#        timeout "${timeout_duration}" cdo mergetime ~/shared/data_downloads/IMERGHH_F/3IMERG.${year}${month}*.nc4 ~/shared/data_downloads/IMERGHH_F_merged/imerg_f_hh_${year}${month}.nc;
#    done 
#done
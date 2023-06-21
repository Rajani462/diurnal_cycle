#!/bin/bash
# Set the timeout duration in seconds (e.g., 3600 for 1 hour)
timeout_duration=3600

for year in {2001..2020} ; do 
    for month in {01..12}; do 
        timeout "${timeout_duration}" cdo -b F32 -shifttime,-15min -hourmean ~/shared/data_downloads/IMERGHH_F_merged/imerg_f_hh_${year}${month}*.nc ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_${year}${month}.nc;
    done 
done

#######################################################
#chmod +x 02_cdo_aggregate.sh
#./02_cdo_aggregate.sh
#### Take the hourly mean of imerg file and keep the time of the first time step

# for single file
#cdo -b F32 -shifttime,-15min -hourmean imerg_201508.nc hourly_imerg_201508.nc

# for all the nc files in month wise

#for nc_file in *.nc; do
#    cdo -b F32 -shifttime,-15min -hourmean $nc_file hourly_$nc_file;
#done


#timeout_duration=3600


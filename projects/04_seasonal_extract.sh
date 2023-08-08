#!/bin/bash
# Set the timeout duration in seconds (e.g., 3600 for 1 hour)
timeout_duration=3600


# Get a list of all the files with the pattern "gsmap_hour_60ns_*.nc"
file_list=~/shared/data_downloads/input_data/*.nc

for file in $file_list; do
    # Extract the three months JJA (June, July, August) from each file
    base_name=$(basename "$file")
    output_file="${base_name%.*}_JJA.nc"
    timeout "${timeout_duration}" cdo -b F32 -P 40 -select,season=JJA "$file" "~/shared/data_downloads/input_data/seasonal$output_file"
done
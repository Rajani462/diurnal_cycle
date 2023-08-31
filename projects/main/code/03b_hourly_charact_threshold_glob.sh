

#############for different threshold----------------------------------

####### for 0.5 mm/hr

### diurnal mean--------------------------------------------------

#!/bin/bash

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data
output_dir=~/shared/data_projects/diurnal_precip/processed

# Loop through the input files
for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="hourly_mean_${name_parts[0]}_0.5.nc"  
    
    # Run the cdo command
    cdo -b 32 -P 50 -dhourmean -expr,'count_value_above_0_5 = (precip >= 0.5 ? precip : 0)' "$input_file" "${output_dir}/$output_file"
done


### diurnal frequency--------------------------------------------------

#!/bin/bash

input_dir=~/shared/data_projects/diurnal_precip/input_data
output_dir=~/shared/data_projects/diurnal_precip/processed

# Loop through the input files
for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    #output_file="hourly_freq_${name_parts[0]}_${name_parts[-3]}_${name_parts[-2]}_${name_parts[-1]}_0.5.nc"
    output_file="hourly_freq_${name_parts[0]}_0.5.nc"            #example output file:hourly_freq_cmorph_0.5.nc
    
    # Run the cdo command
    cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.5 ? 1 : 0)' "$input_file" -dhoursum -expr,'valid_mask = precip >= 0' "$input_file" "${output_dir}/$output_file"
done


### diurnal intensity----------------------------------------------------

input_dir=~/shared/data_projects/diurnal_precip/input_data
output_dir=~/shared/data_projects/diurnal_precip/processed

# Loop through the input files
for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    #output_file="hourly_int_${name_parts[0]}_${name_parts[-3]}_0.5.nc" #example output file:hourly_int_cmorph_025_0.5.nc
    output_file="hourly_int_${name_parts[0]}_0.5.nc"                    #example output file:hourly_int_cmorph_0.5.nc
    
    
    # Run the cdo command
    cdo -P 43 -div -dhoursum -mul "$input_file" -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' "$input_file" -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' "$input_file" "${output_dir}/$output_file"
done

################################################



#try another method to avoid NA's in intensity (try with GSmap first) 
#https://code.mpimet.mpg.de/boards/1/topics/8549

cd ~/shared/data_downloads/input_data

cdo -P 43 -div -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? precip : 0)' gsmap_tp_mm_60ns_2001_15_025_hourly.nc -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' gsmap_tp_mm_60ns_2001_15_025_hourly.nc hourly_int_gsmap_tp_mm_60ns_2015_20_025.nc
#[2673.22s 505MB]
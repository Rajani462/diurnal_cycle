####: extract the seaons and save them into separe Netcdf file for future use


#!/bin/bash

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data
output_dir=~/shared/data_projects/diurnal_precip/input_data/seasonal

# Loop through the input files
for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    output_file="${output_dir}/${file_name}" # Adjust the output file name here

    # Run the cdo command for JJA
    cdo -b F32 -P 40 -select,season=JJA "$input_file" "${output_file}_jja.nc"

    # Run the cdo command for DJF
    cdo -b F32 -P 40 -select,season=DJF "$input_file" "${output_file}_djf.nc"

    echo "Processed: ${input_file} -> ${output_file}_jja.nc, ${output_file}_djf.nc"
done


##########

### Now estimate the diurnal characteritics for each seasonal dataset-----------

### diurnal mean--------------------------------------------------
#!/bin/bash

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data/seasonal
output_dir=~/shared/data_projects/diurnal_precip/processed

# Loop through the input files
for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="hourly_mean_${name_parts[0]}_${name_parts[-1]}.nc"

    # Run the cdo command
    cdo -b 32 -P 50 -dhourmean "$input_file" "${output_dir}/$output_file"
done



### diurnal frequency--------------------------------------------------

#!/bin/bash

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data/seasonal
output_dir=~/shared/data_projects/diurnal_precip/processed

for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
     # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="hourly_freq_${name_parts[0]}_${name_parts[-1]}.nc"
    
    # Run the cdo command
    cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' "$input_file" -dhoursum -expr,'valid_mask = precip >= 0' "$input_file" "${output_dir}/$output_file"
done



### diurnal intensity----------------------------------------------------

#!/bin/bash
cd ~/shared/data_downloads/input_data/seasonal/hourly_character

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data/seasonal
output_dir=~/shared/data_projects/diurnal_precip/processed

for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
     # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="hourly_int_${name_parts[0]}_${name_parts[-1]}.nc"
    
    # Run the cdo command
    cdo -P 43 -div -dhoursum -mul "$input_file" -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' "$input_file" -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' "$input_file" "${output_dir}/$output_file"
done

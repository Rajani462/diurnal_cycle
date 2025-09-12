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


#foe any single file
cdo -b F32 -P 50 -select,season=JJA ~/shared/data_projects/diurnal_precip/input_data/imergv07_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc
cdo -b F32 -P 50 -select,season=DJF ~/shared/data_projects/diurnal_precip/input_data/imergv07_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc

##########

## Note: before running please check that all the input datasets have variable name 'precip' and in NetCDF2 format

### Latitude wise zonal mean--------------------------------------------------
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
    output_file="lat_mean_${name_parts[0]}_${name_parts[-1]}.nc"
    
    
    # Run the cdo command
    cdo -b 32 -P 50 -timmean -zonmean "$input_file" "${output_dir}/$output_file"
done


#for any single file
cdo -b 32 -P 50 -timmean -zonmean ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_imergv07_jja.nc
cdo -b 32 -P 50 -timmean -zonmean ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_imergv07_djf.nc


### seasonal and threshold of 0.1 mm/hr---------

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data/seasonal
output_dir=~/shared/data_projects/diurnal_precip/processed

# Loop through the input files
for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="lat_mean_${name_parts[0]}_${name_parts[-1]}_0.1.nc"
    
    
    # Run the cdo command
    cdo -b 32 -P 50 -timmean -zonmean -expr,'precip = (precip >= 0.1 ? precip : 0)' "$input_file" "${output_dir}/$output_file"
done

#for any single file
cdo -b 32 -P 50 -timmean -zonmean -expr,'precip = (precip >= 0.1 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_imergv07_jja_0.1.nc
cdo -b 32 -P 50 -timmean -zonmean -expr,'precip = (precip >= 0.1 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_imergv07_djf_0.1.nc


### seasonal and threshold of 0.2 mm/hr---------

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data/seasonal
output_dir=~/shared/data_projects/diurnal_precip/processed

# Loop through the input files
for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="lat_mean_${name_parts[0]}_${name_parts[-1]}_0.2.nc"
    
    
    # Run the cdo command
    cdo -b 32 -P 50 -timmean -zonmean -expr,'precip = (precip >= 0.2 ? precip : 0)' "$input_file" "${output_dir}/$output_file"
done


#for any single file
cdo -b 32 -P 50 -timmean -zonmean -expr,'precip = (precip >= 0.2 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_imergv07_jja_0.2.nc
cdo -b 32 -P 50 -timmean -zonmean -expr,'precip = (precip >= 0.2 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_imergv07_djf_0.2.nc


### seasonal and threshold of 0.5 mm/hr---------

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data/seasonal
output_dir=~/shared/data_projects/diurnal_precip/processed

# Loop through the input files
for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="lat_mean_${name_parts[0]}_${name_parts[-1]}_0.5.nc"
    
    
    # Run the cdo command
    cdo -b 32 -P 50 -timmean -zonmean -expr,'precip = (precip >= 0.5 ? precip : 0)' "$input_file" "${output_dir}/$output_file"
done


#for any single file
cdo -b 32 -P 50 -timmean -zonmean -expr,'precip = (precip >= 0.5 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_imergv07_jja_0.5.nc
cdo -b 32 -P 50 -timmean -zonmean -expr,'precip = (precip >= 0.5 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_imergv07_djf_0.5.nc


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


#for any single file
cdo -b 32 -P 50 -dhourmean ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/hourly_mean_imergv07_jja.nc
cdo -b 32 -P 50 -dhourmean ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/hourly_mean_imergv07_djf.nc


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

#for any single file
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc -dhoursum -expr,'valid_mask = precip >= 0' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/hourly_freq_imergv07_jja.nc
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc -dhoursum -expr,'valid_mask = precip >= 0' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/hourly_freq_imergv07_djf.nc


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

#for any single file
cdo -P 43 -div -dhoursum -mul ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/hourly_int_imergv07_jja.nc
cdo -P 43 -div -dhoursum -mul ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/hourly_int_imergv07_djf.nc

########################################################


##  for different season and threshold------------

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
    output_file="hourly_mean_${name_parts[0]}_${name_parts[-1]}_0.5.nc"

    # Run the cdo command
        cdo -b 32 -P 50 -dhourmean -expr,'count_value_above_0_5 = (precip >= 0.5 ? precip : 0)' "$input_file" "${output_dir}/$output_file"
done

#for any single file
cdo -b 32 -P 50 -dhourmean -expr,'count_value_above_0_5 = (precip >= 0.5 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/hourly_mean_imergv07_jja_0.5.nc
cdo -b 32 -P 50 -dhourmean -expr,'count_value_above_0_5 = (precip >= 0.5 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/hourly_mean_imergv07_djf_0.5.nc

# for 0.2 mm/hr
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
    output_file="hourly_mean_${name_parts[0]}_${name_parts[-1]}_0.2.nc"

    # Run the cdo command
        cdo -b 32 -P 50 -dhourmean -expr,'count_value_above_0_2 = (precip >= 0.2 ? precip : 0)' "$input_file" "${output_dir}/$output_file"
done

#for any single file
cdo -b 32 -P 50 -dhourmean -expr,'count_value_above_0_2 = (precip >= 0.2 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/hourly_mean_imergv07_jja_0.2.nc
cdo -b 32 -P 50 -dhourmean -expr,'count_value_above_0_2 = (precip >= 0.2 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/hourly_mean_imergv07_djf_0.2.nc


# for 0.1 mm/hr
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
    output_file="hourly_mean_${name_parts[0]}_${name_parts[-1]}_0.1.nc"

    # Run the cdo command
        cdo -b 32 -P 50 -dhourmean -expr,'count_value_above_0_1 = (precip >= 0.1 ? precip : 0)' "$input_file" "${output_dir}/$output_file"
done

#for any single file
cdo -b 32 -P 50 -dhourmean -expr,'count_value_above_0_1 = (precip >= 0.1 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/hourly_mean_imergv07_jja_0.1.nc
cdo -b 32 -P 50 -dhourmean -expr,'count_value_above_0_1 = (precip >= 0.1 ? precip : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/hourly_mean_imergv07_djf_0.1.nc


### diurnal frequency--------------------------------------------------

### for 0.2 mm/hr

#!/bin/bash

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data/seasonal
output_dir=~/shared/data_projects/diurnal_precip/processed

for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
     # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="hourly_freq_${name_parts[0]}_${name_parts[-1]}_0.2.nc"
    
    # Run the cdo command
    cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_2 = (precip >= 0.2 ? 1 : 0)' "$input_file" -dhoursum -expr,'valid_mask = precip >= 0' "$input_file" "${output_dir}/$output_file"
done


#for any single file
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_2 = (precip >= 0.2 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc -dhoursum -expr,'valid_mask = precip >= 0' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/hourly_freq_imergv07_jja_0.2.nc
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_2 = (precip >= 0.2 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc -dhoursum -expr,'valid_mask = precip >= 0' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/hourly_freq_imergv07_djf_0.2.nc


### for 0.5 mm/hr

#!/bin/bash

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data/seasonal
output_dir=~/shared/data_projects/diurnal_precip/processed

for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
     # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="hourly_freq_${name_parts[0]}_${name_parts[-1]}_0.5.nc"
    
    # Run the cdo command
    cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' "$input_file" -dhoursum -expr,'valid_mask = precip >= 0' "$input_file" "${output_dir}/$output_file"
done

#for any single file
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc -dhoursum -expr,'valid_mask = precip >= 0' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/hourly_freq_imergv07_jja_0.5.nc
cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc -dhoursum -expr,'valid_mask = precip >= 0' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/hourly_freq_imergv07_djf_0.5.nc


### diurnal intensity----------------------------------------------------

##for 0.2 mm/hr-----

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
    output_file="hourly_int_${name_parts[0]}_${name_parts[-1]}_0.2.nc"
    
    # Run the cdo command
    cdo -P 43 -div -dhoursum -mul "$input_file" -expr,'count_value_above_0_2 = (precip >= 0.2 ? 1 : 0)' "$input_file" -dhoursum -expr,'count_value_above_0_2 = (precip >= 0.2 ? 1 : 0)' "$input_file" "${output_dir}/$output_file" 
done

#for any single file
cdo -P 43 -div -dhoursum -mul ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc -expr,'count_value_above_0_2 = (precip >= 0.2 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc -dhoursum -expr,'count_value_above_0_2 = (precip >= 0.2 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/hourly_int_imergv07_jja_0.2.nc 
cdo -P 43 -div -dhoursum -mul ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc -expr,'count_value_above_0_2 = (precip >= 0.2 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc -dhoursum -expr,'count_value_above_0_2 = (precip >= 0.2 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/hourly_int_imergv07_djf_0.2.nc 


##for 0.5 mm/hr-----

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
    output_file="hourly_int_${name_parts[0]}_${name_parts[-1]}_0.5.nc"
    
    # Run the cdo command
    cdo -P 43 -div -dhoursum -mul "$input_file" -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' "$input_file" -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' "$input_file" "${output_dir}/$output_file" 
done

#for any single file
cdo -P 43 -div -dhoursum -mul ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_jja.nc ~/shared/data_projects/diurnal_precip/processed/hourly_int_imergv07_jja_0.5.nc 
cdo -P 43 -div -dhoursum -mul ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc -dhoursum -expr,'count_value_above_0_5 = (precip >= 0.5 ? 1 : 0)' ~/shared/data_projects/diurnal_precip/input_data/seasonal/imergv07_tp_mm_60ns_2001_20_025_hourly_djf.nc ~/shared/data_projects/diurnal_precip/processed/hourly_int_imergv07_djf_0.5.nc 

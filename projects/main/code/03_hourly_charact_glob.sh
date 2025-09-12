## Note: before running please check that all the input datasets have variable name 'precip' and in NetCDF2 format

### Latitude wise zonal mean--------------------------------------------------
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
    output_file="lat_mean_${name_parts[0]}.nc"
    
    
    # Run the cdo command
    cdo -b 32 -P 50 -timmean -zonmean "$input_file" "${output_dir}/$output_file"
done

#for any single data
cdo -b 32 -P 50 -timmean -zonmean ~/shared/data_projects/diurnal_precip/input_data/cmorph ~/shared/data_projects/diurnal_precip/processed/lat_mean_cmorph.nc
cdo -b 32 -P 50 -timmean -zonmean ~/shared/data_projects/diurnal_precip/input_data/gsmap_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_gsmap.nc
cdo -b 32 -P 50 -timmean -zonmean ~/shared/data_projects/diurnal_precip/input_data/imergv07_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_imergv07.nc

### Latitude wise zonal mean hourly --------------------------------------------------
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
    output_file="lat_mean_hourly_${name_parts[0]}.nc"
    
    
    # Run the cdo command
    cdo -b 32 -P 50 -dhourmean -zonmean "$input_file" "${output_dir}/$output_file"
done

#for any single data
cdo -b 32 -P 50 -dhourmean -zonmean ~/shared/data_projects/diurnal_precip/input_data/imergv07_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/processed/lat_mean_hourly_imergv07.nc
##################################################################

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
    output_file="hourly_mean_${name_parts[0]}.nc"
    
    
    # Run the cdo command
    cdo -b 32 -P 50 -dhourmean "$input_file" "${output_dir}/$output_file"
done


#for any single file
cdo -b 32 -P 50 -dhourmean ~/shared/data_projects/diurnal_precip/input_data/gsmap_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/processed/hourly_mean_gsmap.nc
cdo -b 32 -P 50 -dhourmean ~/shared/data_projects/diurnal_precip/input_data/imergv07_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/processed/hourly_mean_imergv07.nc

### diurnal frequency--------------------------------------------------

#!/bin/bash

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data
output_dir=~/shared/data_projects/diurnal_precip/processed

for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="hourly_freq_${name_parts[0]}.nc"
    
    # Run the cdo command
    cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' "$input_file" -dhoursum -expr,'valid_mask = precip >= 0' "$input_file" "${output_dir}/$output_file"
done



###for any signle dataset
cd ~/shared/data_projects/diurnal_precip/input_data
cdo -P 40 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' gsmap_tp_mm_60ns_2001_20_025_hourly.nc -dhoursum -expr,'valid_mask = precip >= 0' gsmap_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/processed/hourly_freq_gsmap.nc 
#[3865.45s 618MB]

cd ~/shared/data_projects/diurnal_precip/input_data
cdo -P 40 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' imergv07_tp_mm_60ns_2001_20_025_hourly.nc -dhoursum -expr,'valid_mask = precip >= 0' imergv07_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/processed/hourly_freq_imergv07.nc 
#


### diurnal intensity----------------------------------------------------

C!/bin/bash
cd ~/shared/data_downloads/input_data/seasonal/hourly_character

# Define input and output directories
input_dir=~/shared/data_projects/diurnal_precip/input_data
output_dir=~/shared/data_projects/diurnal_precip/processed

for input_file in "$input_dir"/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Extract the relevant parts of the file name
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="hourly_int_${name_parts[0]}.nc"
    
    # Run the cdo command
    cdo -P 43 -div -dhoursum -mul "$input_file" -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' "$input_file" -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' "$input_file" "${output_dir}/$output_file"
done


### for any signle dataset
cd ~/shared/data_projects/diurnal_precip/input_data
cdo -P 43 -div -dhoursum -mul gsmap_tp_mm_60ns_2001_20_025_hourly.nc -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' gsmap_tp_mm_60ns_2001_20_025_hourly.nc -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' gsmap_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/processed/hourly_int_gsmap.nc 
#[3959.05s 696MB]
cdo -P 43 -div -dhoursum -mul imerg_tp_mm_60ns_2001_20_025_hourly.nc -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' imerg_tp_mm_60ns_2001_20_025_hourly.nc -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' imerg_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/processed/hourly_int_imerg.nc 
#[4535.67s 977MB]

cdo -P 43 -div -dhoursum -mul imergv07_tp_mm_60ns_2001_20_025_hourly.nc -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' imergv07_tp_mm_60ns_2001_20_025_hourly.nc -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' imergv07_tp_mm_60ns_2001_20_025_hourly.nc ~/shared/data_projects/diurnal_precip/processed/hourly_int_imergv07.nc 


####################################################

#transfer/copy the datasets to metacentrum.cz(storage-brono-2)

scp -r /home/rstudio/shared/data_projects/diurnal_precip/input_data/imerg_tp_mm_60ns_2001_20_025_hourly.nc rajani_hpc_462@storage-brno2.metacentrum.cz:~/project/data/raw/
scp -r /home/rstudio/shared/data_projects/diurnal_precip/input_data/gsmap_tp_mm_60ns_2001_20_025_hourly.nc rajani_hpc_462@storage-brno2.metacentrum.cz:~/project/data/raw/

scp -r ~/shared/data_downloads/GSMAP rajani_hpc_462@storage-brno2.metacentrum.cz:~/project/data/raw/
scp -r ~/shared/data_downloads/IMERGHH_F_merged/ rajani_hpc_462@storage-brno2.metacentrum.cz:~/project/data/raw/IMERGHH_F_merged/
rsync -avh --ignore-existing ~/shared/data_downloads/IMERGHH_F_merged/ rajani_hpc_462@storage-brno2.metacentrum.cz:~/project/data/raw/IMERGHH_F_merged/
#also check if files complete or not
rsync -avh --ignore-existing --checksum ~/shared/data_downloads/IMERGHH_F_merged/ rajani_hpc_462@storage-brno2.metacentrum.cz:~/project/data/raw/IMERGHH_F_merged/

Metacentrum@462
#save on plzen1 (imerg hourly global 20 years)

scp -r /home/rstudio/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_2001_2020.nc rajani_hpc_462@storage-plzen1.metacentrum.cz:

Metacentrum@462
scp -r ~/shared/data_downloads/IMERGHH_F_merged/imerg_f_hh_200106.nc rajani_hpc_462@storage-vestec1-elixir.metacentrum.cz:~



#################################################################################################
# Extra trail and experiments-----------

cdo -b 32 -timmean -zonmean ~/shared/data_projects/diurnal_precip/processed/hourly_mean_imerg.nc ~/shared/data_projects/diurnal_precip/processed/trail_zonmean_imerg.nc

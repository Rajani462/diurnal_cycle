#!/bin/bash

# Define the directory where the input files are located
input_directory="~/rajani/diurnal_cycle/projects/main/data/seasonal"

# Loop through each JJA file in the input directory
for file in $(find "$input_directory" -type f -name "*jja.nc"); do
    # Extract the filename without extension
    filename=$(basename "${file%.*}")

    # Determine the variable name based on the input file name
    if [[ $filename == *"cmorph"* ]]; then
        variable="cmorph"
    elif [[ $filename == *"imerg"* ]]; then
        variable="precipitationCal"
    elif [[ $filename == *"era5"* || $filename == *"gsmap"* ]]; then
        variable="tp"
    elif [[ $filename == *"persiann"* ]]; then
        variable="precip"
    else
        echo "Unsupported filename format: $filename"
        continue
    fi

    # Create the output filename with 'freq_' prefix
    output_filename="freq_${filename}.nc"

    # Perform the cdo operations and save the output with the new filename
    cdo -P 50 -mulc,100 -div -dhoursum -expr,"count_value_above_0_1 = ($variable >= 0.1 ? 1 : 0)" "$file" -dhoursum -expr,"valid_mask = $variable >= 0" "$file" "$output_filename"
done

# Loop through each DJF file in the input directory
for file in $(find "$input_directory" -type f -name "*djf.nc"); do
    # Extract the filename without extension
    filename=$(basename "${file%.*}")

    # Determine the variable name based on the input file name
    if [[ $filename == *"cmorph"* ]]; then
        variable="cmorph"
    elif [[ $filename == *"imerg"* ]]; then
        variable="precipitationCal"
    elif [[ $filename == *"era5"* || $filename == *"gsmap"* ]]; then
        variable="tp"
    elif [[ $filename == *"persiann"* ]]; then
        variable="precip"
    else
        echo "Unsupported filename format: $filename"
        continue
    fi

    # Create the output filename with 'freq_' prefix
    output_filename="freq_${filename}.nc"

    # Perform the cdo operations and save the output with the new filename
    cdo -P 50 -mulc,100 -div -dhoursum -expr,"count_value_above_0_1 = ($variable >= 0.1 ? 1 : 0)" "$file" -dhoursum -expr,"valid_mask = $variable >= 0" "$file" "$output_filename"
done

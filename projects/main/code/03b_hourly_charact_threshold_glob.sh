
cd ~/rajani/diurnal_cycle/projects/main/data/threshold/


#1: Create a mask to filter out values below 0.2 mm:
cdo gec,0.2 input_file.nc mask.nc

#2: Multiply the original precipitation data with the mask:
cdo mul input_file.nc mask.nc masked_input_file.nc

#3 Calculate the time mean of the masked precipitation data:
cdo timmean masked_input_file.nc timemean_precipitation_greater_than_0.2mm.nc


cdo -b 32 -P 50 -dhourmean -expr,'tp_gt_0_2mm = (tp >= 0.2) * tp' ~/shared/data_downloads/input_data/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc hourly_mean_era5_glob_2001_20_greater_than_0.2mm.nc #[4348.63s 354MB]
cdo -b 32 -P 50 -dhourmean -expr,'tp_gt_0_5mm = (tp >= 0.5) * tp' ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc hourly_mean_era5_glob_2001_20_greater_than_0.5mm.nc #[3541.64s 354MB]

### without dhourmean


cdo -b 32 -P 50 -expr,'tp_gt_0_5mm = (tp >= 0.5) * tp' ~/shared/data_downloads/input_data/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc threshold/era5_tp_mm_hourly_60ns_2001_2020_regrid_greater_than_0.5mm.nc #[21248.29s 249MB]
cdo -b 32 -P 50 -expr,'tp_gt_0.2mm = (tp >= 0.2) * tp' ~/shared/data_downloads/input_data/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc threshold/era5_tp_mm_hourly_60ns_2001_2020_regrid_greater_than_0.2mm.nc
cdo -b 32 -P 50 -expr,'tp_gt_0.2mm = (tp >= 0.1) * tp' ~/shared/data_downloads/input_data/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc threshold/era5_tp_mm_hourly_60ns_2001_2020_regrid_greater_than_0.1mm.nc








cdo gec,0.2 input_file.nc -setmisstoc,0 -timmean -mul input_file.nc -gtc,1 output_timemean_precipitation_greater_than_1mm.nc









cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc hourly_mean_era5_glob_2001_20.nc #[2512.92s 244MB]
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_60ns_2001_20_grid025.nc hourly_mean_imergf_glob_2001_20.nc
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc hourly_mean_cmorph_glob_2001_20.nc
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc hourly_mean_persiann_glob_2001_20.nc
#Processed 119442816000 values from 1 variable over 172805 timesteps [2665.61s 430MB]
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_60ns_2015_20_grid_025.nc hourly_mean_gsmap_2001_15.nc #[1971.30s 254MB]


### for seasonal JJA
cd ~/shared/data_downloads/input_data/seasonal/

cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/input_data/seasonal/era5_tp_mm_hourly_60ns_2001_2020_regrid_jja.nc hourly_mean_era5_glob_2001_20.nc #[2512.92s 244MB]
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_60ns_2001_20_grid025.nc hourly_mean_imergf_glob_2001_20.nc
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc hourly_mean_cmorph_glob_2001_20.nc
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc hourly_mean_persiann_glob_2001_20.nc
#Processed 119442816000 values from 1 variable over 172805 timesteps [2665.61s 430MB]
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_60ns_2015_20_grid_025.nc hourly_mean_gsmap_2001_15.nc #[1971.30s 254MB]




cdo -seldate,2001-01-01T00:00:00,2001-12-31T23:59:59 ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc cmorph_hour_2001_rotated.nc
cdo -P 50 dhourmean cmorph_hour_2001_rotated.nc hourly_mean_cmorph_glob_2001.nc

#verifying africa
cdo sellonlatbox,25,40,15,30 cmorph_hour_2001_rotated.nc africa_cmorph_hour_2001_rotated.nc
cdo sellonlatbox,25,40,15,30 ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc africa_cmorph_hour_2001_20_rotated.nc

cdo -b 32 -P 50 dhourmean africa_cmorph_hour_2001_rotated.nc africa_hour_mean_cmorph_hour_2001_rotated.nc
cdo dhourmean africa_cmorph_hour_2001_rotated.nc africa_hour_mean_cmorph_hour_2001_rotated.nc


cdo -b 32 -copy infile outfile
# intensity

cdo -b F64 dhoursum cmorph_hour_kenya_2001_20.nc hourly_charact/trial_hourly_sum_cmorph_kenya_2001_20.nc
cdo -b F64 timsum -ifthen -gtc,0.1 cmorph_hour_kenya_2001_20.nc -settime,00:00:00 -selhour,0/23 cmorph_hour_kenya_2001_20.nc trial_cmorph_precip_hours_hourly.nc

cdo -b F64 -L mergetime $(for hour in {0..23}; do echo "-timsum -gtc,0.1 -selhour,$hour cmorph_hour_kenya_2001_20.nc"; done) tril_trial_precip_hours_hourly.nc
cdo -b F64 -L mergetime $(for hour in {0..23}; do echo "-timsum -gtc,0.1 -selhour,$hour persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc"; done) hourly_charact/tril_persian_precip_hours_hourly.nc



#cdo timmean persiann_ocean_hourly_2015_025_grid.nc mean_persiann.nc

## precipitation occurance (freq): ratio of rainfall counts to total (zero and positive rain rate) measurements

cdo -seldate,2001-01-01T00:00:00,2001-01-01T23:59:59 persiann_hour_kenya_2001_20_regrid.nc trial_persian_2001_jan_25day.nc
cdo dhourmean trial_persian_2001_jan_24.nc trial_hourmean_persian_2001_jan_24.nc


cdo -seldate,2001-01-01T00:00:00,2001-01-25T23:59:59 persiann_hour_kenya_2001_20_regrid_misrmv_negt.nc trial_persian_2001_01_01.nc


#cdo setmissval,">500" trial_persian_2001_jan.nc trial_persian_2001_jan_missremov.nc
#cdo setctomiss,>500 trial_persian_2001_jan.nc trial_persian_2001_jan_missremov.nc

cdo setrtomiss,5.00e+02,9.9692e+37 trial_persian_2001_jan.nc trial_persian_2001_jan_missremov.nc
cdo dhourmean trial_persian_2001_jan_missremov.nc trial_hourmean_persian_2001_jan_missremov.nc

#cdo setctomiss,9.9692e+36 trial_persian_2001_jan.nc trial_persian_2001_jan_missremov.nc


#cdo setrtomiss,1e35,1e36 -gtc,1e35 trial_persian_2001_jan.nc trial_jan_masked_precipitation.nc
#cdo ifthenelse -gec,1e35 trial_persian_2001_jan.nc -setrtomiss,_FillValue trail_2001jan_masked_precipitation.nc


#But if -99 should be uses as missing value than:
cdo setctomiss,-99 persiann_hour_kenya_2001_20_regrid.nc trail_persian_missing.nc
cdo dhourmean trial_persian_2001_jan_missremov.nc trial_hourmean_persian_2001_jan_missremov.nc
cdo dhourmean trial_jan_masked_precipitation.nc trial_hourmean_jan_masked_precipitation.nc

##########
#copy the 60 NSpre-proceeded input file one common folder
cp ~/shared/data_downloads/PERSIANN/PERSIANN_2001_22/persiann_hourly_60ns_2001_2020_regrid_regrid_misrmv_negt.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/ERA5/era5_tp_mm_hourly_60ns_2001_2020_regrid.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/cmorph_merged_hour/cmorph_hour_2001_20_rotated.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/IMERG_F_hourly/imerg_f_hour_60ns_2001_20_grid025.nc ~/shared/data_downloads/input_data/
cp ~/shared/data_downloads/GSMAP/ftp_downloads/netcdf/gsmap_hour_60ns_2015_20_grid_025.nc ~/shared/data_downloads/input_data/

# change the variable names to a common : "precip" among the datasets
cdo chname,SST,new_sst yi_sst.nc yi_sst_2.nc
cdo -b 32 -P 50 chname,cmorph,precip cmorph_tp_mm_60ns_2001_20_025_hourly_rotated.nc cmorph_tp_mm_60ns_2001_20_025_hourly_rotated.nc

############################

#############for different threshold----------------------------------

# for 0.5 mm/hr
### diurnal mean--------------------------------------------------
#!/bin/bash

cd ~/shared/data_downloads/input_data/seasonal/hourly_character (#it will be the output directory)

for input_file in ~/shared/data_downloads/input_data/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Define the output file path
    output_file="hourly_mean_${file_name}.nc"
    
    # Run the cdo command
    cdo -b 32 -P 50 dhourmean "$input_file" "$output_file"
done


### diurnal frequency--------------------------------------------------

#!/bin/bash

cd ~/shared/data_downloads/input_data/seasonal/hourly_character

for input_file in ~/shared/data_downloads/input_data/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Extract the relevant parts of the file name
    # Assuming the format is: cmorph_tp_mm_60ns_2001_20_025_hourly_rotated.nc
    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="hourly_freq_${name_parts[0]}_${name_parts[-3]}_${name_parts[-2]}_${name_parts[-1]}_0.5.nc"
    
    
    # Run the cdo command
    cdo -P 50 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.5 ? 1 : 0)' "$input_file" -dhoursum -expr,'valid_mask = precip >= 0' "$input_file" "$output_file"
done


### diurnal intensity----------------------------------------------------

#!/bin/bash
cd ~/shared/data_downloads/input_data/seasonal/hourly_character

for input_file in ~/shared/data_downloads/input_data/*.nc; do
    # Extract the file name without extension
    file_name=$(basename "$input_file" .nc)
    
    # Define the output file path
    # Extract the relevant parts of the file name
    # Assuming the format is: cmorph_tp_mm_60ns_2001_20_025_hourly_rotated.nc

    IFS="_" read -ra name_parts <<< "$file_name"
    output_file="hourly_freq_${name_parts[0]}_${name_parts[-3]}_${name_parts[-2]}_${name_parts[-1]}_0.5.nc"
    
    
    # Run the cdo command
    cdo -P 43 -div -dhoursum -mul "$input_file" -expr,'count_value_above_0_1 = (precip >= 0.5 ? 1 : 0)' "$input_file" -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.5 ? 1 : 0)' "$input_file" "$output_file"
done




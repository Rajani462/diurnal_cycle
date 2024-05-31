

###########################################################################################
##ERA5-land: https://confluence.ecmwf.int/pages/viewpage.action?pageId=168657911


cd ~/shared/data_downloads/ERA5_land
unzip '*.zip'

#will create a subdirectory for each zip file and extract its contents into that subdirectory, avoiding conflicts between files with the same name from different zip files
for zip_file in *.zip; do
    unzip "$zip_file" -d "${zip_file%.zip}"
done

#
find ~/shared/data_downloads/ERA5_land/ -name "*.nc" -exec cdo -b F32 -f nc2 -P 40 mergetime {} era5land_2015.nc \;

find ~/shared/data_downloads/ERA5_land/ -name "data.nc" -exec cdo -b F32 -f nc2 -P 40 mergetime {} merged_era5land_2015.nc \;


find . -type f -name "*.nc" -exec mv {} ~/shared/data_downloads/ERA5_land/ \;


cdo -b F32 -f nc2 -P 40 -mergetime *.nc era5_land_2015.nc


# Replace "input_file.nc" with the actual name of your ERA5-Land input file
# Replace "output_file.nc" with the desired name for the output file

cdo -daysum -shifttime,-1sec -mulc,1000 era5land_tp_hourly_ene-dic_1981-2010.nc era5land_tp-mm_daily_1981-2010.nc

# No, the 1 second is subtracted not added (shifttime,-1sec). The times will be shifted 1 second back in time, e.g. for 1981 January, 15th
# 
# time 15.01.1981 00:00:00 --> 14.01.1981 23:59:59
# time 15.01.1981 01:00:00 --> 15.01.1981 00:59:59


cdo -b F32 -P 40 -expr,'tp = hour == 1 ? tp * 1000 : (tp - tph) * 1000' ~/shared/data_downloads/ERA5_land/era5_land_2015.nc ~/shared/data_downloads/ERA5_land/era5_land_hourly_2015.nc

cdo -b F32 -P 40 -deltat ~/shared/data_downloads/ERA5_land/era5_land_2015.nc ~/shared/data_downloads/ERA5_land/era5_land_hourly_2015.nc


##now convert the units of precipitation and regrid to 0.25 from 0.1 and then estimate the diurnal hourly mean
cd ~/shared/data_downloads/ERA5_land/

cdo -b F32 -P 40 -remapcon,~/shared/data_downloads/input_data/gsmap_tp_mm_60ns_2001_15_025_hourly.nc -gridboxmean,2,2 -mulc,1000 ~/shared/data_downloads/ERA5_land/era5_land_2015.nc era5land_2015_025.nc #[1342.57s 206MB]
cdo -b F32 -P 40 -dhourmean ~/shared/data_downloads/ERA5_land/era5land_2015_025.nc ~/shared/data_downloads/ERA5_land/hourly_mean_era5land_glob_2015.nc #[1342.57s 206MB]


####################
cd ~/shared/data_downloads/ERA5_land/
cdo -b F32 -P 40 seltimestep,1/48 era5_land_2015.nc era5_land_201501_02jan.nc

#https://stackoverflow.com/questions/41244064/converting-an-accumulated-variable-to-timestep-values-in-a-netcdf-file-with-cdo
cdo -b F32 -P 40 -deltat era5_land_201501_02jan.nc era5_land_hourly_201501_02jan.nc

cdo mergetime -seltimestep,1 in.nc diff.nc diff_with_step1.nc 

cdo -b F32 -shifttime,-1sec era5_land_201501_02jan.nc era5_land_shift_201501_02jan.nc
cdo -b F32 -deltat era5_land_shift_201501_02jan.nc hourly_201501_02jan.nc

cdo -mergetime -seltimestep,1 -selyear,2015 era5_land_shift_201501_02jan.nc -deltat shift_201501_02jan.nc


#dhourmean, freq, and intencity

cdo -b F32 -P 40 dhourmean era5_tp_mm_hourly_60ns_025_2001_2020.nc hourly_mean_era5_2001_20.nc #[5655.40s 126MB]
cdo -b F32 -P 40 -mulc,100 -div -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' era5_tp_mm_hourly_60ns_025_2001_2020.nc -dhoursum -expr,'valid_mask = precip >= 0' era5_tp_mm_hourly_60ns_025_2001_2020.nc hourly_freq_era5_2001_20.nc
cdo -b F32 -P 40 -div -dhoursum -mul era5_tp_mm_hourly_60ns_025_2001_2020.nc -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' era5_tp_mm_hourly_60ns_025_2001_2020.nc -dhoursum -expr,'count_value_above_0_1 = (precip >= 0.1 ? 1 : 0)' era5_tp_mm_hourly_60ns_025_2001_2020.nc hourly_int_era5_2001_20.nc

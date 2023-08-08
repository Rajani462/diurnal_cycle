#!/bin/bash

for year in {2000..2019} ; do
  cdo mergetime EM_Earth_deterministic_daily_trange_${year}??.nc trange_${year}.nc
done 
cdo mergetime sstdas_????.nc sstdas_wholeseries.nc

# merging daily data 
for year in {2005..2019} ; do   
cdo mergetime shared/data/source/em-earth/deterministic_raw_daily/trange/EM_Earth_deterministic_daily_trange_${year}??.nc shared/data_projects/med_datasets/2000_2019_data/em_earth/trange_${year}.nc; 
done

# Daily to monthly calculation 
for year in {2000..2019} ; do    cdo monmean shared/data_projects/med_datasets/2000_2019_data/em_earth/trange_${year}.nc shared/data_projects/med_datasets/2000_2019_data/em_earth/trange_mon_${year}.nc ; done

#merging monthly data 

cdo mergetime shared/data_projects/med_datasets/2000_2019_data/em_earth/trange_mon_*.nc trange_mon_avg.nc

#Code to calculate Tmax and Tmin 
cdo setname,tmax -add  tavg/tmean_mon_avg.nc tmax_min/div2.nc tmax_min/tmax.nc

cdo setname,tmin -sub  tavg/tmean_mon_avg.nc tmax_min/div2.nc tmax_min/tmin.nc

# gridboxmean 
cdo -b F32 gridboxmean,2,2 tmax.nc tmax_0.2_grid.nc

#gridding to 0.25 degree resolution 
cdo remapcon,grid_ithaca.txt tmax_0.2_grid.nc tmax_0.25_grid.nc

#calculation of P-E 

files_1=*.nc
${files_1[3]}

###########rajani

#merge all the nc4 IMERG half hourly files and create yearwise nc files 

for year in {2002..2020} ; do 
    for month in {01..12}; do 
        cdo mergetime ~/shared/data_downloads/IMERGHH_F/3IMERG.${year}${month}*.nc4 ~/shared/data_downloads/IMERGHH_F_merged/imerg_f_hh_${year}${month}.nc;
    done 
done


# yearly not working (proceesses killed after few hours)
for year in {2001..2020} ; do   
cdo mergetime ~/shared/data_downloads/IMERGHH_F/3IMERG.${year}*.nc4 ~/shared/data_downloads/IMERGHH_F_merged/imerg_f_hh_${year}.nc; 
done

#monthly

for year in {2001..2020} ; do 
    for month in {01..12}; do 
        cdo mergetime ~/rajani/diurnal_cycle/data_files/3IMERG.${year}${month}*.nc4 ~/rajani/diurnal_cycle/data_files/imerg_f_hh_${year}${month}.nc;
    done 
done
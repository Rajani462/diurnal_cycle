
###to download use filezilla with the following credintials
# Address: hokusai.eorc.jaxa.jp
# UID: rainmap
# PW: Niskur+1404

########################

###unzip and repaqce the .dat.gz files with .dat (takes around 30 min/year)

root_folder="/home/rstudio/shared/data_downloads/GSMAP/2015"

# Find and unzip .dat.gz files in all subdirectories
find "$root_folder" -type f -name "*.dat.gz" -exec gunzip {} +

########################

#move all the datfiles into a single folder
cd /home/rstudio/shared/data_downloads/GSMAP/v08

#find command to locate all the .dat files in the subdirectories and move to the current directory
find /home/rstudio/shared/data_downloads/GSMAP/2015 -name "*.dat" -exec mv {} . \;

#cdo -f nc import_binary ~/shared/data_downloads/GSMAP/v08/GSMaP_MVK.hourly.rain.v8.ctl gsmap_gauge_2015.nc
cdo -b F32 -P 40 -f nc import_binary ~/shared/data_downloads/GSMAP/v08/GSMaP_MVK.hourly.rain.v8.ctl gsmap_gauge_2015.nc #[6014.34s 115MB]

#######################

#convert the longitude from 360 to -180 to +180

##regrid gsmap from 0.1 to 0.2
#cdo -P 50 gridboxmean,2,2 gsmap_gauge_corrcted_20150101.nc gsmap_gauge_corrcted_20150101_02.nc

##regrid gsmap from 0.2 to 0.25 #time took [18678.52s 561MB]
#cdo -P 50 remapcon,~/shared/data_downloads/input_data/gsmap_tp_mm_60ns_2001_15_025_hourly.nc gsmap_gauge_corrcted_20150101_02.nc gsmap_gauge_corrcted_20150101_025.nc

### chain evrytthing together
cdo -b F32 -f nc2 -P 43 -remapcon,~/shared/data_downloads/input_data/gsmap_tp_mm_60ns_2001_15_025_hourly.nc -gridboxmean,2,2 -sellonlatbox,-180,180,-60,60 gsmap_gauge_2015.nc gsmap_gauge_2015_025.nc
#[1432.75s 306MB]

#######################

#diurnal mean, freq, and int
cdo -b 32 -P 50 dhourmean ~/shared/data_downloads/GSMAP/v08/gsmap_gauge_2015_025.nc hourly_mean_gsmapnew_glob_2015.nc #[96.72s 182MB]

###############################################################################

### Step-1 : unzip all the dat.zip files and moveout them to their respective year folder --------------

#!/bin/bash

# Specify the range of years
start_year=2001
end_year=2020

# Loop through the years
for year in $(seq $start_year $end_year); do
    root_folder="/home/rstudio/shared/data_downloads/GSMAP/$year"

    # Find .dat.gz files and move them after extraction
    find "$root_folder" -type f -name "*.dat.gz" -exec sh -c '
        gunzip -c "$1" > "${1%.gz}"  # Extract the file
        mv "${1%.gz}" "$2"           # Move the extracted file to the year folder
    ' sh {} "$root_folder" \;

done



### Step-2: convert alll the unzipped dat files into nc format (need to have a .ctl file for each yaer in same folder)--------
#!/bin/bash

# Specify the range of years
start_year=2001
end_year=2020

# Loop through the years and convert using the respective .ctl file
for year in $(seq $start_year $end_year); do
    ctl_file="/home/rstudio/shared/data_downloads/GSMAP/$year/GSMaP_MVK.hourly.rain.v8_${year}.ctl"
    output_nc_file="/home/rstudio/shared/data_downloads/GSMAP/v08/gsmap_gauge_${year}.nc"

    # Convert binary files to NetCDF format using the respective .ctl file
    cdo -b F32 -P 40 -f nc import_binary "$ctl_file" "$output_nc_file"
done

cdo -b F32 -P 40 -f nc import_binary ~/shared/data_downloads/GSMAP/v08/GSMaP_MVK.hourly.rain.v8.ctl gsmap_gauge_2015.nc #[6014.34s 115MB]
#[6139.97s 111MB] for each year


### Step-3: convert the resulted nc files from 0 360 to -180 to 180----------------------

#!/bin/bash

# Specify the range of years
start_year=2001
end_year=2020

# Loop through the years
for year in $(seq $start_year $end_year); do
    input_nc_file="/home/rstudio/shared/data_downloads/GSMAP/v08/gsmap_gauge_${year}.nc"
    output_nc_file="/home/rstudio/shared/data_downloads/GSMAP/v08/gsmap_lon_180/gsmap_gauge_${year}.nc"
  
    cdo -b F32 -f nc2 -P 43 -sellonlatbox,-180,180,-60,60 "$input_nc_file" "$output_nc_file"
done



### Step-4: convert the resulted nc files from 0.1 to 0.25----------------------

#!/bin/bash

# Specify the range of years
start_year=2001
end_year=2014

# Loop through the years
for year in $(seq $start_year $end_year); do
    input_nc_file="/home/rstudio/shared/data_downloads/GSMAP/v08/gsmap_gauge_${year}.nc"
    output_nc_file="/home/rstudio/shared/data_downloads/GSMAP/v08/gsmap_gauge_${year}_025.nc"
    
    cdo -b F32 -f nc2 -P 43 -remapcon,~/shared/data_downloads/input_data/gsmap_tp_mm_60ns_2001_15_025_hourly.nc -gridboxmean,2,2 -sellonlatbox,-180,180,-60,60 "$input_nc_file" "$output_nc_file"
done

### Step-3: merge all the yearly nc files into a single nc file-------------

cdo -b F32 -f nc2 -P 43 -mergetime ~/shared/data_downloads/GSMAP/v08/gsmap_gauge_*_025.nc gsmapgauge_tp_mm_60ns_2015_20_025_hourly.nc

cdo -b F32 -f nc2 -P 43 -dhourmean ~/shared/data_downloads/GSMAP/gsmapgauge_tp_mm_60ns_2015_20_025_hourly.nc hourly_mean_gsmapgauge_tp_mm_60ns_2015_20_025_hourly.nc

###############################################################-----------------

#Important note: Since we (mijael) find downloaded and pre-processed the new GSMaP data, I'm decided to
#use this new dataset insted of the old one.

cdo -b F32 -f nc2 -P 43 -selyear,2001/2020 -sellonlatbox,-180,180,-60,60 -chname,tp,precip ~/shared/data/obs/precip/raw/gsmap-v8_tp_mm_global_199801_202306_025_hourly.nc ~/shared/data_projects/diurnal_precip/input_data/gsmap_tp_mm_60ns_2001_20_025_hourly.nc


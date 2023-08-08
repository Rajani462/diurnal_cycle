
cdo import_binary gsmap_gauge.20200101.0000.v6.4133.0.dat.gz output.nc


dat  <- raster("./projects/kenya_example/code/achieve/GPMMRG_MAP_2112010000_H_L3S_MCN_05A.nc")


cdo -seldate,2001-01-01T00:00:00,2001-01-02T23:59:59 /home/rstudio/shared/data_downloads/ERA5/era5_tp_mm_hourly_2001_2021.nc trial_era5_hour_kenya_2001_01.nc


cdo histfreq trial_era5_hour_kenya_2001_01.nc trial_era5_rain_frequency2001_01.nc

cdo selvar,tp input.nc -selname,tp -hist trial_era5_hour_kenya_2001_01.nc trial_era5_rain_frequency2001_01.nc

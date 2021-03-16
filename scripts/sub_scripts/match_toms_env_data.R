rm(list = ls())

library(readr)
library(dplyr)
library(rnaturalearth)
library(rgeos)
library(sf)
library(ggplot2)
library(stringr)
library(raster)
library(TDPanalysis)
library(colorRamps)
library(maps)

#############################################
### master fish & benthic REA data points ###
#############################################

survey <- read_csv(paste0("C:/Users/", Sys.info()[7], "/fish_benthic_env/data/SURVEY MASTER.csv"))

survey$LONGITUDE_LOV = ifelse(survey$LONGITUDE_LOV < 0, 
                              survey$LONGITUDE_LOV + 360, 
                              survey$LONGITUDE_LOV)

time_stamp = str_split_fixed(survey$DATE_, "/", 3)
colnames(time_stamp) = c("Month", "Day", "Year")

survey = cbind(time_stamp, survey)

survey_sub = survey[,c("Year", "Month", "Day", "LONGITUDE_LOV", "LATITUDE_LOV")]

survey_sub = survey_sub %>% 
  group_by(Year, Month, Day, LONGITUDE_LOV, LATITUDE_LOV) %>% 
  summarise(n = n())

survey_sub = survey_sub[complete.cases(survey_sub), ]
survey_sub$doy = paste0(survey_sub$Day, "/", survey_sub$Month, "/", survey_sub$Year)
survey_sub$doy = date.to.DOY(survey_sub$doy)

plot(survey_sub$LONGITUDE_LOV, survey_sub$LATITUDE_LOV, pch = ".", col = 4, cex = 10); map(resolution = 0, add = T, fill = T)

####################
#### TOM's Data ####
####################

setwd("M:/Environmental Data Summary/DataDownload/")

env_df = raster("Bathymetry_ETOPO1/Bathymetry_ETOPO1_Bathy_M_AllIslands.nc")
env_df = raster("Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands_Long360.nc")

#Chlorophyll_A_ESAOCCCI_Clim
env_df = raster("Chlorophyll_A_ESAOCCCI_Clim/Chlorophyll_A_ESAOCCCI_Clim_CumMean_1998_2017_AllIslands.nc")

#Chlorophyll_A_ESAOCCCI_Daily
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean/Chlorophyll_A_ESAOCCCI_Daily_mean_1997-09-04_2018-10-28_AllIslands.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean/Chlorophyll_A_ESAOCCCI_Daily_mean_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_annual_range/Chlorophyll_A_ESAOCCCI_Daily_mean_annual_range_1997-09-04_2018-10-28_AllIslands.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_annual_range/Chlorophyll_A_ESAOCCCI_Daily_mean_annual_range_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_biweekly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_biweekly_range_1997-09-04_2018-10-28_AllIslands.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_biweekly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_biweekly_range_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_monthly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_monthly_range_1997-09-04_2018-10-28_AllIslands.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_monthly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_monthly_range_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_weekly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_weekly_range_1997-09-04_2018-10-28_AllIslands.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/q05/Chlorophyll_A_ESAOCCCI_Daily_q05_1997-09-04_2018-10-28_AllIslands.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/q95/Chlorophyll_A_ESAOCCCI_Daily_q95_1997-09-04_2018-10-28_AllIslands.nc")
env_df = raster("Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/q95/Chlorophyll_A_ESAOCCCI_Daily_q95_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")

#degree heating weeks
env_df = raster("Degree_Heating_Weeks/Domain_Level_Data/DHW.CI95Max/Degree_Heating_Weeks_DHW.CI95Max_1985-03-25_2019-12-31_AllIslands.nc")
env_df = raster("Degree_Heating_Weeks/Domain_Level_Data/DHW.CI95Max_Major/Degree_Heating_Weeks_DHW.CI95Max_Major_1985-03-25_2019-12-31_AllIslands.nc")
env_df = raster("Degree_Heating_Weeks/Domain_Level_Data/DHW.MaxMax/Degree_Heating_Weeks_DHW.MaxMax_1985-03-25_2019-12-31_AllIslands.nc")
env_df = raster("Degree_Heating_Weeks/Domain_Level_Data/DHW.MaxMax_Major/Degree_Heating_Weeks_DHW.MaxMax_Major_1985-03-25_2019-12-31_AllIslands.nc")
env_df = raster("Degree_Heating_Weeks/Domain_Level_Data/DHW.MeanDur/Degree_Heating_Weeks_DHW.MeanDur_1985-03-25_2019-12-31_AllIslands.nc")
env_df = raster("Degree_Heating_Weeks/Domain_Level_Data/DHW.MeanDur_Major/Degree_Heating_Weeks_DHW.MeanDur_Major_1985-03-25_2019-12-31_AllIslands.nc")
env_df = raster("Degree_Heating_Weeks/Domain_Level_Data/DHW.MeanMax/Degree_Heating_Weeks_DHW.MeanMax_1985-03-25_2019-12-31_AllIslands.nc")
env_df = raster("Degree_Heating_Weeks/Domain_Level_Data/DHW.MeanMax_Major/Degree_Heating_Weeks_DHW.MeanMax_Major_1985-03-25_2019-12-31_AllIslands.nc")
env_df = raster("Degree_Heating_Weeks/Domain_Level_Data/DHW.Np10y/Degree_Heating_Weeks_DHW.Np10y_1985-03-25_2019-12-31_AllIslands.nc")
env_df = raster("Degree_Heating_Weeks/Domain_Level_Data/DHW.Np10y_Major/Degree_Heating_Weeks_DHW.Np10y_Major_1985-03-25_2019-12-31_AllIslands.nc")

#Kd490_ESAOCCCI_Daily
env_df = raster("Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean/Kd490_ESAOCCCI_Daily_mean_1998-01-01_2018-06-26_AllIslands.nc")
env_df = raster("Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean/Kd490_ESAOCCCI_Daily_mean_1998-01-01_2018-06-26_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean_annual_range/Kd490_ESAOCCCI_Daily_mean_annual_range_1998-01-01_2018-06-26_AllIslands.nc")
env_df = raster("Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean_biweekly_range/Kd490_ESAOCCCI_Daily_mean_biweekly_range_1998-01-01_2018-06-26_AllIslands.nc")
env_df = raster("Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean_monthly_range/Kd490_ESAOCCCI_Daily_mean_monthly_range_1998-01-01_2018-06-26_AllIslands.nc")
env_df = raster("Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean_weekly_range/Kd490_ESAOCCCI_Daily_mean_weekly_range_1998-01-01_2018-06-26_AllIslands.nc")
env_df = raster("Kd490_ESAOCCCI_Daily/Domain_Level_Data/q05/Kd490_ESAOCCCI_Daily_q05_1998-01-01_2018-06-26_AllIslands.nc")
env_df = raster("Kd490_ESAOCCCI_Daily/Domain_Level_Data/q95/Kd490_ESAOCCCI_Daily_q95_1998-01-01_2018-06-26_AllIslands.nc")
env_df = raster("Kd490_ESAOCCCI_Daily/Domain_Level_Data/q95/Kd490_ESAOCCCI_Daily__Daily_q95_1998-01-01_2018-06-26_AllIslands_30meter5pctmaskSTRM15.nc")

#kdPAR_VIIRS_weekly_2019
env_df = raster("kdPAR_VIIRS_weekly_2019/Domain_Level_Data/mean/kdPAR_VIIRS_weekly_2019_mean_2019-07-23_2019-11-19_AllIslands.nc")
env_df = raster("kdPAR_VIIRS_weekly_2019/Domain_Level_Data/mean/kdPAR_VIIRS_weekly_2019_mean_2019-07-23_2019-11-19_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("kdPAR_VIIRS_weekly_2019/Domain_Level_Data/q05/kdPAR_VIIRS_weekly_2019_q05_2019-07-23_2019-11-19_AllIslands.nc")
env_df = raster("kdPAR_VIIRS_weekly_2019/Domain_Level_Data/q05/kdPAR_VIIRS_weekly_2019_q05_2019-07-23_2019-11-19_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("kdPAR_VIIRS_weekly_2019/Domain_Level_Data/q95/kdPAR_VIIRS_weekly_2019_q95_2019-07-23_2019-11-19_AllIslands.nc")
env_df = raster("kdPAR_VIIRS_weekly_2019/Domain_Level_Data/q95/kdPAR_VIIRS_weekly_2019_q95_2019-07-23_2019-11-19_AllIslands_30meter5pctmaskSTRM15.nc")

#PAR_MODIS_Daily
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/mean/PAR_MODIS_Daily_mean_2002-07-04_2019-12-30_AllIslands.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/mean/PAR_MODIS_Daily_mean_2002-07-04_2019-12-30_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/mean_annual_range/PAR_MODIS_Daily_mean_annual_range_2002-07-04_2019-12-30_AllIslands.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/mean_annual_range/PAR_MODIS_Daily_mean_annual_range_2002-07-04_2019-12-30_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/mean_biweekly_range/PAR_MODIS_Daily_mean_biweekly_range_2002-07-04_2019-12-30_AllIslands.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/mean_biweekly_range/PAR_MODIS_Daily_mean_biweekly_range_2002-07-04_2019-12-30_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/mean_monthly_range/PAR_MODIS_Daily_mean_monthly_range_2002-07-04_2019-12-30_AllIslands.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/mean_monthly_range/PAR_MODIS_Daily_mean_monthly_range_2002-07-04_2019-12-30_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/mean_weekly_range/PAR_MODIS_Daily_mean_weekly_range_2002-07-04_2019-12-30_AllIslands.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/q05/PAR_MODIS_Daily_q05_2002-07-04_2019-12-30_AllIslands.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/q95/PAR_MODIS_Daily_q95_2002-07-04_2019-12-30_AllIslands.nc")
env_df = raster("PAR_MODIS_Daily/Domain_Level_Data/q95/PAR_MODIS_Daily_q95_2002-07-04_2019-12-30_AllIslands_30meter5pctmaskSTRM15.nc")

#PAR_VIIRS_8day_2019
env_df = raster("PAR_VIIRS_8day_2019/Domain_Level_Data/mean/PAR_VIIRS_8day_2019_mean_2019-07-24_2019-11-21_AllIslands.nc")
env_df = raster("PAR_VIIRS_8day_2019/Domain_Level_Data/mean/PAR_VIIRS_8day_2019_mean_2019-07-24_2019-11-21_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("PAR_VIIRS_8day_2019/Domain_Level_Data/q05/PAR_VIIRS_8day_2019_q05_2019-07-24_2019-11-21_AllIslands.nc")
env_df = raster("PAR_VIIRS_8day_2019/Domain_Level_Data/q05/PAR_VIIRS_8day_2019_q05_2019-07-24_2019-11-21_AllIslands_30meter5pctmaskSTRM15.nc")
env_df = raster("PAR_VIIRS_8day_2019/Domain_Level_Data/q95/PAR_VIIRS_8day_2019_q95_2019-07-24_2019-11-21_AllIslands.nc")
env_df = raster("PAR_VIIRS_8day_2019/Domain_Level_Data/q95/PAR_VIIRS_8day_2019_q95_2019-07-24_2019-11-21_AllIslands_30meter5pctmaskSTRM15.nc")

#SST_CRW_Clim
env_df = raster("SST_CRW_Clim/SST_CRW_Clim_CumMean_1985_2018_AllIslands.nc")

#SST_CRW_Daily
env_df = raster("SST_CRW_Daily/Domain_Level_Data/mean/SST_CRW_Daily_mean_1985-01-01_2019-12-31_AllIslands.nc")
env_df = raster("SST_CRW_Daily/Domain_Level_Data/mean_annual_range/SST_CRW_Daily_mean_annual_range_1985-01-01_2019-12-31_AllIslands.nc")
env_df = raster("SST_CRW_Daily/Domain_Level_Data/mean_biweekly_range/SST_CRW_Daily_mean_biweekly_range_1985-01-01_2019-12-31_AllIslands.nc")
env_df = raster("SST_CRW_Daily/Domain_Level_Data/mean_monthly_range/SST_CRW_Daily_mean_monthly_range_1985-01-01_2019-12-31_AllIslands.nc")
env_df = raster("SST_CRW_Daily/Domain_Level_Data/mean_weekly_range/SST_CRW_Daily_mean_weekly_range_1985-01-01_2019-12-31_AllIslands.nc")
env_df = raster("SST_CRW_Daily/Domain_Level_Data/q05/SST_CRW_Daily_q05_1985-01-01_2019-12-31_AllIslands.nc")
env_df = raster("SST_CRW_Daily/Domain_Level_Data/q95/SST_CRW_Daily_q95_1985-01-01_2019-12-31_AllIslands.nc")

var = as.character(env_df$layer@file@name)
var = str_split_fixed(var, "\\", 7)

plot(env_df, pch = ".", col = matlab.like(100), cex = 10); map(resolution = 1, add = T, fill = T)
points(survey_sub$LONGITUDE_LOV, survey_sub$LATITUDE_LOV, pch = 20, col = 4)

survey_sub$env = 1

survey_sub = subset(survey_sub, Year == 2019)

for (i in 1:dim(survey_sub)[1]) {
  
  # i = 130
  
  temp = survey_sub[i,]
  
  colnames(temp)[4:5] = c("lon", "lat")
  
  temp_s = temp$lat-0.05
  temp_n = temp$lat+0.05
  temp_w = temp$lon-0.05
  temp_e = temp$lon+0.05
  
  crop_box = extent(temp_w, temp_e, temp_s, temp_n) 
  e_temp = crop(env_df, crop_box); rm(crop_box)
  
  # plot(e_temp, pch = ".", col = matlab.like(100), cex = 5); map(resolution = 1, add = T, fill = T)
  
  e_temp = rasterToPoints(e_temp)
  e_temp = as.data.frame(e_temp)
  
  if (nrow(e_temp) == 0) {
    
    survey_sub[i, 8] = NA 
    
  } else {
    
    colnames(e_temp) = c("lon", "lat", "env")
    
    coord_e_temp = e_temp[,c("lon", "lat")]
    coord_survey = temp[,c("lon", "lat")]
    
    coord_e_temp = as.matrix(coord_e_temp)
    coord_survey = c(coord_survey$lon, coord_survey$lat)
    
    # Compute distance to points and select nearest index
    nearest.idx <- which.min(colSums((t(coord_e_temp) - coord_survey)^2))
    
    e_temp = e_temp[nearest.idx, ]
    
    # points(temp$lon, temp$lat, col = 2, pch = 20)
    # points(e_temp$lon, e_temp$lat, col = 5, pch = 20, cex = 3)
    
    survey_sub[i, 8] = e_temp$env 
    
  }
  
  print(i)
  
}

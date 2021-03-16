####################################################################################
### Tailor global SST data for fish-benthic REA in-situ data                     ###
### before you begin... download OISSTv2 1981-2020 and save them on your desktop ###
### https://psl.noaa.gov/data/gridded/data.noaa.oisst.v2.highres.html            ###
####################################################################################

rm(list = ls())

library(raster)
library(colorRamps)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(readr)

# master fish-benthic REA survey points 2000-2020
df <- read_csv("data/SURVEY MASTER.csv")

# pacific-centered projection
df$LONGITUDE_LOV = ifelse(df$LONGITUDE_LOV < 0, 
                          df$LONGITUDE_LOV + 360, 
                          df$LONGITUDE_LOV)

range(df$LONGITUDE_LOV, na.rm = T)
range(df$LATITUDE_LOV, na.rm = T)

# match 

for (y in 1981:2020) {
  
  # y = 1986
  
  df = stack(paste0("/Users/Kisei.Tanaka/Desktop/oisst/sst.day.mean.", y, ".nc"), varname = "sst")
  e = extent(120, 206, -15, 29) # fish benthic REA lat-lon range
  # df = raster::rotate(df) #rotate to -180:180
  df = crop(df, e); rm(e)
  assign("df", df, .GlobalEnv)
  df = readAll(df)
  save(df, file = paste0("/Users/Kisei.Tanaka/Desktop/oisst/sst.day.mean.", y, ".RData"))
  
  print(y)
  
}



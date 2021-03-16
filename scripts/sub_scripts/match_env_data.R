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

df <- read_csv("data/SURVEY MASTER.csv")

df$LONGITUDE_LOV = ifelse(df$LONGITUDE_LOV < 0, 
                          df$LONGITUDE_LOV + 360, 
                          df$LONGITUDE_LOV)

time_stamp = str_split_fixed(df$DATE_, "/", 3)
colnames(time_stamp) = c("Month", "Day", "Year")

df = cbind(time_stamp, df)

df = df[,c("Year", "Month", "Day", "LONGITUDE_LOV", "LATITUDE_LOV")]

df = df %>% 
  group_by(Year, Month, Day, LONGITUDE_LOV, LATITUDE_LOV) %>% 
  summarise(n = n())

survey = df[complete.cases(df), ]
survey$doy = paste0(survey$Day, "/", survey$Month, "/", survey$Year)
survey$doy = date.to.DOY(survey$doy)

survey$SST = 1

survey = subset(survey, Year == 2019)

for (i in 1:dim(survey)[1]) {
  
  # i = 1
  
  temp = survey[i,]

  y = temp$Year
  doy = temp$doy
  
  load(paste0("/Users/Kisei.Tanaka/fish_benthic_env/outputs/sst.day.mean.", y, ".RData"))
  sst = df[[doy]]
  
  sst = rasterToPoints(sst)
  sst = as.data.frame(sst)
  
  colnames(sst) = c("lon", "lat", "sst")
  colnames(temp)[4:5] = c("lon", "lat")
  
  temp_s = temp$lat-1
  temp_n = temp$lat+1
  temp_w = temp$lon-1
  temp_e = temp$lon+1
  
  sst = subset(sst, lon < temp_e & lon > temp_w & lat > temp_s & lat < temp_n)
  
  coord_sst = sst[,c("lon", "lat")]
  coord_survey = temp[,c("lon", "lat")]
  
  coord_sst = as.matrix(coord_sst)
  coord_survey = c(coord_survey$lon, coord_survey$lat)
  
  # Compute distance to points and select nearest index
  nearest.idx <- which.min(colSums((t(coord_sst) - coord_survey)^2))

  # Pick out the point
  nearest_sst = sst[nearest.idx, ]

  survey[i, 8] = nearest_sst$sst
  
  print(i)
  
}

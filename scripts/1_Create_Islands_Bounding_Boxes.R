################################################################
### Scripts to create bounding boxes around surveyed islands ###
### Originally developed & conceptualized by T.A.Oliver      ###
### Revised & Maintained by K.R.Tanaka & T.A.Oliver          ###
### POC: kisei.tanaka@noaa.gov & thomas.oliver@noaa.gov      ###
################################################################

rm(list = ls())

detach("package:plyr", unload = TRUE)

library(dplyr)
library(readr)
library(ggplot2)
library(ggdark)
library(rgeos)
library(sf)
library(maps)

load('data/catch_location_date.Rdata'); df = catch_grid

df$lon = df$Lon
df$lat = df$Lat

df$REGION = "MHI"
df$ISLAND = df$Island

drop_LatLonNAs = unique(c(which(is.na(df$lon)), which(is.na(df$lat))))
if(length(drop_LatLonNAs) > 0) df = df[-drop_LatLonNAs,]
dim(df)

df$lon = ifelse(df$lon > 180, df$lon - 360, df$lon)

###############
### mapping ###
###############

unique(df$REGION)
region = unique(df$REGION)

df %>%
  subset(REGION == region) %>%
  group_by(ISLAND) %>%
  summarise(RIGHT_XMAX = min(lon)-0.1,
            LEFT_XMIN = max(lon)+0.1,
            TOP_YMAX = min(lat)-0.1,
            BOTTOM_YMIN = max(lat)+0.1) %>%
  ggplot() +
  annotation_map(map_data("world")) +
  geom_point(data = df %>% subset(REGION == region), aes(lon, lat, color = ISLAND), alpha = 0.5) +
  geom_rect(mapping = aes(
    xmin = LEFT_XMIN,
    xmax = RIGHT_XMAX,
    ymin = BOTTOM_YMIN,
    ymax = TOP_YMAX,
    fill = ISLAND,
    color = ISLAND), alpha = 0.2) +
  # facet_wrap(.~ISLAND, scales = "free") +
  theme_void() +
  theme(legend.position = "none")

############################
### export as a csv file ###
############################

boxes = df %>%
  group_by(ISLAND) %>%
  summarise(RIGHT_XMAX = min(lon)-0.1,
            LEFT_XMIN = max(lon)+0.1,
            TOP_YMAX = min(lat)-0.1,
            BOTTOM_YMIN = max(lat)+0.1)

df_frame = df[,c("REGION", "ISLAND")]

df = merge(boxes, df_frame)
df = df[!duplicated(df), ]

df = df[,c("REGION", "ISLAND", "TOP_YMAX", "BOTTOM_YMIN", "LEFT_XMIN", "RIGHT_XMAX")]
df$INCLUDED.ISLANDS = ""
colnames(df)[2] = "ISLAND.CODE"

df$ISLAND.CODE = gsub(" ", "_", df$ISLAND.CODE)

write_csv(df, 'data/Island_Extents.csv')

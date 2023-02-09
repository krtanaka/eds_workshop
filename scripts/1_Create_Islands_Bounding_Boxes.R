################################################################
### Scripts to create bounding boxes around surveyed islands ###
### Originally developed & conceptualized by T.A.Oliver      ###
### Revised & Maintained by K.R.Tanaka & T.A.Oliver          ###
### POC: kisei.tanaka@noaa.gov, thomas.oliver@noaa.gov,      ###
### jessica.perelman@noaa.gov, & juliette.verstaen@noaa.gov  ###
################################################################

rm(list = ls())

detach("package:plyr", unload = TRUE)
# if you have never downloaded {plyr} this will throw an error - you don't need to run it if that's the case
# Warning would be: "Warning message: ‘plyr’ namespace cannot be unloaded: namespace ‘plyr’ is imported by ‘ClimProjDiags’, ‘multiApply’ so cannot be unloaded"


library(dplyr)
library(readr)
library(ggplot2)
library(ggdark)
library(rgeos)
library(sf)
library(maps)

load('data/SURVEY MASTER.RData'); df = SURVEY_MASTER

df$lon = df$LONGITUDE_LOV
df$lat = df$LATITUDE_LOV

drop_LatLonNAs = unique(c(which(is.na(df$lon)), which(is.na(df$lat))))
if(length(drop_LatLonNAs) > 0) df = df[-drop_LatLonNAs,]
dim(df)

###############
### mapping ###
###############

unique(df$REGION)
region = unique(df$REGION)[5]

df %>%
  subset(REGION == region) %>%
  group_by(ISLAND) %>%
  summarise(RIGHT_XMAX = min(lon)-0.25,
            LEFT_XMIN = max(lon)+0.25,
            TOP_YMAX = min(lat)-0.25,
            BOTTOM_YMIN = max(lat)+0.25) %>%
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
  facet_wrap(.~ISLAND, scales = "free") +
  # theme_void() +
  theme(legend.position = "none")

############################
### export as a csv file ###
############################

boxes = df %>%
  group_by(ISLAND) %>%
  summarise(RIGHT_XMAX = min(lon)-0.25,
            LEFT_XMIN = max(lon)+0.25,
            TOP_YMAX = min(lat)-0.25,
            BOTTOM_YMIN = max(lat)+0.25)

df_frame = df[,c("REGION", "ISLAND")]

df = merge(boxes, df_frame)
df = df[!duplicated(df), ]

df = df[,c("REGION", "ISLAND", "TOP_YMAX", "BOTTOM_YMIN", "LEFT_XMIN", "RIGHT_XMAX")]
df$INCLUDED.ISLANDS = ""
colnames(df)[2] = "ISLAND.CODE"

df$ISLAND.CODE = gsub(" ", "_", df$ISLAND.CODE)

# write_csv(df, 'data/Island_Extents.csv')

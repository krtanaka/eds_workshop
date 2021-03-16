#########################################################
### Quality Control                                   ###
### how many islands fall outside of bounding boxes ? ###
#########################################################

rm(list = ls())

library(spatial)
library(raster)
library(lubridate)
library(ncdf4)
library(metR)
library(readr)
library(ggplot2)

###########################################################################
### read survey data points, assign distinct lat, lon, and time columns ###
###########################################################################
load('data/SURVEY MASTER.RData'); SM = SURVEY_MASTER

SM$LON = SM$LONGITUDE_LOV
SM$LAT = SM$LATITUDE_LOV
SM$DATE_R = mdy(SM$DATE_)

drop_LatLonNAs = unique(c(which(is.na(SM$LON)), which(is.na(SM$LAT))))
if(length(drop_LatLonNAs) > 0) SM = SM[-drop_LatLonNAs,]
dim(SM)

unique(SM$REGION)

##############################################
### read & visualize island bounding boxes ###
##############################################
BB_ISL = read.csv("data/Island_Extents.csv")

map_boxes = subset(BB_ISL, REGION %in% c(
  "SAMOA",
  "MHI",
  "MARIAN",
  "NWHI",
  "PRIAs"
))

map_survey = subset(SM, REGION %in% c(
  "SAMOA",
  "MHI",
  "MARIAN",
  "NWHI",
  "CT",
  "PRIAs"
))

# map_boxes$LEFT_XMIN = ifelse(map_boxes$LEFT_XMIN < 0, map_boxes$LEFT_XMIN + 180, map_boxes$LEFT_XMIN)
# map_boxes$RIGHT_XMAX = ifelse(map_boxes$RIGHT_XMAX < 0, map_boxes$RIGHT_XMAX + 180, map_boxes$RIGHT_XMAX)
# map_survey$LON = ifelse(map_survey$LON < 0, map_survey$LON + 180, map_survey$LON)

ggplot() +
  geom_point(data = map_survey, aes(LON, LAT, color = REGION, fill = REGION), size = 0.5) +
  geom_rect(data = map_boxes,
            mapping = aes(
              xmin = LEFT_XMIN,
              xmax = RIGHT_XMAX,
              ymin = BOTTOM_YMIN,
              ymax = TOP_YMAX,
              fill = REGION,
              color = REGION), alpha = 0.2) +
  facet_wrap(.~ REGION, scales = "free")

# check survey points from PRIAs

map_survey %>%
  subset(REGION == "PRIAs") %>%
  ggplot() +
  geom_point(aes(LON, LAT, color = ISLAND, fill = ISLAND)) +
  geom_text(aes(LON, LAT, label = ISLAND))

map_boxes %>%
  subset(REGION == "PRIAs") %>%
  ggplot() +
  geom_rect(mapping = aes(
    xmin = LEFT_XMIN,
    xmax = RIGHT_XMAX,
    ymin = BOTTOM_YMIN,
    ymax = TOP_YMAX,
    fill = ISLAND.CODE,
    color = ISLAND.CODE), alpha = 0.2)

ggplot() +
  geom_rect(data = map_boxes %>%
              subset(REGION == "PRIAs") %>%
              # subset(ISLAND.CODE %in% c("PAL", "KIN")),
              subset(ISLAND.CODE %in% c("JOH")),
            # subset(ISLAND.CODE %in% c("HOW", "BAK", "JAR")),
            mapping = aes(
              xmin = LEFT_XMIN,
              xmax = RIGHT_XMAX,
              ymin = BOTTOM_YMIN,
              ymax = TOP_YMAX,
              fill = REGION,
              color = REGION), alpha = 0.2) +
  geom_point(data = map_survey %>%
               subset(REGION == "PRIAs") %>%
               # subset(ISLAND %in% c("Palmyra", "Kingman")),
               subset(ISLAND %in% c("Johnston")),
             # subset(ISLAND %in% c("Howland", "Baker", "Jarvis")),
             aes(LON, LAT, color = REGION, fill = REGION), size = 0.5) +
  coord_fixed()

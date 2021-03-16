################################################################
### Scripts to create bounding boxes around surveyed islands ###
### Originally developed & conceptualized by T.A.Oliver      ###
### Revised & Edited & Maintained by K.R.Tanaka & T.A.Oliver ###
################################################################

rm(list = ls())

detach("package:plyr", unload = TRUE)

library(dplyr)
library(readr)
library(ggplot2)

load('data/SURVEY MASTER.RData'); df = SURVEY_MASTER

df$lon = df$LONGITUDE_LOV
df$lat = df$LATITUDE_LOV

drop_LatLonNAs = unique(c(which(is.na(df$lon)), which(is.na(df$lat))))
if(length(drop_LatLonNAs) > 0) df = df[-drop_LatLonNAs,]
dim(df)

# df$lon = ifelse(df$lon < 0, df$lon + 180, df$lon)

###############
### mapping ###
###############

unique(df$REGION)
region = unique(df$REGION)[5]

# I tried different bounding methods... range(pretty()) seems reasonable.

df %>%
  subset(REGION == region) %>%
  group_by(ISLAND) %>%
  # summarise(RIGHT_XMAX = range(pretty(lon))[2],
  #           LEFT_XMIN = range(pretty(lon))[1],
  #           TOP_YMAX = range(pretty(lat))[2],
  #           BOTTOM_YMIN = range(pretty(lat))[1]) %>%
  # summarise(RIGHT_XMAX = (range(lon)*1.001)[2],
  #           LEFT_XMIN = (range(lon)*0.999)[1],
  #           TOP_YMAX = (range(lat)*1.001)[2],
  #           BOTTOM_YMIN = (range(lat)*0.999)[1]) %>%
  summarise(RIGHT_XMAX = min(lon)-0.25,
            LEFT_XMIN = max(lon)+0.25,
            TOP_YMAX = min(lat)-0.25,
            BOTTOM_YMIN = max(lat)+0.25) %>%
  # summarise(RIGHT_XMAX = range(lon)[2],
  #           LEFT_XMIN = range(lon)[1],
  #           TOP_YMAX = range(lat)[2],
  #           BOTTOM_YMIN = range(lat)[1]) %>%
  # summarise(RIGHT_XMAX = ceiling(range(pretty(lon))[2]),
  #           LEFT_XMIN = floor(range(pretty(lon))[1]),
  #           TOP_YMAX = ceiling(range(pretty(lat))[2]),
  #           BOTTOM_YMIN = floor(range(pretty(lat))[1])) %>%
  # summarise(RIGHT_XMAX = ceiling(range(lon)[2]),
  #           LEFT_XMIN = floor(range(lon)[1]),
  #           TOP_YMAX = ceiling(range(lat)[2]),
  #           BOTTOM_YMIN = floor(range(lat)[1])) %>%
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
  # coord_fixed() +
  facet_wrap(.~ISLAND, scales = "free") +
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


################################
### compare with Tom's boxes ###
################################
BB_ISL = read.csv("M:/Environmental Data Summary/GeographyInputFiles/Island_10km_Grid_Extents_CSV.csv") # previous input file

BB_ISL$REGION = ifelse(BB_ISL$REGION == "AMSM", "SAMOA", BB_ISL$REGION)
BB_ISL$REGION = ifelse(BB_ISL$REGION == "PRIA", "PRIAs", BB_ISL$REGION)

BB_ISL_sub = subset(BB_ISL, REGION %in% c("MARIAN", "PRIAs", "NWHI", "MHI", "CT", "SAMOA"))
df_sub = subset(df, REGION %in% c("MARIAN", "PRIAs", "NWHI", "MHI", "CT", "SAMOA"))

df_sub$LEFT_XMIN = ifelse(df_sub$LEFT_XMIN < 0, df_sub$LEFT_XMIN + 360, df_sub$LEFT_XMIN)
df_sub$RIGHT_XMAX = ifelse(df_sub$RIGHT_XMAX < 0, df_sub$RIGHT_XMAX + 360, df_sub$RIGHT_XMAX)

BB_ISL_sub$LEFT_XMIN = ifelse(BB_ISL_sub$LEFT_XMIN < 0, BB_ISL_sub$LEFT_XMIN + 360, BB_ISL_sub$LEFT_XMIN)
BB_ISL_sub$RIGHT_XMAX = ifelse(BB_ISL_sub$RIGHT_XMAX < 0, BB_ISL_sub$RIGHT_XMAX + 360, BB_ISL_sub$RIGHT_XMAX)

world <- rnaturalearth::ne_countries(scale = 'small', returnclass = "sp")
box_cut <- bbox2SP(n = 90, s = -90, w = -20, e = 20, proj4string = world@proj4string)
world_crop <- gDifference(world, box_cut)

pacific_crop <- world_crop %>%
  st_as_sf() %>%
  st_shift_longitude() %>%
  st_crop(c(
    xmin = 120,
    xmax = 206,
    ymin = -14,
    ymax = 29))

ggplot() +
  geom_sf(data = pacific_crop, size = 0.01, fill = "gray10", color = "gray10") +
  geom_rect(data = df_sub,
            mapping = aes(
              xmin = LEFT_XMIN,
              xmax = RIGHT_XMAX,
              ymin = BOTTOM_YMIN,
              ymax = TOP_YMAX,
              fill = "Kisei"), alpha = 0.5) +
  geom_rect(data = BB_ISL_sub,
            mapping = aes(
              xmin = LEFT_XMIN,
              xmax = RIGHT_XMAX,
              ymin = BOTTOM_YMIN,
              ymax = TOP_YMAX,
              fill = "Tom"), alpha = 0.5) +
  scale_fill_viridis_d("") +
  # facet_wrap(.~ REGION, scales = "free") +
  annotation_map(map_data("world"))


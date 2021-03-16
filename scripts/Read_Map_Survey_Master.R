rm(list = ls())

library(readr)
library(dplyr)
library(rnaturalearth)
library(rgeos)
library(sf)
library(ggplot2)
library(stringr)

load("data/SURVEY MASTER.RData"); df = SURVEY_MASTER

df$LONGITUDE_LOV = ifelse(df$LONGITUDE_LOV < 0,
                          df$LONGITUDE_LOV + 360,
                          df$LONGITUDE_LOV)

time_stamp = str_split_fixed(df$DATE_, "/", 3)
colnames(time_stamp) = c("Month", "Day", "Year")

df = cbind(time_stamp, df)

df = df %>%
  group_by(LATITUDE_LOV, LONGITUDE_LOV, Day, Month, Year) %>%
  summarise(n = n())

df = df[complete.cases(df), ]

world <- rnaturalearth::ne_countries(scale = 'small', returnclass = "sp")
box_cut <- bbox2SP(n = 90, s = -90, w = -20, e = 20, proj4string = world@proj4string)
world_crop <- gDifference(world, box_cut)

pacific_crop <- world_crop %>%
  st_as_sf() %>%
  st_shift_longitude() %>%
  st_crop(c(
    # xmin = st_bbox(.)[["xmin"]],
    # xmax = st_bbox(.)[["xmax"]],
    xmin = 110,
    xmax = 220,
    ymin = -20,
    ymax = 30))

df$Month = ifelse(df$Month %in% c(1:9), sprintf("%02d", as.numeric(df$Month)), df$Month)

# pdf("outputs/Survey_Master_Survey_Points.pdf", width = 30, height = 15)

ggplot() +
  geom_sf(data = pacific_crop, size = 0.01, fill = "gray", color = "gray") +
  geom_point(data = df, aes(x = LONGITUDE_LOV, y = LATITUDE_LOV, color = log(n)), alpha = 0.8) +
  scale_color_viridis_c() +
  # facet_grid(Month ~ Year) +
  # facet_wrap(~Year) +
  facet_wrap(~Month) +
  scale_x_continuous(expand = c(0, 0), "") +
  scale_y_continuous(expand = c(0, 0), "") +
  theme_minimal()

dev.off()

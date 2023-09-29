#############################################################
### Scripts to create bounding boxes around survey points ###
### Originally developed & conceptualized by T.A.Oliver.  ###
### Revised & Maintained by K.R.Tanaka & T.A.Oliver.      ###
### POC: kisei.tanaka@noaa.gov, thomas.oliver@noaa.gov,   ###
### jessica.perelman@noaa.gov, juliette.verstaen@noaa.gov ###
#############################################################

# Clear the workspace
rm(list = ls())

# Load necessary libraries
library(dplyr)
library(readr)
library(ggplot2)
library(ggdark)
library(rgeos)
library(sf)
library(maps)

# Unload the 'plyr' namespace if it's loaded
if ("package:plyr" %in% search()) {
  unloadNamespace("plyr")
}

# Load the data
load('data/survey_mhi.RData')

df <- df %>% filter(!is.na(lon) & !is.na(lat))

# spatial unit (e.g., island)
df$unit = df$island

###############
### mapping ###
###############

buffer = 0.1

df %>%
  group_by(unit) %>%
  summarise(x_min = min(lon) - buffer,
            x_max = max(lon) + buffer,
            y_min = min(lat) - buffer,
            y_max = max(lat) + buffer) %>%
  ggplot() +
  annotation_map(map_data("world")) +
  geom_point(data = df, aes(lon, lat, fill = unit), shape = 21, alpha = 0.5) +
  geom_rect(mapping = aes(
    xmin = x_min,
    xmax = x_max,
    ymin = y_min,
    ymax = y_max,
    fill = unit,
    color = unit), alpha = 0.1) +
  theme(legend.position = "none")

############################
### export as a csv file ###
############################

boxes <- df %>%
  group_by(unit) %>%
  summarise(x_min = min(lon) - buffer,
            x_max = max(lon) + buffer,
            y_min = min(lat) - buffer,
            y_max = max(lat) + buffer) %>%
  mutate(across(c(x_max, x_min, y_max, y_min), round, digits = 2))

df_frame <- df %>%
  dplyr::select(unit) %>%
  distinct()

df <- left_join(boxes, df_frame, by = "unit") %>%
  mutate(unit = gsub(" ", "_", unit))

write_csv(df, 'data/Bounding_Boxes.csv')

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
library(maps)

# Unload the 'plyr' namespace if it's loaded
if ("package:plyr" %in% search()) unloadNamespace("plyr")

# Load sample data
load('data/survey_mhi.RData')

# Filter out rows with missing longitude or latitude
df <- df %>% filter(!is.na(lon) & !is.na(lat))

# defining spatial unit (e.g., island)
df$unit = df$island

# Set the buffer size (recommended resolution > 0.5 deg, but smaller boxes for faster ERRDAP download)
buffer = 0.1

# Create bounding boxes for each spatial unit
bounding_boxes <- df %>%
  group_by(unit) %>%
  summarise(
    x_min = min(lon) - buffer,
    x_max = max(lon) + buffer,
    y_min = min(lat) - buffer,
    y_max = max(lat) + buffer) %>%
  mutate(across(c(x_max, x_min, y_max, y_min), round, digits = 2))

# Create a plot with the bounding boxes and data points
df %>%
  ggplot() +
  annotation_map(map_data("world")) +
  geom_point(aes(lon, lat, fill = unit), shape = 21, alpha = 0.5) +
  geom_rect(data = bounding_boxes,
            aes(xmin = x_min,
                xmax = x_max,
                ymin = y_min,
                ymax = y_max,
                fill = unit,
                color = unit),
            alpha = 0.1) +
  theme(legend.position = "none")

# Export the bounding boxes as a CSV file
df = bounding_boxes %>%
  dplyr::select(unit) %>%
  distinct() %>%
  left_join(bounding_boxes, by = "unit") %>%
  mutate(unit = gsub(" ", "_", unit))

write_csv(df, file = 'data/Bounding_Boxes.csv')

# clean work station
rm(list = ls())

library(raster)
library(dplyr)
library(ggplot2)
library(readr)

# load spatial grid
df = raster("data/jplG1SST_b799_dd6c_7662.nc") # spatial grids around MHIs around at 1-km resolution
df = rasterToPoints(df)
df = as.data.frame(df[,1:2])
df$x = ifelse(df$x < 0, df$x + 360, df$x)

# load Maya's list of islands
is = read.csv("data/Reference Spreadsheet - Island_Extents.csv")
is$LEFT_XMIN = ifelse(is$LEFT_XMIN < 0, is$LEFT_XMIN + 360, is$LEFT_XMIN)
is$RIGHT_XMAX = ifelse(is$RIGHT_XMAX < 0, is$RIGHT_XMAX + 360, is$RIGHT_XMAX)
islands = is$ISLAND.CODE

# loop through the list, assign spatial sectors (NW, NE, SW, SE) for each island
mayas_islands = NULL

for (i in 1:length(islands)) {

  # i = 1

  island = is %>% subset(ISLAND.CODE == islands[i])

  grid = df %>%
    subset(x > island$RIGHT_XMAX) %>%
    subset(x < island$LEFT_XMIN) %>%
    subset(y < island$BOTTOM_YMIN) %>%
    subset(y > island$TOP_YMAX)

  grid$island = islands[i]

  grid = grid %>%
    group_by(island) %>%
    mutate(medianY = median(y),
           medianX = median(x),
           NS = ifelse(y < medianY, "S", "N"),
           EW = ifelse(x < medianX, "W", "E"),
           NSEW = paste0(NS, EW))

  grid_1 = grid %>% dplyr::select(x, y, island, NS) %>% as.data.frame(); colnames(grid_1) = c("Lon", "Lat", "Island", "Island_Sector")
  grid_2 = grid %>% dplyr::select(x, y, island, EW) %>% as.data.frame(); colnames(grid_2) = c("Lon", "Lat", "Island", "Island_Sector")
  grid_3 = grid %>% dplyr::select(x, y, island, NSEW) %>% as.data.frame(); colnames(grid_3) = c("Lon", "Lat", "Island", "Island_Sector")

  grid = rbind(grid_1, grid_2, grid_3)
  rm(grid_1, grid_2, grid_3)

  mayas_islands = rbind(grid, mayas_islands)

  print(islands[i])

}

# plot and check output
mayas_islands %>%
  ggplot(aes(Lon, Lat, fill = Island_Sector)) +
  geom_raster() +
  scale_fill_viridis_d("") +
  facet_wrap(.~Island, scale = "free") +
  theme_void()

# export as a csv file
write_csv(mayas_islands, "data/Mayas_Islands.csv")

# Import Maya's catch records
catch <- read_csv("data/Reference Spreadsheet - Sheet10.csv")

# rename columns
colnames(catch) = c("Sp_Common_Name", "Issue_Date", "Date_Caught", "Location", "Island", "Cardinal_Direction", "Island_Sector")

# subset catch records, remove rows with missing date & locations
catch = catch %>%
  subset(Island_Sector != "n/a") %>%
  subset(Island_Sector != "MISSING") %>%
  subset(Date_Caught != "n/a")

# select columns you need
catch = catch %>% dplyr::select(Sp_Common_Name, Date_Caught, Island, Island_Sector)

# rename "Big Island" to "Hawaii"
catch = catch %>% mutate(Island = ifelse(Island %in% c("Big island", "Big Island"), "Hawaii", Island))

# see if islands and sectors match with "mayas_island".
table(catch$Island)
table(mayas_islands$Island)
table(catch$Island_Sector)
table(mayas_islands$Island_Sector)

# merge by whole island
catch_WI = catch %>% subset(Island_Sector == "WI")  %>% mutate(ID = paste0(Island, "_", Island_Sector))
grid_WI = mayas_islands %>% dplyr::select(Lon, Lat, Island) %>% mutate(Island_Sector = "WI", ID = paste0(Island, "_", Island_Sector))
catch_grid_whole_island = merge(catch_WI, grid_WI)

# merge by island sector
catch_sector = catch %>% mutate(ID = paste0(Island, "_", Island_Sector))
grid_sector = mayas_islands %>% mutate(ID = paste0(Island, "_", Island_Sector))

catch_grid_sector = merge(catch_sector, grid_sector)

catch_grid = rbind(
  catch_grid_sector %>% dplyr::select(Island, Island_Sector, Sp_Common_Name, Date_Caught, Lon, Lat),
  catch_grid_whole_island %>% dplyr::select(Island, Island_Sector, Sp_Common_Name, Date_Caught, Lon, Lat))

# too many rows...
catch_grid$Lon = round(catch_grid$Lon, 1)
catch_grid$Lat = round(catch_grid$Lat, 1)

catch_grid = unique(catch_grid)

save(catch_grid, file = "data/catch_location_date.Rdata")

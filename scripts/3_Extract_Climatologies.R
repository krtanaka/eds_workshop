#################################################################
### Scripts to attach climatologies variables to in situ data ###
### Originally developed & conceptualized by T.A.Oliver       ###
### Revised & Edited & Maintained by K.R.Tanaka               ###
### POC: kisei.tanaka@noaa.gov & thomas.oliver@noaa.gov       ###
#################################################################

rm(list = ls())

library(spatial)
library(raster)
library(lubridate)
library(raster)
library(dplyr)
library(ggjoy)

dir = getwd()

# import Tom's functions
source("scripts/ExpandingExtract.R")

# import survey data, SM = master REA survey file, subset if necessary
load('data/SURVEY MASTER.RData'); SM = SURVEY_MASTER
load('data/catch_location_date.Rdata'); SM = catch_grid; SM$REGION = "MHI"; colnames(SM) = c("ISLAND", "SP", "DATE_", "LONGITUDE_LOV", "LATITUDE_LOV", "REGION")

table(SM$REGION)
SM = subset(SM, ISLAND %in% c("Hawaii"))

SM$LONGITUDE_LOV = ifelse(SM$LONGITUDE_LOV < 0, SM$LONGITUDE_LOV + 360, SM$LONGITUDE_LOV)

# remove NAs in lat & lon columns, then turn it into spatial object
SM = SM[complete.cases(SM[,c("LONGITUDE_LOV", "LATITUDE_LOV")]), ]
SM_sp = SM; SM_sp = as.data.frame(SM_sp)
coordinates(SM_sp) = ~LONGITUDE_LOV + LATITUDE_LOV

# get list of rasters (i.e., climatologies)
rasterlist = list.files(c(
  paste0("/Users/", Sys.info()[7], "/Desktop/Environmental Data Summary_Demo/DataDownload/Chlorophyll_A_ESAOCCCI_Clim/"),
  paste0("/Users/", Sys.info()[7], "/Desktop/Environmental Data Summary_Demo/DataDownload/SST_CRW_Clim/")),
  recursive = T,
  pattern = "_AllIslands.nc",
  full.names = T)

# see all rasterarized climatological variables
strsplit(rasterlist, "/")

head(SM)

###################
### normal loop ###
###################
start = Sys.time()
for(raster_i in 1:length(rasterlist)){

  # raster_i = 1

  rasname_full = rasterlist[raster_i]
  rasname_sp = strsplit(rasname_full, "/")[[1]]
  rasname = rasname_sp[length(rasname_sp)]
  rasname = gsub(rasname, pattern = "-", replacement = ".")
  rasname = gsub(rasname, pattern = "_AllIslands.nc", replacement = "")

  this_r = raster(rasterlist[raster_i])

  if (this_r@extent@xmin < 0) this_r = shift(rotate(shift(this_r, 180)), 180)

  crs(SM_sp) = crs(this_r)

  print(paste0("Step ", raster_i, " of ", length(rasterlist), ": ", rasname))

  this_Ex = ExpandingExtract(this_r, SM_sp, Dists = c(0, 50, 100, 1000, 2000, 4000, 8000))

  eval(parse(text = paste0("SM_sp$", rasname, " = this_Ex$values")))

  print(paste0("Step ", raster_i, " of ", length(rasterlist), ": Extraction Complete."))
  print(paste0("Step ", raster_i, " of ", length(rasterlist), ": Write Out Complete."))

}
stop = Sys.time()
start - stop
beepr::beep(2)

SM_climtologies = as.data.frame(SM_sp)
if (!dir.exists(paste0(getwd(),"/outputs/"))) {dir.create(paste0(getwd(),"/outputs/"))}
save(SM_climtologies, file = paste0("outputs/Climatologies_", Sys.Date(), ".RData"))

detach("package:raster", unload = TRUE)

good_sites = SM_climtologies %>% group_by(SP) %>% dplyr::summarise(n = n()) %>% subset(n > 100)
good_sites = unique(good_sites$SP)

clim1 = SM_climtologies %>%
  subset(SP %in% good_sites) %>%
  select(SP, Chlorophyll_A_ESAOCCCI_Clim_CumMean_1998_2017) %>%
  `colnames<-` (c("Species", "chl_a_1998_2017")) %>%
  ggplot(aes(x = chl_a_1998_2017, y = Species, fill = chl_a_1998_2017, color = chl_a_1998_2017)) +
  geom_joy(scale = 2, alpha = 0.8, size = 0.1, bandwidth = 0.05) +
  ylab(NULL) +
  ggdark::dark_theme_minimal() +
  scale_fill_viridis_c("") +
  scale_color_viridis_c("") +
  theme(legend.position = "right")

clim2 = SM_climtologies %>%
  subset(SP %in% good_sites) %>%
  select(SP, SST_CRW_Clim_CumMean_1985_2018) %>%
  `colnames<-` (c("Site", "sst_1985_2018")) %>%
  ggplot(aes(x = sst_1985_2018, y = Site, fill = sst_1985_2018, color = sst_1985_2018)) +
  geom_joy(scale = 2, alpha = 0.8, size = 0, bandwidth = 0.05) +
  ylab(NULL) +
  ggdark::dark_theme_minimal() +
  scale_fill_gradientn(colors =  colorRamps::matlab.like(100), "") +
  scale_color_gradientn(colors =  colorRamps::matlab.like(100), "") +
  theme(legend.position = "right")

library(patchwork)
clim1 + clim2

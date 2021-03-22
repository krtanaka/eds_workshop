#################################################################
### Scripts to attach climatologies variables to in situ data ###
### Originally developed & conceptualized by T.A.Oliver       ###
### Revised & Edited & Maintained by K.R.Tanaka               ###
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
source("scripts/HelperCode/ExpandingExtract.R")

# import survey data, SM = master REA survey file, subset if necessary
load('data/SURVEY MASTER.RData'); SM = SURVEY_MASTER
table(SM$REGION)
SM = subset(SM, REGION %in% c("MHI", "NWHI"))

SM$LONGITUDE_LOV = ifelse(SM$LONGITUDE_LOV < 0, SM$LONGITUDE_LOV + 360, SM$LONGITUDE_LOV)

# remove NAs in lat & lon columns, then turn it into spatial object
SM = SM[complete.cases(SM[,c("LONGITUDE_LOV", "LATITUDE_LOV")]), ]
SM_sp = SM; SM_sp = as.data.frame(SM_sp)
coordinates(SM_sp) = ~LONGITUDE_LOV + LATITUDE_LOV

# get list of rasters (i.e., climatologies)
rasterlist = list.files(c(
  # "M:/Environmental Data Summary/DataDownload/Bathymetry_ETOPO1/",
  # "M:/Environmental Data Summary/DataDownload/Bathymetry_SRTM15/",
  "M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Clim/",
  "M:/Environmental Data Summary/DataDownload/SST_CRW_Clim/"),
  recursive = T,
  pattern = "_AllIslands.nc",
  full.names = T)

# see all rasterarized climatological variables
strsplit(rasterlist, "/")

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

SM_climtologies %>% select(ISLAND, Chlorophyll_A_ESAOCCCI_Clim_CumMean_1998_2017) %>%
  `colnames<-` (c("island", "chl_a_1998_2017")) %>%
  ggplot(aes(x = chl_a_1998_2017, y = island, fill = island, color = island)) +
  geom_joy(scale = 5, alpha = 0.8, size = 0.1, bandwidth = 0.03) +
  ylab(NULL) +
  ggdark::dark_theme_minimal() +
  scale_fill_viridis_d("") +
  scale_color_viridis_d("") +
  theme(legend.position = "none")

SM_climtologies %>% select(ISLAND, SST_CRW_Clim_CumMean_1985_2018) %>%
  `colnames<-` (c("island", "sst_1985_2018")) %>%
  ggplot(aes(x = sst_1985_2018, y = island, fill = island, color = island)) +
  geom_joy(scale = 5, alpha = 0.8, size = 0, bandwidth = 0.5) +
  ylab(NULL) +
  ggdark::dark_theme_minimal() +
  scale_fill_viridis_d("") +
  scale_color_viridis_d("") +
  theme(legend.position = "none")

# ########################################
# ### paralleled loop (in development) ###
# ########################################
#
# library(doParallel)
#
# # find out number of available cores
# cores = detectCores()/2
# registerDoParallel(cores = cores)
#
# ptm <- proc.time()
# r = foreach(raster_i = 1:length(rasterlist), .combine = cbind, .packages = c('raster')) %dopar% {
#
#   # raster_i = 1
#
#   rasname_full = rasterlist[raster_i]
#   rasname_sp = strsplit(rasname_full, "/")[[1]]
#   rasname = rasname_sp[length(rasname_sp)]
#   rasname = gsub(rasname, pattern = "-", replacement = ".")
#   rasname = gsub(rasname, pattern = "_AllIslands.nc", replacement = "")
#
#   this_r = raster(rasterlist[raster_i])
#
#   if (this_r@extent@xmin < 0) this_r = rotate(this_r)
#
#   # map_raster = as.data.frame(rasterToPoints(this_r))
#   # plot(map_raster$x, map_raster$y, pch = 20, col = 2)
#   # points(map_sm$LONGITUDE_LOV, map_sm$LATITUDE_LOV, col = 4, pch = 2)
#   # maps::map(add = T)
#
#   crs(SM_sp) = crs(this_r)
#
#   this_Ex = ExpandingExtract(this_r, SM_sp, Dists = c(0, 50, 100, 1000, 2000, 4000, 8000, 10000))
#
#   colnames(this_Ex)[1] = rasname
#   this_Ex[1]
#
# }
# SM = cbind(SM, r)
# readr::write_csv(SM, "outputs/Survey_Master_Climatologies.csv")
# proc.time() - ptm
#

library(raster)
library(readr)

rm(list = ls())

# see what you downloaded
var = list.files(paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/")); var

# extract entire monthly sst data -----------------------------------------
source <- list.files(paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/", var[6], "/Island_Level_Data/"), pattern = '\\.nc$')
source <- paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/", var[6], "/Island_Level_Data/", source)

sst = NULL

first_date = as.Date("1985-01-31")

for( t in 1:length(source)){

  # t = 2

  filename <- source[t]

  islandname = substr(filename,1,nchar(filename)-41)
  islandname = gsub("/Users/Kisei/Desktop/EDS/DataDownload/SST_CRW_Monthly/Island_Level_Data/", "", islandname)

  env <- stack(filename)

  colnames = seq(first_date, by = "month", length.out = 435)
  names(env) = colnames

  env = as.data.frame(rasterToPoints(env))
  env$island = islandname

  sst = rbind(sst, env)
  print(islandname)

}

colnames(sst)[1:2] = c("Lon", "Lat")
write_csv(sst, paste0("/Users/", Sys.info()[7], "/Desktop/SST_CRW_Monthly.csv"))


# extract entire monthly chl_a data -----------------------------------------
source <- list.files(paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/", var[3], "/Island_Level_Data/"), pattern = '\\.nc$')
source <- paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/", var[3], "/Island_Level_Data/", source)

chla = NULL

first_date = as.Date("1997-09-04")

for( t in 1:length(source)){

  # t = 3

  filename <- source[t]

  islandname = substr(filename,1,nchar(filename)-56)
  islandname = gsub(paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/", var[3], "/Island_Level_Data/"), "", islandname)

  env <- stack(filename)

  colnames = seq(first_date, by = "month", length.out = 280)
  names(env) = colnames

  env = as.data.frame(rasterToPoints(env))
  env$island = islandname

  chla = rbind(chla, env)
  print(islandname)

}

colnames(chla)[1:2] = c("Lon", "Lat")
write_csv(chla, paste0("/Users/", Sys.info()[7], "/Desktop/Chla_ESAOCCCI_Monthly.csv"))


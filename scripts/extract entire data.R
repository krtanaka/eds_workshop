library(raster)
library(readr)
library(doParallel)
library(dplyr)

# Detect the Number of CPU Cores
cores = detectCores()/2
registerDoParallel(cores = cores)

rm(list = ls())

source("/Users/Kisei/env_data_summary/scripts/HelperCode/ExpandingExtract.R")

# see what you downloaded
var = list.files(paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/"))[6]

# extract entire monthly sst data -----------------------------------------
source <- list.files(paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/", var, "/Island_Level_Data/"), pattern = '\\.nc$')
source <- paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/", var, "/Island_Level_Data/", source)

### check EDS parameter table, set first date ###
eds_parameter = read.csv("data/EDS_parameters.csv", stringsAsFactors = F)
eds_parameter = subset(eds_parameter, PARAMETER.NAME == var)

# first_date = as.Date("1985-03-25")
first_date = as.Date("1985-01-31")

load('data/catch_location_date.Rdata'); df = catch_grid; rm(catch_grid)

#####################################################
### loop through islands                          ###
### uses parallelized loop for faster computation ###
#####################################################

for(il in 1:length(source)){

  il = 1

  filename <- source[il]

  islandname = substr(filename,1,nchar(filename)-41)
  islandname = gsub(paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/", var, "/Island_Level_Data/"), "", islandname)

  env <- stack(filename)

  # env = as.data.frame(rasterToPoints(env))
  # env$island = islandname
  # date = seq(first_date, by = "month", length.out = dim(env)[2])
  # colnames(env)[3:dim(env)[2]] = as.character(date)
  # save(env, file = paste0("/Users/", Sys.info()[7], "/Desktop/", islandname, "_raw_", var, ".RData"))

  df_i = df %>% subset(Island == islandname)%>% as.data.frame()
  df_i$X = round(df_i$Lon, 1)
  df_i$Y = round(df_i$Lat, 1)
  df_i$X = ifelse(df_i$X < 0, df_i$X + 180, df_i$X)
  df_i = df_i[complete.cases(df_i[,c("X", "Y")]), ]
  df_i = df_i %>% select(X, Y, Island, Island_Sector, Sp_Common_Name, Date_Caught) %>% distinct(X, Y, .keep_all = TRUE)
  coordinates(df_i) = ~X + Y

  crs(df_i) = crs(env) = "+proj=longlat +datum=WGS84 +no_defs"

  dates = seq(first_date, by = "month", length.out = dim(env)[3])

  ######################################
  ### normal loop, takes very long...###
  ######################################

  # this_Ex_full = NULL
  # for (i in 1:dim(env)[3]) {
  #
  #   # i = 1
  #
  #   env_i = subset(env, i)
  #
  #   this_Ex = ExpandingExtract(env_i, df_i, Dists = c(0, 1, 5)); summary(this_Ex)
  #
  #   this_Ex_full = cbind(this_Ex_full, this_Ex$values)
  #
  #   print(i)
  #
  # }
  # this_Ex_full = as.data.frame(this_Ex_full)


  ########################################
  ### parallelized loop, much faster...###
  ########################################

  r = foreach(i = 1:dim(env)[3], .combine = cbind, .packages = c('raster', 'spatial')) %dopar% {

    # i = 400

    env_i = subset(env, i)

    this_Ex = ExpandingExtract(env_i, df_i, Dists = c(0, 1, 5, 10, 50, 100, 500)); summary(this_Ex)

    this_Ex = this_Ex$values

    # this_Ex = as.data.frame(this_Ex$values)
    # colnames(this_Ex) = colnames[i]
    # this_Ex_full = cbind(this_Ex_full, this_Ex$values)
    # print(i)

  }

  this_Ex_full = as.data.frame(r)
  colnames(this_Ex_full) = dates

  df_i = df %>% subset(Island == islandname)%>% as.data.frame()
  df_i$X = round(df_i$Lon, 1)
  df_i$Y = round(df_i$Lat, 1)
  df_i$X = ifelse(df_i$X < 0, df_i$X + 180, df_i$X)
  df_i = df_i[complete.cases(df_i[,c("X", "Y")]), ]
  df_i = df_i %>% select(X, Y, Island, Island_Sector, Sp_Common_Name, Date_Caught) %>% distinct(X, Y, .keep_all = TRUE)
  df_i = cbind(df_i, this_Ex_full)

  save(df_i, file = paste0("/Users/", Sys.info()[7], "/Desktop/", islandname, "_raw_", var, ".RData"))

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
  islandname = gsub(paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/", var, "/Island_Level_Data/"), "", islandname)

  env <- stack(filename)

  env = as.data.frame(rasterToPoints(env))
  env$island = islandname
  colnames = seq(first_date, by = "month", length.out = 280)
  names(env) = colnames
  # save(env, file = paste0("/Users/", Sys.info()[7], "/Desktop/", islandname, "_raw_", var, ".RData"))

  df_i = df %>% subset(ISLAND == islandname)%>% as.data.frame()
  df_i$X = df_i$LONGITUDE_LOV
  df_i$Y = df_i$LATITUDE_LOV
  df_i$LONGITUDE_LOV = ifelse(df_i$LONGITUDE_LOV < 0, df_i$LONGITUDE_LOV + 180, df_i$LONGITUDE_LOV)
  df_i = df_i[complete.cases(df_i[,c("X", "Y")]), ]
  coordinates(df_i) = ~X + Y

  colnames = seq(first_date, by = "month", length.out = 280)
  names(env) = colnames

  env = as.data.frame(rasterToPoints(env))
  env$island = islandname

  chla = rbind(chla, env)
  print(islandname)

}

colnames(chla)[1:2] = c("Lon", "Lat")
write_csv(chla, paste0("/Users/", Sys.info()[7], "/Desktop/Chla_ESAOCCCI_Monthly.csv"))


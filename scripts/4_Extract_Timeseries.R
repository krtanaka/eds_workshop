#######################################################################
### R scripts to attach timeseries variables to in situ survey data ###
### Originally developed & conceptualized by T.A.Oliver             ###
### Revised & Maintained by K.R.Tanaka & T.A.Oliver                 ###
### POC: kisei.tanaka@noaa.gov & thomas.oliver@noaa.gov             ###
#######################################################################

rm(list = ls())

library(spatial)
library(raster)
library(lubridate)
library(ncdf4)
library(dplyr)
library(ggplot2)
library(patchwork)
library(colorRamps)
library(visdat)
library(corrplot)
library(marmap)
library(ggjoy)
library(ggrepel)

dir = paste0(getwd(), "/")

############################
### load Tom's functions ###
############################
source("scripts/EDS_HelperFunctions.R")

###########################################################################
### read survey data points, assign distinct lat, lon, and time columns ###
###########################################################################
load('data/SURVEY MASTER.RData'); SM = SURVEY_MASTER %>% subset(ISLAND == "Hawaii")
load('data/catch_location_date.Rdata');
SM = catch_grid;
SM$REGION = "MHI";
colnames(SM) = c("ISLAND", "SECTOR", "SP", "DATE_", "LONGITUDE_LOV", "LATITUDE_LOV", "REGION")
SM = SM %>% subset(ISLAND != "Molokai")

SM$ISLAND = gsub(" ", "_", SM$ISLAND)

SM$LON = SM$LONGITUDE_LOV
SM$LAT = SM$LATITUDE_LOV
SM$DATE_R = mdy(SM$DATE_)

#############################################
### drop data points with missing lat lon ###
#############################################
drop_LatLonNAs = unique(c(which(is.na(SM$LON)), which(is.na(SM$LAT))))
if(length(drop_LatLonNAs) > 0) SM = SM[-drop_LatLonNAs,]
dim(SM)

##################################
### read island bounding boxes ###
##################################
BB_ISL = read.csv("data/Island_Extents.csv"); unique(BB_ISL$ISLAND.CODE)

####################################################
### Build list of target environmental variables ###
####################################################
paramdir = paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/")
parameters = c("SST_CRW_Daily", "Chlorophyll_A_ESAOCCCI_8Day"); parameters # select only dynamic variables

#########################################
### Read EDS Parameter/Variable Table ###
#########################################
Parameter_Table = read.csv("data/EDS_parameters.csv")
names(Parameter_Table)
unique(Parameter_Table$PARAMETER.NAME)

#####################################
### Prep For Variable Extractions ###
#####################################

# locate each point in an island bounding box
PT = points.in.polys(SM$LON,
                     SM$LAT,
                     BB_ISL)

SM$DATA_ISL = SM$ISLAND

# drop points outside target area and report
print(paste("Dropping",
            length(which(SM$DATA_ISL == "NONE_ASSIGNED")),
            "points of",
            nrow(SM),
            "entered points, as outside Geographic Scope"))

SM = subset(SM, DATA_ISL != "NONE_ASSIGNED")

# list of islands
unique_islands = sort(unique(SM$DATA_ISL)); unique_islands

############################################################
### Extract Time Series Variables for every survey point ###
############################################################

# normal loop for each parameter
start_time <- Sys.time()
for(parameter_i in 1:length(parameters)){

  # parameter_i = 1

  # Get Island_Level_Data Directory for this Param
  param.name = parameters[parameter_i]; param.name

  this_param_i = which(Parameter_Table$PARAMETER.NAME == param.name); this_param_i

  godir = paste(paramdir, param.name, "/Island_Level_Data", sep = ""); godir

  paramsum = unlist(strsplit(as.vector(Parameter_Table[this_param_i, "Summaries"]), ";")); paramsum

  #For each Island
  for(island_i in 1:length(unique_islands)){

    # island_i = 1

    unique_islands[island_i]

    #Get ISLAND, PARAM data
    ncfile = list.files(godir, pattern = paste0(unique_islands[island_i], "_"), full.names = T)

    nc_p = nc_open(ncfile)

    #pull var array
    rawvar = ncvar_get(nc = nc_p, varid = as.vector(Parameter_Table$GRID.VARIABLE[this_param_i]))

    #pull dim vectors
    lon = ncvar_get(nc = nc_p, varid = "longitude"); lon
    lat = ncvar_get(nc = nc_p, varid = "latitude"); lat
    rawt = ncvar_get(nc = nc_p, varid = "time"); rawt

    #close nc
    nc_close(nc_p)

    t = as_date(as_datetime(as.numeric(rawt), origin = ymd("1970/1/1")))
    head(t); tail(t)

    #Subset to ISLAND
    SM_i = which(SM$DATA_ISL == unique_islands[island_i])

    # locate all points in rawvar array, flagging any out of bound with NA in "ijk"
    ijk = xyt2ijk(xyt_df = as.data.frame(SM[SM_i,c("LON","LAT","DATE_R")]),
                  x_grid = lon,
                  y_grid = lat,
                  t_grid = t)
    ijk

    ###Check for any out of bound points (stored as NA), we want to drop them from both ijk and SM_i
    droprows = which(is.na(ijk), arr.ind = T)[,1] #Finds points as rows in ijk

    # If there are any NA, drops rows from ijk and indices from SM_i
    if(length(droprows)>0){

      ijk = ijk[-droprows,]
      SM_i = SM_i[-droprows]

    }

    if (length(SM_i) == 0) {

      print("No SM_i ERROR ERROR ERROR ERROR ERROR ERROR")
      next()

    } else {

      # Count NA in var array (will use to solve NA issues)
      naP_xy = aaply(rawvar,
                     c(1,2),
                     NAstackcount)/dim(rawvar)[3]

      naP_xy

      #id points sitting on NA-heavy timeseries
      i_masked = which(naP_xy[cbind(ijk$x_i, ijk$y_j)] > 0.9); i_masked

      #Infill selected points with spatial interpolation
      cnt = 1

      for(i_infill in i_masked){

        #update NA blocks
        pNA = naP_xy[cbind(ijk$x_i[i_infill],
                           ijk$y_j[i_infill])]

        #selected NA timeseries +/- x pixel steps
        ij_ex = 1

        while(pNA > 0.9 & ij_ex < 3){

          ij_vec = -ij_ex:ij_ex

          # make sure selected NA timeseries +/- x pixel steps fits within the size of rawvar

          max_x = dim(rawvar)[1]
          max_y = dim(rawvar)[2]

          ts_x = ijk$x_i[i_infill]+ij_vec
          ts_y = ijk$y_j[i_infill]+ij_vec

          ts_x = subset(ts_x, ts_x <= max_x)
          ts_x = subset(ts_x, ts_x > 0)
          ts_y = subset(ts_y, ts_y <= max_y)
          ts_y = subset(ts_y, ts_y > 0)

          #Generates "infill" time series
          ts = aaply(rawvar[ts_x,
                            ts_y,],
                     c(3),
                     mean, na.rm = T)

          pNA = length(which(is.na(ts)))/length(ts)

          if(pNA < 0.9){

            #Update rawvar
            rawvar[ijk$x_i[i_infill],
                   ijk$y_j[i_infill],] = ts

            #Update naP
            naP_xy = aaply(rawvar,c(1,2),
                           NAstackcount)/dim(rawvar)[3]

            print(paste("In-fill complete", cnt, "of", length(i_masked), ". Pixel +/-", ij_ex))

          }

          ij_ex = ij_ex + 1

        }#Close While

        #Fill in interpolated data
        cnt = cnt + 1

      }#close infill for

      #Set Time Step
      if(Parameter_Table$FREQUENCY[this_param_i] == "Daily"){

        tstep = 1

      } else if (Parameter_Table$FREQUENCY[this_param_i] == "Monthly"){

        tstep = 30

      } else if (Parameter_Table$FREQUENCY[this_param_i] == "Weekly"){

        tstep = 7

      } else if (Parameter_Table$FREQUENCY[this_param_i] == "8day"){

        tstep = 8

      } else {

        tstep = 1
      }

      #TimeSeries Pull Indices
      ijk$t_03mo = ijk$t_k - (90/tstep-1)
      ijk$t_01yr = round(ijk$t_k-(1*365.25/tstep-1))

      ijk[,c("t_k",
             "t_03mo",
             "t_01yr")][which(ijk[,c("t_k",
                                     "t_03mo",
                                     "t_01yr")] < 1, arr.ind = T)] = 1

      #Apply Summaries to Timeseries
      for(sum_i in 1:length(paramsum)){

        # sum_i = 1

        paramsum.name = paste0(paramsum[sum_i], "_", param.name); paramsum.name

        if(!paramsum.name %in% substr(names(SM), 1, nchar(paramsum.name))){

          eval(parse(text = paste0("SM$",paramsum.name,"_MO03=-9991")))
          eval(parse(text = paste0("SM$",paramsum.name,"_YR01=-9991")))

        }

        #For each point in SM_i
        for(sumpt_i in 1:length(SM_i)){

          # sumpt_i = 1

          ts_03mo = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_03mo[sumpt_i]:ijk$t_k[sumpt_i]]
          ts_01yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_01yr[sumpt_i]:ijk$t_k[sumpt_i]]
          t_03mo = t[ijk$t_03mo[sumpt_i]:ijk$t_k[sumpt_i]]
          t_01yr = t[ijk$t_01yr[sumpt_i]:ijk$t_k[sumpt_i]]
          eval(parse(text = paste0("SM$", paramsum.name, "_MO03[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_03mo, na.rm = T)")))
          eval(parse(text = paste0("SM$", paramsum.name, "_YR01[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01yr, na.rm = T)")))


        }#END Loop over this island's points (for 1:length(SM_i))

        print(paste(unique_islands[island_i],
                    paramsum.name, "Done.",
                    island_i, "of",
                    length(unique_islands), "islands. Completed",
                    sumpt_i, " points..."))

      }#END Loop over each summary function  (for 1:length(paramsum))

    }

  }#END Loop over each island

}#END Loop over each parameter

save(SM, file = paste0(dir, "outputs/Timeseries_", Sys.Date(), ".Rdata"))

end_time <- Sys.time()
end_time - start_time

SM[SM == -9991] <- NA

#make columns easier to read...
colnames(SM) = gsub("_SST_CRW_Daily_", "_SST_Daily_", colnames(SM))
colnames(SM) = gsub("_Chlorophyll_A_ESAOCCCI_", "_chl_a_", colnames(SM))

vis_miss(SM[,c(12:19)])
vis_miss(SM[,c(20:dim(SM)[2])])

detach("package:plyr", unload = TRUE)
n = SM %>%
  mutate(ID = paste0(SP, "_", ISLAND, "_", SECTOR),
                    ID = gsub(" ", "_", ID)) %>%
  group_by(ID) %>%
  summarise(n = n()) %>%
  subset(n > 100)
good_sites = n$SITE

b = getNOAA.bathy(lon1 = min(pretty(SM$LON)),
                  lon2 = max(pretty(SM$LON)),
                  lat1 = min(pretty(SM$LAT)),
                  lat2 = max(pretty(SM$LAT)),
                  resolution = 2)

b = fortify.bathy(b)

sd = SM %>%
  subset(SITE %in% good_sites) %>%
  group_by(SITE) %>%
  mutate(sd = median(sd_sst_YR01)) %>%
  ggplot(aes(x = sd_sst_YR01, y = SITE , fill = sd, color = sd)) +
  geom_joy(scale = 3, alpha = 0.8, size = 0.01) +
  ylab(NULL) +
  coord_fixed(ratio = 0.06) +
  ggdark::dark_theme_minimal() +
  scale_fill_gradientn(colours = matlab.like(length(good_sites)), "") +
  scale_color_gradientn(colours = matlab.like(length(good_sites)), "") +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  ggtitle("Obs specific SST sd year_1", )

sites_with_high_sd = SM %>%
  subset(SITE %in% good_sites) %>%
  group_by(SITE) %>%
  summarise(sd = median(sd_sst_YR01)) %>%
    top_n(sd, 3)

map = SM %>%
  subset(SITE %in% good_sites) %>%
  group_by(SITE) %>%
  summarise(lon = mean(LON),
            lat = mean(LAT),
            sd = median(sd_sst_YR01))

map = ggplot() +
  geom_point(data = map, aes(x = lon, y = lat),
             alpha = 0.3, size = 5) +
  geom_text_repel(data = map, aes(x = lon, y = lat, label = ifelse(SITE %in% sites_with_high_sd$SITE, SITE, ""))) +
  geom_contour(data = b,
               aes(x = x, y = y, z = z),
               breaks = seq(-8000, 0, by = 200),
               size = c(0.05),
               alpha = 0.8,
               colour = topo.colors(3310)) +
  ggdark::dark_theme_minimal() +
  # theme_void() +
  theme(legend.position = "none",
        axis.title = element_blank()) +
  coord_fixed()

png(paste0("/Users/", Sys.info()[7], "/Desktop/map_sd.png"),units = "in", res = 100,  height = 6, width = 10)
sd + map
dev.off()

################################################################
### Scripts to download gridded data from ERDDAP server      ###
### Originally developed & conceptualized by T.A.Oliver      ###
### Revised & Edited & Maintained by K.R.Tanaka & T.A.Oliver ###
### POC: kisei.tanaka@noaa.gov & thomas.oliver@noaa.gov      ###
################################################################

rm(list = ls())

library(rerddap)
library(zoo)
library(ncdf4)
library(RNetCDF)
library(easyNCDF)
library(raster)
library(lubridate)
library(abind)
library(plyr)
library(acss)

# Library Calls and Function Definition
source("scripts/EDS_HelperFunctions.R")

# Setup ERRDAP Cache
cache_setup(temp_dir = TRUE)
cache_delete_all()
closeAllConnections()

# load your bounding boxes (Islands)
Ibbox = read.csv("data/Island_Extents.csv", stringsAsFactors = F) # Updated Bounding boxes 2021
uI = unique(Ibbox$ISLAND.CODE)[13]; uI

# select ERDDAP data
ParamDF = read.csv("data/EDS_parameters.csv", stringsAsFactors = F)
ParamDF = subset(ParamDF, DOWNLOAD == "YES")
startwith = 1
endwith = nrow(ParamDF)
ParamDF = ParamDF[startwith:endwith,]
uP = unique(ParamDF$PARAMETER.NAME); uP # static and dynamic SST and chl_a

# path to store ERDDAP nc files
EDSpath = paste0("/Users/", Sys.info()[7], "/Desktop/", "EDS/") # w/o VPN

if (!dir.exists(EDSpath)) {dir.create(EDSpath)}

# Pull Data from ERDDAP for each parameter
for (iP in 1:length(uP)){

  # iP = 1

  #Select first parameter
  thisp = subset(ParamDF, PARAMETER.NAME == uP[iP])

  print(thisp$PARAMETER.NAME)

  # Get ERDDAP Info data
  thisinfo = info(datasetid = thisp$DATASET.ID, url = thisp$URL)

  # Ask about how longitude is stored, determine if 180 or 360 style
  longinfo = thisinfo$alldata$longitude
  longrange = longinfo[which(longinfo$attribute_name == "actual_range"), "value"]
  longrange_num = as.numeric(unlist(strsplit(longrange, ",")))

  if(min(longrange_num) < 0){long180or360 = 180} else {long180or360 = 360}

  #find or create my output directory
  paramoutpath = paste0(EDSpath,"DataDownload/", uP[iP])
  if (!dir.exists(paramoutpath)) {dir.create(paramoutpath)}

  #If no time dimension..., just pull into flat NCDF
  if (thisp$FREQUENCY == "Climatology"){

    pib_path = paste0(paramoutpath,"/Island_By_Blocks_Level_Data")

    if (!dir.exists(pib_path)) {dir.create(pib_path)}

    #Loop through each island
    for (ii in 1:length(uI)){

      #select island's data
      thisisland = subset(Ibbox, ISLAND.CODE == uI[ii])

      #Get appropriate Longitude span
      if(long180or360 == 360){

        thislong = long180to360(thisisland[, c("LEFT_XMIN","RIGHT_XMAX")])

      }else{

        thislong = thisisland[, c("LEFT_XMIN","RIGHT_XMAX")]

      }

      ##griddap call to pull data from server
      targetfilename = paste0(pib_path,"/",
                              thisisland$ISLAND.CODE,"_",
                              thisp$PARAMETER.NAME,"_",
                              thisp$START_DATE,".nc")

      if(!file.exists(targetfilename)){

        thisIP = griddap(x = thisp$DATASET.ID,
                         url = thisp$URL,
                         fields = c(thisp$GRID.VARIABLE),
                         longitude = thislong,
                         latitude = thisisland[,c("BOTTOM_YMIN","TOP_YMAX")],
                         fmt = "nc",
                         store = disk(path = pib_path))

        ncstatus = file.rename(thisIP$summary$filename,
                               targetfilename)

        print(paste0(thisisland$ISLAND.CODE," written to disk. ",
                     ii,' of ',
                     length(uI)))

      }

    }# All Island Closes

    print(paste0("Completed ",thisp$PARAMETER.NAME," Check: ",length(uI)," islands data present. "))

    MergedRasterFileName = paste0(paramoutpath, "/",
                                  uP[iP], "_",
                                  thisp$START_DATE, "_AllIslands.nc")

    if(!file.exists(MergedRasterFileName)){

      #reread each file, merge into single ncdf, output...
      print(paste0("completed each island. Merging netcdfs now..."))

      #get list of island-level ncdfs
      ILnc = list.files(pib_path,pattern = "*.nc", full.names = T)
      r = raster(ILnc[1])
      crs(r) = "+proj=longlat +datum=WGS84"

      print(paste0("loaded file ", 1, " of ",length(ILnc)))

      for(rasi in 1:length(ILnc)){

        newr = raster(ILnc[rasi])
        crs(newr) = "+proj=longlat +datum=WGS84"
        origin(newr) = origin(r)
        r = merge(r,newr)
        print(paste0("loaded and merged file ",rasi," of ",length(ILnc)))

      }

      #Write Raster as nc file
      writeRaster(x = r,
                  filename = MergedRasterFileName,
                  format = "CDF",
                  overwrite = T)

    }#end of Merged Raster export...

    print(paste0("Completed ", thisp$PARAMETER.NAME, " Check: All-island merged file present."))

  }else{

    #Start Date
    if(!is.Date(thisp$START_DATE)){
      take1 = ymd(thisp$START_DATE,quiet = T)
      if(is.na(take1)){
        take2 = mdy(thisp$START_DATE,quiet = T)
        if(is.na(take2)){
          take3 = dmy(thisp$START_DATE,quiet = T)
          if(is.na(take3)){
            print("You've got some date errors to deal with. Check out your parameter file")
          }else{thisp$START_DATE = take3}
        }else{thisp$START_DATE = take2}
      }else{thisp$START_DATE = take1}
    }

    #End date
    if(!is.Date(thisp$STOP_DATE)){
      take1 = ymd(thisp$STOP_DATE,quiet = T)
      if(is.na(take1)){
        take2 = mdy(thisp$STOP_DATE,quiet = T)
        if(is.na(take2)){
          take3 = dmy(thisp$STOP_DATE,quiet = T)
          if(is.na(take3)){
            print("You've got some date errors to deal with. Check out your parameter file")
          }else{thisp$STOP_DATE = take3}
        }else{thisp$STOP_DATE = take2}
      }else{thisp$STOP_DATE = take1}
    }

    #For each island
    pib_path = paste0(paramoutpath,"/Island_By_Blocks_Level_Data")
    if (!dir.exists(pib_path)) {dir.create(pib_path)}

    #Loop through each island
    for (ii in 1:length(uI)){

      # ii = 30

      #select island's data
      thisisland = subset(Ibbox,ISLAND.CODE == uI[ii])

      #Get appropriate Longitude span
      if(long180or360 == 360){

        thislong = long180to360(thisisland[,c("LEFT_XMIN","RIGHT_XMAX")])

      }else{

        thislong = thisisland[,c("LEFT_XMIN","RIGHT_XMAX")]

      }

      ##griddap call to pull test data from server
      testIP = griddap(x = thisp$DATASET.ID,
                       url = thisp$URL,
                       fields = c(trim(thisp$GRID.VARIABLE)),
                       time = c('last','last'),
                       longitude = thislong,
                       latitude = thisisland[,c("BOTTOM_YMIN","TOP_YMAX")],
                       store = memory())

      singlestep = nrow(testIP$data)/10

      Nblocks = ceiling((singlestep * thisp$TIMESTEPS) / thisp$BLOCKSIZE); Nblocks

      # Get Metadata
      NCG = thisinfo$alldata$NC_GLOBAL

      #Set Dates for each block
      ts_start = thisp$START_DATE
      ts_end = thisp$STOP_DATE

      if(thisp$FREQUENCY == "Monthly") {
        TIMESTEP = 30#(60*60*24*30)
      } else if (thisp$FREQUENCY == "14day"){
        TIMESTEP = 14#(60*60*24*14)
      } else if (thisp$FREQUENCY == "8day"){
        TIMESTEP = 8#(60*60*24*8)
      } else if (thisp$FREQUENCY == "Weekly"){
        TIMESTEP = 7#(60*60*24*7)
      } else { #Daily
        TIMESTEP = 1#(60*60*24*1)
      }

      block_step = ((ts_end - ts_start)) / Nblocks

      for(blockI in 1:Nblocks){

        # blockI = 11

        this_start = ceiling_date(ts_start + (blockI-1) * block_step, unit = "day")
        this_end = floor_date(ts_start + ((blockI) * block_step) - TIMESTEP, unit = "day")

        if(this_end > ts_end|blockI == Nblocks) this_end = ts_end

        targetfilename = paste0(pib_path,"/",
                                thisisland$ISLAND.CODE,"_",
                                thisp$PARAMETER.NAME,"_",
                                this_start,"_",
                                this_end,".nc")

        #if the targetfile doesn't already exist, call griddap
        if(!file.exists(targetfilename)){

          #Try multiple times...
          Ntries = 10; Ntried = 0; Success = F

          #Try to call griddap until it works or you've tried Ntries times
          repeat{
            thisIP = tryCatch({griddap(x = thisp$DATASET.ID,
                                       url = thisp$URL,
                                       # fields = c(thisp$GRID.VARIABLE),
                                       time = c(this_start,this_end),
                                       longitude = thislong,
                                       latitude = thisisland[,c("BOTTOM_YMIN","TOP_YMAX")],
                                       fmt = "nc",
                                       store = disk(path = pib_path),
                                       read = FALSE)},
                              error = function(e){
                                print("GRIDDAP ERROR")
                              }
            )

            #If it's a griddap_nc object (and not an error), you're good, exit repeat
            if(Ntried > Ntries){

              print(paste0("GRIDDAP exiting after ", Ntried, "tries ... All done."))

              break

            }else if(class(thisIP) == "character" && thisIP == "GRIDDAP ERROR"){

              Ntried = Ntried + 1

              if(Ntried > 3){

                fl = list.files(pib_path) #get list of files

                #check that the first three characters are not a known ISLAND.CODE
                deletiontarget1 = which(!substr(fl, 1, 3) %in% substr(Ibbox$ISLAND.CODE, 1, 3))

                #check that the length of the file matches the length of auto-generated names (i.e. 35)

                if (length(deletiontarget1) > 0){

                  deletiontarget2 = deletiontarget1[which(nchar(fl[deletiontarget1]) == 35)]

                }

                #Finally Check that the entropy of the character string is in the least 5% (i.e. it looks random)
                if (length(deletiontarget2) > 0){

                  Eset = acss::entropy(substr(fl, 1, 32))
                  Ep = pcalc(Eset,Eset[deletiontarget2])
                  deletiontarget3 = which(Ep < 0.05)

                }

                #Now if a file has passed all three tests, delete it any keep going.
                if (length(deletiontarget3) > 0){

                  file.remove(paste0(pib_path,"/",fl[deletiontarget3]))
                  print(paste0("DELETED APPARENT GRIDDAP TEMP FILE: ",fl[deletiontarget3]))

                }

              }

              print(paste0("GRIDDAP returned error: Keeping it together... Try #",Ntried))

            }else{

              print(paste0("GRIDDAP Returned Successfully."))

              break

            }# Close exit condition IF

          }#close REPEAT

          #once the griddap call works, rename the file
          ncstatus = file.rename(thisIP$summary$filename, targetfilename)

          print(paste0(thisisland$ISLAND.CODE,", block #",
                       blockI," of ",
                       Nblocks," written to disk. Island #",
                       ii,' of ',
                       length(uI)))

        }#end target exists IF

      }# end block loop for

      print(paste0("Completed ", thisp$PARAMETER.NAME,
                   " Check: ", Nblocks,
                   " blocks present for ", thisisland$ISLAND.CODE,
                   ". ", ii,
                   " of ", length(uI), " islands."))

      #For each island - set up single island folder
      pi_path = paste0(paramoutpath,"/Island_Level_Data")
      if (!dir.exists(pi_path)) {dir.create(pi_path)}

      outfile = paste0(pi_path, "/",
                       thisisland$ISLAND.CODE, "_",
                       thisp$PARAMETER.NAME, "_",
                       floor_date(ts_start, unit = "day"), "_",
                       floor_date(ts_end, unit = "day"), ".nc")

      if(!file.exists(outfile)){

        AllBlock = list.files(pib_path,
                              full.names = T,
                              pattern = thisisland$ISLAND.CODE)

        out = merge_times_nc_list(infilenamelist = as.list(AllBlock),
                                  variable_name = thisp$GRID.VARIABLE,
                                  outfilename = outfile)
      }

      print(paste0("Completed ",
                   thisp$PARAMETER.NAME,
                   " Check: Merged time-series .nc present for ",
                   thisisland$ISLAND.CODE))

      #For each island - run each time-series average and export
      TsSummaries = strsplit(thisp$Summaries,";")[[1]]

      #if necessary

      #For each summary stat
      for(sumi in 1:length(TsSummaries)){

        #set up summary folder for island-level data
        thisSum = TsSummaries[sumi]

        Spi_path = paste0(pi_path, "/", thisSum)

        if (!dir.exists(Spi_path)) {dir.create(Spi_path)}

        targetfilename = paste0(Spi_path, "/",
                                thisisland$ISLAND.CODE, "_",
                                thisp$PARAMETER.NAME, "_",
                                thisSum, "_",
                                floor_date(ts_start,unit = "day"), "_",
                                floor_date(ts_end,unit = "day"), ".nc")

        if(!file.exists(targetfilename)){

          #Load each island's TS data as raster stack
          island_ts_nc = nc_open(outfile)
          island_ts_t = as_datetime(as.numeric(island_ts_nc$dim$time$vals))
          island_ts_xyt = ncvar_get(island_ts_nc,thisp$GRID.VARIABLE)
          dim(island_ts_xyt)

          #island_ts_xyt raster is in memory
          if (thisSum %in% c("mean","q05","q95","sd")){

            island_sum = apply(island_ts_xyt, c(1,2), thisSum, na.rm = T)

          }else if (thisSum %in% c("mean_annual_range",
                                   "mean_monthly_range",
                                   "mean_biweekly_range")){

            island_sum = apply(island_ts_xyt, c(1,2), thisSum, t = island_ts_t, na.rm = T)

          }else if (thisSum %in% c("DHW.Np10y",
                                   "DHW.MeanMax",
                                   "DHW.MeanDur",
                                   "DHW.MaxMax",
                                   "DHW.CI95Max",
                                   "DHW.Np10y_Major",
                                   "DHW.MeanMax_Major",
                                   "DHW.MeanDur_Major",
                                   "DHW.MaxMax_Major",
                                   "DHW.CI95Max_Major")){

            island_sum = apply(island_ts_xyt, c(1,2), thisSum, t = island_ts_t, na.rm = T)

          }

          #Given Island-Level Summary, create new ncdf file and write it out.
          write_summary_nc_from_ts(template = island_ts_nc,
                                   data = island_sum,
                                   variable_name = thisp$GRID.VARIABLE,
                                   outfile = targetfilename
          )

          #Clean up...
          nc_close(island_ts_nc)

        }

      }# Close Summary For

      print(paste0("Completed ",
                   thisp$PARAMETER.NAME,
                   " Check: Summary files output for ",
                   thisisland$ISLAND.CODE,
                   ": ",
                   paste(TsSummaries, collapse = "; ")))

    } # Close Island For

    #reread each file from each island/summary, merge into single ncdf, output...

    print(paste0("completed each island. Merging netcdfs now..."))

    # get list of island-level ncdfs
    # For each summary stat
    for(sumi in 1:length(TsSummaries)){

      thisSum = TsSummaries[sumi]
      Spi_path = paste0(pi_path, "/", thisSum)

      #Make Needed Directories
      d_path = paste0(paramoutpath, "/Domain_Level_Data")

      if (!dir.exists(d_path)) {dir.create(d_path)}

      ds_path = paste0(d_path, "/", thisSum)

      if (!dir.exists(ds_path)) {dir.create(ds_path)}

      raster_outfile = paste0(ds_path, "/",
                              uP[iP], "_",
                              thisSum, "_",
                              thisp$START_DATE, "_",
                              thisp$STOP_DATE, "_AllIslands.nc")

      if(!file.exists(raster_outfile)){

        ILnc = list.files(Spi_path, pattern = "*.nc", full.names = T)

        r = raster(ILnc[1])

        crs(r) = "+proj=longlat +datum=WGS84"

        print(paste0("loaded file ", 1, " of ", length(ILnc)))

        if(length(ILnc)>=2){

          for(rasi in 2:length(ILnc)){

            newr = raster(ILnc[rasi])
            crs(newr) = "+proj=longlat +datum=WGS84"
            origin(newr) = origin(r)
            r = merge(r, newr)

            print(paste0("loaded and merged file ", rasi, " of ", length(ILnc)))

          }

        }else{

          #Do nothing

        }

        #Write Raster as nc file
        writeRaster(x = r,
                    filename = raster_outfile,
                    format = "CDF",
                    overwrite = T)
      }

      print("-----------------------------------------------------------------------")
      print(paste0("Raster Present for ",thisp$PARAMETER.NAME," ",thisSum,"..."))
      print("-----------------------------------------------------------------------")

    }#Close Summary Output

  } # Close Not a Climatology Else

}#Close each param For

path = paste0("/Users/", Sys.info()[7], "/Desktop/EDS/DataDownload/")

dev.off()

# climatologies
plot(raster(paste0(path, "Chlorophyll_A_ESAOCCCI_Clim/Chlorophyll_A_ESAOCCCI_Clim_CumMean_1998_2017_AllIslands.nc")))
plot(raster(paste0(path, "SST_CRW_Clim/SST_CRW_Clim_CumMean_1985_2018_AllIslands.nc")))

# time steps
plot(stack(paste0(path, "Chlorophyll_A_ESAOCCCI_8Day/Island_Level_Data/", uI, "_Chlorophyll_A_ESAOCCCI_8Day_1997-09-04_2018-10-28.nc")))
plot(stack(paste0(path, "SST_CRW_Monthly/Island_Level_Data/", uI, "_SST_CRW_Monthly_1985-01-31_2021-03-31.nc")))

# summary statistics
par(mfrow = c(2,2))

plot(stack(paste0(path, "SST_CRW_Monthly/Island_Level_Data/mean/", uI, "_SST_CRW_Monthly_mean_1985-01-31_2021-03-31.nc")), main = "mean")
plot(stack(paste0(path, "SST_CRW_Monthly/Island_Level_Data/q05/", uI, "_SST_CRW_Monthly_q05_1985-01-31_2021-03-31.nc")), main = "q05")
plot(stack(paste0(path, "SST_CRW_Monthly/Island_Level_Data/q95/", uI, "_SST_CRW_Monthly_q95_1985-01-31_2021-03-31.nc")), main = "q95")
plot(stack(paste0(path, "SST_CRW_Monthly/Island_Level_Data/sd/", uI, "_SST_CRW_Monthly_sd_1985-01-31_2021-03-31.nc")), main = "sd")

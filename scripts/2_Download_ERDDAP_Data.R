#############################################################
### Scripts to download gridded data from ERDDAP server   ###
### Originally developed & conceptualized by T.A.Oliver.  ###
### Revised & Maintained by K.R.Tanaka & T.A.Oliver.      ###
### POC: kisei.tanaka@noaa.gov, thomas.oliver@noaa.gov,   ###
### jessica.perelman@noaa.gov, juliette.verstaen@noaa.gov ###
#############################################################

rm(list = ls())

# Install specific version of rerddap package
# remotes::install_version("rerddap", version = "1.0.1")

# Library Calls and Function Definition
source("scripts/eds_functions.R")

# Setup ERRDAP Cache
cache_setup(temp_dir = T)
cache_delete_all()
closeAllConnections()

# Read Bounding Boxes
bbox = read_csv("data/Bounding_Boxes.csv")
uI <- distinct(bbox, unit)$unit; uI <- uI[uI %in% c("Guam", "Hawaii")]

# Read Parameter and Time Series Summary Definitions
ParamDF <- read_csv("data/EDS_parameters.csv") %>% filter(Download == "YES")
uP <- ParamDF %>% pull(Dataset) %>% unique()

# Define path (e.g., M drive)
EDS_path = paste0("/Users/", Sys.info()[7], "/Desktop/EDS/") # Local without VPNs

if (!dir.exists(EDS_path)) dir.create(EDS_path, recursive = T)

# "Yes" if you want EDS to provide summary nc files
Summaries_files = c("Yes", "No")[1]

# Download each dataset from ERDDAP
for (iP in 1:length(uP)){

  # iP = 1

  # Select dataset
  thisp = subset(ParamDF, Dataset == uP[iP])

  cat(thisp$Dataset)

  # Fetch dataset information from ERDDAP
  thisinfo <- tryCatch({
    info(
      datasetid = thisp$Dataset_ID,
      url = thisp$URL
    )
  }, error = function(e) {
    cat("An error occurred: ", conditionMessage(e), "\n")
    cat("...Can't find this ERDDAP data by its ID...\n")
    return(NULL)  # Return NULL instead of printing and using next
  })

  # Check if dataset exists in ERDDAP
  if (is.null(thisinfo)) {
    cat("Dataset not found on ERDDAP. Skipping this dataset...\n")
    next
  }

  # Check how longitude is stored, determine if 180 or 360 style
  longinfo = thisinfo$alldata$longitude
  longrange = longinfo[which(longinfo$attribute_name == "actual_range"), "value"]
  longrange_num = as.numeric(unlist(strsplit(longrange, ",")))

  if (min(longrange_num) < 0){ long180or360 = 180 } else { long180or360 = 360 }

  if (thisp$Frequency == "Climatology"){

    # Find or create output directory
    paramoutpath = paste0(EDS_path,"Static_Variables/", uP[iP])
    if (!dir.exists(paramoutpath)) dir.create(paramoutpath, recursive = T)

    pib_path = paste0(paramoutpath,"/Block_Level_Data")
    if (!dir.exists(pib_path)) dir.create(pib_path)

    # Check if any files match the pattern
    if (length(list.files(paste0(paramoutpath, "/"), pattern = "all_units")) > 0) {

      cat(paste0("EDS output for ", thisp$Dataset, " already exists.\n"))
      next

    }

    # Create a list of indices for parallel processing
    indices <- 1:length(uI)

    # Loop through each unit
    foreach(ii = indices, .packages = c("rerddap")) %do% {

      # ii = 1

      # Select unit's data
      this_unit = subset(bbox, unit == uI[ii])

      # Get appropriate Longitude span
      if (long180or360 == 360){

        this_unit[, c("x_min", "x_max")] = ifelse(this_unit[, c("x_min", "x_max")] < 0,
                                                  this_unit[, c("x_min", "x_max")] + 360,
                                                  this_unit[, c("x_min", "x_max")])

        thislong = this_unit[, c("x_min","x_max")]

      } else {

        thislong = this_unit[, c("x_min","x_max")]

      }

      # Make a griddap call to pull data from server
      targetfilename = paste0(pib_path, "/",
                              this_unit$unit, "_",
                              thisp$Dataset, ".nc")

      if (!file.exists(targetfilename)){

        tryCatch({
          thisIP = griddap(datasetx = thisp$Dataset_ID,
                           url = thisp$URL,
                           fields = c(thisp$Fields),
                           longitude = thislong,
                           latitude = this_unit[, c("y_min","y_max")],
                           fmt = "nc",
                           store = disk(path = pib_path))

          ncstatus = file.rename(thisIP$summary$filename,
                                 targetfilename)

          cat(paste0(this_unit$unit,
                     " written to disk. ",
                     ii,
                     ' of ',
                     length(uI), "\n"))
        }, error = function(e) {

          cat(paste0(e$message, ". Skipping ", this_unit$unit, "...\n"))

        })
      }

    }

    cat(paste0("Completed ", thisp$Dataset, ". Check: ", length(uI), " units data present.\n"))

    MergedRasterFileName = paste0(paramoutpath, "/", uP[iP], "_all_units.nc")

    if (!file.exists(MergedRasterFileName)){

      # Re-read each file, merge into single ncdf, output...
      cat(paste0("completed each unit Merging netcdfs now...\n"))

      # Get list of island-level ncdfs
      ILnc = list.files(pib_path,pattern = "*.nc", full.names = T)

      # skip if previous step fails to produce summary .nc files
      if (length(ILnc) == 0) next

      # Load and merge raster files using parallel processing
      r <- foreach(i = 1:length(ILnc), .combine = merge, .packages = c("raster")) %do% {
        r <- raster(ILnc[i])
        crs(r) <- "+proj=longlat +datum=WGS84"
        r
      }

      r = mean(r)

      # Write Raster as nc file
      writeRaster(x = readAll(r),
                  filename = MergedRasterFileName,
                  format = "CDF",
                  overwrite = T)

    } # End of Merged Raster export

    cat(paste0("Completed ", thisp$Dataset, ". Check: all units merged file present.\n"))

  }

  if (thisp$Frequency != "Climatology"){

    # Time Series Summaries
    # This step involves generating time-series summaries from the stored .nc data.
    # We initially store the full time series for each unit.
    # We can choose between storing and merging each time series individually
    # or merging them first and then summarizing the data.

    # Find or create output directory
    paramoutpath = paste0(EDS_path,"Dynamic_Variables/", uP[iP])
    if (!dir.exists(paramoutpath)) dir.create(paramoutpath, recursive = T)

    pib_path = paste0(paramoutpath, "/Block_Level_Data")
    if (!dir.exists(pib_path)) dir.create(pib_path)

    # Loop through each unit
    for (ii in 1:length(uI)){

      # ii = 1

      # Select unit's bounding box data
      this_unit = subset(bbox, unit == uI[ii])

      # Check if any files match the pattern
      if (length(list.files(paste0(paramoutpath, "/Unit_Level_Data/"), pattern = this_unit$unit)) > 0) {
        cat(paste0("EDS output for ", this_unit$unit, " already exists.\n"))
        next
      }

      # Get appropriate Longitude span
      if (long180or360 == 360){

        this_unit[, c("x_min", "x_max")] = ifelse(this_unit[, c("x_min", "x_max")] < 0,
                                                  this_unit[, c("x_min", "x_max")] + 360,
                                                  this_unit[, c("x_min", "x_max")])

        thislong = this_unit[, c("x_min","x_max")]

      } else {

        thislong = this_unit[, c("x_min","x_max")]

      }

      # call griddap() to pull test data from server
      # skip if an unit is outside of data range
      testIP <- tryCatch({
        griddap(datasetx = thisp$Dataset_ID,
                url = thisp$URL,
                fields = c(trim(thisp$Fields)),
                time = c('last', 'last'),
                longitude = thislong,
                latitude = this_unit[, c("y_min", "y_max")],
                store = memory())
      }, error = function(e) {
        cat("GRIDDAP ERROR")
        return(NULL)
      })

      if (is.null(testIP)) {
        cat("One or both longitude values outside data range. Skipping this unit...\n")
        next
      }

      # Get Metadata
      NCG = thisinfo$alldata$NC_GLOBAL

      if (thisp$Frequency == "Monthly") timestep = 30.42 # (60*60*24*30.42)
      if (thisp$Frequency == "14day") timestep = 14 # (60*60*24*14)
      if (thisp$Frequency == "8day") timestep = 8 # (60*60*24*8)
      if (thisp$Frequency == "5day") timestep = 5 # (60*60*24*5)
      if (thisp$Frequency == "Weekly") timestep = 7 # (60*60*24*7)
      if (thisp$Frequency == "Daily") timestep = 1 # (60*60*24*1)

      # Set start and end dates for each block
      ts_start = NCG %>%
        filter(attribute_name == "time_coverage_start") %>%
        dplyr::select(value) %>%
        mutate(value = substring(value, 1, 10)) %>%  # extract only date part
        parse_date_time(orders = c("ymd", "mdy", "dmy"), tz = "UTC"); ts_start

      ts_end = NCG %>%
        filter(attribute_name == "time_coverage_end") %>%
        dplyr::select(value) %>%
        mutate(value = substring(value, 1, 10)) %>%  # extract only date part
        parse_date_time(orders = c("ymd", "mdy", "dmy"), tz = "UTC"); ts_end

      # Point to the last day of the previous month
      ts_end <- format(as.Date(ts_end) - days(as.numeric(format(ts_end, "%d"))), "%Y-%m-%d UTC")
      ts_end = ts_end %>% parse_date_time(orders = c("ymd", "mdy", "dmy"), tz = "UTC"); ts_end

      singlestep = nrow(testIP$data); singlestep

      total_timesteps = round((ts_end - ts_start)/timestep, 0) %>% as.numeric(); total_timesteps

      # Create Nblocks as an integer where block_step is always 100 times longer than timestep
      block_step <- timestep * 1000

      # Calculate Nblocks based on the updated block_step and ensure it's an integer
      Nblocks <- ceiling(as.numeric((ts_end - ts_start) / block_step))
      if (Nblocks %% 1 != 0) {
        Nblocks <- ceiling(Nblocks)
      }

      # Recalculate block_step based on the updated Nblocks
      block_step <- (ts_end - ts_start) / Nblocks

      block_step
      Nblocks

      # Set the number of cores to use
      # num_cores <- min(Nblocks, detectCores()/2)

      # Initialize parallel backend
      # registerDoParallel(cores = num_cores)
      # cl <- makeCluster(num_cores)
      # registerDoParallel(cl)

      # Create a list of indices for parallel processing
      indices <- 1:Nblocks

      # Parallel loop
      foreach(blockI = indices, .packages = c("lubridate", "rerddap")) %do% {

        # blockI = 1

        this_start = floor_date(ts_start + (blockI-1) * block_step, unit = "day")
        if (blockI > 1) this_start = this_start + days(1)

        this_end = floor_date(ts_start + ((blockI) * block_step) - timestep, unit = "day")
        if (this_end > ts_end | blockI == Nblocks) this_end = ts_end

        targetfilename = paste0(pib_path, "/",
                                this_unit$unit, "_",
                                thisp$Dataset, "_",
                                this_start, "_",
                                this_end, ".nc")

        # If the targetfile doesn't already exist, call griddap
        if (!file.exists(targetfilename)) {

          # Define a variable to keep track of whether to continue the loop
          continue_loop = TRUE

          while(continue_loop) {
            tryCatch({
              thisIP = griddap(datasetx = thisp$Dataset_ID,
                               url = thisp$URL,
                               fields = c(thisp$Fields),
                               time = c(this_start, this_end),
                               longitude = thislong,
                               latitude = this_unit[, c("y_min", "y_max")],
                               fmt = "nc",
                               store = disk(path = pib_path),
                               read = TRUE)
              continue_loop = FALSE # If no error occurs, set continue_loop to FALSE to exit the loop
            }, error = function(e) {
              cat("GRIDDAP ERROR") # The loop will continue to run until no error occurs
            })
          }

          # Once the griddap call works, rename the file
          ncstatus = file.rename(thisIP$summary$filename, targetfilename)

          cat(paste0(this_unit$unit, ", block #",
                     blockI, " of ",
                     Nblocks, " written to disk. Unit #",
                     ii, ' of ',
                     length(uI), "\n"))

        }

      }

      # Stop the parallel backend
      # stopImplicitCluster()
      # stopCluster(cl)
      # registerDoSEQ()

      cat(paste0("Completed ", thisp$Dataset,
                 ". Check: ", Nblocks,
                 " blocks present for ", this_unit$unit,
                 ". ", ii,
                 " of ", length(uI),
                 " units.\n"))

      #For each unit - set up single unit folder
      pi_path = paste0(paramoutpath, "/Unit_Level_Data")
      if (!dir.exists(pi_path)) dir.create(pi_path)

      outfile = paste0(pi_path, "/",
                       this_unit$unit, "_",
                       thisp$Dataset, "_",
                       floor_date(ts_start, unit = "day"), "_",
                       floor_date(ts_end, unit = "day"), ".nc")

      if(!file.exists(outfile)){

        AllBlock = list.files(pib_path,
                              full.names = T,
                              pattern = this_unit$unit)

        out = merge_times_nc_list(infilenamelist = as.list(AllBlock),
                                  variable_name = thisp$Fields,
                                  outfilename = outfile)
      }

      cat(paste0("Completed ",
                 thisp$Dataset,
                 ". Merged .nc time-series present for ",
                 this_unit$unit, ".\n"))

      # if you need EDS to generate unit_level summary nc files
      if (Summaries_files == "Yes") {

        # For each unit - run each time-series average and export
        TsSummaries = strsplit(thisp$Summaries,";")[[1]]

        # Loop for for each summary stat
        for (sumi in 1:length(TsSummaries)){

          # sumi = 1

          # Set up summary folder for unit-level data
          thisSum = TsSummaries[sumi]

          Spi_path = paste0(pi_path, "/", thisSum)

          if (!dir.exists(Spi_path)) dir.create(Spi_path)

          targetfilename = paste0(Spi_path, "/",
                                  this_unit$unit, "_",
                                  thisp$Dataset, "_",
                                  thisSum, "_",
                                  floor_date(ts_start,unit = "day"), "_",
                                  floor_date(ts_end,unit = "day"), ".nc")

          cat(paste0("Generating Temporal Summaries...", thisSum, "...\n"))

          if (!file.exists(targetfilename)){

            # Load each unit's TS data as raster stack
            island_ts_nc = nc_open(outfile)
            island_ts_t = as_datetime(as.numeric(island_ts_nc$dim$time$vals))
            island_ts_xyt = ncvar_get(island_ts_nc, thisp$Fields)
            dim(island_ts_xyt)

            # Island_ts_xyt raster is in memory
            if (thisSum %in% c("mean","q05","q95","sd")){

              island_sum = apply(island_ts_xyt, c(1,2), thisSum, na.rm = T)

            } else if (thisSum %in% c("mean_annual_range",
                                      "mean_monthly_range",
                                      "mean_biweekly_range")){

              island_sum = apply(island_ts_xyt, c(1,2), thisSum, t = island_ts_t, na.rm = T)

            } else if (thisSum %in% c("DHW.Np10y",
                                      "DHW.MeanMax",
                                      "DHW.MeanDur",
                                      "DHW.MaxMax",
                                      "DHW.CI95Max",
                                      "DHW.YearsToLast",
                                      "DHW.Np10y_Major",
                                      "DHW.MeanMax_Major",
                                      "DHW.MeanDur_Major",
                                      "DHW.MaxMax_Major",
                                      "DHW.CI95Max_Major",
                                      "DHW.YearsToLast_Major")){

              island_sum = apply(island_ts_xyt, c(1,2), thisSum, t = island_ts_t, na.rm = T)

            }

            # Given Island-Level Summary, create new ncdf file and write it out.
            # Add a try-catch block to handle the error
            tryCatch({

              write_summary_nc_from_ts(template = island_ts_nc,
                                       data = island_sum,
                                       variable_name = thisp$Fields,
                                       outfile = targetfilename)

            }, error = function(e) {

              # Print the error message, but continue with the loop
              cat("Error occurred:", conditionMessage(e), "\n")

            })

            # Clean up...
            nc_close(island_ts_nc)

          }

        }

        cat(paste0("Completed ", thisp$Dataset,
                   ". Check summary files for ", this_unit$unit,
                   ": ", paste(TsSummaries, collapse = "; ")))

      }

    }

    # if you need EDS to generate domain_level summary nc files
    if (Summaries_files == "Yes") {

      NCG = thisinfo$alldata$NC_GLOBAL

      # Set start and end dates for each block
      ts_start = NCG %>%
        filter(attribute_name == "time_coverage_start") %>%
        dplyr::select(value) %>%
        mutate(value = substring(value, 1, 10)) %>%  # extract only date part
        parse_date_time(orders = c("ymd", "mdy", "dmy"), tz = "UTC"); ts_start

      ts_end = NCG %>%
        filter(attribute_name == "time_coverage_end") %>%
        dplyr::select(value) %>%
        mutate(value = substring(value, 1, 10)) %>%  # extract only date part
        parse_date_time(orders = c("ymd", "mdy", "dmy"), tz = "UTC"); ts_end

      pi_path = paste0(paramoutpath, "/Unit_Level_Data")

      # For each unit - run each time-series average and export
      TsSummaries = strsplit(thisp$Summaries,";")[[1]]

      # Re-read each file from each unit/summary, merge into single ncdf, output...
      cat(paste0("Completed each unit. Merging .nc files now...\n"))

      # Get list of unit-level ncdfs for each summary stat
      for (sumi in 1:length(TsSummaries)){

        thisSum = TsSummaries[sumi]
        Spi_path = paste0(pi_path, "/", thisSum)

        # Make Needed Directories
        d_path = paste0(paramoutpath, "/Domain_Level_Data")
        if (!dir.exists(d_path)) dir.create(d_path)

        raster_outfile = paste0(d_path, "/",
                                uP[iP], "_",
                                thisSum, "_",
                                floor_date(ts_start, unit = "day"), "_",
                                floor_date(ts_end, unit = "day"), "_all_units.nc")

        if (!file.exists(raster_outfile)){

          ILnc = list.files(Spi_path, pattern = "*.nc", full.names = T)

          # skip if previous step fails to produce summary .nc files
          if (length(ILnc) == 0) next

          r = raster(ILnc[1])

          crs(r) = "+proj=longlat +datum=WGS84"

          cat(paste0("loaded file ", 1, " of ", length(ILnc), "\n"))

          if (length(ILnc) >= 2){

            for (rasi in 2:length(ILnc)){

              newr = raster(ILnc[rasi])
              crs(newr) = "+proj=longlat +datum=WGS84"
              origin(newr) = origin(r)
              r = merge(r, newr)   #This merge is flipping....

              cat(paste0("loaded and merged file ", rasi, " of ", length(ILnc), "\n"))

            }

          }

          r = readAll(r)

          # Write Raster as nc file
          writeRaster(x = r,
                      filename = raster_outfile,
                      format = "CDF",
                      overwrite = T)
        }

        cat(paste0("Raster Present for ", thisp$Dataset, " ", thisSum, "...\n"))

      }

    }

  }

}

# climatologies
nc_files <- list.files(file.path( paste0("/Users/", Sys.info()[7], "/Desktop/EDS/"), "Static_Variables"), pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
nc_files <- nc_files[!grepl("Block_Level_Data", nc_files)]

par(mfrow = c(1, 2))

for (nc_file in nc_files) {
  file_name <- basename(nc_file)
  plot(raster(nc_file), main = file_name)
}

# time steps
nc_files <- list.files(file.path( paste0("/Users/", Sys.info()[7], "/Desktop/EDS/"), "Dynamic_Variables"), pattern = "\\.nc$", full.names = TRUE, recursive = TRUE)
nc_files <- nc_files[grepl("Unit_Level_Data", nc_files) & !grepl("mean|sd|q05|q95", nc_files)]

plot(stack(nc_files[1]))
plot(stack(nc_files[2]))

# summary statistics
par(mfrow = c(2,2))

plot(stack(paste0(path, "Dynamic_Variables/Sea_Surface_Temperature_CRW_Monthly/Unit_Level_Data/mean/Guam_Sea_Surface_Temperature_CRW_Monthly_mean_1985-01-31_2023-07-31.nc")), main = "mean")
plot(stack(paste0(path, "Dynamic_Variables/Sea_Surface_Temperature_CRW_Monthly/Unit_Level_Data/q05/Guam_Sea_Surface_Temperature_CRW_Monthly_q05_1985-01-31_2023-07-31.nc")), main = "q05")
plot(stack(paste0(path, "Dynamic_Variables/Sea_Surface_Temperature_CRW_Monthly/Unit_Level_Data/q95/Guam_Sea_Surface_Temperature_CRW_Monthly_q95_1985-01-31_2023-07-31.nc")), main = "q95")
plot(stack(paste0(path, "Dynamic_Variables/Sea_Surface_Temperature_CRW_Monthly/Unit_Level_Data/sd/Guam_Sea_Surface_Temperature_CRW_Monthly_sd_1985-01-31_2023-07-31.nc")), main = "sd")

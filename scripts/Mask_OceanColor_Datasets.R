###########################################################################
### Mask Ocean Color Datasets using STRM Bathymetry and Topography data ###
###########################################################################

rm(list = ls())

library(raster)
library(dplyr)
library(maps)
library(readr)
library(ncdf4)
library(filesstrings)

#########################################
### ocean color data masking function ###
#########################################
maskfun = function(x, na.rm = F, depth_threshold = -30, percent_threshold = 5){

  denom = length(x)
  numer = length(which(x > depth_threshold))
  outp = numer/denom

  if(outp > (percent_threshold/100)){

    return(NA)

  }else{

    return(1)

  }

}

#################################################################
### load STRM Bathymetry and Topography data                  ###
### Shuttle Radar Topography Mission (SRTM)                   ###
### Global Bathymetry and Topography at 15 Arc Sec: SRTM15+   ###
### https://doi.org/10.1029/2019EA000658                      ###
#################################################################
# dir = "G:/Environmental Data Summary/DataDownload/" # backup
dir = "M:/Environmental Data Summary/DataDownload/" # main

STRM15_360 = raster(paste0(dir, "Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands_Long360.nc"))
STRM15_180 = raster(paste0(dir, "Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands.nc"))

##############################################
### list ocean color datasets need masking ###
##############################################
oc = read_csv("data/EDS_parameters.csv")

oc = oc %>%
  dplyr::select("PARAMETER.NAME", "DOWNLOAD", "MaskForOceanColor") %>%
  subset(MaskForOceanColor == T & DOWNLOAD == "YES")

oc

#################################
### mask ocean color datasets ###
#################################
var_list = oc$PARAMETER.NAME
var_list = var_list[-which(var_list %in% c("Chlorophyll_A_ESAOCCCI_Clim"))]

start_time <- Sys.time()

print(paste0("Running through ",length(var_list)," parameters: "))

print(var_list)

for (i in 1:length(var_list)) {

  # i = 3

  print(paste0("...","Running ", var_list[i], "..."))

  var_name = var_list[i]; var_name

  unmask_path = paste0(dir, var_name, "/Island_Level_Data", "/unmasked/")
  mask_path = paste0(dir, var_name, "/Island_Level_Data/")

  if (!dir.exists(unmask_path)) {dir.create(unmask_path)}

  island_nc_files = list.files(path = mask_path, pattern = ".nc"); island_nc_files

  setwd(mask_path)

  # copy unmasked files to separate folder
  if(!file.exists(paste0(unmask_path, island_nc_files))){

    current = getwd()
    new = unmask_path
    list.of.files = list.files(current, ".nc$")
    file.move(list.of.files, new)

  }

  for (island_i in 1:length(island_nc_files)) {

    # island_i = 1

    # Get filenames
    island_nc_file_name = island_nc_files[island_i]

    island_masked_nc_file_name = paste0(tools::file_path_sans_ext(island_nc_file_name), "_30meter_5pct_mask_w_STRM15.nc")

    if(!file.exists(paste0(mask_path, island_masked_nc_file_name))){

      print_isl = strsplit(island_nc_files[island_i], "_")[[1]][1]
      print(paste0("     ", "     ", "Masking ", print_isl, "..."))
      print(paste0("     ", "     ", "     ", print_isl, ": Reading data file..."))

      var_df = stack(paste0(unmask_path,island_nc_file_name))

      if (var_df@extent@xmin < 0) {

        strm = STRM15_180

      } else {

        strm = STRM15_360

      }

      #Crop Rotated STRM15, convert to SPDF
      print(paste0("     ","     ","     ",print_isl,": Cropping STRM file..."))
      cropped_strm = crop(strm, extent(var_df));# beepr::beep(2)
      spatial_strm = data.frame(rasterToPoints(cropped_strm))
      coordinates(spatial_strm) <- ~x+y
      crs(spatial_strm) = crs(var_df)

      # Build Mask
      print(paste0("     ","     ","     ",print_isl,": Building depth mask..."))
      depth_mask = rasterize(x = spatial_strm, y = var_df[[1]], vals = "layer ", fun = maskfun)$layer

      # Apply Mask
      print(paste0("     ","     ","     ",print_isl,": Applying depth mask..."))
      var_df_masked = mask(x = var_df, mask = depth_mask)

      # grab var name and unit from unmasked nc file
      nc = nc_open(paste0(unmask_path, island_nc_file_name))

      variable_name = as.character(nc$var[[1]][2])
      variable_unit = as.character(nc$var[[1]][8])

      x_name = nc$dim$longitude$name
      y_name = nc$dim$latitude$name

      z_name = nc$dim$time$name
      z_unit = nc$dim$time$units

      nc_close(nc)

      # write out masked nc.file
      print(paste0("     ","     ","     ",print_isl,": Writing out masked values..."))

      writeRaster(var_df_masked,
                  paste0(mask_path, island_masked_nc_file_name),
                  overwrite = T,
                  varname = variable_name,
                  varunit = variable_unit,
                  xname = x_name,
                  yname = y_name,
                  zname = z_name,
                  zunit = z_unit)

      print(paste0("     ","     ","     ",print_isl,": Done."))

      names <- names(var_df_masked)
      names = gsub("X", "", names)
      names = substr(names, 1, 10)

      # fix time step labels
      nc = nc_open(paste0(mask_path, island_masked_nc_file_name), write = T)
      zvals = lubridate::parse_date_time(names, orders = 'y.m.d', tz = 'UTC')
      zvals = as.integer(zvals)
      ncdf4::ncvar_put(nc, 'time', zvals)
      nc_close(nc)

    }else{

      print(paste0("     ", island_masked_nc_file_name, " already exists. Skipping ..................."))

    }

  }# island loop

  print(paste0("     ","All islands complete for ", var_list[i], "."))

} #param loop

end_time <- Sys.time()
end_time - start_time

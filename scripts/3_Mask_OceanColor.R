####################################################################################
# Masking Ocean Color Datasets using High-Resolution Bathymetry and Topography Data
# ETOPO 2022 15 Arc-Second Global Relief Model
# Source: https://www.ncei.noaa.gov/products/etopo-global-relief-model
# Revised and Maintained by K. R. Tanaka
####################################################################################

# Clear the workspace
rm(list = ls())

select = dplyr::select

# Source custom functions
source("scripts/eds_functions.R")

# Define the path to the EDS data directory
dir = paste0(file.path(Sys.getenv("USERPROFILE"),"Desktop"), "/EDS/")

# Load ETOPO 2022 Bathymetry and Topography data
bathy = raster(paste0(dir, "/Bathymetry_ETOPO_2022_v1_15s/Domain_Level_Data/Bathymetry_ETOPO_2022_v1_15s_all_units.nc"))

# List ocean color datasets that need masking
oc = read_csv("data/EDS_parameters.csv") %>%
  select("Dataset", "Download", "Mask") %>%
  filter(Mask == TRUE & Download == "YES")

# Display the selected ocean color datasets for masking
var_list <- intersect(basename(list.dirs(dir, recursive = FALSE)), oc$Dataset)

start_time <- Sys.time()

cat(sprintf("Running through %d OC datasets:\n%s\n",
            length(var_list), paste(var_list, collapse = "\n")))

for (i in 1:length(var_list)) {

  # i = 2

  var_name = var_list[i]; var_name

  unmask_path = paste0(dir, var_name, "/Unit_Level_Data/unmasked/")
  mask_path = paste0(dir, var_name, "/Unit_Level_Data/")

  if (!dir.exists(unmask_path)) {

    cat(paste0("...Masking ", var_list[i], "...\n"))
    dir.create(unmask_path)

  } else {

    cat(paste0("Masking already done for ", var_name, "...Skipping this unit...\n"))
    next

  }

  unit_nc_files = list.files(path = mask_path, pattern = ".nc"); unit_nc_files

  # copy unmasked files to separate folder
  if(all(!file.exists(paste0(unmask_path, unit_nc_files))) == T){

    setwd(mask_path)
    current = getwd()
    new = unmask_path
    list.of.files = list.files(current, ".nc$")
    file.move(list.of.files, new)
    setwd(paste0("/Users/", Sys.info()[7], "/eds_workshop"))

  } else {

    next

  }

  for (unit_i in 1:length(unit_nc_files)) {

    # unit_i = 1

    # Get file names
    unit_nc_file_name = unit_nc_files[unit_i]
    unit_masked_nc_file_name = paste0(tools::file_path_sans_ext(unit_nc_file_name), "_30m_5pct_masked.nc")

    if(!file.exists(paste0(mask_path, unit_masked_nc_file_name))){

      print_isl = strsplit(unit_nc_files[unit_i], "_")[[1]][1]
      cat(paste0("Masking ", print_isl, "...\n"))
      cat(paste0(print_isl, ": Reading data file...\n"))

      var_df = stack(paste0(unmask_path,unit_nc_file_name))

      if (var_df@extent@xmin < 0) {

        etopo = bathy

      } else {

        etopo = raster::shift(raster::rotate(raster::shift(bathy, 180)), 180)

      }

      #Crop Rotated etopo15, convert to SPDF
      cat(paste0(print_isl,": Cropping bathymetry file...\n"))
      cropped_etopo = crop(etopo, extent(var_df));# beepr::beep(2)
      spatial_etopo = data.frame(rasterToPoints(cropped_etopo))

      if (nrow(spatial_etopo) == 0) {

        cat("No bathymetry data available for masking. Skipping this unit...\n")
        next

      }

      coordinates(spatial_etopo) <- ~x+y
      crs(spatial_etopo) = crs(var_df)

      # Build Mask
      cat(paste0(print_isl,": Building depth mask...\n"))
      depth_mask = rasterize(x = spatial_etopo, y = var_df[[1]], vals = "layer ", fun = maskfun)$layer

      # Apply Mask
      cat(paste0(print_isl,": Applying depth mask...\n"))
      var_df_masked = mask(x = var_df, mask = depth_mask)

      var_df_masked = readAll(var_df_masked)

      # grab var name and unit from unmasked nc file
      nc = nc_open(paste0(unmask_path, unit_nc_file_name))

      variable_name = as.character(nc$var[[1]][2])
      variable_unit = as.character(nc$var[[1]][8])

      x_name = nc$dim$longitude$name
      y_name = nc$dim$latitude$name

      z_name = nc$dim$time$name
      z_unit = nc$dim$time$units

      nc_close(nc)

      # write out masked nc.file
      cat(paste0(print_isl,": Writing out masked values...\n"))

      writeRaster(var_df_masked,
                  paste0(mask_path, unit_masked_nc_file_name),
                  overwrite = T,
                  varname = variable_name,
                  varunit = variable_unit,
                  xname = x_name,
                  yname = y_name,
                  zname = z_name,
                  zunit = z_unit)

      cat(paste0(print_isl,": Done...\n"))

      names <- names(var_df_masked)
      names = gsub("X", "", names)
      names = substr(names, 1, 10)

      # fix time step labels
      nc = nc_open(paste0(mask_path, unit_masked_nc_file_name), write = T)
      zvals = lubridate::parse_date_time(names, orders = 'y.m.d', tz = 'UTC')
      zvals = as.integer(zvals)
      ncdf4::ncvar_put(nc, 'time', zvals)
      nc_close(nc)

    } else {

      cat(paste0(unit_masked_nc_file_name, " already exists. Skipping...\n"))

    }

  }# unit loop

  cat(paste0("All units complete for ", var_list[i], "...\n"))

} #param loop

end_time <- Sys.time()
end_time - start_time


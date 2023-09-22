# you'll need to install ggOceanMapsData from github for mapping
# install.packages("ggOceanMapsData",
#  repos = c("https://mikkovihtakari.github.io/drat",
#            "https://cloud.r-project.org"))

# List of packages to load
packages_to_load <- c(
  "rerddap", "readr", "zoo", "ncdf4", "RNetCDF", "easyNCDF",
  "raster", "lubridate", "abind", "acss", "dplyr",
  "doParallel", "foreach", "colorRamps", "ggplot2", "reshape2",
  "spatial", "data.table", "splitstackshape", "patchwork", "ggjoy",
  "visdat", "corrplot", "marmap", "ggrepel", "ggOceanMaps", "ggOceanMapsData", "metR"
)

# Function to install and load a package
install_and_load_package <- function(package_name) {
  if (!requireNamespace(package_name, quietly = TRUE)) {
    install.packages(package_name, dependencies = TRUE)
  }
  library(package_name, character.only = TRUE)
}

# Load packages
invisible(sapply(packages_to_load, install_and_load_package))

# Unload 'plyr' if it's loaded
if ("package:plyr" %in% search()) {
  unloadNamespace("plyr")
}

maskfun = function(x, na.rm = F, depth_threshold = -30, percent_threshold = 5){

  denom = length(x)
  numer = length(which(x > depth_threshold))
  outp = numer/denom

  if(outp > (percent_threshold/100)){

    return(NA)

  } else {

    return(1)

  }

}

compare_masking <- function(oc_data, unit) {

  # oc_data = "Chlorophyll_A_ESA_OC_CCI_v6.0_8Day"
  # unit = "Guam"

  r = list.files(path = paste0(dir, oc_data, "/Unit_Level_Data/unmasked/"),
                 pattern = unit,
                 full.names = T)

  if(length(r) == 0){

    print("No unmasked file found. Returning empty plot"); return(ggplot())

  }

  r[2] = list.files(path = paste0(dir, oc_data, "/Unit_Level_Data/"),
                    pattern = unit,
                    full.names = T)

  if(length(r) == 1){

    print("No masked file found. Returning empty plot"); return(ggplot())

  }

  r1 = stack(r[1])
  r2 = stack(r[2])

  # plot(r1,  pch = ".", col = matlab.like(100))
  # plot(r2,  pch = ".", col = matlab.like(100))

  r1 = as.data.frame(rasterToPoints(r1))
  r2 = as.data.frame(rasterToPoints(r2))

  r1$z = rowMeans(r1[,3:dim(r1)[2]], na.rm = T)
  r2$z = rowMeans(r2[,3:dim(r2)[2]], na.rm = T)

  r1$mask = "Unmasked"
  r2$mask = "Masked"

  r1 = r1[,c("x", "y", "z", "mask")]
  r2 = r2[,c("x", "y", "z", "mask")]

  r = rbind(r1, r2)

  r$var = oc_data

  p_r = r %>%
    ggplot(aes(x, y, fill = z)) +
    geom_raster() +
    annotation_map(map = map_data("world"), fill = "gray50") +
    scale_fill_viridis_c("", limits = c(quantile(r$z,p = 0.05, na.rm = T),
                                        quantile(r$z,p = 0.95, na.rm = T)),
                         oob = squish) +
    coord_equal() +
    facet_grid(~ mask) +
    ggtitle(oc_data) +
    labs(x = "", y = "")

  return(p_r)

}

long180to360 = function(long){

  long360 = long

  long360[long360 < 0] = (long360[long360 < 0]) + 360

  return(long360)

}

nonalength = function(x){

  length(na.omit(x))

}

CI95 = function(x, na.rm = T){

  n = length(x)

  if(n>0){

    ci = 1.96 * sd(x, na.rm = na.rm)/sqrt(n)

  } else {ci = NA}

  return(ci)
}

ts2event = function(x, t, threshold = 0, edge = T, na.rm = T){
  if(all(is.na(x))){return(NA)}
  #convert x to 0/1 vector
  w = rep(0, length(x))
  w[which(x>threshold)] = 1

  #Kick back if no signal
  if(all(w == 0)){return(NULL)}

  #Find edges in vector
  starts = which.subv(subv = c(0, 1), vec = w)+1
  ends = which.subv(subv = c(1, 0), vec = w)
  if(w[1] == 1){starts = c(1, starts)}
  if(w[length(w)] == 1){ends = c(ends, length(w))}

  ev = data.frame(event.n = 1:length(starts),
                  start.i = starts, end.i = ends,
                  start.t = t[starts], end.t = t[ends],
                  event.dur = 1+difftime(t[ends], t[starts], units = "days"))
  for(evi in 1:nrow(ev)){
    ev$event.max[evi] = max(x[ev$start.i[evi]:ev$end.i[evi]], na.rm = T)
  }
  if(!edge){
    if(ev$start.i[1] == 1){
      if(nrow(ev) == 1){return(NULL)}else{ev = ev[-1, ]}
    }
    if(ev$end.i[nrow(ev)] == length(x)){
      if(nrow(ev) == 1){return(NULL)}else{ev = ev[-nrow(ev), ]}
    }
    ev$event.n = 1:nrow(ev)
  }
  return(ev)
}

merge_times_nc = function(filename1, filename2, variable_name, outfilename){

  file1  <- nc_open(filename1)
  file2  <- nc_open(filename2)

  #Get Dimensions for output
  f1Dims = NcReadDims(file1)

  # Just for one variable for now
  mat1 = ncvar_get(file1, variable_name)
  mat2 = ncvar_get(file2, variable_name)

  #Check for Temporal Overlap, remove any overlapping times from mat2
  t1 = ncvar_get(file1, "time")
  t2 = ncvar_get(file2, "time")

  tINT = intersect(t1, t2)

  if(length(tINT) > 0) {
    tINTi = match(tINT, t2)
    mat2 = mat2[, , -tINTi]
  }

  dat_new = abind(mat1, mat2, along = 3)
  dim(dat_new)
  eval(parse(text = paste0("var = file1$var[variable_name]$", variable_name)))

  #Get dimensions - Keep in Same Orientation as first input file
  #Lon
  DEC_LON=file1$dim$longitude$vals[1]>file1$dim$longitude$vals[2]
  Dx = ncdim_def("longitude", "degrees_east", sort(unique(c(file1$dim$longitude$vals, file2$dim$longitude$vals)),decreasing=DEC_LON))

  #Lat
  DEC_LAT=file1$dim$latitude$vals[1]>file1$dim$latitude$vals[2]
  Dy = ncdim_def("latitude", "degrees_north", sort(unique(c(file1$dim$latitude$vals, file2$dim$latitude$vals)),decreasing=DEC_LAT))

  #Time
  DEC_T=file1$dim$time$vals[1]>file1$dim$time$vals[2]
  if (is.na(DEC_T) == T) DEC_T = FALSE
  Dt = ncdim_def("time", "seconds since 1970-01-01T00:00:00Z", sort(unique(c(file1$dim$time$vals, file2$dim$time$vals)),decreasing=DEC_T))

  # Create a new file
  file_new  <- nc_create(
    filename = outfilename,
    # We need to define the variables here
    vars = ncvar_def(
      name = variable_name,
      units = var$units,
      dim = list(Dx, Dy, Dt)))

  #copy over all the metadata, but not the dimensions...
  #  modifyNcdfCopyMetadata(file.con.orig = file1, file.con.copy = file_new, glob.atts = T, dimensions = F)

  # And write to it
  ncvar_put(
    nc = file_new,
    varid = variable_name,
    vals = dat_new)

  # Finally, close the files
  nc_close(file1)
  nc_close(file2)
  nc_close(file_new)
}

merge_times_nc_list = function(infilenamelist, variable_name, outfilename){

  Nfiles = length(infilenamelist)

  if(Nfiles == 0){

    print("No files to join")
    return(1)

  } else if (Nfiles == 1){

    print("single filename provided, copying/renaming file")
    file.copy(from = infilenamelist[[1]], to = outfilename, )
    return(1)

  } else if (Nfiles == 2){

    print("Two filenames provided, calling 'merge_times_nc'")
    status = merge_times_nc(filename1 = infilenamelist[[1]],
                            filename2 = infilenamelist[[2]],
                            variable_name = variable_name,
                            outfilename = outfilename)
    return(1)

  } else if (Nfiles > 2){

    print("More than two filenames provided, calling 'merge_times_nc' successively")
    status = merge_times_nc(filename1 = infilenamelist[[1]],
                            filename2 = infilenamelist[[2]],
                            variable_name = variable_name,
                            outfilename = outfilename)

    for(i in 3:length(infilenamelist)){
      print(paste0("First ", i-1, " files merged.."))
      status = merge_times_nc(filename1 = outfilename,
                              filename2 = infilenamelist[[i]],
                              variable_name = variable_name,
                              outfilename = outfilename)
    }
    print(paste0("All ", Nfiles, " complete. Written to ", outfilename))
  }
}

write_summary_nc_from_ts = function(template, data, variable_name, outfile){

  # Get Dimensions for output
  f1Dims = NcReadDims(template)

  dat_new = data
  dat_new[is.infinite(dat_new)]  = NA
  dat_new[is.nan(dat_new)]  = NA
  eval(parse(text = paste0("var = template$var[variable_name]$", variable_name)))

  # Get dimensions
  Dx = ncdim_def("longitude", "degrees_east", template$dim$longitude$vals)
  Dy = ncdim_def("latitude", "degrees_north", template$dim$latitude$vals)
  Var2d = ncvar_def(name = variable_name, units = var$units, dim = list(Dx, Dy), missval = NA)

  # Create a new file
  file_new  <- nc_create(
    filename = outfile,
    # We need to define the variables here
    vars = Var2d, verbose = F
  )

  # copy over all the metadata, but not the dimensions...
  # modifyNcdfCopyMetadata(file.con.orig = template, file.con.copy = file_new, glob.atts = T, dimensions = F)

  # And write to it
  ncvar_put(
    nc = file_new,
    varid = variable_name,
    vals = dat_new)

  # Finally, close the files
  # nc_close(template)
  nc_close(file_new)
}

points.in.polys = function(pts.x, pts.y, bb){

  nbox = nrow(bb)
  npts = length(pts.x)
  InMat = matrix(0, ncol = nbox, nrow = npts)
  colnames(InMat) = bb$unit
  DataISL = rep("NONE_ASSIGNED", npts)

  for(i in 1:nbox){

    InMat[,i] = point.in.polygon(pts.x,
                                 pts.y,
                                 bb[i,c("x_min","x_max","x_max","x_min")],
                                 bb[i,c("y_min","y_min","y_max","y_max")])

    DataISL[which(InMat[,i] >= 1)] = as.vector(bb$unit[i])

  }

  out = list(DataISL,InMat)
  names(out) = c("DATA_UNIT","IN_MATRIX")

  return(out)
}

xyt2ijk = function(xyt_df, x_grid, y_grid, t_grid){

  #Make sure data are apples to apples class-wise
  xyt_df = data.frame(x = as.numeric(xyt_df[, 1]),
                      y = as.numeric(xyt_df[, 2]),
                      t = as.Date(xyt_df[, 3]))

  xyt_df$x[xyt_df$x > 180] = xyt_df$x[xyt_df$x > 180] - 360

  x_grid = as.numeric(x_grid)
  x_grid[x_grid>180] = x_grid[x_grid > 180] - 360
  y_grid = as.numeric(y_grid)
  t_grid = as.Date(t_grid)

  #Sizes
  n_pts = nrow(xyt_df)
  n_xg = length(x_grid)
  n_yg = length(y_grid)
  n_tg = length(t_grid)

  # set x, y, t buffers
  x_buffer = mean(diff(lon))/2; x_buffer
  y_buffer = mean(diff(lat))/2; y_buffer
  t_buffer = mean(diff(t))/2; t_buffer

  #X Match
  x_gMat = outer(rep(1, n_pts), x_grid)
  x_pMat = outer(xyt_df$x, rep(1, n_xg))
  x_i = apply(abs(x_pMat-x_gMat), 1, which.min)
  #check for out of bound points
  oob_x = which(xyt_df$x < (min(x_grid, na.rm = T) - x_buffer) | xyt_df$x > (max(x_grid, na.rm = T) + x_buffer))
  x_i[oob_x] = NA

  #Y Match
  y_gMat = outer(rep(1, n_pts), y_grid)
  y_pMat = outer(xyt_df$y, rep(1, n_yg))
  y_j = apply(abs(y_pMat-y_gMat), 1, which.min)
  #check for out of bound points
  oob_y = which(xyt_df$y < (min(y_grid, na.rm = T) - y_buffer) | xyt_df$y > (max(y_grid, na.rm = T) + y_buffer))
  y_j[oob_y] = NA

  #T Match
  t_gMat = outer(rep(1, n_pts), t_grid)
  t_pMat = outer(xyt_df$t, rep(1, n_tg))
  t_k = apply(abs(t_pMat-t_gMat), 1, which.min)
  #check for out of bound points
  oob_t = which(xyt_df$t < (min(t_grid, na.rm = T) - t_buffer) | xyt_df$t > (max(t_grid, na.rm = T) + t_buffer))
  t_k[oob_t] = NA

  #Build output df
  ijk = data.frame(x_i, y_j, t_k)

  #return...
  return(ijk)
}

NAstackcount = function(x){

  return(length(which(is.na(x))))

}

q05 = function(x, na.rm = T){
  return(quantile(x, .05, na.rm = T))
}

q95 = function(x, na.rm = T){
  return(quantile(x, .95, na.rm = T))
}

which.subv = function(vec, subv){

  nv = length(vec)
  ns = length(subv)
  last_st = nv-ns + 1

  if(ns>nv){return(NULL)}
  out = NULL
  for (i in 1:last_st){
    if(all(subv == vec[i:(i+ns-1)])){out = c(out, i)}
  }

  return(out)

}

mean_annual_range = function(x, t, na.rm = T){

  #Error Handle, NaN and Inf as NA
  x[is.nan(x)] = NA
  x[is.infinite(x)] = NA
  t[is.nan(t)] = NA
  t[is.infinite(t)] = NA

  #Build Data Frame with appropriate time factors
  DFxt = data.frame(x = x, t = t)
  DFxt$Year = year(DFxt$t)

  #execute with different na.rm settings
  if(na.rm) {

    Arng = ddply(DFxt, .(Year), summarize, Arng = abs(diff(range(x, na.rm = T))), N = nonalength(x))

  }else{

    Arng = ddply(DFxt, .(Year), summarize, Arng = abs(diff(range(x, na.rm = F))), N = length(x))
  }

  #error handle missing data to Inf
  Arng$Arng[is.nan(Arng$Arng)] = NA
  Arng$Arng[is.infinite(Arng$Arng)] = NA

  return(mean(Arng$Arng[which(Arng$N > 1)], na.rm = na.rm))

}

mean_monthly_range = function(x, t, na.rm = T){

  #Error Handle, NaN and Inf as NA
  x[is.nan(x)] = NA
  x[is.infinite(x)] = NA
  t[is.nan(t)] = NA
  t[is.infinite(t)] = NA

  #Build Data Frame with appropriate time factors  DFxt = data.frame(x = x, t = t)
  DFxt = data.frame(x = x, t = t)
  DFxt$Year = year(DFxt$t)
  DFxt$Month = month(DFxt$t, label = T, abbr = T)

  #execute with different na.rm settings
  if(na.rm) {

    Arng = ddply(DFxt, .(Month), summarize, Arng = abs(diff(range(x, na.rm = T))), N = nonalength(x))

  }else{

    Arng = ddply(DFxt, .(Month), summarize, Arng = abs(diff(range(x, na.rm = F))), N = length(x))

  }

  #error handle missing data to Inf
  Arng$Arng[is.nan(Arng$Arng)] = NA
  Arng$Arng[is.infinite(Arng$Arng)] = NA

  return(mean(Arng$Arng[which(Arng$N > 1)], na.rm = na.rm))

}

mean_biweekly_range = function(x, t, na.rm = T){

  #Error Handle, NaN and Inf as NA
  x[is.nan(x)] = NA
  x[is.infinite(x)] = NA
  t[is.nan(t)] = NA
  t[is.infinite(t)] = NA

  #Build Data Frame with appropriate time factors
  DFxt = data.frame(x = x, t = t)
  DFxt$Year = year(DFxt$t)
  DFxt$Biweek = floor(week(DFxt$t)/2)

  #execute with different na.rm settings
  if(na.rm) {

    Arng = ddply(DFxt, .(Biweek), summarize, Arng = abs(diff(range(x, na.rm = T))), N = nonalength(x))

  }else{

    Arng = ddply(DFxt, .(Biweek), summarize, Arng = abs(diff(range(x, na.rm = F))), N = length(x))

  }

  #error handle missing data to Inf
  Arng$Arng[is.nan(Arng$Arng)] = NA
  Arng$Arng[is.infinite(Arng$Arng)] = NA

  return(mean(Arng$Arng[which(Arng$N > 1)], na.rm = na.rm))

}

mean_weekly_range = function(x, t, na.rm = T){

  #Error Handle, NaN and Inf as NA
  x[is.nan(x)] = NA
  x[is.infinite(x)] = NA
  t[is.nan(t)] = NA
  t[is.infinite(t)] = NA

  #Build Data Frame with appropriate time factors
  DFxt = data.frame(x = x, t = t)
  DFxt$Year = year(DFxt$t)
  DFxt$Week = floor(week(DFxt$t))

  #execute with different na.rm settings
  if(na.rm) {
    Arng = ddply(DFxt, .(Week), summarize, Arng = abs(diff(range(x, na.rm = T))), N = nonalength(x))
  }else{
    Arng = ddply(DFxt, .(Week), summarize, Arng = abs(diff(range(x, na.rm = F))), N = length(x))
  }

  #error handle missing data to Inf
  Arng$Arng[is.nan(Arng$Arng)] = NA
  Arng$Arng[is.infinite(Arng$Arng)] = NA

  return(mean(Arng$Arng[which(Arng$N > 1)], na.rm = na.rm))
}

DHW.Np10y = function(x, t, na.rm = T, threshold = 0, edge = T){
  ev = ts2event(x = x, t = t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(ev)){return(0)} else if (all(is.na(ev))){return(NA)}else {
    ts.years = as.numeric(difftime(max(t, na.rm = na.rm), min(t, na.rm = na.rm), unit = "days")/365.25)
    return(10 * (nrow(ev)/ts.years))
  }
}

DHW.MeanMax = function(x, t, na.rm = T, threshold = 0, edge = T){
  ev = ts2event(x, t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(ev)){return(0)} else if (all(is.na(ev))){return(NA)}else {
    return(mean(ev$event.max, na.rm = na.rm))
  }
}

DHW.CI95Max = function(x, t, na.rm = T, threshold = 0, edge = T){
  ev = ts2event(x, t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(ev)){return(NA)} else if (all(is.na(ev))){return(NA)}else {
    return(CI95(ev$event.max, na.rm = na.rm))
  }
}

DHW.MeanDur = function(x, t, na.rm = T, threshold = 0, edge = T){
  ev = ts2event(x, t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(ev)){return(NA)} else if (all(is.na(ev))){return(NA)}else {
    return(mean(ev$event.dur, na.rm = na.rm))
  }
}

DHW.MaxMax = function(x, t, na.rm = T, threshold = 0, edge = T){
  ev = ts2event(x, t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(ev)){return(0)} else if (all(is.na(ev))){return(NA)}else {
    return(max(ev$event.max, na.rm = na.rm))
  }
}

DHW.YearsToLast= function(x, t, na.rm = T, threshold = 0, edge = T){
  #ev = ts2event(x = x, t = t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(x)){return(0)} else if (all(is.na(x))){return(NA)}else {
    #Figure out last timepoint above threshold
    extra_is=which(x>threshold)
    if(length(extra_is)==0){    # If there is no timepoint above threshold, return timeseries length
      ts.years = as.numeric(difftime(max(t, na.rm = na.rm), min(t, na.rm = na.rm), unit = "days")/365.25)
      return(ts.years)
    }else{ #if there is at least one timepoint above threshold, calc time of most recent and return
      lastx_i=max(extra_is,na.rm=T)
      last.years = as.numeric(difftime(max(t, na.rm = na.rm), t[lastx_i], unit = "days")/365.25)
      return(last.years)
    }
  }
}

DHW.Np10y_Major = function(x, t, na.rm = T, threshold = 4, edge = T){
  ev = ts2event(x = x, t = t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(ev)){return(0)} else if (all(is.na(ev))){return(NA)}else {
    ts.years = as.numeric(difftime(max(t, na.rm = na.rm), min(t, na.rm = na.rm), unit = "days")/365.25)
    return(10 * (nrow(ev)/ts.years))
  }
}

DHW.MeanMax_Major = function(x, t, na.rm = T, threshold = 4, edge = T){
  ev = ts2event(x, t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(ev)){return(0)} else if (all(is.na(ev))){return(NA)}else {
    return(mean(ev$event.max, na.rm = na.rm))
  }
}

DHW.CI95Max_Major = function(x, t, na.rm = T, threshold = 4, edge = T){
  ev = ts2event(x, t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(ev)){return(NA)} else if (all(is.na(ev))){return(NA)}else {
    return(CI95(ev$event.max, na.rm = na.rm))
  }
}

DHW.MeanDur_Major = function(x, t, na.rm = T, threshold = 4, edge = T){
  ev = ts2event(x, t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(ev)){return(NA)} else if (all(is.na(ev))){return(NA)}else {
    return(mean(ev$event.dur, na.rm = na.rm))
  }
}

DHW.MaxMax_Major = function(x, t, na.rm = T, threshold = 4, edge = T){
  ev = ts2event(x, t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(ev)){return(0)} else if (all(is.na(ev))){return(NA)}else {
    return(max(ev$event.max, na.rm = na.rm))
  }
}

DHW.YearsToLast_Major= function(x, t, na.rm = T, threshold = 4, edge = T){
  #ev = ts2event(x = x, t = t, threshold = threshold, na.rm = na.rm, edge = edge)
  if(is.null(x)){return(0)} else if (all(is.na(x))){return(NA)}else {
    #Figure out last timepoint above threshold
    extra_is=which(x>threshold)
    if(length(extra_is)==0){    # If there is no timepoint above threshold, return timeseries length
      ts.years = as.numeric(difftime(max(t, na.rm = na.rm), min(t, na.rm = na.rm), unit = "days")/365.25)
      return(ts.years)
    }else{ #if there is at least one timepoint above threshold, calc time of most recent and return
      lastx_i=max(extra_is,na.rm=T)
      last.years = as.numeric(difftime(max(t, na.rm = na.rm), t[lastx_i], unit = "days")/365.25)
      return(last.years)
    }
  }
}

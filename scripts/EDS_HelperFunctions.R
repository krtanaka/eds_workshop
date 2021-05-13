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

as.nv = function(x){
  return(as.numeric(as.vector(x)))
}

leadz = function(x, n){
  return(formatC(as.numeric(as.vector(x)), width = n, flag = 0))
}

nonalength = function(x){
  length(na.omit(x))
}

CI95 = function(x, na.rm = T){
  n = length(x)
  if(n>0){
    ci = 1.96 * sd(x, na.rm = na.rm)/sqrt(n)
  }else{ci=NA}
  return(ci)
}

pcalc = function(S, v){

  p = rep(NA, length(v))

  for(i in 1:length(v)){
    p[i] = length(which(S < v[i]))/length(S)
  }

  return(p)
}

rollmin = function(x, k, fill = if (na.pad) NA, na.pad = T, align = c("center", "left", "right")){
  return(-rollmax(-(x), k, fill = fill, na.pad = na.pad, align = align))
}

rollrange = function(x, k, fill = if (na.pad) NA, na.pad = T, align = c("center", "left", "right")){
  rng = abs(rollmax(x, k, fill = fill, na.pad = na.pad, align = align)-rollmin(x, k, fill = fill, na.pad = na.pad, align = align))
  return(rng)
}

long180to360 = function(long){
  long360 = long
  long360[long360 < 0] = (long360[long360 < 0])+360
  return(long360)
}

ZoomToIsland = function(ras, Isldf, target){
  thisi = subset(Isldf, ISLAND.CODE == target)
  ex = extent(thisi$LEFT_XMIN, thisi$RIGHT_XMAX , thisi$BOTTOM_YMIN , thisi$TOP_YMAX)
  plot(ras, ext = ex)
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
  if(length(tINT)>0) {
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
  }else if(Nfiles == 1){
    print("single filename provided, copying/renaming file")
    file.copy(from = infilenamelist[[1]], to = outfilename, )
    return(1)
  }else if(Nfiles == 2){
    print("Two filenames provided, calling 'merge_times_nc'")
    status = merge_times_nc(filename1 = infilenamelist[[1]],
                            filename2 = infilenamelist[[2]],
                            variable_name = variable_name,
                            outfilename = outfilename)
    return(1)
  }else if(Nfiles>2){
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
  #Get Dimensions for output
  f1Dims = NcReadDims(template)

  dat_new = data
  dat_new[is.infinite(dat_new)]  = NA
  dat_new[is.nan(dat_new)]  = NA
  eval(parse(text = paste0("var = template$var[variable_name]$", variable_name)))

  #Get dimensions
  Dx = ncdim_def("longitude", "degrees_east", sort(unique(c(template$dim$longitude$vals))))
  Dy = ncdim_def("latitude", "degrees_north", sort(unique(c(template$dim$latitude$vals))))
  Var2d = ncvar_def(name = variable_name, units = var$units, dim = list(Dx, Dy), missval = NA)
  # Create a new file
  file_new  <- nc_create(
    filename = outfile,
    # We need to define the variables here
    vars = Var2d, verbose = F
  )

  #copy over all the metadata, but not the dimensions...
  #modifyNcdfCopyMetadata(file.con.orig = template, file.con.copy = file_new, glob.atts = T, dimensions = F)

  # And write to it
  ncvar_put(
    nc = file_new,
    varid = variable_name,
    vals = dat_new)

  # Finally, close the files
  # nc_close(template)
  nc_close(file_new)
}

ndig = function(x, n){
  return(formatC(x, width = n, flag = 0))
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
  last_st = nv-ns+1
  if(ns>nv){return(NULL)}
  out = NULL
  for (i in 1:last_st){
    if(all(subv == vec[i:(i+ns-1)])){out = c(out, i)}
  }
  return(out)
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

points.in.polys = function(pts.x, pts.y, bb){

  nbox = nrow(bb)
  npts = length(pts.x)
  InMat = matrix(0, ncol = nbox, nrow = npts)
  colnames(InMat) = bb$ISLAND.CODE
  DataISL = rep("NONE_ASSIGNED", npts)

  for(i in 1:nbox){

    InMat[,i] = point.in.polygon(pts.x,pts.y,
                                 bb[i,c("LEFT_XMIN","RIGHT_XMAX","RIGHT_XMAX","LEFT_XMIN")],
                                 bb[i,c("BOTTOM_YMIN","BOTTOM_YMIN","TOP_YMAX","TOP_YMAX")])

    DataISL[which(InMat[,i] >= 1)] = as.vector(bb$ISLAND.CODE[i])

  }

  out = list(DataISL,InMat)
  names(out) = c("DATA_ISLAND","IN_MATRIX")

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


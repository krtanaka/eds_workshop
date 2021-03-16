rm(list=ls())#LIbrary and Supporting Sources

# Load Libraries ----------------------------------------------------------
library(spatial)
library(raster)
library(lubridate)
library(ncdf4)
library(metR)
#source("M:/Environmental Data Summary/HelperCode/ExpandingExtract.R")
source("M:/Environmental Data Summary/HelperCode/EDS_HelperFunctions.R")

#functions
points.in.polys=function(pts.x,pts.y,bb){
  nbox=nrow(bb)
  npts=length(pts.x)
  InMat=matrix(0,ncol=nbox,nrow=npts)
  colnames(InMat)=bb$ISLAND.CODE
  DataISL=rep("NONE_ASSIGNED",npts)
  for(i in 1:nbox){
    InMat[,i]=point.in.polygon(pts.x,pts.y,
                               bb[i,c("LEFT_XMIN","RIGHT_XMAX","RIGHT_XMAX","LEFT_XMIN")],
                               bb[i,c("BOTTOM_YMIN","BOTTOM_YMIN","TOP_YMAX","TOP_YMAX")])
    DataISL[which(InMat[,i]>=1)]=as.vector(bb$ISLAND.CODE[i])
  }
  out=list(DataISL,InMat)
  names(out)=c("DATA_ISLAND","IN_MATRIX")
  return(out)
}
xyt2ijk=function(xyt_df,x_grid,y_grid,t_grid){
  #Make sure data are apples to apples class-wise
  xyt_df=data.frame(x=as.numeric(xyt_df[,1]),
                    y=as.numeric(xyt_df[,2]),
                    t=as.Date(xyt_df[,3]))
  xyt_df$x[xyt_df$x>180]=xyt_df$x[xyt_df$x>180]-360
  x_grid=as.numeric(x_grid)
  x_grid[x_grid>180]=x_grid[x_grid>180]-360
  y_grid=as.numeric(y_grid)
  t_grid=as.Date(t_grid)
  
  #Sizes
  n_pts=nrow(xyt_df)
  n_xg=length(x_grid)
  n_yg=length(y_grid)
  n_tg=length(t_grid)
  
  #X Match
  x_gMat=outer(rep(1,n_pts),x_grid)
  x_pMat=outer(xyt_df$x,rep(1,n_xg))
  x_i=apply(abs(x_pMat-x_gMat),1,which.min)
  #Y Match
  y_gMat=outer(rep(1,n_pts),y_grid)
  y_pMat=outer(xyt_df$y,rep(1,n_yg))
  y_j=apply(abs(y_pMat-y_gMat),1,which.min)
  #T Match
  t_gMat=outer(rep(1,n_pts),t_grid)
  t_pMat=outer(xyt_df$t,rep(1,n_tg))
  t_k=apply(abs(t_pMat-t_gMat),1,which.min)
  ijk=data.frame(x_i,y_j,t_k)
  return(ijk)
}
NAstackcount=function(x){return(length(which(is.na(x))))}

# Read Data Point and Box data --------------------------------------------

#Read In Points Data
#SM=read.csv("C:/Users/Thomas.Oliver/WORK/Git/fish-paste-master/data/SURVEY MASTER.csv")
SM=read.csv("M:/Environmental Data Summary/InSituPointFiles/HCBC_2019_04022020_ObsOnly.csv")
#Assign Distinct LAT LON DATE_R columns
SM$LON=SM$Longitude_DD
SM$LAT=SM$Latitude_DD
SM$DATE_R=mdy(SM$ObservationDate)
#CleanUp
drop_LLNA=unique(c(which(is.na(SM$LON)),which(is.na(SM$LAT))))
if(length(drop_LLNA)>0){SM=SM[-drop_LLNA,]} # Drop unknown locations
dim(SM)

# Read In Island Bounding Boxes
BB_ISL=read.csv("M:/Environmental Data Summary/GeographyInputFiles/Island_10km_Grid_Extents_CSV.csv")

#Build list of parameter targets
paramdir="M:/Environmental Data Summary/DataDownload/"
paramlist=list.files(path = paramdir,full.names = F)
golist=paramlist[c(7)]

#Pull Plan Table
PARAMTAB=read.csv("M:/Environmental Data Summary/ParameterInputFiles/EDS_parameters_OW01.csv")

# Prep For Extractions ----------------------------------------------------

###Locate each point in an island bounding box
PT=points.in.polys(SM$LON,SM$LAT,BB_ISL)
SM$DATA_ISL=PT$DATA_ISLAND
# Drop points outside target area and report
print(paste("Dropping",length(which(SM$DATA_ISL=="NONE_ASSIGNED")),"points of",nrow(SM),"entered points, as outside Geographic Scope"))
SM=subset(SM,DATA_ISL!="NONE_ASSIGNED")


#DEBUG SM=SM[sample(1:nrow(SM),10,replace=FALSE),]

# Run Extractions ----------------------------------------------------
uISL=unique(SM$DATA_ISL)
#Parameter to run
#DEBUG: par_i=1;isl_i=1;sum_i=1;sumpt_i=1
for(par_i in 1:length(golist)){
  #Get Island_Level_Data Directory for this Param
  param.name=golist[par_i]
  this_param_i=which(PARAMTAB$PARAMETER.NAME==param.name)
  godir=paste(paramdir,param.name,"/Island_Level_Data",sep="")
  paramsum=unlist(strsplit(as.vector(PARAMTAB[this_param_i,"Summaries"]),";"))
  
  #For each Island
  for(isl_i in 1:length(uISL)){
    #Subset to ISLAND
    SM_i=which(SM$DATA_ISL==uISL[isl_i])
    #Get ISLAND,PARAM data
    ncfile=list.files(godir,pattern=paste0(uISL[isl_i],"_"),full.names = T)
    nc_p=nc_open(ncfile)
    
    
    
    #pull var array
    rawvar=ncvar_get(nc=nc_p,varid = as.vector(PARAMTAB$GRID.VARIABLE[this_param_i]))#readNcdfVarName(ncfile))
    
    
    #pull dim vectors
    lon=ncvar_get(nc = nc_p,varid = "longitude")
    lat=ncvar_get(nc = nc_p,varid = "latitude")
    rawt=ncvar_get(nc = nc_p,varid = "time")
    
    #close nc
    nc_close(nc_p)
    t=as_date(as_datetime(as.numeric(rawt),origin=ymd("1970/1/1")))
    
    #locate all points in rawvar array
    ijk=xyt2ijk(xyt_df = SM[SM_i,c("LON","LAT","DATE_R")],
                x_grid = lon,
                y_grid = lat,
                t_grid = t)
    
    ####Count NA in var array (will use to solve NA issues)
    naP_xy=aaply(rawvar,c(1,2),NAstackcount)/dim(rawvar)[3]
    #id points sitting on NA-heavy timeseries
    i_masked=which(naP_xy[cbind(ijk$x_i,ijk$y_j)]>.9)
    #Infill selected points with spatial interpolation
    cnt=1
    for(i_infill in i_masked){
      #update NA blocks
      pNA=naP_xy[cbind(ijk$x_i[i_infill],ijk$y_j[i_infill])]
      #selected NA timeseries +/- x pixel steps
      ij_ex=1
      while(pNA>.9&ij_ex<3){
        ij_vec=-ij_ex:ij_ex
        ts=aaply(rawvar[ijk$x_i[i_infill]+ij_vec,ijk$y_j[i_infill]+ij_vec,],c(3),mean,na.rm=T)
        pNA=length(which(is.na(ts)))/length(ts)
        if(pNA<.9){
          #Update rawvar
          rawvar[ijk$x_i[i_infill],ijk$y_j[i_infill],]=ts
          #Update naP
          naP_xy=aaply(rawvar,c(1,2),NAstackcount)/dim(rawvar)[3]
          print(paste("In-fill complete",cnt,"of",length(i_masked),". Pixel +/-",ij_ex))
        }
        ij_ex=ij_ex+1
      }#Close While
      #Fill in interpolated data
      cnt=cnt+1
    }#close infill for
    
    #Set Time Step
    if(PARAMTAB$FREQUENCY[this_param_i]=="Daily"){tstep=1}else if(PARAMTAB$FREQUENCY[this_param_i]=="Monthly"){tstep=30}else{tstep=1}
    
    #TimeSeries Pull Indices
    ijk$t_03mo=ijk$t_k-(90/tstep-1)
    ijk$t_01yr=round(ijk$t_k-(1*365.25/tstep-1))
    ijk$t_03yr=round(ijk$t_k-(3*365.25/tstep-1))
    ijk$t_05yr=round(ijk$t_k-(5*365.25/tstep-1))
    ijk$t_10yr=round(ijk$t_k-(10*365.25/tstep-1))
    ijk[,c("t_k","t_03mo","t_01yr","t_03yr","t_05yr","t_10yr")][which(ijk[,c("t_k","t_03mo","t_01yr","t_03yr","t_05yr","t_10yr")]<1,arr.ind = T)]=1
    
    #Apply Summaries to Timeseries
    for(sum_i in 1:length(paramsum)){
      paramsum.name=paste0(paramsum[sum_i],"_",param.name)
      if(!paramsum.name%in%substr(names(SM),1,nchar(paramsum.name))){
        eval(parse(text=paste0("SM$",paramsum.name,"_MO03=NA")))
        eval(parse(text=paste0("SM$",paramsum.name,"_YR01=NA")))
        eval(parse(text=paste0("SM$",paramsum.name,"_YR03=NA")))
        eval(parse(text=paste0("SM$",paramsum.name,"_YR05=NA")))
        eval(parse(text=paste0("SM$",paramsum.name,"_YR10=NA")))
        eval(parse(text=paste0("SM$",paramsum.name,"_YR10YR01=NA")))
        eval(parse(text=paste0("SM$",paramsum.name,"_ALLB4=NA")))
      }
      #For each point in SM_i
      for(sumpt_i in 1:length(SM_i)){
        ts_03mo=rawvar[ijk$x_i[sumpt_i],ijk$y_j[sumpt_i],ijk$t_03mo[sumpt_i]:ijk$t_k[sumpt_i]]
        ts_01yr=rawvar[ijk$x_i[sumpt_i],ijk$y_j[sumpt_i],ijk$t_01yr[sumpt_i]:ijk$t_k[sumpt_i]]
        ts_03yr=rawvar[ijk$x_i[sumpt_i],ijk$y_j[sumpt_i],ijk$t_03yr[sumpt_i]:ijk$t_k[sumpt_i]]
        ts_05yr=rawvar[ijk$x_i[sumpt_i],ijk$y_j[sumpt_i],ijk$t_05yr[sumpt_i]:ijk$t_k[sumpt_i]]
        ts_10yr=rawvar[ijk$x_i[sumpt_i],ijk$y_j[sumpt_i],ijk$t_10yr[sumpt_i]:ijk$t_k[sumpt_i]]
        ts_10yr01yr=rawvar[ijk$x_i[sumpt_i],ijk$y_j[sumpt_i],ijk$t_10yr[sumpt_i]:ijk$t_01yr[sumpt_i]]
        ts_ALLB4=rawvar[ijk$x_i[sumpt_i],ijk$y_j[sumpt_i],1:ijk$t_k[sumpt_i]]
        t_03mo=t[ijk$t_03mo[sumpt_i]:ijk$t_k[sumpt_i]]
        t_01yr=t[ijk$t_01yr[sumpt_i]:ijk$t_k[sumpt_i]]
        t_03yr=t[ijk$t_03yr[sumpt_i]:ijk$t_k[sumpt_i]]
        t_05yr=t[ijk$t_05yr[sumpt_i]:ijk$t_k[sumpt_i]]
        t_10yr=t[ijk$t_10yr[sumpt_i]:ijk$t_k[sumpt_i]]
        t_10yr01yr=t[ijk$t_10yr[sumpt_i]:ijk$t_01yr[sumpt_i]]
        t_ALLB4=t[1:ijk$t_k[sumpt_i]]
        if(paramsum[sum_i]%in%c("mean","q05","q95")){
          eval(parse(text=paste0("SM$",paramsum.name,"_MO03[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_03mo,na.rm=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_YR01[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_01yr,na.rm=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_YR03[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_03yr,na.rm=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_YR05[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_05yr,na.rm=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_YR10[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_10yr,na.rm=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_YR10YR01[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_10yr01yr,na.rm=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_ALLB4[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_ALLB4,na.rm=T)")))
        }else{
          eval(parse(text=paste0("SM$",paramsum.name,"_MO03[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_03mo,na.rm=T,t=t_03mo,edge=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_YR01[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_01yr,na.rm=T,t=t_01yr,edge=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_YR03[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_03yr,na.rm=T,t=t_03yr,edge=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_YR05[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_05yr,na.rm=T,t=t_05yr,edge=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_YR10[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_10yr,na.rm=T,t=t_10yr,edge=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_YR10YR01[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_10yr01yr,na.rm=T,t=t_10yr01yr,edge=T)")))
          eval(parse(text=paste0("SM$",paramsum.name,"_ALLB4[SM_i[sumpt_i]]=",paramsum[sum_i],"(x=ts_ALLB4,na.rm=T,t=t_ALLB4,edge=T)")))
        }#END if
      }#END Loop over this island's points (for 1:length(SM_i))
      print(paste(uISL[isl_i],paramsum.name,"Done.",isl_i,"of",length(uISL),"islands. Completed",sumpt_i," points..."))
      write.csv(x = SM,file = "M:/Environmental Data Summary/InSituPointFiles/HCBC_TEST.csv")
    }#END Loop over each summary function  (for 1:length(paramsum))
  }#END Loop over each island (for  1:length(uISL))
}#END Loop over each parameter (for  1:length(golist))

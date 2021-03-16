rm(list=ls())
library(raster)
library(sp)
library(rerddap)
library(lubridate)

setwd("C:/Users/Thomas.Oliver/WORK/Projects/PIFSC satellite course/R_scripts/")
ARMS=read.csv("data/ARMS_Points.csv")
ARMS$RecoveryDate=mdy(ARMS$RecoveryDate)
ARMS$DeployDate=mdy(ARMS$DeployDate)
head(ARMS)

table(ARMS$Island)
Wake=subset(ARMS,Island=="Wake")
coordinates(Wake)<- ~Deploy_Longitude+Deploy_Latitude

scale=.05
CW_u='https://coastwatch.pfeg.noaa.gov/erddap/'#https://oceanwatch.pifsc.noaa.gov/erddap/'
ETOPO1_id='etopo180'#'bathymetry_b38e_27c7_f8c3'
ETOPO1_info=info(datasetid = ETOPO1_id,url = CW_u)
WakeBath=griddap(ETOPO1_info,url=CW_u,
                 latitude=(range(Wake$Deploy_Latitude)+c(-1,1)*scale),
                 longitude=(range(Wake$Deploy_Longitude)+c(-1,1)*scale),
                 store=disk(),fmt = "nc")

VIIRS_id='nesdisVHNSQchlaMonthly'
VIIRS_info=info(datasetid = VIIRS_id,url = CW_u)
WakeVIIRS=griddap(VIIRS_info,url=CW_u,
                 latitude=(range(Wake$Deploy_Latitude)+c(-1,1)*scale),
                 longitude=(range(Wake$Deploy_Longitude)+c(-1,1)*scale),
                 time=((c("2016-04-19","2017-04-19"))),
                 store=disk(),fmt = "nc")

#Read netcdf to generate bathymetry raster
rWB=raster(WakeBath$summary$filename)

#Read netcdf to generate chla raster
rVI=stack(WakeVIIRS$summary$filename,varname="chlor_a")
#Take a single timpoint in CHLA Raster layer,
#or mean, or whatever
rVI.1=rVI[[1]]
rVI.mn=mean(rVI,na.rm=T)
rVI.max=max(rVI,na.rm=T)

#Show Rasters
par(mfrow=c(2,1))
plot(rWB,main="ETOPO1 Bathymetry Raster")
contour(rWB,levels=c(-30,-1000,-2000),add=T)
plot(Wake,add=T,pch=16)
plot(log(rVI.mn),main="VIIRS CHLA Raster (log scale)")
plot(Wake,add=T,pch=16)

#example of extract functioning well
Wake$Depth=raster::extract(x=rWB,y=Wake)

#MASKING LAYER
#To generate masking layer, we:
#1.Convert depth raster to a SpatialPointsDataFrame
extent(rWB)=extent(rVI.mn)
spWB=data.frame(rasterToPoints(rWB))
coordinates(spWB)=~x+y
#2.Define a function to consider a pixel necessary to mask
#this one counts pixels below a threshold
count_shallow_pixels=function(depths,threshold=-30,na.rm=T){
  return(length(which(depths>threshold)))}
count_all=function(x,na.rm=T){return(length(x))}
#3.rasterize the points into the Chla grid, using the function to count
#how many (smaller) depth pixels in each (larger) Chla pixel are "too shallow"
rVI.wb_N_ALL=rasterize(x = spWB,y=rVI.mn,field="Altitude",fun=count_all)
rVI.wb_N_SHALLOW=rasterize(x = spWB,y=rVI.mn,field="Altitude",fun=count_shallow_pixels)
#4. show it off
par(mfrow=c(2,1))
plot(rVI.wb_N_SHALLOW,main="N Shallow Pixels")
plot(spWB,add=T)
plot(rVI.wb_N_ALL,main="N Total Pixels")
plot(spWB,add=T)

#5. decide what threshold means a pixel is 'bad', i.e. 2 of 4,
#then make masked CHLA layer
rVI.mnm=rVI.mn
rVI.mnm[rVI.wb_N_SHALLOW>=2]=NA
#show off
par(mfrow=c(1,1))
plot(rVI.mnm,main="Masked VIIRS Chl-A")
plot(Wake,add=T,pch=21)

#This function takes a spatial raster, and a spatial data frame of in situ points
#Then it will fill any NA value in the SpDF with the first-discovered non-NA values from r
lengthNONA=function(x){return(length(x[!is.na(x)]))}
ExpandingExtract=function(r,SpDF,Dists=c(500,1000,2000,4000,8000)){
  OutDF=data.frame(values=rep(NA,nrow(SpDF)),Dist=rep(NA,nrow(SpDF)),N=rep(NA,nrow(SpDF)))
  nDists=length(Dists)
  cnt=1
  NAi=which(is.na(OutDF$values))
  NAsLeft=length(NAi)>0
  while(cnt<=nDists&NAsLeft){
    NAi=which(is.na(OutDF$values))
    pull=raster::extract(x=r,y=SpDF[NAi,],
                         buffer=Dists[cnt],
                         small=TRUE,
                         na.rm=TRUE)
    Nper=unlist(lapply(pull,lengthNONA))
    OutDF$values[NAi]=unlist(lapply(pull,mean,na.rm=TRUE))
    OutDF$Dist[NAi]=Dists[cnt]
    OutDF$N[NAi]=Nper
    NAi=which(is.na(OutDF$values))
    NAsLeft=length(NAi)>0
    cnt=cnt+1
  }
  return(OutDF)
}

#Call Expanding Extract
EEdf=ExpandingExtract(rVI.mnm,Wake,Dists=c(500,1000,2000,4000,4500))
EEdf

Wake$VIIRS_CHLA=EEdf$values
Wake$VIIRS_CHLA_nomask=raster::extract(rVI.mn,Wake)

library(ggplot2)
library(ggspatial)

Wake.=data.frame(coordinates(Wake),Wake@data)
WakeCHLA=ggplot(Wake.,aes(Deploy_Longitude,Deploy_Latitude,
                 color=log(VIIRS_CHLA),
                 size=log(VIIRS_CHLA)))+
  geom_osm()+
  geom_point()+
  scale_color_viridis_c(name="ARMS Site Chl-A")+
  scale_size_continuous(name="ARMS Site Chl-A")+
  coord_map()
WakeCHLA
WakeCHLA_BAD=ggplot(Wake.,aes(Deploy_Longitude,Deploy_Latitude,
                          color=log(VIIRS_CHLA_nomask),
                          size=log(VIIRS_CHLA_nomask)))+
  geom_osm()+
  geom_point()+
  scale_color_viridis_c(name="BAD ARMS Site Chl-A")+
  scale_size_continuous(name="BAD ARMS Site Chl-A")+
  coord_map()
WakeCHLA_BAD
library(ggarrange)
ggsave("WakeCHLA_MASKED.png",WakeCHLA)
ggsave("WakeCHLA_BAD.png",WakeCHLA_BAD)

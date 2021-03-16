# Library, Functions ------------------------------------------------------
library(spatial)
library(raster)
library(ggplot2)
library(ggspatial)
library(ggrepel)
library(ggthemes)
library(plyr)
source("M:/Environmental Data Summary/HelperCode/prioritize_on.R")
source("M:/Environmental Data Summary/HelperCode/better.biplot.R")
source("M:/Environmental Data Summary/HelperCode/gcdist.R")
#This function takes a spatial raster, and a spatial data frame of in situ points
#Then it will fill any NA value in the SpDF with the first-discovered non-NA values from r
lengthNONA=function(x){return(length(x[!is.na(x)]))}
ExpandingExtract=function(r,SpDF,Dists=c(0,500,1000,2000,4000,8000)){
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

long180to360=function(long){
  long360=long
  long360[long360<0]=(long360[long360<0])+360
  return(long360)
}


ZoomToIsland=function(ras,Isldf,target){
  thisi=subset(Isldf,ISLAND.CODE==target)
  ex=extent(thisi$LEFT_XMIN,thisi$RIGHT_XMAX ,thisi$BOTTOM_YMIN ,thisi$TOP_YMAX)
  plot(ras,ext=ex)
}
# Load Data ---------------------------------------------------------------
Isl=read.csv("M:/Environmental Data Summary/GeographyInputFiles/Island_10km_Grid_Extents_CSV.csv")
FSM=read.csv("M:/Environmental Data Summary/InSituPointFiles/ALLRAMP_Mosaic_Metadata_v2.01.csv")
#Drop no Lat/Lon
purge=which(is.na(FSM$Latitude))
if (length(purge)>0) FSM=FSM[-purge,]

FSM1=ddply(FSM,.(Region,Island.Code,CleanID),summarize,
           Longitude=mean(Longitude),
           Latitude=mean(Latitude),
           N=length(LL))
FSM1$Long360=long180to360(FSM1$Longitude)
hh=hist(FSM1$N,breaks=0:10-.5)
sum(hh$counts[hh$mids>1])
#Collapse Time Points

print("Gathering rasters in...")



# Gather Rasters ----------------------------------------------------------
#Rasters
ETOPO1=raster("M:/Environmental Data Summary/DataDownload/Bathymetry_ETOPO1/Bathymetry_ETOPO1_Bathy_M_AllIslands.nc")
STRM15=raster("M:/Environmental Data Summary/DataDownload/Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands_Long360.nc")

#SST
SSTmn=raster("M:/Environmental Data Summary/DataDownload/SST_CRW_Daily/Domain_Level_Data/mean/SST_CRW_Daily_mean_1985-01-01_2019-12-31_AllIslands.nc")
SSTmax=raster("M:/Environmental Data Summary/DataDownload/SST_CRW_Daily/Domain_Level_Data/q95/SST_CRW_Daily_q95_1985-01-01_2019-12-31_AllIslands.nc")
SSTwk=raster("M:/Environmental Data Summary/DataDownload/SST_CRW_Daily/Domain_Level_Data/mean_weekly_range/SST_CRW_Daily_mean_weekly_range_1985-01-01_2019-12-31_AllIslands.nc")
SSTbiwk=raster("M:/Environmental Data Summary/DataDownload/SST_CRW_Daily/Domain_Level_Data/mean_biweekly_range/SST_CRW_Daily_mean_biweekly_range_1985-01-01_2019-12-31_AllIslands.nc")
SSTann=raster("M:/Environmental Data Summary/DataDownload/SST_CRW_Daily/Domain_Level_Data/mean_annual_range/SST_CRW_Daily_mean_annual_range_1985-01-01_2019-12-31_AllIslands.nc")


#CHLA
CHLAmn=raster("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean/Chlorophyll_A_ESAOCCCI_Daily_mean_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
CHLAmax=raster("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/q95/Chlorophyll_A_ESAOCCCI_Daily_q95_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
CHLAbiwk=raster("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_biweekly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_biweekly_range_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
CHLAmon=raster("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_monthly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_monthly_range_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
CHLAann=raster("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_annual_range/Chlorophyll_A_ESAOCCCI_Daily_mean_annual_range_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")


#DHW
Np=raster("M:/Environmental Data Summary/DataDownload/Degree_Heating_Weeks/Domain_Level_Data/DHW.Np10y_Major/Degree_Heating_Weeks_DHW.Np10y_Major_1985-03-25_2019-12-31_AllIslands.nc")
mnMax=raster("M:/Environmental Data Summary/DataDownload/Degree_Heating_Weeks/Domain_Level_Data/DHW.MaxMax_Major/Degree_Heating_Weeks_DHW.MaxMax_Major_1985-03-25_2019-12-31_AllIslands.nc")
CI95Max=raster("M:/Environmental Data Summary/DataDownload/Degree_Heating_Weeks/Domain_Level_Data/DHW.CI95Max_Major/Degree_Heating_Weeks_DHW.CI95Max_Major_1985-03-25_2019-12-31_AllIslands.nc")
Np_min=raster("M:/Environmental Data Summary/DataDownload/Degree_Heating_Weeks/Domain_Level_Data/DHW.Np10y/Degree_Heating_Weeks_DHW.Np10y_1985-03-25_2019-12-31_AllIslands.nc")
mnMax_min=raster("M:/Environmental Data Summary/DataDownload/Degree_Heating_Weeks/Domain_Level_Data/DHW.MeanMax/Degree_Heating_Weeks_DHW.MeanMax_1985-03-25_2019-12-31_AllIslands.nc")
CI95Max_min=raster("M:/Environmental Data Summary/DataDownload/Degree_Heating_Weeks/Domain_Level_Data/DHW.CI95Max/Degree_Heating_Weeks_DHW.CI95Max_1985-03-25_2019-12-31_AllIslands.nc")

print("all rasters in...extracting ")
FSM1.180=FSM1
FSM1.360=FSM1
coordinates(FSM1.360) <- ~Long360+Latitude
coordinates(FSM1.180) <- ~Longitude+Latitude
crs(FSM1.180)=crs(Np)
crs(FSM1.360)=crs(Np)
FSM1$Depth_ETOPO1=ExpandingExtract(r = ETOPO1,SpDF = FSM1.360)$values
FSM1$Depth_ETOPO1ed=FSM1$Depth_ETOPO1
FSM1$Depth_ETOPO1ed[FSM1$Depth_ETOPO1ed>=0]=-1
FSM1$Depth_ETOPO1ed[FSM1$Depth_ETOPO1ed<(-33)]=-33
par(mfrow=c(1,2))
hist(FSM1$Depth_ETOPO1ed,breaks=seq(from = 0,to = -33,by = -3)-.5,freq=F,ylim=c(0,.15))

FSM1$Depth_STRM15=ExpandingExtract(r = STRM15, SpDF = FSM1.180)$values
FSM1$Depth_STRM15ed=FSM1$Depth_STRM15
FSM1$Depth_STRM15ed[FSM1$Depth_STRM15ed>=0]=-1
FSM1$Depth_STRM15ed[FSM1$Depth_STRM15ed<(-33)]=-33
hist(FSM1$Depth_STRM15ed,breaks=seq(from = 0,to = -33,by = -3)-.5,freq=F,ylim=c(0,.15))
#this says to me (Tom O.) use STRM15 to mask

FSM1$DHW_Np10Y_Maj=ExpandingExtract(r = Np, SpDF = FSM1.360)$values
FSM1$DHW_MnMax_Maj=ExpandingExtract(r = mnMax, SpDF = FSM1.360)$values
FSM1$DHW_CI95Max_Maj=ExpandingExtract(r = CI95Max, SpDF = FSM1.360)$values
FSM1$DHW_Np10Y_Min=ExpandingExtract(r = Np_min, SpDF = FSM1.360)$values
FSM1$DHW_MnMax_Min=ExpandingExtract(r = mnMax_min, SpDF = FSM1.360)$values
FSM1$DHW_CI95Max_Min=ExpandingExtract(r = CI95Max_min, SpDF = FSM1.360)$values
FSM1$SST_mn=ExpandingExtract(r = SSTmn, SpDF = FSM1.360)$values
FSM1$SST_max=ExpandingExtract(r = SSTmax, SpDF = FSM1.360)$values
FSM1$SST_wkRange=ExpandingExtract(r = SSTwk, SpDF = FSM1.360)$values
FSM1$SST_biwkRange=ExpandingExtract(r = SSTbiwk, SpDF = FSM1.360)$values
FSM1$SST_annRange=ExpandingExtract(r = SSTann, SpDF = FSM1.360)$values
FSM1$CHLA_mn=ExpandingExtract(r = CHLAmn, SpDF = FSM1.360)$values
FSM1$CHLA_max=ExpandingExtract(r = CHLAmax, SpDF = FSM1.360)$values
FSM1$CHLA_biwkRange=ExpandingExtract(r = CHLAbiwk, SpDF = FSM1.360)$values
FSM1$CHLA_monRange=ExpandingExtract(r = CHLAmon, SpDF = FSM1.360)$values
FSM1$CHLA_annRange=ExpandingExtract(r = CHLAann, SpDF = FSM1.360)$values

FSM1.=as.data.frame(FSM1)
FSMn=subset(FSM1.,N>1)
#Prioritize On Frequency:
Prior=prioritize_on(df=FSMn,on=c("DHW_Np10Y_Min"),
                    off=c("DHW_MnMax_Min","Depth_STRM15ed","CHLA_mn","CHLA_annRange"),
                    Nselect = 30,
                    Nsample = nrow(FSMn),
                    Ntries = 50,
                    seed = which(FSMn$CleanID=="PHR-OCC-016"))

PriorDF=data.frame(Prior,SiteName=FSMn$CleanID[Prior])
PriorSites=PriorDF$SiteName
write.csv(data.frame(PriorSites),"M:/Environmental Data Summary/Saved Files/SitePriorityList.csv")
#Regional In-Situ
MARAMP=as.data.frame(subset(FSM1.,Region%in%c("GCNMI")))
HARAMP=as.data.frame(subset(FSM1.,Region%in%c("NWHI","MHI")))

print("all extraction in... plotting ")


DeliniatedSites=c("OAH-OCC-010",
                  "OAH-OCC-005",
                  "MAI-OCC-002")
AlignedSites=c("OAH-OCC-010",
               "OAH-OCC-005",
               "MAI-OCC-002",
               "FFS-OCC-002",
               "MOL-SIO-908",
               "KAH-SIO-S01",
               "LIS-OCC-002",
               "LIS-OCC-005",
               "MAU-SIO-001")

DelS=FSM1.[match(DeliniatedSites,FSM1.$CleanID),]
AliS=FSM1.[match(AlignedSites,FSM1.$CleanID),]
PriS=FSM1.[match(PriorSites,FSM1.$CleanID),]

FSMn2p=subset(FSM1.,N>1)
DHW_fs_ALL=ggplot(FSMn2p,aes(x=DHW_Np10Y_Min,
                         y=DHW_MnMax_Min,
                         color=Island.Code,
                         shape=Region,
                         label=paste0(CleanID," - ",N)))+
  geom_point(data=FSM1.,size=2,color="grey")+
  geom_point(aes(size=N))+
  geom_point(data=AliS,size=10,pch=1,color="red")+
  geom_point(data=DelS,size=10,pch=4,color="red")+
  geom_point(data=PriS,size=9,pch=1,color="green")+
  geom_text(data=PriS,aes(label=1:nrow(PriS)),size=3,pch=1,nudge_x = .1,nudge_y = .1,color="darkgreen")+
  geom_text_repel(data=FSMn2p,size=3)+
  xlab("Frequency (# per Decade)")+
  ylab("Event Severity (Mean of Events' Max DHW)")+
  theme_bw()+ggtitle("DHW History : All Events")+
  scale_x_continuous(limits=c(0,8))+
  scale_y_continuous(limits=c(0,10))+
  scale_size_continuous(range=c(3,10)/2)

DHW_fs_MAJ=ggplot(FSMn2p,aes(x=DHW_Np10Y_Maj,
                             y=DHW_MnMax_Maj,
                             color=Island.Code,
                             shape=Region,
                             label=paste0(CleanID," - ",N)))+
  geom_point(data=FSM1.,size=2,color="grey")+
  geom_point(aes(size=N))+
  geom_point(data=AliS,size=10,pch=1,color="red")+
  geom_point(data=DelS,size=10,pch=4,color="red")+
  geom_text_repel(data=FSMn2p,size=3)+
  xlab("Frequency (# per Decade)")+
  ylab("Event Severity (Mean of Events' Max DHW)")+
  theme_bw()+ggtitle("DHW History : Events > 4 DHW")+
  scale_x_continuous(limits=c(0,3.5))+
  scale_y_continuous(limits=c(0,32))+
  scale_size_continuous(range=c(3,10))

BOTH=grid.arrange(DHW_fs_ALL,DHW_fs_MAJ,nrow=2)
ggsave(plot = DHW_fs_ALL,filename = "M:/Environmental Data Summary/Saved Files/TargetSheet_DHW_ALL_NOJAR.png",width=18,height=9)
ggsave(plot = DHW_fs_MAJ,filename = "M:/Environmental Data Summary/Saved Files/TargetSheet_DHW_MAJ.png",width=18,height=9)
ggsave(plot = BOTH,filename = "M:/Environmental Data Summary/Saved Files/TargetSheet_DHW_BOTH.png",width=18,height=18)
# 
# 
# 
# ggplot(FSM1.,aes(x=SST_mn+0.1*runif(nrow(FSM1.)),y=DHW_MnMax_Min+0.1*runif(nrow(FSM1.)),
#                 #ymin=DHW_MnMax_Min-DHW_CI95Max_Min,
#                 #ymax=DHW_MnMax_Min+DHW_CI95Max_Min,
#                 color=Region,label=Island.Code))+
#   #geom_linerange()+
#   geom_point()+
#   geom_text(size=4)+
#   xlab("Mean SST")+
#   ylab("Event Severity (Mean Max Event DHW, CI95)")
# 
# 
# ggplot(FSM1.,aes(x=DHW_MnMax_Min,
#                 y=CHLA_max,
#                 #ymin=DHW_MnMax_Min-DHW_CI95Max_Min,
#                 #ymax=DHW_MnMax_Min+DHW_CI95Max_Min,
#                 color=Region,label=Island.Code))+
#   #geom_linerange()+
#   geom_point()+
#   geom_text()+
#   xlab("DHW Severity")+
#   ylab("CHLA Max")+
#   scale_y_log10()
# 
# ggplot(FSM1.,aes(x=DHW_Np10Y_Min,
#                 y=CHLA_max,
#                 #ymin=DHW_MnMax_Min-DHW_CI95Max_Min,
#                 #ymax=DHW_MnMax_Min+DHW_CI95Max_Min,
#                 color=Region,label=Island.Code))+
#   #geom_linerange()+
#   geom_point()+
#   geom_text()+
#   xlab("DHW Freq")+
#   ylab("CHLA Max")+
#   scale_y_log10()
# 
# ggplot(HARAMP,aes(x=DHW_Np10Y_Maj,
#                 y=DHW_MnMax_Maj,
#                 #ymin=DHW_MnMax_Min-DHW_CI95Max_Min,
#                 #ymax=DHW_MnMax_Min+DHW_CI95Max_Min,
#                 color=Island.Code,label=CleanID))+
#   #geom_linerange()+
#   geom_point()+
#   geom_text_repel(size=2)+
#   xlab("DHW Freq")+
#   ylab("DHW Sev")+
#   scale_y_sqrt()+
#   scale_x_sqrt()
#   

rm(list=ls())
library(ncdf4)
library(reshape2)
library(lubridate)
library(plyr)
Idf=read.csv("M:/Environmental Data Summary/Island_10km_Grid_Extents_CSV.csv")

uREG=unique(Idf$REGION)

thisparam="analysed_sst"
paraminpath="M:/Environmental Data Summary/DataDownload/SST_CRW_Daily/Island_Level_Data/"
paramoutpath="M:/Environmental Data Summary/DataDownload/SST_CRW_Daily/Island_Level_Data/"
fl=list.files(paraminpath,pattern=".nc")
ifl=matrix(unlist(strsplit(fl,"_")),ncol=6,byrow=T)[,1]

regoutpath="M:/Environmental Data Summary/DataDownload/SST_CRW_Daily/Region_Level_Data/"
domoutpath="M:/Environmental Data Summary/DataDownload/SST_CRW_Daily/Domain_Level_Data/"
if(!dir.exists(regoutpath)){dir.create(regoutpath)}
if(!dir.exists(domoutpath)){dir.create(domoutpath)}



#Run through both strucutures to provide complete timepoint vector
uTIMES=NULL
for(REGi in 1:length(uREG)){
  thisREG_islandlist=as.vector(subset(Idf,REGION==uREG[REGi])$ISLAND.CODE)
  REGfl=fl[match(thisREG_islandlist,ifl)]
  for(ISLi in 1:length(thisREG_islandlist)){
    thisnc=nc_open(paste0(paraminpath,REGfl[ISLi]))
    thist=ymd("1970-01-01")+ncvar_get(thisnc,"time")/(60*60*24)
    if(ISLi==1) {uTIMES=thist}else{uTIMES=unique(c(uTIMES,thist))}
    print(length(thist))
    print(length(uTIMES))
    nc_close(thisnc)
  }
}

for(REGi in 1:length(uREG)){
  theseISLs=as.vector(subset(Idf,REGION==uREG[REGi])$ISLAND.CODE)
  REGfl=fl[match(theseISLs,ifl)]
  #open each island file, hold xyt in memory
  REGts=data.frame(time=uTIMES,matrix(rep(NA,prod(length(uTIMES),length(REGfl))),ncol=length(REGfl)))
  names(REGts)=c("time",theseISLs)
  for(ISLi in 1:length(REGfl)){
    thisisl_nc=nc_open(paste0(paraminpath,REGfl[ISLi]))
    thisisl_xyt=ncvar_get(thisisl_nc,thisparam)
    #get island-level mean
    thisisl_t_Val=apply(FUN = mean,MARGIN = c(3),X = thisisl_xyt,na.rm=T)
    thisisl_t=ymd("1970-01-01")+ncvar_get(thisisl_nc,"time")/(60*60*24)
    REGts[match(thisisl_t,REGts$time),theseISLs[ISLi]]=thisisl_t_Val
    print(ISLi)
  }
  REGts$ALL=apply(X = REGts[,theseISLs],MARGIN = 1,FUN = mean,na.rm=T)
  write.csv(REGts,paste0(regoutpath,"/",uREG[REGi],"_DHW_Timeseries.csv"))
  print(paste0(uREG[REGi]))
}

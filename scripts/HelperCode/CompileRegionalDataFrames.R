closeAllConnections()
rm(list=ls())
library(ncdf4)
library(ncdf.tools)
#closeAllNcfiles()
library(reshape2)
library(lubridate)
library(plyr)
Idf=read.csv("M:/Environmental Data Summary/Island_10km_Grid_Extents_CSV.csv")
Param=read.csv("M:/Environmental Data Summary/EDS_parameters_OW01.csv")
Param=subset(Param,DOWNLOAD=="YES"&FREQUENCY!="Climatology")
uREG=unique(Idf$REGION)

for(iP in 1:nrow(Param)){
  print(paste0("Starting with "),Param$PARAMETER.NAME[iP])
  thisparam=Param$GRID.VARIABLE[iP]#"degree_heating_week"
  paraminpath=paste0("M:/Environmental Data Summary/DataDownload/",Param$PARAMETER.NAME[iP],"/Island_Level_Data/")
  paramoutpath=paraminpath
  fl=list.files(paraminpath,pattern=".nc")
  ifl=matrix(unlist(strsplit(fl,"_")),ncol=6,byrow=T)[,1]
  
  regoutpath=paste0("M:/Environmental Data Summary/DataDownload/",Param$PARAMETER.NAME[iP],"Region_Level_Data/")
  domoutpath=paste0("M:/Environmental Data Summary/DataDownload/",Param$PARAMETER.NAME[iP],"Domain_Level_Data/")
  if(!dir.exists(regoutpath)){dir.create(regoutpath)}
  if(!dir.exists(domoutpath)){dir.create(domoutpath)}
  
  #Run through both strucutures to provide outfile size
  sizedf=data.frame(Idf[,c("REGION","ISLAND.CODE")],x=NA,y=NA,t=NA)
  for(REGi in 1:length(uREG)){
    thisREG_islandlist=as.vector(subset(Idf,REGION==uREG[REGi])$ISLAND.CODE)
    REGfl=fl[match(thisREG_islandlist,ifl)]
    for(ISLi in 1:length(thisREG_islandlist)){
      thisnc=nc_open(paste0(paraminpath,REGfl[ISLi]))
      sizedf$x[which(sizedf$ISLAND.CODE==thisREG_islandlist[ISLi])]=thisnc$dim$longitude$len
      sizedf$y[which(sizedf$ISLAND.CODE==thisREG_islandlist[ISLi])]=thisnc$dim$latitude$len
      sizedf$t[which(sizedf$ISLAND.CODE==thisREG_islandlist[ISLi])]=thisnc$dim$time$len
    }
  }
  sizedf$n=apply(sizedf[,c("x","y","t")],1,prod)
  regsizedf=ddply(sizedf,.(REGION),summarize,N=sum(n))
  domsize=sum(regsizedf$N)
  DFcolnames=c("REGION","ISLAND.CODE","Longitude","Latitude","Time",thisparam)
  
  DOMAIN=FALSE
  if(DOMAIN){
    print("Pre-allocating Domain Memory")
    thisdomiandf=data.frame(matrix(rep(NA,domsize*6),ncol=6))
    print("Done.")
    names(thisdomiandf)=DFcolnames
    domrowi=1
  }
  for(REGi in 1:length(uREG)){
    regrowi=1
    print(paste0("Starting ",uREG[REGi]))
    thisREG_islandlist=as.vector(subset(Idf,REGION==uREG[REGi])$ISLAND.CODE)
    REGfl=fl[match(thisREG_islandlist,ifl)]
    print("Pre-allocating Region Memory")
    thisregdf=data.frame(matrix(rep(NA,regsizedf[which(regsizedf$REGION==uREG[REGi]),"N"]*6),ncol=6))
    names(thisregdf)=DFcolnames
    print("Done Pre-allocating Region Memory")
    print(paste0("Successively Loading ",length(thisREG_islandlist)," islands: ",paste(thisREG_islandlist,collapse=", ")))
    #Build Data.Frame
    for(ISLi in 1:length(thisREG_islandlist)){
      thisnc=nc_open(paste0(paraminpath,REGfl[ISLi]))
      thisx=ncvar_get(thisnc,"longitude")
      thisy=ncvar_get(thisnc,"latitude")
      thist=ymd("1970-01-01")+ncvar_get(thisnc,"time")/(60*60*24)
      thisv=ncvar_get(thisnc,thisparam)
      
      thisdf=melt(thisv)
      thisdf$Longitude=thisx[thisdf$Var1]
      thisdf$Latitude=thisy[thisdf$Var2]
      thisdf$Time=thist[thisdf$Var3]
      thisdf$REGION=uREG[REGi]
      thisdf$ISLAND.CODE=thisREG_islandlist[ISLi]
      eval(parse(text=paste0("thisdf$",thisparam,"=thisdf$value")))
      thisregdf[regrowi:((regrowi-1)+nrow(thisdf))]=thisdf[,DFcolnames]
      regrowi=regrowi+nrow(thisdf)
      #now ditch thisdf
      rm(list="thisdf")
      print(paste0("Done with ",thisREG_islandlist[ISLi]))
    }
    starttime=date(min(thisregdf$Time))
    stoptime=date(max(thisregdf$Time))
    write.csv(x = thisregdf,
              file = paste0(regoutpath,thisparam,"_",uREG[REGi],"_",starttime,"_",stoptime,"_DataFrame.csv"))
    
    if(DOMAIN){
      thisdomaindf[domrowi:((domrowi-1)+nrow(thisregdf))]=thisregdf
      domrowi=(domrowi+nrow(thisregdf))
    }
    
    #now ditch regfile
    rm(list="thisregdf")
    print(paste0("Done with ",uREG[REGi]))
  }
  
  if(DOMAIN){
    starttime=date(min(thisdomaindf$Time))
    stoptime=date(max(thisdomaindf$Time))
    write.csv(x = thisdomaindf,
              file = paste0(domoutpath,thisparam,"_FullDomain_",starttime,"_",stoptime,"_DataFrame.csv"))
  }
  print(paste0("Done with "),Param$PARAMETER.NAME[iP])
  
}
print(paste0("Done"))


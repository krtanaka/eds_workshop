#install.packages("RNetCDF")
library(RNetCDF)

#Open data file
fname<-"/Users/thomas.oliver/Downloads/erdMH1chlamday_aefc_104b_f1d9.nc"
fid<-open.nc(fname)

#Print dataset summary information (see Figure 1)
print.nc(fid)

#Read data
dat=read.nc(fid)

#Close file
close.nc(fid)

# netcdf2df=function(){
#   library(reshape2)
# }


length(dat)
names(dat)
time=dat$time#[["time"]]
lat=dat$latitude
lon=dat$longitude
chl=dat$chlorophyll

library(ggplot2)
library(reshape2)
library(animation)

m1 <- chl
tdm=melt(m1)
tdm$lon=lon[tdm$Var1];tdm$Var1=NULL
tdm$lat=lat[tdm$Var2];tdm$Var2=NULL
tdm$time=as.POSIXct(time[tdm$Var3],origin="1970-01-01");
names(tdm)[which(names(tdm)=="value")]="chl"
tdm$chl=as.numeric(as.vector(tdm$chl))
head(tdm)

path="/Users/thomas.oliver/WORK/CRED_WORK/CAU/CAU\ Maps/Animations/NWHI/"
LisLat=26.02
LisLon=-173.95
N=unique(sort(tdm$Var3))
for(i in N){
  df=subset(tdm,Var3==i)
  chlaP=ggplot(df, aes(x = lon, y = lat)) +
    geom_raster(aes(fill=chl))+
    labs(x="Longitude",y="Latitude")+
    ggtitle(paste("NorthWest Hawaiian Islands Monthly Chl-A:",format(df$time[1],"%Y/%m")))+
    coord_fixed(ratio=169/337)+
    scale_fill_continuous(low="lightblue",high="darkgreen",name="Chl-A (mg/m3)",limits=c(quantile(tdm$chl,.001,na.rm=TRUE),quantile(tdm$chl,.999,na.rm=TRUE)))+
    geom_point(aes(x=LisLon,y=LisLat),size=20,shape=1)
  
  fname=paste(path,"NWHI_CHLA_Movie_",i,"of",length(N),".jpg",sep="")
  ggsave(filename = fname,plot = chlaP,device = "jpg",width=8.5,height=8.5*length(lat)/length(lon))
  print(paste(i,"in",length(N)))
}


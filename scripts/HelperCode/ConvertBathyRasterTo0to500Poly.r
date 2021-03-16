library(raster)
library(spatial)
library(ncdf4)
library(rgdal)

Br=raster("M:/Environmental Data Summary/FinalRasters/NC/Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands.nc")
#Be=extent(c(-156.2,-154.6,18.75,20.4))
#BrMHI=crop(x = Br,y = Be)
#plot(BrMHI)

Br500=Br
Br500[Br<(-500)]=NA
Br500[Br>(0)]=NA
Bp500=rasterToPolygons(x = Br500,n=8)
plot(Br500)
plot(Bp500)
outpath="M:/Environmental Data Summary/FinalRasters/SHP/Bathymetry_SRTM15"
if(!dir.exists(outpath)){dir.create(outpath)}
writeOGR(obj = Bp500,dsn = outpath,layer = "Bathymetry_SRTM15_Bathy_000to500M_AllIslands",
         driver = "ESRI Shapefile",verbose=T)

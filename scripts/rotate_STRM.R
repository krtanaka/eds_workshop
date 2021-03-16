rm(list = ls())

library(raster)

dir = "M:/Environmental Data Summary/DataDownload"
dir = "G:/Environmental Data Summary/DataDownload/"

STRM15 = raster(paste0(dir, "Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands.nc"))
STRM15 = shift(rotate(shift(STRM15, 180)), 180); beepr::beep(2)
writeRaster(x = STRM15, paste0(dir, "Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands_Long360.nc"), overwrite = T)
#STRM15_180 = rotate(STRM15)

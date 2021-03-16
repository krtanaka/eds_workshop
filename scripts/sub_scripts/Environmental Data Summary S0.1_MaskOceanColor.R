# # Try Out Masking Ocean Color ---------------------------------------------
maskfun=function(x,na.rm=F,depth_threshold=-30,percent_threshold=5){
  denom=length(x)
  numer=length(which(x>depth_threshold))
  outp=numer/denom
  if(outp>(percent_threshold/100)){return(NA)}else{return(1)}
}
# Do it -------------------------------------------------------------------
#STRM15=raster("M:/Environmental Data Summary/DataDownload/Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands.nc")
#STRM15 <- shift(rotate(shift(STRM15, 180)), 180)
#writeRaster(x = STRM15,"M:/Environmental Data Summary/DataDownload/Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands_Long360.nc")
STRM15=raster("M:/Environmental Data Summary/DataDownload/Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands_Long360.nc")
#STRM15_180=rotate(STRM15)
STRM15_180=raster("M:/Environmental Data Summary/DataDownload/Bathymetry_SRTM15/Bathymetry_SRTM15_Bathy_M_AllIslands.nc")
#VIIRS PAR
vPARmn=raster("M:/Environmental Data Summary/DataDownload/PAR_VIIRS_8day_2019/Domain_Level_Data/mean/PAR_VIIRS_8day_2019_mean_2019-07-24_2019-11-21_AllIslands.nc")

#Crop Rotated STRM15, convert to SPDF
STRM15c=crop(STRM15_180,extent(vPARmn))
STRM15SPd=data.frame(rasterToPoints(STRM15c))
coordinates(STRM15SPd)<- ~x+y
crs(STRM15SPd)=crs(vPARmn)
#Build and Apply Mask
rMASK_vPAR=rasterize(x = STRM15SPd,y=vPARmn,vals="Altitude",fun=maskfun)
vPARmn_mask=mask(vPARmn,rMASK_vPAR)$layer.2
writeRaster(vPARmn_mask,"M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean/PAR_VIIRS_8day_2019_mean_2019-07-24_2019-11-21_AllIslands_30meter5pctmaskSTRM15.nc")


#VIIRS PAR
vKdPARmn=raster("M:/Environmental Data Summary/DataDownload/kdPAR_VIIRS_weekly_2019/Domain_Level_Data/mean/kdPAR_VIIRS_weekly_2019_mean_2019-07-23_2019-11-19_AllIslands.nc")

#Crop Rotated STRM15, convert to SPDF
# STRM15c=crop(STRM15_180,extent(vPARmn))
# STRM15SPd=data.frame(rasterToPoints(STRM15c))
# coordinates(STRM15SPd)<- ~x+y
# crs(STRM15SPd)=crs(vPARmn)
#Build and Apply Mask
rMASK_vKdPAR=rasterize(x = STRM15SPd,y=vKdPARmn,vals="Altitude",fun=maskfun)
vKdPARmn_mask=mask(vKdPARmn,rMASK_vKdPAR)$layer.2
writeRaster(vKdPARmn_mask,"M:/Environmental Data Summary/DataDownload/kdPAR_VIIRS_weekly_2019/Domain_Level_Data/mean/kdPAR_VIIRS_weekly_2019_mean_2019-07-23_2019-11-19_AllIslands_30meter5pctmaskSTRM15.nc")


# 
# CHLAmn=raster("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean/Chlorophyll_A_ESAOCCCI_Daily_mean_1997-09-04_2018-10-28_AllIslands.nc")
# rMASK_CHLA=rasterize(x = STRM15SPd,y=CHLAmn,vals="Altitude",fun=maskfun)
# CHLAmn_mask=mask(CHLAmn,rMASK_CHLA)$layer.2
# writeRaster(CHLAmn_mask,"M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean/Chlorophyll_A_ESAOCCCI_Daily_mean_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
# 
# CHLAmax=raster("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/q95/Chlorophyll_A_ESAOCCCI_Daily_q95_1997-09-04_2018-10-28_AllIslands.nc")
# CHLAmax_mask=mask(CHLAmax,rMASK_CHLA)$layer.2
# writeRaster(CHLAmax_mask,"M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/q95/Chlorophyll_A_ESAOCCCI_Daily_q95_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
# 
# CHLAbiwk=raster("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_biweekly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_biweekly_range_1997-09-04_2018-10-28_AllIslands.nc")
# CHLAbiwk_mask=mask(CHLAbiwk,rMASK_CHLA)$layer.2
# writeRaster(CHLAbiwk_mask,"M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_biweekly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_biweekly_range_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
# 
# CHLAmon=raster("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_monthly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_monthly_range_1997-09-04_2018-10-28_AllIslands.nc")
# CHLAmon_mask=mask(CHLAmon,rMASK_CHLA)$layer.2
# writeRaster(CHLAmon_mask,"M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_monthly_range/Chlorophyll_A_ESAOCCCI_Daily_mean_monthly_range_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")
# 
# CHLAann=raster("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_annual_range/Chlorophyll_A_ESAOCCCI_Daily_mean_annual_range_1997-09-04_2018-10-28_AllIslands.nc")
# CHLAann_mask=mask(CHLAann,rMASK_CHLA)$layer.2
# writeRaster(CHLAann_mask,"M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Domain_Level_Data/mean_annual_range/Chlorophyll_A_ESAOCCCI_Daily_mean_annual_range_1997-09-04_2018-10-28_AllIslands_30meter5pctmaskSTRM15.nc")


#K490
K490mn=raster("M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean/Kd490_ESAOCCCI_Daily_mean_1998-01-01_2018-06-26_AllIslands.nc")
crs(STRM15SPd)=crs(K490mn)
rMASK_K490=rasterize(x = STRM15SPd,y=K490mn,vals="Altitude",fun=maskfun)
K490mn_mask=mask(K490mn,rMASK_K490)$layer.2
writeRaster(K490mn_mask,"M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean/Kd490_ESAOCCCI_Daily_mean_1998-01-01_2018-06-26_AllIslands_30meter5pctmaskSTRM15.nc")

K490max=raster("M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/q95/Kd490_ESAOCCCI_Daily_q95_1998-01-01_2018-06-26_AllIslands.nc")
K490max_mask=mask(K490max,rMASK_K490)$layer.2
# writeRaster(K490max_mask,"M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/q95/Kd490_ESAOCCCI_Daily__Daily_q95_1998-01-01_2018-06-26_AllIslands_30meter5pctmaskSTRM15.nc")

K490biwk=raster("M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean_biweekly_range/Kd490_ESAOCCCI_Daily__Daily_mean_biweekly_range_1998-01-01_2018-06-26_AllIslands.nc")
K490biwk_mask=mask(K490biwk,rMASK_K490)$layer.2
writeRaster(K490biwk_mask,"M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean_biweekly_range/Kd490_ESAOCCCI_Daily__Daily_mean_biweekly_range_1998-01-01_2018-06-26_AllIslands_30meter5pctmaskSTRM15.nc")

K490mon=raster("M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean_monthly_range/Kd490_ESAOCCCI_Daily__Daily_mean_monthly_range_1998-01-01_2018-06-26_AllIslands.nc")
K490mon_mask=mask(K490mon,rMASK_K490)$layer.2
writeRaster(K490mon_mask,"M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean_monthly_range/Kd490_ESAOCCCI_Daily__Daily_mean_monthly_range_1998-01-01_2018-06-26_AllIslands_30meter5pctmaskSTRM15.nc")

K490ann=raster("M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean_annual_range/Kd490_ESAOCCCI_Daily__Daily_mean_annual_range_1998-01-01_2018-06-26_AllIslands.nc")
K490ann_mask=mask(K490ann,rMASK_K490)$layer.2
writeRaster(K490ann_mask,"M:/Environmental Data Summary/DataDownload/Kd490_ESAOCCCI_Daily/Domain_Level_Data/mean_annual_range/Kd490_ESAOCCCI_Daily__Daily_mean_annual_range_1998-01-01_2018-06-26_AllIslands_30meter5pctmaskSTRM15.nc")


#PAR
PARmn=raster("M:/Environmental Data Summary/DataDownload/PAR_MODIS_Daily/Domain_Level_Data/mean/PAR_MODIS_Daily_mean_2002-07-04_2019-12-30_AllIslands.nc")
rMASK_PAR=rasterize(x = STRM15SPd,y=PARmn,vals="Altitude",fun=maskfun)
PARmn_mask=mask(PARmn,rMASK_PAR)$layer.2
writeRaster(PARmn_mask,"M:/Environmental Data Summary/DataDownload/PAR_MODIS_Daily/Domain_Level_Data/mean/PAR_MODIS_Daily_mean_2002-07-04_2019-12-30_AllIslands_30meter5pctmaskSTRM15.nc")

PARmax=raster("M:/Environmental Data Summary/DataDownload/PAR_MODIS_Daily/Domain_Level_Data/q95/PAR_MODIS_Daily_q95_2002-07-04_2019-12-30_AllIslands.nc")
PARmax_mask=mask(PARmax,rMASK_PAR)$layer.2
writeRaster(PARmax_mask,"M:/Environmental Data Summary/DataDownload/PAR_MODIS_Daily/Domain_Level_Data/q95/PAR_MODIS_Daily_q95_2002-07-04_2019-12-30_AllIslands_30meter5pctmaskSTRM15.nc")

PARbiwk=raster("M:/Environmental Data Summary/DataDownload/PAR_MODIS_Daily/Domain_Level_Data/mean_biweekly_range/PAR_MODIS_Daily_mean_biweekly_range_2002-07-04_2019-12-30_AllIslands.nc")
PARbiwk_mask=mask(PARbiwk,rMASK_PAR)$layer.2
writeRaster(PARbiwk_mask,"M:/Environmental Data Summary/DataDownload/PAR_MODIS_Daily/Domain_Level_Data/mean_biweekly_range/PAR_MODIS_Daily_mean_biweekly_range_2002-07-04_2019-12-30_AllIslands_30meter5pctmaskSTRM15.nc")

PARmon=raster("M:/Environmental Data Summary/DataDownload/PAR_MODIS_Daily/Domain_Level_Data/mean_monthly_range/PAR_MODIS_Daily_mean_monthly_range_2002-07-04_2019-12-30_AllIslands.nc")
PARmon_mask=mask(PARmon,rMASK_PAR)$layer.2
writeRaster(PARmon_mask,"M:/Environmental Data Summary/DataDownload/PAR_MODIS_Daily/Domain_Level_Data/mean_monthly_range/PAR_MODIS_Daily_mean_monthly_range_2002-07-04_2019-12-30_AllIslands_30meter5pctmaskSTRM15.nc")

PARann=raster("M:/Environmental Data Summary/DataDownload/PAR_MODIS_Daily/Domain_Level_Data/mean_annual_range/PAR_MODIS_Daily_mean_annual_range_2002-07-04_2019-12-30_AllIslands.nc")
PARann_mask=mask(PARann,rMASK_PAR)$layer.2
writeRaster(PARann_mask,"M:/Environmental Data Summary/DataDownload/PAR_MODIS_Daily/Domain_Level_Data/mean_annual_range/PAR_MODIS_Daily_mean_annual_range_2002-07-04_2019-12-30_AllIslands_30meter5pctmaskSTRM15.nc")



# IslandPlot="HAW"
# CHLAmn_isl=raster(paste0("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Island_Level_Data/mean/",
#                          list.files("M:/Environmental Data Summary/DataDownload/Chlorophyll_A_ESAOCCCI_Daily/Island_Level_Data/mean/",pattern=IslandPlot)))
# STRM15_isl=raster(paste0("M:/Environmental Data Summary/DataDownload/Bathymetry_SRTM15/Island_By_Blocks_Level_Data/",
#                          list.files("M:/Environmental Data Summary/DataDownload/Bathymetry_SRTM15/Island_By_Blocks_Level_Data/",pattern=IslandPlot)))
# STRM15_isl <- shift(rotate(shift(STRM15_isl, 180)), 180)
# STRM15_islSPd=data.frame(rasterToPoints(STRM15_isl))
# coordinates(STRM15_islSPd)<- ~x+y
# crs(STRM15_islSPd)=crs(CHLAmn_isl)
# 
# 
# 
# rMASK=rasterize(x = STRM15_islSPd,y=CHLAmn_isl,vals="Altitude",fun=maskfun)
# CHLAmn_isl_mask=mask(CHLAmn_isl,rMASK)$layer.2
# par(mfrow=c(1,3))
# # plot(STRM15wak)
# # contour(STRM15wak,levels=c(-30),add=T)
# plot(CHLAmn_isl)
# contour(STRM15_isl,levels=c(-30),add=T)
# plot(rMASK$Altitude)
# contour(STRM15_isl,levels=c(-30),add=T)
# plot(CHLAmn_isl_mask)
# contour(STRM15_isl,levels=c(-30),add=T)
# 
# 

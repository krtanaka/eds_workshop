library(raster)
library(readr)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(colorRamps)

##############################################
### list ocean color datasets need masking ###
############################################## 
oc = read_csv("data/EDS_parameters.csv")

oc = oc %>%
  select("PARAMETER.NAME", "DOWNLOAD", "MaskForOceanColor") %>%
  subset(MaskForOceanColor == T & DOWNLOAD == "YES")

oc = oc$PARAMETER.NAME
oc = oc[-which(oc %in% c("Chlorophyll_A_ESAOCCCI_Clim"))]

oc

compare_masking <- function(param,islname = "Hawaii", basepath = "M:/Environmental Data Summary/DataDownload/") {
  
  # param = "Chlorophyll_A_ESAOCCCI_8Day"
  # islname = "Hawaii"
  
  r = list.files(path = paste0(basepath, param, "/Island_Level_Data/unmasked/"),
                 pattern = islname,full.names = T)
  
  if(length(r)==0){
    print("No unmasked file found. Returning empty plot"); return(ggplot())
  }
  
  r[2] = list.files(path = paste0(basepath, param, "/Island_Level_Data/"),
                    pattern = islname,full.names = T)
  
  if(length(r) == 1){
    print("No masked file found. Returning empty plot"); return(ggplot())
  }
  
  r1 = stack(r[1])
  r2 = stack(r[2])
  
  # plot(r1,  pch = ".", col = matlab.like(100))
  # plot(r2,  pch = ".", col = matlab.like(100))
  
  r1 = as.data.frame(rasterToPoints(r1))
  r2 = as.data.frame(rasterToPoints(r2))
  
  r1$z = rowMeans(r1[,3:dim(r1)[2]], na.rm = T)
  r2$z = rowMeans(r2[,3:dim(r2)[2]], na.rm = T)

  r1$mask = "Unmasked"
  r2$mask = "Masked"
  
  r1 = r1[,c("x", "y", "z", "mask")]
  r2 = r2[,c("x", "y", "z", "mask")]
  
  r = rbind(r1, r2)
  
  r$var = param
  
  p_r = r %>% 
    ggplot(aes(x, y, fill = z)) + 
    geom_raster() + 
    annotation_map(map = map_data("world"), fill = "gray50") +
    scale_fill_viridis_c("", limits = c(quantile(r$z,p = 0.05, na.rm = T),
                                        quantile(r$z,p = 0.95, na.rm = T)),
                         oob = squish) + 
    coord_equal() +
    facet_grid(~ mask) + 
    ggtitle(param) + 
    theme_void()
  
  return(p_r)
  
}

r1 = compare_masking("Chlorophyll_A_ESAOCCCI_8Day",islname = "Oahu")
r2 = compare_masking("Kd490_ESAOCCCI_8Day",islname = "Oahu")
r3 = compare_masking("PAR_MODIS_Daily",islname = "Oahu")
r4 = compare_masking("kdPAR_VIIRS_Weekly",islname = "Oahu")

dev.off()

(r1+r2)/(r3+r4)



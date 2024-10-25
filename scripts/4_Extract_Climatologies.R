#################################################################
# Script for Attaching Climatology Variables to In Situ Data
# Revised and Maintained by K. R. Tanaka and T. A. Oliver
# Point of Contact:
#   - kisei.tanaka@noaa.gov
#   - thomas.oliver@noaa.gov
#   - jessica.perelman@noaa.gov
#   - juliette.verstaen@noaa.gov
#################################################################

rm(list = ls())

dir = getwd()

# Import EDS functions
source("scripts/eds_functions.R")
source("scripts/ExpandingExtract.R")

load('data/survey_mhi.RData')
df$lon = ifelse(df$lon < 0, df$lon + 360, df$lon)
df$unit = df$island
# df = subset(df, unit %in% c("Hawaii"))

df$lon = ifelse(df$lon < 0, df$lon + 360, df$lon)

# Remove NAs in lat & lon columns. Convert into a spatial object
df = df[complete.cases(df[,c("lon", "lat")]), ]
df_sp = df; df_sp = as.data.frame(df_sp)
coordinates(df_sp) = ~lon + lat

# get list of rasters (i.e., climatologies)
EDS_path = paste0("/Users/", Sys.info()[7], "/Desktop/EDS/")

# Create list of climatology raster files
rasterlist = list.files(EDS_path,
                        recursive = T,
                        pattern = "_all_units.nc",
                        full.names = T)

rasterlist <- rasterlist[!grepl("mean|sd|q05|q95", rasterlist)]

# Check rasterarized climatological variables
strsplit(rasterlist, "/")

###################
### normal loop ###
###################

start = Sys.time()

for (raster_i in 1:length(rasterlist)){

  # raster_i = 1

  rasname_full = rasterlist[raster_i]
  rasname_sp = strsplit(rasname_full, "/")[[1]]
  rasname = rasname_sp[length(rasname_sp)]
  rasname = gsub(rasname, pattern = "-", replacement = ".")
  rasname = gsub(rasname, pattern = "_all_units.nc", replacement = "")

  this_r = raster(rasterlist[raster_i])

  if (grepl("Bathymetry", rasname)) {
    this_r[this_r > 0] <- NA
  }

  if (this_r@extent@xmin < 0) this_r = raster::shift(rotate(raster::shift(this_r, 180)), 180)

  crs(df_sp) = crs(this_r)

  cat(paste0("Step ", raster_i, " of ", length(rasterlist), ": ", rasname, "\n"))

  this_Ex = ExpandingExtract(this_r, df_sp, Dists = seq(0, 100, 10))

  eval(parse(text = paste0("df_sp$", rasname, " = this_Ex$values")))

  cat(paste0("Step ", raster_i, " of ", length(rasterlist), ": Extraction Complete.\n"))

}

stop = Sys.time()
start - stop
beepr::beep(2)

df = as.data.frame(df_sp)

save(df, file = paste0("outputs/EDS_Climatologies_", Sys.Date(), ".RData"))

good_sites = df %>% group_by(site) %>% dplyr::summarise(n = n()) %>% subset(n > 3)
good_sites = unique(good_sites$site)

clim1 = df %>%
  filter(site %in% good_sites) %>%
  na.omit() %>%
  arrange(Chlorophyll_A_ESA_OC_CCI_v6.0_Clim) %>%
  ungroup() %>%
  mutate(site = factor(site, unique(site))) %>%
  ggplot(aes(x = Chlorophyll_A_ESA_OC_CCI_v6.0_Clim,
             y = site  ,
             fill = Chlorophyll_A_ESA_OC_CCI_v6.0_Clim,
             color = Chlorophyll_A_ESA_OC_CCI_v6.0_Clim)) +
  geom_joy(scale = 2, alpha = 0.8, size = 0.1, bandwidth = 0.1) +
  ylab(NULL) +
  xlab("Chlorophyll_A_Climatology_1998_2022") +
  ggdark::dark_theme_minimal() +
  scale_fill_viridis_c("", trans = "sqrt") +
  scale_color_viridis_c("", trans = "sqrt") +
  theme(legend.position = "right")

clim2 = df %>%
  filter(site %in% good_sites) %>%
  na.omit() %>%
  arrange(Sea_Surface_Temperature_NOAA_geopolar_blended_Clim) %>%
  ungroup() %>%
  mutate(site = factor(site, unique(site))) %>%
  ggplot(aes(x = Sea_Surface_Temperature_NOAA_geopolar_blended_Clim,
             y = site,
             fill = Sea_Surface_Temperature_NOAA_geopolar_blended_Clim,
             color = Sea_Surface_Temperature_NOAA_geopolar_blended_Clim)) +
  geom_joy(scale = 2, alpha = 0.8, size = 0, bandwidth = 0.1) +
  ylab(NULL) +
  xlab("SST_Climatology_2002_2017") +
  ggdark::dark_theme_minimal() +
  scale_fill_gradientn(colors =  colorRamps::matlab.like(100), "", trans = "sqrt") +
  scale_color_gradientn(colors =  colorRamps::matlab.like(100), "", trans = "sqrt") +
  theme(legend.position = "right")

library(patchwork)
clim1 + clim2

ggsave(last_plot(), filename = "outputs/EDS_Climatology.png", height = 8, width = 14)

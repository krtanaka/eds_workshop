################################################################
### R scripts to attach timeseries variables to in situ data ###
### Originally developed & conceptualized by T.A.Oliver.     ###
### Revised & Maintained by K.R.Tanaka & T.A.Oliver.         ###
### POC: kisei.tanaka@noaa.gov, thomas.oliver@noaa.gov,      ###
### jessica.perelman@noaa.gov, juliette.verstaen@noaa.gov    ###
################################################################

rm(list = ls())

dir = paste0(getwd(), "/")

source("scripts/eds_functions.R")

###########################################################################
### read survey data points, assign distinct lat, lon, and time columns ###
###########################################################################
load('data/survey_marian.RData')

df$island = gsub(" ", "_", df$island)
df$date_r = mdy(df$date)

df <- df %>% filter(!is.na(lon) & !is.na(lat))

# spatial units (e.g., region, unit)
df$unit = df$island

###########################
### read bounding boxes ###
###########################
Bounding_Boxes = read.csv("data/Bounding_Boxes.csv"); unique(Bounding_Boxes$unit)

#######################################################################
### Build list of target environmental variables                    ###
### See folder names in M:/Environmental Data Summary/DataDownload/ ###
#######################################################################
paramdir = paste0("/Users/", Sys.info()[7], "/Desktop/EDS/Dynamic_Variables/")
parameters = list.files(path = paramdir, full.names = F); parameters

#########################################
### Read EDS Parameter/Variable Table ###
#########################################
Parameter_Table = read.csv("data/EDS_parameters.csv")
names(Parameter_Table)
unique(Parameter_Table$Dataset)

#####################################
### Prep For Variable Extractions ###
#####################################

# locate each point in an unit bounding box
PT = points.in.polys(df$lon,
                     df$lat,
                     Bounding_Boxes)

df$DATA_UNIT = df$unit

# Drop points outside target area and report
cat(paste("Dropping", length(which(df$DATA_UNIT == "NONE_ASSIGNED")),
          "points of", nrow(df),
          "entered points, as outside Geographic Scope"))

df = subset(df, DATA_UNIT != "NONE_ASSIGNED")
df = df[!duplicated(df[c("lat", "lon", "date_r")]), ]

# Use smaller dataset for debugging
df = df %>% subset(unit %in% c("Guam", "Hawaii"))

# List of spatial units
unique_units = sort(unique(df$DATA_UNIT)); unique_units

############################################################
### Extract Time Series Variables for every survey point ###
############################################################

# Normal loop for each parameter
start_time <- Sys.time()
for(parameter_i in 1:length(parameters)){

  # parameter_i = 2

  # Get Unit_Level_Data Directory for this Param
  param.name = parameters[parameter_i]; param.name

  this_param_i = which(Parameter_Table$Dataset == param.name); this_param_i

  godir = paste(paramdir, param.name, "/Unit_Level_Data", sep = ""); godir

  paramsum = unlist(strsplit(as.vector(Parameter_Table[this_param_i, "Summaries"]), ";")); paramsum

  # For each unit
  for(unit_i in 1:length(unique_units)){

    # unit_i = 1

    unique_units[unit_i]

    # retrieve raw netcdf data for each unit
    ncfile = list.files(godir, pattern = paste0(unique_units[unit_i], "_"), full.names = T)

    # if there are no data then move to next unit
    if(length(ncfile) == 0){

      print(paste0("Skipping ", unique_units[unit_i], " because there are no data"))

      next

    }

    nc_p = nc_open(ncfile)

    # Pull var array
    rawvar = ncvar_get(nc = nc_p, varid = as.vector(Parameter_Table$Fields[this_param_i]))

    # Pull dim vectors
    lon = ncvar_get(nc = nc_p, varid = "longitude"); lon
    lat = ncvar_get(nc = nc_p, varid = "latitude"); lat
    rawt = ncvar_get(nc = nc_p, varid = "time"); rawt

    # Close nc
    nc_close(nc_p)

    t = as_date(as_datetime(as.numeric(rawt), origin = ymd("1970/1/1")))
    head(t); tail(t)

    # Subset to Unit
    df_i = which(df$DATA_UNIT == unique_units[unit_i])

    # Locate all points in rawvar array, flagging any out of bound with NA in "ijk"
    ijk = xyt2ijk(xyt_df = as.data.frame(df[df_i,c("lon","lat","date_r")]),
                  x_grid = lon,
                  y_grid = lat,
                  t_grid = t)
    ijk

    # Check for any out of bound points (stored as NA), we want to drop them from both ijk and df_i
    droprows = which(is.na(ijk), arr.ind = T)[,1] #Finds points as rows in ijk

    # If there are any NA, drops rows from ijk and indices from df_i
    if (length(droprows) > 0){

      ijk = ijk[-droprows,]
      df_i = df_i[-droprows]

    }

    if (length(df_i) == 0) {

      print("No df_i ERROR ERROR ERROR ERROR ERROR ERROR")
      next()

    } else {

      # Count NA in var array (will use to solve NA issues)
      naP_xy = plyr::aaply(rawvar,
                           c(1,2),
                           NAstackcount)/dim(rawvar)[3]

      naP_xy

      # Id points sitting on NA-heavy timeseries
      i_masked = which(naP_xy[cbind(ijk$x_i, ijk$y_j)] > 0.8); i_masked

      # Infill selected points with spatial interpolation
      cnt = 1

      for(i_infill in i_masked){

        # Update NA blocks
        pNA = naP_xy[cbind(ijk$x_i[i_infill],
                           ijk$y_j[i_infill])]

        # Selected NA timeseries +/- x pixel steps
        ij_ex = 1

        while(pNA > 0.8 & ij_ex < 3){

          ij_vec = -ij_ex:ij_ex

          # Make sure selected NA timeseries +/- x pixel steps fits within the size of rawvar

          max_x = dim(rawvar)[1]
          max_y = dim(rawvar)[2]

          ts_x = ijk$x_i[i_infill]+ij_vec
          ts_y = ijk$y_j[i_infill]+ij_vec

          ts_x = subset(ts_x, ts_x <= max_x)
          ts_x = subset(ts_x, ts_x > 0)
          ts_y = subset(ts_y, ts_y <= max_y)
          ts_y = subset(ts_y, ts_y > 0)

          # Generates "infill" time series
          ts = plyr::aaply(rawvar[ts_x,
                                  ts_y,],
                           c(3),
                           mean, na.rm = T)

          pNA = length(which(is.na(ts)))/length(ts)

          if(pNA < 0.9){

            # Update rawvar
            rawvar[ijk$x_i[i_infill],
                   ijk$y_j[i_infill],] = ts

            # Update naP
            naP_xy = plyr::aaply(rawvar,c(1,2),
                           NAstackcount)/dim(rawvar)[3]

            print(paste("In-fill complete", cnt, "of", length(i_masked), ". Pixel +/-", ij_ex))

          }

          ij_ex = ij_ex + 1

        } # Close While

        # Fill in interpolated data
        cnt = cnt + 1

      } # Close infill for

      # Set Time Step
      if (Parameter_Table$Frequency[this_param_i] == "Monthly") tstep = 30.42 # (60*60*24*30.42)
      if (Parameter_Table$Frequency[this_param_i] == "14day") tstep = 14 # (60*60*24*14)
      if (Parameter_Table$Frequency[this_param_i] == "8day") tstep = 8 # (60*60*24*8)
      if (Parameter_Table$Frequency[this_param_i] == "5day") tstep = 5 # (60*60*24*5)
      if (Parameter_Table$Frequency[this_param_i] == "Weekly") tstep = 7 # (60*60*24*7)
      if (Parameter_Table$Frequency[this_param_i] == "Daily") tstep = 1 # (60*60*24*1)

      # TimeSeries Pull Indices
      ijk$t_01dy = ijk$t_k - (1/tstep-1)
      ijk$t_01wk = ijk$t_k - (7/tstep-1)
      ijk$t_01mo = round(ijk$t_k - (30.42/tstep-1))
      ijk$t_03mo = round(ijk$t_k - (91.25/tstep-1))
      ijk$t_06mo = round(ijk$t_k - (182.5/tstep-1))
      ijk$t_01yr = round(ijk$t_k - (1*365.25/tstep-1))
      ijk$t_03yr = round(ijk$t_k - (3*365.25/tstep-1))
      ijk$t_05yr = round(ijk$t_k - (5*365.25/tstep-1))
      ijk$t_10yr = round(ijk$t_k - (10*365.25/tstep-1))

      ijk[,c("t_k",
             "t_01dy",
             "t_01wk",
             "t_01mo",
             "t_03mo",
             "t_06mo",
             "t_01yr",
             "t_03yr",
             "t_05yr",
             "t_10yr")][which(ijk[,c("t_k",
                                     "t_01dy",
                                     "t_01wk",
                                     "t_01mo",
                                     "t_03mo",
                                     "t_06mo",
                                     "t_01yr",
                                     "t_03yr",
                                     "t_05yr",
                                     "t_10yr")] < 1, arr.ind = T)] = 1

      # Apply Summaries to Timeseries
      for(sum_i in 1:length(paramsum)){

        # sum_i = 1

        paramsum.name = paste0(paramsum[sum_i], "_", param.name); paramsum.name

        if(!paramsum.name %in% substr(names(df), 1, nchar(paramsum.name))){

          eval(parse(text = paste0("df$",paramsum.name,"_DY01=-9991")))
          eval(parse(text = paste0("df$",paramsum.name,"_WK01=-9991")))
          eval(parse(text = paste0("df$",paramsum.name,"_MO01=-9991")))
          eval(parse(text = paste0("df$",paramsum.name,"_MO03=-9991")))
          eval(parse(text = paste0("df$",paramsum.name,"_MO06=-9991")))
          eval(parse(text = paste0("df$",paramsum.name,"_YR01=-9991")))
          eval(parse(text = paste0("df$",paramsum.name,"_YR03=-9991")))
          eval(parse(text = paste0("df$",paramsum.name,"_YR05=-9991")))
          eval(parse(text = paste0("df$",paramsum.name,"_YR10=-9991")))
          eval(parse(text = paste0("df$",paramsum.name,"_YR10YR01=-9991")))
          eval(parse(text = paste0("df$",paramsum.name,"_ALLB4=-9991")))

        }

        # For each point in df_i
        for(sumpt_i in 1:length(df_i)){

          # sumpt_i = 1

          ts_01dy = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_01dy[sumpt_i]:ijk$t_k[sumpt_i]]
          ts_01wk = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_01wk[sumpt_i]:ijk$t_k[sumpt_i]]
          ts_01mo = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_01mo[sumpt_i]:ijk$t_k[sumpt_i]]
          ts_03mo = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_03mo[sumpt_i]:ijk$t_k[sumpt_i]]
          ts_06mo = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_06mo[sumpt_i]:ijk$t_k[sumpt_i]]
          ts_01yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_01yr[sumpt_i]:ijk$t_k[sumpt_i]]
          ts_03yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_03yr[sumpt_i]:ijk$t_k[sumpt_i]]
          ts_05yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_05yr[sumpt_i]:ijk$t_k[sumpt_i]]
          ts_10yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_10yr[sumpt_i]:ijk$t_k[sumpt_i]]

          ts_10yr01yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_10yr[sumpt_i]:ijk$t_01yr[sumpt_i]]
          ts_ALLB4 = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], 1:ijk$t_k[sumpt_i]]

          t_01dy = t[ijk$t_01dy[sumpt_i]:ijk$t_k[sumpt_i]]
          t_01wk = t[ijk$t_01wk[sumpt_i]:ijk$t_k[sumpt_i]]
          t_01mo = t[ijk$t_01mo[sumpt_i]:ijk$t_k[sumpt_i]]
          t_03mo = t[ijk$t_03mo[sumpt_i]:ijk$t_k[sumpt_i]]
          t_06mo = t[ijk$t_06mo[sumpt_i]:ijk$t_k[sumpt_i]]
          t_01yr = t[ijk$t_01yr[sumpt_i]:ijk$t_k[sumpt_i]]
          t_03yr = t[ijk$t_03yr[sumpt_i]:ijk$t_k[sumpt_i]]
          t_05yr = t[ijk$t_05yr[sumpt_i]:ijk$t_k[sumpt_i]]
          t_10yr = t[ijk$t_10yr[sumpt_i]:ijk$t_k[sumpt_i]]
          t_10yr01yr = t[ijk$t_10yr[sumpt_i]:ijk$t_01yr[sumpt_i]]
          t_ALLB4 = t[1:ijk$t_k[sumpt_i]]

          if(paramsum[sum_i] %in% c("mean", "q05", "q95","sd")){

            eval(parse(text = paste0("df$", paramsum.name, "_DY01[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01dy, na.rm = T)")))
            eval(parse(text = paste0("df$", paramsum.name, "_WK01[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01wk, na.rm = T)")))
            eval(parse(text = paste0("df$", paramsum.name, "_MO01[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01mo, na.rm = T)")))
            eval(parse(text = paste0("df$", paramsum.name, "_MO03[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_03mo, na.rm = T)")))
            eval(parse(text = paste0("df$", paramsum.name, "_MO06[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_06mo, na.rm = T)")))
            eval(parse(text = paste0("df$", paramsum.name, "_YR01[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01yr, na.rm = T)")))
            eval(parse(text = paste0("df$", paramsum.name, "_YR03[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_03yr, na.rm = T)")))
            eval(parse(text = paste0("df$", paramsum.name, "_YR05[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_05yr, na.rm = T)")))
            eval(parse(text = paste0("df$", paramsum.name, "_YR10[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_10yr, na.rm = T)")))
            eval(parse(text = paste0("df$", paramsum.name, "_YR10YR01[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_10yr01yr, na.rm = T)")))
            eval(parse(text = paste0("df$", paramsum.name, "_ALLB4[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_ALLB4, na.rm = T)")))

          }else{

            eval(parse(text = paste0("df$", paramsum.name, "_DY01[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01dy, na.rm = T, t = t_01dy)")))
            eval(parse(text = paste0("df$", paramsum.name, "_WK01[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01wk, na.rm = T, t = t_01wk)")))
            eval(parse(text = paste0("df$", paramsum.name, "_MO01[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01mo, na.rm = T, t = t_01mo)")))
            eval(parse(text = paste0("df$", paramsum.name, "_MO03[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_03mo, na.rm = T, t = t_03mo)")))
            eval(parse(text = paste0("df$", paramsum.name, "_MO06[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_06mo, na.rm = T, t = t_06mo)")))
            eval(parse(text = paste0("df$", paramsum.name, "_YR01[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01yr, na.rm = T, t = t_01yr)")))
            eval(parse(text = paste0("df$", paramsum.name, "_YR03[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_03yr, na.rm = T, t = t_03yr)")))
            eval(parse(text = paste0("df$", paramsum.name, "_YR05[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_05yr, na.rm = T, t = t_05yr)")))
            eval(parse(text = paste0("df$", paramsum.name, "_YR10[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_10yr, na.rm = T, t = t_10yr)")))
            eval(parse(text = paste0("df$", paramsum.name, "_YR10YR01[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_10yr01yr, na.rm = T, t = t_10yr01yr)")))
            eval(parse(text = paste0("df$", paramsum.name, "_ALLB4[df_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_ALLB4, na.rm = T, t = t_ALLB4)")))

          } # END if

        } # END Loop over this unit's points (for 1:length(df_i))

        print(paste(unique_units[unit_i],
                    paramsum.name, "Done.",
                    unit_i, "of",
                    length(unique_units), "units. Completed",
                    sumpt_i, " points..."))

        save(df, file = paste0(dir, "outputs/EDS_Timeseries_", Sys.Date(), ".Rdata"))


      } # END Loop over each summary function  (for 1:length(paramsum))

    }

  } # END Loop over each unit

} # END Loop over each parameter
end_time <- Sys.time()
end_time - start_time

df[df == -9991] <- NA

#make columns easier to read...
colnames(df) = gsub("_SST_CRW_Monthly_", "_sst_", colnames(df))
colnames(df) = gsub("_Chlorophyll_A_ESAOCCCI_", "_chl_a_", colnames(df))

vis_miss(df[,c(10:53)])
vis_miss(df[,c(54:dim(df)[2])])

chla = cor(df[,c(10:20)], use = "complete.obs")
sst = cor(df[,c(54:64)], use = "complete.obs")

dev.off()
corrplot(sst, method = "shade", cl.lim = c(min(sst), 1), is.corr = F, tl.cex = 0.5)
corrplot(chla, method = "shade", cl.lim = c(min(chla), 1), is.corr = F, tl.cex = 0.5)

n = df %>% group_by(site) %>% summarise(n = n()) %>% subset(n > 2)
good_sites = n$site

b = getNOAA.bathy(lon1 = min(pretty(df$lon)),
                  lon2 = max(pretty(df$lon)),
                  lat1 = min(pretty(df$lat)),
                  lat2 = max(pretty(df$lat)),
                  resolution = 1)

b = fortify.bathy(b)

sd = df %>%
  subset(site %in% good_sites) %>%
  group_by(site) %>%
  mutate(sd = median(sd_Sea_Surface_Temperature_CRW_Monthly_YR01)) %>%
  arrange(sd) %>%
  ungroup() %>%
  mutate(site=factor(site, unique(site))) %>%
  ggplot(aes(x = sd_Sea_Surface_Temperature_CRW_Monthly_YR01, y = site, fill = sd, color = sd)) +
  geom_joy(scale = 3, alpha = 0.8, size = 0.01, bandwidth = 0.05) +
  ylab(NULL) +
  coord_fixed(ratio = 0.06) +
  ggdark::dark_theme_minimal() +
  scale_fill_gradientn(colours = matlab.like(length(good_sites)), "") +
  scale_color_gradientn(colours = matlab.like(length(good_sites)), "") +
  theme(legend.position = "none",
        axis.title.x = element_blank()) +
  ggtitle("Obs specific SST sd year_1")

sites_with_high_sd = df %>%
  subset(site %in% good_sites) %>%
  group_by(site) %>%
  summarise(sd = median(sd_Sea_Surface_Temperature_CRW_Monthly_YR01)) %>%
  slice_max(sd, n = 5)

map = df %>%
  subset(site %in% good_sites) %>%
  group_by(site) %>%
  summarise(lon = mean(lon),
            lat = mean(lat),
            sd = median(sd_Sea_Surface_Temperature_CRW_Monthly_YR01))

site_map = ggplot() +
  geom_point(data = map, aes(x = lon, y = lat),
             alpha = 0.5, size = 5, shape = 21, fill = "red") +
  geom_label_repel(data = map,
                   aes(x = lon, y = lat,
                       label = ifelse(site %in% sites_with_high_sd$site, site, "")),
                   fill = alpha(c("red"), 0.5)) +
  geom_contour(data = b,
               aes(x = x, y = y, z = z),
               breaks = seq(-3000, 0, by = 300),
               # size = c(0.1),
               alpha = 0.8,
               colour = topo.colors(522)) +
  labs(x = "", y = "") +
  ggdark::dark_theme_minimal()

sd + site_map
ggsave(last_plot(), filename = "outputs/EDS_Timeseries.png", height = 8, width = 14)

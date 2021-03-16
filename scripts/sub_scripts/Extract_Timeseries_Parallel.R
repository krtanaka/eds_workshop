




# parallelized loop for each parameter
library(doParallel)
cores = detectCores()/2
registerDoParallel(cores = cores)

ptm <- proc.time()
r = foreach(parameter_i = 1:length(parameters), 
            .combine = cbind, 
            .packages = c('ncdf4', 'lubridate', 'plyr')) %dopar% {
              
              source(paste0(dir, 'scripts/HelperCode/EDS_HelperFunctions.R'))
              # parameter_i = 1
              
              #Get Island_Level_Data Directory for this Param
              param.name = parameters[parameter_i]; param.name
              
              this_param_i = which(Parameter_Table$PARAMETER.NAME == param.name); this_param_i
              
              godir = paste(paramdir, param.name, "/Island_Level_Data", sep = ""); godir
              
              paramsum = unlist(strsplit(as.vector(Parameter_Table[this_param_i, "Summaries"]), ";"))
              
              #For each Island
              for(island_i in 1:length(unique_islands)){
                
                # island_i = 1
                
                unique_islands[island_i]
                
                #Subset to ISLAND
                SM_i = which(SM$DATA_ISL == unique_islands[island_i])
                
                #Get ISLAND, PARAM data
                ncfile = list.files(godir, pattern = paste0(unique_islands[island_i], "_"), full.names = T)
                
                nc_p = nc_open(ncfile)
                
                #pull var array
                rawvar = ncvar_get(nc = nc_p, varid = as.vector(Parameter_Table$GRID.VARIABLE[this_param_i]))#readNcdfVarName(ncfile))
                
                #pull dim vectors
                lon = ncvar_get(nc = nc_p,varid = "longitude")
                lat = ncvar_get(nc = nc_p,varid = "latitude")
                rawt = ncvar_get(nc = nc_p,varid = "time")
                
                #close nc
                nc_close(nc_p)
                
                t = as_date(as_datetime(as.numeric(rawt), origin = ymd("1970/1/1")))
                head(t); tail(t)
                
                # locate all points in rawvar array
                ijk = xyt2ijk(xyt_df = SM[SM_i,c("LON","LAT","DATE_R")],
                              x_grid = lon,
                              y_grid = lat,
                              t_grid = t)
                ijk
                
                # Count NA in var array (will use to solve NA issues)
                naP_xy = aaply(rawvar, 
                               c(1,2),
                               NAstackcount)/dim(rawvar)[3]
                
                #id points sitting on NA-heavy timeseries
                i_masked = which(naP_xy[cbind(ijk$x_i, ijk$y_j)] > 0.9)
                
                #Infill selected points with spatial interpolation
                cnt = 1
                
                for(i_infill in i_masked){
                  
                  #update NA blocks
                  pNA = naP_xy[cbind(ijk$x_i[i_infill],
                                     ijk$y_j[i_infill])]
                  
                  #selected NA timeseries +/- x pixel steps
                  ij_ex = 1
                  
                  while(pNA > 0.9 & ij_ex < 3){
                    
                    ij_vec = -ij_ex:ij_ex
                    
                    ts = aaply(rawvar[ijk$x_i[i_infill]+ij_vec,
                                      ijk$y_j[i_infill]+ij_vec,],
                               c(3),
                               mean, na.rm = T)
                    
                    pNA = length(which(is.na(ts)))/length(ts)
                    
                    if(pNA < 0.9){
                      
                      #Update rawvar
                      rawvar[ijk$x_i[i_infill],
                             ijk$y_j[i_infill],] = ts
                      
                      #Update naP
                      naP_xy = aaply(rawvar,c(1,2),
                                     NAstackcount)/dim(rawvar)[3]
                      
                      print(paste("In-fill complete", cnt, "of", length(i_masked), ". Pixel +/-", ij_ex))
                      
                    }
                    
                    ij_ex = ij_ex + 1
                    
                  }#Close While
                  
                  #Fill in interpolated data
                  cnt = cnt + 1
                  
                }#close infill for
                
                #Set Time Step
                if(Parameter_Table$FREQUENCY[this_param_i] == "Daily"){
                  
                  tstep = 1
                  
                } else if (Parameter_Table$FREQUENCY[this_param_i] == "Monthly"){
                  
                  tstep = 30
                  
                } else if (Parameter_Table$FREQUENCY[this_param_i] == "Weekly"){
                  
                  tstep = 7
                  
                } else if (Parameter_Table$FREQUENCY[this_param_i] == "8day"){
                  
                  tstep = 8
                  
                }else{
                  
                  tstep = 1
                }
                
                #TimeSeries Pull Indices
                ijk$t_03mo = ijk$t_k - (90/tstep-1)
                ijk$t_01yr = round(ijk$t_k-(1*365.25/tstep-1))
                ijk$t_03yr = round(ijk$t_k-(3*365.25/tstep-1))
                ijk$t_05yr = round(ijk$t_k-(5*365.25/tstep-1))
                ijk$t_10yr = round(ijk$t_k-(10*365.25/tstep-1))
                ijk[,c("t_k","t_03mo","t_01yr","t_03yr","t_05yr","t_10yr")][which(ijk[,c("t_k","t_03mo","t_01yr","t_03yr","t_05yr","t_10yr")] < 1, arr.ind = T)] = 1
                
                #Apply Summaries to Timeseries
                for(sum_i in 1:length(paramsum)){
                  
                  # sum_i = 1
                  
                  paramsum.name = paste0(paramsum[sum_i], "_", param.name)
                  
                  if(!paramsum.name %in% substr(names(SM), 1, nchar(paramsum.name))){
                    
                    eval(parse(text = paste0("SM$",paramsum.name,"_MO03=NA")))
                    eval(parse(text = paste0("SM$",paramsum.name,"_YR01=NA")))
                    eval(parse(text = paste0("SM$",paramsum.name,"_YR03=NA")))
                    eval(parse(text = paste0("SM$",paramsum.name,"_YR05=NA")))
                    eval(parse(text = paste0("SM$",paramsum.name,"_YR10=NA")))
                    eval(parse(text = paste0("SM$",paramsum.name,"_YR10YR01=NA")))
                    eval(parse(text = paste0("SM$",paramsum.name,"_ALLB4=NA")))
                    
                  }
                  
                  #For each point in SM_i
                  for(sumpt_i in 1:length(SM_i)){
                    
                    # sumpt_i = 1
                    
                    ts_03mo = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_03mo[sumpt_i]:ijk$t_k[sumpt_i]]
                    ts_01yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_01yr[sumpt_i]:ijk$t_k[sumpt_i]]
                    ts_03yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_03yr[sumpt_i]:ijk$t_k[sumpt_i]]
                    ts_05yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_05yr[sumpt_i]:ijk$t_k[sumpt_i]]
                    ts_10yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_10yr[sumpt_i]:ijk$t_k[sumpt_i]]
                    
                    ts_10yr01yr = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], ijk$t_10yr[sumpt_i]:ijk$t_01yr[sumpt_i]]
                    ts_ALLB4 = rawvar[ijk$x_i[sumpt_i], ijk$y_j[sumpt_i], 1:ijk$t_k[sumpt_i]]
                    
                    t_03mo = t[ijk$t_03mo[sumpt_i]:ijk$t_k[sumpt_i]]
                    t_01yr = t[ijk$t_01yr[sumpt_i]:ijk$t_k[sumpt_i]]
                    t_03yr = t[ijk$t_03yr[sumpt_i]:ijk$t_k[sumpt_i]]
                    t_05yr = t[ijk$t_05yr[sumpt_i]:ijk$t_k[sumpt_i]]
                    t_10yr = t[ijk$t_10yr[sumpt_i]:ijk$t_k[sumpt_i]]
                    t_10yr01yr = t[ijk$t_10yr[sumpt_i]:ijk$t_01yr[sumpt_i]]
                    t_ALLB4 = t[1:ijk$t_k[sumpt_i]]
                    
                    if(paramsum[sum_i] %in% c("mean", "q05", "q95")){
                      
                      eval(parse(text = paste0("SM$", paramsum.name, "_MO03[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_03mo, na.rm = T)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_YR01[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01yr, na.rm = T)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_YR03[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_03yr, na.rm = T)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_YR05[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_05yr, na.rm = T)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_YR10[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_10yr, na.rm = T)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_YR10YR01[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_10yr01yr, na.rm = T)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_ALLB4[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_ALLB4, na.rm = T)")))
                      
                    }else{
                      
                      eval(parse(text = paste0("SM$", paramsum.name, "_MO03[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_03mo, na.rm = T, t = t_03mo)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_YR01[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_01yr, na.rm = T, t = t_01yr)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_YR03[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_03yr, na.rm = T, t = t_03yr)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_YR05[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_05yr, na.rm = T, t = t_05yr)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_YR10[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_10yr, na.rm = T, t = t_10yr)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_YR10YR01[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_10yr01yr, na.rm = T, t = t_10yr01yr)")))
                      eval(parse(text = paste0("SM$", paramsum.name, "_ALLB4[SM_i[sumpt_i]] = ", paramsum[sum_i], "(x = ts_ALLB4, na.rm = T, t = t_ALLB4)")))
                      
                    }#END if
                    
                  }#END Loop over this island's points (for 1:length(SM_i))
                  
                  print(paste(unique_islands[island_i], 
                              paramsum.name, "Done.", 
                              island_i, "of", 
                              length(unique_islands), "islands. Completed", 
                              sumpt_i, " points..."))
                  
                  # write.csv(x = SM, paste0("/Users/", Sys.info()[7], "/fish_benthic_env/outputs/Survey_Master_Timeseries.csv"))
                  
                  
                }#END Loop over each summary function  (for 1:length(paramsum))
                
              }#END Loop over each island
              
              timeseries_i = SM[,52:58]
              timeseries_i
              
            }#END Loop over each parameter
proc.time() - ptm
SM = cbind(SM, r)
readr::write_csv(SM, paste0("/Users/", Sys.info()[7], "/fish_benthic_env/outputs/Survey_Master_Timeseries_", Sys.Date(), ".csv"))
beepr::beep(2)
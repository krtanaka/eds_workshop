#This function takes a spatial raster,  and a spatial data frame of in situ points
#Then it will fill any NA value in the SpDF with the first-discovered non-NA values from r

lengthNONA = function(x){return(length(x[!is.na(x)]))}

ExpandingExtract = function(r, SpDF, Dists = c(0, 500, 1000, 2000, 4000, 8000)){
  
  require(raster)
  require(spatial)
  
  OutDF = data.frame(values = rep(NA, nrow(SpDF)), 
                     Dist = rep(NA, nrow(SpDF)), 
                     N = rep(NA, nrow(SpDF)))
  
  nDists = length(Dists)
  cnt = 1
  
  NAi = which(is.na(OutDF$values))
  
  NAsLeft = length(NAi) > 0
  
  while(cnt <= nDists & NAsLeft){
    
    NAi = which(is.na(OutDF$values))
    
    pull = raster::extract(x = r, y = SpDF[NAi, ], 
                           buffer = Dists[cnt], 
                           small = TRUE, 
                           na.rm = TRUE)
    
    Nper = unlist(lapply(pull, lengthNONA))
    
    OutDF$values[NAi] = unlist(lapply(pull, mean, na.rm = TRUE))
    OutDF$Dist[NAi] = Dists[cnt]
    OutDF$N[NAi] = Nper
    
    NAi = which(is.na(OutDF$values))
    NAsLeft = length(NAi) > 0
    
    cnt = cnt + 1
  }
  
  return(OutDF)
  
}


ExpandingExtract.Stack = function(STACK, SpDF, Dists = c(500, 1000, 2000, 4000, 8000)){
  
  for(i in 1:nlayers(STACK)){
    
  }
}
matchA2B_gcdist=function(latA,lonA,latB,lonB){
  #gives first match on distance tie
  matchout=NULL
  d=gcdist.set1vset2(lats1 = latA,lons1 = lonA,lats2 = latB,lons2 = lonB)
  matchout=apply(d,1,which.min)
  return(matchout)
}



#Calculate Great Circle Distance
gcdist=function(lat1,lon1,lats,lons){
  rlat1=lat1*pi/180
  rlon1=lon1*pi/180
  rlats=lats*pi/180
  rlons=lons*pi/180
  x=sin(rlat1)*sin(rlats)+cos(rlat1)*cos(rlats)*cos(abs(rlons-rlon1))
  dists=6371*acos(x-2e-16)
  return(dists)
}

#Calculate Great Circle Distance
gcdist.pw=function(latc1,lonc1,latc2,lonc2){
  #get gc.dist between two points, enter in columns
  rlatc1=latc1*pi/180
  rlonc1=lonc1*pi/180
  rlatc2=latc2*pi/180
  rlonc2=lonc2*pi/180
  x=sin(rlatc1)*sin(rlatc2)+cos(rlatc1)*cos(rlatc2)*cos(abs(rlonc1-rlonc2))
  dists=6371*acos(x-2e-16)
  return(dists)
}

gcdist.allpw=function(lats,lons,lower="Mirror"){
  N=length(lats)
  if (N==1) return(0) else {
    dmat=matrix(NA,nrow=N,ncol=N)
    for(i in 1:(N-1)){
      for (j in (i+1):N){
        dmat[i,j]=gcdist.pw(lats[i],lons[i],lats[j],lons[j])
      }
    }
    diag(dmat)=0
    utri.ind=which(upper.tri(dmat),arr.ind=TRUE)
    ltri.ind=utri.ind[,2:1]
    if (lower=="Mirror"){
      dmat[ltri.ind]=dmat[utri.ind]
    }else{
      dmat[ltri.ind]=NA
    }
    return(dmat)
  }
}

gcdist.set1vset2=function(lats1,lons1,lats2,lons2,lower="Mirror"){
#return all pts from col1 vs all from col2
  N1=length(lats1)
  N2=length(lats2)
  if (N1==1|N2==1) return(0) else {
    dmat=matrix(NA,nrow=N1,ncol=N2)
    for(i in 1:N1){
#      for (j in 1:N2){
        dmat[i,]=gcdist(lats1[i],lons1[i],lats2,lons2)
 #     }
    }
    return(dmat)
  }
}
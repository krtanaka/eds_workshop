prioritize_on=function(df,on,off,seed=NULL,DEBUG=FALSE,Nselect=10,Nsample=100,Ntries=1){
  if(Nsample>nrow(df)){Nsample=nrow(df)}
  mpd=function(x){return(mean(dist(x),na.rm=T))}
  Non=length(on)
  Noff=length(off)
  ON=df[,on]
  OFF=df[,off]
  #Now start with random point
  #Given that start, add points in order of:
  #Max max marginal increase in mean pairwise D of ON,
  #while Min marginal increase pairwise D of OFF,
  Ntried=0
  TriedScores=rep(NA,Ntries)
  BestScore=0
  BestSort=rep(NA,Nselect)
  for(Ntried in 1:Ntries){
    unsorted=sample(1:nrow(df),Nsample,replace = F)
    sorted=rep(NA,Nselect)
    if(is.null(seed)){
      randi=sample(unsorted,1)
      sorted[1]=randi
      unsorted[randi]=NA
    }else{
      sorted[1]=seed
      unsorted[seed]=NA
    }
    
    for(i_allpnts in 2:length(sorted)){
      #Now calc pairwise stuff
      un_nona=as.vector(na.omit(unsorted))
      so_nona=as.vector(na.omit(sorted))
      #trackDF=data.frame(un_nona,Range_ON=rep(NA,length(un_nona)),MEANPWD_ON=rep(NA,length(un_nona)),SUMPWD_OFF=rep(NA,length(un_nona)))
      trackDF=data.frame(un_nona,MEANPWD_ON=NA,MEANPWD_OFF=NA)
      for(ipnt in 1:nrow(trackDF)){
        testsort=sorted
        testsort[i_allpnts]=trackDF$un_nona[ipnt]
        trackDF$MEANPWD_ON[ipnt]=mpd(df[testsort,on])
        trackDF$MEANPWD_OFF[ipnt]=mpd(df[testsort,off])
      }#end of testsort loop
      SortedStat=mpd(df[sorted,on])/mpd(df[sorted,off])
      if(is.nan(SortedStat)){SortedStat=0}
      trackDF$Stat=trackDF$MEANPWD_ON/trackDF$MEANPWD_OFF
      trackDF$MargStat=trackDF$Stat-SortedStat
      select=trackDF$un_nona[which.max(trackDF$MargStat)]
      
      if(length(select)>1){
        select=sample(select,1)
      }else if(length(select)<1){
        select=NA
      }
      
      #Assign selected point, and remove from further contention
      sorted[i_allpnts]=select
      unsorted[which(unsorted==select)]=NA
      
      if(DEBUG){
        print("###")
        print(i_allpnts)
        print("###")
        print(sorted)
        print(unsorted)
      }
      
    }#end of Nselect loop  
    TriedScores[Ntried]=mpd(df[sorted,on])/mpd(df[sorted,off])
    if(TriedScores[Ntried]>BestScore){
      BestSort=sorted
      BestScore=TriedScores[Ntried]
      print("Better Score!")
      print(TriedScores)
      print(sorted)
      }else{print(TriedScores)}
    
  }
  return(BestSort)
  
}

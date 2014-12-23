selectTrails<-function(info,data,meta,trails){
  rdata<-data[trails]
  rinfo<-info[,trails]
  rmeta=meta
  rmeta$ntrails=length(trails)
  rmeta$nsnapshots=sum(unlist(info["len",]))
  tstart=1
  for(j in 1:rmeta$ntrails){
    tend=tstart+as.numeric(rinfo["len",j])-1
    rinfo["mint",j]=tstart
    rinfo["maxt",j]=tend
    tstart=tend+1
  }
  return(list(info=rinfo,data=rdata,meta=rmeta))
}

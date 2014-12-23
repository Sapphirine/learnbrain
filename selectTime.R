selectTime<-function(info,data,meta,snapshots){
  ntrials<-length(data)
  rmeta<-meta
  rdata<-data
  rinfo<-info
  for(j in 1:ntrials){
    rdata[[j]]<-data[[j]][snapshots,]
    rinfo["len",j]<-length(snapshots)
  }
  rmeta$nsnapshots=sum(unlist(info["len",]))
  return(list(info=rinfo,data=rdata,meta=rmeta))
}

mri_infoTrials<-function(info,data,meta){
  study<-meta$study
  ntrials<-length(data)
  nvoxels<-dim(data[[1]])[2]
  minTrialLenCond<-vector()
  for(nt in 1:ntrials){
    cond<-info[["cond",nt]]
    if(cond>0){
    minTrialLenCond[cond]<-1000
    }
    
    
  }
  nconds<-length(minTrialLenCond)
  ntrialsCond<-matrix(0,nconds,1)
  
  for(nt in 1:ntrials){
    len<-info[["len",nt]]
    cond<-info[["cond",nt]]
    if(cond>0 & cond<=nconds){
      ntrialsCond[cond]<-ntrialsCond[cond]+1
      if(len<minTrialLenCond[cond]){
        minTrialLenCond[cond]<-len
      }
    }
  }
  trialBegin<-matrix(1,nconds,1)
  trialEnd<-apply(cbind(minTrialLenCond,matrix(1,nconds,1)+37),1,min)
  return(list(ntrials=ntrials,nvoxels=nvoxels,nconds=nconds,minTrialLenCond=minTrialLenCond,ntrialsCond=ntrialsCond,trialBegin=trialBegin,trialEnd=trialEnd))
}

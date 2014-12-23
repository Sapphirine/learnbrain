selectActiveVoxels<-function(info,data,meta,nToKeep){
  IDM_information<-IDMinfomation(info,data,meta,meta$study)
  ntrials<-IDM_information$nTrials
  nvoxels<-IDM_information$nVoxels
  nconds<-IDM_information$nConds
  minTrialLenCond<-IDM_information$minTrialLenCond
  ntrialsCond<-IDM_information$nTrialsCond
  
  information<-localInformation(meta$study)
  dimensions<-information$dimensions
  dimx<-dimensions[1]
  dimy<-dimensions[2]
  dimz<-dimensions[3]
  
  nvoxels<-dim(data[[1]])[2]
  activeCount<-matrix(0,1,nvoxels)
  pvalueMaps<-mri_computeTvalues(info,data,meta)
  nconds<-sqrt(length(pvalueMaps))
  for(c in 2:nconds){
    pvalues<-pvalueMaps[[nconds*1+c]]
    ensemble<-order(pvalues)
    topVoxels<-ensemble[1:nToKeep]
    activeCount[topVoxels]<-activeCount[topVoxels]+1
  }
  activeVoxels<-which(activeCount>0)
  nactive<-length(activeVoxels)
  nmeta<-meta
  nmeta$nvoxels<-nactive
  nmeta$colToCoord<-meta$colToCoord[activeVoxels,]
  nmeta$coordToCol<-array(0,dim(meta$coordToCol))
  for(v in 1:nactive){
    vc<-nmeta$colToCoord[v,]
    vx<-vc[1]
    vy<-vc[2]
    vz<-vc[3]
    nmeta$coordToCol[vx,vy,vz]=v

  }
  ndata<-list()
  for(nt in 1:meta$ntrials){
    ndata[[nt]]<-data[[nt]][,activeVoxels]
  }
  return(list(info=info,data=ndata,meta=nmeta))
}

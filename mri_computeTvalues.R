mri_computeTvalues<-function(info,data,meta){
  study<-meta$study
  subject<-meta$subject
  mri_infoT<-mri_infoTrials(info,data,meta)
  ntrials<-mri_infoT$ntrials
  nvoxels<-mri_infoT$nvoxels
  nconds<-mri_infoT$nconds
  minTrialLenCond<-mri_infoT$minTrialLenCond
  ntrialsCond<-mri_infoT$ntrialsCond
  condPairsToTest<-cbind(matrix(1,nconds-1,1),2:nconds)
  databins<-list()
  binidx<-matrix(0,nconds,1)
  clen<-matrix(0,nconds,1)
  
  for(c in 1:nconds){
    databins[[c]]<-list()
    for(i in 1:ntrials){
      cond<-info[["cond",i]]
      len<-info[["len",i]]
      if(cond>0 & cond==c){
        binidx[c]<-binidx[c]+1
        databins[[c]][[binidx[c]]]<-data[[i]]
        clen[c]<-clen[c]+len
      }
    }
  }
  cdata<-list()
  for(c in 1:nconds){
    cdata[[c]]<-matrix(,clen[c],nvoxels)
    idx<-1
    for(bi in 1:binidx[c]){
      nextidx<-idx+dim(databins[[c]][[bi]])[1]
      cdata[[c]][idx:(nextidx-1),]<-databins[[c]][[bi]]
      idx<-nextidx
    }
  }
  ntests<-dim(condPairsToTest)[1]
  results<-list()
  results[[nconds*nconds]]<-NA
  alpha<-0.01
  for(t in 1:ntests){
    c1<-condPairsToTest[t,1]
    c2<-condPairsToTest[t,2]
    if(c1==1){alternative<-"less"} else if(c2==1){alternative<-"greater"} else {alternative<-"two.sided"}
    pvalues<-matrix(0,1,nvoxels)
    for(v in 1:nvoxels){
      tt<-t.test(cdata[[c1]][,v],cdata[[c2]][,v],alternative=alternative)
      pvalues[v]<-tt$p.value
    }
    results[[c2+c1*nconds]]<-pvalues
  }
  return(results)
}

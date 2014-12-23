plotSnap<-function(i,d,m,trialNum,z,n){
  img<-matrix(0,m$dimx,m$dimy)
  snap<-data.frame(data[trialNum])[n,]
  snap<-as.numeric(snap)
  minactiv<-min(snap)
  maxactiv<-max(snap)
  snap<-snap-minactiv
  snap<-snap*63/(maxactiv-minactiv)+1
  index<-which(m$colToCoord[,3]==z)
  coord<-m$colToCoord
  for(i in 1:length(index)){
    img[coord[index[i],1],coord[index[i],2]]<-snap[index[i]]
  }
  img[1,1:64]=1:64
  img[2,1:64]=1:64
  img<-img[,64:1]
  return(img)
  #image(img,col=rainbow(64,start=0.64,end=0.63))
  
}

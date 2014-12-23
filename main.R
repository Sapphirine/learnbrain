#setwd("My Course/EECS Big Data/project/")
#library("R.matlab")
#library("dichromat")

source('~/My Course/EECS Big Data/project/fmri/plotSnap.R')
source('~/My Course/EECS Big Data/project/fmri/selectTrials.R')
source('~/My Course/EECS Big Data/project/fmri/selectTime.R')
source('~/My Course/EECS Big Data/project/fmri/idmToExamples.R')
source('~/My Course/EECS Big Data/project/fmri/IDMinformation.R')
source('~/My Course/EECS Big Data/project/fmri/localInformation.R')
source('~/My Course/EECS Big Data/project/fmri/mri_computeTvalues.R')
source('~/My Course/EECS Big Data/project/fmri/mri_infoTrials.R')
source('~/My Course/EECS Big Data/project/fmri/selectActiveVoxels.R')





subjects<-c("data-starplus-04847-v7.mat","data-starplus-04799-v7.mat","data-starplus-04820-v7.mat","data-starplus-05675-v7.mat","data-starplus-05680-v7.mat","data-starplus-05710-v7.mat")
for(i in 1:6){
dataraw<-readMat(subjects[i])
data<-dataraw$data
data<-lapply(data,function(x){return(x[[1]])})
info<-data.frame(dataraw$info)
meta<-dataraw$meta
meta<-meta[,1,1]
#info=info[,1,]
reduce<-selectActiveVoxels(info,data,meta,240)
data<-reduce$data
meta<-reduce$meta
info<-reduce$info
trails<-which(info["cond",]>1)
select<-selectTrails(info,data,meta,trails)
data1<-select$data
info1<-select$info
meta1<-select$meta
SP1<-selectTrails(info1,data1,meta1,which(info1["firstStimulus",]=="P"))
SS1<-selectTrails(info1,data1,meta1,which(info1["firstStimulus",]=="S"))
infoP1<-SP1$info
dataP1<-SP1$data
metaP1<-SP1$meta

infoS1<-SS1$info
dataS1<-SS1$data
metaS1<-SS1$meta

SP2<-selectTime(infoP1,dataP1,metaP1,1:16)
SS2<-selectTime(infoP1,dataP1,metaP1,17:32)
SP3<-selectTime(infoS1,dataS1,metaP1,17:32)
SS3<-selectTime(infoS1,dataS1,metaP1,1:16)

infoP2<-SP2$info
dataP2<-SP2$data
metaP2<-SP2$meta

infoS2<-SS2$info
dataS2<-SS2$data
metaS2<-SS2$meta

infoP3<-SP3$info
dataP3<-SP3$data
metaP3<-SP3$meta

infoS3<-SS3$info
dataS3<-SS3$data
metaS3<-SS3$meta

iEP2<-idmToExamples(infoP2,dataP2,metaP2)
iEP3<-idmToExamples(infoP3,dataP3,metaP3)
iES2<-idmToExamples(infoS2,dataS2,metaS2)
iES3<-idmToExamples(infoS3,dataS3,metaS3)
examplesP2<-iEP2$examples
labelsP2<-iEP2$labels
exInfoP2<-iEP2$expInfo
examplesP3<-iEP3$examples
labelsP3<-iEP3$labels
exInfoP3<-iEP3$expInfo

examplesS2<-iES2$examples
labelsS2<-iES2$labels
exInfoS2<-iES2$expInfo
examplesS3<-iES3$examples
labelsS3<-iES3$labels
exInfoS3<-iES3$expInfo

examplesP<-rbind(examplesP2,examplesP3)
examplesS<-rbind(examplesS2,examplesS3)
labelsP<-matrix(1,dim(examplesP)[1],1)
labelsS<-matrix(2,dim(examplesS)[1],1)
examples<-rbind(examplesP,examplesS)
labels<-rbind(labelsP,labelsS)
labels<-as.factor(labels)
index1<-sample(1:40,10)
index2<-sample(41:80,10)
train<-examples[c(-index1,-index2),]
test<-examples[c(index1,index2),]
test_labels<-labels[c(index1,index2)]
train_labels<-labels[c(-index1,-index2)]
fit<-naiveBayes(train,train_labels)
pred<-predict(fit,test)
print(table(pred,test_labels))
fit2<-svm(train,train_labels)
pred2<-predict(fit2,test)
print(table(pred2,test_labels))
}

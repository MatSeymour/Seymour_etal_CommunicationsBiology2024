


rm(list=ls())

setwd("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts/0.1.FinalScripts_githubbed")

dt<-read.csv("Data/SupplementaryData.csv")
ProjectInfo<-read.csv("Data/SupplementaryData_02.csv")

Counttables<-list()

for(i in unique(dt$SiteNumber)){
  
  temp<-dt[which(dt$SiteNumber==i),]
  temp<-temp[!is.na(temp$BIN),]
  temp2<-matrix(0,nrow=length(unique(temp$TrapEvent)),ncol=length(unique(temp$BIN)),dimnames = list(c(unique(temp$TrapEvent)),c(unique(temp$BIN))))
  
  for(j in unique(temp$TrapEvent)){
    temp2[j,colnames(temp2)[which(colnames(temp2)%in%as.character(data.frame(table(temp[which(temp$TrapEvent==j),"BIN"]))[,1]))]]<-data.frame(table(temp[which(temp$TrapEvent==j),"BIN"]))[,2]
  }
  
  Counttables[[which(unique(dt$SiteNumber)==i)]]<-temp2
  names(Counttables)[which(unique(dt$SiteNumber)==i)]<-i
  print(paste(which(unique(dt$SiteNumber)==i),"of",length(unique(dt$SiteNumber))))
};rm(list=c("temp","temp2","i","j"))


save(Counttables,file="Counttables.RData")

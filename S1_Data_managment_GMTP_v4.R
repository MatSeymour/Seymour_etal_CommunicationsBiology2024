
rm(list=ls())

library(adespatial)

setwd("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts/0.1.FinalScripts_githubbed")
load("Data/Counttables.RData")

EventInfo = read.csv("Data/SupplementaryData_02.csv")


sites <-unique(EventInfo$SiteNumber)
events <-c()
spp <-c()

for(i in sites){
  events <-c(events,rownames(Counttables[[as.character(i)]]))
  spp <-c(unique(c(spp,colnames(Counttables[[as.character(i)]]))))
}

CountMatrix <- matrix(0,nrow = length(events),ncol = length(spp),dimnames=list(c(events),c(spp)))

for(i in sites){
  CountMatrix[rownames(Counttables[[as.character(i)]]),colnames(Counttables[[as.character(i)]])] = 1*(Counttables[[as.character(i)]]>0)
}

CountMatrix <-CountMatrix[,colSums(CountMatrix)>0]
hist(colSums(CountMatrix))
hist(rowSums(CountMatrix))

EventInfo<-EventInfo[match(events,EventInfo$TrapEvent),]
all(EventInfo[,"TrapEvent"]==events)

PairedData<-data.frame(as.table(matrix(0,length(events),length(events),dimnames=list(c(events),c(events)))))[,1:2]
colnames(PairedData)<-c("Trap1","Trap2")

PairedData$Lat1<-NA;PairedData$Lat2<-NA;PairedData$Lon1<-NA;PairedData$Lon2<-NA;PairedData$Reg1<-NA;PairedData$Reg2<-NA;PairedData$Num1<-NA;PairedData$Num2<-NA

for(i in unique(EventInfo$SiteNumber)){
  S1<-EventInfo[which(EventInfo$SiteNumber==as.character(i)),"TrapEvent"]
  
  PairedData[which(PairedData$Trap1%in%S1),"Lat1"]<-unique(EventInfo[which(EventInfo$SiteNumber==as.character(i)),"Lat"])
  PairedData[which(PairedData$Trap2%in%S1),"Lat2"]<-unique(EventInfo[which(EventInfo$SiteNumber==as.character(i)),"Lat"])
  
  PairedData[which(PairedData$Trap1%in%S1),"Lon1"]<-unique(EventInfo[which(EventInfo$SiteNumber==as.character(i)),"Lon"])
  PairedData[which(PairedData$Trap2%in%S1),"Lon2"]<-unique(EventInfo[which(EventInfo$SiteNumber==as.character(i)),"Lon"])
  
  PairedData[which(PairedData$Trap1%in%S1),"Reg1"]<-unique(EventInfo[which(EventInfo$SiteNumber==as.character(i)),"Region"])
  PairedData[which(PairedData$Trap2%in%S1),"Reg2"]<-unique(EventInfo[which(EventInfo$SiteNumber==as.character(i)),"Region"])
  
  PairedData[which(PairedData$Trap1%in%S1),"Num1"]<-length(EventInfo[which(EventInfo$SiteNumber==as.character(i)),"TrapEvent"])
  PairedData[which(PairedData$Trap2%in%S1),"Num2"]<-length(EventInfo[which(EventInfo$SiteNumber==as.character(i)),"TrapEvent"])
  
  print(paste(which(unique(EventInfo$SiteNumber)==i)," - of - ",length(unique(EventInfo$SiteNumber))))
}

PairedData$Date1<-as.Date(NA);PairedData$Date2<-as.Date(NA)

for(i in unique(EventInfo$TrapEvent)){
  PairedData[which(PairedData$Trap1==i),"Date1"]<-as.Date(EventInfo[which(EventInfo$TrapEvent==i),"Date"])
  PairedData[which(PairedData$Trap2==i),"Date2"]<-as.Date(EventInfo[which(EventInfo$TrapEvent==i),"Date"])
  print(paste(which(unique(EventInfo$TrapEvent)==i)," - of - ",length(unique(EventInfo$TrapEvent))))
}

PairedData$beta.D.pod.qJ<-0;PairedData$beta.repl.pod.qJ<-0;PairedData$beta.rich.pod.qJ<-0

beta.pod.qJ<-beta.div.comp(CountMatrix,coef="J",quant=T)
PairedData$beta.D.pod.qJ<-1-data.frame(as.table(as.matrix(beta.pod.qJ$D)))$Freq
PairedData$beta.repl.pod.qJ<-data.frame(as.table(as.matrix(beta.pod.qJ$repl)))$Freq
PairedData$beta.rich.pod.qJ<-data.frame(as.table(as.matrix(beta.pod.qJ$rich)))$Freq

PairedData$RegionGroup<-NA

for(i in unique(paste(PairedData$Reg1,PairedData$Reg2,sep=":"))){
  
  S1<-unlist(strsplit(i,":"))[1]
  S2<-unlist(strsplit(i,":"))[2]
  
  if(S1==S2){
    PairedData[which(PairedData$Reg1==S1&PairedData$Reg2==S2),"RegionGroup"]<-S1
  }else{PairedData[which(PairedData$Reg1==S1&PairedData$Reg2==S2),"RegionGroup"]<-"InterRegion"}
  }

PairedData$Site1<-NA;PairedData$Site2<-NA

for(i in unique(EventInfo$Site)){
  PairedData[which(PairedData$Trap1%in%EventInfo[which(EventInfo$Site==as.character(i)),"TrapEvent"]),"Site1"]<-i
  PairedData[which(PairedData$Trap2%in%EventInfo[which(EventInfo$Site==as.character(i)),"TrapEvent"]),"Site2"]<-i
}


m1<-length(unique(as.character(PairedData$Trap1)))*length(unique(as.character(PairedData$Trap1)))-length(unique(as.character(PairedData$Trap1)))
#number of unique pairs
m1/2+length(unique(as.character(PairedData$Trap1)))

#remove duplicates
u1<-combn(unique(as.character(PairedData$Trap1)),2,simplify=TRUE)
u2<-paste(u1[1,],u1[2,],sep="_")
u2<-c(u2,paste(unique(as.character(PairedData$Trap1)),unique(as.character(PairedData$Trap1)),sep="_"))
PairedData<-PairedData[which(paste(PairedData$Trap1,PairedData$Trap2,sep="_")%in%u2),]


PairedData$Date1_angle<-0
PairedData$Date2_angle<-0

PairedData$Date1_angle<-as.POSIXlt(PairedData$Date1)$yday*(360/365)
PairedData$Date2_angle<-as.POSIXlt(PairedData$Date2)$yday*(360/365)

for(i in 1:nrow(PairedData)){
  PairedData$TimeAngleDiff[i]<-min(c(abs(PairedData$Date1_angle[i]-PairedData$Date2_angle[i]),
                                     (360-abs(PairedData$Date1_angle[i]-PairedData$Date2_angle[i]))))/(360/365)
  
  print(paste(i,"of",nrow(PairedData),sep=" "))
}

#creaete a site level summary data file

SiteInfo<-data.frame(Site=unique(EventInfo$SiteNumber))

SiteInfo$Longitude<-0
SiteInfo$Latitude<-0
SiteInfo$TrapEvents<-0
SiteInfo$Country<-NA
SiteInfo$Region<-NA


for(i in SiteInfo$Site){
  SiteInfo[which(SiteInfo$Site==i),"Longitude"]<-unique(EventInfo[which(EventInfo$SiteNumber==i),"Lon"])
  SiteInfo[which(SiteInfo$Site==i),"Latitude"]<-unique(EventInfo[which(EventInfo$SiteNumber==i),"Lat"])
  SiteInfo[which(SiteInfo$Site==i),"TrapEvents"]<-nrow(EventInfo[which(EventInfo$SiteNumber==i),])
  SiteInfo[which(SiteInfo$Site==i),"Country"]<-unique(EventInfo[which(EventInfo$SiteNumber==i),"Country"])
  SiteInfo[which(SiteInfo$Site==i),"Region"]<-unique(EventInfo[which(EventInfo$SiteNumber==i),"Region"])
}




save(EventInfo,SiteInfo,PairedData,file = "S1_output2v2.RData")

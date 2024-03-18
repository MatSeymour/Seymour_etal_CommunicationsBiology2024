
rm(list = ls())

library(lubridate)
library(ggplot2)
library(viridis)
library(gridExtra)


setwd("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts")


load(file = "S1_output2v2.RData")
 
EventInfo<-read.csv("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.01_FinalSubmissions/0.1.ComsBio2023/0.3.Revision18Mar2024/0.5.SupplementaryData_02.csv")
WorkingData<-read.csv("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.01_FinalSubmissions/0.1.ComsBio2023/0.3.Revision18Mar2024/0.4.SupplementaryData.csv")
 

EventInfo$Richness<-0

for(i in unique(EventInfo$TrapEvent)){
  
  E1<-unique(EventInfo[which(EventInfo$TrapEvent==i),"SiteNumber"])

  temp<-PairedData[which(PairedData$Trap1==i|PairedData$Trap2==i),]
  temp<-temp[which(temp$Reg1==temp$Reg2),]
  
  EventInfo[which(EventInfo$TrapEvent==i),"Richness"]<-length(unique(WorkingData[which(WorkingData$TrapEvent==i),"BIN"]))
  
  EventInfo[which(EventInfo$TrapEvent==i),"TotalBeta"]<-mean(temp[,"beta.D.pod.qJ"])
  EventInfo[which(EventInfo$TrapEvent==i),"TotalBeta_se"]<-sd(temp[,"beta.D.pod.qJ"])/sqrt(length(temp[,"beta.D.pod.qJ"]))
  
  EventInfo[which(EventInfo$TrapEvent==i),"Repl"]<-mean(temp[,"beta.repl.pod.qJ"])
  EventInfo[which(EventInfo$TrapEvent==i),"Repl_se"]<-sd(temp[,"beta.repl.pod.qJ"])/sqrt(length(temp[,"beta.repl.pod.qJ"]))
  
  EventInfo[which(EventInfo$TrapEvent==i),"RichD"]<-mean(temp[,"beta.rich.pod.qJ"])
  EventInfo[which(EventInfo$TrapEvent==i),"RichD_se"]<-sd(temp[,"beta.rich.pod.qJ"])/sqrt(length(temp[,"beta.rich.pod.qJ"]))
}


################################################################### Figure S01 #########################################################################


for(i in 1:nrow(EventInfo)){
  temp<-PairedData[which(PairedData$Trap1==EventInfo$TrapEvent[i]|PairedData$Trap2==EventInfo$TrapEvent[i]),]
  temp<-temp[which(temp$RegionGroup2!="Global"),]
  
  EventInfo$MeanDist[i]<-mean(temp$d.space)
  EventInfo$Region[i]<-unique(temp$RegionGroup2)
}

unique(EventInfo$Region)

EventInfo$Region<-factor(EventInfo$Region,levels=c("North America","South America","Eurasia","Africa","Oceania"))
EventInfo$AbsLat<-round(abs(EventInfo$Lat)/5)*5
EventInfo$MeanDistR<-round(EventInfo$MeanDist/100)*100

EventInfo$Month<-month(EventInfo$Date,label=FALSE)

Cboard<-data.frame(Region=c("North America", "Oceania","Eurasia","South America","Africa"),colour=NA)
Cboard$colour<-viridis(length(unique(SiteInfo$Region)),option="H")

P1<-ggplot(data=EventInfo)+
  theme_classic()+
  facet_wrap(~Region,scales = "free",nrow=1)+
  geom_boxplot(aes(x=AbsLat,y=Richness,group=AbsLat,fill=Region))+
  scale_fill_manual(breaks = Cboard$Region,values=Cboard$colour)+
  theme(legend.position="none")+
  xlab("Absolute latitude")

P2<-ggplot(data=EventInfo)+
  theme_classic()+
  facet_wrap(~Region,scales = "free",nrow=1)+
  geom_boxplot(aes(x=MeanDistR,y=Richness,group=MeanDistR,fill=Region))+
  scale_fill_manual(breaks = Cboard$Region,values=Cboard$colour)+
  theme(legend.position="none",axis.text.x=element_text(angle=45,vjust=0.5) )+
  xlab("Mean distance (km)")
  

P3<-ggplot(data=EventInfo)+
  theme_classic()+
  facet_wrap(~Region,scales = "free",nrow=1)+
  geom_boxplot(aes(x=Month,y=Richness,group=Month,fill=Region))+
  scale_fill_manual(breaks = Cboard$Region,values=Cboard$colour)+
  theme(legend.position="none")+
  scale_x_continuous(breaks=seq(1,12,by=2))+
  xlab("Month")


ggsave(filename="Figures/Figure_S01.png",grid.arrange(P1,P2,P3,ncol=1),width=30,height=20,units="cm",dpi=300)

############### figure S06

ggplot(data=EventInfo)+
  theme_classic()+
  facet_wrap(~Habitat,nrow=1)+
  geom_point(shape=21,size=2,aes(x=SiteNumber,y=Richness,group=Habitat,fill=Region),color="grey")+
  scale_fill_manual(breaks = Cboard$Region,values=Cboard$colour)+
  labs(fill = "Region")+
  xlab("TrapNumber")

ggsave(filename="Figures/Figure_S06.png",width=20,height=15,units="cm",dpi=300)

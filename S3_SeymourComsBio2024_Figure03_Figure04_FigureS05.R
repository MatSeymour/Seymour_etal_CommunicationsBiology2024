

rm(list=ls())

library(ggplot2)
library(viridis)
library(gridExtra)

setwd("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts")
load(file = "S1_output2v2.RData")

PairedData[which(PairedData$RegionGroup=="InterRegion"),"RegionGroup"]<-"Global"
RegionDat<-data.frame(Region=c("Global","North America", "South America", "Eurasia", "Africa", "Oceania"))

for(i in RegionDat$Region){
  RegionDat[which(RegionDat$Region==i),"d.space1"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.space"])[1],2)
  RegionDat[which(RegionDat$Region==i),"d.space2"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.space"])[2],2)
  RegionDat[which(RegionDat$Region==i),"d.lat1"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.lat"])[1],1)
  RegionDat[which(RegionDat$Region==i),"d.lat2"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.lat"])[2],2)
  RegionDat[which(RegionDat$Region==i),"d.time1"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.time"])[1],0)
  RegionDat[which(RegionDat$Region==i),"d.time2"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.time"])[2],0)
}

for(i in 1:nrow(RegionDat)){
RegionDat$d.spaceb[i]<-diff(seq(from=RegionDat$d.space1[i],to=RegionDat$d.space2[2],length.out=25)[c(1,2)])
RegionDat$d.timeb[i]<-diff(seq(from=RegionDat$d.time1[i],to=RegionDat$d.time2[2],length.out=25)[c(1,2)])
RegionDat$d.latb[i]<-diff(seq(from=RegionDat$d.lat1[i],to=RegionDat$d.lat2[2],length.out=25)[c(1,2)])
}

#### setting up heat maps

Tot_LTS<-c("Global","North America");Tot_A<-c("South America","Oceania","Eurasia","Africa")
Repl_LTS<-c("Global");Repl_LS<-c();Repl_LT<-c("North America", "Eurasia");Repl_A<-c("South America","Oceania","Africa")
Rich_LTS<-c("Global");Rich_LS<-c("North America");Rich_LT<-c();Rich_A<-c("Eurasia","South America","Oceania","Africa")


#space/distance
newdat_dist<-list()

for(i in 1:length(RegionDat[,1])){

  temp<-expand.grid(d.space=seq(from=RegionDat$d.space1[i],to=RegionDat$d.space2[i],length.out=100),
                    d.lat=seq(from=RegionDat$d.lat1[i],to=RegionDat$d.lat2[i],length.out=100),3)
  
  temp$d.time<-0
  
  temp2<-PairedData[which(PairedData$RegionGroup2==RegionDat$Region[i]),]
  temp2$site.weights<-1/(temp2$Num1*temp2$Num2)
  
  if(RegionDat$Region[i]%in%Tot_LTS){
    mod0<-lm(data=temp2,(1-beta.D.pod.qJ)~d.space*d.lat+d.time*d.lat,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Repl_A){
    mod0<-lm(data=temp2,(1-beta.D.pod.qJ)~d.space+d.time+d.lat,weights = site.weights)
  }
  
  if(RegionDat$Region[i]%in%Repl_LTS){
    mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space*d.lat+d.time*d.lat,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Repl_LS){
    mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space*d.lat+d.time,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Repl_LT){
    mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space+d.time*d.lat,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Repl_A){
    mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space+d.time+d.lat,weights = site.weights)
  }
  
  if(RegionDat$Region[i]%in%Rich_LTS){
    mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space*d.lat+d.time*d.lat,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Rich_LS){
    mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space*d.lat+d.time,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Rich_LT){
    mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space+d.time*d.lat,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Rich_A){
    mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space+d.time+d.lat,weights = site.weights)
  }
  
  t0<-pmin(predict(mod0,newdata=temp,type="response"),1)
  tT<-predict(mod1,newdata=temp,type="response")
  tN<-predict(mod2,newdata=temp,type="response")
  
  temp$total<-t0
  temp$Turnover<-tT
  temp$Nestedness<-tN
  
  temp$total_t<-t0
  temp$Turnover_t<-tT
  temp$Nestedness_t<-tN
  
  
  newdat_dist[[i]]<-temp
  names(newdat_dist)[i]<-RegionDat$Region[i]
}

#time

newdat_time<-list()


for(i in 1:length(RegionDat[,1])){
  temp<-expand.grid(d.time=seq(from=RegionDat$d.time1[i],to=RegionDat$d.time2[i],length.out=200),
                    d.lat=seq(from=RegionDat$d.lat1[i],to=RegionDat$d.lat2[i],length.out=200))
  temp$d.space<-0
  temp2<-PairedData[which(PairedData$RegionGroup2==RegionDat$Region[i]),]
  temp2$site.weights<-1/(temp2$Num1*temp2$Num2)
  
  if(RegionDat$Region[i]%in%Repl_LTS){
    mod0<-lm(data=temp2,(1-beta.D.pod.qJ)~d.space*d.lat+d.time*d.lat,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Repl_A){
    mod0<-lm(data=temp2,(1-beta.D.pod.qJ)~d.space+d.time+d.lat,weights = site.weights)
  }
  
  
  if(RegionDat$Region[i]%in%Repl_LTS){
    mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space*d.lat+d.time*d.lat,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Repl_LS){
    mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space*d.lat+d.time,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Repl_LT){
    mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space+d.time*d.lat,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Repl_A){
    mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space+d.time+d.lat,weights = site.weights)
  }
  
  if(RegionDat$Region[i]%in%Rich_LTS){
    mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space*d.lat+d.time*d.lat,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Rich_LS){
    mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space*d.lat+d.time,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Rich_LT){
    mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space+d.time*d.lat,weights = site.weights)
  }else if(RegionDat$Region[i]%in%Rich_A){
    mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space+d.time+d.lat,weights = site.weights)
  }
  
  t0<-pmin(predict(mod0,newdata=temp,type="response"),1)
  tT<-predict(mod1,newdata=temp,type="response")
  tN<-predict(mod2,newdata=temp,type="response")
  
  temp$total<-t0
  temp$Turnover<-tT
  temp$Nestedness<-tN
  
  temp$total_t<-t0
  temp$Turnover_t<-tT
  temp$Nestedness_t<-tN
  
  newdat_time[[i]]<-temp
  names(newdat_time)[i]<-RegionDat$Region[i]
}


Lims1<-c()
for(i in names(newdat_dist)){
  Lims1<-c(Lims1,round(range(newdat_dist[[i]]$Turnover_t),2))
}
range(Lims1)

Lims2<-c()
for(i in names(newdat_dist)){
  Lims2<-c(Lims2,round(range(newdat_dist[[i]]$Nestedness_t),2))
}
range(Lims2)

Lims3<-c()
for(i in names(newdat_time)){
  Lims3<-c(Lims3,round(range(newdat_time[[i]]$Turnover_t),2))
}
range(Lims3)

Lims4<-c()
for(i in names(newdat_time)){
  Lims4<-c(Lims4,round(range(newdat_time[[i]]$Nestedness_t),2))
}
range(Lims4)


Lims5<-c()
for(i in names(newdat_dist)){
  Lims5<-c(Lims5,round(range(newdat_dist[[i]]$total_t),2))
}
range(Lims5)

Lims6<-c()
for(i in names(newdat_time)){
  Lims6<-c(Lims6,round(range(newdat_time[[i]]$total_t),2))
}
range(Lims6)




dist_breaks<-data.frame(matrix(ncol=4,nrow=0,dimnames = list(c(),c("Region","lat","dist","time"))))

for(i in 1:length(newdat_dist)){
  newdat_dist[[i]]$w<-(max(newdat_dist[[i]]$d.space)-min(newdat_dist[[i]]$d.space))*0.05
  newdat_dist[[i]]$h<-(max(newdat_dist[[i]]$d.lat)-min(newdat_dist[[i]]$d.lat))*0.05
  
  tod<-max(newdat_dist[[i]]$d.space);fromd<-min(newdat_dist[[i]]$d.space)
  if(tod-fromd<3000){x<- -2;tod<-round(tod,x);fromd<-round(fromd,x)}else{
    x <- -3;tod<-round(tod,x);fromd<-round(fromd,x)}
  d1<-round(seq(from=fromd,to=tod,by=((tod-fromd)/3)),x)
  
  tol<-max(newdat_dist[[i]]$d.lat);froml<-min(newdat_dist[[i]]$d.lat)
  if(tol-froml<10){x<- 0;tol<-round(tol,x);fromd<-round(froml,x)}else{
    x <- -1;tol<-round(tol,x);froml<-round(froml,x)}
  l1<-round(seq(from=froml,to=tol,by=((tol-froml)/3)),x)
  
  tot<-max(newdat_time[[i]]$d.time);fromt<-min(newdat_time[[i]]$d.time)
  if(tot-fromt<1000){x<- -1;tot<-round(tot,x);fromd<-round(fromt,x)}else{
    x <- -2;tot<-round(tot,x);fromt<-round(fromt,x)}
  t1<-round(seq(from=fromt,to=tot,by=((tot-fromt)/3)),x)
  
  dist_breaks<-rbind(dist_breaks,data.frame(Region=names(newdat_dist)[i],lat=l1,dist=d1,time=t1))
}

#matching scales

for(i in 1:nrow(RegionDat)){
  RegionDat$UpperTurn[i]<-max(c(newdat_dist[[which(names(newdat_dist)==RegionDat$Region[i])]][,"Turnover_t"],
                                newdat_time[[which(names(newdat_time)==RegionDat$Region[i])]][,"Turnover_t"]))
  RegionDat$LowerTurn[i]<-min(c(newdat_dist[[which(names(newdat_dist)==RegionDat$Region[i])]][,"Turnover_t"],
                                newdat_time[[which(names(newdat_time)==RegionDat$Region[i])]][,"Turnover_t"]))
  
  RegionDat$UpperNest[i]<-max(c(newdat_dist[[which(names(newdat_dist)==RegionDat$Region[i])]][,"Nestedness_t"],
                                newdat_time[[which(names(newdat_time)==RegionDat$Region[i])]][,"Nestedness_t"]))
  RegionDat$LowerNest[i]<-min(c(newdat_dist[[which(names(newdat_dist)==RegionDat$Region[i])]][,"Nestedness_t"],
                                newdat_time[[which(names(newdat_time)==RegionDat$Region[i])]][,"Nestedness_t"]))
  
  RegionDat$UpperTot[i]<-max(c(newdat_dist[[which(names(newdat_dist)==RegionDat$Region[i])]][,"total_t"],
                                newdat_time[[which(names(newdat_time)==RegionDat$Region[i])]][,"total_t"]))
  RegionDat$LowerTot[i]<-min(c(newdat_dist[[which(names(newdat_dist)==RegionDat$Region[i])]][,"total_t"],
                                newdat_time[[which(names(newdat_time)==RegionDat$Region[i])]][,"total_t"]))
  
}


L1<-min(RegionDat$LowerTurn-0.01);L2<-max(RegionDat$UpperTurn+0.01)
L3<-min(RegionDat$LowerNest-0.01);L4<-max(RegionDat$UpperNest+0.01)


P1<-ggplot(data=newdat_dist[[1]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_dist[1]),"*"))+  labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P2<-ggplot(data=newdat_dist[[2]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_dist[2]),""))+labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P3<-ggplot(data=newdat_dist[[3]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_dist[3])))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P4<-ggplot(data=newdat_dist[[4]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(names(newdat_dist[4]))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P5<-ggplot(data=newdat_dist[[5]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_dist[5]),""))+  labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P6<-ggplot(data=newdat_dist[[6]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_dist[6]),""))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))


P8<-ggplot(data=newdat_dist[[1]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_dist[1]),"*"))+  labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P9<-ggplot(data=newdat_dist[[2]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_dist[2]),""))+  labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P10<-ggplot(data=newdat_dist[[3]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_dist[3])))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P11<-ggplot(data=newdat_dist[[4]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(names(newdat_dist[4]))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P12<-ggplot(data=newdat_dist[[5]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"dist"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_dist[5])))+  labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P13<-ggplot(data=newdat_dist[[6]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[6]]$w),height=unique(newdat_dist[[6]]$h)))+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_dist[6]),""))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))



temp<-list(P1,P2,P3,P4,P5,P6,P8,P9,P10,P11,P12,P13)

lay<-rbind(c(1,2,3),
           c(4,5,6),
           c(7,9,10),
           c(11,12,13))

ggsave(filename="Figures/Figure03.pdf",(grid.arrange(grobs = temp, layout_matrix = lay)),width=30,height=30,units="cm",dpi=300)

#time


for(i in 1:length(newdat_time)){
  newdat_time[[i]]$w<-(max(newdat_time[[i]]$d.time)-min(newdat_time[[i]]$d.time))*0.03
  newdat_time[[i]]$h<-(max(newdat_time[[i]]$d.lat)-min(newdat_time[[i]]$d.lat))*0.03
  
  newdat_time[[i]][which(newdat_time[[i]][,"Nestedness_t"]<0),"Nestedness"]<-0
}


L1<-min(RegionDat$LowerTurn);L2<-max(RegionDat$UpperTurn)
L3<-min(RegionDat$LowerNest);L4<-max(RegionDat$UpperNest)


P1<-ggplot(data=newdat_time[[1]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[1]),"*"))+  labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P2<-ggplot(data=newdat_time[[2]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[2]),"*"))+labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P3<-ggplot(data=newdat_time[[3]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[3]),""))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P4<-ggplot(data=newdat_time[[4]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[4]),"*"))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P5<-ggplot(data=newdat_time[[5]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[5]),""))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P6<-ggplot(data=newdat_time[[6]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[6])))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P8<-ggplot(data=newdat_time[[1]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_time[1]),"*"))+  labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P9<-ggplot(data=newdat_time[[2]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(names(newdat_time[2]))+labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P10<-ggplot(data=newdat_time[[3]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_time[3])))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P11<-ggplot(data=newdat_time[[4]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(names(newdat_time[4]))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P12<-ggplot(data=newdat_time[[5]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(names(newdat_time[5]))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())


P13<-ggplot(data=newdat_time[[6]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(names(newdat_time[6]))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

temp<-list(P1,P2,P3,P4,P5,P6,P8,P9,P10,P11,P12,P13)

lay<-rbind(c(1,2,3),
           c(4,5,6),
           c(7,9,10),
           c(11,12,13))

ggsave(filename="Figures/Figure04.pdf",(grid.arrange(grobs = temp, layout_matrix = lay)),width=30,height=30,units="cm",dpi=300)


####################### supplement total beta diversity Figure S5

L5<-min(RegionDat$LowerTot-0.01);L6<-max(RegionDat$UpperTot+0.01)
L7<-min(RegionDat$LowerTot);L8<-max(RegionDat$UpperTot)

P1<-ggplot(data=newdat_dist[[1]],aes(y=d.lat,x=d.space,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L5,L6))+
  ggtitle(paste0(names(newdat_dist[1]),""))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P2<-ggplot(data=newdat_dist[[2]],aes(y=d.lat,x=d.space,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[2]]$w),height=unique(newdat_dist[[2]]$h)))+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L5,L6))+
  ggtitle(paste0(names(newdat_dist[2]),""))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P3<-ggplot(data=newdat_dist[[3]],aes(y=d.lat,x=d.space,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[3]]$w),height=unique(newdat_dist[[3]]$h)))+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L5,L6))+
  ggtitle(paste0(names(newdat_dist[3]),""))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P4<-ggplot(data=newdat_dist[[4]],aes(y=d.lat,x=d.space,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[4]]$w),height=unique(newdat_dist[[4]]$h)))+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L5,L6))+
  ggtitle(paste0(names(newdat_dist[4]),""))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P5<-ggplot(data=newdat_dist[[5]],aes(y=d.lat,x=d.space,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[5]]$w),height=unique(newdat_dist[[5]]$h)))+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L5,L6))+
  ggtitle(paste0(names(newdat_dist[5]),""))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P6<-ggplot(data=newdat_dist[[6]],aes(y=d.lat,x=d.space,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[6]]$w),height=unique(newdat_dist[[6]]$h)))+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L5,L6))+
  ggtitle(paste0(names(newdat_dist[6]),""))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

#time
P7<-ggplot(data=newdat_time[[1]],aes(y=d.lat,x=d.time,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L7,L8))+
  ggtitle(names(newdat_time[1]))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P8<-ggplot(data=newdat_time[[2]],aes(y=d.lat,x=d.time,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L7,L8))+
  ggtitle(names(newdat_time[2]))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P9<-ggplot(data=newdat_time[[3]],aes(y=d.lat,x=d.time,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L7,L8))+
  ggtitle(names(newdat_time[3]))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P10<-ggplot(data=newdat_time[[4]],aes(y=d.lat,x=d.time,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L7,L8))+
  ggtitle(names(newdat_time[4]))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P11<-ggplot(data=newdat_time[[5]],aes(y=d.lat,x=d.time,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L7,L8))+
  ggtitle(names(newdat_time[5]))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P12<-ggplot(data=newdat_time[[6]],aes(y=d.lat,x=d.time,fill=total_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"time"]))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L7,L8))+
  ggtitle(names(newdat_time[6]))+ labs(fill='Beta Diversity',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())



temp<-list(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12)

lay<-rbind(c(1,2,3),
           c(4,5,6),
           c(7,8,9),
           c(10,11,12))

ggsave(filename="Figures/Figure_S05.pdf",(grid.arrange(grobs = temp, layout_matrix = lay)),width=30,height=30,units="cm",dpi=600)













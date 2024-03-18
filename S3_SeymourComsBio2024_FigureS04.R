
rm(list=ls())

library(ggplot2)
library(cowplot)


setwd("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts")
load(file = "S1_output2v2.RData")
load("Data/Counttables.RData")

s1<-16


BetaMeta<-data.frame(lat=rep(seq(from=range(PairedData$d.lat)[1],to=range(PairedData$d.lat)[2],length.out=s1)[2:s1],times=6),
           space=rep(seq(from=range(PairedData$d.space)[1],to=range(PairedData$d.space)[2],length.out=s1)[2:s1],times=6),
           time=rep(seq(from=range(PairedData$d.time)[1],to=range(PairedData$d.time)[2],length.out=s1)[2:s1],times=6),
           Region=rep(unique(PairedData$RegionGroup2),each=s1-1),
           beta_lat=NA,beta_space=NA,beta_time=NA,beta_latse=NA,beta_spacese=NA,beta_timese=NA
           )
           

  for(i in 1:nrow(BetaMeta)){
    if(i==1){
      
      t1<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.lat<=BetaMeta$lat[i]),"beta.D.pod.qJ"]
      if(length(t1)>0){
        BetaMeta$beta_lat[i]<-mean(t1)
        BetaMeta$beta_latse[i]<-sd(t1)/sqrt(length(t1)) }
      
      t2<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.space<=BetaMeta$space[i]),"beta.D.pod.qJ"]
      if(length(t2)>0){
        BetaMeta$beta_space[i]<-mean(t2)
        BetaMeta$beta_spacese[i]<-sd(t2)/sqrt(length(t2))}
      
      t3<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.ld.timeat<=BetaMeta$time[i]),"beta.D.pod.qJ"]
      if(length(t3)>0){
        BetaMeta$beta_time[i]<-mean(t3)
        BetaMeta$beta_timese[i]<-sd(t3)/sqrt(length(t3))}  

    }else if(BetaMeta$Region[i]!=BetaMeta$Region[i-1]){
      
      t1<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.lat<=BetaMeta$lat[i]),"beta.D.pod.qJ"]
      if(length(t1)>0){
        BetaMeta$beta_lat[i]<-mean(t1)
        BetaMeta$beta_latse[i]<-sd(t1)/sqrt(length(t1))}
      
      t2<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.space<=BetaMeta$space[i]),"beta.D.pod.qJ"]
      if(length(t2)>0){
        BetaMeta$beta_space[i]<-mean(t2)
        BetaMeta$beta_spacese[i]<-sd(t2)/sqrt(length(t2))}
           
      t3<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.ld.timeat<=BetaMeta$time[i]),"beta.D.pod.qJ"]
      if(length(t3)>0){
        BetaMeta$beta_time[i]<-mean(t3)
        BetaMeta$beta_timese[i]<-sd(t3)/sqrt(length(t3))}
       
    }else if(BetaMeta$Region[i]==BetaMeta$Region[i-1]){
      
      t1<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.lat<=BetaMeta$lat[i]&PairedData$d.lat>BetaMeta$lat[i-1]),"beta.D.pod.qJ"]
      if(length(t1)>0){
        BetaMeta$beta_lat[i]<-mean(t1)
        BetaMeta$beta_latse[i]<-sd(t1)/sqrt(length(t1))}
      
      t2<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.space<=BetaMeta$space[i]&PairedData$d.space>BetaMeta$space[i-1]),"beta.D.pod.qJ"]
      if(length(t2)>0){
        BetaMeta$beta_space[i]<-mean(t2)
        BetaMeta$beta_spacese[i]<-sd(t2)/sqrt(length(t2))}
      
      t3<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.time<=BetaMeta$time[i]&PairedData$d.time>BetaMeta$time[i-1]),"beta.D.pod.qJ"]
      if(length(t3)>0){
        BetaMeta$beta_time[i]<-mean(t3)
        BetaMeta$beta_timese[i]<-sd(t3)/sqrt(length(t3))}
      }
    
  }

BetaMeta$Region<-factor(BetaMeta$Region,levels=c("Global","North America","South America","Eurasia","Africa","Oceania"))


P1<-ggplot(data=BetaMeta[!is.na(BetaMeta$beta_lat),],aes(x=lat,y=1-beta_lat))+
  geom_point()+
  geom_errorbar(data=BetaMeta[!is.na(BetaMeta$beta_lat),],aes(ymin=1-beta_lat-beta_latse,ymax=1-beta_lat+beta_latse))+
  facet_wrap(~Region,nrow=1,scales="free_y")+
  geom_smooth(method = lm, se = FALSE)+
  theme_bw()+
  labs(x="Absolute latitude",y="Beta-diversity \n (Jaccard dissimilarity)")+
  scale_y_continuous(breaks = function(x) pretty(x, n = 3),labels=function(x) sprintf("%.3f", x))+
  scale_x_continuous(breaks = function(x) pretty(x, n = 3),labels=function(x) sprintf("%.0f", x))


P2<-ggplot(data=BetaMeta[!is.na(BetaMeta$beta_time),],aes(x=time,y=1-beta_time))+
  geom_point()+
  geom_errorbar(data=BetaMeta[!is.na(BetaMeta$beta_time),],aes(ymin=1-beta_time-beta_timese,ymax=1-beta_time+beta_timese))+
  facet_wrap(~Region,nrow=1,scales="free_y")+
  geom_smooth(method = lm, se = FALSE)+
  theme_bw()+
  labs(x="Time (days)",y="Beta-diversity \n (Jaccard dissimilarity)")+
  scale_y_continuous(breaks = function(x) pretty(x, n = 3),labels=function(x) sprintf("%.3f", x))+
  scale_x_continuous(breaks = function(x) pretty(x, n = 3),labels=function(x) sprintf("%.0f", x))

P3<-ggplot(data=BetaMeta[!is.na(BetaMeta$beta_space),],aes(x=space,y=1-beta_space))+
  geom_point()+
  geom_errorbar(data=BetaMeta[!is.na(BetaMeta$beta_space),],aes(ymin=1-beta_space-beta_spacese,ymax=1-beta_space+beta_spacese))+
  facet_wrap(~Region,nrow=1,scales="free_y")+
  geom_smooth(method = lm, se = FALSE)+
  theme_bw()+
  scale_x_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(1000,7000,13000,19000))+
  labs(x="Distance (km)",y="Beta-diversity \n (Jaccard dissimilarity)")+
  scale_y_continuous(breaks = function(x) pretty(x, n = 3),labels=function(x) sprintf("%.3f", x))+
  theme(axis.text.x=element_text(angle=-35,vjust=0))


ggsave(file="Figures/Figure_S04.png",
       cowplot:::plot_grid(P1,P2,P3, align = "v", nrow = 3, rel_heights = c(1/3,1/3, 1/3)),
       width=25,height=20,units="cm",dpi=300)






















library(ggplot2)
library(sf)
library(rnaturalearth)
library(viridis)
library(rgl)
library(gridExtra)
library(geosphere)
library(rgl)
library(fasttime)
library(vegan)
library(ggfortify)
library(geosphere)
library(maps)
library(cowplot)

#"C:/Users/User/Desktop/External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts"

setwd("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts")
load(file = "S1_output2v2.RData")
load("Data/Counttables.RData")

# PairedData$Date1_angle<-0
# PairedData$Date2_angle<-0
# 
# PairedData$Date1_angle<-as.POSIXlt(PairedData$Date1)$yday*(360/365)
# PairedData$Date2_angle<-as.POSIXlt(PairedData$Date2)$yday*(360/365)
# 
# for(i in 1:nrow(PairedData)){
#   PairedData$TimeAngleDiff[i]<-min(c(abs(PairedData$Date1_angle[i]-PairedData$Date2_angle[i]),
#                                      (360-abs(PairedData$Date1_angle[i]-PairedData$Date2_angle[i]))))/(360/365)
#   
#   print(paste(i,"of",nrow(PairedData),sep=" "))
# }
# 
# save(SiteInfo,EventInfo,PairedData,file = "S1_output2v2.RData")

PairedData<-PairedData[which(PairedData$Trap1!=PairedData$Trap2),]

unique(PairedData$RegionGroup2)

nrow(PairedData[which(PairedData$RegionGroup2=="Global"&PairedData$beta.D.pod.qJ==0),])/nrow(PairedData[which(PairedData$RegionGroup2=="Global"),])
nrow(PairedData[which(PairedData$RegionGroup2=="North America"&PairedData$beta.D.pod.qJ==0),])/nrow(PairedData[which(PairedData$RegionGroup2=="North America"),])
nrow(PairedData[which(PairedData$RegionGroup2=="South America"&PairedData$beta.D.pod.qJ==0),])/nrow(PairedData[which(PairedData$RegionGroup2=="South America"),])
nrow(PairedData[which(PairedData$RegionGroup2=="Oceania"&PairedData$beta.D.pod.qJ==0),])/nrow(PairedData[which(PairedData$RegionGroup2=="Oceania"),])
nrow(PairedData[which(PairedData$RegionGroup2=="Eurasia"&PairedData$beta.D.pod.qJ==0),])/nrow(PairedData[which(PairedData$RegionGroup2=="Eurasia"),])
nrow(PairedData[which(PairedData$RegionGroup2=="Africa"&PairedData$beta.D.pod.qJ==0),])/nrow(PairedData[which(PairedData$RegionGroup2=="Africa"),])

PairedData$d.time_julian<-PairedData$d.time
PairedData$d.time<-PairedData$TimeAngleDiff

range(PairedData$d.time)
range(PairedData$TimeAngleDiff)

# for(i in 1:length(Counttables)){
#   
#   temp<-paste0("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts/Data/Counttables/Site_",i,".csv")
#   
#   write.csv(Counttables[[i]],temp)
# }

#raw<-read.csv("Data/CleanData_12Oct20.csv")

load("S0.Figure_2023_March.Rdata")

rm("raw");save.image("S0.Figure_2023_March.Rdata")

# SiteInfo3<-read.csv("//storage.slu.se/Home$/mwse0001/Desktop/SLU/0.LIFEPLAN/4.Manuscripts/0.11_DataForScripts_Desposit/MetaDataSite.csv")
# dat<-read.csv("//storage.slu.se/Home$/mwse0001/Desktop/SLU/0.LIFEPLAN/4.Manuscripts/0.11_DataForScripts_Desposit/PairwiseData.csv")

##################################### Figure 1 ################################################################################

SiteInfo[which(SiteInfo$Region=="Central America"),"Region"]<-"North America"


Cboard<-data.frame(Region=c("North America", "Oceania","Eurasia","South America","Africa"),colour=NA)
Cboard$colour<-viridis(length(unique(SiteInfo$Region)),option="H")

world <- ne_countries(scale = "medium", returnclass = "sf")
unique(world$name)
unique(world$sovereignt)
unique(world$continent)
world <- world[which(world$continent!="Antarctica"),]

#because french guiana is in south america, not europe
#france<-ne_countries(geounit='france',scale = "medium", returnclass = "sf",type = "map_units")
franceg<-ne_countries(geounit='french guiana',scale = "medium", returnclass = "sf",type = "map_units")

Region_NorthAmerica<-world[which(world$subregion%in%c("Central America","Caribbean","Northern America")),]
Region_SouthAmerica<-world[which(world$subregion%in%c("South America")),]
Region_Eurasia<-world[which(world$subregion%in%c("Southern Asia","Southern Europe","Northern Europe","Western Asia","Western Europe",
                                                 "Eastern Europe","South-Eastern Asia","Eastern Asia","Central Asia")),]
Region_Africa<-world[which(world$subregion%in%c("Middle Africa","Western Africa","Southern Africa","Northern Africa","Eastern Africa")),]
Region_Oceania<-world[which(world$subregion%in%c("Polynesia","Australia and New Zealand","Melanesia","Micronesia")),]

# ggplot() +geom_sf(data=world,fill="white")+geom_sf(data=world[which(world$name=="France"),],fill="red")+theme_void()
# g1<-ggplot() +geom_sf(data=world,fill="white")+theme_void()

# DS<-unique(c(unique(dat$Site1),unique(dat$Site2)))

#needs to be a tibble to use sf (as tibble then)
ggsave(filename="Figures/Figure1.png",
       ggplot() +geom_sf(data=world,colour="black")+theme_void()+
         geom_sf(data=Region_NorthAmerica,fill=Cboard[which(Cboard$Region=="North America"),"colour"],color="black")+
         geom_sf(data=Region_SouthAmerica,fill=Cboard[which(Cboard$Region=="South America"),"colour"],color="black")+
         geom_sf(data=Region_Eurasia,fill=Cboard[which(Cboard$Region=="Eurasia"),"colour"],color="black")+
         geom_sf(data=Region_Africa,fill=Cboard[which(Cboard$Region=="Africa"),"colour"],color="black")+
         geom_sf(data=Region_Oceania,fill=Cboard[which(Cboard$Region=="Oceania"),"colour"],color="black")+
         geom_sf(data=franceg,fill=Cboard[which(Cboard$Region=="South America"),"colour"],color="black")+
         geom_point(data=tibble::as_tibble(SiteInfo),aes(x=Longitude,y=Latitude),shape=21,fill="white",size=4,stroke=1.5)+
         annotate(geom="text", x=-120, y=-40, label="Sampling sites = 129",color="Black",size=4)+
         annotate(geom="text", x=-120, y=-45, label="Trapping events = 2412",color="Black",size=4),
       width=30,height=25,dpi=300,units="cm")

#####################################################################################################################################################

################################################## gradient figures - setup ##########################################################################################

#PairedData$d.space<-geosphere:::distHaversine(PairedData[,c("Lon1","Lat1")],PairedData[,c("Lon2","Lat2")])/1000 
#PairedData$d.lat<-(abs(PairedData$Lat1)+abs(PairedData$Lat2))/2
#PairedData$d.time<-as.numeric(abs(fastPOSIXct(PairedData$Date1)-fastPOSIXct(PairedData$Date2))/(3600*24))

PairedData[which(PairedData$Reg1=="Central America"&PairedData$Reg2=="North America"),"RegionGroup"]<-"North America"
PairedData[which(PairedData$Reg2=="Central America"&PairedData$Reg1=="North America"),"RegionGroup"]<-"North America"
PairedData[which(PairedData$Reg1=="Central America"&PairedData$Reg2=="Central America"),"RegionGroup"]<-"North America"
PairedData[which(PairedData$Reg2=="Central America"&PairedData$Reg1=="Central America"),"RegionGroup"]<-"North America"
PairedData[which(PairedData$Reg1=="Central America"),"Reg1"]<-"North America"
PairedData[which(PairedData$Reg2=="Central America"),"Reg2"]<-"North America"

table(PairedData$RegionGroup)

PairedData[which(PairedData$RegionGroup=="Central America"),]

unique(PairedData$RegionGroup)

PairedData$RegionGroup2<-PairedData$RegionGroup
PairedData[which(PairedData$RegionGroup=="InterRegion"),"RegionGroup2"]<-"Global"

length(PairedData[which(PairedData$beta.D.pod.qJ==1),1])/nrow(PairedData)

#### plain beta-diversity figures

# pdf
# 
# ggplot(data=PairedData,aes(x=d.lat,y=beta.sim2,group=Reg2,fill=Reg2))+
#   geom_point(shape=21)+
#   facet_wrap(~Reg2)+
#   scale_fill_manual(breaks = Cboard$Region,values=Cboard$colour)+
#   labs(fill='Region',x="Latitude (absolute)",y="Beta-diversity")+
#   theme_bw()
#   
length(which(PairedData$RegionGroup2=="Global"&PairedData$beta.D.pod.qJ==1))/length(which(PairedData$RegionGroup2=="Global"))
length(which(PairedData$RegionGroup2=="North America"&PairedData$beta.D.pod.qJ==1))/length(which(PairedData$RegionGroup2=="North America"))
length(which(PairedData$RegionGroup2=="Oceania"&PairedData$beta.D.pod.qJ==1))/length(which(PairedData$RegionGroup2=="Oceania"))
length(which(PairedData$RegionGroup2=="Eurasia"&PairedData$beta.D.pod.qJ==1))/length(which(PairedData$RegionGroup2=="Eurasia"))
length(which(PairedData$RegionGroup2=="Africa"&PairedData$beta.D.pod.qJ==1))/length(which(PairedData$RegionGroup2=="Africa"))
length(which(PairedData$RegionGroup2=="South America"&PairedData$beta.D.pod.qJ==1))/length(which(PairedData$RegionGroup2=="South America"))


#checking transformation

# PairedData$beta.sim2<-log(1-(PairedData$beta.sim*b1))
# PairedData$beta.sne2<-log(1-((1-PairedData$beta.sne))*b1)
# PairedData$beta.sor2<-log(1-(PairedData$beta.sor*b1))

# all(floor(PairedData$beta.rich.pod.qJ)==floor(1-((1-exp(PairedData$beta.sne2))/0.999)))
# all(floor(PairedData$beta.repl.pod.qJ)==floor((1-exp(PairedData$beta.sor2))/0.999))

#dat$distlog<-log(dat$dist+1)

RegionDat<-data.frame(Region=c("InterRegion","North America", "South America", "Eurasia", "Africa", "Oceania"))

for(i in RegionDat$Region){
  
  RegionDat[which(RegionDat$Region==i),"d.space1"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.space"])[1],2)
  RegionDat[which(RegionDat$Region==i),"d.space2"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.space"])[2],2)
  
  RegionDat[which(RegionDat$Region==i),"d.lat1"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.lat"])[1],1)
  RegionDat[which(RegionDat$Region==i),"d.lat2"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.lat"])[2],2)
  
  RegionDat[which(RegionDat$Region==i),"d.time1"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.time"])[1],0)
  RegionDat[which(RegionDat$Region==i),"d.time2"]<-round(range(PairedData[which(PairedData$RegionGroup==i),"d.time"])[2],0)
  
  
  # RegionDat[which(RegionDat$Region==i),"d.time1"]<-0
  # RegionDat[which(RegionDat$Region==i),"d.time2"]<-365/2
  
  # if(round(range(PairedData[which(PairedData$RegionGroup==i),"d.time"])[2]-0.5,0)>600){
  #   RegionDat[which(RegionDat$Region==i),"d.time2"]<-600
  # }
}

 # RegionDat[which(RegionDat$Region=="Africa"),"d.time2"]<-1100
 # RegionDat[which(RegionDat$Region=="South America"),"d.time2"]<-600

for(i in 1:nrow(RegionDat)){

RegionDat$d.spaceb[i]<-diff(seq(from=RegionDat$d.space1[i],to=RegionDat$d.space2[2],length.out=25)[c(1,2)])
RegionDat$d.timeb[i]<-diff(seq(from=RegionDat$d.time1[i],to=RegionDat$d.time2[2],length.out=25)[c(1,2)])
RegionDat$d.latb[i]<-diff(seq(from=RegionDat$d.lat1[i],to=RegionDat$d.lat2[2],length.out=25)[c(1,2)])
}
# 
# RegionDat$d.spaceb<-(RegionDat$d.space2-RegionDat$d.space1)/25
# RegionDat$d.timeb<-(RegionDat$d.time2-RegionDat$d.time1)/25
# RegionDat$d.latb<-(RegionDat$d.lat2-RegionDat$d.lat1)/25

RegionDat[which(RegionDat$Region=="InterRegion"),"Region"]<-"Global"

newdat_dist<-list()

Repl_LTS<-c("Global");Repl_LS<-c();Repl_LT<-c("North America", "Eurasia");Repl_A<-c("South America","Oceania","Africa")
Rich_LTS<-c("Global");Rich_LS<-c("North America");Rich_LT<-c();Rich_A<-c("Eurasia","South America","Oceania","Africa")

for(i in 1:length(RegionDat[,1])){
  # temp<-expand.grid(d.space=round(seq(RegionDat$d.space1[i],RegionDat$d.space2[i],by=RegionDat$d.spaceb[i]/2),0),
  #                        d.lat=round(seq(RegionDat$d.lat1[i],RegionDat$d.lat2[i],by=RegionDat$d.latb[i]/2),3))
  
  temp<-expand.grid(d.space=seq(from=RegionDat$d.space1[i],to=RegionDat$d.space2[i],length.out=100),
                     d.lat=seq(from=RegionDat$d.lat1[i],to=RegionDat$d.lat2[i],length.out=100),3)
  
  temp$d.time<-0
  
  temp2<-PairedData[which(PairedData$RegionGroup2==RegionDat$Region[i]),]
  temp2$site.weights<-1/(temp2$Num1*temp2$Num2)
  
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
  
  # if(RegionDat$Region[i]%in%c("Global","North America","Central America","Africa")){
  #   mod1<-lm(data=temp2,beta.sor2~d.space*d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c("Eurasia","South America","Oceania")){
  #   mod1<-lm(data=temp2,beta.sor2~d.space+d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c()){
  #   mod1<-lm(data=temp2,beta.sor2~1,weights = site.weights)
  # }
  
  # if(RegionDat$Region[i]%in%c("Global","Central America","Africa")){
  #   mod2<-lm(data=temp2,beta.sne2~d.space*d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c("Eurasia","North America","South America","Oceania")){
  #   mod2<-lm(data=temp2,beta.sne2~d.space+d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c()){
  #   mod2<-lm(data=temp2,beta.sne2~d.space,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c()){
  #   mod2<-lm(data=temp2,beta.sne2~1,weights = site.weights)
  # }
  
  tT<-predict(mod1,newdata=temp,type="response")
  tN<-predict(mod2,newdata=temp,type="response")
  
  temp$Turnover<-tT
  temp$Nestedness<-tN
  
  temp$Turnover_t<-tT
  temp$Nestedness_t<-tN
  
  # temp$Turnover<-predict(mod1,newdata=temp,type="response")
  # temp$Nestedness<-predict(mod2,newdata=temp,type="response")
  
  # PairedData$beta.sor2_t<-log(1-(PairedData$beta.sor*b1))
  # PairedData$beta.sne2_t<-log(1-((1-PairedData$beta.sne))*b1)

  # temp$Turnover_t<-round((1-exp(predict(mod1,newdata=temp,type="response")))/0.999,4)
  # temp$Nestedness_t<-round(1-((1-exp(predict(mod2,newdata=temp,type="response")))/0.999),4)
  newdat_dist[[i]]<-temp
  names(newdat_dist)[i]<-RegionDat$Region[i]
}

newdat_dist[[1]]
range(temp2$d.lat)

#interact_plot(mod1,pred=d.space,modx=d.lat,plot.points=TRUE,modx.values=c(18,40,81),partial.residuals = TRUE,)

# library(sjPlot)
# library(sjlabelled)
# library(sjmisc)

#library(arm)

PairedData$d.lat_scaled<-scale(PairedData$d.lat)
PairedData$d.time_scaled<-scale(PairedData$d.time)
PairedData$d.space_scaled<-scale(PairedData$d.space)


mod1.1<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[1]),],beta.D.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[1]),"Num1"]* PairedData[which(PairedData$RegionGroup2==RegionDat$Region[1]),"Num2"]))
mod1.2<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[2]),],beta.D.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[2]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[2]),"Num2"]))
mod1.3<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[3]),],beta.D.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[3]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[3]),"Num2"]))
mod1.4<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[4]),],beta.D.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[4]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[4]),"Num2"]))
mod1.5<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[5]),],beta.D.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[5]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[5]),"Num2"]))
mod1.6<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[6]),],beta.D.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[6]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[6]),"Num2"]))

t1<-data.frame(summary(mod1.1)$coefficients[,1:2]);t1$vars=rownames(t1);t1$model=RegionDat$Region[1];t1$response="Total beta-diveristy"
t2<-data.frame(summary(mod1.2)$coefficients[,1:2]);t2$vars=rownames(t2);t2$model=RegionDat$Region[2];t2$response="Total beta-diveristy"
t3<-data.frame(summary(mod1.3)$coefficients[,1:2]);t3$vars=rownames(t3);t3$model=RegionDat$Region[3];t3$response="Total beta-diveristy"
t4<-data.frame(summary(mod1.4)$coefficients[,1:2]);t4$vars=rownames(t4);t4$model=RegionDat$Region[4];t4$response="Total beta-diveristy"
t5<-data.frame(summary(mod1.5)$coefficients[,1:2]);t5$vars=rownames(t5);t5$model=RegionDat$Region[5];t5$response="Total beta-diveristy"
t6<-data.frame(summary(mod1.6)$coefficients[,1:2]);t6$vars=rownames(t6);t6$model=RegionDat$Region[6];t6$response="Total beta-diveristy"

test<-rbind(t1,t2,t3,t4,t5,t6)

mod1.1<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[1]),],beta.repl.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[1]),"Num1"]* PairedData[which(PairedData$RegionGroup2==RegionDat$Region[1]),"Num2"]))
mod1.2<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[2]),],beta.D.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[2]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[2]),"Num2"]))
mod1.3<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[3]),],beta.repl.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[3]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[3]),"Num2"]))
mod1.4<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[4]),],beta.repl.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[4]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[4]),"Num2"]))
mod1.5<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[5]),],beta.repl.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[5]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[5]),"Num2"]))
mod1.6<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[6]),],beta.repl.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[6]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[6]),"Num2"]))

t1<-data.frame(summary(mod1.1)$coefficients[,1:2]);t1$vars=rownames(t1);t1$model=RegionDat$Region[1];t1$response="Species replacement"
t2<-data.frame(summary(mod1.2)$coefficients[,1:2]);t2$vars=rownames(t2);t2$model=RegionDat$Region[2];t2$response="Species replacement"
t3<-data.frame(summary(mod1.3)$coefficients[,1:2]);t3$vars=rownames(t3);t3$model=RegionDat$Region[3];t3$response="Species replacement"
t4<-data.frame(summary(mod1.4)$coefficients[,1:2]);t4$vars=rownames(t4);t4$model=RegionDat$Region[4];t4$response="Species replacement"
t5<-data.frame(summary(mod1.5)$coefficients[,1:2]);t5$vars=rownames(t5);t5$model=RegionDat$Region[5];t5$response="Species replacement"
t6<-data.frame(summary(mod1.6)$coefficients[,1:2]);t6$vars=rownames(t6);t6$model=RegionDat$Region[6];t6$response="Species replacement"


test<-rbind(test,t1,t2,t3,t4,t5,t6)

mod1.1<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[1]),],beta.rich.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[1]),"Num1"]* PairedData[which(PairedData$RegionGroup2==RegionDat$Region[1]),"Num2"]))
mod1.2<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[2]),],beta.rich.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[2]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[2]),"Num2"]))
mod1.3<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[3]),],beta.rich.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[3]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[3]),"Num2"]))
mod1.4<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[4]),],beta.rich.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[4]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[4]),"Num2"]))
mod1.5<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[5]),],beta.rich.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[5]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[5]),"Num2"]))
mod1.6<-lm(data=PairedData[which(PairedData$RegionGroup2==RegionDat$Region[6]),],beta.rich.pod.qJ~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat),
           weights = 1/(PairedData[which(PairedData$RegionGroup2==RegionDat$Region[6]),"Num1"]*PairedData[which(PairedData$RegionGroup2==RegionDat$Region[6]),"Num2"]))

t1<-data.frame(summary(mod1.1)$coefficients[,1:2]);t1$vars=rownames(t1);t1$model=RegionDat$Region[1];t1$response="Richness difference"
t2<-data.frame(summary(mod1.2)$coefficients[,1:2]);t2$vars=rownames(t2);t2$model=RegionDat$Region[2];t2$response="Richness difference"
t3<-data.frame(summary(mod1.3)$coefficients[,1:2]);t3$vars=rownames(t3);t3$model=RegionDat$Region[3];t3$response="Richness difference"
t4<-data.frame(summary(mod1.4)$coefficients[,1:2]);t4$vars=rownames(t4);t4$model=RegionDat$Region[4];t4$response="Richness difference"
t5<-data.frame(summary(mod1.5)$coefficients[,1:2]);t5$vars=rownames(t5);t5$model=RegionDat$Region[5];t5$response="Richness difference"
t6<-data.frame(summary(mod1.6)$coefficients[,1:2]);t6$vars=rownames(t6);t6$model=RegionDat$Region[6];t6$response="Richness difference"


test<-rbind(test,t1,t2,t3,t4,t5,t6)

colnames(test)[2]<-"SD"

test<-test[which(test$vars!="(Intercept)"),]

test$vars<-as.factor(test$vars)
levels(test$vars)<-c("d.lat","d.lat x d.time", "d.space", "d.lat x d.space", "d.time")
test$vars<- factor(test$vars, levels=c("d.lat x d.space","d.lat x d.time","d.space","d.time","d.lat"))
test$response<- factor(test$response, levels=c("Total beta-diveristy","Species replacement","Richness difference"))
test$model<- factor(test$model, levels=c("Global","North America","South America","Eurasia","Africa","Oceania"))


ggplot(test,aes(vars,Estimate))+
  geom_hline(yintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_errorbar(data=test,aes(ymin=Estimate - SD, ymax=Estimate + SD,colour=vars,lwd=1, width=0))+
  geom_point(size=3, aes(colour=vars)) +
  facet_grid(response ~ model,scales="free") +
  coord_flip() +
  labs(x="Standardized coefficient", y="Value") +
  theme_bw()+
  guides(color=FALSE,linewidth=FALSE)

  
ggsave(filename="Figures/StandardizedEstimates_S3.png",width=20,height=20,units="cm",dpi=300)




?coefplot
coefplot(mod1)

newdat_time<-list()

for(i in 1:length(RegionDat[,1])){
  # temp<-expand.grid(d.time=round(seq(RegionDat$d.time1[i],RegionDat$d.time2[i],by=RegionDat$d.timeb[i]/2),0),
  #                   d.lat=round(seq(RegionDat$d.lat1[i],RegionDat$d.lat2[i],by=RegionDat$d.latb[i]/2),3))
  
  temp<-expand.grid(d.time=seq(from=RegionDat$d.time1[i],to=RegionDat$d.time2[i],length.out=200),
                    d.lat=seq(from=RegionDat$d.lat1[i],to=RegionDat$d.lat2[i],length.out=200))
  
  temp$d.space<-0

  temp2<-PairedData[which(PairedData$RegionGroup2==RegionDat$Region[i]),]
  temp2$site.weights<-1/(temp2$Num1*temp2$Num2)
  
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
  
  
  # 
  # 
  # if(RegionDat$Region[i]%in%c("")){
  #   mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space*d.lat+d.time*d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c("North America","Oceania")){
  #   mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space*d.lat+d.time,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c("")){
  #   mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space+d.time*d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c("Eurasia","Africa","South America","Global")){
  #   mod1<-lm(data=temp2,beta.repl.pod.qJ~d.space+d.time+d.lat,weights = site.weights)
  # }
  # 
  # if(RegionDat$Region[i]%in%c("Global","Africa")){
  #   mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space*d.lat+d.time*d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c("")){
  #   mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space*d.lat+d.time,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c("South America")){
  #   mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space+d.time*d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c("Eurasia","Oceania","North America")){
  #   mod2<-lm(data=temp2,beta.rich.pod.qJ~d.space+d.time+d.lat,weights = site.weights)
  # }
  # 
  # if(RegionDat$Region[i]%in%c("South America","Global","Central America","Africa")){
  #   mod1<-lm(data=temp2,beta.sor2~d.time*d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c("Oceania","North America","Eurasia")){
  #   mod1<-lm(data=temp2,beta.sor2~d.time+d.lat,weights = site.weights)
  # }
  # 
  # if(RegionDat$Region[i]%in%c("Global","Central America")){
  #   mod2<-lm(data=temp2,beta.sne2~d.time*d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c("Eurasia","Africa","North America","South America","Oceania")){
  #   mod2<-lm(data=temp2,beta.sne2~d.time+d.lat,weights = site.weights)
  # }else if(RegionDat$Region[i]%in%c()){
  #   mod2<-lm(data=temp2,beta.sne2~1,weights = site.weights)
  # }
  
  #temp2[temp2$RegionGroup]
  
  tT<-predict(mod1,newdata=temp,type="response")
  tN<-predict(mod2,newdata=temp,type="response")
  
  temp$Turnover<-tT
  temp$Nestedness<-tN
  
  # tT2<-round((1-exp(tT))/0.999,4)
  # tN2<-round(1-((1-exp(tN))/0.999),4)
  
  # tT2[which(tT2>1)]<-1
  # tN2[which(tN2<0)]<-0
  
  temp$Turnover_t<-tT
  temp$Nestedness_t<-tN
  
  # temp$Turnover_t<-round((1-exp(predict(mod1,newdata=temp,type="response")))/0.999,4)
  # temp$Nestedness_t<-round(1-((1-exp(predict(mod2,newdata=temp,type="response")))/0.999),4)
  
  newdat_time[[i]]<-temp
  names(newdat_time)[i]<-RegionDat$Region[i]
}


range(newdat_time[[4]][which(newdat_time[[4]]$Turnover_t<0.1),"d.time"])
range(newdat_time[[4]][,"d.time"])

range(newdat_time[[6]][which(newdat_time[[6]]$Turnover_t<0.1),"d.time"])
range(newdat_time[[6]][,"d.time"])

#check they are all the same length

for(i in names(newdat_dist)){
  print(length(newdat_dist[[i]][,1]))
  print(round(range(newdat_dist[[i]]$Turnover_t),2))
  print(round(range(newdat_dist[[i]]$Nestedness_t),2))
}

for(i in names(newdat_time)){
  print(length(newdat_time[[i]][,1]))
  print(round(range(newdat_time[[i]]$Turnover_t),2))
  print(round(range(newdat_time[[i]]$Nestedness_t),2))
}


### something is off with africa and south america - nope these are find ... it's the scale projection

newdat_dist$`South America`[which(newdat_dist$`South America`$d.time==0&newdat_dist$`South America`$d.space==0),"Turnover_t"]
newdat_time$`South America`[which(newdat_time$`South America`$d.space==0&newdat_time$`South America`$d.time==0),"Turnover_t"]

newdat_dist$`Africa`[which(newdat_dist$`Africa`$d.time==0&newdat_dist$`Africa`$d.space==0),"Turnover_t"]
newdat_time$`Africa`[which(newdat_time$`Africa`$d.space==0&newdat_time$`Africa`$d.time==0),"Turnover_t"]

newdat_dist$`Global`[which(newdat_dist$`Africa`$d.time==0&newdat_dist$`Africa`$d.space==0),"Turnover_t"]
newdat_time$`Global`[which(newdat_time$`Africa`$d.space==0&newdat_time$`Africa`$d.time==0),"Turnover_t"]

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


################################################# beta - diversity figure for each of the key trends 

range(round(PairedData$d.lat,0))
range(round(PairedData$d.space,digits = -3))
range(round(PairedData$d.time,-1))

s1<-16


BetaMeta<-data.frame(lat=rep(seq(from=range(PairedData$d.lat)[1],to=range(PairedData$d.lat)[2],length.out=s1)[2:s1],times=6),
           space=rep(seq(from=range(PairedData$d.space)[1],to=range(PairedData$d.space)[2],length.out=s1)[2:s1],times=6),
           time=rep(seq(from=range(PairedData$d.time)[1],to=range(PairedData$d.time)[2],length.out=s1)[2:s1],times=6),
           Region=rep(unique(PairedData$RegionGroup2),each=s1-1),
           beta_lat=NA,beta_space=NA,beta_time=NA,beta_latse=NA,beta_spacese=NA,beta_timese=NA
           )
           

  for(i in 1:nrow(BetaMeta)){
    if(i==1){
      
      t1<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.lat<=BetaMeta$lat[i]),"beta.D.pod.qJ"]
      if(length(t1)>0){
        BetaMeta$beta_lat[i]<-mean(t1)
        BetaMeta$beta_latse[i]<-sd(t1)/sqrt(length(t1)) }
      
      t2<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.space<=BetaMeta$space[i]),"beta.D.pod.qJ"]
      if(length(t2)>0){
        BetaMeta$beta_space[i]<-mean(t2)
        BetaMeta$beta_spacese[i]<-sd(t2)/sqrt(length(t2))}
      
      t3<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.ld.timeat<=BetaMeta$time[i]),"beta.D.pod.qJ"]
      if(length(t3)>0){
        BetaMeta$beta_time[i]<-mean(t3)
        BetaMeta$beta_timese[i]<-sd(t3)/sqrt(length(t3))}  

    }else if(BetaMeta$Region[i]!=BetaMeta$Region[i-1]){
      
      t1<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.lat<=BetaMeta$lat[i]),"beta.D.pod.qJ"]
      if(length(t1)>0){
        BetaMeta$beta_lat[i]<-mean(t1)
        BetaMeta$beta_latse[i]<-sd(t1)/sqrt(length(t1))}
      
      t2<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.space<=BetaMeta$space[i]),"beta.D.pod.qJ"]
      if(length(t2)>0){
        BetaMeta$beta_space[i]<-mean(t2)
        BetaMeta$beta_spacese[i]<-sd(t2)/sqrt(length(t2))}
           
      t3<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.ld.timeat<=BetaMeta$time[i]),"beta.D.pod.qJ"]
      if(length(t3)>0){
        BetaMeta$beta_time[i]<-mean(t3)
        BetaMeta$beta_timese[i]<-sd(t3)/sqrt(length(t3))}
       
    }else if(BetaMeta$Region[i]==BetaMeta$Region[i-1]){
      
      t1<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.lat<=BetaMeta$lat[i]&PairedData$d.lat>BetaMeta$lat[i-1]),"beta.D.pod.qJ"]
      if(length(t1)>0){
        BetaMeta$beta_lat[i]<-mean(t1)
        BetaMeta$beta_latse[i]<-sd(t1)/sqrt(length(t1))}
      
      t2<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.space<=BetaMeta$space[i]&PairedData$d.space>BetaMeta$space[i-1]),"beta.D.pod.qJ"]
      if(length(t2)>0){
        BetaMeta$beta_space[i]<-mean(t2)
        BetaMeta$beta_spacese[i]<-sd(t2)/sqrt(length(t2))}
      
      t3<-PairedData[which(PairedData$RegionGroup2==BetaMeta$Region[i]&PairedData$d.time<=BetaMeta$time[i]&PairedData$d.time>BetaMeta$time[i-1]),"beta.D.pod.qJ"]
      if(length(t3)>0){
        BetaMeta$beta_time[i]<-mean(t3)
        BetaMeta$beta_timese[i]<-sd(t3)/sqrt(length(t3))}
      }
    
  }

BetaMeta$Region<-factor(BetaMeta$Region,levels=c("Global","North America","South America","Eurasia","Africa","Oceania"))


P1<-ggplot(data=BetaMeta[!is.na(BetaMeta$beta_lat),],aes(x=lat,y=beta_lat))+
  geom_point()+
  geom_errorbar(data=BetaMeta[!is.na(BetaMeta$beta_lat),],aes(ymin=beta_lat-beta_latse,ymax=beta_lat+beta_latse))+
  facet_wrap(~Region,nrow=1,scales="free_y")+
  geom_smooth(method = lm, se = FALSE)+
  theme_bw()+
  labs(x="absolute latitude",y="Beta-diversity (Jaccard similarity)")+
  scale_y_continuous(labels=function(x) sprintf("%.4f", x))


P2<-ggplot(data=BetaMeta[!is.na(BetaMeta$beta_time),],aes(x=time,y=beta_time))+
  geom_point()+
  geom_errorbar(data=BetaMeta[!is.na(BetaMeta$beta_time),],aes(ymin=beta_time-beta_timese,ymax=beta_time+beta_timese))+
  facet_wrap(~Region,nrow=1,scales="free_y")+
 geom_smooth(method = lm, se = FALSE)+
  theme_bw()+
  labs(x="time (days)",y="Beta-diversity (Jaccard similarity)")+
  scale_y_continuous(labels=function(x) sprintf("%.4f", x))

P3<-ggplot(data=BetaMeta[!is.na(BetaMeta$beta_space),],aes(x=space,y=beta_space))+
  geom_point()+
  geom_errorbar(data=BetaMeta[!is.na(BetaMeta$beta_space),],aes(ymin=beta_space-beta_spacese,ymax=beta_space+beta_spacese))+
  facet_wrap(~Region,nrow=1,scales="free_y")+
  geom_smooth(method = lm, se = FALSE)+
  theme_bw()+
  scale_x_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(1000,7000,13000,19000))+
  labs(x="distance (km)",y="Beta-diversity (Jaccard similarity)")+
  scale_y_continuous(labels=function(x) sprintf("%.4f", x))+
  theme(axis.text.x=element_text(angle=-35,vjust=0))

ggsave(filename = "Figures/beta_trends.png",plot=grid.arrange(P1,P2,P3,nrow=3),height=20,width=20,units="cm",dpi=300)



################################################### trying to create a heatmap version of figure 3 ####################

#transform (original)
# b1<-0.999
# 
# PairedData$beta.sim2_t<-log(1-(PairedData$beta.sim*b1))
# PairedData$beta.sne2_t<-log(1-((1-PairedData$beta.sne)*b1))
# PairedData$beta.sor2_t<-log(1-(PairedData$beta.sor*b1))
# 
# #(1-exp(x))/b1
# 
# #transform(otso's updated request v5) - just for note - just changes plus to minus for their comfort
# 
# PairedData$beta.sim2_t<- -log(1-(PairedData$beta.sim*b1))
# PairedData$beta.sne2_t<-log(1-((1-PairedData$beta.sne))*b1)
# PairedData$beta.sor2_t<- -log(1-(PairedData$beta.sor*b1))

#Otso request to back transform to beta - so take the calculated data and backtransform the beta valuse (using the original transformation) 

#first try heat maps with color 
#y is beta
#x is lat
#z is space or time


# 1-((1-exp(PairedData$beta.sne2_t))/0.999)

#1-exp(log(3))/0.99

#space

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

#trying to match up the scale for the two figures

for(i in 1:nrow(RegionDat)){
  RegionDat$UpperTurn[i]<-max(c(newdat_dist[[which(names(newdat_dist)==RegionDat$Region[i])]][,"Turnover_t"],
                                newdat_time[[which(names(newdat_time)==RegionDat$Region[i])]][,"Turnover_t"]))
  RegionDat$LowerTurn[i]<-min(c(newdat_dist[[which(names(newdat_dist)==RegionDat$Region[i])]][,"Turnover_t"],
                                newdat_time[[which(names(newdat_time)==RegionDat$Region[i])]][,"Turnover_t"]))
  
  RegionDat$UpperNest[i]<-max(c(newdat_dist[[which(names(newdat_dist)==RegionDat$Region[i])]][,"Nestedness_t"],
                                newdat_time[[which(names(newdat_time)==RegionDat$Region[i])]][,"Nestedness_t"]))
  RegionDat$LowerNest[i]<-min(c(newdat_dist[[which(names(newdat_dist)==RegionDat$Region[i])]][,"Nestedness_t"],
                                newdat_time[[which(names(newdat_time)==RegionDat$Region[i])]][,"Nestedness_t"]))
}


L1<-min(RegionDat$LowerTurn-0.01);L2<-max(RegionDat$UpperTurn+0.01)
L3<-min(RegionDat$LowerNest-0.01);L4<-max(RegionDat$UpperNest+0.01)

P1<-ggplot(data=newdat_dist[[1]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_dist[1]),"*"))+  labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P2<-ggplot(data=newdat_dist[[2]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_dist[2]),""))+labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P3<-ggplot(data=newdat_dist[[3]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_dist[3])))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P4<-ggplot(data=newdat_dist[[4]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(names(newdat_dist[4]))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P5<-ggplot(data=newdat_dist[[5]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_dist[5]),""))+  labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P6<-ggplot(data=newdat_dist[[6]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_dist[6]),""))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))


P8<-ggplot(data=newdat_dist[[1]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_dist[1]),"*"))+  labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P9<-ggplot(data=newdat_dist[[2]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_dist[2]),""))+  labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P10<-ggplot(data=newdat_dist[[3]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_dist[3])))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P11<-ggplot(data=newdat_dist[[4]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(names(newdat_dist[4]))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P12<-ggplot(data=newdat_dist[[5]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"dist"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
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

ggsave(filename="Figures/DistanceHeatmap_Figure3v2_v2.png",(grid.arrange(grobs = temp, layout_matrix = lay)),width=30,height=30,units="cm",dpi=300)
 
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
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[1]),"*"))+  labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P2<-ggplot(data=newdat_time[[2]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[2]),"*"))+labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P3<-ggplot(data=newdat_time[[3]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[3]),""))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P4<-ggplot(data=newdat_time[[4]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[4]),"*"))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P5<-ggplot(data=newdat_time[[5]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[5]),""))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P6<-ggplot(data=newdat_time[[6]],aes(y=d.lat,x=d.time,fill=Turnover_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L1,L2))+
  ggtitle(paste0(names(newdat_time[6])))+ labs(fill='Species\nReplacement',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())



P8<-ggplot(data=newdat_time[[1]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_time[1]),"*"))+  labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P9<-ggplot(data=newdat_time[[2]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(names(newdat_time[2]))+labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P10<-ggplot(data=newdat_time[[3]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(paste0(names(newdat_time[3])))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P11<-ggplot(data=newdat_time[[4]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(names(newdat_time[4]))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P12<-ggplot(data=newdat_time[[5]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(names(newdat_time[5]))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())


P13<-ggplot(data=newdat_time[[6]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"time"]))+
  #geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+
  geom_tile()+
  scale_fill_viridis(label = function(x) sprintf("%.1f", x),direction=-1,option="H",limits=c(L3,L4))+
  ggtitle(names(newdat_time[6]))+ labs(fill='Richness\nDifference',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())



temp<-list(P1,P2,P3,P4,P5,P6,P8,P9,P10,P11,P12,P13)

lay<-rbind(c(1,2,3),
           c(4,5,6),
           c(7,9,10),
           c(11,12,13))

ggsave(filename="Figures/TimeHeatmap_Figure4v2_v2.png",(grid.arrange(grobs = temp, layout_matrix = lay)),width=30,height=30,units="cm",dpi=300)




############################### illustrative figures to show 




#################################latitude vs temporal sampling period, order diversity plot 

















#distplots<-list()


# 
# for(i in 1:length(newdat_dist)){
#   
#   newdat_dist[[i]]$w<-(max(newdat_dist[[i]]$d.space)-min(newdat_dist[[i]]$d.space))*0.1
#   newdat_dist[[i]]$h<-(max(newdat_dist[[i]]$d.lat)-min(newdat_dist[[i]]$d.lat))*0.1
#   
#   distplots[[i]]<-ggplot(data=newdat_dist[[i]],aes(y=d.lat,x=d.space,fill=Turnover_t))+
#     geom_tile(aes(width=unique(newdat_dist[[i]]$w),height=unique(newdat_dist[[i]]$h)))+
#     scale_fill_viridis(label = function(x) sprintf("%.3f", x))+
#     #xlim(min(newdat_dist[[i]]$d.space), max(newdat_dist[[i]]$d.space))+ylim(min(newdat_dist[[i]]$d.lat), max(newdat_dist[[i]]$d.lat))+
#     ggtitle(names(newdat_dist[i]))+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())+
#     labs(fill='Turnover',y="Latitude (absolute)",x="Distance (km)") 
#   
#   names(distplots)[i]<-names(newdat_dist)[i]
# }
# 
# for(i in 1:length(newdat_dist)){
#   distplots[[i+7]]<-ggplot(data=newdat_dist[[i]],aes(y=d.lat,x=d.space,fill=Nestedness_t))+
#     geom_tile(aes(width=unique(newdat_dist[[i]]$w),height=unique(newdat_dist[[i]]$h)))+
#     scale_fill_viridis(label = function(x) sprintf("%.3f", x))+
#     #xlim(min(newdat_dist[[i]]$d.space), max(newdat_dist[[i]]$d.space))+ylim(min(newdat_dist[[i]]$d.lat), max(newdat_dist[[i]]$d.lat))+
#     ggtitle(names(newdat_dist[i]))+
#     theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())+
#     labs(fill='Nestedness',y="Latitude (absolute)",x="Distance (km)") 
#   names(distplots)[i+7]<-paste0(names(newdat_dist)[i],"_nest")
# }

#legend1 <- cowplot::get_legend(distplots[[1]])
#legend2 <- cowplot::get_legend(distplots[[8]])
# 
# temp<-newdat_dist[[i]]
# length(unique(temp$d.lat))
# 
# 
# lay<-rbind(c(1,1,2,2,3,3,4,4),
#       c(NA,5,5,6,6,7,7,NA),
#       c(8,8,9,9,10,10,11,11),
#       c(NA,12,12,13,13,14,14,NA))
# 
# lay<-rbind(c(1,2,3,4),
#            c(5,6,7,NA),
#            c(8,9,10,11),
#            c(12,13,14,NA))
# 

#grid.arrange(grobs = distplots, layout_matrix = lay)

#time

timeplots<-list()

for(i in 1:length(newdat_time)){
  timeplots[[i]]<-ggplot(data=newdat_time[[i]],aes(y=d.lat,x=d.time,fill=Turnover_t))+geom_tile()+scale_fill_viridis()+ggtitle(names(newdat_time[i]))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
  names(timeplots)[i]<-names(newdat_time)[i]
}

for(i in 1:length(newdat_time)){
  timeplots[[i+7]]<-ggplot(data=newdat_time[[i]],aes(y=d.lat,x=d.time,fill=Nestedness_t))+geom_tile()+scale_fill_viridis()+ggtitle(names(newdat_time[i]))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
  names(timeplots)[i+7]<-paste0(names(newdat_time)[i],"_nest")
}


legend1 <- cowplot::get_legend(timeplots[[1]])
legend2 <- cowplot::get_legend(timeplots[[8]])

lay<-rbind(c(1,1,2,2,3,3,4,4),
           c(NA,5,5,6,6,7,7,NA),
           c(8,8,9,9,10,10,11,11),
           c(NA,12,12,13,13,14,14,NA))


grid.arrange(grobs = timeplots, layout_matrix = lay)



#################################################### Spatial Figure 3 (older pre version 5)

porder<-data.frame(plot=c(1:30),
                   type=c("title","text","text","text","text","plot","plot","plot","plot",
                          "text","text","text","plot","plot","plot",
                          "title","text","text","text","text","plot","plot","plot","plot",
                          "text","text","text","plot","plot","plot"),
                   ID=c("Spatial Turnover",1,2,3,4,1,2,3,4,5,6,7,5,6,7,
                        "Spatial Nestedness",1,2,3,4,1,2,3,4,5,6,7,5,6,7),
                   ID2=c(NA,NA,NA,NA,NA,"Global","North America","Central America","South America",NA,NA,NA,"Eurasia","Africa","Oceania",
                         NA,NA,NA,NA,NA,"Global","North America","Central America","South America",NA,NA,NA,"Eurasia","Africa","Oceania"),
                   group=c(rep("Turnover",15),rep("Nestedness",15)))

m0<-matrix(c(1,1,1,1,1,1,1,1,
             2,2,3,3,4,4,5,5,
             6,6,7,7,8,8,9,9,
             0,10,10,11,11,12,12,0,
             0,13,13,14,14,15,15,0,
             16,16,16,16,16,16,16,16,
             17,17,18,18,19,19,20,20,
             21,21,22,22,23,23,24,24,
             0,25,25,26,26,27,27,0,
             0,28,28,29,29,30,30,0),10,byrow = TRUE)
height<-c(1,0.5,4,0.5,4,1,0.5,4,0.5,4)

width<-c(1,1,1,1,1,1,1,1)
layout3d(m0,heights=height,widths=width,sharedMouse = TRUE)
par3d(windowRect = c(0, 0, 750, 750))

par(mar=c(0.1,0.1,0.1,0.1))
par(mar=c(0.5,0.5,0.5,0.5))

xlab<-"Latitude (absolute)"
ylab<-"Distance (10K km)"

yt<-c(0,5000,10000,15000,20000)
ytl<-c("0","","10","","20")

xt<-c(0,20,40,60,80)
xtl<-c("0","","40","","80")

zt<-c(-7,-5,-3,-1,0)
ztl<-c("-7","-5","-3","-1","0")

for(i in 1:length(porder[,1])){
  if(porder$group[i]=="Turnover"){l1<- range(Lims1)[1];l2<- range(Lims1)[2]}else if(porder$group[i]=="Nestedness"){l1<- range(Lims2)[1];l2<- range(Lims2)[2]}
  if(porder$group[i]=="Turnover"){zlab="Turnover"}else if(porder$group[i]=="Nestedness"){zlab="Nestedness"}
  if(porder$group[i]=="Turnover"){tlab="Spatial Turnover"}else if(porder$group[i]=="Nestedness"){tlab="Spatial Nestedness"}
  if(porder$group[i]=="Turnover"){zt<-c(-12,-10,-8,-6,-4,-2);ztl<-c("","-10","","-6","","-2")}else if(porder$group[i]=="Nestedness"){zt<-c(-11,-9,-7,-5,-3);ztl<-c("","-9","","-5","")}
  
  if(porder$type[i]=="title"){
    if(i>1){next3d()
      text3d(0,0,0,tlab,cex=2)
      next3d()
    }else if(i==1){
      text3d(0,0,0,tlab,cex=2)
      next3d() 
    }
    
  }else if(porder$type[i]=="text"){
    if(porder$type[i-1]=="plot"){next3d()}
    
    if(i%in%c(2,3,4,11,17,19,26)){
      text3d(0,0,0,paste(names(newdat_time)[as.numeric(porder$ID[i])]," *",sep=""),cex=1.5)
    }else{text3d(0,0,0,names(newdat_time)[as.numeric(porder$ID[i])],cex=1.5)}
    
    next3d()
    
    # }else if(porder$type[i]=="text"){
    # if(porder$type[i-1]=="plot"){next3d()}
    #   text3d(0,0,0,paste(names(newdat_dist)[as.numeric(porder$ID[i])]," *",sep=""),cex=2,font=1)
    #   
    # if(i%in%c(2,3,4,10,17,19,20,25)){
    #   text3d(0,0,0,paste(names(newdat_dist)[as.numeric(porder$ID[i])]," *",sep=""),cex=2,font=1)}else{
    #     text3d(0,0,0,names(newdat_dist)[as.numeric(porder$ID[i])],cex=2,font=1)}
    # 
    
    # next3d()
    
  }else if(porder$type[i]=="plot"){
    col<-viridis(10)[cut(newdat_dist[[as.numeric(porder$ID[i])]][,porder$group[i]],breaks=10)]
    
    #x<-unique(newdat_dist[[as.numeric(porder$ID[i])]]$d.lat)
    #y<-unique(newdat_dist[[as.numeric(porder$ID[i])]]$d.space/1000)
    
    x<-unique(newdat_dist[[porder$ID2[i]]][,"d.lat"])
    y<-unique(newdat_dist[[porder$ID2[i]]][,"d.space"])
    z<-newdat_dist[[porder$ID2[i]]][,as.character(porder$group[i])]
    
    # z1<-newdat_dist[[porder$ID2[i]]][,paste0(as.character(porder$group[i]),"_lwr")]
    # z2<-newdat_dist[[porder$ID2[i]]][,paste0(as.character(porder$group[i]),"_upr")]
    # 
    #plot3d(x,y,z,type="n",xlab="",ylab="",zlab="",xlim=c(0,80),ylim=c(0,20))
    if(porder$group[i]=="Turnover"){
      plot3d(x,y,z,type="n",xlab="",ylab="",zlab="", xlim=c(xt[1],xt[5]),ylim=c(yt[1],yt[5]),zlim=c(zt[1],zt[6]),axes=FALSE)}else if(
        porder$group[i]=="Nestedness" ){
        plot3d(x,y,z,type="n",xlab="",ylab="",zlab="", xlim=c(xt[1],xt[5]),ylim=c(yt[1],yt[5]),zlim=c(zt[1],zt[5]),axes=FALSE)
      }
    
    surface3d(x,y,z,color=col,alpha=0.7,add=TRUE)
    # bg3d(sphere=FALSE,color="green",lit=FALSE,alpha=0.2)
    aspect3d(1,1,1)
    #
    box3d()
    
    #plot3d(x,y,z,type="n",xlab="",ylab="",zlab="")
    
    #  plot3d(x,y,z,type="n",xlab="",ylab="",zlab="",
    #        xlim=c(0,80),ylim=c(0,4000),zlim=c(l1,l2),
    #        box=F,axes=F)
    # 
    # surface3d(x,y,z,,color=col,alpha=0.7,add=TRUE)
    
    # if(i%in%c(6,7,8,14,21,23,24,29)){
    #   rgl.bbox(xlen=0,ylen=0,zlen=0,color=c("#88BFFF","black"),emission="black",specular="black",alpha=0.1,lit=TRUE)
    #   }
    
    axes3d(edges = "x--",at=xt,labels=xtl,cex=0.75,color="black",alpha=1)
    axes3d(edges = "y--",at=yt,labels=ytl,cex=0.75,color="black",alpha=1)
    axes3d(edges = "z+-",at=zt,labels=ztl,cex=0.75,color="black",alpha=1)
    
    mtext3d(xlab, "x+-", line =-1 ,level=4,cex=0.7,color="black",alpha=1)
    mtext3d(ylab, "y+-", line = 2.5,cex=0.7,color="black",alpha=1)
    mtext3d(zlab, "z+-",line=3,level=3,cex=0.7,color="black",alpha=1)
    
    
    rgl.viewpoint(  zoom = 0.8 )
    
    # plot3d(x,y,z,type="n",xlab="",ylab="",zlab="",
    #        xlim=c(0,80),ylim=c(0,20),zlim=c(l1,l2))
    # 
    # #plot3d(x,y,z,type="n",xlab="",ylab="",zlab="")
    # 
    # surface3d(x,y,z,,color=col,alpha=0.7,add=TRUE)
    # 
    # aspect3d(1,1,1)
    # mtext3d(xlab, "x--", line = 2)
    # mtext3d(ylab, "y+-", line = 2)
    # mtext3d(zlab, "z+-",line=1)
    
  }
}

par3d(windowRect = c(0, 0, 1500, 1000))

snapshot3d("Figures/Figure3.png",fmt="png")
#rgl.snapshot(filename="Figures/Figure3.png",fmt="png",width=300,height=300)

rgl.close()

temp<-c()
for(i in names(newdat_time)){
  temp<-c(temp,round(range(newdat_time[[i]]$Nestedness),2))
}
range(temp)

temp<-c()
for(i in names(newdat_time)){
  temp<-c(temp,round(range(newdat_time[[i]]$Turnover),2))
}
range(temp)

#################################### temporal 

Lims1<-c()
for(i in names(newdat_time)){
  Lims1<-c(Lims1,round(range(newdat_time[[i]]$Turnover),2))
}
range(Lims1)

Lims2<-c()
for(i in names(newdat_time)){
  Lims2<-c(Lims2,round(range(newdat_time[[i]]$Nestedness),2))
}
range(Lims2)

m0<-matrix(c(1,1,1,1,1,1,1,1,
             2,2,3,3,4,4,5,5,
             6,6,7,7,8,8,9,9,
             0,10,10,11,11,12,12,0,
             0,13,13,14,14,15,15,0,
             16,16,16,16,16,16,16,16,
             17,17,18,18,19,19,20,20,
             21,21,22,22,23,23,24,24,
             0,25,25,26,26,27,27,0,
             0,28,28,29,29,30,30,0),10,byrow = TRUE)
height<-c(1,0.5,4,0.5,4,1,0.5,4,0.5,4)
width<-c(1,1,1,1,1,1,1,1)
layout3d(m0,heights=height,widths=width,sharedMouse = TRUE)
par3d(windowRect = c(0, 0, 750, 750))

par(mar=c(0.1,0.1,0.1,0.1))
#par(xpd=TRUE)

xlab<-"Latitude (absolute)"
ylab<-"Time (days)"

yt<-c(0,500,1000,1500,2000)
ytl<-c("0","","1000","","2000")

xt<-c(0,20,40,60,80)
xtl<-c("0","","40","","80")

for(i in 1:length(porder[,1])){
  if(porder$group[i]=="Turnover"){l1<- range(Lims1)[1];l2<- range(Lims1)[2]}else if(porder$group[i]=="Nestedness"){l1<- range(Lims2)[1];l2<- range(Lims2)[2]}
  
  #if(porder$group[i]=="Turnover"){l1<- -1.5;l2<- 13}else if(porder$group[i]=="Nestedness"){l1<- -9.5;l2<- -3.7}
  if(porder$group[i]=="Turnover"){zlab="Turnover"}else if(porder$group[i]=="Nestedness"){zlab="Nestedness"}
  if(porder$group[i]=="Turnover"){tlab="Temporal Turnover"}else if(porder$group[i]=="Nestedness"){tlab="Temporal Nestedness"}
  if(porder$group[i]=="Turnover"){zt<-c(-13,-10,-7,-4,-1);ztl<-c("","-10","","-4","")}else if(porder$group[i]=="Nestedness"){zt<-c(-8,-7,-6,-5,-4);ztl<-c("","-7","","-5","")}
  
  if(porder$type[i]=="title"){
    if(i>1){next3d()}
    text3d(0,0,0,tlab,cex=2)
    next3d()
    
  }else if(porder$type[i]=="text"){
    if(porder$type[i-1]=="plot"){next3d()}
    
    if(i%in%c(2,4,5,11,17,19)){
      text3d(0,0,0,paste(names(newdat_time)[as.numeric(porder$ID[i])]," *",sep=""),cex=1.5)
    }else{text3d(0,0,0,names(newdat_time)[as.numeric(porder$ID[i])],cex=1.5)}
    
    next3d()
    
  }else if(porder$type[i]=="plot"){
    col<-viridis(10)[cut(newdat_time[[as.numeric(porder$ID[i])]][,porder$group[i]],breaks=10)]
    x<-unique(newdat_time[[as.numeric(porder$ID[i])]]$d.lat)
    y<-unique(newdat_time[[as.numeric(porder$ID[i])]]$d.time)
    z<-newdat_time[[as.numeric(porder$ID[i])]][,porder$group[i]]
    
    
    # plot3d(x,y,z,type="n",xlab="",ylab="",zlab="",
    #        xlim=c(0,80),ylim=c(0,20),zlim=c(l1,l2))
    
    plot3d(x,y,z,type="n",xlab="",ylab="",zlab="",xlim=c(xt[1],xt[5]),ylim=c(yt[1],yt[5]),zlim=c(zt[1],zt[5]),axes=FALSE)
    
    
    surface3d(x,y,z,color=col,alpha=0.7,add=TRUE)
    aspect3d(1,1,1)
    
    box3d()
    
    #plot3d(x,y,z,type="n",xlab="",ylab="",zlab="")
    
    #  plot3d(x,y,z,type="n",xlab="",ylab="",zlab="",
    #        xlim=c(0,80),ylim=c(0,4000),zlim=c(l1,l2),
    #        box=F,axes=F)
    # 
    # surface3d(x,y,z,,color=col,alpha=0.7,add=TRUE)
    axes3d(edges = "x--",at=xt,labels=xtl,cex=0.75)
    axes3d(edges = "y--",at=yt,labels=ytl,cex=0.75)
    axes3d(edges = "z+-",at=zt,labels=ztl,cex=0.75)
    
    mtext3d(xlab, "x+-", line =-1 ,level=4,cex=0.7)
    mtext3d(ylab, "y+-", line = 2.5,cex=0.7)
    mtext3d(zlab, "z+-",line=3,level=3,cex=0.7)
    
    
    
    # axes3d(edges = "x+-",at=xt,labels=xtl,cex=0.75)
    # axes3d(edges = "y+-",at=yt,labels=ytl,cex=0.75)
    # axes3d(edges = "z-+",at=zt,labels=ztl,cex=0.75)
    # #axes3d(edges =        , ),nticks=c(4,4,4),cex=0.75)
    # 
    # 
    # mtext3d(xlab, "x--", line =-1 ,level=4,cex=0.7)
    # mtext3d(ylab, "y--", line = -1,level=4,cex=0.7)
    # mtext3d(zlab, "z-+",line=3,level=3,cex=0.7)
    # 
    rgl.viewpoint(  zoom = 0.8 )
    #par3d(userMatrix=rotationMatrix(3,1,0,0))
    
  }
}

par3d(windowRect = c(0, 0, 1500, 1000))

snapshot3d("Figures/Figure4.png",fmt="png")

rgl.close()





ggplot(data=PairedData,aes(x=beta.sim2,y=beta.sim))+geom_point()+facet_wrap(~Reg2)
################################################################### Figure 2 #########################################################################


##figure 2 richness box plots
library(lubridate)

for(i in 1:nrow(EventInfo)){
  temp<-PairedData[which(PairedData$Trap1==EventInfo$TrapEvent[i]|PairedData$Trap2==EventInfo$TrapEvent[i]),]
  temp<-temp[which(temp$RegionGroup2!="Global"),]
  
  EventInfo$MeanDist[i]<-mean(temp$d.space)
  #EventInfo$Month[i]<-month(fastPOSIXct(EventInfo$Date[i]),label=FALSE)
  EventInfo$Region[i]<-unique(temp$RegionGroup2)
}

unique(EventInfo$Region)

EventInfo$Region2<-factor(EventInfo$Region,levels=c("North America", "Central America","South America","Eurasia","Africa","Oceania"))
EventInfo$AbsLat<-round(abs(EventInfo$Lat)/5)*5
EventInfo$MeanDistR<-round(EventInfo$MeanDist/100)*100

P1<-ggplot(data=EventInfo)+
  theme_classic()+
  facet_wrap(~Region2,scales = "free",nrow=1)+
  geom_boxplot(aes(x=AbsLat,y=Richness,group=AbsLat,fill=Region2))+
  scale_fill_manual(breaks = Cboard$Region,values=Cboard$colour)+
  theme(legend.position="none")+
  xlab("Absolute latitude")

P2<-ggplot(data=EventInfo)+
  theme_classic()+
  facet_wrap(~Region2,scales = "free",nrow=1)+
  geom_boxplot(aes(x=MeanDistR,y=Richness,group=MeanDistR,fill=Region2))+
  scale_fill_manual(breaks = Cboard$Region,values=Cboard$colour)+
  theme(legend.position="none",axis.text.x=element_text(angle=45,vjust=0.5) )+
  xlab("Mean distance (km)")
  

P3<-ggplot(data=EventInfo)+
  theme_classic()+
  facet_wrap(~Region2,scales = "free",nrow=1)+
  geom_boxplot(aes(x=Month,y=Richness,group=Month,fill=Region2))+
  scale_fill_manual(breaks = Cboard$Region,values=Cboard$colour)+
  theme(legend.position="none")+
  scale_x_continuous(breaks=seq(1,12,by=2))+
  xlab("Month")


ggsave(filename="Figures/Figure2.png",grid.arrange(P1,P2,P3,ncol=1),width=30,height=20,units="cm",dpi=300)


##################################### Diversity and Tempral figure (now two panels)  ###########################################################################################

raw<-read.csv("Data/CleanData_12Oct20.csv")

#supplementary Data files (GEB uploaded - needed to do this anyway)

SupplementaryData<-raw[,c("Project_Code","BIN","Order","Family")]
SupplementaryData<-SupplementaryData[which(SupplementaryData$Project_Code%in%EventInfo$TrapEvent),]
write.csv(SupplementaryData,row.names = FALSE,
          "C:/Users/User/Desktop/HKU/1.1.External Projects/GMP_keep_trying/0.01_FinalSubmissions/GEB_2023Feb/SupplementaryData/SupplementaryData.csv")


SupplmentaryData_02<-EventInfo[,c("TrapEvent","Date","Country","Lat","Lon","Site")]
colnames(SupplmentaryData_02)<-c("TrapEvent","Date","Country","Lat","Lon","Site Code Number")
SupplmentaryData_02<-SupplmentaryData_02[order(SupplmentaryData_02$`Site Code Number`),]
write.csv(SupplmentaryData_02,row.names = FALSE,
          "C:/Users/User/Desktop/HKU/1.1.External Projects/GMP_keep_trying/0.01_FinalSubmissions/GEB_2023Feb/SupplementaryData/SupplementaryData_02.csv")

SupplmentaryData_02<-read.csv("C:/Users/User/Desktop/HKU/1.1.External Projects/GMP_keep_trying/0.01_FinalSubmissions/GEB_2023Feb/SupplementaryData/SupplementaryData_02.csv")
SupplementaryData<-read.csv("C:/Users/User/Desktop/HKU/1.1.External Projects/GMP_keep_trying/0.01_FinalSubmissions/GEB_2023Feb/SupplementaryData/SupplementaryData.csv")


all(EventInfo$TrapEvent%in%raw$Project_Code)
all(raw$Project_Code%in%EventInfo$TrapEvent)
all(EventInfo$TrapEvent%in%SupplementaryData$Project_Code)
all(SupplementaryData$Project_Code%in%EventInfo$TrapEvent)
all(SupplementaryData$Project_Code%in%SupplmentaryData_02$TrapEvent)
all(SupplementaryData02$TrapEvent%in%SupplementaryData$Project_Code)

###################################################################################


SiteInfo$Site%in%EventInfo$Site


#diversity table (order frequency per site)

SiteTax<-data.frame(matrix(ncol=5,nrow=0,dimnames=list(c(),c("Site","Lat","Order","Freq","LogValue"))))

for(i in 1:nrow(SiteInfo)){
  
  S1<-EventInfo[which(EventInfo$Site==SiteInfo$Site[i]),"TrapEvent"]

  
  temp<-raw[which(raw$Project_Code%in%S1),]
  
  T1<-nrow(temp)
  
  for(j in unique(temp$BIN)){
    temp2<-data.frame(Site=SiteInfo$Site[i],
                      Lat=SiteInfo$Latitude[i],
                      Order=unique(temp[which(temp$BIN==j),"Order"]),
                      BIN=j,
                      Freq=length(temp[which(temp$BIN==j),"BIN"])/T1,
                      LogValue=log(length(temp[which(temp$BIN==j),"BIN"])))
    
    SiteTax<-rbind(SiteTax,temp2)
  }
  
  SiteInfo$BIN_mean[i]<-mean(EventInfo[which(EventInfo$Site==SiteInfo$Site[i]),"TotalUniqueBins"])
  SiteInfo$BIN_se[i]<-sd(EventInfo[which(EventInfo$Site==SiteInfo$Site[i]),"TotalUniqueBins"])/
    sqrt(length(EventInfo[which(EventInfo$Site==SiteInfo$Site[i]),"TotalUniqueBins"]))
  
  print(i)
  }

SiteInfo

#colours = c( "chocolate",  "brown", "darkgoldenrod")
# Define the number of colors you want
unique(SiteTax$Order)
SiteTax<-SiteTax[which(SiteTax$Order!=""),]

#reduce the number of groups to most frequent 

orderfreq<-data.frame(Order=unique(SiteTax$Order),
           Freq=0)

for(i in unique(orderfreq$Order)){
  orderfreq[which(orderfreq$Order==i),"Freq"]<-round(max(SiteTax[which(SiteTax$Order==i),"Freq"]),3)
}

orderfreq<-orderfreq[order(-orderfreq$Freq),]

SiteTax$Order2<-SiteTax$Order

SiteTax[which(SiteTax$Order%in%orderfreq$Order[which(orderfreq$Freq<0.01)]),"Order2"]<-"Other"

nb.cols <- length(unique(SiteTax$Order2))

library(RColorBrewer)

mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

mycolors <-viridis(n=nb.cols,option="H")
#order variables

SiteInfo[order(SiteInfo$Latitude),"Site"]

#SiteTax$Site <- factor(dat$Site,levels=unique(pcm$Site))

# temp2<-Meta[which(Meta$SampleID%in%unique(dat$Site)),c("SampleID","Station")]
# temp2<-temp2[match(unique(dat$Site),temp2$SampleID),]
# temp2<-temp2[order(temp2$Station),]
# dat$Site<-factor(dat$Site,levels=temp2$SampleID)

#SiteTax$BINr<-1

#levels(dat$Site)

SiteInfo$Lata<-abs(SiteInfo$Latitude)
SiteInfo[order(SiteInfo$Lata),"Site"]
SiteInfo$Site2<-factor(SiteInfo$Site,levels=SiteInfo[order(SiteInfo$Lata),"Site"])
EventInfo$Site2<-factor(EventInfo$Site,levels=SiteInfo[order(SiteInfo$Lata),"Site"])
SiteInfo$Site2<-factor(SiteInfo$Site,levels=SiteInfo[order(SiteInfo$Lata),"Site"])
SiteTax$Site2<-factor(SiteTax$Site,levels=SiteInfo[order(SiteInfo$Lata),"Site"])

SiteInfo[order(SiteInfo$Lata),"Site"]

SiteInfo[order(SiteInfo$Site),c("Latitude")]


labs<-as.character(format(round(SiteInfo[order(SiteInfo$Lata),c("Lata")],1),nsmall=1))
labs[seq(2,128,by=2)]<-""

SiteTax$Order2 <- factor(SiteTax$Order2, levels=c( "Other","Araneae","Archaeognatha","Blattodea","Coleoptera","Diptera","Entomobryomorpha","Hemiptera",
                                                   "Hymenoptera","Lepidoptera","Mesostigmata","Orthoptera","Plecoptera","Poduromorpha","Psocodea",
                                                   "Sarcoptiformes","Symphypleona","Thysanoptera","Trichoptera","Trombidiformes"))

DivFig = ggplot(SiteTax, aes(y=Freq,x = as.factor(Site2), fill = Order2)) + 
  geom_bar(stat = "identity") + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, size = 16, colour = "black", vjust = 0.5, hjust = 1, face= "bold"), 
        axis.title.y = element_text(size = 18, face = "bold"), legend.title = element_text(size = 16, face = "bold"), 
        legend.text = element_text(size = 12, face = "bold", colour = "black"), 
        axis.text.y = element_text(colour = "black", size = 16, face = "bold"),
        legend.position ="bottom") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_discrete(labels=labs)+
  labs(x = "", y = "Relative abundance", fill = "Order2") + 
  scale_fill_manual(values = mycolors)+
  guides(fill=guide_legend(title="Order"))
  

ggsave(file="Figures/Figure_DivFig.png",DivFig,width=45,height=25,units="cm",dpi=300)


#### log scales

DivFig2 = ggplot(SiteTax, aes(x = as.factor(Site2),y=LogValue, fill = Order2)) + 
  geom_bar(stat='identity') + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, size = 16, colour = "black", vjust = 0.5, hjust = 1, face= "bold"), 
        axis.title.y = element_text(size = 18, face = "bold"), legend.title = element_text(size = 16, face = "bold"), 
        legend.text = element_text(size = 12, face = "bold", colour = "black"), 
        axis.text.y = element_text(colour = "black", size = 16, face = "bold"),
        legend.position ="bottom") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_discrete(labels=labs)+
  labs(x = "", y = "Log abundance", fill = "Order2") + 
  scale_fill_manual(values = mycolors)+
  guides(fill=guide_legend(title="Order"))


ggsave(file="Figures/Figure_DivFig_log.png",DivFig2,width=45,height=25,units="cm",dpi=300)


Tabo<-data.frame(Order=unique(SiteTax$Order),
                 uniqueBINs=0,
                 Proportion=0)

for(i in unique(SiteTax$Order)){
  Tabo[which(Tabo$Order==i),"uniqueBINs"]<-length(unique(SiteTax[which(SiteTax$Order==i),"BIN"]))
  Tabo[which(Tabo$Order==i),"Proportion"]<-round((length(SiteTax[which(SiteTax$Order==i),"BIN"])/nrow(SiteTax)*100),5)
          
}


Tabo[order(-Tabo$Proportion),]

length(which(abs(EventInfo$Lat)<23.4))/length(EventInfo$Lat)


###temporal plot

library(fasttime)


P1<-ggplot(data=EventInfo,aes(y=as.Date(Date),x=as.factor(Site2),group=Site2,fill=Region))+
  geom_path()+
  geom_point(shape=21,size=3,stroke=NA)+
  theme_bw()+
  scale_x_discrete(labels=labs)+
 # scale_x_discrete(labels=format(round(SiteInfo[order(SiteInfo$Site2),c("Lata")],1),nsmall=1))+
  scale_fill_manual(breaks = Cboard$Region,values=Cboard$colour)+
  labs(y="Date",x="Latitude (absolute)")+
  theme(axis.text.y = element_text( size = 16, colour = "black"), 
        axis.title.y = element_text(size = 18, face = "bold", colour = "black"),
        axis.title.x = element_text(size = 18, face = "bold", colour = "black"), 
        legend.title = element_text(size = 18, face = "bold", colour = "black"), 
        legend.text = element_text(size = 12, face = "bold", colour = "black"), 
        axis.text.x = element_text(angle = 90, size = 16, colour = "black"),
        legend.position = c(0.93, 0.17)) 


ggsave(file="Figures/Figure_TempFig.png",P1,width=45,height=25,units="cm",dpi=300)


## i think this is what you want for relative abundance (%)
# 
cowplot:::plot_grid(DivFig, P1, align = "v", nrow = 2, rel_heights = c(2/3, 1/3))

ggsave(file="Figures/Figure_DivandTempFig.png",cowplot:::plot_grid(DivFig, P1, align = "v", nrow = 2, rel_heights = c(3/5, 2/5)),width=45,height=50,units="cm",dpi=300)


ggsave(file="Figures/Figure_DivandTempFig_2.png",
       cowplot:::plot_grid(DivFig,DivFig2, P1, align = "v", nrow = 3, rel_heights = c(1/3,1/3, 1/3)),
       width=45,height=50,units="cm",dpi=300)


######################### Beta diversity plots
library(ade4)

PairedData$beta.D.pod.qJ2<-(1-PairedData$beta.D.pod.qJ)

which(is.na(PairedData$beta.D.pod.qJ2))

range(PairedData$beta.D.pod.qJ)
range(PairedData$beta.D.pod.qJ2)

#mean(PairedData[which(PairedData$RegionGroup==i),c("beta.rich.pod.qJ")])

unique(PairedData$RegionGroup2)

Reg1<-c( "Global","North America","South America","Eurasia","Africa","Oceania")

pdf(file = "Figures/Beta.pdf",width=7,height=7)

par(mfrow=c(3,2))

for(i in Reg1){

triangle.plot(PairedData[which(PairedData$RegionGroup2==i),c("beta.rich.pod.qJ","beta.repl.pod.qJ","beta.D.pod.qJ")],
              show=FALSE,labeltriangle=FALSE,addmean=TRUE,addaxes = TRUE)
text(-0.45,0.5,"RichDiff",cex=1)
text(0.4,0.5,"Repl",cex=1)
text(0,-0.6,"Jaccard similarity",cex=1)
text(0,0.95,i,cex=1.5)

}

dev.off()

data(euro123)
tot <- rbind.data.frame(euro123$in78, euro123$in86, euro123$in97)
row.names(tot) <- paste(row.names(euro123$in78), rep(c(1, 2, 3), rep(12, 3)), sep = "")
triangle.plot(tot, label = row.names(tot), clab = 1)

histogram(PairedData$beta.D.pod.qJ)




# mean(PairedData[which(PairedData$RegionGroup2=="South America"),c("beta.repl.pod.qJ")])
# mean(PairedData[which(PairedData$RegionGroup2=="South America"),c("beta.rich.pod.qJ")])
# mean(PairedData[which(PairedData$RegionGroup2=="South America"),c("beta.D.pod.qJ")])
# 
# triangle.plot(PairedData[which(PairedData$RegionGroup2=="Oceania"),c("beta.rich.pod.qJ","beta.D.pod.qJ","beta.repl.pod.qJ")],
#               show=FALSE,labeltriangle=FALSE,addmean=TRUE)
# text(-0.45,0.5,"RichDiff",cex=1)
# text(0.4,0.5,"Repl",cex=1)
# text(0,-0.6,"Jaccard similarity",cex=1)
# text(0,0.95,i,cex=1.5)


png("Figures/Beta.png", width= 22,height= 25,units= "cm",res = 300,pointsize = 15)

par(mfrow=c(3,2))

for(i in Reg1){
  
  triangle.plot(PairedData[which(PairedData$RegionGroup2==i),c("beta.rich.pod.qJ","beta.D.pod.qJ","beta.repl.pod.qJ")],
                show=FALSE,labeltriangle=FALSE,addmean=FALSE,
                min3 = c(0,0, 0), max3 = c(1, 1,1))
  text(-0.45,0.5,"RichDiff",cex=1)
  text(0.4,0.5,"Repl",cex=1)
  text(0,-0.6,"Jaccard similarity",cex=1)
  text(0,0.95,i,cex=1.5)
  
}

dev.off()


##### beta across latitude (not viable really)
# 
# ggplot(data=PairedData,aes(y=beta.rich.pod.qJ,x=d.lat))+
#   geom_boxplot(aes(group=d.lat))+facet_wrap(~RegionGroup2,scales="free_x")
# 
# 
# ggplot(data=PairedData,aes(y=beta.rich.pod.qJ,x=d.lat))+
#   geom_point()+facet_wrap(~RegionGroup2,scales="free_x")

################################## alternative heatmap using untransformed data




##########################################################################



P1<-ggplot(data=newdat_dist[[1]],aes(y=d.lat,x=d.space,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_dist[1]))+  labs(fill='Turnover',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P2<-ggplot(data=newdat_dist[[2]],aes(y=d.lat,x=d.space,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[2]]$w),height=unique(newdat_dist[[2]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_dist[2]))+labs(fill='Turnover',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P3<-ggplot(data=newdat_dist[[3]],aes(y=d.lat,x=d.space,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[3]]$w),height=unique(newdat_dist[[3]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_dist[3]))+ labs(fill='Turnover',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P4<-ggplot(data=newdat_dist[[4]],aes(y=d.lat,x=d.space,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[4]]$w),height=unique(newdat_dist[[4]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_dist[4]))+ labs(fill='Turnover',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P5<-ggplot(data=newdat_dist[[5]],aes(y=d.lat,x=d.space,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[5]]$w),height=unique(newdat_dist[[5]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_dist[5]))+ labs(fill='Turnover',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P6<-ggplot(data=newdat_dist[[6]],aes(y=d.lat,x=d.space,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[6]]$w),height=unique(newdat_dist[[6]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_dist[6]))+ labs(fill='Turnover',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P7<-ggplot(data=newdat_dist[[7]],aes(y=d.lat,x=d.space,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[7])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[7])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[7]]$w),height=unique(newdat_dist[[7]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_dist[7]))+ labs(fill='Turnover',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))

P8<-ggplot(data=newdat_dist[[1]],aes(y=d.lat,x=d.space,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[1])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[1]]$w),height=unique(newdat_dist[[1]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_dist[1]))+  labs(fill='Nestedness',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P9<-ggplot(data=newdat_dist[[2]],aes(y=d.lat,x=d.space,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[2])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[2]]$w),height=unique(newdat_dist[[2]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_dist[2]))+labs(fill='Nestedness',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P10<-ggplot(data=newdat_dist[[3]],aes(y=d.lat,x=d.space,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[3])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[3]]$w),height=unique(newdat_dist[[3]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_dist[3]))+ labs(fill='Nestedness',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P11<-ggplot(data=newdat_dist[[4]],aes(y=d.lat,x=d.space,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[4])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[4]]$w),height=unique(newdat_dist[[4]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_dist[4]))+ labs(fill='Nestedness',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P12<-ggplot(data=newdat_dist[[5]],aes(y=d.lat,x=d.space,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[5])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[5]]$w),height=unique(newdat_dist[[5]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_dist[5]))+ labs(fill='Nestedness',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P13<-ggplot(data=newdat_dist[[6]],aes(y=d.lat,x=d.space,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[6])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[6]]$w),height=unique(newdat_dist[[6]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_dist[6]))+ labs(fill='Nestedness',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))
P14<-ggplot(data=newdat_dist[[7]],aes(y=d.lat,x=d.space,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[7])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_dist[7])),"dist"]))+
  geom_tile(aes(width=unique(newdat_dist[[7]]$w),height=unique(newdat_dist[[7]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_dist[7]))+ labs(fill='Nestedness',y="Latitude (absolute)",x="Distance (km)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank(),
        axis.text.x=element_text(angle=-25,vjust=0))


temp<-list(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14)

lay<-rbind(c(1,1,2,2,3,3,4,4),
           c(NA,5,5,6,6,7,7,NA),
           c(8,8,9,9,10,10,11,11),
           c(NA,12,12,13,13,14,14,NA))

ggsave(filenam="Figures/DistanceHeatmap_Figure3b.png",(grid.arrange(grobs = temp, layout_matrix = lay)),width=35,height=30,units="cm",dpi=300)

#time

for(i in 1:length(newdat_time)){
  newdat_time[[i]]$w<-(max(newdat_time[[i]]$d.time)-min(newdat_time[[i]]$d.time))*0.04
  newdat_time[[i]]$h<-(max(newdat_time[[i]]$d.lat)-min(newdat_time[[i]]$d.lat))*0.04
  
  newdat_time[[i]][which(newdat_time[[i]][,"Nestedness_t"]<0),"Nestedness"]<-0
}



P1<-ggplot(data=newdat_time[[1]],aes(y=d.lat,x=d.time,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[1]]$w),height=unique(newdat_time[[1]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_time[1]))+  labs(fill='Turnover',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P2<-ggplot(data=newdat_time[[2]],aes(y=d.lat,x=d.time,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[2]]$w),height=unique(newdat_time[[2]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_time[2]))+labs(fill='Turnover',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P3<-ggplot(data=newdat_time[[3]],aes(y=d.lat,x=d.time,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[3]]$w),height=unique(newdat_time[[3]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_time[3]))+ labs(fill='Turnover',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P4<-ggplot(data=newdat_time[[4]],aes(y=d.lat,x=d.time,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[4]]$w),height=unique(newdat_time[[4]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_time[4]))+ labs(fill='Turnover',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P5<-ggplot(data=newdat_time[[5]],aes(y=d.lat,x=d.time,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[5]]$w),height=unique(newdat_time[[5]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_time[5]))+ labs(fill='Turnover',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P6<-ggplot(data=newdat_time[[6]],aes(y=d.lat,x=d.time,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[6]]$w),height=unique(newdat_time[[6]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_time[6]))+ labs(fill='Turnover',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P7<-ggplot(data=newdat_time[[7]],aes(y=d.lat,x=d.time,fill=Turnover))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[7])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[7])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[7]]$w),height=unique(newdat_time[[7]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),direction=-1,option="H")+
  ggtitle(names(newdat_time[7]))+ labs(fill='Turnover',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())

P8<-ggplot(data=newdat_time[[1]],aes(y=d.lat,x=d.time,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[1])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[1]]$w),height=unique(newdat_time[[1]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_time[1]))+  labs(fill='Nestedness',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P9<-ggplot(data=newdat_time[[2]],aes(y=d.lat,x=d.time,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[2])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[2]]$w),height=unique(newdat_time[[2]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_time[2]))+labs(fill='Nestedness',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P10<-ggplot(data=newdat_time[[3]],aes(y=d.lat,x=d.time,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[3])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[3]]$w),height=unique(newdat_time[[3]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_time[3]))+ labs(fill='Nestedness',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P11<-ggplot(data=newdat_time[[4]],aes(y=d.lat,x=d.time,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[4])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[4]]$w),height=unique(newdat_time[[4]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_time[4]))+ labs(fill='Nestedness',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P12<-ggplot(data=newdat_time[[5]],aes(y=d.lat,x=d.time,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[5])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[5]]$w),height=unique(newdat_time[[5]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_time[5]))+ labs(fill='Nestedness',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P13<-ggplot(data=newdat_time[[6]],aes(y=d.lat,x=d.time,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[6])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[6]]$w),height=unique(newdat_time[[6]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_time[6]))+ labs(fill='Nestedness',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
P14<-ggplot(data=newdat_time[[7]],aes(y=d.lat,x=d.time,fill=Nestedness))+
  scale_y_continuous(labels=function(x) sprintf("%.0f", x),breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[7])),"lat"]))+
  scale_x_continuous(breaks=c(dist_breaks[which(dist_breaks$Region==names(newdat_time[7])),"time"]))+
  geom_tile(aes(width=unique(newdat_time[[7]]$w),height=unique(newdat_time[[7]]$h)))+scale_fill_viridis(label = function(x) sprintf("%.3f", x),option="H")+
  ggtitle(names(newdat_time[7]))+ labs(fill='Nestedness',y="Latitude (absolute)",x="Time (days)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.ticks=element_blank())
d

temp<-list(P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14)

lay<-rbind(c(1,1,2,2,3,3,4,4),
           c(NA,5,5,6,6,7,7,NA),
           c(8,8,9,9,10,10,11,11),
           c(NA,12,12,13,13,14,14,NA))

ggsave(filenam="Figures/TimeHeatmap_Figure4b.png",(grid.arrange(grobs = temp, layout_matrix = lay)),width=35,height=30,units="cm",dpi=300)









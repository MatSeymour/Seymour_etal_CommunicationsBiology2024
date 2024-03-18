
rm(list=ls())

library(data.table)
library(geosphere)

setwd("")

load(file = "S1_output2v2.RData")

nperm = 1
Explan<-c("beta.D.pod.qJ","beta.rich.pod.qJ","beta.repl.pod.qJ")
res <-data.frame(beta.D.pod.qJ=rep(NA,nperm+1),
                 beta.rich.pod.qJ=rep(NA,nperm+1),
                 beta.repl.pod.qJ=rep(NA,nperm+1))

StatsResults<-data.frame(
  perm.model=rep(c("~my.d.time+my.d.space+my.lat",
                   "~my.d.time*my.lat+my.d.space",
                   "~my.d.space*my.lat+my.d.time",
                   "~my.d.space*my.lat+my.d.time*my.lat",
                   "~my.d.space*my.lat+my.d.time*my.lat"),time=length(unique(PairedData$RegionGroup))),
  reduced.term=rep(c("my.lat",
                     "my.d.space",
                     "my.d.time",
                     "my.d.time:my.lat",
                     "my.d.space:my.lat"),time=length(unique(PairedData$RegionGroup))),
  pspace=rep(c("N","Y","N","N","Y"),time=length(unique(PairedData$RegionGroup))),
  plat=rep(c("Y","N","N","Y","Y"),time=length(unique(PairedData$RegionGroup))),
  ptime=rep(c("N","N","Y","Y","N"),time=length(unique(PairedData$RegionGroup))),
  Response=rep(c("lat","d.space","d.time","d.time x lat","d.space x lat"),time=length(unique(PairedData$RegionGroup))),
  Region=rep(unique(PairedData$RegionGroup),each=length(c("lat","d.space","d.time","d.time x lat","d.space x lat"))),
 
  permuted_Pod_total2=NA,permuted_Pod_Repl=NA,permuted_Pod_nest=NA)

for(s in 1:length(StatsResults[,1])){
  temp<-PairedData[which(PairedData$RegionGroup==StatsResults$Region[s]),]
  temp$Date1<-as.character(temp$Date1)
  temp$Date2<-as.character(temp$Date2)
  
  temp2<-as.data.table(temp)
  
  res <-data.frame(beta.sim2=rep(NA,nperm+1),
                   beta.sne2=rep(NA,nperm+1),
                   beta.sor2=rep(NA,nperm+1))
  
  for(perm in 1:(1+nperm)){
    print(c(StatsResults$Region[s],StatsResults$Response[s],perm,nperm+1))
    
    if(perm==1){temp2$my.d.space<-geosphere:::distHaversine(temp[,c("Lon1","Lat1")],temp[,c("Lon2","Lat2")])/1000 
    temp2$my.lat<-(abs(temp$Lat1)+abs(temp$Lat2))/2
    temp2$my.d.time<-round(temp$TimeAngleDiff/(360/365),0)
  
    temp2[,site.weights:=1/(temp2[,Num1]*temp2[,Num2])]}
    
    if(perm>1){
      site.permutation<-sample(unique(temp2$Site1))

      temp2$my.Lat1<-0;temp2$my.Lat2<-0;temp2$my.Lon1<-0;temp2$my.Lon2<-0
      
      if(StatsResults$pspace[s]=="Y"){
        for(i in unique(temp2$Site1)){
          temp2[Site1==i,my.Lat1 := SiteInfo[which(SiteInfo$Site==site.permutation[which(unique(temp[,"Site1"])==i)]),"Latitude"]]
          temp2[Site2==i,my.Lat2 := SiteInfo[which(SiteInfo$Site==site.permutation[which(unique(temp[,"Site1"])==i)]),"Latitude"]]
          
          temp2[Site1==i,my.Lon1 := SiteInfo[which(SiteInfo$Site==site.permutation[which(unique(temp[,"Site1"])==i)]),"Longitude"]]
          temp2[Site2==i,my.Lon2 := SiteInfo[which(SiteInfo$Site==site.permutation[which(unique(temp[,"Site1"])==i)]),"Longitude"]]
          }
        }else{
          temp2[,my.Lat1 :=Lat1]
          temp2[,my.Lat2 :=Lat2]
          temp2[,my.Lon1 :=Lon1]
          temp2[,my.Lon2 :=Lon2]
        }
      
      temp2[,my.d.space := geosphere:::distHaversine(temp2[,.(my.Lon1,my.Lat1)],temp2[,.(my.Lon2,my.Lat2)])/1000]
      
      if(StatsResults$plat[s]=="Y"&StatsResults$pspace[s]=="N"){
        for(i2 in unique(temp2$Site1)){
          temp2[Site1==i2,my.Lat1 := SiteInfo[which(SiteInfo$Site==site.permutation[which(unique(temp[,"Site1"])==i2)]),"Latitude"]]
          temp2[Site2==i2,my.Lat2 := SiteInfo[which(SiteInfo$Site==site.permutation[which(unique(temp[,"Site1"])==i2)]),"Latitude"]]
        }
        }else if(StatsResults$plat[s]=="N"&StatsResults$pspace[s]=="Y"|StatsResults$plat[s]=="N"&StatsResults$pspace[s]=="N"){
          temp2[,my.Lat1 :=Lat1]
          temp2[,my.Lat2 :=Lat2]
        }
      
      temp2[,my.lat := (abs(temp2[,my.Lat1])+abs(temp2[,my.Lat2]))/2]
    
      temp2$my.Date1<-"c";temp2$my.Date2<-"c"   
      
      if(StatsResults$ptime[s]=="Y"){ 
           for(i3 in unique(temp2$Site1)){
             permute.time1<-sample(unique(temp2[which(temp2$Site1==i3),"Date1_angle"]))
             permute.time2<-sample(unique(temp2[which(temp2$Site1==i3),"Date2_angle"]))
             
             for(t3 in unique(temp2[Site1==i3,Date1_angle])){
               
               temp2[temp2$Site1==i3&temp$Date1_angle==t3,my.Date1 := permute.time1[which(unique(temp2[Site1==i3,Date1_angle])==t3)]]
               temp2[temp2$Site2==i3&temp$Date2_angle==t3,my.Date2 := permute.time2[which(unique(temp2[Site1==i3,Date2_angle])==t3)]]
               }
             }
        }else{
          temp2[,my.Date1:=temp2[,Date1_angle]]
          temp2[,my.Date2:=temp2[,Date2_angle]]
        }
      
      temp2$my.d.time<-0 
 
      temp2$my.d.date1<-round(abs(as.numeric(temp2$my.Date1)-as.numeric(temp2$my.Date2))/(360/365),0)
      temp2$my.d.date2<-round((360-abs(as.numeric(temp2$my.Date1)-as.numeric(temp2$my.Date2))/(360/365)),0)
      
      temp2$my.d.time<-apply(temp2[,c("my.d.date1","my.d.date2")],1,min)
      
      temp2$site.weights<-0
      temp2[,site.weights:=1/(Num1*Num2)]
    }
    
    for(e in Explan){
        f1<-as.formula(paste(e,StatsResults$perm.model[s],sep=""))
        m.full <- lm(data=temp2,f1,weights = site.weights)
        m.reduced <- update(m.full,paste(". ~ .-",StatsResults$reduced.term[s]))
        res[perm,e]<- logLik(m.full)-logLik(m.reduced)
        }
    }
  StatsResults[s,c("permuted_Pod_total","permuted_Pod_nest","permuted_Pod_Repl")]<-c(1-mean(res[1,"beta.D.pod.qJ"]>res[c(2:(nperm+1)),"beta.D.pod.qJ"]),
                                                                                    1-mean(res[1,"beta.rich.pod.qJ"]>res[c(2:(nperm+1)),"beta.rich.pod.qJ"]),
                                                                                    1-mean(res[1,"beta.repl.pod.qJ"]>res[c(2:(nperm+1)),"beta.repl.pod.qJ"]))
  print(c(1-mean(res[1,"beta.D.pod.qJ"]>res[c(2:(nperm+1)),"beta.D.pod.qJ"]),
          1-mean(res[1,"beta.rich.pod.qJ"]>res[c(2:(nperm+1)),"beta.rich.pod.qJ"]),
          1-mean(res[1,"beta.repl.pod.qJ"]>res[c(2:(nperm+1)),"beta.repl.pod.qJ"])))
  
  write.csv(StatsResults,file = paste0("S2_results2_perm",nperm,".csv"))
  
  }

save(SiteInfo,StatsResults,file = "S2_output2.RData")

write.csv(StatsResults,file = paste0("S2_results2_perm",nperm,".csv"))






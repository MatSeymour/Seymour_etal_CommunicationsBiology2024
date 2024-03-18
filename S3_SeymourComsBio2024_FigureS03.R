

rm(list=ls())

library(ggplot2)

setwd("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts")
load(file = "S1_output2v2.RData")

P1<-c("beta.D.pod.qJ","beta.repl.pod.qJ","beta.rich.pod.qJ")

FigureS3<-data.frame(predictor=rep(P1,times=3,each=5),
                     variable=NA,
                     coef=0,
                     std_err=0,
                     Regions=rep(unique(PairedData$RegionGroup2),times=1,each=15))

PairedData$beta.D.pod.qJ<-1-PairedData$beta.D.pod.qJ


for(i in 1:length(unique(PairedData$RegionGroup2))){
  temp<-PairedData[which(PairedData$RegionGroup2==unique(PairedData$RegionGroup2)[i]),]
  
  
  for(j in P1){
    
   # formula<-as.formula(paste(j,"~scale(d.space)*scale(d.lat)+scale(d.time)*scale(d.lat)"))
    formula<-as.formula(paste(j,"~d.space*d.lat+d.time*d.lat"))
    
    coefmod<-lm(data=temp,formula,weights=1/(temp$Num1*temp$Num2))
    std_err<-sqrt(diag(vcov(coefmod)))
    
    FigureS3[which(FigureS3$predictor==j&FigureS3$Regions==unique(PairedData$RegionGroup2)[i]),"variable"]<-names(coefmod$coefficients)[2:6]
    FigureS3[which(FigureS3$predictor==j&FigureS3$Regions==unique(PairedData$RegionGroup2)[i]),"coef"]<-coef(coefmod)[2:6]
    FigureS3[which(FigureS3$predictor==j&FigureS3$Regions==unique(PairedData$RegionGroup2)[i]),"std_err"]<-std_err[2:6]
    
  }
  
}

FigureS3$predictor2<-rep(c("Total beta-diversity", "Species replacement", "Richness difference"),times=3,each=5)
FigureS3$predictor2 <- factor(FigureS3$predictor2, levels = c("Total beta-diversity", "Richness difference", "Species replacement"))
FigureS3$Regions <- factor(FigureS3$Regions, levels = c("Global", "North America", "South America","Oceania","Eurasia","Africa"))
#FigureS3$variable<-factor(FigureS3$variable,levels=c("scale(d.lat)","scale(d.time)","scale(d.space)",
#                                                     "scale(d.lat):scale(d.time)","scale(d.space):scale(d.lat)"))

FigureS3$variable<-factor(FigureS3$variable,levels=c("d.space:d.lat","d.lat:d.time","d.time","d.space","d.lat"))

hist(temp$beta.D.pod.qJ)
ggplot(temp,aes(x=beta.D.pod.qJ,y=d.lat))+geom_point()+geom_smooth()
FigureS3[which(FigureS3$variable=="scale(d.lat)"),]

FigureS3[which(FigureS3$Regions=="Eurasia"),]


ggplot()+
  geom_vline(xintercept=0, lty=2, lwd=1, colour="grey50") +
  geom_point(data=FigureS3,aes(x=coef,y=variable,color=variable),size=3) +
  geom_errorbar(data=FigureS3,aes(x=coef,y=variable,xmin=coef-std_err, xmax=coef+std_err,colour=variable),lwd=1, width=0.5)+
  facet_grid(predictor2 ~ Regions,scales="free") +
  labs(x="Value", y="Standardize Coefficient") +
  scale_x_continuous(breaks = function(x) pretty(x, n = 3), labels = function(x) sprintf("%.3f", x))+
  scale_y_discrete(labels = c("d.space x d.lat","d.lat x d.time","d.time","d.space","d.lat"))+
  theme_bw()+
  theme(legend.position = "none")


ggsave(filename="Figures/StandardizedEstimates_S3.png",width=30,height=20,units="cm",dpi=300)

FigureS3[which(FigureS3$Regions=="Global"),][c(2,1,3,5,4),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="Global"),][c(12,11,13,15,14),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="Global"),][c(7,6,8,10,9),c(1,2,3)]

FigureS3[which(FigureS3$Regions=="North America"),][c(2,1,3,5,4),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="North America"),][c(12,11,13,15,14),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="North America"),][c(7,6,8,10,9),c(1,2,3)]

FigureS3[which(FigureS3$Regions=="South America"),][c(2,1,3,5,4),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="South America"),][c(12,11,13,15,14),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="South America"),][c(7,6,8,10,9),c(1,2,3)]

FigureS3[which(FigureS3$Regions=="Oceania"),][c(2,1,3,5,4),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="Oceania"),][c(12,11,13,15,14),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="Oceania"),][c(7,6,8,10,9),c(1,2,3)]

FigureS3[which(FigureS3$Regions=="Eurasia"),][c(2,1,3,5,4),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="Eurasia"),][c(12,11,13,15,14),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="Eurasia"),][c(7,6,8,10,9),c(1,2,3)]

FigureS3[which(FigureS3$Regions=="Africa"),][c(2,1,3,5,4),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="Africa"),][c(12,11,13,15,14),c(1,2,3)]
FigureS3[which(FigureS3$Regions=="Africa"),][c(7,6,8,10,9),c(1,2,3)]

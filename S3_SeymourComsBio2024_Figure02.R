

rm(list=ls())

library(ggplot2)
library(viridis)
library(fasttime)
library(cowplot)

setwd("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts")
load(file = "S1_output2v2.RData")
load("Data/Counttables.RData")

raw<-read.csv("Data/CleanData_12Oct20.csv")


#diversity table (order frequency per site) - this takes some time to run

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
  
  print(paste(i,"of",nrow(SiteInfo)))
}


# Define the number of colors you want
unique(SiteTax$Order)
SiteTax<-SiteTax[which(SiteTax$Order!=""),]

#reduce the number of groups to most frequent to simplify the legend box
orderfreq<-data.frame(Order=unique(SiteTax$Order),Freq=0)

for(i in unique(orderfreq$Order)){
  orderfreq[which(orderfreq$Order==i),"Freq"]<-round(max(SiteTax[which(SiteTax$Order==i),"Freq"]),3)
}

#set the order for the figure
orderfreq<-orderfreq[order(-orderfreq$Freq),]
SiteTax$Order2<-SiteTax$Order
SiteTax[which(SiteTax$Order%in%orderfreq$Order[which(orderfreq$Freq<0.01)]),"Order2"]<-"Other"
nb.cols <- length(unique(SiteTax$Order2))

#assign colors
mycolors <-viridis(n=nb.cols,option="H")

SiteInfo[order(SiteInfo$Latitude),"Site"]

SiteInfo$Lata<-abs(SiteInfo$Latitude)
SiteInfo[order(SiteInfo$Lata),"Site"]
SiteInfo$Site2<-factor(SiteInfo$Site,levels=SiteInfo[order(SiteInfo$Lata),"Site"])
EventInfo$Site2<-factor(EventInfo$Site,levels=SiteInfo[order(SiteInfo$Lata),"Site"])
SiteInfo$Site2<-factor(SiteInfo$Site,levels=SiteInfo[order(SiteInfo$Lata),"Site"])
SiteTax$Site2<-factor(SiteTax$Site,levels=SiteInfo[order(SiteInfo$Lata),"Site"])

SiteInfo[order(SiteInfo$Lata),"Site"]

SiteInfo[order(SiteInfo$Site),c("Latitude")]


labs<-as.character(format(round(SiteInfo[order(SiteInfo$Lata),c("Lata")],1),nsmall=1))
seq1<-seq(1,128,by=1)
seq1<-which(!seq1%in%seq(2,128,by=4))
labs[seq1]<-""

SiteTax$Order2 <- factor(SiteTax$Order2, levels=c( "Other","Araneae","Archaeognatha","Blattodea","Coleoptera","Diptera","Entomobryomorpha","Hemiptera",
                                                   "Hymenoptera","Lepidoptera","Mesostigmata","Orthoptera","Plecoptera","Poduromorpha","Psocodea",
                                                   "Sarcoptiformes","Symphypleona","Thysanoptera","Trichoptera","Trombidiformes"))


AxisSize<-20
AxisTitle<-22


#upper panel
# DivFig = ggplot(SiteTax, aes(y=Freq,x = as.factor(Site2), fill = Order2)) + 
#   geom_bar(stat = "identity") + 
#   theme_classic()+
#   theme(axis.text.x = element_text(angle = 90, size = AxisSize, colour = "black", vjust = 0.5, hjust = 1, face= "bold"), 
#         axis.title.y = element_text(size = AxisTitle, face = "bold"), legend.title = element_text(size = 16, face = "bold"), 
#         legend.text = element_text(size = 12, face = "bold", colour = "black"), 
#         axis.text.y = element_text(colour = "black", size = AxisSize, face = "bold"),
#         legend.position ="bottom") + 
#   scale_y_continuous(expand = c(0,0), limits=c(0,1)) + 
#   scale_x_discrete(labels=labs)+
#   labs(x = "", y = "Relative abundance", fill = "Order2") + 
#   scale_fill_manual(values = mycolors)+
#   guides(fill=guide_legend(title="Order"))

DivFig =ggplot(SiteTax, aes(y = Freq, x = as.factor(Site2), fill = Order2)) + 
  geom_bar(stat = "identity", width = 1) +  # Adjust the width parameter
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 90, size = AxisSize, colour = "black", vjust = 0.5, hjust = 1, face = "bold"), 
    axis.title.y = element_text(size = AxisTitle, face = "bold"), 
    legend.title = element_text(size = 16, face = "bold"), 
    legend.text = element_text(size = 12, face = "bold", colour = "black"), 
    axis.text.y = element_text(colour = "black", size = AxisSize, face = "bold"),
    legend.position = "bottom",
    plot.margin = margin(1, 1, 1.5, 1, "cm")  # Adjust the margins as needed
  ) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) + 
  scale_x_discrete(labels = labs) +
  labs(x = "", y = "Relative abundance", fill = "Order2") + 
  scale_fill_manual(values = mycolors) +
  guides(fill = guide_legend(title = "Order"))



#### middle panel

DivFig2 = ggplot(SiteTax, aes(x = as.factor(Site2),y=LogValue, fill = Order2)) + 
  geom_bar(stat='identity',width=1) + 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, size = AxisSize, colour = "black", vjust = 0.5, hjust = 1, face= "bold"), 
        axis.title.y = element_text(size = AxisTitle, face = "bold"), legend.title = element_text(size = 16, face = "bold"), 
        legend.text = element_text(size = 12, face = "bold", colour = "black"), 
        axis.text.y = element_text(colour = "black", size = AxisSize, face = "bold"),
        legend.position ="bottom") + 
  scale_y_continuous(expand = c(0,0)) + 
  scale_x_discrete(labels=labs)+
  labs(x = "", y = "Log abundance", fill = "Order2") + 
  scale_fill_manual(values = mycolors)+
  guides(fill=guide_legend(title="Order"))

Tabo<-data.frame(Order=unique(SiteTax$Order),
                 uniqueBINs=0,
                 Proportion=0)

for(i in unique(SiteTax$Order)){
  Tabo[which(Tabo$Order==i),"uniqueBINs"]<-length(unique(SiteTax[which(SiteTax$Order==i),"BIN"]))
  Tabo[which(Tabo$Order==i),"Proportion"]<-round((length(SiteTax[which(SiteTax$Order==i),"BIN"])/nrow(SiteTax)*100),5)
  
}


Tabo[order(-Tabo$Proportion),]
length(which(abs(EventInfo$Lat)<23.4))/length(EventInfo$Lat)


for(i in 1:nrow(EventInfo)){
  temp<-PairedData[which(PairedData$Trap1==EventInfo$TrapEvent[i]|PairedData$Trap2==EventInfo$TrapEvent[i]),]
  temp<-temp[which(temp$RegionGroup2!="Global"),]
  
  EventInfo$MeanDist[i]<-mean(temp$d.space)
  EventInfo$Region[i]<-unique(temp$RegionGroup2)
}

Cboard<-data.frame(Region=c("North America", "Oceania","Eurasia","South America","Africa"),colour=NA)
Cboard$colour<-viridis(length(unique(SiteInfo$Region)),option="H")


P1<-ggplot(data=EventInfo,aes(y=as.Date(Date),x=as.factor(Site2),group=Site2,fill=Region))+
  geom_path(color="grey")+
  geom_point(shape=21,size=2,stroke=NA)+
  theme_bw()+
  scale_x_discrete(labels=labs)+
  # scale_x_discrete(labels=format(round(SiteInfo[order(SiteInfo$Site2),c("Lata")],1),nsmall=1))+
  scale_fill_manual(breaks = Cboard$Region,values=Cboard$colour)+
  labs(y="Date",x="Latitude (absolute)")+
  theme(axis.text.y = element_text( size = AxisSize, colour = "black"), 
        axis.title.y = element_text(size = AxisTitle, face = "bold", colour = "black"),
        axis.title.x = element_text(size = AxisTitle, face = "bold", colour = "black"), 
        legend.title = element_text(size = 18, face = "bold", colour = "black"), 
        legend.text = element_text(size = 12, face = "bold", colour = "black"), 
        axis.text.x = element_text(angle = 90, size = AxisSize, colour = "black",face="bold"),
        legend.position = c(0.93, 0.17)) 

#Figure 2
ggsave(file="Figures/Figure02.png",
       cowplot:::plot_grid(DivFig,DivFig2, P1, align = "v", nrow = 3, rel_heights = c(1/3,1/3, 1/3)),
       width=45,height=50,units="cm",dpi=300)

ggsave(file="Figures/Figure02.pdf",
       cowplot:::plot_grid(DivFig,DivFig2, P1, align = "v", nrow = 3, rel_heights = c(1/3,1/3, 1/3)),
       width=45,height=50,units="cm",dpi=300)


### proportion of groups by regions

SiteTax$Region<-NA

for(i in SiteInfo$Site){
  SiteTax[SiteTax$Site==i,"Region"]<-SiteInfo[which(SiteInfo$Site==i),"Region"]
}

unique(SiteTax$Region)

SiteTaxNA<-SiteTax[which(SiteTax$Region=="North America"),]
SiteTaxSA<-SiteTax[which(SiteTax$Region=="South America"),]
SiteTaxE<-SiteTax[which(SiteTax$Region=="Eurasia"),]
SiteTaxO<-SiteTax[which(SiteTax$Region=="Oceania"),]
SiteTaxA<-SiteTax[which(SiteTax$Region=="Africa"),]

#reduce the number of groups to most frequent to simplify the legend box
orderfreq01<-data.frame(Order=unique(SiteTaxNA$Order),Freq=0)
orderfreq02<-data.frame(Order=unique(SiteTaxSA$Order),Freq=0)
orderfreq03<-data.frame(Order=unique(SiteTaxE$Order),Freq=0)
orderfreq04<-data.frame(Order=unique(SiteTaxO$Order),Freq=0)
orderfreq05<-data.frame(Order=unique(SiteTaxA$Order),Freq=0)

for(i in unique(orderfreq01$Order)){
  orderfreq01[which(orderfreq01$Order==i),"Freq"]<-round(sum(exp(SiteTaxNA[which(SiteTaxNA$Order==i),"LogValue"]))/sum(exp(SiteTaxNA$LogValue)),3)
    }
orderfreq01<-orderfreq01[order(-orderfreq01$Freq),]

for(i in unique(orderfreq02$Order)){
  orderfreq02[which(orderfreq02$Order==i),"Freq"]<-round(sum(exp(SiteTaxSA[which(SiteTaxSA$Order==i),"LogValue"]))/sum(exp(SiteTaxSA$LogValue)),3)
}
orderfreq02<-orderfreq02[order(-orderfreq02$Freq),]

for(i in unique(orderfreq05$Order)){
  orderfreq05[which(orderfreq05$Order==i),"Freq"]<-round(sum(exp(SiteTaxA[which(SiteTaxA$Order==i),"LogValue"]))/sum(exp(SiteTaxA$LogValue)),3)
}
orderfreq05<-orderfreq05[order(-orderfreq05$Freq),]


for(i in unique(orderfreq03$Order)){
  orderfreq03[which(orderfreq03$Order==i),"Freq"]<-round(sum(exp(SiteTaxE[which(SiteTaxE$Order==i),"LogValue"]))/sum(exp(SiteTaxE$LogValue)),3)
}
orderfreq03<-orderfreq03[order(-orderfreq03$Freq),]


for(i in unique(orderfreq04$Order)){
  orderfreq04[which(orderfreq04$Order==i),"Freq"]<-round(sum(exp(SiteTaxO[which(SiteTaxO$Order==i),"LogValue"]))/sum(exp(SiteTaxO$LogValue)),3)
}
orderfreq04<-orderfreq04[order(-orderfreq04$Freq),]







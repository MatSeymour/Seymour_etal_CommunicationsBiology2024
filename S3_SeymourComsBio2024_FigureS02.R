rm(list = ls())

library(ade4)

setwd("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts")
load(file = "S1_output2v2.RData")


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

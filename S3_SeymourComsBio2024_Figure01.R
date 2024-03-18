

rm(list=ls())

library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(ggplot2)

#Set working directory

setwd("C:/Users/User/Desktop/HKU/0.1.2.External Projects/GMP_keep_trying/0.11_DataForScripts_Desposit/0.1.GMP_Scripts")

#load data files from S1 script output
load(file = "S1_output2v2.RData")
load("Data/Counttables.RData")

#Setup color scheme for different regions
Cboard<-data.frame(Region=c("North America", "Oceania","Eurasia","South America","Africa"),colour=NA)
Cboard$colour<-viridis(length(unique(SiteInfo$Region)),option="H")

#world base map
world <- ne_countries(scale = "medium", returnclass = "sf")
#remove antartica
world <- world[which(world$continent!="Antarctica"),]

colnames(world)

#reclass french guiana to south america
franceg<-ne_countries(geounit='french guiana',scale = "medium", returnclass = "sf",type = "map_units")
Papua <- ne_states(country = "Indonesia", returnclass = "sf")
Papua<-Papua[which(Papua$name%in%c("Papua Barat","Papua")),]

#assign regions 
Region_NorthAmerica<-world[which(world$subregion%in%c("Central America","Caribbean","Northern America")),]
Region_SouthAmerica<-world[which(world$subregion%in%c("South America")),]
Region_Eurasia<-world[which(world$subregion%in%c("Southern Asia","Southern Europe","Northern Europe","Western Asia","Western Europe",
                                                 "Eastern Europe","South-Eastern Asia","Eastern Asia","Central Asia")),]
Region_Africa<-world[which(world$subregion%in%c("Middle Africa","Western Africa","Southern Africa","Northern Africa","Eastern Africa")),]
Region_Oceania<-world[which(world$subregion%in%c("Polynesia","Australia and New Zealand","Melanesia","Micronesia","Indonesia")),]


#Figure 1
ggsave(filename="Figures/Figure1.png",
       ggplot() +geom_sf(data=world,colour="black")+theme_void()+
         geom_sf(data=Region_NorthAmerica,fill=Cboard[which(Cboard$Region=="North America"),"colour"],color="black")+
         geom_sf(data=Region_SouthAmerica,fill=Cboard[which(Cboard$Region=="South America"),"colour"],color="black")+
         geom_sf(data=Region_Eurasia,fill=Cboard[which(Cboard$Region=="Eurasia"),"colour"],color="black")+
         geom_sf(data=Region_Africa,fill=Cboard[which(Cboard$Region=="Africa"),"colour"],color="black")+
         geom_sf(data=Region_Oceania,fill=Cboard[which(Cboard$Region=="Oceania"),"colour"],color="black")+
         geom_sf(data=franceg,fill=Cboard[which(Cboard$Region=="South America"),"colour"],color="black")+
         geom_sf(data=Papua,fill=Cboard[which(Cboard$Region=="Oceania"),"colour"],color="black")+
         geom_point(data=tibble::as_tibble(SiteInfo),aes(x=Longitude,y=Latitude),shape=21,fill="white",size=4,stroke=1.5)+
         annotate(geom="text", x=-120, y=-40, label="Sampling sites = 129",color="Black",size=4)+
         annotate(geom="text", x=-120, y=-45, label="Trapping events = 2412",color="Black",size=4),
       width=30,height=15,dpi=300,units="cm")

#Figure 1
ggsave(filename="Figures/Figure1.pdf",
       ggplot() +geom_sf(data=world,colour="black")+theme_void()+
         geom_sf(data=Region_NorthAmerica,fill=Cboard[which(Cboard$Region=="North America"),"colour"],color="black")+
         geom_sf(data=Region_SouthAmerica,fill=Cboard[which(Cboard$Region=="South America"),"colour"],color="black")+
         geom_sf(data=Region_Eurasia,fill=Cboard[which(Cboard$Region=="Eurasia"),"colour"],color="black")+
         geom_sf(data=Region_Africa,fill=Cboard[which(Cboard$Region=="Africa"),"colour"],color="black")+
         geom_sf(data=Region_Oceania,fill=Cboard[which(Cboard$Region=="Oceania"),"colour"],color="black")+
         geom_sf(data=franceg,fill=Cboard[which(Cboard$Region=="South America"),"colour"],color="black")+
         geom_sf(data=Papua,fill=Cboard[which(Cboard$Region=="Oceania"),"colour"],color="black")+
         geom_point(data=tibble::as_tibble(SiteInfo),aes(x=Longitude,y=Latitude),shape=21,fill="white",size=4,stroke=1.5)+
         annotate(geom="text", x=-120, y=-40, label="Sampling sites = 129",color="Black",size=4)+
         annotate(geom="text", x=-120, y=-45, label="Trapping events = 2412",color="Black",size=4),
       width=30,height=15,dpi=300,units="cm")


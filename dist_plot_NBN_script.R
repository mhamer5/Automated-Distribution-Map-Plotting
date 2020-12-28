#!/usr/bin/env Rscript
rm(list=ls())
dev.off()
setwd("E:/UK Isopoda")

#Load libraries
library(tidyverse)
library(purrr)
library(svglite)

#Load in data (here I am using the isopod interim database found on NBN. Citation : NBN Atlas occurrence download at NBN Atlas accessed on Wed Dec 16 15:29:17 UTC 2020.)
#Although I use the isopod occurrence dataset, NBN has a standard format that should be uniform across downloads
rawdata<-read_csv("NBN_isopod_interim_records-2020-12-16.csv")[,c("OSGR 10km","Scientific name","Taxon author","Start date year")]
#Merge scientific name and taxon author
rawdata$`Scientific name`<-paste(rawdata$`Scientific name`,rawdata$`Taxon author`)
#Remove excess columns
rawdata<-rawdata[-3]
#Give standardised names across columns 
names(rawdata)<-c("OS_grid_ref", "descriptive", "upper_date")
#Load in every 10km square coordinates for UK
coords<-read_csv("gb_10kms.csv")
#Add 5 to coordinates because Stuart Ball said so (center points of each square)
coords$x<-coords$x+5
coords$y<-coords$y+5
#Remane coloumn names to be more uniform across datasets
names(coords)<-c("OS_grid_ref", "x", "y")
#Load in GB outline data
gb <- read_csv("gb.txt")
#Remove -1 and replace with NA because Stuart Ball said so
gb$x[gb$x==-1]<-NA
gb$y[gb$y==-1]<-NA

#Merge UK 10km squares coordinates with record grid squares
rawdata<-merge(rawdata, coords, by="OS_grid_ref")
#Date stuff I'll do this later
rawdata$upper_year<-str_sub(rawdata$upper_date,1,4)
rawdata$upper_year<-as.numeric(rawdata$upper_year)

#Find the latest recorded year for each species for each 10km it is recorded in
rawdata<-aggregate(upper_year~x+y+descriptive, data=rawdata, FUN = max)

rawdata$period<-cut(rawdata$upper_year,
                        breaks=c(0, 1900, 1960, 2000),
                        labels=c("Before 1900", "1900-1960", "1960-2000"),
                        right = F)
plot.distribution<-function(){
#Generate a unique species list from dataset
uniq_species <-unique(rawdata$descriptive)

#Plot GB map and species points, alongside title and legend
for(i in seq_along(uniq_species)){
  
  dist = ggplot() +
    geom_polygon(data=gb, 
                 aes(x=x, y=y), 
                 colour="black",
                 fill=NA) +
    scale_x_continuous(breaks=seq(0,660,100))+
    scale_y_continuous(breaks=seq(0,1240,100))+
    coord_fixed()+
    theme_bw()+
    theme(axis.text=element_blank(),
          axis.title = element_blank(),
          axis.ticks=element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = c(0.8,0.63),
          legend.text=element_text(size=30),
          legend.title=element_text(size=32),
          plot.title=element_text(vjust = -20,hjust=0.1, size=22, family="Times New Roman", face="bold.italic"))+
    ggtitle(uniq_species[i])+
    geom_point(data=filter(rawdata,
                           descriptive == uniq_species[i]), 
               aes(x=x, y=y, fill=period), 
               pch=21, colour = "grey30", size = 2.5)+
    scale_fill_manual(values = c("Before 1900" ="white",
                                 "1900-1960"="grey",
                                 "1960-2000"="black"),
                      name="Latest Record")
  
  ggsave(filename=paste0("iso_dist_maps/",
                         uniq_species[i],
                         ".svg"),
         plot=dist,
         width = 7, height=13, units ="in")
  
  print(plot)
  
}     
}
plot.distribution()

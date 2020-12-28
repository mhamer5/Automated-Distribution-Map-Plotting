#!/usr/bin/env Rscript
rm(list=ls())
dev.off()
setwd("E:/Hymenoptera/United_Kingdom/Dis_maps/plotdistmaps")

#Load libraries
library(tidyverse)
library(purrr)
library(svglite)

#Load in data
rawdata<-read_csv("20200821_BWARS_Public.csv")[,c("descriptive", "upper_date", "OS_grid_ref")]
#Remove semicolons (files in Window can't handle them), this could be done before hand in the datafile
rawdata$descriptive<-gsub(":", "", rawdata$descriptive)
#Create upper year columns
rawdata$upper_year<-str_sub(rawdata$upper_date,1,4)
rawdata$upper_year<-as.numeric(rawdata$upper_year)
#generate breaks in time series
rawdata$period<-cut(rawdata$upper_year,
                    breaks=c(0,1980,2000,2030),
                    labels=c("Before 1980", "1980-99", "2000 onwards"),
                    right = F)

#Load in every 10km square coordinates for UK
coords<-read_csv("gb_10kms.csv")
#Rename coloumn names to be more uniform across datasets
names(coords)<-c("OS_grid_ref", "x", "y")
#Add 5 to coordinates because Stuart Ball said so (center points of each square)
coords$x<-coords$x+5
coords$y<-coords$y+5

#Load in GB outline data
gb <- read_csv("gb.txt")
#Remove -1 and replace with NA because Stuart Ball said so
gb$x[gb$x==-1]<-NA
gb$y[gb$y==-1]<-NA

#Merge UK 10km squares coordinates with record grid squares
rawdata<-merge(rawdata, coords, by="OS_grid_ref")
#Create upper year columns
rawdata$upper_year<-str_sub(rawdata$upper_date,1,4)
rawdata$upper_year<-as.numeric(rawdata$upper_year)

#Find the latest recorded year for each species for each 10km it is recorded in
rawdata<-aggregate(upper_year~x+y+descriptive, data=rawdata, FUN = max)

#Generate unique species list to loop through
uniq_species <-unique(rawdata$descriptive)

#Delimit distribution map function 
plot.distribution<- function(){

  for(i in seq_along(uniq_species)){
    
    dist=ggplot() +
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
            legend.text=element_text(size=25),
            legend.title=element_text(size=26))+
      geom_point(data=filter(rawdata,
                             descriptive == uniq_species[i]),
                           aes(x=x, y=y, fill = period), 
                 pch=21, colour = "grey30", size = 2.5)+
      scale_fill_manual(values = c("Before 1980" ="white",
                                   "1980-99"="grey",
                                   "2000 onwards"="black"),
                        name="Latest Record")
    #save plots into acu_diss_maps2020/ file as .svgs
    ggsave(filename=paste0("acu_diss_maps2020/",
                          uniq_species[i],
                          ".svg"),
    plot=dist,
    width = 7, height=13, units ="in")

   print(plot)
}
}
#Run function
plot.distribution()


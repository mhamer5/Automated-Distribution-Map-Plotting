dev.off()
rm(list=ls())
setwd("~/phenology_graph")

#load packages
library(Cairo)
library(tidyverse)
#read in data
rawdata<-read_csv("20200821_BWARS_Public.csv")
#remove : from species names, windoes files cant handle them
rawdata$descriptive<-gsub(":", "", rawdata$descriptive)

#strip upper year down to month only
rawdata$upper_year<-str_sub(rawdata$upper_date,1,7)
rawdata$upper_month<-str_sub(rawdata$upper_date,6,7)

#vector of months in order which is needed latter
labels=c("Jan", "Feb","Mar", "Apr","May", "Jun","Jul", "Aug","Sep", "Oct","Nov", "Dec")

#change month from number to word form - must be a better way to do this
rawdata$upper_month<-gsub("01", "Jan", rawdata$upper_month)
rawdata$upper_month<-gsub("02", "Feb", rawdata$upper_month)
rawdata$upper_month<-gsub("03", "Mar", rawdata$upper_month)
rawdata$upper_month<-gsub("04", "Apr", rawdata$upper_month)
rawdata$upper_month<-gsub("05", "May", rawdata$upper_month)
rawdata$upper_month<-gsub("06", "Jun", rawdata$upper_month)
rawdata$upper_month<-gsub("07", "Jul", rawdata$upper_month)
rawdata$upper_month<-gsub("08", "Aug", rawdata$upper_month)
rawdata$upper_month<-gsub("09", "Sep", rawdata$upper_month)
rawdata$upper_month<-gsub("10", "Oct", rawdata$upper_month)
rawdata$upper_month<-gsub("11", "Nov", rawdata$upper_month)
rawdata$upper_month<-gsub("12", "Dec", rawdata$upper_month)

#change upper month to a factor rather than character
rawdata$upper_month<-as.factor(rawdata$upper_month)

#generate unique species list to loop through
uniq_species <-unique(rawdata$descriptive)
unique_sp_list<-as.list(uniq_species)

#make an empty list for plots to be chucked into
plot_list=list()

#loop through each species and plot the number of records per month. Frustratingly, tally() will
#drop months with 0 records which eventually messes up the bar plot, so each species needs their 
#missing months put back in with a 0. plots will need to be stylised better
for (i in uniq_species){

subsetted_rawdata<-subset(rawdata, descriptive == i)
month_counts<-subsetted_rawdata %>% group_by(upper_month) %>% tally()

missing_months<-labels[which(!labels %in% month_counts$upper_month)]

missing_months<-as.data.frame(missing_months)

names(missing_months)<-"upper_month"

month_counts<-bind_rows(month_counts, missing_months)

month_counts[is.na(month_counts)] <- 0

plot<-ggplot(data=month_counts, 
             aes(x = factor(upper_month, level = labels), y=n)) +
  geom_bar(stat="identity")+
  ylab("Number of Records")+
  xlab("Record Month")+
  theme_classic()+
  theme(axis.text = element_text(color = "black", size = 28),
        axis.title=element_text(size=30,face="bold"),
        axis.ticks = element_blank())+ 
  scale_y_continuous(expand = c(0,0))

plot_list[[i]]=plot
}
#for loop to spit out plots into acu_phenology
for (i in uniq_species){
  file_name = paste("acu_phenology/", i, ".png")
  CairoPNG(file=file_name, width = 1400, height=1000)
  print(plot_list[[i]])
  dev.off()
}

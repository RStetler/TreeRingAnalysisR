setwd("C://Users/Ruth/Documents/R/ITRDB_plots")
library(ggplot2)
library(tidyr)
#A theme I like to use with facets. Change accordingly, particualarly axis titles as they might
#turn out too big or small
FacetTheme<-theme_bw()+theme(plot.title=element_text(size=15),axis.title.x=element_text(size=12),
                          axis.title.y=element_text(size=12), axis.text.y=element_text(size=9),
                          axis.text.x=element_text(size=9),strip.background=element_blank(),
                          panel.background=element_blank(),axis.line=element_line(color="black"))+theme(legend.key=element_blank())

File1<-read.csv("C://Users/Ruth/Documents/R/ITRDB_Data_W_TRADER_ALL.csv")


#This is something of a cop-out, but I've used this approach frequently because I can see the process
 #step by step. I'm sure dplyr has a better way. Basically, each group of site you want to facet on
 #a single print-out page needs a separate "group" so that we can subset each of these groups within
 #the loop below and save individually

Sites<-as.character(as.factor(unique(File1$siteID))) #character string of all unique sites.

Cats<-length(Sites)/12  #Make a category for each n groups. This will be subsetted later in the loop to make the individual facetted graphs
 #for example, if you want 12 facets per page, divide by 12 here.

Groups<-rep(1:Cats,each=12)  #ID variable for each of the groups. Repeat by # of facets you want per page

#If Cats is not a whole number, add on a group for the odd one out
Extras<-rep((max(Groups)+1),times=length(Sites)-length(Groups))

Groups<-c(Groups,Extras) #Concatenate Groups and Extras

#Make a data frame to merge these new groups with original
SiteGroups<-data.frame(Site=Sites,Groups=as.character(as.numeric(Groups))) #Make a data frame with the groups you created 
File1<-merge(File1,SiteGroups,by.x = "siteID", by.y = "Site")

groupnum <- 1:25
#Loops through each facet, saves a separate file each time. Adjust x,y,facet variable, filenames, etc.
for(i in 1:length(groupnum)){
    CurrentGroup<-groupnum[i]
    subPlot<-subset(File1,File1$Groups == CurrentGroup)
    Plot1<-ggplot(data=subPlot,aes(x=Years,y=AllReleasesFreq))+stat_unique(geom = "col")+facet_wrap(~siteID+Speciesinformation,ncol=2,scales = "free")+FacetTheme
    ggsave(plot=Plot1,filename=paste0("PercRel_Facet","_",CurrentGroup,".png",sep=""),limitsize=FALSE,width=8,height=10,units="in",dpi=450)
    rm(subPlot)
    graphics.off()
  }
############################

all <- data.frame(NULL)
for (i in 1:length(Sites)){
  site <- subset(File1, File1$siteID == Sites[i])
  site <- subset(site, site$CoreID == as.character(unique(site$CoreID)[1]))
  all <- rbind(all, site)
}


plot2 <- ggplot(all, aes(x=AllReleasesFreq))+
  geom_histogram(stat = "bin", binwidth = 2)+
  theme_bw()+
  scale_x_continuous(breaks = seq(0,100,10))+
  scale_y_continuous(breaks = seq(0,5750,500))+
  xlab("Percent of Trees Showing Release in a Year")+
  ylab("Count")+
  ggtitle("Distribution of % Released Trees over all Sites, n ~ 21264")+
  geom_vline(xintercept = 18)
ggsave(plot=plot2,filename=paste0("PercRel_Distribution.png",sep=""),limitsize=FALSE,width=8,height=10,units="in",dpi=450)


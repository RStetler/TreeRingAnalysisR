#setwd("C://Users/Ruth/My Documents/R/ITRDB_Plots")
setwd("N://R_Files/All_ITRDB")
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)

site <- read.csv("ITRDB_SiteData.csv", header = TRUE)

dat <- read.csv("N://R_Files/All_ITRDB/ITRDB_DatabySite.csv", header = TRUE)
dat <- mutate(dat, distCat = ifelse(dat$percreleased >= 20, "L", NA))   ## create L=low category
dat$distCat <- factor(dat$distCat, levels = c("H", "M", "L"), ordered = TRUE)
dat <- transform(dat, siteID = as.character(siteID))
##############################
# finding sites with dist0
z <- data.frame(NULL)
for (i in 1:29){
  si <- ze[i]
  a <- subset(dat, siteID==si)
  z <- rbind(z, a)
}
###################################################
## create a logical vector (out of disturbance category)
dat <- mutate(dat, released = ifelse(is.na(dat$distCat), FALSE, TRUE))
dat <- data.frame(siteID=dat$siteID, year=dat$year, released=dat$released) #cut out extra columns
dat <- subset(dat, year >= 1400) # limit data to that after 1600
#years <- data.frame(year=1400:2004)
dat <- left_join(df, dat, by=c("year","siteID"))

siteNames <- as.character(site$siteID)
#####################################################
## for dividing rate and int by century
c16 <- subset(dat, year>=1600 & year<1700)
c17 <- subset(dat, year>=1700 & year<1800)
c18 <- subset(dat, year>=1800 & year<1900)
c19 <- subset(dat, year>=1900 & year<2000)
###################################################
siteNames <- unique(as.character(c19$siteID))
newcolrate <- vector('numeric')
newcolint <- vector('numeric')
for (i in 1:length(siteNames)){
  si <- siteNames[i]
  curr <- subset(c19, siteID==si)
  fir <- min(curr$year)
  las <- max(curr$year)
  totyr <- las-fir
  
  rate <- length(which(curr[ ,3]))/(totyr/100)
  newcolrate[i] <- rate
  
  
  int <- ifelse(length(which(curr[ ,3]))==0, 0, totyr/length(which(curr[ ,3])))
  newcolint[i] <- int
}
ne <- data.frame(cbind(siteNames, int=newcolint, rate=newcolrate))
write.csv(ne, "N://R_Files/All_ITRDB/ITRDB_Rate_c19.csv", row.names = FALSE)
################################################################################

new <- read.csv("N://R_Files/All_ITRDB/ITRDB_SiteData_Rate2.csv", header = TRUE)
mei <- mean(new$int)
mer <- mean(new$rate)
c16 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Rate_c16.csv", header = TRUE)
c17 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Rate_c17.csv", header = TRUE)
c18 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Rate_c18.csv", header = TRUE)
c19 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Rate_c19.csv", header = TRUE)
c17$Century <- rep("1700's", times=208)
c18$Century <- rep("1800's", times=256)
c19$Century <- rep("1900's", times=256)
al <- rbind(c17,c18,c19)
wilcox.test(c17$rate, c19$rate)
mer16 <- mean(c16$rate)
mer17 <- mean(c17$rate, na.rm = TRUE)
mer18 <- mean(c18$rate)
mer19 <- mean(c19$rate)
new$sp <- substr(new$species, 1, 2)

ggplot(new)+
  geom_density(data=new, aes(x=int, y=..count..), show.legend = FALSE, fill="deepskyblue4", alpha=0.5)+
  xlab("Return Interval (years)")+
  geom_vline(xintercept = 12.6, linetype="dotdash", size=1)+ geom_label(x=14, y=2.5, label="ga009", size = 5)+
  geom_vline(xintercept = 0, linetype="dotted", size=1)+ geom_label(x=2.5, y=1, label="X = 0 \r\n No Disturbance \r\n Detected", size=4)+
  geom_vline(xintercept = 347, linetype="longdash", size=1)+geom_label(x=345, y=2, label="tn008", size = 5)+
  geom_vline(xintercept = mei, linetype="solid", size=1)+geom_label(x=73, y=1.5, label="Mean = 73", size = 5)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_text(size=12),
        axis.title.y=element_text(size=12), axis.text.y=element_text(size=10),
        axis.text.x=element_text(size=10))
  ggsave(filename="N://R_Files/All_ITRDB/interval7.png",limitsize=FALSE,width=12,height=8,units="in",dpi=450)

spe <- subset(new, sp=="QU"|sp=="PI"|sp=="PC"|sp=="TS")
  ggplot(new)+
    #geom_density(data=c16, aes(x=rate, y=..count..), show.legend = TRUE, fill="green4", alpha=0.5)+
    geom_density(data=spe, aes(x=int, y=..count.., color=sp), show.legend = TRUE, size=1.5)+
    #geom_density(data=al, aes(x=rate, y=..count.., color=Century), show.legend = TRUE, size=1.5)+
    #geom_density(data=c18, aes(x=rate, y=..count..), show.legend = TRUE, color="indianred3", size=1)+
    #geom_density(data=c19, aes(x=rate, y=..count..), show.legend = TRUE, color="gold3", size=1)+
    #scale_color_manual(values=c("deepskyblue4", "indianred3", "gold3"), guide=guide_legend(override.aes = list(shape=95))) +
    xlab("Rate of Disturbance (events per century)")+
    labs(color="Species") +
    #geom_vline(xintercept = 0.29, linetype="longdash", size=1)+geom_label(x=0.3, y=30, label="tn008", size = 5)+
    #geom_vline(xintercept = 0, linetype="dotted", size=1)+ geom_label(x=0.05, y=15, label="X = 0 \r\n No Disturbance \r\n Detected", size=4)+
    #geom_vline(xintercept = 7.9, linetype="dotdash", size=1)+ geom_label(x=7.8, y=20, label="ga009", size = 5)+
    #geom_vline(xintercept = mer, linetype="solid", size=1)+geom_label(x=1.9, y=20, label="Mean = 1.9", size = 5)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.title.x=element_text(size=12),
          axis.title.y=element_text(size=12), axis.text.y=element_text(size=10),
          axis.text.x=element_text(size=10))
    ggsave(filename="N://R_Files/All_ITRDB/ratesbysp.png",limitsize=FALSE,width=12,height=8,units="in",dpi=450)
  
  
####################################################  

ggplot()+
  #geom_point(data=new, aes(x=latitude, y=rate, color="rate"))+
  #geom_dotplot(data=new, method="histodot", aes(x=rate, fill=factor(latitude)), show.legend=FALSE, 
    #binpositions = "all", stackgroups = TRUE, dotsize = 0.5, binwidth=0.25)+
  geom_point(data=new, aes(x=latitude, y=int, color="interval"))+
  theme_bw()+
  theme(panel.grid.major = element_blank())
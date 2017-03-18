setwd("N://R_Files/Thesis")
library(ggplot2)
library(tidyr)
library(gtable)
library(grid)
library(extrafont)

FacetTheme<-theme_bw()+theme(plot.title=element_text(size=15),axis.title.x=element_text(size=12),
                             axis.title.y=element_text(size=12), axis.text.y=element_text(size=9),
                             axis.text.x=element_text(size=9),strip.background=element_blank(),
                             panel.background=element_blank(),axis.line=element_line(color="black"))+theme(legend.key=element_blank())


cfxqu <- read.csv("N://R_Files/Thesis/CFXQU_CompareMethods.csv")
cfxli <- read.csv("N://R_Files/Thesis/CFXLI_CompareMethods.csv")
slli <- read.csv("N://R_Files/Thesis/SLWLI_CompareMethods.csv")
slqu <- read.csv("N://R_Files/Thesis/SLWQU_CompareMethods.csv")
QU <- rbind(cfxqu, slqu)
LI <- rbind(slli, cfxli)

plot1 <- ggplot(QU, aes(x=Years, y=AllReleasesFreq, color=Site))+
  geom_line()+
  theme_bw()+
  scale_x_continuous(breaks = seq(0,100,10))+
  scale_y_continuous(breaks = seq(0,5750,500))+
  xlab("Percent of Trees Showing Release in a Year")+
  ylab("Count")+
  ggtitle("Distribution of % Released Trees over all Sites, n ~ 21264")+
  
m1 <- mean(LI$bl_chnge, na.rm=TRUE)
m2 <- mean(LI$ga_chnge, na.rm=TRUE)
m3 <- mean(LI$X515_bl_chnge, na.rm=TRUE)
m4 <- mean(LI$X5_bl_chnge, na.rm=TRUE)
m5 <- mean(LI$X515_ga_chnge, na.rm=TRUE)
m6 <- mean(LI$X5_ga_chnge, na.rm=TRUE)

ggplot(QU)+
  geom_density(data=QU, aes(x=bl_chnge, color="bl10"))+
  geom_density(data=QU, aes(x=ga_chnge, color="ga10"))+
  geom_density(data=QU, aes(x=X515_bl_chnge, color="bl5/15"))+
  geom_density(data=QU, aes(x=X5_bl_chnge, color="bl5"))+
  geom_density(data=QU, aes(x=X515_ga_chnge, color="ga5/15"))+
  geom_density(data=QU, aes(x=X5_ga_chnge, color="ga5"))+
  scale_x_continuous(limits= c(-1,1))+
  facet_wrap(~Site,ncol=1)+
  FacetTheme+
  xlab("% Growth Change")+
  ylab("Density")+
  ggtitle("Distribution of PGC in Quercus")+
  geom_vline(xintercept = 0.2)+
  geom_vline(xintercept = 0.5)+
  geom_vline(LI, xintercept = m1, aes(color="bl10"))+
  ggsave(filename="N://R_Files/Thesis/QUPGCcomp.png",limitsize=FALSE,width=8,height=10,units="in",dpi=450)

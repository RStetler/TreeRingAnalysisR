setwd("C://Users/Ruth/Documents/WVU_MA_Things/Thesis/Summersville Project")
library(ggplot2)
library(tidyr)
library(gtable)
library(grid)
library(extrafont)

cfxqu <- read.csv("C://Users/Ruth/Documents/WVU_MA_Things/Thesis/Summersville Project/CFXQUM_defaultcrit.csv")
cfxli <- read.csv("C://Users/Ruth/Documents/WVU_MA_Things/Thesis/Summersville Project/CFXLIM_defaultcrit.csv")
slli <- read.csv("C://Users/Ruth/Documents/WVU_MA_Things/Thesis/Summersville Project/SLWLIM_defaultcrit.csv")
slqu <- read.csv("C://Users/Ruth/Documents/WVU_MA_Things/Thesis/Summersville Project/SLWQUM_defaultcrit.csv")
uniqqu <- data.frame(Site=cfxqu$Site, Species=cfxqu$Species, Years=cfxqu$Years, AllReleasesFreq=cfxqu$AllReleasesFreq, NumberofAllTrees=cfxqu$NumberOfAllTrees)
uniqli <- data.frame(Site=cfxli$Site, Species=cfxli$Species, Years=cfxli$Years, AllReleasesFreq=cfxli$AllReleasesFreq, NumberofAllTrees=cfxli$NumberOfAllTrees)
uniqslli <- data.frame(Site=slli$Site, Species=slli$Species, Years=slli$Years, AllReleasesFreq=slli$AllReleasesFreq, NumberofAllTrees=slli$NumberOfAllTrees)
uniqslqu <- data.frame(Site=slqu$Site, Species=slqu$Species, Years=slqu$Years, AllReleasesFreq=slqu$AllReleasesFreq, NumberofAllTrees=slqu$NumberOfAllTrees)

uniqli <- unique(uniqli)
uniqqu <- unique(uniqqu)
uniqslli <- unique(uniqslli)
uniqslqu <- unique(uniqslqu)

uniqAll <- rbind(uniqli, uniqqu, uniqslli, uniqslqu)
uniqAll <- transform(uniqAll, Species = as.character(Species))
uniqAll <- mutate(uniqAll, sp=ifelse(uniqAll$Species==c("QUAL","QUVE","QURU"), "QU", "LITU"))

allfreq <- ggplot(uniqAll, aes(x=Years, y=AllReleasesFreq))+
  geom_col()+
  theme_bw()+
  scale_x_continuous(limits = c(1750,2015))+
  scale_y_continuous(limits = c(0,50))+
  facet_wrap(~Site +sp, ncol=2)
allfreq
ggsave(plot=bl,filename="cfxqu_blchnge.png",limitsize=FALSE,width=10,height=8,units="in",dpi=450)


samplesize <- ggplot(uniqAll, aes(x=Years, y=NumberofAllTrees, color=Species))+
  geom_point()+
  geom_smooth()+
  theme_bw()+
  scale_x_continuous(limits = c(1750,2015))+
  facet_wrap(~Site, ncol=1)
samplesize

bl <- ggplot(cfxqu, aes(x=Years, y=bl_chnge*100, color=Canopy.Class))+
  geom_point()+
  theme_bw()+
  scale_y_continuous(limits=c(-200, 200))+
  geom_hline(yintercept = 20)+
  geom_hline(yintercept = 50)
bl

bl <- ggplot(cfxqu, aes(x=Years, y=bl_chnge*100, by=CoreID))+
  geom_line()+
  theme_bw()+
  scale_y_continuous(limits=c(-200, 200))
bl

den <- ggplot(cfxqu, aes(x=bl_chnge*100, fill=Canopy.Class))+
  geom_density(alpha=0.4)+
  scale_x_continuous(limits=c(-200, 200))+
  theme_bw()
den
  
#########
plot1 <- ggplot(uniq, aes(x=Years, y=AllReleasesFreq))+
  geom_col()+
  theme_bw()+
  scale_x_continuous(breaks = seq(0,100,10))+
  scale_y_continuous(breaks = seq(0,5750,500))+
  xlab("Percent of Trees Showing Release in a Year")+
  ylab("Count")+
  ggtitle("Distribution of % Released Trees over all Sites, n ~ 21264")+

  
p1 <- ggplot(uniqli, aes(x=Years, y=NumberofAllTrees))+
  geom_line(data=uniqli[!is.na(uniqli$NumberofAllTrees),], color="red", group=1)+
  scale_x_continuous(limits=c(1650, 2015))+
  theme_bw()

  p2 <- ggplot(uniq, aes(x=Years, y=NumberofAllTrees))+
    geom_line(data=uniq[!is.na(uniq$NumberofAllTrees),], color="blue", group=1)+
    scale_x_continuous(limits=c(1650, 2015))+
    theme_bw()
  
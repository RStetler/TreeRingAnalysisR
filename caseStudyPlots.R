setwd("N://R_Files/All_ITRDB")
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)

ggplot()+
  geom_line(data=tn, aes(y=GChange*100, x=Years, by=CoreID), color="grey20")+
  theme_bw()+
  ylab("Percent Growth Change")+
  geom_hline(yintercept = 25, linetype="dashed", size=1)
ggsave("N://R_Files/All_ITRDB/examplesitetn2.png",limitsize=FALSE,width=8,height=6,units="in",dpi=450)

setwd("N://R_Files/Thesis/derechodetect")
library(ggplot2)
library(tidyr)
library(dplR)
library(TRADER)
library(dplyr)
library(tibble)
library(reshape2)
library(readxl)

cf <- read.rwl("N://R_Files/Thesis/TreeMeans/CFXQU_mean.raw")

## with series mean ring widths
me <- colMeans(cf, na.rm = TRUE)

df <- list()
for (i in 1:length(me)){
  cop <- rep.int(me[i], times=10)
  df[[i]] <- cop
}

mydf <- data.frame(df)
colnames(mydf) <- colnames(cf)
ext <- rbind(cf, mydf)

ext <- rownames_to_column(ext, var = "years")
ext[362:371,1] <- 2016:2022
ext <- column_to_rownames(ext, var = "years")

#####  With random sampling of last 15 years
cf <- read.rwl("C://Users/Ruth/Documents/R/Summersville Project/CFXQU/CFXQU_mean.raw")
nco <- as.numeric(ncol(cf))
df <- list()
for (i in 1:nco){
  curr <- cf[[i]]
  n <- length(curr)
  l15 <- curr[(n-14):n]
  vr <- sample(l15, 7, replace = TRUE)
  df[[i]] <- vr
}

mydf <- data.frame(df)
colnames(mydf) <- colnames(cf)
ext <- rbind(cf, mydf)

ext <- rownames_to_column(ext, var = "years")
ext[362:368,1] <- 2016:2022
ext <- column_to_rownames(ext, var = "years")


growthAveragingALL(ext, name = "CFXQU_ext", releases=NULL,m1=10,m2=10, buffer=2, drawing=FALSE, 
                   criteria=0.25, criteria2=0.5, prefix="ga", gfun=mean, length=2, storedev=jpeg)


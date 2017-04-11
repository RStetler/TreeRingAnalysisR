rm(list = ls())
#setwd("N://R_Files/Thesis")
setwd("C://Users/Ruth/My Documents/WVU_MA_Things/Thesis/Summersville Project/")
library(dplR)
library(TRADER)
library(tidyr)
library(dplyr)
library(tibble)
library(reshape2)
library(readxl)

###############################################################
# Separate CFX from SLW
fileNames <- "N://R_Files/Thesis/SLWLI.txt"
## read file listing individual raw files
df <- readLines(fileNames)
dff <- gsub("(.*raw$)", "N://Measurements/\\1", df)

## create empty list
rawFiles <- list()

## add raw files to list
for (i in 1:length(df)) {
  current <- as.character(dff[i])
  currentRead <- read.rwl(current)
  rawFiles[[length(rawFiles)+1]] <- currentRead
}

## combine and save listed raw files into one file
fun <- combine.rwl(rawFiles, y=NULL)
write.rwl(rwl.df = fun, fname = "N://R_Files/Thesis/SLWLI.raw", format = "tucson", 
          long.names = TRUE, prec = 0.001)
######################################################################
## Compute tree means
da <- read.rwl("N://R_Files/Thesis/SLWQU.raw")
daIDs <- read.ids(da, stc = c(2, 5, 1))
treeMeans <- treeMean(da, daIDs)
plot(treeMeans, plot.type = "spag")
rwl.20 <- corr.rwl.seg(treeMeans, seg.length=40, pcrit=0.01)
mean(interseries.cor(treeMeans)[, 1])
write.rwl(rwl.df = treeMeans, fname = "N://R_Files/Thesis/SLWQU_mean.raw", format = "tucson", 
          long.names = TRUE, prec = 0.001)
##########################################################################
## Create flat file... works now!! (2/27)
rawFile <- read.rwl("N://R_Files/Thesis/TreeMeans/SLWLI_mean.raw")
noRownames <- rownames_to_column(rawFile, var = "Years")
rawFileMelt <- melt(noRownames, id.vars = "Years", variable.name = "CoreID", 
                    na.rm = TRUE, value.name = "RingWidths", factorsAsStrings = TRUE)
rawFileMelt <- transform(rawFileMelt, CoreID = gsub("([0-9]{5}$)", "SLW\\1", CoreID))
rawFileMelt <- transform(rawFileMelt, CoreID = gsub("(^[0-9]{4}$)", "SLW0\\1", CoreID))
rawFileMelt <- add_column(rawFileMelt, Site = rep("SL", nrow(rawFileMelt)))

meta <- read_excel("N://ThesisDataforR.xlsx", sheet = 4, col_names = TRUE, col_types = NULL, skip = 0)
meta <- data.frame(meta[,-(5:7)])
meta[[9]]=NULL
joinResults <- left_join(x = rawFileMelt, y = meta, by = c("CoreID" = "Core.ID"))
write.csv(joinResults, file = "N://R_Files/Thesis/TreeMeans/SLWLIM_flat.csv", row.names = FALSE)


currentDf <- cbind.data.frame(replicate, rawFileMelt)
outputDf <- rbind(outputDf, currentDf)


#######################################################################
## create flat file from TRADER results & incorporate into raw flat file

#### FOR _CHANGE.CSV FILES
dat <- read.table("C://Users/Ruth/My Documents/WVU_MA_Things/Thesis/Summersville Project/SLWQU_ga_change.csv", header = TRUE)
dat <- melt(dat, id.vars = "years", variable.name = "CoreID", 
                    na.rm = TRUE, value.name = "ga_chnge", factorsAsStrings = TRUE)
dat <- transform(dat, CoreID = gsub("X([0-9]{5}$)", "SLW\\1", CoreID))
dat <- transform(dat, CoreID = gsub("X([0-9]{4}$)", "SLW0\\1", CoreID))

rawdat <- read.csv("C://Users/Ruth/My Documents/WVU_MA_Things/Thesis/Summersville Project/SLWQUM_flat.csv", header = TRUE)
rawdat <- transform(rawdat, CoreID = as.character(CoreID))

dat2 <- left_join(x = dat3, y = dat, by = c("CoreID", "Years"="years"))

#### FOR _RELEASES_YEARS_TOTAL.CSV FILES
rm(dat)
dat <- read.table("C://Users/Ruth/My Documents/WVU_MA_Things/Thesis/Summersville Project/SLWQU_ga_releases_years_total.csv", header = TRUE)
colnames(dat) <- c("ga_AllReleasesYear",	"ga_AllReleasesFreq", "ga_NumberOfAllReleses", "ga_ModerateReleasesFreq", "ga_NumberOfModeraeReleases", "ga_NumberOfAllTrees")
dat3 <- left_join(x = dat2, y = dat, by = c("Years"="ga_AllReleasesYear"))

#### SAVE FINISHED FILE
write.csv(dat3, file = "C://Users/Ruth/My Documents/WVU_MA_Things/Thesis/Summersville Project/SLWQUM_defaultcrit.csv", row.names = FALSE)
############################################################################

## testing & analyzing
plot(rawFile, plot.type = "spag")
growthAveragingALL(rawFile, releases=NULL,m1=10,m2=10, buffer=2, drawing=FALSE, 
                   criteria=0.25, criteria2=0.5, prefix="ga", gfun=mean, length=2, storedev=jpeg)
change <- read.csv("N://R_Files/Thesis/ga_change.csv", header = TRUE)


####################################
library(httr)
url <- "ftp://ftp.ncdc.noaa.gov/pub/data/paleo/treering/measurements/northamerica/usa/"
####link to LITU crn from Scots Gap, TN  &  QUPR crn from Fernow, WV
download.file("https://www1.ncdc.noaa.gov/pub/data/paleo/treering/measurements/northamerica/usa/tn.rwl", destfile="N://R_Files/Thesis/tn.rwl")
download.file("https://www1.ncdc.noaa.gov/pub/data/paleo/treering/chronologies/northamerica/usa/wv003.crn", destfile="N://R_Files/Thesis/wv003.crn")

qu <- read.crn("N://R_Files/Thesis/wv003.crn", header = TRUE)
li <- read.rwl("N://R_Files/Thesis/tn.rwl", header = TRUE)
licrn <- chron(li, prefix = "tn")

## compare my crn's to standards above
cfxli <- read.rwl("N://R_Files/Thesis/TreeMeans/CFXLI_mean.raw")
cflicn <- chron(cfxli, prefix = "cfl")
slwli <- read.rwl("N://R_Files/Thesis/TreeMeans/SLWLI_mean.raw")
com2 <- combine.rwl(li, slwli)
stat <- corr.rwl.seg(com2, seg.length = 50, pcrit = 0.01)

## Explore data w/ correlation and spag plot
file <- "N://R_Files/Thesis/SLWLI.raw"
dat <- read.rwl(file)
dat.sum <- summary(dat)
plot(dat, plot.type = "spag")
rwl.20 <- corr.rwl.seg(dat, seg.length=20, pcrit=0.01)
mean(interseries.cor(dat)[, 1])

win <- 1900:2015
dat.yrs <- as.numeric(rownames(dat))
dat.trunc <- dat[dat.yrs %in% win, ]
plot(dat.trunc, plot.type = "spag")
abline(v=1984, col = "black")

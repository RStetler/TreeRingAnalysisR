rm(list = ls())
#setwd("N://R_Files/All_ITRDB")
setwd("C://Users/Ruth/My Documents/R/ITRDB")
library(httr)  ### seems like a lot of packages, but all are used somewhere...
library(dplR)
library(TRADER)
library(tidyr)
library(dplyr)
library(tibble)
library(reshape2)

url <- "ftp://ftp.ncdc.noaa.gov/pub/data/paleo/treering/measurements/northamerica/usa/"
url2 <- "ftp://ftp.ncdc.noaa.gov/pub/data/paleo/treering/measurements/correlation-stats/"
allFiles <- GET(url)   # equivalent of curl() function
getContent <- content(allFiles, "text") 

# separate & parse individual .rwl files
fileList <- unlist(strsplit(getContent, "-rw-rw-r--|lrwxrwxrwx"))
fileListFix <- unlist(strsplit(fileList, "->"))
fileNameFix_All <- gsub("^(.{36})", "", fileListFix)  
fileNamesSimple_All <- gsub("^.*?([a-z]{2}.*\\.rwl).+$", "\\1", fileNameFix_All)

# select state(s) of interest to download, create a vector of files
      ##other eastern states...wv|pa|ky |me|vt|nh|ma|ct|ri|ny|pa|nj|de|md|oh|va|mi|in|tn|nc|ga|al|ms|wi|il|sc|fl
fileList_IndexOfSelection <- grep("ky|wv", fileNamesSimple_All)
fileList_Selection <- fileNamesSimple_All[fileList_IndexOfSelection[1:length(fileList_IndexOfSelection)]]
txtFileList_Selection <- gsub("^([a-z]{2}.*)\\.rwl", "\\1\\.txt", fileList_Selection)

outputDf <- data.frame(NULL)
for(i in 1:length(fileList_Selection)) {
  # download selected files and associated metadata, read into dplr
  # the tryCatch function allows the loop to keep running after encountering
  #    an error. The problem file is skipped, and an error message printed
  tryCatch({
    #.rwl file details. Raw file download commented out because of corrected file list
    #individualFileName <- paste0("N://R_Files/All_ITRDB/", fileList_Selection[i]) 
    #download.file(paste0(url, fileList_Selection[i]), destfile=individualFileName)
    rawFile <- read.rwl(paste0(fileList_Selection[i]))
    noRownames <- rownames_to_column(rawFile, var = "Years")
    rawFileMelt <- melt(noRownames, id.vars = "Years", variable.name = "CoreID", 
                  na.rm = TRUE, value.name = "RingWidths", factorsAsStrings = TRUE)
    # metadata details
    metaFile <- as.character(txtFileList_Selection[i])
    download.file(paste0(url2, txtFileList_Selection[i]), destfile=metaFile)
    metadata <- readLines(txtFileList_Selection[i])

    indexOfSiteName <- grep("Site name", metadata)
    name <- metadata[indexOfSiteName]
    
    indexOfSiteLocation <- grep("Site location", metadata)
    location <- metadata[indexOfSiteLocation]

    indexOfSpecies <- grep("Species information", metadata)
    species <- metadata[indexOfSpecies]

    indexOfLatitude <- grep("Latitude", metadata)
    latitude <- metadata[indexOfLatitude]

    indexOfLongitude <- grep("Longitude", metadata)
    longitude <- metadata[indexOfLongitude]

    indexOfStartYr <- grep("Beginning year", metadata)
    indexOfEndYr <- grep("Ending year", metadata)
    startYr <- metadata[indexOfStartYr]
    endYr <- metadata[indexOfEndYr]

    indexOfNumSeries <- grep("Number dated series", metadata)
    numDatedSeries <- metadata[indexOfNumSeries]
    
    siteID <- paste0("siteID : ", gsub("^([a-z]{2}.*)\\.rwl", "\\1", fileList_Selection[i]))
    
    # clean up, Organize, and save all data
    metad <- c(siteID, name, location, species, latitude, longitude, startYr, endYr, numDatedSeries)
    metada <- as.data.frame(metad, stringsAsFactors = FALSE)
    metadat <- separate(data = metada, col = metad, into = c("variable", "value"), sep = ":", convert = TRUE)
    metadat$variable <- gsub("[[:blank:]]", "", metadat$variable)
    metadataTransform <- spread(metadat, variable, value)
    metadataTransform[1,3] <- gsub("(^.+)N", "\\1", metadataTransform[1,3])  ##fixing Lat (getting rid of "N")
    metadataTransform[1,4] <- gsub("(^.+)W", "-\\1", metadataTransform[1,4])   ##fixing long (getting rid of "W")
    metadataTransform[1,4] <- gsub("[[:blank:]]", "", metadataTransform[1,4])
    state <- gsub("([a-z]{2}).*", "\\1", metadataTransform[1,6])
    metadataTransform[1,10] <- state
    replicate <- metadataTransform[rep(seq_len(1), nrow(rawFileMelt)), ]
    replicate <- transform(replicate, Beginningyear= as.numeric(Beginningyear),
                           Endingyear = as.numeric(Endingyear),
                           Latitude = as.numeric(Latitude),
                           Longitude = as.numeric(Longitude),
                           Numberdatedseries = as.numeric(Numberdatedseries))
    colnames(replicate)[10] <- "state"
    currentDf <- cbind.data.frame(replicate, rawFileMelt)
    outputDf <- rbind(outputDf, currentDf)
  
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

write.csv(outputDf, file = "N://R_Files/All_ITRDB/ITRDB_Data_EastUSA.csv", row.names = FALSE)
ITRDBdata <- read.csv("N://R_Files/Test3/ITRDB_Data.csv", header = TRUE)

############################################################
#  Fixing sample size/yr issue. Using raw files and rwl.stats() function call in dplR

###  First step makes a dataframe with siteID and all years in which that site is recording
df <- data.frame(NULL)
for (i in 1:length[fileList_Selection]){
  current <- read.rwl(fileList_Selection[i])
  siteID <- gsub("^([a-z]{2}.*)\\.rwl", "\\1", fileList_Selection[i])
  stat <- rwl.stats(current)  # this function produces a dataframe with stats for each series in a raw file
  first <- min(stat$first)
  last <- max(stat$last)
  len <- first:last
  siteIDvec <- rep(siteID, times=length(len))
  da <- data.frame(year=len, siteID=siteIDvec)
  df <- rbind(df, da)
}


##  Next, making a simple timeseries dataframe, with years, and n= number of sites recording in each year
early <- min(df$year)
late <- max(df$year)
range <- early:late   #a vector with all the years our data represents


timser <- data.frame(NULL)
for (i in 1:length(range)){
  al <- df$year
  n <- length(which(al==range[i]))  
  d <- data.frame(year=range[i], NumSitesRec=n)
  timser <- rbind(timser, d)   ## timser is the sample depth (of sites) time series
}

#old <- read.csv("C://Users/Ruth/My Documents/R/ITRDB_Data/ITRDB_DatabySite.csv", header=TRUE)
#fix <- left_join(yrs, old, by=c("year", "siteID"))

##################################################################################
# Edited version of TRADER, lines 41-72
growthAveragingALL<- function(data, name, releases=NULL,m1=10,m2=10, buffer=2, drawing=TRUE,
                              criteria=0.25, criteria2=0.5, prefix="ga",gfun=mean,
                              length=2, storedev=jpeg, ...) {
  if ( is.null(releases) )
    releases<-noblabrams(data,m1=m1,m2=m2,buffer=buffer,length=length,
                         criteria=criteria,criteria2=criteria2,black=FALSE,gfun=gfun)
  
  #figures
  if (drawing){
    for(i in 1:length(data)) {
      storedev(paste(prefix,"_",gsub("/","_",names(data)[i]),".",deparse(substitute(storedev)),sep=""))
      plotRelease(data,releases$change,releases, i, method="NowackiAbrams",
                  addHLines=c(criteria,criteria2),...)
      dev.off()
    }
  }
  #tables
  releases$onlyModerate$year=rownames(releases$onlyModerate)
  releases$onlyModerate = releases$onlyModerate[,c(ncol(releases$onlyModerate),1:(-1+ncol(releases$onlyModerate)))]
  releases$onlyMajor$year=rownames(releases$onlyMajor)
  releases$onlyMajor = releases$onlyMajor[,c(ncol(releases$onlyMajor),1:(-1+ncol(releases$onlyMajor)))]
  
  
  #write.table ( releases$change, paste(name, "_", prefix,"_change.csv", sep = ""), append = TRUE, sep="\t",row.names=F)
  #write.table ( releases$releases, paste(name, "_", prefix,"_releases_tops.csv", sep = ""), append = TRUE, sep="\t",row.names=F)
  #write.table ( releases$all_releases, paste(name, "_", prefix,"_releases_all.csv", sep = ""), append = TRUE, sep="\t",row.names=F)
  write.table ( releases$years_list_total, paste(name, "_", prefix,"_releases_years_total.csv", sep = ""),append = TRUE, row.names=F,sep="\t")
  #write.table ( releases$pgc, paste(name, "_", prefix,"_releases_values_total.csv", sep = ""), row.names=F, append = TRUE, sep="\t")
  write.table ( releases$onlyMajor, paste(name, "_", prefix,"_releases_Only_Major.csv", sep = ""), row.names=F,append = TRUE, sep="\t")
  write.table ( releases$onlyModerate, paste(name, "_", prefix,"_releases_Only_Moderate.csv", sep = ""), row.names=F, append = TRUE, sep="\t")
  #return(releases)
}

for(i in 1:length(fileList_Selection)){
  tryCatch({
    currentFile <- read.rwl(fileList_Selection[i])
    growthAveragingALL(currentFile, name = fileList_Selection[i], releases=NULL,m1=10,m2=10, buffer=2, drawing=FALSE, 
                     criteria=0.25, criteria2=0.5, prefix="ga", gfun=mean, length=2, storedev=jpeg)
    results <- read.csv(paste0(fileList_Selection[i], "_ga_releases_years_total.csv", header = TRUE))
    site <- gsub("(.*)\\.raw", "\\1", fileList_Selection[i])
    joinResults <- join(subset(dat, siteID == site, results, type = "left", by = c("Years" = "AllReleasesYear")))
    allResults <- rbind(allResults, joinResults)
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
write.csv(allResults, file = "N://R_Files/All_ITRDB/ITRDB_Data_W_TRADER.csv", row.names = FALSE)
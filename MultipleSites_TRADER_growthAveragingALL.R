rm(list = ls()) #removes everything in your workspace
setwd("C://Users/Ruth/My Documents/R/Thesis")
library(httr)
library(dplR)
library(TRADER)
library(curl)
library(tidyr)

url <- "ftp://ftp.ncdc.noaa.gov/pub/data/paleo/treering/measurements/northamerica/usa/"
# open index of all .rwl files from url above
allFiles <- GET(url)
# access content of page
getContent <- content(allFiles, "text") 

# separate individual entries
fileList <- unlist(strsplit(getContent, "-rw-rw-r--|lrwxrwxrwx"))
fileListFix <- unlist(strsplit(fileList, "->"))

# remove extra text and gibbrish, leaving just file names
# results in a list of all .rwl files currently available on ITRDB for USA
fileNameFix_All <- gsub("^(.{36})", "", fileListFix)  
fileNamesSimple_All <- gsub("^.*?([a-z]{2}.*\\.rwl).+$", "\\1", fileNameFix_All)


# select state(s) of interest to download, create a list of files
        ##|me|vt|nh|ma|ct|ri|ny|pa|nj|de|md|oh|va|mi|in|tn|nc|ga|al|ms|wi|il|sc|fl
fileList_IndexOfSelection <- grep("wv|pa|ky", fileNamesSimple_All)
fileList_Selection <- fileNamesSimple_All[fileList_IndexOfSelection[1:length(fileList_IndexOfSelection)]]


# loop to download files in list
for(i in 1:length(fileList_Selection)) {
  # download selected files, read into dplr
  # the tryCatch function allows the loop to keep running after encountering
  #    an error. The problem file is skipped, and an error message printed
  tryCatch({
    individualFileName <- as.character(fileList_Selection[i]) 
    download.file(paste0(url, fileList_Selection[i]), destfile=individualFileName)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


# Begin download of metadata files to pull site info into a list
txtFileList_Selection <- gsub("^([a-z]{2}.*)\\.rwl", "\\1\\.txt", fileList_Selection)
url2 <- "ftp://ftp.ncdc.noaa.gov/pub/data/paleo/treering/measurements/correlation-stats/"

for(i in 1:length(txtFileList_Selection)) {
  destFile <- as.character(txtFileList_Selection[i])
  download.file(paste0(url2, txtFileList_Selection[i]), destfile=destFile)
}


# Extract relevent metadata details
for(i in 1:3) {
  # read in metadata file
  metadata <- readLines(txtFileList_Selection[i])
  
  # find site name
  indexOfSiteName <- grep("Site name", metadata)
  name <- metadata[indexOfSiteName]
  
  # find site location
  indexOfSiteLocation <- grep("Site location", metadata)
  location <- metadata[indexOfSiteLocation]
  
  # find species
  indexOfSpecies <- grep("Species information", metadata)
  species <- metadata[indexOfSpecies]
  
  # find latitude
  indexOfLatitude <- grep("Latitude", metadata)
  latitude <- metadata[indexOfLatitude]
  
  # find longitude
  indexOfLongitude <- grep("Longitude", metadata)
  longitude <- metadata[indexOfLongitude]
  
  # find years of chronology
  indexOfStartYr <- grep("Beginning year", metadata)
  indexOfEndYr <- grep("Ending year", metadata)
  startYr <- metadata[indexOfStartYr]
  endYr <- metadata[indexOfEndYr]
  
  # find number of series in chronology
  indexOfNumSeries <- grep("Number dated series", metadata)
  numDatedSeries <- metadata[indexOfNumSeries]
  
  # .rwl file
  #rwl <- fileList_Selection[i]
  
  # combine into list (and write to a csv???)
  listName <- paste0(gsub("^([a-z]{2}.*)\\.rwl", "\\1\\", fileList_Selection[i]), "_Metadata.csv")
  metaList <- c(name, location, species, latitude, longitude, startYr, endYr, numDatedSeries)
  meta2 <- as.data.frame(metaList, stringsAsFactors = default.stringsAsFactors())
  meta3 <- separate(meta2, metaList, into = c("variable", "value"), sep = ":")
  write.csv(meta3, file=listName)
}


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
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

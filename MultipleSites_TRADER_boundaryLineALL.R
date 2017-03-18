rm(list = ls())
setwd("N://R_Files/Test2")
library(httr)
library(dplR)
library(TRADER)

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
fileList_IndexOfSelection <- grep("pa|md", fileNamesSimple_All)
fileList_Selection <- fileNamesSimple_All[fileList_IndexOfSelection[1:length(fileList_IndexOfSelection)]]


# loop to download files in list
for(i in 1:length(fileList_Selection)) {
# download selected files, read into dplr
# the tryCatch function allows the loop to keep running after encountering
#    an error. The problem file is skipped, and an error message printed
  tryCatch({
    individualFileName <- as.character(fileList_Selection[i]) 
    download.file(paste0(url, fileList_Selection[i]), destfile=individualFileName)
    read.rwl(individualFileName)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


# Begin download of metadata files to pull site info into a list
txtFileList_Selection <- gsub("^([a-z]{2}.*)\\.rwl", "\\1\\.txt", fileList_Selection)
url2 <- "ftp://ftp.ncdc.noaa.gov/pub/data/paleo/treering/measurements/correlation-stats/"


############################################

# Edited version of boundary line function call, lines 79-113
boundaryLineALL<- function(data,name, releases=NULL,m1=10,m2=10, boundary=NULL, buffer=2, 
                           criteria=0.2, criteria2=0.5, segment=0.5, segment2=0.5,
                           prefix="bl", drawing=TRUE,gfun=mean,length=2, 
                           notop=10,notop2=10,storedev=jpeg,...) {
  if ( is.null(releases) )
    releases<-noblabrams(data,m1=m1,m2=m2,boundary=boundary,buffer=buffer,gfun=gfun,length=length,
                         criteria=criteria,criteria2=criteria2,segment=segment,
                         segment2=segment2,storedev=storedev,notop=notop,notop2=notop2,black=TRUE)
  
 # figures (from TRADER code)
  if (drawing){
    for(i in 1:length(data)) {
      #print(paste(i))
      storedev(paste(prefix,"_",gsub("/","_",names(data)[i]),".",deparse(substitute(storedev)),sep=""))
      plotRelease(data,releases$change,releases, i, method="BlackAbrams",
                  addHLines=c(criteria, criteria2))
      dev.off()
    }
  }
  #tables (from TRADER code). Unwanted tables commented out.
  releases$onlyModerate$year=rownames(releases$onlyModerate)
  releases$onlyModerate = releases$onlyModerate[,c(ncol(releases$onlyModerate),1:(-1+ncol(releases$onlyModerate)))]
  releases$onlyMajor$year=rownames(releases$onlyMajor)
  releases$onlyMajor = releases$onlyMajor[,c(ncol(releases$onlyMajor),1:(-1+ncol(releases$onlyMajor)))]
  
  write.table ( releases$change, paste(name, "_", prefix,"_change.csv", sep = ""), sep="\t",row.names=F)
  #write.table ( releases$releases, paste(prefix,"_releases_tops.csv", sep = ""), sep="\t",row.names=F)
  #write.table ( releases$all_releases, paste(name, "_", prefix,"_releases_all.csv", sep = ""), sep="\t",row.names=F)
  write.table ( releases$years_list_total, paste(name, "_", prefix,"_releases_years_total.csv", sep = ""), row.names=F,sep="\t")
  #write.table ( releases$pgc, paste(name, "_", prefix,"_releases_values_total.csv", sep = ""), row.names=F,sep="\t")
  write.table ( releases$onlyMajor, paste(name, "_", prefix,"_releases_Only_Major.csv", sep = ""), row.names=F,sep="\t")
  write.table ( releases$onlyModerate, paste(name, "_", prefix,"_releases_Only_Moderate.csv", sep = ""), row.names=F,sep="\t")
  #return(releases)
}
##################################
# run TRADER on list of selected files
for(i in 1:length(fileList_Selection)){
  tryCatch({
    currentFile <- read.rwl(fileList_Selection[i])
    boundaryLineALL(currentFile, name = fileList_Selection[i], releases=NULL, m1 = 10, m2 = 10, boundary = NULL, buffer = 2, criteria=0.2, 
                  criteria2=0.5, segment=0.5, segment2=0.5, prefix="bl", gfun = mean, length = 2, notop = 10, notop2 = 10, storedev = jpeg, 
                  drawing=FALSE)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}  
##########################
dat <- read.rwl("N://R_Files/Thesis/TreeMeans/CFXLI_mean.raw")
setwd("N://R_Files/Thesis/TreeMeans")


boundaryLineALL(dat, name = "CFXLI", releases=NULL, m1 = 10, m2 = 10, boundary = NULL, buffer = 2, criteria=0.2, 
                criteria2=0.5, segment=0.5, segment2=0.5, prefix="bl", gfun = mean, length = 2, notop = 10, notop2 = 10, storedev = jpeg, 
                drawing=FALSE)

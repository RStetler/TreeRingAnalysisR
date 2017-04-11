rm(list = ls())
setwd("C://Users/Ruth/My Documents/R/ITRDB_Plots")
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyr)


all <- read.csv("C://Users/Ruth/My Documents/R/ITRDB_Data/ITRDB_DataforPlots.csv", header = TRUE)
dat <- read.csv("C://Users/Ruth/My Documents/R/ITRDB_Data/ITRDB_DatabySite.csv", header = TRUE)

plotdata <- data.frame(siteID=dat$siteID, year=dat$Years, percreleased=dat$AllReleasesFreq)
plot <- data.frame(unique(plotdata))
pl <- left_join(plot, meta, by ="siteID")
write.csv(pl, "C://Users/Ruth/My Documents/WVU_MA_Things/Thesis/ITRDB Project/ITRDB_DatabySite.csv")

# Creating disturbance category column
dat <- mutate(dat, distCat = ifelse(dat$percreleased >= 20, "L", NA))   ## create L=low category
for (i in 1:length(dat$percreleased)){
  if (dat$percreleased[i] >= 50){
    dat$distCat[i] = "M"      ## add M=medium category
  } else {
    NULL
  }
}
for (i in 1:length(dat$percreleased)){
  if (dat$percreleased[i] >= 70){
    dat$distCat[i] = "H"    ## add H=high category
  } else {
    NULL
  }
}

# making disturbance category a factor with ordered levels
###  H = major
###  M = moderate
# sub <- subset(dat, state == "wv" | state == "tn" |state == "oh" |state == "me")
dat$distCat <- factor(dat$distCat, levels = c("H", "M", "L"), ordered = TRUE)
dat <- transform(dat, siteID = as.character(siteID))

## create a logical vector (out of disturbance category) for calculating moving average
dat <- mutate(dat, released = ifelse(is.na(dat$distCat), FALSE, TRUE))


spre <- data.frame(siteID=dat$siteID, year=dat$year, released=dat$released) #cut out extra columns
spre <- subset(spre, year > 1600) # limit data to that after 1600
spre <- spread(spre, key = year, value = released)  # this makes the years into columns, with rows corresponding to
                                                    #    site. columns show T/F, T= more than 20% series showed release

# this loop counts the number of TRUE's for each year (ie, how many sites show a release in a given year)
new <- vector('numeric')
for (i in 2:ncol(spre)){
  x <- length(which(spre[ ,i]))
  new[i] <- x
}

# create a data frame for the above yearly counts
years <- 1601:2004
ave <- data.frame(years=years, rawrel=new) #raw counts per year
aver <- movingAverage(ave$rawrel, n=3, centered = TRUE) #calculate moving average on raw counts
                                                        #   this function is not in base R, I found it online. can share the code
ave <- data.frame(years=years, rawrel=new, movAve3=aver) # update df

dat2 <- subset(dat, year >= 1600 & year <=2005)
write.csv(dat2, "C://Users/Ruth/My Documents/WVU_MA_Things/Thesis/ITRDB Project/ITRDB_DatabySitePost1600.csv")
# plot
ggplot(dat2, aes(x=year, y=siteID)) + 
  geom_point(aes(color=distCat), shape="|", size=2) + 
  scale_colour_manual(values=c("red", "green", "blue", "black")) +
  scale_x_continuous(breaks=seq(from = 1601, to = 2005, by = 25)) +
  geom_segment(aes(x=startyr, xend= endyr, y=siteID, yend=siteID))+
  geom_line(data=ave, aes(x=years, y=movAve3))+
  #layer(geom = "line", data = ave, aes(x=years, y=movAve3), position = "fill", show.legend = NA)+
  theme_bw()


ave <- transform(ave, years=as.numeric(years))
ave <- transform(ave, movAve3=as.numeric(movAve3))
ggplot()+
  geom_line(data=ave, aes(x=years, y=movAve3))+
  scale_x_continuous(breaks=seq(from = 1601, to = 2005, by = 25))

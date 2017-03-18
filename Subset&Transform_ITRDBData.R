rm(list = ls())
setwd("N://R_Files/All_ITRDB")
library(httr)  ### seems like a lot of packages, but all are used somewhere...
library(dplR)
library(TRADER)
library(tidyr)
library(dplyr)
library(tibble)
library(reshape2)

dat <- read.csv("N://R_Files/All_ITRDB/ITRDB_Data_Canada.csv", header = TRUE)

dat <- transform(dat, siteID = as.character(siteID),
                    Sitename = as.character(Sitename),
                    Sitelocation = as.character(Sitelocation),
                    Speciesinformation = as.character(Speciesinformation),
                    state = as.character(state))

dat <- transform(dat, Speciesinformation = gsub("([A-Z]{4}).*", "\\1", Speciesinformation))
dat <- transform(dat, Speciesinformation = gsub("[[:blank:]]", "", Speciesinformation))
dat <- transform(dat, siteID = gsub("[[:blank:]]", "", siteID))
dat <- transform(dat, state = gsub("[[:blank:]]", "", state))

write.csv(dat, file = "N://R_Files/All_ITRDB/ITRDB_Data_Canada_Clean.csv", row.names = FALSE)

dat1 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Data_W_TRADER_50.csv", header = TRUE)
dat2 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Data_W_TRADER_100.csv", header = TRUE)
dat3 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Data_W_TRADER_150.csv", header = TRUE)
dat4 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Data_W_TRADER_200.csv", header = TRUE)
dat5 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Data_W_TRADER_280.csv", header = TRUE)
dat6 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Data_W_TRADER_350.csv", header = TRUE)
dat7 <- read.csv("N://R_Files/All_ITRDB/ITRDB_Data_W_TRADER_413.csv", header = TRUE)

com1 <- rbind(dat1, dat2, dat3)
com2 <- rbind(com1, dat4, dat5, dat6, dat7)
write.csv(com2, file = "N://R_Files/All_ITRDB/ITRDB_Data_W_TRADER_ALL.csv", row.names = FALSE)


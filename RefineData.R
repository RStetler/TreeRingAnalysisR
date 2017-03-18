rm(list = ls()) #removes everything in your workspace
setwd("C://Users/Ruth/My Documents/R/Plots")
library(dplyr)
library(dplR)
library(ggplot2)
library(tibble)

#clean up data
years <- data.frame(yr = 1600:2010)
distFile <- read.table("C://Users/Ruth/My Documents/R/Plots/ky002.rwl_ga_releases_years_total.csv", header = TRUE)
dist_AllYears <- full_join(years, distFile, by = c("yr" = "AllReleasesYear"))

#plot a single site: years x % series showing release
plot <- ggplot(dist_AllYears, aes(x = dist_AllYears$yr, y = dist_AllYears$AllReleasesFreq)) +
  geom_col(aes(x = dist_AllYears$yr, y = dist_AllYears$AllReleasesFreq), color = "black")
plot

#plot a single site: years x simple tally of releases
maj <- read.table("C://Users/Ruth/My Documents/R/Plots/ky002.rwl_ga_releases_Only_Major.csv", header = TRUE)
mod <- read.table("C://Users/Ruth/My Documents/R/Plots/ky002.rwl_ga_releases_Only_Moderate.csv", header = TRUE)
row.names(maj) <- maj$year
row.names(mod) <- mod$year
maj$year <- NULL
mod$year <- NULL 
majTr <- t(maj)
modTr <- t(mod)
majTranspose <- as.data.frame(majTr)
modTranspose <- as.data.frame(modTr)
tallyMaj <- summarise_all(majTranspose, funs(sum(., na.rm = TRUE)))
tallyMod <- summarise_all(modTranspose, funs(sum(., na.rm = TRUE)))
all <- bind_rows(tallyMaj, tallyMod)
tallyAll <- summarise_all(all, funs(sum(., na.rm = TRUE)))
row.names(tallyAll) <- fileList_Selection[i]
finalTally <- as.data.frame(t(tallyAll))
finalTally <- rownames_to_column(finalTally, var = "year")
logTally <- mutate(finalTally, if_else(finalTally$V1 > 0, TRUE, FALSE))
disturbanceEvents <- mutate(logTally, if_else(logTally$`if_else(finalTally$V1 > 0, TRUE, FALSE)` == TRUE, 1, 0))

plot <- ggplot(finalTally) +
  geom_point(x = finalTally$V1, y = finalTally$year)
plot


## convert to a binary plot? (yes/no disturbance per year)
plot <- plot(finalTally$year, finalTally$V1)

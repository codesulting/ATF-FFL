# ATF - FFL
# US Census Electoral Profile Data
# http://www.census.gov/data/tables/time-series/demo/voting-and-registration/electorate-profiles-2016.html
# 2014: http://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-577.html
# 2012: http://www.census.gov/data/tables/2012/demo/voting-and-registration/p20-568.html

# "Selected Characteristics of the Citizen, 18 years and older"
# .xls file with cumulative US estimates on electorate by race.01

# "Voting and Registration in the Election of November 2014"
# two .xls files:
# of note is "table04b.xls", which contains:
# "Reported Voting and Registration by Sex, race.01 and Hispanic Origin, for States: November 2014 [<1.0 MB]"

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(zoo)

# Raw Data:
# Table 4b: Electorate by Sex, race.01, and Hispanic Origin
race.01 <- read.csv("~/GitHub/ATF-FFL/data/01-census/2014-electorate/2014-table4b-race.csv")
str(race.01)

# Cleanse: --------------------------------------------------------------------
# convert blank to NA; carry last observation forward
race.01$State[race.01$State == ""] <- NA
race.01$State <- na.locf(race.01$State)

# remove commas - columns 3:5, 10
# replace dashes with zero
# replace '(B)' with NA
race.01 <- as.data.frame(apply(race.01, 2, function(x) gsub(",", "", x)))
race.01 <- as.data.frame(apply(race.01, 2, function(x) gsub("-", 0, x)))
race.01 <- as.data.frame(apply(race.01, 2, function(x) gsub("\\(B\\)", NA, x)))

# rename column and levels of race.and.Hispanic.origin
colnames(race.01)[2] <- "Race"
levels(race.01$Race)[levels(race.01$Race) == ".White non0Hispanic alone"] <- "White non-Hispanic alone"
race.01$Race <- factor(race.01$Race)
levels(race.01$Race)

# rename 'Margin of Error' variables to void confusion
colnames(race.01)[c(7, 9, 12, 14)] <- c("MarginError1", "MarginError2", "MarginError3", "MarginError4")

# rename levels of race.01
levels(race.01$Race)[1:11]
levels(race.01$Race)[1:11] <- c("White.NonHispanic", "Asian", "Asian.combination", "Black", "Black.combination",
                             "Female", "Hispanic", "Male", "Total", "White", "White.combination")

race.01$Race <- factor(race.01$Race)
levels(race.01$Race)

write.csv(race.01, file = "~/GitHub/ATF-FFL/data/01-census/2014-electorate/V1-2014-table4b-race.01.csv",
           row.names = F)

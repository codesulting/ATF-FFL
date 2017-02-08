# ATF - FFL
# US Census Electoral Profile Data
# http://www.census.gov/data/tables/time-series/demo/voting-and-registration/electorate-profiles-2016.html
# 2014: http://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-577.html
# 2012: http://www.census.gov/data/tables/2012/demo/voting-and-registration/p20-568.html



# "Selected Characteristics of the Citizen, 18 years and older"
# .xls file with cumulative US estimates on electorate by race

# "Voting and Registration in the Election of November 2014"
# two .xls files:
# of note is "table04b.xls", which contains:
# "Reported Voting and Registration by Sex, Race and Hispanic Origin, for States: November 2014 [<1.0 MB]"


# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(corrplot)
library(zoo)

# Table 4b: Electorate by Sex, Race, and Hispanic Origin
race <- read.csv("~/Documents/ATF-FFL/data/census/2014-electorate/2014-table4b-race.csv")
str(race)

# convert blank to NA; carry last observation forward
race$State[race$State == ""] <- NA
race$State <- na.locf(race$State)

# remove commas - columns 3:5, 10
race <- as.data.frame(apply(race, 2, function(x) gsub(",", "", x)))

# replace dashes with zero
race <- as.data.frame(apply(race, 2, function(x) gsub("-", 0, x)))

# replace '(B)' with NA
race <- as.data.frame(apply(race, 2, function(x) gsub("\\(B\\)", NA, x)))





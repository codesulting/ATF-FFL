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
library(ggplot2)
library(corrplot)
library(zoo)

# Cleansed Data:
# Table 4b: Electorate by Sex, Race, and Hispanic Origin
# skip past 'Cleanse' to 'Explore'
race <- read.csv("~/Documents/ATF-FFL/data/census/2014-electorate/V1-2014-table4b-race.csv")
str(race)

# Raw Data:
# Table 4b: Electorate by Sex, Race, and Hispanic Origin
race <- read.csv("~/Documents/ATF-FFL/data/census/2014-electorate/2014-table4b-race.csv")
str(race)

# Cleanse: --------------------------------------------------------------------
# convert blank to NA; carry last observation forward
race$State[race$State == ""] <- NA
race$State <- na.locf(race$State)

# remove commas - columns 3:5, 10
# replace dashes with zero
# replace '(B)' with NA
race <- as.data.frame(apply(race, 2, function(x) gsub(",", "", x)))
race <- as.data.frame(apply(race, 2, function(x) gsub("-", 0, x)))
race <- as.data.frame(apply(race, 2, function(x) gsub("\\(B\\)", NA, x)))

# convert columns 3:14 to numeric
for (i in 3:ncol(race)) {
  race[, i] <- as.numeric(race[, i])
}

# rename column and levels of Race.and.Hispanic.origin
colnames(race)[2] <- "Race"
levels(race$Race)[levels(race$Race) == ".White non0Hispanic alone"] <- "White non-Hispanic alone"
race$Race <- factor(race$Race)
levels(race$Race)

# rename 'Margin of Error' variables to void confusion
colnames(race)[c(7, 9, 12, 14)] <- c("MarginError1", "MarginError2", "MarginError3", "MarginError4")

# rename levels of Race
levels(race$Race)[1:11] <- c("Asian", "Asian.combination", "Black", "Black.combination",
                             "Female", "Hispanic", "Male", "Total", "White",
                             "White.combination", "White.NonHispanic")

race$Race <- factor(race$Race)
levels(race$Race)

write.csv(race, file = "~/Documents/ATF-FFL/data/census/2014-electorate/V1-2014-table4b-race.csv",
           row.names = F)

# Explore: --------------------------------------------------------------------

# correlation matrix on numeric variables
# subset numerics
race.cor <- race %>%
  select(-State, -Race)

# create correlation matrix
race.cor <- cor(race.cor)

# define a new palette
race.pal <- colorRampPalette(c("deeppink4",
                              "deeppink2",
                              "antiquewhite1",
                              "white",
                              "antiquewhite1",
                              "deepskyblue2",
                              "deepskyblue4"))

# plot correlation matrix
corrplot(race.cor, method = "shade", shade.col = NA, col = race.pal(100),
         tl.col = "gray23", tl.srt = 45, tl.cex = 0.75, na.label.col = "black",
         addCoef.col = "black", number.cex = 0.5, mar = c(2, 2, 2, 2))

# There are NA values in nearly every variable - 
# likely because of values too small to be counted (< 75000 as per US Census)

# Is it worth seeing which groups are under-represented in each state? 
# To do so would require seeing which rows have the most NAs, 
# and checking the State and Race variables.

which(is.na(race), arr.ind=TRUE)
# quite a long index there.

colnames(race)

ggplot(race, aes(reorder(State, Percent.voted..Total.), Percent.voted..Total.)) +
  geom_bar(stat = "identity") +
  coord_flip()









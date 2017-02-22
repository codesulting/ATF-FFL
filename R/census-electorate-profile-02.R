# ATF - FFL
# US Census Electoral Profile Data
# http://www.census.gov/data/tables/time-series/demo/voting-and-registration/electorate-profiles-2016.html
# 2014: http://www.census.gov/data/tables/time-series/demo/voting-and-registration/p20-577.html
# 2012: http://www.census.gov/data/tables/2012/demo/voting-and-registration/p20-568.html

# "Selected Characteristics of the Citizen, 18 years and older"
# .xls file with cumulative US estimates on electorate by race

# Totals are reported in thousands

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
race <- read.csv("~/GitHub/ATF-FFL/data/01-census/2014-electorate/V1-2014-table4b-race.01.csv",
                 stringsAsFactors = F)
str(race)

# Explore: --------------------------------------------------------------------

ggplot(race, aes(reorder(State, Percent.voted..Total.), Percent.voted..Total.)) +
  geom_bar(stat = "identity") +
  coord_flip()

# Cleanse ---------------------------------------------------------------------

# The Race Column might need to be spread.
race$Race <- factor(race$Race)
levels(as.factor(race$Race))

# state populations by voters
race.voting <- race %>%
  select(State, Race, Total.voted) %>%
  spread(key = Race, value = Total.voted)

# percentage by race is of total population of race, not total population
race.percent.voting <- race %>%
  select(State, Race, Percent.voted..Total.) %>%
  spread(key = Race, value = Percent.voted..Total.)

239874*1000
# 239,874,000 accounts for 239.8 million for total US population.

# state populations by race
race.population <- race %>%
  select(State, Race, Total.Population) %>%
  spread(key = Race, value = Total.Population, fill = 0)

race.population <- race.population[, c(1, 8, 6, 9, 2, 3, 4, 5, 7, 10:12)]

write.csv(race.population, file = "~/GitHub/ATF-FFL/data/01-census/2014-electorate/V1-race-pop.csv",
          row.names = F)

# Cleanse: ACS Industry data --------------------------------------------------

industry <- read.csv("~/GitHub/ATF-FFL/data/01-census/ACS_15_1YR_industry/ACS_15_1YR_S2407.csv")

# select total estimate data for each industry
# for each industry there is finer grain data
industry <- industry %>%
  select(GEO.id2, GEO.display.label, 
         HC01_EST_VC01, HC01_EST_VC02, HC01_EST_VC03, HC01_EST_VC04, HC01_EST_VC05, 
         HC01_EST_VC06, HC01_EST_VC07, HC01_EST_VC08, HC01_EST_VC09, HC01_EST_VC10,
         HC01_EST_VC11, HC01_EST_VC12, HC01_EST_VC13, HC01_EST_VC14)

# create key before renaming columns
industry.key <- industry[1, ]

# rename columns and remove first row
colnames(industry) <- c("GEO.id2", "NAME", "01.Civilian.16", "02.Agriculture.Forestry.Fish.Hunt.Mining",
                        "03.Construction", "04.Manufacturing", "05.Wholesale.Trade", "06.Retail.Trade",
                        "07.Transportation.Warehousing.Util", "08.Information", "09.Finance.Insurance.RealEstate",
                        "10.Professional.Scientific.Mgmt.Admin.Waste", "11.Education.HealthCare.Social", 
                        "12.Arts.Entertain.Accomodation.FoodService", "13.OtherServices", "14.PublicAdministration")

# add prefix to variables names for ID
colnames(industry) <- paste0("ind.", colnames(industry))

industry <- industry[-c(1), ]
rownames(industry) <- NULL

write.csv(industry, file = "~/GitHub/ATF-FFL/data/2015-ACS-industry.csv", row.names = F)
write.csv(industry.key, file = "~/Documents/ATF-FFL/data/census/2015-ACS-industry-key.csv", row.names = F)

# Cleanse: ACS Working Class data ---------------------------------------------

working.class <- read.csv("~/GitHub/ATF-FFL/data/01-census/ACS_15_1YR_working-class/ACS_15_1YR_S2408.csv")
# 53 obs of 93 variables

# select total estimate data for each class category
# there is also male-female data
# 53 obs of 11 variables
working.class <- working.class %>%
  select(GEO.id2, GEO.display.label,
         HC01_EST_VC01, HC01_EST_VC02, HC01_EST_VC03, HC01_EST_VC04, HC01_EST_VC05,
         HC01_EST_VC06, HC01_EST_VC07, HC01_EST_VC08, HC01_EST_VC09)

# create key before renaming columns
working.class.key <- working.class[1, ]

# rename columns
colnames(working.class) <- c("GEO.id2", "NAME", "01.Civilian.16", "02.Private.ForProfit", "03.Private.ForProfit.Employee",
                             "04.Private.ForProfit.Self.Inc", "05.Private.NonProfit", "06.LocalGov", "07.StateGov",
                             "08.FederalGov", "09.Self.Employed")

# add prefix to variables names for ID
colnames(working.class) <- paste0("work.", colnames(working.class))

# remove first row
working.class <- working.class[-c(1), ]
rownames(working.class) <- NULL

write.csv(working.class, file = "~/GitHub/ATF-FFL/data/2015-ACS-working-class.csv", row.names = F)
write.csv(working.class.key, file = "~/Documents/ATF-FFL/data/census/2015-ACS-working-class-key.csv", row.names = F)

# Cleanse: ACS Education data -------------------------------------------------

# 53 obs of 771 variables
education <- read.csv("~/GitHub/ATF-FFL/data/01-census/ACS_15_1YR_education/ACS_15_1YR_S1501.csv",
                      stringsAsFactors = F)

# select total estimate data for each age bracket
# and also male and female data
# 53 obs of 53 variables
education <- education %>%
  select(GEO.id2, GEO.display.label, 
         HC01_EST_VC02, HC03_EST_VC02, HC05_EST_VC02,
         HC01_EST_VC03, HC03_EST_VC03, HC05_EST_VC03,
         HC01_EST_VC04, HC03_EST_VC04, HC05_EST_VC04,
         HC01_EST_VC05, HC03_EST_VC05, HC05_EST_VC05,
         HC01_EST_VC06, HC03_EST_VC06, HC05_EST_VC06,
         HC01_EST_VC20, HC03_EST_VC20, HC05_EST_VC20,
         HC01_EST_VC21, HC03_EST_VC21, HC05_EST_VC21,
         HC01_EST_VC22, HC03_EST_VC22, HC05_EST_VC22,
         HC01_EST_VC24, HC03_EST_VC24, HC05_EST_VC24,
         HC01_EST_VC25, HC03_EST_VC25, HC05_EST_VC25,
         HC01_EST_VC26, HC03_EST_VC26, HC05_EST_VC26,
         HC01_EST_VC28, HC03_EST_VC28, HC05_EST_VC28,
         HC01_EST_VC29, HC03_EST_VC29, HC05_EST_VC29,
         HC01_EST_VC30, HC03_EST_VC30, HC05_EST_VC30,
         HC01_EST_VC32, HC03_EST_VC32, HC05_EST_VC32,
         HC01_EST_VC33, HC03_EST_VC33, HC05_EST_VC33,
         HC01_EST_VC34, HC03_EST_VC34, HC05_EST_VC34)

# create key
edu.key <- education[1, ]

# rename columns
colnames(education) <- c("GEO.id2", "NAME", 
                         "02.Total.18to24", "02.Total.18.to24.Male", "02.Total.18to24.Female",
                         "03.Total.18to24.LessHS", "03.Total.18to24.LessHS.Male", "03.Total.18to24.LessHS.Female",
                         "04.Total.18to24.HS", "04.Total.1824.HS.Male", "04.Total.18to24.HS.Female",
                         "05.Total.18to24.AA", "05.Total.18to24.AA.Male", "05.Total.18to24.AA.Female",
                         "06.Total.18to24.BA", "06.Total.18to24.BA.Male", "06.Total.18to24.BA.Female",
                         "20.Total.25to34", "20.Total.25to34.Male", "20.Total.25to34.Female",
                         "21.Total.25to34.HS", "21.Total.25to34.HS.Male", "21.Total.25to34.HS.Female",
                         "22.Total.25to34.BA", "22.Total.25to34.BA.Male", "22.Total.25to34.BA.Female",
                         "24.Total.35to44", "24.Total.35to44.Male", "24.Total.35to44.Female",
                         "25.Total.35to44.HS", "25.Total.35to44.HS.Male", "25.Total.35to44.HS.Female",
                         "26.Total.35to44.BA", "26.Total.35to44.BA.Male", "26.Total.35to44.BA.Female",
                         "28.Total.45to64", "28.Total.45to64.Male", "28.Total.45to64.Female",
                         "29.Total.45to64.HS", "29.Total.45to64.HS.Male", "29.Total.45to64.HS.Female",
                         "30.Total.45to64.BA", "30.Total.45to64.BA.Male", "30.Total.45to64.BA.Female",
                         "32.Total.65plus", "32.Total.65plus.Male", "32.Total.65plus.Female",
                         "33.Total.65plus.HS", "33.Total.65plus.HS.Male", "33.Total.65plus.HS.Female",
                         "34.Total.65plus.BA", "34.Total.65plus.BA.Male", "34.Total.65plus.BA.Female")

# add prefix to variables names for ID
colnames(education) <- paste0("edu.", colnames(education))

education <- education[-c(1), ]
rownames(education) <- NULL

write.csv(education, file = "~/GitHub/ATF-FFL/data/2015-ACS-education.csv", row.names = F)
write.csv(edu.key, file = "~/Documents/ATF-FFL/data/census/2015-ACS-education-key.csv", row.names = F)

# Cleanse: ACS Finance Characteristics data -----------------------------------

# Financial Characteristics: Occupied Housing

# There's two additional sub-categories to Occupied Housing Units: 
# Owner-Occupied and Renter-Occupied.
# This data can be extracted in the same manner as below.

# 53 obs of 279 variables
finance <- read.csv("~/GitHub/ATF-FFL/data/01-census/ACS_15_1YR_financial/ACS_15_1YR_S2503.csv",
                    stringsAsFactors = F)

# create key
finance.key <- finance[1, ]

# select household income by bracket
# Occupied Housing Units (total of Owner-Occupied and Renter-Occupied Housing)
# Income bracket observations are percentages of total Occupied Housing Units
# 53 obs of 15 variables
finance <- finance %>%
  select(GEO.id2, GEO.display.label,
         HC01_EST_VC01, HC01_EST_VC03, HC01_EST_VC04, HC01_EST_VC05, HC01_EST_VC06,
         HC01_EST_VC07, HC01_EST_VC08, HC01_EST_VC09, HC01_EST_VC10, HC01_EST_VC11,
         HC01_EST_VC12, HC01_EST_VC13, HC01_EST_VC14)

colnames(finance) <- c("GEO.id2", "NAME", 
                       "01.OccupiedHousingUnits", "03.LessThan5000", "04.5000to9999",
                       "05.10000to14999", "06.15000to19999", "07.20000to24999",
                       "08.25000to34999", "09.35000to49999", "10.50000to74999",
                       "11.75000to99999", "12.100000to149999", "13.150000.or.more",
                       "14.MedianHouseholdIncome")

# add prefix
colnames(finance) <- paste0("fin.", colnames(finance))

# remove first row
finance <- finance[-c(1), ]
rownames(finance) <- NULL

# write out data
write.csv(finance, file = "~/GitHub/ATF-FFL/data/2015-ACS-finance.csv", row.names = F)

# read back in to convert to numeric
finance <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-finance.csv", stringsAsFactors = F)

# create raw counts from percentages
percentage.income <- function(x) {
  x <- finance$fin.01.OccupiedHousingUnits * 0.01 * x
  x
}

# create new columns with raw counts derived from percentages
finance$fin.LessThan5000 <- percentage.income(finance$fin.03.LessThan5000)
finance$fin.5000to9999 <- percentage.income(finance$fin.04.5000to9999)
finance$fin.10000to14999 <- percentage.income(finance$fin.05.10000to14999)
finance$fin.15000to19999 <- percentage.income(finance$fin.06.15000to19999)
finance$fin.20000to24999 <- percentage.income(finance$fin.07.20000to24999)
finance$fin.25000to34999 <- percentage.income(finance$fin.08.25000to34999)
finance$fin.35000to49999 <- percentage.income(finance$fin.09.35000to49999)
finance$fin.50000to74999 <- percentage.income(finance$fin.10.50000to74999)
finance$fin.75000to99999 <- percentage.income(finance$fin.11.75000to99999)
finance$fin.100000to149999 <- percentage.income(finance$fin.12.100000to149999)
finance$fin.150000.or.more <- percentage.income(finance$fin.13.150000.or.more)

write.csv(finance, file = "~/GitHub/ATF-FFL/data/2015-ACS-finance.csv", row.names = F)
write.csv(finance.key, file = "~/Documents/ATF-FFL/data/census/2015-ACS-finance-key.csv", row.names = F)

# Bind all data ---------------------------------------------------------------

race.pop <- read.csv("~/GitHub/ATF-FFL/data/01-census/2014-electorate/V1-race-pop.csv",
                     stringsAsFactors = F)

industry <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-industry.csv",
                     stringsAsFactors = F)

working.class <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-working-class.csv",
                          stringsAsFactors = F)

education <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-education.csv",
                      stringsAsFactors = F)

finance <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-finance.csv",
                    stringsAsFactors = F)

# clean and remove extra variables
colnames(race.pop)[1] <- "NAME"
colnames(race.pop) <- paste0("race.", colnames(race.pop)) 

industry$ind.GEO.id2 <- NULL
working.class$work.GEO.id2 <- NULL
education$edu.GEO.id2 <- NULL
finance$fin.GEO.id2 <- NULL

colnames(industry)[1] <- "NAME"
colnames(working.class)[1] <- "NAME"
colnames(education)[1] <- "NAME"
colnames(finance)[1] <- "NAME"

acs.data <- left_join(industry, working.class, by = "NAME")
acs.data <- left_join(acs.data, education, by = "NAME")
acs.data <- left_join(acs.data, finance, by = "NAME")

# removing DC, USA, and PR from all will make them bind.

acs.all.data <- acs.data[-c(9, 52), ]
rownames(acs.all.data) <- NULL

origin.pop <- race.pop[-c(9, 45), ]
rownames(origin.pop) <- NULL

# function to normalize state names in origin.pop
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

origin.pop$NAME <- tolower(origin.pop$NAME)
origin.pop$NAME <- capwords(origin.pop$NAME)

# bind origin data to all ACS data
acs.all.data <- left_join(acs.all.data, origin.pop, by = "NAME")
colnames(acs.all.data)

write.csv(acs.all.data, file = "~/GitHub/ATF-FFL/data/2015-ACS-all-data.csv", row.names = F)

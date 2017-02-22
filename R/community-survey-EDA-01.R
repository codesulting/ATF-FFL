# ATF- FFL
# Feature Selection + Engineering
# Census Electorate Characteristics & American Community Survey data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# dataset containing data by state for:
# - population by sex, race/origin
# - industry
# - working class
# - educational attainment
# - financial characteristics

acs <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-all-data.csv", stringsAsFactors = F)

# individual datasets to explore
education <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-education.csv", stringsAsFactors = F)
finance <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-education.csv", stringsAsFactors = F)
industry <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-industry.csv", stringsAsFactors = F)
working.class <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-working-class.csv", stringsAsFactors = F)

# custom plot themes
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# FFL data --------------------------------------------------------------------
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita.csv")

# create a compact 2016 FFL count
ffl <- ffl %>%
  select(NAME, atf.Region, POPESTIMATE2016, LicCount, LicCountMonthly, perCapFFLyear, perCapitaFFL)

colnames(ffl)[1:3] <- c("NAME", "ATF.Region", "Pop2016")
write.csv(ffl, file = "~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", row.names = F)

# Population data -------------------------------------------------------------
pop <- read.csv("~/GitHub/ATF-FFL/data/nst-est2016-alldata.csv")

# create compact population dataframe
pop <- pop %>%
  select(NAME, 
         POPESTIMATE2010, POPESTIMATE2011, POPESTIMATE2012, 
         POPESTIMATE2013, POPESTIMATE2014, POPESTIMATE2015, POPESTIMATE2016)

# remove first 4 rows (states only)
pop <- pop[-c(1:5, 14, 57), ]
pop$NAME <- factor(pop$NAME)
colnames(pop) <- c("NAME", 
                   "Pop2010", "Pop2011", "Pop2012",
                   "Pop2013", "Pop2014", "Pop2015", "Pop2016")

rownames(pop) <- NULL

write.csv(pop, file = "~/GitHub/ATF-FFL/data/population-compact.csv", row.names = F)


# EDA: Industry ---------------------------------------------------------------

industry <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-industry.csv", stringsAsFactors = F)
str(industry)
summary(industry)

# remove DC and Puerto Rico
industry <- industry[-c(9, 52), ]

summary(industry$ind.01.Civilian.16)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   295300   778100  1983000  3003000  3491000 18050000 

18050000/2
# 9025000

# who has the largest workforce? 
ggplot(industry, aes(reorder(ind.NAME, ind.01.Civilian.16), ind.01.Civilian.16, 
                     fill = ind.01.Civilian.16)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient2(low = "firebrick3",
                       mid = "antiquewhite1",
                       high = "cadetblue3", midpoint = 9025000) +
  labs(title = "Total Workforce Population ~ State", x = "", y = "workforce population aged 16+") +
  pd.theme + coord_flip()

# These figures need to be per capita.
# Bind population data, and compute per capita

# Per Capita function
perCap2015 <- function (x) {
  x <- (x / pop$Pop2015) * 100000
  x
}

colnames(industry)[2] <- 'NAME'

industryPerCapita <- industry %>%
  left_join(pop, by = "NAME") %>%
  mutate(workforcePC = perCap2015(ind.01.Civilian.16), 
         agriculturePC = perCap2015(ind.02.Agriculture.Forestry.Fish.Hunt.Mining),
         constructionPC = perCap2015(ind.03.Construction),
         manufacturingPC = perCap2015(ind.04.Manufacturing),
         wholesalePC = perCap2015(ind.05.Wholesale.Trade),
         retailPC = perCap2015(ind.06.Retail.Trade),
         transportationPC = perCap2015(ind.07.Transportation.Warehousing.Util),
         informationPC = perCap2015(ind.08.Information),
         financePC = perCap2015(ind.09.Finance.Insurance.RealEstate),
         pro.scientificPC = perCap2015(ind.10.Professional.Scientific.Mgmt.Admin.Waste),
         educationPC = perCap2015(ind.11.Education.HealthCare.Social),
         artsPC = perCap2015(ind.12.Arts.Entertain.Accomodation.FoodService),
         otherPC = perCap2015(ind.13.OtherServices),
         publicAdminPC = perCap2015(ind.14.PublicAdministration))


# plot workforce per 100k population 

summary(industryPerCapita$workforcePC)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   40110   45200   47400   47430   50200   53630

ggplot(industryPerCapita, aes(reorder(NAME, workforcePC), workforcePC, 
                     fill = workforcePC)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient2(low = "firebrick4",
                       mid = "antiquewhite1",
                       high = "cadetblue3", midpoint = 9025000) +
  labs(title = "Total Workforce Population ~ State", x = "", 
       y = "workforce population aged 16+", fill = "") +
  pd.theme + coord_flip()

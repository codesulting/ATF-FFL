# ATF - Federal Firearms Licenses
# Exploratory Data Analysis

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(scales)

# 2016 data -------------------------------------------------------------------

# data: Federal Firearms Licenses 2016 ----------------------------------------
f16 <- fread("~/Documents/ATF-FFL/data/ffl-2016-V3.csv", stringsAsFactors = T)
f16 <- as.data.frame(f16)
str(f16)

# data: US Census Population estimates 2010-2016 ------------------------------
pop <- fread("~/Documents/ATF-FFL/data/census/nst-est2016-alldata.csv")
pop <- as.data.frame(pop)
str(pop)

# 2016 population: US Census --------------------------------------------------
p16 <- pop %>%
  select(REGION, DIVISION, STATE, NAME, contains("2016")) %>%
  arrange(desc(POPESTIMATE2016))

p16u <- p16[-c(1, 2, 3, 4, 5), ]

# data: US Census Congressional Apportionment ----------------------------------
house <- read.csv("~/Documents/ATF-FFL/data/census/HouseSeats.csv")

# rename columns
years <- seq(2010, 1910, -10)
colnames(house) <- c("State", "Total", "resident.pop", "overseas.pop", years)

# clean observations
house$State <- as.character(house$State)
house$State <- gsub("\\.", "", house$State)
house$State <- gsub("\\s+$", "", house$State)
levels(as.factor(house$State))

# data: ATF Firearms Commerce -------------------------------------------------

commerce.FFL.total <- read.csv("~/Documents/ATF-FFL/data/commerce/10-FFL-total.csv")





# Per Capita FFLs -------------------------------------------------------------

# 1. find monthly average of FFLs per state
# 2. calculate number of FFLs per 100,000 people

# 1:
# monthly license count: sum monthly counts, divide by 12. 
# monthly license count (from data): sum monthly counts, divide by number of months

lic.month <- f16 %>%
  group_by(month, PremiseStateFull) %>%
  distinct(LicenseName) %>%
  count()

colnames(lic.month) <- c("month", "PremiseStateFull", "NumFFLs")  

# accurate mean from the data
lic.month <- lic.month %>% group_by(PremiseStateFull) %>%
  mutate(meanFFL = sum(NumFFLs) / 10)

avgMonthlyFFL <- lic.month %>% 
  select(PremiseStateFull, meanFFL) %>%
  distinct(meanFFL)

# 2:
# Using Census 2016 data, find license counts per 100,000 residents
# (number of FFLS / population) * 100,000

f16u <- f16 %>%
  select(PremiseStateFull, LicCount, EstPop2016, EstPopPerHouseSeat) %>%
  arrange(desc(EstPop2016)) %>%
  distinct() %>%
  mutate(HouseRate = EstPop2016/EstPopPerHouseSeat,
         LicCountMonthly = LicCount/12,
         perCapFFLyear = (LicCount/EstPop2016)*100000,
         perCapitaFFL = (LicCountMonthly/EstPop2016)*100000)

# 'perCapitaFFL' is the truer number of FFLs per 100,000: 
# it takes the mean of FFLs monthly.
# The annual FFL count has many duplicates, repeat businesses.
colnames(f16u) <- c("NAME", "LicCount", "EstPop16.Wiki", "EstPopPerHouseSeat",
                    "HouseRate", "LicCountMonthly", "perCapFFLyear", "perCapitaFFL")

f16u <- f16u[, c(1, 2, 7, 6, 8, 3, 4, 5)]

# merge FFL and Census Population data
fp16 <- left_join(f16u, p16u, by = as.character("NAME"))
summary(fp16)


# Exploratory Plots -----------------------------------------------------------

# define a theme for plotting
# modifies theme_minimal() with type set in Gill Sans
# and italic axis titles in Times
pd.theme <- theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)))

pd.classic <- theme_classic(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)))

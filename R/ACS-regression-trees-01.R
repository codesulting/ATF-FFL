# ATF- FFL - ACS Financial Characteristics Data
# Feature Selection + Engineering
# Census Electorate Characteristics & American Community Survey data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(broom)

# dataset containing data by state for:
# - population by sex, race/origin
# - industry
# - working class
# - educational attainment
# - financial characteristics

# all ACS data, per capita FFLs by State, Population data, Rural-Urban Proportions data
acs <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-all-data.csv", stringsAsFactors = F)
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)
pop <- read.csv("~/GitHub/ATF-FFL/data/population-compact.csv", stringsAsFactors = F)
rural.urban <- read.csv("~/GitHub/ATF-FFL/data/rural-urban-compact.csv")

str(acs)
str(pop)
str(ffl)
str(rural.urban)

# merge all data --------------------------------------------------------------

# merge FFL and pop
all.features <- ffl %>%
  right_join(pop, by = "NAME")

# remove/rename columns
all.features$Pop2016.x <- NULL
colnames(all.features)[13] <- "Pop2016"

# merge ACS features
all.features <- all.features %>%
  right_join(acs, by = "NAME")

# select certain variables from rural-urban
# and then bind
all <- rural.urban %>%
  dplyr::select(NAME, POPPCT_UC, POPPCT_RURAL, AREA_UC, AREA_UA, AREA_RURAL) %>%
  left_join(all.features, by = "NAME")

# 50 observations of 127 variables
str(all)

# check end of dataframe variables
all[, 120:127]

# Model 00 --------------------------------------------------------------------

mod.00 <- lm(perCapitaFFL ~ .-NAME, data = all)
summary(mod.00)
# singularity:
# the model is perfectly fit, having 0 predictive or descriptive power.

# Regression Trees: all data  -------------------------------------------------

# subset training data
set.seed(144)
train <- sample.split(all, SplitRatio = 0.7)

tree.01 <- tree(perCapitaFFL ~ .-NAME, data = all, subset = train)
rpart.01 <- rpart(perCapitaFFL ~ .-NAME, data = all)
summary(tree.01)
summary(rpart.01)

plot(tree.01)
text(tree.01, pretty = 0)

prp(rpart.01)
plot(rpart.01)
text(rpart.01)

# Industry Data preparation ---------------------------------------------------

# load themes and functions
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
source("~/GitHub/ATF-FFL/R/capwords.R")

# cleanse industry data
industry <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-industry.csv", stringsAsFactors = F)
colnames(industry)[2] <- "NAME"
industry <- industry[-c(9, 52), ]

# per capita Industry data ----------------------------------------------------

# Per Capita function
perCap2015 <- function (x) {
  x <- (x / pop$Pop2015) * 100000
  x
}

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

# merge data ------------------------------------------------------------------
ffl.pop <- ffl %>%
  left_join(pop, by = "NAME")

ffl.pop$Pop2016.x <- NULL
colnames(ffl.pop)[13] <- "Pop2016"

industry <- industryPerCapita %>%
  left_join(ffl.pop, by = "NAME")

rownames(industry) <- industry$NAME

# Regression Trees: Industry Data ---------------------------------------------

industry.model <- industry %>%
  dplyr::select(perCapitaFFL, 24:37)

str(industry.model)

# grow regression tree
industry.tree01 <- rpart(perCapitaFFL ~ ., data = industry.model)

# plot tree
rpart.plot(industry.tree01, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(industry.tree01)
printcp(industry.tree01)

ggplot(industry.model, aes(agriculturePC, perCapitaFFL)) +
  geom_point() +
  geom_vline(xintercept = 2287) +
  geom_vline(xintercept = 1269)


# Income Data preparation -----------------------------------------------------
income <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-financePerCapita-all.csv", 
                   stringsAsFactors = F)
colnames(income)[3] <- "NAME"
income$X <- NULL
income$total.GEO.id2 <- NULL

# subset variables for modeling
incomePerCapita <- income %>%
  dplyr::select(15:25, 38)

rownames(incomePerCapita) <- income$NAME
summary(incomePerCapita)

# Regression Trees: Income data -----------------------------------------------
str(incomePerCapita)
income.tree01 <- rpart(perCapitaFFL ~ ., data = incomePerCapita)
rpart.plot(income.tree01, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(income.tree01)
summary(incomePerCapita$perCapita.50000to74999)

# Education Data preparation --------------------------------------------------

# ACS Financial Characteristics data, FFL data, Total Population data
education <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-education.csv", stringsAsFactors = F)
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)
pop <- read.csv("~/GitHub/ATF-FFL/data/population-compact.csv", stringsAsFactors = F)

# Per Capita function
perCap2015 <- function (x) {
  x <- (x / pop$Pop2015) * 100000
  x
}

# custom plot themes and maps
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# cleanse and bind
education <- education[-c(9, 52), ]
education$edu.GEO.id2 <- NULL
colnames(education) <- gsub("edu.[0-9][0-9].", "", colnames(education))
colnames(education)[1] <- "NAME"
rownames(education) <- education$NAME

# per capita calculation
edu <- as.data.frame(apply(education[2:52], 2, perCap2015))
edu$NAME <- rownames(edu)

# merge FFL and population data
ffl.pop <- ffl %>%
  left_join(pop, by = "NAME")

ffl.pop$Pop2016.x <- NULL
colnames(ffl.pop)[13] <- "Pop2016"

edu.pc <- ffl.pop %>%
  left_join(edu)

# remove and rename columns: Per Capita
edu.pc[c(2:5, 7:11)] <- NULL
colnames(edu.pc) <- gsub("Total", "pc", colnames(edu.pc))

# subset education categories for 
# population/age bracket, high school, and bachelor's degree
edu.pc2 <- edu.pc %>%
  dplyr::select(NAME, perCapitaFFL, Pop2015, Pop2016,
                pc.18to24, pc.18to24.HS, pc.18to24.BA,
                pc.25to34, pc.25to34.HS, pc.25to34.BA,
                pc.35to44, pc.35to44.HS, pc.35to44.BA,
                pc.45to64, pc.45to64.HS, pc.45to64.BA,
                pc.65plus, pc.65plus.HS, pc.65plus.BA)

rownames(edu.pc2) <- edu.pc2$NAME

# Regression Trees: Education data --------------------------------------------

str(edu.perCapita)
edu.perCapita <- edu.pc2 %>%
  select(-c(NAME, Pop2015, Pop2016))

edu.tree01 <- rpart(perCapitaFFL ~ ., data = edu.perCapita)
rpart.plot(edu.tree01, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(edu.tree01)

# Rural-Urban data preparation ------------------------------------------------

# cleansed dataframe with FFL & Rural-Urban data
rural.urban <- read.csv("~/GitHub/ATF-FFL/data/rural-urban-compact.csv")
str(rural.urban)
names(rural.urban)

rownames(rural.urban) <- rural.urban$NAME

# Regression Trees: Rural-Urban data  -----------------------------------------

rural.urban <- rural.urban %>%
  select(perCapitaFFL, 7:38)

rural.urban.tree01 <- rpart(perCapitaFFL ~ ., data = rural.urban)
rpart.plot(rural.urban.tree01, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85,
           fallen.leaves = T)

print(rural.urban.tree01)

# Legislature data preparation ------------------------------------------------

# load 2014 data
leg.14 <- read.csv("~/GitHub/ATF-FFL/data/02-state-legislatures/control/2014.csv")

# add year variable 
leg.14$Year <- "2014"

# rename for merging
colnames(leg.14)[1] <- "NAME"

# merge ffl data
leg.ffl <- leg.14 %>%
  left_join(ffl)

# clean Nebraska
str(leg.ffl)

leg.ffl$Total.House <- as.numeric(levels(leg.ffl$Total.House))[leg.ffl$Total.House]
leg.ffl[27, c(4, 5, 7, 8, 9, 10)] <- 0
leg.ffl$Total.House <- as.integer(leg.ffl$Total.House)
leg.ffl$Total.House

leg.model <- leg.ffl %>%
  dplyr::select(1:9, 11, 12, 13, 20)

rownames(leg.model) <- leg.model$NAME

leg.model <- leg.model %>%
  select(-NAME)

leg.model$House.Dem <- as.integer(leg.model$House.Dem)
leg.model$House.Rep <- as.integer(leg.model$House.Rep)
leg.model$Senate.Dem <- as.integer(leg.model$Senate.Dem)
leg.model$Senate.Rep <- as.integer(leg.model$Senate.Rep)

leg.model$Legis.Control <- gsub("Dem\\*", "Dem", leg.model$Legis.Control)
leg.model$State.Control <- gsub("Dem\\*", "Dem", leg.model$State.Control)
leg.model$Legis.Control <- factor(leg.model$Legis.Control)
leg.model$State.Control <- factor(leg.model$State.Control)

colnames(leg.model)[9:11] <- c("Legislative.Control",
                               "Governing.Party",
                               "State.Control")


# Regression Trees: Legislature data  -----------------------------------------

str(leg.model)

legislative.tree <- rpart(perCapitaFFL ~ ., data = leg.model)

rpart.plot(legislative.tree, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85,
           fallen.leaves = T)

print(legislative.tree)

# All data preparation --------------------------------------------------------

all.features <- leg.model %>%
  left_join(edu.perCapita) %>%
  left_join(incomePerCapita) %>%
  left_join(industry.model) %>%
  left_join(rural.urban)

# Regression Tree: All Features -----------------------------------------------

str(all.features)
all.tree <- rpart(perCapitaFFL ~ ., data = all.features)
rpart.plot(all.tree, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85,
           fallen.leaves = T)

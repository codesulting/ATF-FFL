# ATF- FFL - ACS Industry Data
# Feature Selection + Engineering
# Census Electorate Characteristics & American Community Survey data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(MASS)

source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
source("~/GitHub/ATF-FFL/R/capwords.R")

# dataset containing data by state for:
# - population by sex, race/origin
# - industry
# - working class
# - educational attainment
# - financial characteristics
# acs <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-all-data.csv", stringsAsFactors = F)

# ACS Industry data, FFL data, Total Population data
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)
pop <- read.csv("~/GitHub/ATF-FFL/data/population-compact.csv", stringsAsFactors = F)

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

# Model 00: baseline linear ---------------------------------------------------

industry.model <- industry %>%
  dplyr::select(perCapitaFFL, 24:37)

mod.00 <- lm(perCapitaFFL ~ ., industry.model)
summary(mod.00)
plot(mod.00)

mod.01 <- lm(perCapitaFFL ~ 
               agriculturePC + wholesalePC + financePC +
               constructionPC + manufacturingPC + pro.scientificPC, data = industry.model)
summary(mod.01)
plot(mod.01)

# remove agriculture - its a strong predictor
mod.02 <- lm(perCapitaFFL ~ wholesalePC + financePC + constructionPC + 
               manufacturingPC + pro.scientificPC, data = industry.model)
summary(mod.02)
plot(mod.02)

## Single Predictor Regressions ----------------------------------------------=

# agriculture
mod.03 <- lm(perCapitaFFL ~ agriculturePC, data = industry.model)
summary(mod.03)

# science
mod.04 <- lm(perCapitaFFL ~ pro.scientificPC, data = industry.model)
summary(mod.04)

# construction
mod.05 <- lm(perCapitaFFL ~ constructionPC, data = industry.model)
summary(mod.05)


# Minimal Adequate Model ------------------------------------------------------

# agricultre, science, construction
mod.06 <- lm(perCapitaFFL ~ agriculturePC + constructionPC + 
               pro.scientificPC, data = industry.model)
summary(mod.06)
plot(mod.06)


# Robust Regression 01 --------------------------------------------------------

industry.01 <- rlm(perCapitaFFL ~ agriculturePC + wholesalePC + financePC +
                       constructionPC + manufacturingPC + pro.scientificPC, data = industry.model)

summary(industry.01)
plot(industry.01)

huber.01 <- data.frame(.rownames = industryPerCapita$NAME, 
                       .resid = industry.01$resid,
                       weight = industry.01$w) %>% arrange(weight)

huber.01[1:12, ]
#         .rownames      .resid    weight
# 1         Montana  40.3769700 0.2088998
# 2          Alaska  30.1881675 0.2794054
# 3   New Hampshire  20.9669698 0.4022963
# 4       Louisiana -16.2234726 0.5198691

# join with fitted and observed data
industry.huber01 <- augment(industry.01) %>%
  left_join(huber.01) %>%
  arrange(weight) %>%
  mutate(weighted.resid = .resid * weight,
         weighted.fit = .fitted + weighted.resid)

# distibution of weighted residuals
ggplot(industry.huber01, aes(weighted.resid)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(x = "weighted residuals") +
  pd.scatter

summary(industry.huber01$weighted.fit)

# which states were reduced in weight by the model? 
ggplot(industry.huber01, 
       aes(reorder(.rownames, desc(weight)), 
           weight, fill = weight)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient2(low = "coral4",
                       mid = "antiquewhite1",
                       high = "deepskyblue4",
                       midpoint = 0.5, guide = F) +
  pd.theme + coord_flip() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  labs(x = "", y = "weight")

# specific look at states w/ reduced weights
industry.huber01 %>%
  filter(weight < 1) %>%
  arrange(desc(weight)) %>%
  ggplot(aes(reorder(.rownames, desc(weight)), 
             weight, 
             fill = weight)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "coral4",
                       mid = "antiquewhite1",
                       high = "deepskyblue4",
                       midpoint = 0.5, guide = F) +
  pd.theme + coord_flip() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14)) +
  labs(x = "", y = "weight")
  

# Agriculture per capita vs per capita FFL
ggplot(industry.huber01, aes(agriculturePC, perCapitaFFL, label = .rownames)) + 
  geom_point(size = 1.5) +
  geom_point(aes(agriculturePC, weighted.fit), color = "firebrick3", 
             shape = 23, size = 4, alpha = 0.8, data = industry.huber01) +
  geom_text(aes(agriculturePC, perCapitaFFL), 
            size = 3, hjust = 1, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_smooth(method = "loess", se = F, color = "deepskyblue4", 
              size = 0.2, linetype = "longdash") +
  geom_errorbar(aes(x = agriculturePC, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                linetype = "dotted") +
  labs(x = "per capita population: Agriculture, Hunting & Fishing, Mining Industry",
       y = "per capita Federal Firearms Licenses",
       title = "Robust Regression 01: Observed vs. Fitted Values") +
  pd.scatter

# facetted plot of all variables in model - fitted vs observed ----------------

# create long dataframe
industry.huber02 <- industry.huber01 %>%
  gather("industry", "ind.perCapita", 2:8)

# append perCapitaFFL data
ffl$.rownames <- ffl$NAME

industry.huber02 <- ffl %>%
  dplyr::select(.rownames, perCapitaFFL) %>%
  left_join(industry.huber02) %>%
  filter(industry != "perCapitaFFL")

# rename and clean up industry categories
industry.huber02$industry <- gsub("PC", "", industry.huber02$industry)
industry.huber02$industry <- capwords(industry.huber02$industry)

# factor industry categories
industry.huber02$industry <- factor(industry.huber02$industry)

# create facetted plot
industry.huber02 %>%
  group_by(industry) %>%
  ggplot(aes(ind.perCapita, perCapitaFFL, label = .rownames)) +
  geom_point(size = 0.75, alpha = 0.75) +
  geom_point(aes(ind.perCapita, weighted.fit),
             color = "firebrick3", alpha = 0.8,
             shape = 23, size = 2.5,
             data = industry.huber02) +
  geom_text(size = 2.5, alpha = 0.7,
            position = "jitter",
            check_overlap = T,
            hjust = 1, 
            vjust = 1) +
  geom_errorbar(aes(x = ind.perCapita, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                linetype = "dotted") +
  geom_smooth(method = "loess", se = F, size = 0.2, 
              color = "deepskyblue4", 
              linetype = "longdash") +
  facet_wrap(~ industry, scales = "free_x", nrow = 3) +
  pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black")) +
  labs(x = "per capita population by industry",
       y = "per capita Federal Firearms Licenses")

# facetted plot of select variables in model - --------------------------------

# subset for residuals > 5 or residuals < -9
industry.outliers <- industry.huber02 %>%
  group_by(industry) %>%
  filter(.resid > 5 | .resid < -9)

# create facetted plot
industry.outliers %>%
  group_by(industry) %>%
  ggplot(aes(ind.perCapita, perCapitaFFL, label = .rownames)) +
  geom_point(size = 0.75, alpha = 0.75) +
  geom_point(aes(ind.perCapita, weighted.fit),
             color = "firebrick3", alpha = 0.8,
             shape = 23, size = 2.5,
             data = industry.outliers) +
  geom_text(size = 2.5, alpha = 0.7,
            position = "jitter",
            check_overlap = T,
            hjust = 1, 
            vjust = 1) +
  geom_errorbar(aes(x = ind.perCapita, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                linetype = "dotted") +
  geom_smooth(method = "loess", se = F, size = 0.2, 
              color = "deepskyblue4", 
              linetype = "longdash") +
  facet_wrap(~ industry, scales = "free_x", nrow = 3) +
  pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black")) +
  labs(x = "per capita population by industry",
       y = "per capita Federal Firearms Licenses")




  
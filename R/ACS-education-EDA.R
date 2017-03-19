# ATF- FFL - ACS Financial Characteristics Data
# Feature Selection + Engineering
# Census Electorate Characteristics & American Community Survey data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

# dataset containing data by state for:
# - population by sex, race/origin
# - industry
# - working class
# - educational attainment
# - financial characteristics
# acs <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-all-data.csv", stringsAsFactors = F)

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

# cleanse and bind ------------------------------------------------------------

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

# baseline linear model -------------------------------------------------------

str(edu.pc2)
mod00 <- lm(perCapitaFFL ~ .-NAME, data = edu.pc2)
summary(mod00)

par(mfrow = c(2, 2), family = "GillSans")
plot(mod00)

par(mfrow = c(1, 1), family = "GillSans")
hist(mod00$residuals)

# Influential Outliers: Utah, Montana, Hawaii, Michigan
# What is it about these states that separates them from the rest in terms of education?
# Before plotting, summarize variables:
# 1. Total population by age
# 2. Total HS
# 3. Total BA 

edu.pc2 <- edu.pc2 %>%
  mutate(pop.per.capita = pc.18to24 + pc.25to34 + pc.35to44 + pc.45to64 + pc.65plus,
         hs.per.capita = pc.18to24.HS + pc.25to34.HS + pc.35to44.HS + pc.45to64.HS + pc.65plus.HS,
         ba.per.capita = pc.18to24.BA + pc.25to34.BA + pc.35to44.BA + pc.45to64.BA + pc.65plus.BA)

rownames(edu.pc2) <- edu.pc2$NAME

# simplified model with simplified variables
mod01 <- lm(perCapitaFFL ~ pop.per.capita + hs.per.capita + ba.per.capita, data = edu.pc2)
summary(mod01)

plot(mod01)

# plot education across states: High School -----------------------------------
summary(edu.pc2$hs.per.capita)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  55770   61330   63290   62850   64790   68940

ggplot(edu.pc2, aes(reorder(NAME, hs.per.capita), hs.per.capita,
                    fill = hs.per.capita)) +
  geom_bar(stat = "identity") + pd.theme + coord_flip() +
  scale_fill_gradient2(low = "firebrick4", 
                       mid = "antiquewhite1", 
                       high = "deepskyblue4",
                       midpoint = 63290) +
  scale_y_continuous(breaks = c(55000, 60000, 65000, 70000),
                     labels = c(55000, 60000, 65000, 70000),
                     limits = c(0, 70000),
                     expand = c(0.025, 0)) +
  theme(axis.text.x = element_text(angle = 45, size = 11.25,
                                   hjust = 1, vjust = 1),
        axis.text = element_text(size = 11.25)) +
  labs(x = "", y = "high school graduates, per capita",
       fill = "")

# plot education across states: Bachelor's Degree -----------------------------
summary(edu.pc2$ba.per.capita)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14250   18460   20490   21080   23400   30560

ggplot(edu.pc2, aes(reorder(NAME, ba.per.capita), ba.per.capita,
                    fill = ba.per.capita)) +
  geom_bar(stat = "identity") + pd.theme + coord_flip() +
  scale_fill_gradient2(low = "firebrick4", 
                       mid = "antiquewhite1", 
                       high = "deepskyblue4",
                       midpoint = 20490) +
  labs(x = "", y = "college graduates (BA), per capita")

# plot education across states: Surveyed Population ---------------------------
summary(edu.pc2$pop.per.capita)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  69670   76220   77270   77240   78470   80800

ggplot(edu.pc2, aes(reorder(NAME, pop.per.capita), pop.per.capita,
                    fill = pop.per.capita)) +
  geom_bar(stat = "identity") + pd.theme + coord_flip() +
  scale_fill_gradient2(low = "firebrick4", 
                       mid = "antiquewhite2", 
                       high = "deepskyblue4",
                       midpoint = 77270)

# Education ratio variables ---------------------------------------------------
# 1. HS:pop
# 2. BA:pop
# 2. HS:BA

edu.pc2 <- edu.pc2 %>%
  mutate(hs.pop.ratio = hs.per.capita/pop.per.capita,
         ba.pop.ratio = ba.per.capita/pop.per.capita,
         hs.ba.ratio = hs.per.capita/ba.per.capita)

edu.pc2$Pop2015.perCapita <- perCap2015(edu.pc2$Pop2015)
rownames(edu.pc2) <- edu.pc2$NAME

mod02 <- lm(perCapitaFFL ~ hs.pop.ratio + ba.pop.ratio + hs.ba.ratio, data = edu.pc2)
summary(mod02)
plot(mod02)

par(mfrow = c(1, 1), family = "GillSans")
hist(mod02$residuals)

summary(edu.pc2$hs.pop.ratio)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.7529  0.7962  0.8190  0.8135  0.8299  0.8633

ggplot(edu.pc2, aes(reorder(NAME, hs.pop.ratio), hs.pop.ratio, fill = hs.pop.ratio)) +
  geom_bar(stat = "identity") + pd.theme + coord_flip() +
  scale_fill_gradient2(low = "firebrick4", 
                       mid = "antiquewhite2", 
                       high = "deepskyblue4",
                       midpoint = 0.82) +
  scale_y_continuous(breaks = c(0.75, 0.80, 0.85, 0.90, 0.95),
                     labels = c(0.75, 0.80, 0.85, 0.90, 0.95),
                     expand = c(0.025, 0)) +
  theme(axis.text = element_text(size = 11.25),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(y = "high school graduate:sample population ratio",
       x = "", fill = "")

# Age Brackets only -----------------------------------------------------------

mod03 <- lm(perCapitaFFL ~ pc.18to24 + pc.25to34 + pc.35to44 + pc.45to64 + pc.65plus,
            data = edu.pc2)
summary(mod03)

par(mfrow = c(2, 2), family = "GillSans")
plot(mod03)

# plot age brackets by state
edu.pc2 %>%
  select(NAME, pop.per.capita, pc.18to24, pc.25to34, pc.35to44, pc.45to64, pc.65plus) %>%
  gather("age", "n", 2:7) %>%
  ggplot(aes(n, NAME)) + geom_point() +
  facet_wrap(~ age, ncol = 6) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  coord_flip()

# Accurate Age and Education Brackets -----------------------------------------

# create total pop, HS, BA, male, female variables
edu.pc3 <- education %>%
  select(NAME, 
         Total.18to24, Total.25to34, Total.35to44, Total.45to64, Total.65plus,
         Total.1824.HS.Male, Total.25to34.HS, Total.35to44.HS, Total.45to64.HS, Total.65plus.HS,
         Total.18to24.BA, Total.25to34.BA, Total.35to44.BA, Total.45to64.BA, Total.65plus.BA,
         Total.18.to24.Male, Total.25to34.Male, Total.35to44.Male, Total.45to64.Male, Total.65plus.Male,
         Total.18to24.Female, Total.25to34.Female, Total.35to44.Female, Total.45to64.Female, Total.65plus.Female) %>%
  mutate(total.pop = Total.18to24 + Total.25to34 + Total.35to44 + Total.45to64 + Total.65plus,
         total.HS = Total.1824.HS.Male + Total.25to34.HS + Total.35to44.HS + Total.45to64.HS + Total.65plus.HS,
         total.BA = Total.18to24.BA + Total.25to34.BA + Total.35to44.BA + Total.45to64.BA + Total.65plus.BA,
         total.male = Total.18.to24.Male + Total.25to34.Male + Total.35to44.Male + Total.45to64.Male + Total.65plus.Male,
         total.female = Total.18to24.Female + Total.25to34.Female + Total.35to44.Female + Total.45to64.Female + Total.65plus.Female)

# merge FFL data
edu.pc3 <- ffl.pop %>%
  left_join(edu.pc3) 

# compute population estimate error
edu.pc3 <- edu.pc3 %>%
  mutate(pop.err.2015 = Pop2015 - total.pop,
         pop.err.2016 = Pop2016 - total.pop)

# compute per capita for 5 totals
edu.pc3 <- edu.pc3 %>%
  mutate(perCap.pop = (total.pop/Pop2015) * 100000,
         perCap.HS = (total.HS/Pop2015) * 100000,
         perCap.BA = (total.BA/Pop2015) * 100000,
         perCap.male = (total.male/Pop2015) * 100000,
         perCap.female = (total.female/Pop2015) * 100000)

rownames(edu.pc3) <- edu.pc3$NAME

# write.csv(edu.pc3, file = "~/GitHub/ATF-FFL/data/2015-education-clean.csv", row.names = F)

# Model and Plot --------------------------------------------------------------

mod04 <- lm(perCapitaFFL ~ perCap.HS + perCap.BA + perCap.male + perCap.female,
            data = edu.pc3)
summary(mod04)

# diagnostic plots
par(mfrow = c(2, 2))
plot(mod04)

# historgram of residuals
par(mfrow = c(1, 1))
hist(mod04$residuals)

# High School vs Bachelors by State
edu.pc3 %>%
  select(NAME, perCap.HS, perCap.BA) %>%
  gather("category", "n", 2:3) %>%
  ggplot(aes(n, reorder(NAME, n))) + 
  geom_point() +
  facet_wrap(~ category, ncol = 1) + pd.scatter +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(x = "", y = "")
  
# HS and College Graduate Ratios ----------------------------------------------

edu.pc3 <- edu.pc3 %>%
  mutate(ratio.HS = total.HS/total.pop,
         ratio.BA = total.BA/total.pop,
         ratio.18to24 = Total.18to24/total.pop,
         ratio.25to34 = Total.25to34/total.pop,
         ratio.35to44 = Total.35to44/total.pop,
         ratio.45to64 = Total.45to64/total.pop,
         ratio.65plus = Total.65plus/total.pop)

rownames(edu.pc3) <- edu.pc3$NAME

mod05 <- lm(perCapitaFFL ~ ratio.HS + ratio.BA + ratio.18to24 + ratio.25to34 +
              ratio.35to44 + ratio.45to64 + ratio.65plus, data = edu.pc3)
summary(mod05)
plot(mod05)

edu.pc3 %>%
  select(NAME, total.pop, Total.18to24, Total.25to34, Total.35to44, Total.45to64, Total.65plus) %>%
  mutate(perCapita.18to24 = (Total.18to24/total.pop) * 100000,
         perCapita.25to34 = (Total.25to34/total.pop) * 100000,
         perCapita.35to44 = (Total.35to44/total.pop) * 100000,
         perCapita.45to64 = (Total.45to64/total.pop) * 100000,
         perCapita.65plus = (Total.65plus/total.pop) * 100000) %>%
  gather("age", "n", 8:12) %>%
  ggplot(aes(n, reorder(NAME, n))) + 
  geom_point() +
  facet_wrap(~ age, ncol = 1) + pd.scatter +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.border = element_rect(fill = NA, color = "black"),
        axis.text.y = element_text(size = 6)) +
  labs(x = "", y = "")

age <- edu.pc3 %>%
  select(NAME, perCapitaFFL, total.pop, Total.18to24, Total.25to34, Total.35to44, Total.45to64, Total.65plus) %>%
  mutate(perCapita.18to24 = (Total.18to24/total.pop) * 100000,
         perCapita.25to34 = (Total.25to34/total.pop) * 100000,
         perCapita.35to44 = (Total.35to44/total.pop) * 100000,
         perCapita.45to64 = (Total.45to64/total.pop) * 100000,
         perCapita.65plus = (Total.65plus/total.pop) * 100000)

rownames(age) <- age$NAME

summary(lm(perCapitaFFL ~ perCapita.18to24 + perCapita.25to34 + perCapita.35to44 +
             perCapita.45to64 + perCapita.65plus, data = age))

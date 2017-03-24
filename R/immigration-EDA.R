# Federal Firearms Licenses
# FFA & Immigration Data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(MASS)
library(quantreg)

# load themes and functions
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# ATF - FFL data
ffl <- read.csv("data/ffl-2016-perCapita-compact.csv")

# US Census - all population data 2010-2016
pop <- read.csv("data/nst-est2016-alldata.csv")
pop <- pop %>%
  dplyr::select(NAME, contains("2012"), contains("2013"), contains("2014"))

pop <- pop[-c(1:5, 14, 57), ]


# FFA data --------------------------------------------------------------------
# Firearms Freedom Act
# http://gunwars.news21.com/interactives/nullification.html

ffa <- read.csv("data/ffa-2014.csv")

# merge with ffl
ffa <- ffa %>%
  left_join(ffl) %>%
  dplyr::select(-LicCount, -Pop2016, -LicCountMonthly, -perCapFFLyear)

rownames(ffa) <- ffa$NAME

ffa.model <- lm(perCapitaFFL ~ .-NAME, data = ffa)
summary(ffa.model)
plot(ffa.model)

# Immigration Data ------------------------------------------------------------
# Department of Homeland Security Immigration Yearbooks
# https://www.dhs.gov/immigration-statistics/yearbook/2014

# This table (4) is defined as:
# Persons Obtaining Lawful Permanent Resident Status By State Or Territory Of Residence: Fiscal Years 2012 To 2014
# Foreign nationals who are granted lawful permanent residence
# immigrants who receive a "green card"
# immigrants admitted as temporary nonimmigrants
# immigrants granted asylum or refugee status
# or immigrants who are naturalized

# load immigration data
immigration <- read.csv("data/Immigration-Permanent-Residents-2014-Table4-Homeland-Security.csv",
                        stringsAsFactors = F)
colnames(immigration)[2:4] <- c("y2012", "y2013", "y2014")

# remove commas
immigration$y2012 <- gsub(",", "", immigration$y2012)
immigration$y2013 <- gsub(",", "", immigration$y2013)
immigration$y2014 <- gsub(",", "", immigration$y2014)
immigration$y2012 <- as.integer(immigration$y2012)
immigration$y2013 <- as.integer(immigration$y2013)
immigration$y2014 <- as.integer(immigration$y2014)

# merge FFL, population, and immigration data
immigration <- ffl %>%
  dplyr::select(NAME, perCapitaFFL) %>%
  left_join(immigration) %>%
  left_join(pop)

# per capita 2014
perCapita2014 <- function (x) {
  x <- (x / immigration$POPESTIMATE2014) * 100000
  x
}

# create per capita variables 
# and ratio to international migration
immigration <- immigration %>%
  mutate(y2014.perCapita = perCapita2014(y2014),
         y2014.ratio = y2014 / INTERNATIONALMIG2014,
         log2014.perCapita = log10(y2014.perCapita),
         logFFL = log10(perCapitaFFL),
         intl.migration.perCapita = (INTERNATIONALMIG2014/POPESTIMATE2014) * 100000)

# plot FFL vs immigration w/ lm and rlm lines
ggplot(immigration, aes(y2014.perCapita, perCapitaFFL, label = NAME)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = F, size = 0.5, 
              linetype = "dashed", color = "steelblue") +
  geom_smooth(method = "rlm", se = F, size = 0.5, 
              linetype = "dashed", color = "firebrick3") +
  geom_text(hjust = -0.1, vjust = 1.1, 
            size = 3.25, alpha = 1, 
            check_overlap = T) +
  expand_limits(x = c(0, 800)) +
  pd.scatter +
  labs(x = "per capita immigration", 
       y = "per capita Federal Firearms Licenses")

# plot FFL vs immigration on log scale w/ lm and rlm lines
ggplot(immigration, aes(log2014.perCapita, logFFL, label = NAME)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, size = 0.5,
              linetype = "dashed", color = "steelblue4") +
  geom_smooth(method = "rlm", se = F, size = 0.5, 
              linetype = "dashed", color = "firebrick3") +
  geom_text(hjust = -0.10, vjust = 1.05, 
            size = 3.25, alpha = 0.85,
            check_overlap = T) +
  scale_x_log10() + scale_y_log10() +
  expand_limits(x = c(2, 3), y = c(2, 3)) +
  pd.scatter +
  labs(x = "per capita immigration", 
       y = "per capita Federal Firearms Licenses")

# Regression Tree Model -------------------------------------------------------

library(rpart)
library(rpart.plot)
library(tree)

rownames(immigration) <- immigration$NAME

# remove state names and log FFL
immigration <- immigration %>%
  dplyr::select(-NAME, -logFFL)

# subset of immigration-related variables
immigration.sub <- immigration %>%
  mutate(.rownames = rownames(immigration),
         logIntlMigration = log10(INTERNATIONALMIG2014),
         log2014 = log10(y2014)) %>%
  dplyr::select(.rownames, perCapitaFFL, y2014, y2014.perCapita, 
                logIntlMigration, log2014,
                intl.migration.perCapita, INTERNATIONALMIG2014)


# `rpart` model on full immigration data
immigration.tree.a <- rpart(perCapitaFFL ~ ., data = immigration)
rpart.plot(immigration.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# `tree` model on full immigration data
immigration.tree.a2 <- tree(perCapitaFFL ~ ., data = immigration)
par(mfrow = c(1, 1), family = "GillSans")
plot(immigration.tree.a2)
text(immigration.tree.a2, pretty = 0)

# `rpart` model on immigration data subset
immigration.tree.b <- rpart(perCapitaFFL ~ .-.rownames, data = immigration.sub)
rpart.plot(immigration.tree.b, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# `tree` model on immigration data subset
immigration.tree.b2 <- tree(perCapitaFFL ~ .-.rownames, data = immigration.sub)
plot(immigration.tree.b2)
text(immigration.tree.b2, pretty = 0)

# Immigration: Decision Tree Splits -------------------------------------------

summary(immigration.sub$INTERNATIONALMIG2014)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     516    2741    7169   19480   21650  138800 

# per capita immigration histograms
immigration.sub %>%
  dplyr::select(.rownames, perCapitaFFL, y2014.perCapita, intl.migration.perCapita) %>%
  gather(type, value, 3:4) %>%
  group_by(type) %>%
  ggplot(aes(value)) + 
  geom_histogram(binwidth = 20, color = "black", fill = "white") +
  facet_wrap(~ type, scales = "free", ncol = 1)

# total immigration histograms
immigration.sub %>%
  dplyr::select(.rownames, perCapitaFFL, y2014, INTERNATIONALMIG2014) %>%
  gather(type, value, 3:4) %>%
  group_by(type) %>%
  ggplot(aes(value)) + 
  geom_histogram(binwidth = 1000, color = "black", fill = "white") +
  facet_wrap(~ type, scales = "free", ncol = 1)

# scatterplot with labels and decision-tree splits: rpart model
ggplot(immigration.sub, aes(logIntlMigration, intl.migration.perCapita, 
                           label = .rownames, size = perCapitaFFL)) +
  geom_segment(x = 3.348, xend = 3.348, y = 0, yend = 1000,
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = 3.348, xend = 10000, y = 220.5, yend = 220.5, 
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = 3.841, xend = 3.841, y = 220.5, yend = 0, 
               linetype = "dotted", color = "red3", size = 1) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", 
                        midpoint = 52, guide = F) +
  scale_size(name = "per capita FFLs", range = c(1, 20), guide = F) +
  scale_x_log10(breaks = seq(2, 6, by = 0.5)) +
  geom_text(aes(logIntlMigration,intl.migration.perCapita),
            position = "jitter", size = 3, 
            hjust = -0.1, vjust = 1.15,
            check_overlap = T, family = "GillSans") +
  pd.facet +
  expand_limits(x = c(2.5, 5.5)) +
  theme(panel.grid = element_blank()) +
  labs(x = "(log10) net international migration", 
       y = "per capita international migration",
       color = "per capita FFLs")

ggplot(immigration.sub, aes(logIntlMigration, perCapitaFFL, 
                            label = .rownames, size = perCapitaFFL)) +
  geom_segment(x = 3.348, xend = 3.348, y = 0, yend = 1000,
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = 3.348, xend = 6, y = 220.5, yend = 220.5, 
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = 3.841, xend = 3.841, y = 220.5, yend = 0, 
               linetype = "dotted", color = "red3", size = 1) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", 
                        midpoint = 52, guide = F) +
  scale_size(name = "per capita FFLs", range = c(1, 20), guide = F) +
  scale_x_log10(breaks = seq(2, 6, by = 0.5)) +
  geom_text(aes(logIntlMigration, perCapitaFFL),
            position = "jitter", size = 3, 
            hjust = -0.1, vjust = 1.15,
            check_overlap = T, family = "GillSans") +
  pd.facet +
  expand_limits(x = c(2.5, 5.5)) +
  theme(panel.grid = element_blank()) +
  labs(x = "(log10) net international migration", 
       y = "per capita Federal Firearms Licenses")


# Single Linear Regressions on Immigration Variables --------------------------

im01 <- tidy(lm(perCapitaFFL ~ y2014, data = immigration.sub))
im02 <- tidy(lm(perCapitaFFL ~ y2014.perCapita, data = immigration.sub))
im03 <- tidy(lm(perCapitaFFL ~ intl.migration.perCapita, data = immigration.sub))
im04 <- tidy(lm(perCapitaFFL ~ INTERNATIONALMIG2014, data = immigration.sub))

single.immigrations <- bind_rows(im01, im02, im03, im04) %>%
  filter(term != "(Intercept)") %>%
  arrange(p.value)

single.immigrations
#                       term      estimate    std.error statistic      p.value
# 1          y2014.perCapita -0.0839143551 0.0169882558 -4.939551 9.901620e-06
# 2 intl.migration.perCapita -0.0850352876 0.0176028836 -4.830759 1.431094e-05
# 3     INTERNATIONALMIG2014 -0.0003056767 0.0000907178 -3.369534 1.492890e-03
# 4                    y2014 -0.0002259620 0.0000756239 -2.987971 4.415590e-03

# Although the regression tree splits the data according first based on International Migration,
# single regressions show the highest p-value in 2014 per capita Immigrant data.



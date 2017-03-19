# ATF- FFL - ACS Industry Data
# Feature Selection + Engineering
# Census Electorate Characteristics & American Community Survey data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# dataset containing data by state for:
# - population by sex, race/origin
# - industry
# - working class
# - educational attainment
# - financial characteristics

# acs <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-all-data.csv", stringsAsFactors = F)
# ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)
# pop <- read.csv("~/GitHub/ATF-FFL/data/population-compact.csv", stringsAsFactors = F)

income <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-financePerCapita-all.csv", stringsAsFactors = F)
colnames(income)[3] <- "NAME"
income$X <- NULL
income$total.GEO.id2 <- NULL

# subset variables for modeling -----------------------------------------------
incomePerCapita <- income %>%
  dplyr::select(15:25, 38)

rownames(incomePerCapita) <- income$NAME
summary(incomePerCapita)

# Model 00: Baseline linear regressions ---------------------------------------

# all variables
mod.00 <- lm(perCapitaFFL ~ ., incomePerCapita)
summary(mod.00)
plot(mod.00)

# significant variables
mod.01 <- lm(perCapitaFFL ~ perCapita.LessThan5000 + perCapita.20000to24999 +
               perCapita.100000to149999 + perCapita.150000.or.more, data = incomePerCapita)
summary(mod.01)

# 20k-25k only
mod.02 <- lm(perCapitaFFL ~ perCapita.20000to24999, incomePerCapita)
summary(mod.02)

# Model 01: Robust Regression -------------------------------------------------

income.01 <- rlm(perCapitaFFL ~ ., data = incomePerCapita)
summary(income.01)

# check weights
huber01 <- data.frame(.rownames = income$NAME, 
                             .resid = income.01$resid,
                             weight = income.01$w) %>% arrange(weight)

huber01

# join with fitted and observed data
income.huber.01 <- augment(income.01) %>%
  left_join(huber01) %>%
  arrange(weight) %>%
  mutate(weighted.resid = .resid * weight,
         weighted.fit = .fitted + weighted.resid)

# distibution of weighted residuals
ggplot(income.huber.01, aes(weighted.resid)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(x = "weighted residuals") +
  pd.scatter

# distibution of residuals
ggplot(income.huber.01, aes(.resid)) +
  geom_histogram(binwidth = 0.5, color = "black", fill = "white") +
  labs(x = "residuals") +
  pd.scatter

# plot - less than 5k ---------------------------------------------------------
ggplot(income.huber.01, aes(perCapita.LessThan5000, perCapitaFFL,
                            fill = weighted.fit, label = .rownames)) + 
  geom_point(size = 1.5) +
  geom_point(aes(perCapita.LessThan5000, weighted.fit), color = "firebrick3", 
             shape = 23, size = 4, alpha = 0.8, data = income.huber.01) +
  geom_text(aes(perCapita.LessThan5000, perCapitaFFL), 
            size = 3, hjust = 1, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_smooth(method = "loess", se = F, color = "deepskyblue4", 
              size = 0.25, linetype = "longdash") +
  geom_errorbar(aes(x = perCapita.LessThan5000, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                linetype = "dotted") +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite1", 
                       high = "firebrick4",
                       midpoint = 30, guide = F) +
  labs(x = "Income: Less than $5,000 (per capita)",
       title = "FFL ~ Income: Observed vs. Fitted Values (Income < $5,000)") +
  pd.scatter

# plot - more than 150k -------------------------------------------------------
ggplot(income.huber.01, aes(perCapita.150000.or.more, perCapitaFFL,
                            fill = weighted.fit, label = .rownames)) + 
  geom_point(size = 1.5) +
  geom_point(aes(perCapita.150000.or.more, weighted.fit), color = "firebrick3", 
             shape = 23, size = 4, alpha = 0.8, data = income.huber.01) +
  geom_text(aes(perCapita.150000.or.more, perCapitaFFL), 
            size = 3, hjust = 1, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_smooth(method = "loess", se = F, color = "deepskyblue4", 
              size = 0.25, linetype = "longdash") +
  geom_errorbar(aes(x = perCapita.150000.or.more, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                linetype = "dotted") +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite1", 
                       high = "firebrick4",
                       midpoint = 30, guide = F) +
  labs(x = "Income: over $150,000 (per capita)",
       title = "FFL ~ Income: Observed vs. Fitted Values (Income > $150,000)") +
  pd.scatter

# plot - 20-35k ---------------------------------------------------------------
ggplot(income.huber.01, aes(perCapita.20000to24999, perCapitaFFL,
                            fill = weighted.fit, label = .rownames)) + 
  geom_point(size = 1.5) +
  geom_point(aes(perCapita.20000to24999, weighted.fit), color = "firebrick3", 
             shape = 23, size = 4, alpha = 0.8, data = income.huber.01) +
  geom_text(aes(perCapita.20000to24999, perCapitaFFL), 
            size = 3, hjust = 1, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_smooth(method = "lm", se = F, color = "deepskyblue4", 
              size = 0.25, linetype = "longdash") +
  geom_errorbar(aes(x = perCapita.20000to24999, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                linetype = "dotted") +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite1", 
                       high = "firebrick4",
                       midpoint = 30, guide = F) +
  labs(x = "Income: $20,000 - $24,999",
       title = "Robust Model 01: Observed vs. Fitted Values, Huber Weighting") +
  pd.scatter

# merge brackets --------------------------------------------------------------

colnames(incomePerCapita)[1:11] <- gsub("perCapita", "pc", colnames(incomePerCapita)[1:11])

inc <- incomePerCapita %>%
  mutate(lessThan15k = pc.LessThan5000 + pc.5000to9999 + pc.10000to14999,
         to20k = pc.5000to9999 + pc.10000to14999 + pc.15000to19999,
         to75k = pc.20000to24999 + pc.25000to34999 + pc.35000to49999 + pc.50000to74999,
         to100k = pc.75000to99999,
         to150k = pc.100000to149999,
         moreThan150k = pc.150000.or.more)

inc.model <- inc %>%
  dplyr::select(perCapitaFFL, 13:18)

inc.mod00 <- lm(perCapitaFFL ~ ., inc.model)
summary(inc.mod00)

# The Pooerst and Wealthiest states will have less guns.
# States with the most poor and most rich will have less guns.

# remove outliers -------------------------------------------------------------

general.trend <- incomePerCapita[-c(2, 26, 50), ]
gt00 <- lm(perCapitaFFL ~ ., general.trend)
summary(gt00)

full.weight <- income.huber.01 %>% 
  filter(weight == 1) %>%
  dplyr::select(2:13) %>%
  do(tidy(lm(perCapitaFFL ~ ., data = .)))
  

full.weight <- income.huber.01 %>% 
  filter(weight == 1) %>%
  dplyr::select(1:13)

fw00 <- lm(perCapitaFFL ~ .-.rownames, full.weight)
summary(fw00)
plot(fw00)


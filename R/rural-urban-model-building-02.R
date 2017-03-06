# ATF - FFL
# Rural-Urban Proportions Model Building - 02
# Robust Regression

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(MASS)

# State Level Rural-Urban Proportions
# rural.urban <- read.csv("~/GitHub/ATF-FFL/data/PctUrbanRural_State.csv")

# per Capita FFLs 2016
# perCap16 <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita.csv")

# load plot themes
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# cleansed dataframe with FFL & Rural-Urban data
ffl.16 <- read.csv("~/GitHub/ATF-FFL/data/rural-urban-compact.csv")
str(ffl.16)
names(ffl.16)

rownames(ffl.16) <- ffl.16$NAME

# Robust Regression Model 01 --------------------------------------------------

# Linear Model 00 -------------------------------------------------------------

# model
rr00 <- lm(perCapitaFFL ~ POPPCT_UC + POPPCT_RURAL + AREA_RURAL + AREA_UC, data = ffl.16)
summary(rr00)

# look at coefficients and r-squared + p-values
tidy(rr00)
glance(rr00)

# plot the model
par(mfrow = c(2, 2))
plot(rr00)

# Robust Model 01: Huber ------------------------------------------------------

# model
summary(rr01 <- rlm(perCapitaFFL ~ POPPCT_UC + POPPCT_RURAL + 
                      AREA_RURAL + AREA_UC, data = ffl.16))

# check weights
huber01 <- data.frame(.rownames = ffl.16$NAME, 
                      .resid = rr01$resid,
                      weight = rr01$w) %>% arrange(weight)

huber01[1:12, ]

# join with fitted and observed data
rr.huber01 <- augment(rr01) %>%
  left_join(huber01) %>%
  arrange(weight) %>%
  mutate(weighted.resid = .resid * weight,
         weighted.fit = .fitted + weighted.resid)
  
rr.huber01

# distribution of weighted residuals
ggplot(rr.huber01, aes(weighted.resid)) +
  geom_histogram(binwidth = 8, color = "black", fill = NA) +
  labs(x = "weighted residuals") +
  pd.scatter

# compute distances between observed and weighted fit
# rr.huber01 <- rr.huber01 %>% 
#   mutate(distance = perCapitaFFL - weighted.fit)

# plot weighted fit values
ggplot(rr.huber01, aes(POPPCT_UC, perCapitaFFL, fill = weighted.fit, label = .rownames)) + 
  geom_point(size = 1.5) +
  geom_text(aes(POPPCT_UC, perCapitaFFL), 
            size = 3, hjust = 1.15, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_point(aes(POPPCT_UC, weighted.fit), color = "firebrick3", 
             shape = 23, size = 4, alpha = 0.8, data = rr.huber01) +
  geom_smooth(method = "lm", se = F, color = "deepskyblue4", 
              size = 0.25, linetype = "longdash") +
  geom_errorbar(aes(x = POPPCT_UC, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                    linetype = "dotted") +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite1", 
                       high = "firebrick4",
                       midpoint = 40, guide = F) +
  labs(x = "percentage of population living in Urban Clusters",
       title = "Robust Model 01: Observed vs. Fitted Values, Huber Weighting") +
  pd.scatter

rr01.coef <- tidy(rr01)
summary(rr.huber01$weighted.fit)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.687  20.070  26.100  30.490  38.780  81.820

# plot fitted values with regression line, fill mapped to fit value
ggplot(rr.huber01, aes(POPPCT_UC, weighted.fit, 
                       fill = weighted.fit, 
                       label = .rownames)) + 
  geom_point(color = "firebrick3", shape = 23, size = 3.5, 
             alpha = 0.8, data = rr.huber01) +
  geom_point(aes(POPPCT_UC, perCapitaFFL), shape = 1, size = 1.25) +
  geom_text(aes(POPPCT_UC, weighted.fit), 
            size = 3, hjust = 1.15, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_smooth(method = "lm", se = F, color = "deepskyblue4", 
              size = 0.5, linetype = "dashed") +
  geom_errorbar(aes(x = POPPCT_UC, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                linetype = "dotted", lineend = "butt") +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite1", 
                       high = "firebrick3",
                       midpoint = 40, guide = F) +
  labs(x = "percentage of population living in Urban Clusters",
       title = "Robust Model 01: Weighted Fit Values (Huber)") +
  pd.scatter


# Robust Model 02 -------------------------------------------------------------

# model - bisquare weighted
summary(rr02 <- rlm(perCapitaFFL ~ POPPCT_UC + POPPCT_RURAL + AREA_RURAL + AREA_UC, 
                    data = ffl.16, method = "MM"))

# check weights
bisquare01 <- data.frame(.rownames = ffl.16$NAME, 
                         .resid = rr02$resid, 
                         weight = rr02$w) %>% arrange(weight)

bisquare01

# join with fitted and observed data
rr.bisquare01 <- augment(rr02) %>%
  left_join(bisquare01) %>%
  arrange(weight) %>%
  mutate(weighted.resid = .resid * weight,
         weighted.fit = .fitted + weighted.resid)
  
rr.bisquare01

par(mfrow = c(2, 2), familiy = "GillSans")
plot(rr02)

# plot bisquare-fitted vs observed
ggplot(rr.bisquare01, aes(POPPCT_UC, perCapitaFFL, fill = weighted.fit, label = .rownames)) + 
  geom_point(size = 1.5) +
  geom_text(aes(POPPCT_UC, perCapitaFFL), 
            size = 3, hjust = 1.15, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_point(aes(POPPCT_UC, weighted.fit), 
             shape = 23, size = 4, alpha = 0.8, 
             data = rr.bisquare01) +
  geom_smooth(method = "lm", se = F, color = "deepskyblue4", 
              size = 0.25, linetype = "dashed") +
  geom_errorbar(aes(x = POPPCT_UC, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                linetype = "dotted") +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite1", 
                       high = "firebrick3",
                       midpoint = 40, guide = F) +
  labs(x = "percentage of population living in Urban Clusters",
       title = "Robust Model 02: Observed vs. Fitted Values, Bisquare Weighting") +
  pd.scatter

rr02.coef <- tidy(rr02)
summary(rr.bisquare01$weighted.fit)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.912  19.410  26.010  29.460  38.150  81.890

# plot fitted with regression line, fill mapped to fit value
ggplot(rr.bisquare01, aes(POPPCT_UC, weighted.fit, 
                       fill = weighted.fit, 
                       label = .rownames)) + 
  geom_point(color = "firebrick3", shape = 23, size = 3.5, 
             alpha = 0.8, data = rr.bisquare01) +
  geom_text(aes(POPPCT_UC, weighted.fit), 
            size = 3, hjust = 1.15, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_smooth(method = "lm", se = F, color = "deepskyblue4", 
              size = 0.5, linetype = "dashed") +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite1", 
                       high = "firebrick3",
                       midpoint = 40, guide = F) +
  labs(x = "percentage of population living in Urban Clusters",
       title = "Robust Model 02: Weighted Fit Values (Bisquare)") +
  pd.scatter

# plot fitted with regression line, no color fill
ggplot(rr.bisquare01, aes(POPPCT_UC, weighted.fit, 
                          label = .rownames)) + 
  geom_point(color = "firebrick3", shape = 23, size = 3.5, 
             alpha = 0.8, data = rr.bisquare01) +
  geom_text(aes(POPPCT_UC, weighted.fit), 
            size = 3, hjust = 1.15, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_smooth(method = "lm", se = F, color = "deepskyblue4", 
              size = 0.5, linetype = "dashed") +
  labs(x = "percentage of population living in Urban Clusters",
       title = "Robust Model 02: Weighted Fit Values (Bisquare)") +
  pd.scatter

# Bootstrap Regressions -------------------------------------------------------

# Bootstrap Regression 01: Tidy Huber -----------------------------------------

# construct 100 bootstrap replications of the Robust Huber Model 01
bootHub01 <- ffl.16 %>%
  bootstrap(1000) %>%
  do(tidy(rlm(perCapitaFFL ~ POPPCT_UC + POPPCT_RURAL + 
                AREA_RURAL + AREA_UC, data = .)))

bootHub01

# calculate confidence intervals: percentile method
# group observations by term (variable)
# use quantile() on coefficients, 
# 2.5% = 0.5 / 2, 97.5% = 1 - 0.5/2
alpha = .05
bootHub01 %>%
  group_by(term) %>%
  summarize(low = quantile(estimate, alpha / 2),
            high = quantile(estimate, 1 - alpha / 2))

# histogram of confidence intervals
bootHub01 %>% 
  filter(term != "(Intercept)") %>%
  ggplot(aes(estimate)) +
  geom_histogram(binwidth = 0.1, color = "black", fill = NA) +
  facet_wrap(~ term, scales = "free") +
  pd.theme

bh.coef <- bootHub01 %>% 
  filter(term != "(Intercept)")

summary(bh.coef$estimate)
summary(bh.coef$std.error)

# look at residuals, fitted values
bootHub01b <- ffl.16 %>% 
  bootstrap(1000) %>%
  do(augment(rlm(perCapitaFFL ~ POPPCT_UC + POPPCT_RURAL + 
                   AREA_RURAL + AREA_UC, data = .)))

ggplot(bootHub01b, aes(POPPCT_UC, perCapitaFFL)) +
  geom_point() +
  geom_line(aes(POPPCT_UC, y = .fitted, group = replicate), alpha = 0.1) +
  geom_smooth(method = "lm") +
  pd.scatter

# Bootstrap Regression 02: Boot Huber -----------------------------------------

library(boot)

# r-squared function
rsq <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- lm(formula, data=d)
  return(summary(fit)$r.square)
}

# boot Huber function
boot.huber <- function(X, i, maxit = 30) {
  
  ## Select observations by row numbers
  X <- X[i, ]
  
  ## Fit model
  res.rlm <- rlm(perCapitaFFL ~ POPPCT_UC + POPPCT_RURAL + AREA_RURAL + AREA_UC, 
                 data = X, maxit = maxit)
  
  ## Return coefficient vector
  coefficients(res.rlm)
}

# bootstrap
bootHub02 <- boot(data = ffl.16, statistic = boot.huber, R = 1000)

bootHub02
plot(bootHub02)
boot.ci(bootHub02, type = "bca")

summary(rr01)
bootHub02

# Bootstrapping Mulitple Statistics -------------------------------------------

# function to obtain regression weights
bs <- function(formula, data, indices) {
  d <- data[indices,] # allows boot to select sample 
  fit <- rlm(formula, data = d)
  return(coef(fit)) 
}

bootHub03 <- boot(data = ffl.16, statistic = bs, 
                R = 1000, formula = perCapitaFFL ~ 
                  POPPCT_UC + POPPCT_RURAL + AREA_RURAL + AREA_UC)

bootHub03
plot(bootHub03)
par(mfrow = c(2, 2))
plot(bootHub03, index = 1)
plot(bootHub03, index = 2)
plot(bootHub03, index = 3)
plot(bootHub03, index = 4)

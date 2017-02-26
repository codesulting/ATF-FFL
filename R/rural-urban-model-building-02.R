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

# Robust Model 01 -------------------------------------------------------------

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
ggplot(rr.huber01, aes(POPPCT_UC, perCapitaFFL, label = .rownames)) + 
  geom_point(size = 1.5) +
  geom_text(aes(POPPCT_UC, perCapitaFFL), 
            size = 3, hjust = 1.15, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_point(aes(POPPCT_UC, weighted.fit), color = "firebrick3", 
             shape = 23, size = 4, alpha = 0.8, data = rr.huber01) +
  geom_errorbar(aes(x = POPPCT_UC, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                    linetype = "dotted") +
  labs(x = "percentage of population living in Urban Clusters",
       title = "Robust Model 01: Observed vs. Fitted Values, Huber Weighting") +
  pd.scatter

rr01.coef <- tidy(rr01)
summary(rr.huber01$weighted.fit)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.687  20.070  26.100  30.490  38.780  81.820

# plot fitted with regression line, fill mapped to fit value
ggplot(rr.huber01, aes(POPPCT_UC, weighted.fit, 
                       fill = weighted.fit, 
                       label = .rownames)) + 
  geom_point(color = "firebrick3", shape = 23, size = 3.5, 
             alpha = 0.8, data = rr.huber01) +
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
       title = "Robust Model 01: Weighted Fit Values (Huber)") +
  pd.scatter

# plot fitted with regression line, no color fill
ggplot(rr.huber01, aes(POPPCT_UC, weighted.fit, label = .rownames)) + 
  geom_point(color = "firebrick3", shape = 23, size = 3.5, 
             alpha = 0.8, data = rr.huber01) +
  geom_text(aes(POPPCT_UC, weighted.fit), 
            size = 3, hjust = 1.15, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_smooth(method = "lm", se = F, color = "deepskyblue4", 
              size = 0.5, linetype = "dashed") +
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
ggplot(rr.bisquare01, aes(POPPCT_UC, perCapitaFFL, label = .rownames)) + 
  geom_point(size = 1.5) +
  geom_text(aes(POPPCT_UC, perCapitaFFL), 
            size = 3, hjust = 1.15, vjust = -0.65, 
            check_overlap = T, family = "GillSans") +
  geom_point(aes(POPPCT_UC, weighted.fit), color = "firebrick3", 
             shape = 23, size = 4, alpha = 0.8, data = rr.bisquare01) +
  geom_errorbar(aes(x = POPPCT_UC, 
                    ymin = weighted.fit, 
                    ymax = perCapitaFFL), 
                linetype = "dotted") +
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











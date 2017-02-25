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
  mutate(weighted.resid = .resid * weight)

rr.huber01



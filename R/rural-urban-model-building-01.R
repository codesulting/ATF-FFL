# ATF - FFL
# Rural-Urban Proportions Model Building - 01
# https://www.census.gov/geo/reference/ua/urban-rural-2010.html
# https://www.census.gov/geo/reference/ua/ualists_layout.html

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

# Model Building --------------------------------------------------------------

# mddel 01 - Do Rural and Urban Cluster Population Percentages and Land Area 
# have a relationship with per capita FFL counts? 

rural.urban.01 <- lm(perCapitaFFL ~ 
                       POPPCT_RURAL + POPPCT_UC + 
                       AREAPCT_RURAL + AREAPCT_UC + 
                       AREA_RURAL + AREA_UC, data = ffl.16)

summary(rural.urban.01)
tidy(rural.urban.01)
glance(rural.urban.01)[, c(1:5, 8, 10)]

ru.01.fit <- augment(rural.urban.01)
ru.01.fit 

# histogram of residuals
ggplot(ru.01.fit, aes(.resid)) +
  geom_histogram(binwidth = 8, color = "black", fill = "white") +
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Distribution of Residuals - lm `rural.urban.01`")

# histogram of per capita FFL counts
ggplot(ffl.16, aes(perCapitaFFL)) +
  geom_histogram(binwidth = 8, color = "black", fill = "white") +
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Distribution of FFLs per capita, by state")

# plot of model 01
par(mfrow = c(2, 2))
plot(rural.urban.01)

# plot fitted vs observed
ggplot(ru.01.fit, aes(perCapitaFFL, reorder(.rownames, perCapitaFFL))) + 
  geom_point(size = 2) + pd.theme + 
  geom_errorbarh(aes(xmin = .fitted, xmax = perCapitaFFL), data = ru.01.fit,
                 color = "gray50", alpha = 0.90, height = 0.70) +
  geom_point(aes(.fitted, reorder(.rownames, .fitted)), 
             color = "firebrick3", size = 2, data = ru.01.fit) +
  labs(y = "", x = "fitted & observed per capita FFLs",
       title = "Per Capita FFL ~ Rural & UrbanCluster Population Percentages + Land Area")

# Looking at Residuals vs. Leverage: Alaska, Wyoming, and Delaware 
# seem to be exerting outsize influence on the model.
# How much of an influence can be observed? 
# Model 02 --------------------------------------------------------------------

ffl.16.2 <- ffl.16 %>%
  filter(NAME != "Alaska" & NAME != "Wyoming" & NAME != "Delaware")

rural.urban.02 <- update(rural.urban.01, subset = NAME != "Alaska" & 
                           NAME != "Wyoming" & 
                           NAME != "Montana")

summary(rural.urban.02)
summary(rural.urban.01)

tidy(rural.urban.02)
ru.02.fit <- augment(rural.urban.02)

# plot model 02 residuals
ggplot(ru.02.fit, aes(.resid)) + pd.theme +
  geom_histogram(binwidth = 8, color = "black", fill = "white") +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Distibution of Residuals: lm `rural.urban.02`")
  
# plot of model 02
par(mfrow = c(2, 2))
plot(rural.urban.02)

# plot fitted vs observed by state
ggplot(ru.02.fit, aes(perCapitaFFL, reorder(.rownames, perCapitaFFL))) + 
  geom_point(size = 2) + pd.theme + 
  geom_errorbarh(aes(xmin = .fitted, xmax = perCapitaFFL), data = ru.02.fit,
                 color = "gray50", alpha = 0.90, height = 0.70) +
  geom_point(aes(.fitted, reorder(.rownames, .fitted)), 
             color = "firebrick3", size = 2, data = ru.02.fit) +
  labs(y = "", x = "fitted & observed per capita FFLs",
       title = "Per Capita FFL ~ Rural & Urban Cluster Population Percentages + Land Area")

# plot fitted + observed  by 
ggplot(ru.02.fit, aes(POPPCT_RURAL, perCapitaFFL, label = .rownames)) + geom_point() +
  geom_text(aes(POPPCT_RURAL, perCapitaFFL), size = 2,
            hjust = -0.1, vjust = -0.1, check_overlap = T) +
  geom_point(aes(POPPCT_RURAL, .fitted, label = .rownames), 
             color = "firebrick3", shape = 5, size = 2, data = ru.02.fit) +
  geom_text(aes(POPPCT_RURAL, .fitted), size = 2, color = "firebrick3",
            hjust = -0.1, vjust = -0.1, check_overlap = T) +
  geom_smooth(stat = "smooth", method = "lm", se = F, color = "deepskyblue3") +
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Fitted and Observed Values: lm `rural.urban.02`",
       x = "Rural Population Percentage")
  


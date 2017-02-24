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

# plot fitted vs observed BY STATE
ggplot(ru.01.fit, aes(perCapitaFFL, reorder(.rownames, perCapitaFFL))) + 
  geom_point(size = 2) + pd.theme + 
  geom_errorbarh(aes(xmin = .fitted, xmax = perCapitaFFL), data = ru.01.fit,
                 color = "gray50", alpha = 0.90, height = 0.70) +
  geom_point(aes(.fitted, reorder(.rownames, .fitted)), 
             color = "firebrick3", size = 2, data = ru.01.fit) +
  labs(y = "", x = "fitted & observed per capita FFLs",
       title = "Per Capita FFL ~ Rural & UrbanCluster Population Percentages + Land Area")

# plot fitted + observed by % of population in Urban Clusters
ggplot(ru.01.fit, aes(POPPCT_UC, perCapitaFFL, label = .rownames)) + 
  geom_point() +
  geom_text(aes(POPPCT_UC, perCapitaFFL), 
            size = 2, 
            hjust = -0.1, vjust = -0.1, 
            check_overlap = T) +
  geom_point(aes(POPPCT_UC, .fitted, label = .rownames), 
             color = "firebrick3", shape = 1, size = 2, 
             data = ru.01.fit) +
  geom_text(aes(POPPCT_UC, .fitted), 
            size = 2, color = "firebrick3",
            hjust = -0.1, vjust = -0.1, 
            check_overlap = T) +
  geom_smooth(stat = "smooth", method = "lm", se = F, 
              color = "deepskyblue3", size = 0.25,
              linetype = "dashed") +
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Fitted and Observed Values: lm `rural.urban.01`",
       x = "Percentage of Population living in Urban Clusters")

# plot fitted + observed by Rural Land Area
ggplot(ru.01.fit, aes(log(AREA_RURAL), perCapitaFFL, label = .rownames)) + 
  geom_point() +
  geom_text(aes(log(AREA_RURAL), perCapitaFFL), 
            size = 2, 
            hjust = -0.1, vjust = -0.1, 
            check_overlap = T) +
  geom_point(aes(log(AREA_RURAL), .fitted, label = .rownames), 
             color = "firebrick3", shape = 5, size = 2, 
             data = ru.01.fit) +
  geom_text(aes(log(AREA_RURAL), .fitted), 
            size = 2, color = "firebrick3",
            hjust = -0.1, vjust = -0.1, 
            check_overlap = T) +
  geom_smooth(method = "lm", se = F, color = "deepskyblue3", 
              size = 0.25, linetype = "dashed") +
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Fitted and Observed Values: lm `rural.urban.01`",
       x = "log(Rural Land Area)")


# plot fitted + observed  
ggplot(ru.01.fit, aes(POPPCT_UC, perCapitaFFL, label = .rownames)) + 
  geom_point() +
  geom_text(aes(POPPCT_UC, perCapitaFFL), 
            size = 2.65, 
            hjust = -0.07, vjust = -0.1, 
            check_overlap = T) +
  geom_smooth(stat = "smooth", method = "lm", se = F, 
              color = "deepskyblue3", size = 0.25,
              linetype = "dashed") +
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Per Capita FFLs ~ Percentage of Population in Urban Clusters",
       x = "Percentage of Population living in Urban Clusters")

# plot observed vs Area Rural
ggplot(ru.01.fit, aes(log(AREA_RURAL), perCapitaFFL, label = .rownames)) + 
  geom_point() +
  geom_text(aes(log(AREA_RURAL), perCapitaFFL), 
            size = 2.65, 
            hjust = -0.1, vjust = -0.1, 
            check_overlap = T) +
  geom_smooth(method = "lm", se = F, color = "deepskyblue3", 
              size = 0.25, linetype = "dashed") +
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Per Capita FFLs by Rural Land Area",
       x = "log(Rural Land Area)")

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

# plot fitted + observed by POPPCT_UC
ggplot(ru.02.fit, aes(POPPCT_UC, perCapitaFFL, label = .rownames)) + geom_point() +
  geom_text(aes(POPPCT_UC, perCapitaFFL), size = 2,
            hjust = -0.1, vjust = -0.1, check_overlap = T) +
  geom_point(aes(POPPCT_UC, .fitted, label = .rownames), 
             color = "firebrick3", shape = 5, size = 2, data = ru.02.fit) +
  geom_text(aes(POPPCT_UC, .fitted), size = 2, color = "firebrick3",
            hjust = -0.1, vjust = -0.1, check_overlap = T) +
  geom_smooth(stat = "smooth", method = "lm", se = F, 
              color = "deepskyblue3", size = 0.5) +
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Fitted and Observed Values: lm `rural.urban.02`",
       x = "Percentage of Population living in Urban Clusters")

# plot fitted + observed vs Area Rural
ggplot(ru.02.fit, aes(log(AREA_RURAL), perCapitaFFL, label = .rownames)) + geom_point() +
  geom_text(aes(log(AREA_RURAL), perCapitaFFL), size = 2,
            hjust = -0.1, vjust = -0.1, check_overlap = T) +
  geom_point(aes(log(AREA_RURAL), .fitted, label = .rownames), 
             color = "firebrick3", shape = 5, size = 2, data = ru.02.fit) +
  geom_text(aes(log(AREA_RURAL), .fitted), size = 2, color = "firebrick3",
            hjust = -0.1, vjust = -0.1, check_overlap = T) +
  geom_smooth(stat = "smooth", method = "lm", se = F, 
              color = "deepskyblue3", size = 0.5) +
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Fitted and Observed Values: lm `rural.urban.02`",
       x = "log(Rural Land Area)")

# Reduced Model ---------------------------------------------------------------

rural.urban.03 <- lm(perCapitaFFL ~ POPPCT_UC + AREA_RURAL, data = ffl.16)
summary(rural.urban.03)
plot(rural.urban.03)

ru.03.fit <- augment(rural.urban.03)

# histogram of residuals
ggplot(ru.03.fit, aes(.resid)) +
  geom_histogram(binwidth = 8, color = "black", fill = "white") +
  geom_histogram(aes(.std.resid), binwidth = 1,
                 color = "black", fill = "red", alpha = 0.25) +
  pd.theme + theme(axis.line = element_line(color = "black")) +
  labs(title = "Distribution of Residuals - lm `rural.urban.03`")

# plot fitted vs observed BY STATE
ggplot(ru.03.fit, aes(perCapitaFFL, reorder(.rownames, perCapitaFFL))) + 
  geom_point(size = 2) + pd.theme + 
  geom_errorbarh(aes(xmin = .fitted, xmax = perCapitaFFL), data = ru.03.fit,
                 color = "gray50", alpha = 0.90, height = 0.70) +
  geom_point(aes(.fitted, reorder(.rownames, .fitted)), 
             color = "firebrick3", size = 2, data = ru.03.fit) +
  labs(y = "", x = "fitted & observed per capita FFLs",
       title = "Per Capita FFL ~ Urban Cluster Population % + Rural Land Area")

# Compare all 3 models --------------------------------------------------------

g01 <- glance(rural.urban.01)
g02 <- glance(rural.urban.02)
g03 <- glance(rural.urban.03)

model.comparison <- bind_rows(g01, g02, g03) %>%
  mutate(model = 1:3) %>%
  print


ggplot(model.comparison, aes(model, adj.r.squared)) +
  geom_bar(stat = "identity", color = "black", fill = "white") + 
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(y = "adjusted R-squared")

model.comparison <- glance(rural.urban.01) %>%
  bind_rows(glance(rural.urban.02), 
            glance(rural.urban.03)) %>%
  mutate(model = 1:3) %>%
  print
 
# check collinearity on first model
library(rms)
library(corrplot)

vif(rural.urban.01)
sqrt(vif(rural.urban.01))

model.01.cor <- ru.01.fit %>%
  select(3:8) %>%
  cor(.)

par(mfrow = c(1, 1), family = "GillSans")
corrplot(model.01.cor, method = "shade", shade.col = NA,
         tl.col = "gray23", tl.srt = 45, tl.cex = 1, 
         addCoef.col = "black", number.cex = 1,
         order = "hclust", mar = c(1, 1, 1, 1))

# Inverse Population Model ----------------------------------------------------


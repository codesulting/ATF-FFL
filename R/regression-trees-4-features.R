# Federal Firearms License data
# Regression Trees: Characteristics

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(broom)
library(rpart)
library(rpart.plot)
library(tree)
library(ggplot2)
library(ggdendro)

# load themes and functions
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
setwd("~/GitHub/ATF-FFL")
edu.perCapita <- read.csv("data/per-capita-clean/per-capita-education.csv")
income.perCapita <- read.csv("data/per-capita-clean/per-capita-income.csv")
industry.perCapita <- read.csv("data/per-capita-clean/per-capita-industry.csv")
legislative.perCapita <- read.csv("data/per-capita-clean/per-capita-legislative.csv")

pop <- read.csv("data/rural-urban-compact.csv")

all.features <- pop %>%
  left_join(edu.perCapita) %>%
  left_join(income.perCapita) %>%
  left_join(industry.perCapita) %>%
  left_join(legislative.perCapita)


write.csv(all.features, file = "data/per-capita-all.csv", row.names = F)

# clean up rownames -----------------------------------------------------------

rownames(edu.perCapita) <- edu.perCapita$X
rownames(income.perCapita) <- income.perCapita$X
rownames(industry.perCapita) <- industry.perCapita$X
rownames(legislative.perCapita) <- legislative.perCapita$X

edu.perCapita$X <- NULL
income.perCapita$X <- NULL
industry.perCapita$X <- NULL
legislative.perCapita$X <- NULL

four.features <- edu.perCapita %>%
  select(perCapitaFFL, contains("BA"), contains("HS")) %>%
  left_join(income.perCapita) %>%
  left_join(industry.perCapita) %>%
  left_join(legislative.perCapita)

feature.3 <- edu.perCapita %>%
  select(perCapitaFFL, contains("BA"), contains("HS")) %>%
  left_join(income.perCapita) %>%
  left_join(legislative.perCapita)

# Regression Trees ------------------------------------------------------------

# Four Feature Regression ------------------------------------------------------
# Regression Trees fit to 4 datasets on population characteristics:
# Annual Household Income, Educational Attainment, Workforce Industry, and State Government

# rpart - tree a
four.tree.a <- rpart(perCapitaFFL ~ ., data = four.features)

rpart.plot(four.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# tree- tree b
four.tree.b <- tree(perCapitaFFL ~ ., data = four.features)
par(family = "GillSans", cex = 0.85)
plot(four.tree.b)
text(four.tree.b, pretty = 0)

# Agriculture, Hunting & Fishing, and Mining remain the strongest features.
# Democratic Legislative Control comes next, 
# then per capita income for the middle class.

# Three Feature Regression Tree -----------------------------------------------
# Regression Trees fit to 3 datasets on population characteristics:
# Annual Household Income, Educational Attainment, and State Government

# `rpart` regression tree
three.tree.a <- rpart(perCapitaFFL ~ ., data = feature.3)

rpart.plot(three.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# `tree` regression tree
three.tree.b <- tree(perCapitaFFL ~ ., data = feature.3)
plot(three.tree.b)
text(three.tree.b, pretty = 0) 

# Having a Bachlor's Degree, especially young (right out of high school) 
# generally corresponds to a lower FFL count. States in the bottom quantile
# of 18 to 24 year olds with a BA have a higher mean FFL count per capita.

summary(feature.3$pc.18to24.BA)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   442.5   738.4   931.0   961.9  1164.0  1787.0

summary(feature.3$perCapita.35000to49999)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   3411    4679    5249    5087    5601    6224

summary(feature.3$perCapita.75000to99999)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   3990    4326    4711    4749    5098    5780 

# If the middle class population is less than the national median...
# If the upper middle class population is above the ~90th percentile...

# Two Features Each: Regression Trees -----------------------------------------

# How do state legislature and annual income features interact? 
# How do state legislature and educational attainment features interact? 
# How do educational attainment and annual income features interact? 
# How do educational attainment and workforce industry features interact? 

# State Government and Annual Household Income --------------------------------
# How do state legislature and annual income features interact? 

state.plus.income <- legislative.perCapita %>%
  left_join(income.perCapita)

# `rpart` tree model
state.income.tree.a <- rpart(perCapitaFFL ~ ., data = state.plus.income)

rpart.plot(state.income.tree.a , type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# `tree` tree model
state.income.tree.b <- tree(perCapitaFFL ~ ., data = state.plus.income)
plot(state.income.tree.b)
text(state.income.tree.b, pretty = 0)

# State Government and Educational Attainment ---------------------------------
# How do state legislature and educational attainment features interact? 

state.edu <- edu.perCapita %>%
  select(perCapitaFFL, contains("HS"), contains("BA")) %>%
  left_join(legislative.perCapita)

# `rpart` model
state.edu.tree.a <- rpart(perCapitaFFL ~ ., data = state.edu)

rpart.plot(state.edu.tree.a , type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# `tree` tree model
state.edu.tree.b <- tree(perCapitaFFL ~ ., data = state.edu)
plot(state.edu.tree.b) 
text(state.edu.tree.b, pretty = 0)

# Balances in the State House and Senate, when combined in the right thresholds by party - 
# can lead to lower FFL counts:
# e.g. More than 22 Senate Dems and less than 42 House Republicans

# Income and Educational Attainment -------------------------------------------
# How do educational attainment and annual income features interact? 

income.education <- edu.perCapita %>%
  select(perCapitaFFL, contains("HS"), contains("BA")) %>%
  left_join(income.perCapita)

# `rpart` model 
income.edu.tree.a <- rpart(perCapitaFFL ~ ., data = income.education)

rpart.plot(income.edu.tree.a , type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# `tree` tree model
income.edu.tree.b <- tree(perCapitaFFL ~ ., data = income.education)
plot(income.edu.tree.b)
text(income.edu.tree.b, pretty = 0)

# Income and Industry ---------------------------------------------------------
# How do educational attainment and workforce industry features interact? 

income.industry <- income.perCapita %>%
  left_join(industry.perCapita)

# `rpart` tree model
income.industry.tree.a <- rpart(perCapitaFFL ~ ., data = income.industry)

rpart.plot(income.industry.tree.a , type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# `tree` tree model
income.industry.tree.b <- tree(perCapitaFFL ~ ., data = income.industry)
plot(income.industry.tree.b)
text(income.industry.tree.b, pretty = 0)

# industry continues to be an overpowering predictor.


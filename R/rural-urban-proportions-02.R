# ATF - FFL
# Rural-Urban Proportions Exploration
# https://www.census.gov/geo/reference/ua/urban-rural-2010.html

# Rural-Urban Proportion data from the 2010 census
# Two .xls files were downloaded: 
# one with state-level data, the other with county-level data.
# The link above is the source. 
# The links below have specific Rural and Urban datasets:

# https://www.census.gov/geo/reference/ua/ualists_layout.html
# 

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(corrplot)
library(scales)

# State Level Rural-Urban Proportions
rural.urban <- read.csv("~/GitHub/ATF-FFL/data/PctUrbanRural_State.csv")
summary(rural.urban)
str(rural.urban)
names(rural.urban)

# per Capita FFLs 2016
perCap16 <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita.csv")
str(perCap16)

# load plot themes
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# Transforms + Wrangling ------------------------------------------------------

# Merge with Population and per 100k data
# remove DC and PR
rural.urban <- rural.urban[-c(9,52), ]
colnames(rural.urban)[2] <- "NAME"
rural.urban$NAME <- factor(rural.urban$NAME)

ffl.16 <- left_join(perCap16, rural.urban, by = "STATE")
str(ffl.16)

ffl.16$NAME.y <- NULL
colnames(ffl.16)[1] <- "NAME"
# This has combined ATF FFL data, US Census Population data, and Rural-Urban data

# Model Building --------------------------------------------------------------

library(broom)

# Model 01 --------------------------------------------------------------------
# model.01 looks at variables that appear to be correlated to monthly per capita FFL counts.

# sort states alphabetically
ffl.16 <- ffl.16 %>%
  arrange(NAME)

# fit model
# the STATE variable is a number/index that can be matched to state NAME;
# i.e. should not have predictive power
model.01 <- lm(perCapitaFFL ~ STATE + POPPCT_UC + POPPCT_UA + POPPCT_RURAL + POP_UC 
               + POP_UA + POP_RURAL + AREA_UC + AREA_UA + AREA_RURAL, data = ffl.16)

summary(model.01)

# an adjusted r^2 of 0.82 (0.8567 unadjusted) appears promising;
# AREA_RURAL appears to be the most significant predictor.
# AREA_UC and AREA_UA are the next most significant, but with much higher p-values. (0.0854/0.0692)
# But the variables in this model are highly correlated to the independent variable.

# look at coefficients, sorted by p-value
model.01.tidy <- tidy(model.01) %>%
  arrange(p.value)

model.01.tidy

# create key-value df for state names and numbers
state.key <- ffl.16 %>%
  select(NAME, STATE)

# store fitted values in dataframe, merge to include state names
model.01.fit <- augment(model.01) %>%
  arrange(desc(perCapitaFFL)) %>%
  left_join(state.key)

# plot observed and fitted perCapitaFFL counts
ggplot(model.01.fit, aes(x = reorder(NAME, perCapitaFFL), y = perCapitaFFL)) +
  geom_point() +
  geom_point(aes(x = reorder(NAME, perCapitaFFL), y = .fitted), 
             color = "red", shape = 15, size = 2, data = model.01.fit) +
  pd.theme + coord_flip() +
  labs(x = "State", y = "monthly per capita FFL count", 
       title = "Per Capita FFL count: Observed and Fitted Values - model.01")

# residuals 01
ggplot(model.01.fit, aes(x = reorder(NAME, .resid), y = .resid)) +
  geom_boxplot() + 
  geom_point(aes(x = reorder(NAME, .resid), y = .fitted), 
             color = "red", shape = 15, size = 2, data = model.01.fit) +
  pd.theme + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "", y = "residuals", title = "Per Capita FFL count: model.01 residuals")

# residuals 02
ggplot(model.01.fit, aes(.resid)) +
  geom_histogram(binwidth = 1, color = "black", fill = "grey99") +
  pd.theme

# model 01b -------------------------------------------------------------------

# normalize populations
ffl.16b <- ffl.16 %>%
  select(NAME, perCapitaFFL, AREA_ST, AREA_URBAN, AREA_UC, AREA_RURAL, POPESTIMATE2016,
         POP_ST, POP_URBAN, POP_UC, POP_RURAL) %>%
  mutate(pop.total.100k = POPESTIMATE2016/100000,
         pop.state.100k = POP_ST/100000,
         pop.urban.100k = POP_URBAN/100000,
         pop.uc.100k = POP_UC/100000,
         pop.rural.100k = POP_RURAL/100000)
  
# model 1b - stratifications of Land Area and perCap population
model.01b <- lm(perCapitaFFL ~ AREA_ST + AREA_URBAN + AREA_UC + AREA_RURAL + pop.total.100k + 
                  pop.state.100k + pop.urban.100k + pop.uc.100k + pop.rural.100k, data = ffl.16b)

summary(model.01b)

model.01c <- lm(perCapitaFFL ~ AREA_RURAL + POP_URBAN, data = ffl.16)
summary(model.01c)

model.01d <- lm(perCapitaFFL ~ AREA_RURAL + POP_URBAN + STATE + atf.Region, data = ffl.16)
summary(model.01d)

model.01e <- lm(perCapitaFFL ~ POP_RURAL + AREA_URBAN + STATE + atf.Region, data = ffl.16)
summary(model.01e)

model.01f <- lm(perCapitaFFL ~ POP_RURAL + AREA_RURAL + STATE + atf.Region, data = ffl.16)
summary(model.01f)

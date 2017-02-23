# ATF - FFL
# Rural-Urban Proportions Model Building
# https://www.census.gov/geo/reference/ua/urban-rural-2010.html
# https://www.census.gov/geo/reference/ua/ualists_layout.html

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

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

# reduce dataset
ffl.16 <- ffl.16 %>%
  select(-c(EstPop16.Wiki, EstPopPerHouseSeat, HouseRate, REGION, DIVISION,
            RBIRTH2016, RDEATH2016, RNATURALINC2016, RINTERNATIONALMIG2016, 
            RDOMESTICMIG2016, RNETMIG2016))

write.csv(ffl.16, file = "~/GitHub/ATF-FFL/data/rural-urban-compact.csv", row.names = F)

# Model Building --------------------------------------------------------------

ffl.16 <- read.csv("~/GitHub/ATF-FFL/data/rural-urban-compact.csv")

ffl.16m <- ffl.16
rownames(ffl.16m) <- ffl.16m$NAME
# create key-value df for state names and numbers
state.key <- ffl.16 %>%
  select(NAME, STATE)

# Population Percentages ------------------------------------------------------

# Is there a relationship between per capita FFLs and 
# the percentage of the population living in Rural, Urban Cluster, and Urban Areas? 
model.01 <- lm(perCapitaFFL ~ POPPCT_RURAL + POPPCT_UC + POPPCT_UA, data = ffl.16m)
summary(model.01)
glance(model.01)

# fitted values
m01.fit <- augment(model.01) %>%
  arrange(desc(perCapitaFFL)) %>%
  left_join(ffl.16m)

# coefficients
m01.coef <- tidy(model.01, conf.int = T)

# observed and fitted by state
ggplot(m01.fit, aes(perCapitaFFL, reorder(NAME, perCapitaFFL))) + geom_point() +
  geom_point(aes(.fitted, reorder(NAME, perCapitaFFL)), color = "firebrick3", data = m01.fit) +
  pd.theme + labs(y = "", x = "fitted & observed per capita FFLs",
                  title = "Per Capita FFL ~ Population Percentages:\nRural + Urban Cluster + Urbanized Areas")

# observed and fitted by population
ggplot(m01.fit, aes(POP_ST, perCapitaFFL, label = .rownames)) + geom_point() +
  geom_text(aes(POP_ST, perCapitaFFL), size = 1.25,
            hjust = -0.1, vjust = -0.1, check_overlap = T) +
  geom_point(aes(POP_ST, .fitted, label = .rownames), color = "firebrick3", data = m01.fit) +
  geom_text(aes(POP_ST, .fitted), size = 2, color = "firebrick3",
            hjust = -0.1, vjust = -0.1, check_overlap = T) +
  pd.theme +
  theme(axis.line = element_line(color = "black")) +
  labs(title = "Per Capita FFLs by Population (fitted and observed)",
       x = "population", y = "per Capita FFLs")

# Population Raw --------------------------------------------------------------

pop.m1 <- lm(perCapitaFFL ~ POP_RURAL + POP_UC + POP_UA, data = ffl.16m)
summary(pop.m1)

pop.m1.fit <- augment(pop.m1) %>%
  arrange(desc(perCapitaFFL)) %>%
  left_join(ffl.16m)

# fitted and observed values: Land Area linear model
ggplot(pop.m1.fit, aes(perCapitaFFL, reorder(NAME, perCapitaFFL))) + geom_point() +
  geom_point(aes(.fitted, reorder(NAME, perCapitaFFL)), color = "firebrick3", data = pop.m1.fit) +
  pd.theme + labs(y = "", x = "fitted & observed per capita FFLs",
                  title = "Per Capita FFL ~ Population:\nRural + Urban Cluster + Urbanized Area")

# Land Area Models ------------------------------------------------------------

land.area.m1 <- lm(perCapitaFFL ~ AREA_RURAL + AREA_UC + AREA_UA, data = ffl.16m)
summary(land.area.m1)
tidy(land.area.m1)

land.area.m1.fit <- augment(land.area.m1) %>%
  arrange(desc(perCapitaFFL)) %>%
  left_join(ffl.16m)

# fitted and observed values: Land Area linear model
ggplot(land.area.m1.fit, aes(perCapitaFFL, reorder(NAME, perCapitaFFL))) + geom_point() +
  geom_point(aes(.fitted, reorder(NAME, perCapitaFFL)), color = "firebrick3", data = land.area.m1.fit) +
  pd.theme + labs(y = "", x = "fitted & observed per capita FFLs",
                  title = "Per Capita FFL ~ Land Area:\nRural + Urban Cluster + Urbanized Area")


# Population PCT and Land Area ------------------------------------------------

land.poppct.m1 <- lm(perCapitaFFL ~ AREA_RURAL + AREA_UC + AREA_UA +
                       POPPCT_RURAL + POPPCT_UC + POPPCT_UA, data = ffl.16m)

summary(land.poppct.m1)
tidy()





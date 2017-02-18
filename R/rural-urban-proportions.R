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

# Correlations ----------------------------------------------------------------
# check correlation between variables
# although some variables will show high correlation, 
# there may be some unexpected correlations to look for.

# subset out categorical variable
rural.urban.corr <- rural.urban %>%
  select(-STATENAME)

# remove DC and PR; create corrleation matrix
rural.urban.corr <- rural.urban.corr[-c(9, 52), ]
rural.urban.corr <- cor(rural.urban.corr)

# plot correlation matrix
corrplot(rural.urban.corr, method = "ellipse", order = "hclust", hclust.method = "ward.D",
         tl.col = "black", tl.srt = 45, tl.cex = 1.0)

# plot correlation matrix w/ coefficients
par(mfrow = c(1, 1), family = "GillSans")
corrplot(rural.urban.corr, method = "shade", shade.col = NA, tl.col = "gray23",
         tl.srt = 45, tl.cex = 0.75, addCoef.col = "black", number.cex = 0.6,
         mar = c(2, 2, 2, 2))

# plot correlation matrix w/ coefficients and order by hierarchical clustering
par(mfrow = c(1, 1), family = "GillSans")
corrplot(rural.urban.corr, method = "shade", shade.col = NA, tl.col = "gray23",
         tl.srt = 45, tl.cex = 0.75, addCoef.col = "black", number.cex = 0.6,
         order = "hclust", mar = c(2, 2, 2, 2))


# Wrangle: Merge with Population and per 100k data ----------------------------

# remove DC and PR
rural.urban <- rural.urban[-c(9,52), ]
colnames(rural.urban)[2] <- "NAME"
rural.urban$NAME <- factor(rural.urban$NAME)
 
ffl.16 <- left_join(perCap16, rural.urban, by = "STATE")
str(ffl.16)

ffl.16$NAME.y <- NULL
colnames(ffl.16)[1] <- "NAME"

# create correlation matrix
ffl.cor16 <- ffl.16 %>%
  select(-NAME)

ffl.cor16 <- cor(ffl.cor16)

# define a new palette
ffl.pal <- colorRampPalette(c(muted("firebrick4"),
                              muted("firebrick2"),
                              "antiquewhite1",
                              "white",
                              "antiquewhite1",
                              "deepskyblue2",
                              "deepskyblue4"))

# plot correlation matrix
par(mfrow = c(1, 1), family = "GillSans")
corrplot(ffl.cor16, method = "shade", shade.col = NA,
         tl.col = "gray23", tl.srt = 45, tl.cex = 0.70, 
         addCoef.col = "black", number.cex = 0.50,
         order = "hclust", mar = c(1, 1, 1, 1))

# After examining the matrix, there's a certain number of variables 
# that might be meaningfully correlated to Monthly License Counts and
# Per100k FFLs. 

# Population and Area across the different stratifications:
# Urbanized Areas, Urban Clusters, and Rural Areas.

# Each has to do with the divide between Urban and Rural  
# - be it in population, population density, area - 
# but also in the space *between* Urban and Rural. 
# The US Census defines this as "Urban Cluster". 
# but especially the scale between. 

# Urbanized Areas are defined as having a population of over 50,000.
# Urban Clusters have a population 5,000 < n < 50,000.
# Rural Areas have a population less than 5,000.

# TODO: stratifying Urbanized Areas and Urbanized Clusters into further intervals.
# Lists of UAs and UCs:
# https://www.census.gov/geo/reference/ua/ualists_layout.html

# Additionally, there are spatial definitions that separate rural and urban. 
# A table in the article 'Life Off the Highway' shows the relationship between 
# Relationship Between Land Area and Population in the United States

# Filter: Correlated Variables ------------------------------------------------

# Filter for variables across the 3 population classes:
# Population Percentage, Population, Area
rural.urban.filter <- ffl.16 %>%
  select(STATE, LicCount, LicCountMonthly, perCapFFLyear, perCapitaFFL, 
         POPPCT_RURAL, POPPCT_UC, POPPCT_UA, POPPCT_URBAN, 
         POP_RURAL, POP_UC, POP_UA, AREA_RURAL, AREA_UC, AREA_URBAN,
         NPOPCHG_2016, NETMIG2016)

rural.urban.f.corr <- cor(rural.urban.filter)
# For raw average of monthly license counts, area and population of Urban Clusters
# show the highest positive correlation (r^2 = 0.90 & 0.88). 
# Area of Urbanized Areas and Rural Population also are strong (r^2 = 0.83 & 0.82)

# For monthly per capita FFL counts, Population Percentage of Urban Clusters
# show the highest positive correlation (r^2 = 0.82). Population Percentage 
# of Urbanized Areas shows strong negative correlation (-0.75)

corrplot(rural.urban.f.corr, method = "shade", shade.col = NA,
         tl.col = "gray23", tl.srt = 45, tl.cex = 0.85, 
         addCoef.col = "black", number.cex = 0.85,
         order = "hclust", mar = c(1, 1, 1, 1))

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

# Model 02 --------------------------------------------------------------------
# Will adding more variables increase our r^2, 
# or help identify significant variables? 

# with this many variables, many fail to converge
# removing those that resulted in singularity
ffl.16.02 <- ffl.16 %>%
  arrange(NAME) %>%
  select(-c(NAME, LicCountMonthly, POPESTIMATE2016, NATURALINC2016, NETMIG2016,
            RESIDUAL2016, RNATURALINC2016, RNETMIG2016, POP_UC, AREA_UC, RINTERNATIONALMIG2016,
            POP_RURAL, POPPCT_RURAL, AREA_RURAL, AREAPCT_RURAL, perCapFFLyear,
            LicCount))

model.02 <- lm(perCapitaFFL ~ ., data = ffl.16.02)
summary(model.02)

# Residual standard error: 8.516 on 19 degrees of freedom
# Multiple R-squared:  0.9397,	Adjusted R-squared:  0.8446 
# again, promising R-squared results, but rather high p-values.

# look at coefficients, sorted by p-value
model.02.tidy <- tidy(model.02) %>%
  arrange(p.value)

# store fitted values in dataframe, join to include state names
model.02.fit <- augment(model.02) %>%
  arrange(desc(perCapitaFFL)) %>%
  left_join(state.key)

# plot observed vs. fitted values (model.02)
ggplot(model.02.fit, aes(x = reorder(NAME, perCapitaFFL), y = perCapitaFFL)) + 
  geom_point() +
  geom_point(aes(x = reorder(NAME, perCapitaFFL), y = .fitted), 
             color = "red", shape = 15, size = 2,  data = model.02.fit) +
  pd.theme + coord_flip() +
  labs(x = "State", y = "mothly per capita FFL count", 
       title = "Per Capita FFL count: Observed and Fitted Values, model.02")

# plot observed vs. fitted values, descending order 
ggplot(model.02.fit, aes(x = reorder(NAME, desc(perCapitaFFL)), y = perCapitaFFL)) + 
  geom_point() +
  geom_point(aes(x = reorder(NAME, desc(perCapitaFFL)), y = .fitted), 
             color = "red", shape = 15, size = 2,  data = model.02.fit) +
  pd.theme + coord_flip() +
  labs(x = "State", y = "mothly per capita FFL count", 
       title = "Per Capita FFL count: Observed and Fitted Values, model.02")



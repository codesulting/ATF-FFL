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
rural.urban <- read.csv("~/Documents/ATF-FFL/data/census/PctUrbanRural_State.csv")
summary(rural.urban)
str(rural.urban)
names(rural.urban)

# per Capita FFLs 2016
perCap16 <- read.csv("~/Documents/ATF-FFL/data/ffl-2016-perCapita.csv")
str(perCap16)

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

# check correlations
ffl.cor16 <- ffl.16 %>%
  select(-NAME)

# create correlation matrix
ffl.cor16 <- cor(ffl.cor16)

# define a new palette
ffl.pal <- colorRampPalette(c(muted("deeppink4"),
                              muted("deeppink2"),
                              "antiquewhite1",
                              "white",
                              "antiquewhite1",
                              "deepskyblue2",
                              "deepskyblue4"))

# plot correlation matrix
par(mfrow = c(1, 1), family = "GillSans")
corrplot(ffl.cor16, method = "shade", shade.col = NA, col = ffl.pal(100),
         tl.col = "gray23", tl.srt = 45, tl.cex = 0.75,
         addCoef.col = "black", number.cex = 0.5,
         order = "hclust", mar = c(2, 2, 2, 2))

# After examining the matrix, there's a certain number of variables 
# that might be meaningfully correlated to Monthly License Counts and
# Per100k FFLs. 

# Each has to do with the divide between Urban and Rural  
# - be it in population, population density, area - 
# but also in the space *between* Urban and Rural. 
# The US Census defines this as "Urban Cluster". 
# but especially the scale between. 

# Urbanized Areas are defined as having a population of over 50,000.
# Urban Clusters have a population 5,000 < n < 50,000.
# Rural Areas have a population less than 5,000.
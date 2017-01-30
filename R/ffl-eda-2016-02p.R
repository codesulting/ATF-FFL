# ATF - Federal Firearms Licenses
# Exploratory Data Analysis

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(scales)

# 2016 data -------------------------------------------------------------------

# data: Federal Firearms Licenses 2016 ----------------------------------------
f16 <- fread("~/Documents/ATF-FFL/data/ffl-2016-V3.csv", stringsAsFactors = T)
f16 <- as.data.frame(f16)
str(f16)

# data: Census Population and License Counts ----------------------------------
perCapita.16 <- read.csv("~/Documents/ATF-FFL/data/ffl-2016-perCapita.csv")
perCapita.16 <- as.data.frame(perCapita.16)
str(perCapita.16)

# from earlier cleansing/binding script, we now have
# 50 observations of 26 variables

# Measures of Central Tendency
summary(perCapita.16$perCapitaFFL)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   3.687  19.430  26.100  31.170  38.180 104.700 

# In each state, there is a mean of 31 FFLs per 100k residents.
# that factors out to 1 firearms dealer for ever 3226 residents. 
# Are there more in certain states than others? 
# And what factors might influence why there would or wouldnt be more?

# Exploratory Plots -----------------------------------------------------------

# define a theme for plotting
# modifies theme_minimal() with type set in Gill Sans
# and italic axis titles in Times
pd.theme <- theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)))

pd.classic <- theme_classic(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
        axis.title.y = element_text(margin = margin(0, 20, 0, 0)))

# Are there more in certain states than others? 
# And what factors might influence why there would or wouldnt be more?

ggplot()



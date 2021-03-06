# ATF - Federal Firearms Licenses
# Exploratory Data Analysis

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(scales)
library(maps)
library(mapproj)

# 2016 data -------------------------------------------------------------------

# data: Federal Firearms Licenses 2016 ----------------------------------------
f16 <- fread("~/Documents/ATF-FFL/data/ffl-2016-V3.csv", stringsAsFactors = T)
f16 <- as.data.frame(f16)
str(f16)

# data: Census Population and License Counts ----------------------------------
perCap16 <- read.csv("~/Documents/ATF-FFL/data/ffl-2016-perCapita.csv")
str(perCap16)

# from earlier cleansing/binding script, we now have
# 50 observations of 26 variables

# Measures of Central Tendency
summary(perCap16$perCapitaFFL)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   3.687  19.430  26.100  31.170  38.180 104.700 

# In each state, there is a mean of 31 FFLs per 100k residents.
# that factors out to 1 firearms dealer for every 3226 residents. 
# Are there more in certain states than others? 
# And what factors might influence why there would or wouldnt be more?

# Exploratory Plots -----------------------------------------------------------

source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
source("~/GitHub/ATF-FFL/R/usa-map-prep.R")

# remove unnecessary
rm(county_map)
rm(us.county)
rm(usa)

# FFL per 100k by state -------------------------------------------------------

# Are there more FFLs in certain states than others? 
# And what factors might influence why there would or wouldnt be more?

# Bar: Per Capita FFL ~ State -------------------------------------------------
perCap16 %>% 
  arrange(desc(perCapitaFFL)) %>%
  ggplot(aes(reorder(NAME, perCapitaFFL), perCapitaFFL, fill = perCapitaFFL)) +
    geom_bar(stat = "identity") + 
    scale_fill_gradient2(low = "deepskyblue4",
                         mid = "antiquewhite3",
                         high = "coral4",
                         midpoint = 52, guide = F) +
    scale_y_discrete(limits = c(0, 10, 25, 50, 75, 100, 125)) +
    labs(title = "2016: FFLs by State (per 100k residents)",
         x = "", y = "number of licenses per 100k residents") +
    pd.theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 24),
          axis.text.y = element_text(size = 24))

# How does this relate to population ranking across the states?
# First look at the expected population values overall, then plot.

summary(perCap16$POPESTIMATE2016)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 585500  1850000  4559000  6449000  7199000 39250000

perCap16 <- perCap16 %>%
  mutate(perCapPop = POPESTIMATE2016 / 100000)

summary(perCap16$perCapPop)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   5.855  18.500  45.590  64.490  71.990 392.500 

# plot FFLs by state with population mapped to color
perCap16 %>% 
  arrange(desc(POPESTIMATE2016)) %>%
  ggplot(aes(reorder(NAME, perCapitaFFL), perCapitaFFL, fill = perCapPop)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite3",
                       high = "coral4", midpoint = 200) +
  scale_y_discrete(limits = c(0, 10, 25, 50, 75, 100, 125)) +
  labs(title = "2016: Federal Firearms Licenses by State (per 100,000 residents)",
       x = "", y = "number of licenses per 100k residents", fill = "Population / 100k") +
  pd.theme + 
  theme(legend.position = "right",
        axis.text = element_text(size = 24)) +
  coord_flip()

# FFLs per 100k map -----------------------------------------------------------

# merge USA map data with FFL data
perCapitaMap <- left_join(perCap16, fifty_states, by = "NAME")

# Map with Per Capita FFL data
# reorder group and order variables from `usa` data
perCapitaMap <- perCapitaMap %>%
  arrange(group, order)

# Map 01: FFL per cap, default color fill ------------------------------------
ggplot(perCapitaMap, aes(lon, lat, group = group, fill = perCapitaFFL)) +
  geom_polygon(color = "black") +
  coord_map("polyconic")

# Map 02: FFL per cap, divergent color fill -----------------------------------

summary(perCapitaMap$perCapitaFFL)

# FFL Per 100k map
ggplot(perCapitaMap, aes(lon, lat, group = group, fill = perCapitaFFL)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 52) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "2016: Federal Firearms Licenses ~ State (per 100k residents)", 
       x = "", y = "", fill = "")

# Map 03: Raw population data, divergent color fill ----------------------------

summary(perCapitaMap$POPESTIMATE2016)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   585500  4093000  6651000  9828000 10310000 39250000

# bar plot of population by state ---------------------------------------------
perCap16 %>%
  arrange(desc(POPESTIMATE2016)) %>%
ggplot(aes(reorder(NAME, desc(POPESTIMATE2016)), POPESTIMATE2016, fill = POPESTIMATE2016)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 15000000, guide = F) +
  scale_y_discrete(limits = c(0, 10000000, 20000000, 30000000, 40000000)) +
  pd.theme +
  theme(axis.text.x = element_text(angle = 45, size = 9.5, hjust = 1, vjust = 1,
                                   lineheight = 1.5)) +
  labs(title = "2016: US Census Population ~ State", x = "", y = "", fill = "")


# bar plot of population/100k by state ----------------------------------------

summary(perCap16$perCapPop)
summary(perCap16$POPESTIMATE2016)

perCap16 %>%
  arrange(desc(perCapPop)) %>%
  ggplot(aes(reorder(NAME, desc(perCapPop)), perCapPop, fill = perCapPop)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 200, guide = F) +
  scale_y_discrete(limits = c(0, 100, 200, 300, 425)) +
  pd.theme +
  theme(axis.text.x = element_text(angle = 45, size = 24, hjust = 1, vjust = 1,
                                   lineheight = 1.5),
        axis.text.y = element_text(size = 24)) +
  labs(title = "2016: US Census Population ~ State (per capita)", 
       y = "population / 100k", x = "", fill = "")

# map of population by state --------------------------------------------------

ggplot(perCapitaMap, aes(lon, lat, group = group, fill = POPESTIMATE2016)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 19625000) +
  coord_map("polyconic") + pd.theme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12)) +
  labs(title = "2016: US Census Population ~ State", 
       x = "", y = "", fill = "population")

# Almost tempted to say there's an inverse relationship between a state's population
# and the number of Federal Firearms License holders.
# Two new rank variables can be created to specifically look at this possibility.
# Or first - a scatterplot.

# map of per capita population by state
ggplot(perCapitaMap, aes(lon, lat, group = group, fill = perCapPop)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 200) +
  coord_map("polyconic") + pd.theme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12)) +
  labs(title = "2016: US Census Population ~ State", 
       x = "", y = "", fill = "population")

# Pattern? : FFLs by Population -----------------------------------------------

# create a new dataframe with only FFLs and population
ffl.pop <- perCap16 %>%
  select(NAME, POPESTIMATE2016, LicCount, LicCountMonthly, perCapitaFFL) %>%
  mutate(pop100k = POPESTIMATE2016/100000)

# assign state names as row names
rownames(ffl.pop) <- ffl.pop$NAME
ffl.pop$NAME <- NULL

# by raw counts
ggplot(ffl.pop, aes(LicCountMonthly, POPESTIMATE2016, label = rownames(ffl.pop))) +
  geom_text(size = 3, position = "jitter", alpha = 0.75, hjust = 1, vjust = 1,
            check_overlap = T)

# per 100k
pop.ffl <- ggplot(ffl.pop, aes(perCapitaFFL, pop100k, label = rownames(ffl.pop))) +
  geom_text(size = 3.5, alpha = 0.95, hjust = -0.1, vjust = 1, 
            check_overlap = T, family = "GillSans") +
  geom_point(aes(perCapitaFFL, pop100k), size = 0.75, data = ffl.pop, alpha = 0.25) +
  pd.classic + expand_limits(x = c(-5, 120)) +
  labs(x = "Federal Firearms Licenses per 100k", y = "population / 100k")

pop.ffl

ggplot(ffl.pop, aes(perCapitaFFL, pop100k, label = rownames(ffl.pop))) +
  geom_text(size = 3.5, alpha = 0.95, hjust = -0.05, vjust = -0.25, 
            check_overlap = T, family = "GillSans") +
  geom_point(aes(perCapitaFFL, pop100k), size = 0.75, 
             data = ffl.pop, alpha = 0.25) +
  pd.theme + expand_limits(x = c(0, 120)) +
  labs(x = "Federal Firearms Licenses per 100k", y = "population / 100k")

# per 100k - log scale
pop.ffl <- ggplot(ffl.pop, aes(perCapitaFFL, pop100k, label = rownames(ffl.pop))) +
  geom_text(size = 3.75, alpha = 0.95, hjust = -0.05, vjust = 1, check_overlap = T,
            family = "GillSans") +
  geom_point(aes(perCapitaFFL, pop100k), size = 0.75, data = ffl.pop, alpha = 0.25) +
  pd.classic + expand_limits(x = c(-5, 120)) +
  labs(x = "log(Federal Firearms Licenses per 100k)", y = "log(population / 100k)")

# FFL ~ Population w/ log scales  
pop.ffl + scale_x_log10() + scale_y_log10()

ggplot(ffl.pop, aes(perCapitaFFL, pop100k, label = rownames(ffl.pop))) +
  geom_text(size = 3.75, alpha = 0.95, hjust = -0.05, vjust = 1, 
            check_overlap = T, family = "GillSans") +
  geom_point(aes(perCapitaFFL, pop100k), size = 0.75, data = ffl.pop, alpha = 0.25) +
  scale_x_log10() + 
  scale_y_log10() + 
  expand_limits(x = c(0, 120)) +
  theme(axis.title = element_text(family = "Times", face = "italic", size = 12),
        axis.text = element_text(size = 12),
        panel.background = element_rect(fill = "gray92")) +
  labs(x = "log(Federal Firearms Licenses per 100k)", y = "log(population / 100k)")

# holy shit it does look inversely proportional

# Exploratory Models ----------------------------------------------------------

library(broom)

# model 01:  on raw counts
pop.model.01 <- lm(LicCountMonthly ~ POPESTIMATE2016, data = ffl.pop)
summary(pop.model.01)

tidy(pop.model.01)
pm01.fitted <- augment(pop.model.01)

ggplot(pm01.fitted, aes(POPESTIMATE2016, .fitted, label = .rownames)) +
  geom_line(linetype = "dashed", color = "red3") +
  geom_text(aes(POPESTIMATE2016, LicCountMonthly), size = 3,
            hjust = 1, vjust = 1, check_overlap = T) +
  geom_point(aes(y = LicCountMonthly), color = "black", 
             alpha = 0.25, data = pm01.fitted) +
  expand_limits(x = c(-2000000, 4000000)) +
  pd.theme +
  scale_x_log10() +
  labs(x = "log(2016 population)", y = "fitted FFL count ~ population",
       title = "Raw Counts: Monthly Licenses by State Population")

# model 02: on per capita
pm02 <- lm(perCapitaFFL ~ pop100k, data = ffl.pop)
summary(pm02)
tidy(pm02)

pm02.fitted <- augment(pm02)

# plot per capita 
ggplot(pm02.fitted, aes(pop100k, .fitted, label = .rownames)) +
  geom_line(linetype = "dashed", color = "red3") +
  geom_text(aes(pop100k, perCapitaFFL), size = 3,
            hjust = 1.1, vjust = 1.1, check_overlap = T) +
  geom_point(aes(y = perCapitaFFL), color = "black", 
             alpha = 0.25, data = pm02.fitted) +
  expand_limits(x = c(-25, 425), y = c(0, 110)) +
  pd.classic +
  labs(x = "2016 population", y = "FFL rate per 100k",
       title = "Per Capita FFL Counts: Monthly Licenses by State Population")

# plot per capita on log scale
ggplot(pm02.fitted, aes(pop100k, .fitted, label = .rownames)) +
  geom_line(linetype = "dashed", color = "red3") +
  geom_text(aes(pop100k, perCapitaFFL), size = 3,
            hjust = 1, vjust = 1, check_overlap = T) +
  geom_point(aes(y = perCapitaFFL), color = "black", 
             alpha = 0.25, data = pm02.fitted) +
  scale_x_log10() + scale_y_log10() +
  expand_limits(x = c(log(1), log(200))) +
  pd.classic +
  labs(x = "log(2016 population)", y = "FFL rate per 100k",
       title = "Per Capita FFL Counts: Monthly Licenses by State Population (log scale)")

# It appears in both models that an FFL count commensurate to population
# leads to high residuals in the outliers.

# on log transformed per capita
pm03 <- lm(perCapitaFFL ~ POPESTIMATE2016, data = ffl.pop)
summary(pm03)
tidy(pm03)

pm03.fitted <- augment(pm03)

ggplot(pm03.fitted, aes(POPESTIMATE2016, .fitted, label = .rownames)) +
  geom_line(linetype = "dashed", color = "red3") +
  geom_text(aes(POPESTIMATE2016, perCapitaFFL), size = 3,
            hjust = -0.1, vjust = 1.1, check_overlap = T) +
  geom_point(aes(y = perCapitaFFL), color = "black", 
             alpha = 0.25, data = pm03.fitted) +
  expand_limits(x = c(0, 5000000), y = c(0, 110)) +
  pd.classic +
  labs(x = "2016 population", y = "FFL rate per 100k",
       title = "Per Capita FFL ~ State Populations")

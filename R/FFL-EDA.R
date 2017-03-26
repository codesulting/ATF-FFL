# ATF - Federal Firearms Licenses
# Exploratory Data Analysis
# Firearm Licenses and Population by State

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(maps)
library(mapproj)

# data: Census Population and License Counts
perCap16 <- read.csv("~/Documents/ATF-FFL/data/ffl-2016-perCapita.csv")
str(perCap16)

# load custom theme and map data
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
source("~/GitHub/ATF-FFL/R/usa-map-prep.R")

# Map Preparation --------------------------------------------------------------

# merge USA map data with FFL data
perCapitaMap <- left_join(perCap16, fifty_states, by = "NAME")

# Map with Per Capita FFL data
# reorder group and order variables from `usa` data
perCapitaMap <- perCapitaMap %>%
  arrange(group, order)

# Map 01: FFL per cap, default color fill -------------------------------------
ggplot(perCapitaMap, aes(lon, lat, group = group, fill = perCapitaFFL)) +
  geom_polygon(color = "black") +
  coord_map("polyconic")

# Bar Plot FFLs by state with FFLs mapped to color ----------------------------

summary(perCapitaMap$perCapitaFFL)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.687  16.500  22.720  26.360  31.840 104.700

perCap16 %>%
  arrange(desc(perCapitaFFL)) %>%
  ggplot(aes(reorder(NAME, desc(perCapitaFFL)), perCapitaFFL, fill = perCapitaFFL)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 52, guide = F) +
  pd.theme +
  theme(axis.text.x = element_text(angle = 45, size = 9.5, hjust = 1, vjust = 1,
                                   lineheight = 1.5)) +
  labs(title = "2016: Federal Firearms Licenses ~ State", x = "", y = "", fill = "")

# Bar Plot FFLs by state with population mapped to color ----------------------

perCap16 %>% 
  mutate(perCapPop = POPESTIMATE2016/100000) %>%
  arrange(desc(perCapPop)) %>%
  ggplot(aes(reorder(NAME, perCapitaFFL), perCapitaFFL, fill = perCapPop)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite3",
                       high = "coral4", midpoint = 200) +
  scale_y_discrete(limits = c(0, 10, 25, 50, 75, 100, 125)) +
  labs(title = "2016: Federal Firearms Licenses by State (per 100,000 residents)",
       x = "", y = "number of licenses per 100k residents", fill = "") +
  pd.theme + 
  theme(legend.position = "right",
        panel.background = element_blank()) +
  coord_flip()

# Map 02 : FFL Per 100k -------------------------------------------------------

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
                       high = "coral4", midpoint = 19625000, guide = F) +
  pd.theme +
  theme(axis.text.x = element_text(angle = 45, size = 9.5, hjust = 1, vjust = 1,
                                   lineheight = 1.5)) +
  labs(title = "2016: US Census Population ~ State", x = "", y = "", fill = "")

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

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

# FFL per 100k by state -------------------------------------------------------

# Are there more FFLs in certain states than others? 
# And what factors might influence why there would or wouldnt be more?

perCapita.16 %>% 
  arrange(desc(perCapitaFFL)) %>%
  ggplot(aes(reorder(NAME, perCapitaFFL), perCapitaFFL, fill = perCapitaFFL)) +
    geom_bar(stat = "identity") + 
    scale_fill_gradient2(low = "deepskyblue4",
                         mid = "antiquewhite3",
                         high = "coral4",
                         midpoint = 52, guide = F) +
    scale_y_discrete(limits = c(0, 10, 25, 50, 75, 100, 125)) +
    labs(title = "2016: Federal Firearms Licenses by State (per 100,000 residents)",
         x = "", y = "number of licenses per 100k residents") +
    pd.theme +
    coord_flip()

# How does this relate to population ranking across the states?
# First look at the expected population values overall, then plot.

summary(perCapita.16$EstPop16.Wiki)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 585500  1850000  4559000  6449000  7199000 39250000

perCapita.16 <- perCapita.16 %>%
  mutate(perCapPop = EstPop16.Wiki / 100000)

summary(perCapita.16$perCapPop)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   5.855  18.500  45.590  64.490  71.990 392.500 

# plot FFLs by state with population mapped to color
perCapita.16 %>% 
  arrange(desc(EstPop16.Wiki)) %>%
  ggplot(aes(reorder(NAME, perCapitaFFL), perCapitaFFL, fill = perCapPop)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite3",
                       high = "coral4", midpoint = 200) +
  scale_y_discrete(limits = c(0, 10, 25, 50, 75, 100, 125)) +
  labs(title = "2016: Federal Firearms Licenses by State (per 100,000 residents)",
       x = "", y = "number of licenses per 100k residents", fill = "Population / 100k") +
  pd.theme + 
  theme(legend.position = "right") +
  coord_flip()

# FFLs per 100k map -----------------------------------------------------------

library(maps)
library(mapproj)


# load map data for US
usa <- map_data("state")

# test map out
ggplot(usa, aes(long, lat, group = group)) +
  geom_path() + coord_map("polyconic") 

# match variable names in FFL data to merge with US map
colnames(usa) <- c("lon", "lat", "group", "order", "NAME", "subregion")

# capitalize state.name (function from tolower() documentation)
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

usa$NAME <- capwords(usa$NAME)

# merge USA map data with FFL data
perCapitaMap <- left_join(perCapita.16, usa, by = "NAME")
summary(as.factor(perCapitaMap$subregion))
# there are 16 'subregions' 
# eg. long island, main, staten island, manhattan, nantucket, north, chesapeake

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
  scale_fill_gradient2(low = "steelblue4",
                       mid = "antiquewhite1",
                       high = "coral3", midpoint = 52) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "bottom",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "gray18"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "2016: Federal Firearms License ~ State (per 100k residents)", 
       x = "", y = "", fill = "FFLs / 100k")

# Map 03: Raw population data, divergent color fill ----------------------------

summary(perCapitaMap$POPESTIMATE2016)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   585500  4093000  6651000  9828000 10310000 39250000

# bar plot of population by state
ggplot(perCapita.16, aes(reorder(NAME, POPESTIMATE2016), POPESTIMATE2016, fill = POPESTIMATE2016)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue3",
                       mid = "antiquewhite1",
                       high = "coral3", midpoint = 15000000) +
  pd.theme +
  theme(axis.text.x = element_text(angle = 45, size = 9.5, hjust = 1, vjust = 1,
                                   lineheight = 1.5)) +
  labs(title = "2016: US Census Population ~ State", x = "", y = "", fill = "") 

# map of population by state
ggplot(perCapitaMap, aes(lon, lat, group = group, fill = POPESTIMATE2016)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue3",
                       mid = "antiquewhite1",
                       high = "coral3", midpoint = 15000000) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "bottom",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "gray18"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(angle = 45, size = 12, hjust = 1, vjust = 1)) +
  labs(title = "2016: US Census Population ~ State", 
       x = "", y = "", fill = "population")











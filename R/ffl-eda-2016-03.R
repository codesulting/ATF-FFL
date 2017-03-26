# ATF - Federal Firearms Licenses
# Exploratory Data Analysis

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

# Map 01: FFL per cap, default color fill ------------------------------------
ggplot(perCapitaMap, aes(lon, lat, group = group, fill = perCapitaFFL)) +
  geom_polygon(color = "black") +
  coord_map("polyconic")

# Map 02: FFL per cap, divergent color fill -----------------------------------

summary(perCapitaMap$perCapitaFFL)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.687  16.500  22.720  26.360  31.840 104.700

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


# Map: FFL Per 100k -----------------------------------------------------------
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

####
# ATF Regions -----------------------------------------------------------------
####

library(RColorBrewer)
display.brewer.all()

# Cleanse/Join ATF Regions to Per Capita data ---------------------------------

# Match levels of NAME in each dataframe
perCap16 <- read.csv("~/Documents/ATF-FFL/data/ffl-2016-perCapita.csv")
str(perCap16)
levels(perCap16$NAME)
perCap16$NAME <- as.character(perCap16$NAME)

atf.regions <- read.csv("~/Documents/ATF-FFL/data/ATF-Regions.csv")
colnames(atf.regions)[2] <- "NAME"
levels(atf.regions$NAME)
str(atf.regions)

atf.regions$NAME <- as.character(atf.regions$NAME)
# atf.regions$NAME <- gsub("^\\s", "", atf.regions$NAME)
levels(as.factor(atf.regions$NAME))
levels(as.factor(perCap16$NAME))

# join ATF Region codes
perCap16 <- left_join(perCap16, atf.regions, by = "NAME")
perCapitaMap <- left_join(perCapitaMap, atf.regions, by = "NAME")

summary(perCapitaMap$atf.Region)
perCapitaMap$atf.Region <- as.factor(perCapitaMap$atf.Region)

# Map 04: ATF Regions ---------------------------------------------------------

# define palettes
display.brewer.pal(7,"BrBG")
brbg.pd <- c('#8c510a','#bf812d','#dfc27d','#eddeb8','#c7eae5','#80cdc1','#35978f','#01665e')

# map of ATF Regions
ggplot(perCapitaMap, aes(lon, lat, group = group, fill = atf.Region)) +
  geom_polygon(linetype = "solid", color = "white", size = 0.075) +
  scale_fill_manual(values = brbg.pd) +
  coord_map("polyconic") + pd.theme +
  theme(panel.border = element_rect(linetype = "solid",
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.position = "right") +
  labs(title = "ATF Federal Firearms License Regions", 
       x = "", y = "", fill = "Region")

# Compute Per Capita FFL by Region --------------------------------------------

# per Capita FFLs by Region
perCapitaRegion <- perCap16 %>%
  group_by(atf.Region) %>%
  summarize(ffl.region = sum(LicCountMonthly),
            ffl.pop = sum(POPESTIMATE2016)) %>%
  mutate(perCapFFLRegion = (ffl.region/ffl.pop)*100000)

# check values
perCap16 %>% 
  select(LicCountMonthly, atf.Region) %>%
  filter(atf.Region == 3) %>%
  summarise(sum(LicCountMonthly))

# by state (repeated)
perCapitaRegionTotal <- perCap16 %>%
  group_by(atf.Region, NAME) %>%
  summarize(ffl.region = sum(LicCountMonthly),
            ffl.pop = sum(POPESTIMATE2016)) %>%
  mutate(perCapFFLRegion = (ffl.region/ffl.pop)*100000)

# join to map dataframe
perCapitaRegion$atf.Region <- factor(perCapitaRegion$atf.Region)
perCapitaMapRegion <- left_join(perCapitaMap, perCapitaRegion, by = "atf.Region")

summary(perCapitaMapRegion$perCapFFLRegion)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   11.91   16.52   20.47   21.50   24.45   30.30

levels(as.factor(perCapitaMapRegion$perCapFFLRegion))
# "11.9075881831947" "12.5174778907644" "16.5153543818352" "20.4674221254215" "23.2532855847123" "24.4531173804022" "30.2975398789806"

# write.csv(perCap16, file = "~/GitHub/ATF-FFL/data/ffl-2016-perCapita.csv", row.names = F)
# write.csv(perCapitaMapRegion, file = "~/GitHub/ATF-FFL/data/map-perCapitaRegions.csv", row.names = F)

# Map 05: FFLs per capita, by ATF region --------------------------------------

perCapitaRegions <- read.csv("~/GitHub/ATF-FFL/data/map-perCapitaRegions.csv")
perCapitaRegions$atf.Region <- factor(perCapitaRegions$atf.Region)
perCapitaRegions <- perCapitaRegions[, -c(28:32)]

perCapitaRegions <- perCapitaRegions %>%
  left_join(fifty_states, by = "NAME")

summary(perCapitaRegions$perCapFFLRegion)

write.csv(perCapitaRegions, file = "~/GitHub/ATF-FFL/data/map-perCapitaRegions.csv",
          row.names = F)

# map of ATF Regions
ggplot(perCapitaRegions, aes(lon, lat, group = group, fill = perCapFFLRegion)) +
  geom_polygon(linetype = "solid", color = "white", size = 0.075) +
  scale_fill_gradient2(low = "deepskyblue4", mid = "antiquewhite1", high = "firebrick3",
                       midpoint = 21.5) +
  coord_map("polyconic") + pd.theme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12)) +
  labs(title = "Per Capita Federal Firearms License by Region", 
       x = "", y = "", fill = "FFLs")

# bar plot
ggplot(perCapitaRegions, aes(reorder(NAME, perCapFFLRegion), perCapFFLRegion, fill = perCapFFLRegion)) +
  geom_bar(stat = "identity", position = "dodge") + pd.theme +
  scale_fill_gradient2(low = "deepskyblue4", mid = "antiquewhite1", high = "firebrick3",
                       midpoint = 21.5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Per Capita Federal Firearms Licenses by Region", x = "", y = "", fill = "")

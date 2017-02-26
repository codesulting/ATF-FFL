# ATF - FFL
# Rural-Urban Proportions Model Building - 02
# Robust Regression

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(maps)
library(mapproj)

# load plot themes and US map
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
source("~/GitHub/ATF-FFL/R/usa-map-prep.R")

# cleansed dataframe with FFL & Rural-Urban data
ffl.16 <- read.csv("~/GitHub/ATF-FFL/data/rural-urban-compact.csv")
str(ffl.16)
names(ffl.16)
rownames(ffl.16) <- ffl.16$NAME

# Mapping Features ------------------------------------------------------------

# merge USA map data with FFL data
# perCapitaMap <- left_join(ffl.16, usa, by = "NAME")
# summary(as.factor(perCapitaMap$subregion))

# perCapitaMap <- perCapitaMap %>%
#   arrange(group, order)

# including AK and HI
perCap50 <- left_join(ffl.16, fifty_states, by = "NAME") %>%
  arrange(group, order)

# Map 01: Pop Pct UC ----------------------------------------------------------

summary(perCap50$POPPCT_UC)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    0.28    7.69   10.55   11.51   15.28   40.25

ggplot(perCap50, aes(lon, lat, group = group, fill = POPPCT_UC)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 20.12) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "2016: Percentage of Population Living in Urban Clusters", 
       x = "", y = "", fill = "")


# Map 02: Pop Pct Rural -------------------------------------------------------

summary(perCap50$POPPCT_RURAL)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    5.05   15.30   25.43   25.84   33.91   61.34

ggplot(perCap50, aes(lon, lat, group = group, fill = POPPCT_RURAL)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 30.17) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "2016: Percentage of Population Living in Rural Areas by State", 
       x = "", y = "", fill = "")

# Map 02B: Pop Pct URBAN ------------------------------------------------------

summary(perCap50$POPPCT_URBAN)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 38.66   66.02   70.58   71.40   81.03   94.95 

mid.PopPctUrban <- ((94.94 - 38.66)/2) + min(perCap50$POPPCT_URBAN)
mid.PopPctUrban

ggplot(perCap50, aes(lon, lat, group = group, fill = POPPCT_URBAN)) +
  geom_polygon() +
  scale_fill_gradient2(low = "coral4",
                       mid = "antiquewhite1",
                       high = "deepskyblue4", 
                       midpoint = mid.PopPctUrban) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "2016: Percentage of Population Living in Urban Areas by State", 
       x = "", y = "", fill = "")


# Map 03: Land Area Urban Clusters  -------------------------------------------

summary(perCap50$AREA_UC)
#      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# 10850000  933200000 1265000000 1403000000 1539000000 3953000000

3953000000/2

# TOTAL LAND AREA
ggplot(perCap50, aes(lon, lat, group = group, fill = AREA_UC)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 1976500000) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Total Urban Cluster Land Area by State", 
       x = "", y = "", fill = "")

# PERCENTAGE LAND AREA
summary(perCap50$AREAPCT_UC)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.0200  0.2100  0.8000  0.8159  1.1200  6.2100

mid.ucArea.PCT <- ((6.21 - 0.02)/2) + min(perCap50$AREAPCT_UC)
mid.ucArea.PCT

ggplot(perCap50, aes(lon, lat, group = group, fill = AREAPCT_UC)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = mid.ucArea.PCT) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Percentage of Urban Cluster Land Area by State", 
       x = "", y = "", fill = "")


# Map 04: Rural Land Area  ----------------------------------------------------

summary(perCap50$AREA_RURAL)
#        Min.       1st Qu.        Median          Mean       3rd Qu.          Max. 
# 1640000000   98620000000  119700000000  171200000000  174300000000 1477000000000

max(perCap50$AREA_RURAL)/2
# 738639753828

ggplot(perCap50, aes(lon, lat, group = group, fill = AREA_RURAL)) +
  geom_polygon(color = "white", size = 0.05) +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 738639753828) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Total Rural Land Area by State", 
       x = "", y = "", fill = "")

# Bar Plot
ffl.16 %>% 
  arrange(desc(AREA_RURAL)) %>%
  ggplot(aes(reorder(NAME, AREA_RURAL), AREA_RURAL, fill = AREA_RURAL)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite3",
                       high = "coral4", midpoint = 738639753828) +
  labs(title = "2016: Total Rural Land Area by State",
       x = "", y = "", fill = "Land Area") +
  pd.theme + 
  theme(legend.position = "right") +
  coord_flip()


summary(perCap50$AREAPCT_RURAL)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  60.30   92.95   96.65   95.04   99.30   99.95

# calculate midpoint
# midpoint of range + minumum value
mid.RuralArea.PCT <- ((99.95 - 60.30)/2) + min(perCap50$AREAPCT_RURAL)

ggplot(perCap50, aes(lon, lat, group = group, fill = AREAPCT_RURAL)) +
  geom_polygon(color = "white", size = 0.075) +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", 
                       midpoint = mid.RuralArea.PCT) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Percentage of Rural Land Area by State", 
       x = "", y = "", fill = "")

# Map: Urban Land Area --------------------------------------------------------

summary(perCap50$AREA_URBAN)
#       Min.     1st Qu.      Median        Mean     3rd Qu.        Max. 
# 404400000  1293000000  4417000000  6253000000  7735000000 22650000000

max(perCap50$AREA_URBAN)/2
# 738639753828

# TOTAL URBAN LAND AREA
ggplot(perCap50, aes(lon, lat, group = group, fill = AREA_URBAN)) +
  geom_polygon(color = "white", size = 0.05) +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 11325504800) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Total Urban Land Area by State", 
       x = "", y = "", fill = "")

summary(perCap50$AREAPCT_URBAN)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.050   0.700   3.350   4.959   7.050  39.700 

mid.UrbanPCT <- ((39.700 - 0.050)/2) + min(perCap50$AREAPCT_URBAN)

# PERCENTAGE URBAN LAND AREA
ggplot(perCap50, aes(lon, lat, group = group, fill = AREAPCT_URBAN)) +
  geom_polygon(color = "white", size = 0.05) +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = mid.UrbanPCT) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Percentage Urban Land Area by State", 
       x = "", y = "", fill = "")

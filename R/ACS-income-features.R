# ATF- FFL - ACS Financial Characteristics Data
# Feature Selection + Engineering
# Census Electorate Characteristics & American Community Survey data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# dataset containing data by state for:
# - population by sex, race/origin
# - industry
# - working class
# - educational attainment
# - financial characteristics
# acs <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-all-data.csv", stringsAsFactors = F)

# ACS Financial Characteristics data, FFL data, Total Population data
finance <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-finance.csv", stringsAsFactors = F)
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)
pop <- read.csv("~/GitHub/ATF-FFL/data/population-compact.csv", stringsAsFactors = F)

# Per Capita function
perCap2015 <- function (x) {
  x <- (x / pop$Pop2015) * 100000
  x
}

# custom plot themes and maps
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
source("~/GitHub/ATF-FFL/R/usa-map-prep.R")
rm(county_map)
rm(usa)

# Explore Features ------------------------------------------------------------

str(ffl)
str(pop)
str(finance)

# rename variables
colnames(finance)[2] <- "NAME"
colnames(finance)[4:14] <- gsub("fin", "pct", colnames(finance)[4:14])

# remove DC and PR
finance <- finance[-c(9, 52), ]
rownames(finance) <- NULL

# Map Income by State ---------------------------------------------------------

# create dataframe with map info
income.map <- finance %>%
  left_join(fifty_states, by = "NAME") %>%
  arrange(group, order)

# income less than $5000 ------------------------------------------------------
summary(income.map$pct.03.LessThan5000)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  1.700   2.500   3.200   3.169   3.700   5.500

((5.5 - 1.7) / 2) + 1.7
# 3.6

ggplot(income.map, aes(lon, lat, group = group, fill = pct.03.LessThan5000)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint =  3.6) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income less than $5,000", 
       x = "", y = "", fill = "")

# income $5,000 to $9,999 -----------------------------------------------------
summary(income.map$pct.04.5000to9999)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  2.100   2.700   3.400   3.586   4.100   6.000

((6 - 2.1) / 2) + 2.1
# 4.05

ggplot(income.map, aes(lon, lat, group = group, fill = pct.04.5000to9999)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 4.05) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income $5,000 to $9,999", 
       x = "", y = "", fill = "")

# income $10,000 to $14,999 -----------------------------------------------------
summary(income.map$pct.05.10000to14999)
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    3.100   3.800   5.000   4.941   5.700   7.100

((7.1 - 3.1) / 2) + 3.1
# 5.1

ggplot(income.map, aes(lon, lat, group = group, fill = pct.05.10000to14999)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 5.1) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income $10,000 to $14,999", 
       x = "", y = "", fill = "")

# income $15,000 to $19,999 -----------------------------------------------------
summary(income.map$pct.06.15000to19999)
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     3.20    4.00    5.00    5.05    5.80    7.40

((7.4 - 3.2) / 2) + 3.2
# 5.3

ggplot(income.map, aes(lon, lat, group = group, fill = pct.06.15000to19999)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 5.3) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income $15,000 to $19,999", 
       x = "", y = "", fill = "")

# income $20,000 to $24,999 -----------------------------------------------------
summary(income.map$pct.07.20000to24999)
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     3.200   4.200   5.500   5.165   6.000   6.900 

((6.9 - 3.2) / 2) + 3.2
# 5.05

ggplot(income.map, aes(lon, lat, group = group, fill = pct.07.20000to24999)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 5.05) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income $20,000 to $24,999", 
       x = "", y = "", fill = "")

# income $25,000 to $34,999 -----------------------------------------------------
summary(income.map$pct.08.25000to34999)
#      Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     6.800   8.500  10.300   9.866  11.200  12.600

# range divided 2, plus min
((12.6 - 6.8) / 2) + 6.8
# 9.7

ggplot(income.map, aes(lon, lat, group = group, fill = pct.08.25000to34999)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 9.7) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income $25,000 to $34,999", 
       x = "", y = "", fill = "")


# income $35,000 to $49,999 ---------------------------------------------------
summary(income.map$pct.09.35000to49999)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  10.20   12.00   13.60   13.42   14.60   15.60

((15.6 - 10.2) / 2) + 10.20
# 12.9

# percentage of population: income $35,000 to $49,999
ggplot(income.map, aes(lon, lat, group = group, fill = pct.09.35000to49999)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint =  12.9) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income $35,000 to $49,999", 
       x = "", y = "", fill = "")

# income $50,000 to $74,999 ---------------------------------------------------
summary(income.map$pct.10.50000to74999)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  15.60   17.70   18.00   18.05   18.70   20.80

((20.80 - 15.6) / 2) + 15.6
# 18.2

# percentage of population: Household Income $50,000 to $74,999
ggplot(income.map, aes(lon, lat, group = group, fill = pct.10.50000to74999)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint =  18.2) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 14, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income $50,000 to $74,999", 
       x = "", y = "", fill = "")

# income $75,000 to $99,999 ---------------------------------------------------
summary(income.map$pct.11.75000to99999)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  10.40   11.50   12.20   12.62   13.90   15.00

((15 - 10.4) / 2) + 10.4
# 12.7

# percentage of population: Household Income $50,000 to $74,999
ggplot(income.map, aes(lon, lat, group = group, fill = pct.11.75000to99999)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 12.7) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 14, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income $75,000 to $99,999", 
       x = "", y = "", fill = "")

# income $100,000 to $149,999 ---------------------------------------------------
summary(income.map$pct.12.100000to149999)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   9.30   11.50   13.10   13.77   15.90   18.80 

((18.8 - 9.3) / 2) + 9.3
# 14.05

# percentage of population: Household Income $50,000 to $74,999
ggplot(income.map, aes(lon, lat, group = group, fill = pct.12.100000to149999)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 14.05) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 14, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income $100,000 to $149,999", 
       x = "", y = "", fill = "")

# income $150,000 or more -----------------------------------------------------
summary(income.map$pct.13.150000.or.more)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   4.60    7.20    8.70   10.38   15.20   19.50 

((19.5 - 4.6) / 2) + 4.6
# 12.05

# percentage of population: Household Income $50,000 to $74,999
ggplot(income.map, aes(lon, lat, group = group, fill = pct.13.150000.or.more)) +
  geom_polygon(color = "white", size = 0.1) + 
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 12.05) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        plot.title = element_text(size = 24),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 24, hjust = 1, vjust = 1)) +
  labs(title = "Percentage of Population by State:\nHousehold Income $150,000 or more", 
       x = "", y = "", fill = "")

# Stack Income Bracket Observations -------------------------------------------

# create long dataframe
income.map.stack <- income.map %>%
  select(2:15, 27:32) %>%
  gather(key = Bracket, value = Percentage, 3:14)
  
colnames(income.map.stack)[2] <- "Occupied.Housing.Units"

# rename Bracket levels
income.map.stack$Bracket <- gsub("pct.[0-9][0-9].", "", income.map.stack$Bracket)
income.map.stack$Bracket <- gsub("Than", " than ", income.map.stack$Bracket)
income.map.stack$Bracket <- gsub("to", " to ", income.map.stack$Bracket)
income.map.stack$Bracket <- gsub(".or\\.", " or ", income.map.stack$Bracket)
income.map.stack$Bracket <- gsub("fin.14.MedianHouseholdIncome", 
                                 "Median Household Income", 
                                 income.map.stack$Bracket)

levels(as.factor(income.map.stack$Bracket))
income.map.stack$Bracket <- factor(income.map.stack$Bracket)

# ACS Income Regression: Percentages ------------------------------------------

library(MASS)
library(broom)
library(dplyr)

income.ffl <- ffl %>%
  left_join(finance, by = "NAME") 

# Model 00: Linear Baseline Model ---------------------------------------------

income.ffl.pct <- income.ffl %>%
  dplyr::select(1:3, 7:21)

rownames(income.ffl.pct) <- income.ffl.pct$NAME
income.ffl.pct$NAME <- NULL

income.00 <- lm(perCapitaFFL ~ ., data = income.ffl.pct)
summary(income.00)

par(mfrow = c(2, 2))
plot(income.00)

# Model 000: Linear Baseline Model --------------------------------------------
income.ffl.all <- income.ffl
rownames(income.ffl.all) <- income.ffl.all$NAME
income.ffl.all$NAME <- NULL
income.ffl.all <- income.ffl.all %>%
  dplyr::select(1:2, 6:31)

income.000 <- lm(perCapitaFFL ~ ., data = income.ffl.all)
summary(income.000)
plot(income.000)

# Model 01: Robust Regression 01 ----------------------------------------------
income.01 <- rlm(perCapitaFFL ~ ., data = income.ffl.pct)
summary(income.01)




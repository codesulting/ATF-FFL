# ATF - Federal Firearms Licenses
# Exploratory Data Analysis
# Firearms Commerce Data - Exhibit 08
# National Firearms Act - firearms registration data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(scales)

library(maps)
library(mapproj)
library(sp)
library(rgdal)

# data: Census Population and License Counts ----------------------------------

# This is FFL counts per 100k residents:
# A merging of 2016 ATF-FFL data and 2016 US Census Data.
perCapita.16 <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita.csv")
str(perCapita.16)

# data: National Firearms Act registration by state ---------------------------

# 2016 ATF Firearms Commerce Report, Exhibit 8
# This data was collected in February 2016, 
# and details counts of firearms registrations by type by state.
# Variables: Any Other Weapon, Destructive Device, Machine Gun, Silencer, Rifle, Shotgun

reg.by.state <- read.csv("~/Documents/ATF-FFL/data/commerce/08-registration-by-state.csv",
                                  stringsAsFactors = T)

# add month and year variables to registration data
reg.by.state$month <- as.factor("02")
reg.by.state$year <- as.factor("2016")

# change column names
colnames(reg.by.state) <- c("NAME", "OtherWeapon", "DestructiveDevice", "MachineGun", 
                            "Silencer", "Rifle", "Shotgun", "Total", "month", "year")

# function to remove commas from numeric variables
commas <- function(x) {
  x <- gsub(",", "", x)
  x <- as.integer(x)
  x
}

# loop comma function over columns with numeric values
for (i in 2:8) {
  reg.by.state[, i] <- commas(reg.by.state[, i])
}

str(reg.by.state)
summary(reg.by.state)

# write.csv(reg.by.state, file = "~/Documents/ATF-FFL/data/08-registrastion-by-state-V2.csv",
#          row.names = F)

# Wrangle: per capita counts for each type of registered firearm --------------

# remove DC and 'Other Territories'
reg.by.state <- reg.by.state[-c(8, 52), ]
reg.by.state$NAME <- factor(reg.by.state$NAME)

# merge population data
registered.pop <- left_join(perCapita.16, reg.by.state, by = "NAME")
registered.pop$month <- NULL
registered.pop$year <- NULL

# function to find rate per 100k
perCap <- function (x) {
  x <- (x / registered.pop$POPESTIMATE2016) * 100000
  x
}

# test function
perCap(registered.pop$MachineGun)
perCap(registered.pop$DestructiveDevice)

# for (i in 27:33) {
#   registered.pop <- registered.pop %>% 
#        mutate(paste0(colnames(registered.pop[i]), ".per100k") = perCap(registered.pop[, i]))
# }

# paste0(colnames(registered.pop[27:33]), ".per100k")
# perCap(registered.pop[, 27])

registered.pop$OtherWeapon.per100k <- perCap(registered.pop$OtherWeapon)
registered.pop$DestructiveDevice.per100k <- perCap(registered.pop$DestructiveDevice)
registered.pop$MachineGun.per100k <- perCap(registered.pop$MachineGun)
registered.pop$Silencer.per100k <- perCap(registered.pop$Silencer)
registered.pop$Rifle.per100k <- perCap(registered.pop$Rifle)
registered.pop$Shotgun.per100k <- perCap(registered.pop$Shotgun)
registered.pop$Total.per100k <- perCap(registered.pop$Total)

# add population/100k variable
registered.pop <- registered.pop %>%
  mutate(Pop100k = POPESTIMATE2016 / 100000)

# write.csv(registered.pop, file = "~/Documents/ATF-FFL/data/2016-firearms-per100k.csv", row.names = F)

# Exploratory Plots -----------------------------------------------------------

# load custom plot themes
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# raw counts of Destructive Devices -------------------------------------------

# Destructive Device is defined for firearms as those having a bore larger than 1/2",
# or 12.7mm. A 9mm pistol would not fall into this category.

reg.by.state %>% arrange(desc(DestructiveDevice)) %>%
  ggplot(aes(reorder(NAME, DestructiveDevice), DestructiveDevice)) +
  geom_point() +
  pd.theme + coord_flip()

# plot per 100k observations of registered firearms ---------------------------

# total firearms per capita
registered.pop %>% arrange(desc(Total.per100k)) %>%
  ggplot(aes(reorder(NAME, Total.per100k), Total.per100k, fill = Total.per100k)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                      mid = "antiquewhite1",
                      high = "firebrick3", midpoint = 10000) +
  pd.theme + coord_flip() +
  theme(legend.position = "right") +
  labs(x = "", y = "Registered Firearms per 100k residents", fill = "Registered\nFirearms",
       title = "2016 National Firearms Act: Total Registered Weapons ~ State (per 100k residents)")

# total firearms per capita, color fill with state population
ggplot(registered.pop, aes(reorder(NAME, Total.per100k), Total.per100k, fill = POPESTIMATE2016)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "firebrick3", midpoint = 20000000) +
  pd.theme + coord_flip() +
  theme(legend.position = "right") +
  labs(x = "", y = "Registered Firearms per 100k residents", fill = "Population",
       title = "2016 National Firearms Act: Total Registered Weapons ~ State (per 100k residents)")


# filter for per capita licensed firearms
firearms.perCapita <- registered.pop %>%
  select(NAME, POPESTIMATE2016, Pop100k, OtherWeapon, DestructiveDevice, MachineGun, Silencer,
         Rifle, Shotgun, Total, OtherWeapon.per100k, DestructiveDevice.per100k,
         MachineGun.per100k, Silencer.per100k, Rifle.per100k, Shotgun.per100k, Total.per100k)

# plot: per capita Destructive Devices
ggplot(firearms.perCapita, aes(reorder(NAME, DestructiveDevice.per100k), DestructiveDevice.per100k,
                               fill = POPESTIMATE2016)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "firebrick3", midpoint = 20000000) +
  pd.theme + coord_flip() +
  theme(legend.position = "right") +
  labs(x = "", y = "Registered Destructive Devices per 100k residents", fill = "Population",
       title = "2016: Registered Destructive Devices per 100k, by State")

# Wyoming is off the chart. Per 100k statistics for Destructive Devices actually eclipses the 
# actual number, and population of the state. The actual rate is about 20.6% - 
# 1 in 5 people in Wyoming have a Destructive Device.

# plot: per capita Machine Guns
ggplot(firearms.perCapita, aes(reorder(NAME, MachineGun.per100k), MachineGun.per100k,
                               fill = POPESTIMATE2016)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "firebrick3", midpoint = 20000000) +
  pd.theme + coord_flip() +
  theme(legend.position = "right") +
  labs(x = "", y = "Registered Machine Guns per 100k", fill = "Population",
       title = "2016: Registered Machine Guns per 100k, by State")

# Over 1000 machine guns per 100k residents in Connecticut

# plot: registered Silencers per 100k
ggplot(firearms.perCapita, aes(reorder(NAME, Silencer.per100k), Silencer.per100k,
                               fill = POPESTIMATE2016)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "firebrick3", midpoint = 20000000) +
  pd.theme + coord_flip() +
  theme(legend.position = "right") +
  labs(x = "", y = "Registered Silencers per 100k", fill = "Population",
       title = "2016: Registered Silencers per 100k, by State")

# keep it quiet in Idaho

# plot: Rifles per 100k
ggplot(firearms.perCapita, aes(reorder(NAME, Rifle.per100k), Rifle.per100k,
                               fill = POPESTIMATE2016)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "firebrick3", midpoint = 20000000) +
  pd.theme + coord_flip() +
  theme(legend.position = "right") +
  labs(x = "", y = "Registered Rifles per 100k", fill = "Population",
       title = "2016: Registered Rifles per 100k, by State")

# New Hampshire lives free

# plot: Shotgun per 100k
ggplot(firearms.perCapita, aes(reorder(NAME, Shotgun.per100k), Shotgun.per100k,
                               fill = POPESTIMATE2016)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "firebrick3", midpoint = 20000000) +
  pd.theme + coord_flip() +
  theme(legend.position = "right") +
  labs(x = "", y = "Registered Shotguns per 100k", fill = "Population",
       title = "2016: Registered Shotguns per 100k, by State")

# write.csv(firearms.perCapita, file = "~/GitHub/ATF-FFL/data/2016-firearms-per-capita.csv", row.names = F)

# Maps: Registrations by State by Firearm Type --------------------------------

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
firearms.perCapitaMap <- left_join(firearms.perCapita, usa, by = "NAME")
summary(as.factor(firearms.perCapitaMap$subregion))
# there are 16 'subregions' 
# eg. long island, main, staten island, manhattan, nantucket, north, chesapeake

# Map with Per Capita FFL data
# reorder group and order variables from `usa` data
firearms.perCapitaMap <- firearms.perCapitaMap %>%
  arrange(group, order)

# Map: Total Firearms Registered ----------------------------------------------

summary(firearms.perCapitaMap$Total.per100k)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 356.6  1007.0  1345.0  1514.0  1682.0 21830.0 

ggplot(firearms.perCapitaMap, aes(lon, lat, group = group, fill = Total.per100k)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 21830/2) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Total Registered Weapons ~ State (per 100k residents)", 
       x = "", y = "", fill = "")

# Map: Destructive Devices ----------------------------------------------------

summary(firearms.perCapitaMap$DestructiveDevice.per100k)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  207.9   434.6   677.1   840.4   828.8 20650.0 


ggplot(firearms.perCapitaMap, aes(lon, lat, group = group, fill = DestructiveDevice.per100k)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 20650.0 / 2) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Destructive Devices ~ State (per 100k residents)", 
       x = "", y = "", fill = "")

# Map: Machine Guns -----------------------------------------------------------

summary(firearms.perCapitaMap$MachineGun.per100k)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  28.7   125.1   166.8   200.5   240.0  1061.0  

ggplot(firearms.perCapitaMap, aes(lon, lat, group = group, fill = MachineGun.per100k)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 1061.0  / 2) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Registered Machine Guns ~ State (per 100k residents)", 
       x = "", y = "", fill = "")

# Map: Silencers --------------------------------------------------------------

summary(firearms.perCapitaMap$Silencer.per100k)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   2.745  185.300  296.000  335.800  480.900 1080.000  

ggplot(firearms.perCapitaMap, aes(lon, lat, group = group, fill = Silencer.per100k)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 1080 / 2) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Silencers ~ State (per 100k residents)", 
       x = "", y = "", fill = "")

# Map: Rifles -----------------------------------------------------------------

summary(firearms.perCapitaMap$Rifle.per100k)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   4.13   54.43   70.45   73.85   89.31  239.30   

ggplot(firearms.perCapitaMap, aes(lon, lat, group = group, fill = Rifle.per100k)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 239.3 / 2) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Rifles ~ State (per 100k residents)", 
       x = "", y = "", fill = "")

# Map: Shotguns  --------------------------------------------------------------

summary(firearms.perCapitaMap$Shotgun.per100k)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   4.34   26.87   35.15   41.73   42.69  170.90  

firearms.perCapitaMap2 <- firearms.perCapitaMap %>%
  filter(NAME != "Alaska")

summary(firearms.perCapitaMap2$Shotgun.per100k)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#    4.34   26.87   35.15   41.72   42.69  132.90 

ggplot(firearms.perCapitaMap2, aes(lon, lat, group = group, fill = Shotgun.per100k)) +
  geom_polygon() +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite1",
                       high = "coral4", midpoint = 132.90  / 2) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.border = element_rect(linetype = "solid", 
                                    fill = NA, 
                                    color = "white"),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "2016: Registered Shotguns ~ State (per 100k residents)", 
       x = "", y = "", fill = "")



# ATF - Federal Firearms Licenses
# Exploratory Data Analysis
# Firearms Commerce Data

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

# 2016 data -------------------------------------------------------------------

# data: Federal Firearms Licenses 2016 ----------------------------------------
# f16 <- fread("~/Documents/ATF-FFL/data/ffl-2016-V3.csv", stringsAsFactors = T)
# f16 <- as.data.frame(f16)
# str(f16)

# data: Census Population and License Counts ----------------------------------
perCapita.16 <- read.csv("~/Documents/ATF-FFL/data/ffl-2016-perCapita.csv")
str(perCapita.16)

# data: ATF Firearms Commerce -------------------------------------------------
# in the ATF Commerce report, there are numerous tables relating to:
# manufacturing, exports, imports, firearms by ATF form type, 
# registered weapons by state, taxpayers by state, total FFLs, FFL actions, and FFL  

commerce.FFL.total <- read.csv("~/Documents/ATF-FFL/data/commerce/10-FFL-total.csv",
                               stringsAsFactors = T)
str(commerce.FFL.total)
summary(commerce.FFL.total)

# data: National Firearms Act registration by state ---------------------------
reg.by.state <- read.csv("~/Documents/ATF-FFL/data/commerce/08-registrastion-by-state.csv",
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


# Destructive Device is defined for firearms as those having a bore larger than 1/2",
# or 12.7mm. A 9mm pistol would not fall into this category.

# raw counts of Destructive Devices -------------------------------------------

reg.by.state %>% arrange(desc(DestructiveDevice)) %>%
  ggplot(aes(reorder(NAME, DestructiveDevice), DestructiveDevice)) +
  geom_point() +
  pd.theme + coord_flip()

# calculate per capita counts for each type of registered firearm -------------

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
#    registered.pop <- registered.pop %>% 
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

# write.csv(registered.pop, file = "~/Documents/ATF-FFL/data/2016-firearms-per100k.csv", row.names = F)

# plot per 100k observations of registered firearms ---------------------------

# registered.pop <- read.csv("~/Documents/ATF-FFL/data/2016-firearms-per100k.csv")

# total firearms per capita
registered.pop %>% arrange(desc(Total.per100k)) %>%
  ggplot(aes(reorder(NAME, Total.per100k), Total.per100k, fill = Total.per100k)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "steelblue3",
                      high = "sienna3") +
  pd.theme + coord_flip() +
  theme(legend.position = "right") +
  labs(x = "", y = "Registered Firearms per 100k residents", fill = "")

# filter out per capita licensed firearms
firearms.perCapita <- registered.pop %>%
  select(NAME, POPESTIMATE2016, OtherWeapon.per100k, DestructiveDevice.per100k,
         MachineGun.per100k, Silencer.per100k, Rifle.per100k, Shotgun.per100k, Total.per100k)

ggplot(firearms.perCapita, aes(reorder(NAME, DestructiveDevice.per100k), DestructiveDevice.per100k,
                               fill = DestructiveDevice.per100k)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "steelblue",
                      high = "sienna3") +
  pd.theme + coord_flip() +
  theme(legend.position = "right") +
  labs(x = "", y = "Destructive Devices per 100k residents", fill = "")


# Wyoming is off the chart. Per 100k statistics for Destructive Devices actually eclipses the 
# actual number, and population of the state. The actual rate is about 20.6% - 
# 1 in 5 people in Wyoming have a Destructive Device.  
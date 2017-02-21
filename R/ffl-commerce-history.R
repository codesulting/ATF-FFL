# ATF - Federal Firearms Licenses
# Exploratory Data Analysis
# Firearms Commerce Data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# data: ATF Firearms Commerce History -----------------------------------------

# 2016 ATF Firearms Commerce Report, Exhibit 10
# in the ATF Commerce report, there are numerous tables relating to:
# manufacturing, exports, imports, firearms by ATF form type, 
# registered weapons by state, taxpayers by state, total FFLs, FFL actions, and FFL 
# This data is from 1976-2015.

commerce.FFL.total <- read.csv("~/GitHub/ATF-FFL/data/00-commerce/10-FFL-total.csv",
                               stringsAsFactors = T)
str(commerce.FFL.total)
summary(commerce.FFL.total)

# consider making this a 'long' dataframe.
# or consider making it a wide one:
ffl.history <- as.data.frame(t(commerce.FFL.total))
colnames(ffl.history) <- 1975:2015
summary(ffl.history)

# long dataframe
FFLs <- commerce.FFL.total %>%
  gather(key = LicenseType, value = n, 2:11)

summary(FFLs)
# 'n' variable has commas and is reading as character string

# Function: remove commas from numeric variables ------------------------------

commas <- function(x) {
  x <- gsub(",", "", x)
  x <- as.integer(x)
  x
}

FFLs$n <- commas(FFLs$n)
colnames(FFLs)[3] <- "FFL.Rate"

# Exhibit 10: Federal Firearms Licenses Total 1975-2015 -----------------------

# Plot: Tufte Style sparklines for historic FFL data 
# http://motioninsocial.com/tufte/#range-frame-plot

# load custom themes
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# compute mins, maxes, ends, and quartile ranges for plot
mins <- group_by(FFLs, LicenseType) %>% slice(which.min(FFL.Rate))
maxs <- group_by(FFLs, LicenseType) %>% slice(which.max(FFL.Rate))
ends <- group_by(FFLs, LicenseType) %>% filter(Fiscal.Year == max(Fiscal.Year))
start <- group_by(FFLs, LicenseType) %>% filter(Fiscal.Year == min(Fiscal.Year))

quarts <- FFLs %>% group_by(LicenseType) %>%
  summarize(quart1 = quantile(FFL.Rate, 0.25),
            quart2 = quantile(FFL.Rate, 0.75)) %>%
  right_join(FFLs)

# plot : Tufte Style Sparklines, Tufte theme
ggplot(FFLs, aes(Fiscal.Year, FFL.Rate, group = LicenseType)) +
  facet_grid(LicenseType ~ ., scales = "free_y") +
  geom_ribbon(data = quarts, aes(ymin = quart1, max = quart2), fill = "gray96") +
  geom_line(size = 0.25) +
  geom_point(data = mins, col = "firebrick3") +
  geom_point(data = maxs, col = "deepskyblue") +
  geom_text(data = mins, aes(label = FFL.Rate), vjust = -1, size = 2.75) +
  geom_text(data = maxs, aes(label = FFL.Rate), vjust = -1, size = 2.75) +
  geom_text(data = ends, aes(label = FFL.Rate), hjust = 0, nudge_x = 1,
            family = "GillSans", size = 3.5) +
  geom_text(data = ends, aes(label = LicenseType), hjust = 0, nudge_x = 4, 
            family = "GillSans", size = 5) +
  expand_limits(x = max(FFLs$Fiscal.Year) + (0.25 * (max(FFLs$Fiscal.Year) - min(FFLs$Fiscal.Year)))) +
  scale_x_continuous(breaks = seq(1975, 2015, 5)) +
  scale_y_continuous(expand = c(0.25, 0)) +
  theme_tufte(base_size = 10, base_family = "GillSans") +
  theme(axis.title = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), strip.text = element_blank(),
        axis.text.x = element_text(size = 10),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Plot: Tufte Style, Classic Theme
ggplot(FFLs, aes(Fiscal.Year, FFL.Rate, group = LicenseType)) +
  facet_grid(LicenseType ~ ., scales = "free_y") +
  geom_ribbon(data = quarts, aes(ymin = quart1, max = quart2), fill = "gray96") +
  geom_line(size = 0.25) +
  geom_point(data = mins, col = "firebrick3") +
  geom_point(data = maxs, col = "deepskyblue") +
  geom_text(data = mins, aes(label = FFL.Rate), vjust = -1, size = 2.75) +
  geom_text(data = maxs, aes(label = FFL.Rate), vjust = -1, size = 2.75) +
  geom_text(data = ends, aes(label = FFL.Rate), hjust = 0, nudge_x = 1, size = 3.5) +
  expand_limits(x = max(FFLs$Fiscal.Year) + (0.05 * (max(FFLs$Fiscal.Year) - min(FFLs$Fiscal.Year)))) +
  scale_x_continuous(breaks = seq(1975, 2015, 5)) +
  scale_y_continuous(expand = c(0.25, 0)) +
  theme_classic(base_size = 10, base_family = "GillSans") +
  theme(axis.title = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Exhibit 01: Firearms Manufactured 1986-2014 ---------------------------------

# ATF Commerce Report Exhibit 01: Firearms Manufactured 1986-2014
# This table contains data on the number of 
# Pistols, Revolvers, Rifles, Shotguns, Miscellaneous, and Total firearms manufactured
# excluding those produced for military,
# but including those produced for law enforcement. 
# Broadly, this should provide an overview of domestic firearms production.

manufactured.firearms <- read.csv("~/GitHub/ATF-FFL/data/00-commerce/01-manufacturing.csv")
str(manufactured.firearms)

# gather values into long dataframe
manufactured.firearms <- manufactured.firearms %>%
  gather(key = "FirearmType", value = n, 2:7)

colnames(manufactured.firearms)[3] <- "NumFirearms"
manufactured.firearms$NumFirearms <- commas(manufactured.firearms$NumFirearms)

write.csv(manufactured.firearms, 
          file = "~/GitHub/ATF-FFL/data/00-commerce/01-manufacturing-long.csv",
          row.names = F)

# prep plot for Historical Manufacturing data ---------------------------------

# compute min/max, start/end
mins <- group_by(manufactured.firearms, FirearmType) %>% slice(which.min(NumFirearms))
maxs <- group_by(manufactured.firearms, FirearmType) %>% slice(which.max(NumFirearms))
ends <- group_by(manufactured.firearms, FirearmType) %>% filter(Year == max(Year))
start <- group_by(manufactured.firearms, FirearmType) %>% filter(Year == min(Year))

# compute and bind quartile range
quarts <- manufactured.firearms %>% group_by(FirearmType) %>%
  summarize(quart1 = quantile(NumFirearms, 0.25),
            quart2 = quantile(NumFirearms, 0.75)) %>%
  right_join(manufactured.firearms)

# plot : Tufte Style Sparklines, Tufte theme
ggplot(manufactured.firearms, aes(Year, NumFirearms, group = FirearmType)) +
  facet_grid(FirearmType ~ ., scales = "free_y") +
  geom_ribbon(data = quarts, aes(ymin = quart1, max = quart2), fill = "gray96") +
  geom_line(size = 0.25) +
  geom_point(data = mins, col = "firebrick3") +
  geom_point(data = maxs, col = "deepskyblue") +
  geom_text(data = mins, aes(label = NumFirearms), vjust = -1, size = 2.75) +
  geom_text(data = maxs, aes(label = NumFirearms), vjust = -1, size = 2.75) +
  geom_text(data = ends, aes(label = NumFirearms), hjust = 0, nudge_x = 1,
            family = "GillSans", size = 3.5) +
  geom_text(data = ends, aes(label = FirearmType), hjust = 0, nudge_x = 4, 
            family = "GillSans", size = 3) +
  expand_limits(x = max(FFLs$Year) + (0.25 * (max(FFLs$Year) - min(FFLs$Year)))) +
  scale_x_continuous(breaks = seq(1986, 2014, 4)) +
  scale_y_continuous(expand = c(0.25, 0)) +
  theme_tufte(base_size = 10, base_family = "GillSans") +
  theme(axis.title = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), strip.text = element_blank(),
        axis.text.x = element_text(size = 10),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))

# Firearm Manufacturing by Type, common scale
manufactured.firearms %>%
  group_by(Year, FirearmType) %>%
  ggplot(aes(Year, NumFirearms, color = FirearmType)) +
  geom_freqpoly(stat = "identity") +
  scale_x_continuous(breaks = seq(1986, 2014, by = 4)) +
  scale_y_continuous(breaks = seq(0, 12000000, by = 2000000)) +
  pd.theme

# Firearm Manufacturing by Type, common scale (total removed)
manufactured.firearms %>%
  group_by(Year, FirearmType) %>%
  filter(FirearmType != "Firearms" & FirearmType != "Misc..Firearms1") %>%
  ggplot(aes(Year, NumFirearms, color = FirearmType)) +
  geom_freqpoly(stat = "identity", size = 1) +
  scale_color_manual(values = c("firebrick3", "antiquewhite2", 
                                "deepskyblue4", "cadetblue3")) +
  scale_x_continuous(breaks = seq(1986, 2014, by = 1)) + pd.theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Firearms Manufacturing by Type, 1986-2014",
       x = "", y = "number of firearms")

levels(as.factor(manufactured.firearms$FirearmType))
manufactured.firearms$FirearmType <- factor(manufactured.firearms$FirearmType)
summary(manufactured.firearms$FirearmType)

# What are there more of? 
# Pistols vs. Rifles
manufactured.firearms %>%
  group_by(Year, FirearmType) %>%
  filter(FirearmType == "Pistols" | FirearmType == "Rifles") %>%
  ggplot(aes(Year, NumFirearms, color = FirearmType)) +
  geom_freqpoly(stat = "identity", size = 1.1) +
  scale_color_manual(values = c("firebrick3", "deepskyblue4")) +
  scale_x_continuous(breaks = seq(1986, 2014, by = 1)) + pd.theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Firearms Manufacturing: Pistols vs. Rifles, 1986-2014",
       x = "", y = "number of firearms")

# What are there more of? 
# Pistols vs. Shotguns
manufactured.firearms %>%
  group_by(Year, FirearmType) %>%
  filter(FirearmType == "Pistols" | FirearmType == "Shotguns") %>%
  ggplot(aes(Year, NumFirearms, color = FirearmType)) +
  geom_freqpoly(stat = "identity", size = 1.1) +
  scale_color_manual(values = c("firebrick3", "cadetblue3")) +
  scale_x_continuous(breaks = seq(1986, 2014, by = 1)) + pd.theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Firearms Manufacturing: Pistols vs. Shotguns, 1986-2014",
       x = "", y = "number of firearms")

# What happens when combining Rifles and Shotguns vs Pistols and Revolvers? ---
# create two broad categories: Handguns and Long Guns

# read in Firearms Manufacturing Data
mf0 <- read.csv("~/GitHub/ATF-FFL/data/00-commerce/01-manufacturing.csv", stringsAsFactors = F)

# remove commas from columns 2 through 7
for (i in 2:ncol(mf0)) {
  mf0[, i] <- commas(mf0[, i])
}

# sum each observation by year of Rifles+Shotguns and Pistols+Revolvers
for (i in 1:nrow(mf0)) {
  mf0$LongGuns[i] <- mf0$Rifles[i] + mf0$Shotguns[i]
  mf0$Handguns[i] <- mf0$Pistols[i] + mf0$Revolvers[i]
}

# create long dataframe
mf0 <- mf0 %>%
  gather(key = "FirearmType", value = n, 2:7)

colnames(mf0)[5] <- "NumFirearms"

mf0 %>%
  select(Year, LongGuns, Handguns, FirearmType, NumFirearms) %>%
  filter(FirearmType != "Firearms" & FirearmType != "Misc..Firearms1") %>% 
  ggplot(aes(Year, NumFirearms, fill = FirearmType)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ FirearmType, nrow = 2) +
  scale_fill_manual(values = c("firebrick3", "antiquewhite2", 
                               "deepskyblue4", "cadetblue3")) +
  scale_x_continuous(breaks = seq(1986, 2014, 4)) + pd.theme +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "Firearms Manufacturing by Type, 1986-2014",
       x = "", y = "number of firearms manufactured")

# Exhibit 3: Firearms Imports 1986-2015 ---------------------------------------







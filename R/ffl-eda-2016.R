# ATF - Federal Firearms Licenses
# Exploratory Data Analysis

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)
library(RColorBrewer)
library(scales)

# 2016 data -------------------------------------------------------------------

f16 <- fread("data/ffl-2016-V2.csv", stringsAsFactors = T)
f16 <- as.data.frame(f16)

# Exploratory Plots -----------------------------------------------------------

# Broadly: which states had the most firearms licenses? -----------------------

display.brewer.all()

# define a palette for each region (7 total)
region.pal <- brewer.pal(7, "PuBu")

summary(f16$LicCount)

# A look at license count by state, filled by region
ggplot(f16, aes(reorder(PremiseStateFull, LicCount), fill = LicCount)) + 
  geom_bar() +
  scale_fill_gradient2(low = "deepskyblue2", mid = "antiquewhite2", high = muted("firebrick3"),
                       midpoint = 25220) +
  scale_y_discrete(limits = c(0, 5000, 10000, 20000, 40000, 80000)) +
  theme_minimal(base_size = 13.75, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.x = element_text(family = "Times", face = "italic", size = 12,
                                    margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 12.75, angle = 30, hjust = 1, vjust = 1),
        panel.grid.major = element_line(color = "gray96")) +
  labs(title = "2016: Federal Firearms Licenses by State",
       y = "number of licenses", x = "", fill = "") +
  coord_flip()

# same variables; points instead of bar
ggplot(f16, aes(PremiseStateFull, LicCount, color = LicCount)) + 
  geom_point(alpha = 0.85, size = 3) +
  scale_color_gradient(low = "antiquewhite2", high = "firebrick4") +
  theme_minimal(base_size = 13.75, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.x = element_text(family = "Times", face = "italic", size = 12,
                                    margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 13.25),
        panel.grid.major = element_line(color = "gray92")) +
  labs(title = "2016: Federal Firearms Licenses by State",
       y = "number of licenses", x = "", color = "") +
  coord_flip()


# facet plots by region -------------------------------------------------------

f16 %>% group_by(Region)

ggplot(f16, aes(LicCount, PremiseState, color = LicCount)) +
  geom_point(size = 3) +
  facet_grid(. ~ Region) +
  scale_color_gradient(low = "deepskyblue3", high = muted("firebrick4"),
                        guide = FALSE) +
  theme_gray(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title = element_text(family = "Times", face = "italic")) +
  labs(title = "2016: Federal Firearms Licenses by State ~ Region",
       x = "number of licenses", y = "premise state")

# by region, without state
ggplot(f16, aes(LicCount, color = LicCount)) +
  geom_bar() +
  facet_grid(. ~ Region) +
  scale_color_gradient(low = "deepskyblue3", high = muted("firebrick4"),
                       guide = FALSE) +
  theme_gray(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        axis.title = element_text(family = "Times", face = "italic")) +
  labs(title = "2016: Federal Firearms Licenses by Region",
       x = "number of licenses", y = "premise state")

# Was there much variance from month to month? --------------------------------

# define a palette for each month
month_pal <- c("deepskyblue4", "deepskyblue3", "lightblue2", "bisque2", "bisque3",
               "firebrick2", "firebrick3", "firebrick4", "lightblue4", "lightblue3")

# license holder count by state, stack by month
ggplot(f16, aes(PremiseStateFull, fill = month)) + 
  geom_bar() +
  scale_fill_manual(values = month_pal) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12,
                                  margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))) +
  labs(title = "2016: Federal Firearms Licenses per month, by State/Territory", 
       x = "", y = "number of licenses") +
  coord_flip()

# facet by month
ggplot(f16, aes(PremiseStateFull, fill = LicCount)) + 
  geom_bar() +
  facet_grid(. ~ month) +
  scale_fill_gradient2(low = "deepskyblue2", mid = "antiquewhite2", high = muted("firebrick3"),
                        midpoint = 25220) +
  scale_y_continuous(limits = c(0, 8000), breaks = c(2000, 4000, 6000, 8000)) +
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12,
                                  margin = unit(c(0.5, 0, 0, 0), "cm")),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        legend.title = element_text(family = "Times", face = "italic", size = 12)) +
  labs(title = "2016: Federal Firearms Licenses per month, by State/Territory", 
       x = "", y = "number of licenses", fill = "annual total") +
  coord_flip()

# There was not much variance from month to month.
# this is likely due to licenses not expiring.

# Summary by Region -----------------------------------------------------------

f16_regions <- f16 %>%
  group_by(Region) %>%
  count(PremiseState)

colnames(f16_regions) <- c("Region", "PremiseState", "LicCount")

# there has to be a more efficient way to do this
region01 <- f16_regions %>%  filter(Region == 1)
region03 <- f16_regions %>% filter(Region == 3)
region04 <- f16_regions %>% filter(Region == 4)
region05 <- f16_regions %>% filter(Region == 5)
region06 <- f16_regions %>% filter(Region == 6)
region08 <- f16_regions %>% filter(Region == 8)
region09 <- f16_regions %>% filter(Region == 9)

f16_regions2 <- f16 %>%
  group_by(Region) %>%
  count(PremiseState)

# facet plots by firearms type ------------------------------------------------
getwd()
type <- read.csv("data/type.tsv", sep = "\t")

levels(as.factor(f16$Type))
f16$Type <- as.numeric(f16$Type)

f16_typ <- f16 %>%
  gather()


# What percentage of the population has a Federal Firearms License? -----------

# data sourced from Wikipedia: 
# https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population#States_and_territories

# load and cleanse population data
statepop <- read.csv("data/population-state-estimate.csv", stringsAsFactors = F)
str(statepop)

statepop <- statepop %>% select(X.State.or.territory., Population.estimate..July.1..2016, Estimated.pop..per.House.seat..2016,
                    Percent.of.total.U.S..pop...2016.note.1.)

colnames(statepop) <- c("state", "EstPop2016", "EstPopPerHouseSeat", "PercentUS")

# add state abbreviation to statepop dataframe
statepop$PremiseState <- state.abb[match(statepop$state, state.name)]

# merge license count df with population df 
FFL16_pop <- merge(FFL16_Regions, statepop, by = "PremiseState")

# convert population numbers to int and double

FFL16_pop$EstPop2016 <- gsub(",", "", FFL16_pop$EstPop2016)
FFL16_pop$EstPop2016 <- as.integer(FFL16_pop$EstPop2016)

FFL16_pop$EstPopPerHouseSeat <- gsub(",", "", FFL16_pop$EstPopPerHouseSeat)
FFL16_pop$EstPopPerHouseSeat <- as.integer(FFL16_pop$EstPopPerHouseSeat)

FFL16_pop$PercentUS <- gsub("%", "", FFL16_pop$PercentUS)
FFL16_pop$PercentUS <- as.double(FFL16_pop$PercentUS)

# create new column with proportion of FFL holding pop to total pop per state
FFL16_pop <- FFL16_pop %>%
  mutate(percentFFL = LicCount/EstPop2016)

# write.csv(FFL16_pop, file = "data/2016-FFL-population", row.names = F)

# explore ratio of FFL:total population
summary(FFL16_pop$percentFFL)

FFL16_pop$state <- factor(FFL16_pop$state)
FFL16_pop$Region <- factor(FFL16_pop$Region)

ggplot(data = FFL16_pop) +
  geom_point(mapping = aes(state, LicCount, fill = Region), size = 3) +
  scale_fill_manual(values = region.pal) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(title = "2016: Federal Firearms License count by state", x = "", y = "")


sum(FFL16_pop$LicCount)
# 794562
sum(as.numeric(FFL16_pop$EstPop2016))
# 3224463440

794562/3224463440


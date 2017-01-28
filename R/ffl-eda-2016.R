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

f16 <- fread("data/ffl-2016-V3.csv", stringsAsFactors = T)
f16 <- as.data.frame(f16)

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


# Broadly: which states had the most firearms licenses? -----------------------

display.brewer.all()

# define a palette for each region (7 total)
region.pal <- brewer.pal(7, "RdBu")

summary(f16$LicCount)

texas <- f16 %>% filter(PremiseState == "TX")
nrow(texas)

# A look at license count by state, filled by region
ggplot(f16, aes(reorder(PremiseStateFull, LicCount), fill = LicCount)) + 
  geom_bar() +
  scale_fill_gradient2(low = "deepskyblue4", mid = "gray96", high = muted("firebrick4"),
                       midpoint = 25220) +
  scale_y_discrete(limits = c(0, 5000, 10000, 20000, 40000, 80000)) +
  theme_minimal(base_size = 13.75, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title.x = element_text(family = "Times", face = "italic", size = 12,
                                    margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.text.x = element_text(size = 12),
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
ggplot(f16, aes(reorder(PremiseStateFull, LicCount), fill = month)) + 
  geom_bar() +
  scale_fill_manual(values = month_pal) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12,
                                  margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
        axis.title.x = element_text(margin = margin(20, 0, 0, 0))) +
  labs(title = "2016: Federal Firearms Licenses per month, by State/Territory", 
       x = "", y = "number of licenses") +
  coord_flip()

# facet by month
ggplot(f16, aes(reorder(PremiseStateFull, LicCount), fill = LicCount)) + 
  geom_bar() +
  facet_grid(. ~ month) +
  scale_fill_gradient2(low = "deepskyblue4", mid = "antiquewhite1", high = "firebrick4",
                        midpoint = 25220, guide = F) +
  scale_y_continuous(limits = c(0, 8000), breaks = c(2000, 4000, 6000, 8000)) +
  theme_gray(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm"),
        axis.title = element_text(family = "Times", face = "italic", size = 12,
                                  margin = unit(c(0.5, 0, 0, 0), "cm")),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 10),
        legend.title = element_text(family = "Times", face = "italic", size = 12)) +
  labs(title = "2016: Federal Firearms Licenses per month, by State/Territory", 
       x = "", y = "number of licenses", fill = "annual total") +
  coord_flip()

# There was not much variance from month to month.
# this is likely due to licenses not expiring.

# There's a Dealer in Destructive Devices in each region.

#### The following two sections may warrant their own script
#### 1. Summary by Region and 2. Percentage of Population


# What percentage of population holds a firearms license? ---------------------

# explore ratio of FFL:total population
summary(f16$percentFFL)
# at the maximum, there's 1.3% of a states population holding firearms licenses.

sum(f16$LicCount)
# 794562
sum(as.numeric(f16$EstPop2016))
# 3224463440

794562/3224463440

# Single Month Exploration ----------------------------------------------------

# How many licenses were there each month? 

months16 <- f16 %>%
  group_by(month) %>%
  count()

ggplot(months16, aes(month, n, group = 1)) +
  geom_line(linetype = "dashed", size = 1) +
  geom_point(size = 4, shape = 21, aes(fill = n)) +
  scale_fill_gradient(low = "antiquewhite2", 
                      high = "firebrick3",
                       guide = F) +
  labs(title = "2016: Number of Licenses by Month", x = "month",
       y = "number of licenses") +
  pd.theme

# How does this compare to 2015?
f15 <- fread("data/ffl-2015.csv")

months15 <- f15 %>%
  group_by(month) %>%
  count()

ggplot(months15, aes(month, n, group = 1)) +
  geom_line(linetype = "dashed", size = 1) +
  geom_point(size = 4, shape = 21, aes(fill = n)) +
  scale_fill_gradient(low = "antiquewhite2", 
                      high = "firebrick3",
                      guide = F) +
  labs(title = "2015: Number of Licenses by Month", x = "month",
       y = "number of licenses") +
  pd.theme

# There's a dip in the number of licenses in December - 
# perhaps some expire at the end of the year.

# How does the overall from 2015 through 2016 count look?
months15$year <- "2015"
months16$year <- "2016"

m1516 <- rbind(months15, months16)
m1516$monthyear <- paste(m1516$year, m1516$month, "01", sep = "/")
m1516$monthyear <- as.Date(m1516$monthyear)
datebreaks <- m1516$monthyear

ggplot(m1516, aes(monthyear, n, group = 1)) +
  geom_line(linetype = "dashed", size = 1) +
  geom_point(size = 4, shape = 21, aes(fill = n)) +
  scale_x_date(breaks = datebreaks) +
  scale_fill_gradient(low = "antiquewhite2", 
                      high = "firebrick3",
                      guide = F) +
  pd.theme + theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                              vjust = 1, size = 10)) +
  labs(title = "2015-2016: Number of Licenses by Month", 
       x = "month (gap indicates no data provided)",
       y = "number of licenses")

# from 2015 through 2016, approximately 2000 more licenses were issued.

jan16 <- f16 %>% filter(month == "01")







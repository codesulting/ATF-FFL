# Federal Firearms Licenses
# FFA & Immigration Data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(MASS)
library(quantreg)

# load themes and functions
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# ATF - FFL data
ffl <- read.csv("data/ffl-2016-perCapita-compact.csv")

# US Census - all population data 2010-2016
pop <- read.csv("data/nst-est2016-alldata.csv")
pop <- pop %>%
  dplyr::select(NAME, contains("2012"), contains("2013"), contains("2014"))

# remove US total, regions, DC, PR, 
pop <- pop[-c(1:5, 14, 57), ]

# Immigration Data ------------------------------------------------------------
# Department of Homeland Security Immigration Yearbooks
# https://www.dhs.gov/immigration-statistics/yearbook/2014

# This table (4) is defined as:
# Persons Obtaining Lawful Permanent Resident Status By State Or Territory Of Residence: Fiscal Years 2012 To 2014
# Foreign nationals who are granted lawful permanent residence
# immigrants who receive a "green card"
# immigrants admitted as temporary nonimmigrants
# immigrants granted asylum or refugee status
# or immigrants who are naturalized

# load immigration data
immigration <- read.csv("data/Immigration-Permanent-Residents-2014-Table4-Homeland-Security.csv",
                        stringsAsFactors = F)
colnames(immigration)[2:4] <- c("y2012", "y2013", "y2014")

# remove commas
immigration$y2012 <- gsub(",", "", immigration$y2012)
immigration$y2013 <- gsub(",", "", immigration$y2013)
immigration$y2014 <- gsub(",", "", immigration$y2014)
immigration$y2012 <- as.integer(immigration$y2012)
immigration$y2013 <- as.integer(immigration$y2013)
immigration$y2014 <- as.integer(immigration$y2014)

# merge FFL, population, and immigration data
immigration <- ffl %>%
  dplyr::select(NAME, perCapitaFFL) %>%
  left_join(immigration) %>%
  left_join(pop)

# per capita 2014
perCapita2014 <- function (x) {
  x <- (x / immigration$POPESTIMATE2014) * 100000
  x
}

# create per capita variables 
# and ratio to international migration
immigration <- immigration %>%
  mutate(y2014.perCapita = perCapita2014(y2014),
         y2014.ratio = y2014 / INTERNATIONALMIG2014,
         log2014.perCapita = log10(y2014.perCapita),
         logFFL = log10(perCapitaFFL),
         intl.migration.perCapita = (INTERNATIONALMIG2014/POPESTIMATE2014) * 100000)

# Exploratory Plots -----------------------------------------------------------

# plot FFL vs immigration w/ lm and rlm lines
ggplot(immigration, aes(y2014.perCapita, perCapitaFFL, label = NAME)) +
  geom_point(alpha = 0.75) +
  geom_smooth(method = "lm", se = F, size = 0.5, 
              linetype = "dashed", color = "steelblue") +
  geom_smooth(method = "rlm", se = F, size = 0.75, 
              linetype = "dotted", color = "firebrick3") +
  geom_text(hjust = -0.08, vjust = 1.05, 
            size = 3.25, alpha = 0.9, 
            check_overlap = T) +
  expand_limits(x = c(0, 800)) +
  pd.scatter +
  labs(x = "net immigration per capita", 
       y = "per capita Federal Firearms Licenses")

# The robust line shows a slightly gentler slope.

# plot FFL vs immigration on log scale w/ lm and rlm lines
ggplot(immigration, aes(log2014.perCapita, perCapitaFFL, label = NAME)) +
  geom_point() +
  geom_smooth(method = "lm", se = F, size = 0.5,
              linetype = "dashed", color = "steelblue4") +
  geom_smooth(method = "rlm", se = F, size = 1.1, 
              linetype = "dotted", color = "firebrick3") +
  geom_text(hjust = -0.08, vjust = 1.025, 
            size = 3.25, alpha = 0.9,
            check_overlap = T) +
  scale_x_log10() + scale_y_log10() +
  expand_limits(x = c(2, 3), y = c(10, 150)) +
  pd.scatter +
  labs(x = "(log) net immigration per capita", 
       y = "per capita Federal Firearms Licenses")

# Regression Tree Model -------------------------------------------------------

library(rpart)
library(rpart.plot)
library(tree)

rownames(immigration) <- immigration$NAME

# remove state names and log FFL
immigration <- immigration %>%
  dplyr::select(-NAME, -logFFL)

########
#### The log/per capita/total derivations are probably too correlated to be useful 
#### on their own.
########
# subset of 2014 immigration-related variables
# immigration.sub <- immigration %>%
#   mutate(.rownames = rownames(immigration),
#          logIntlMigration = log10(INTERNATIONALMIG2014),
#          log2014 = log10(y2014)) %>%
#   dplyr::select(.rownames, perCapitaFFL, y2014, y2014.perCapita, 
#                 logIntlMigration, log2014,
#                 intl.migration.perCapita, INTERNATIONALMIG2014)

# Regression Trees on Full Immigration Data -----------------------------------

# `rpart` model on full immigration data
immigration.tree.a <- rpart(perCapitaFFL ~ ., data = immigration)
rpart.plot(immigration.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# `tree` model on full immigration data
immigration.tree.a2 <- tree(perCapitaFFL ~ ., data = immigration)
par(mfrow = c(1, 1), family = "GillSans")
plot(immigration.tree.a2)
text(immigration.tree.a2, pretty = 0)

# Regression Trees on  Immigration Data Subset---------------------------------
# the subset includes only data from the year 2014,
# and remove rate variables

# subset of 2014 immigration-related variables,
# with per capita transformation applied
immigration.sub <- immigration %>%
  mutate(.rownames = rownames(immigration)) %>%
  dplyr::select(.rownames, perCapitaFFL, contains("2014"), -starts_with("R"), 
                -y2014.perCapita, -y2014.ratio, -log2014.perCapita, -POPESTIMATE2014) %>%
  mutate_each(funs(perCapita2014), 3:10)

str(immigration.sub)

# `rpart` model on immigration data subset
immigration.tree.b <- rpart(perCapitaFFL ~ .-.rownames, data = immigration.sub)
rpart.plot(immigration.tree.b, type = 1, extra = 1,
           digits = 4, cex = 0.8, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.8,
           fallen.leaves = T)

# `tree` model on immigration data subset
immigration.tree.b2 <- tree(perCapitaFFL ~ .-.rownames, data = immigration.sub)
plot(immigration.tree.b2)
text(immigration.tree.b2, pretty = 0)

# Immigration: Decision Tree Splits -------------------------------------------

summary(immigration.sub$INTERNATIONALMIG2014)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     516    2741    7169   19480   21650  138800

summary(immigration.sub$NATURALINC2014)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   -58.91  259.80  368.80  396.30  533.60 1181.00

summary(immigration.sub$y2014)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#   42.36  122.60  183.00  222.80  305.50  717.10

# plot Natural Increase (Births - Deaths)
ggplot(immigration.sub, 
       aes(reorder(.rownames, NATURALINC2014), 
           NATURALINC2014, 
           fill = NATURALINC2014)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite2",
                       high = "firebrick4",
                       midpoint = 396, guide = F) +
  coord_flip() + pd.theme +
  labs(x = "", y = "per capita natural population increase")

# plot natural increase < 509
immigration.sub %>%
  filter(NATURALINC2014 > 509) %>%
  ggplot(aes(reorder(.rownames, NATURALINC2014), 
             NATURALINC2014, 
             fill = NATURALINC2014)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite2",
                       high = "firebrick4",
                       midpoint = 396, guide = F) +
  coord_flip() + pd.theme +
  labs(x = "", y = "per capita natural population increase")

# per capita immigration histograms
immigration.sub %>%
  dplyr::select(.rownames, perCapitaFFL, INTERNATIONALMIG2014, DEATHS2014, y2014) %>%
  gather(type, value, 3:5) %>%
  group_by(type) %>%
  ggplot(aes(value)) + 
  geom_histogram(binwidth = 25, color = "black", fill = "white") +
  facet_wrap(~ type, scales = "free", ncol = 1) +
  pd.facet

# total immigration histograms
immigration.sub %>%
  dplyr::select(.rownames, perCapitaFFL, y2014, INTERNATIONALMIG2014) %>%
  gather(type, value, 3:4) %>%
  group_by(type) %>%
  ggplot(aes(value)) + 
  geom_histogram(binwidth = 25, color = "black", fill = "white") +
  facet_wrap(~ type, scales = "free", ncol = 1)

# Decision Tree Splits --------------------------------------------------------

# scatterplot with labels and decision-tree splits: rpart model
ggplot(immigration.sub, 
       aes(y2014, NATURALINC2014, 
           label = .rownames, 
           size = perCapitaFFL)) +
  geom_segment(x = 216.3, xend = 216.3, y = -200, yend = 1400,
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 0, xend = 216.3, y = 509.8, yend = 509.8, 
               linetype = "dashed", color = "red3", size = 0.3) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", 
                        midpoint = 52, guide = F) +
  scale_size(name = "per capita FFLs", range = c(1, 20), guide = F) +
  geom_text(aes(y2014, NATURALINC2014), size = 3, 
            hjust = -0.1, vjust = 1.15,
            check_overlap = T, family = "GillSans", 
            data = immigration.sub) +
  pd.facet + expand_limits(x = c(75, 750)) +
  labs(x = "foreign-born immigrants per capita", 
       y = "per capita natural population increase",
       color = "per capita FFLs")

# scatterplot with labels and decision-tree splits: rpart model
ggplot(immigration.sub, 
       aes(y2014, NATURALINC2014, 
           label = .rownames, 
           size = perCapitaFFL)) +
  geom_segment(x = 216.3, xend = 216.3, y = -200, yend = 1400,
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = 0, xend = 216.3, y = 509.8, yend = 509.8, 
               linetype = "dashed", color = "red3", size = 1) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", 
                        midpoint = 52, guide = F) +
  scale_size(name = "per capita FFLs", range = c(1, 44), guide = F) +
  geom_text(aes(y2014, NATURALINC2014), size = 9.5, 
            hjust = -0.1, vjust = 1.15,
            check_overlap = T, family = "GillSans", 
            data = immigration.sub) +
  pd.hires + expand_limits(x = c(75, 750)) +
  labs(x = "foreign-born immigrants per capita", 
       y = "per capita natural population increase",
       color = "per capita FFLs")

# Single Linear Regressions on Immigration Variables --------------------------

im01 <- tidy(lm(perCapitaFFL ~ y2014, data = immigration.sub))
im02 <- tidy(lm(perCapitaFFL ~ INTERNATIONALMIG2014, data = immigration.sub))
im03 <- tidy(lm(perCapitaFFL ~ NATURALINC2014, data = immigration.sub))
im04 <- tidy(lm(perCapitaFFL ~ DEATHS2014, data = immigration.sub))

single.immigrations <- bind_rows(im01, im02, im03, im04) %>%
  filter(term != "(Intercept)") %>%
  arrange(p.value)

single.immigrations
#                       term      estimate    std.error statistic      p.value
#      1                y2014 -0.083914355 0.01698826 -4.9395510 0.00000990162
#      2 INTERNATIONALMIG2014 -0.085035288 0.01760288 -4.8307589 0.00001431094
#      3       NATURALINC2014  0.013249795 0.01313756  1.0085432 0.31825098157
#      4           DEATHS2014  0.009657999 0.02468805  0.3912014 0.69737952716

im.all <- lm(perCapitaFFL ~ .-.rownames, data = immigration.sub)
summary(im.all)

# Robust Regressions - Outlier Detection --------------------------------------

rownames(immigration.sub) <- immigration.sub$.rownames

immigration.robust <- immigration.sub %>%
  dplyr::select(-.rownames, -NETMIG2014, -NATURALINC2014)

str(immigration.robust)

# Robust Regression 02
im.rr01 <- rlm(perCapitaFFL ~ ., data = immigration.robust)
summary(im.rr01)
fortify(im.rr01)

im.weights <- data.frame(.rownames = rownames(immigration.sub),
                         resid = im.rr01$resid, 
                         weight = im.rr01$w) %>% arrange(weight)

im.weights

# Robust Regression 02
im.rr02 <- rlm(perCapitaFFL ~ NATURALINC2014, data = immigration.sub)
summary(im.rr02)

im.weights.02 <- data.frame(.rownames = rownames(immigration.sub),
                         resid = im.rr02$resid, 
                         weight = im.rr02$w) %>% arrange(weight)

im.weights.02

# Robust Regression 03
im.rr03 <- rlm(perCapitaFFL ~ y2014 + NATURALINC2014 + INTERNATIONALMIG2014,
                data = immigration.sub)
summary(im.rr03)

im.weights03 <- data.frame(.rownames = rownames(immigration.sub),
                           resid = im.rr03$resid,
                           weight = im.rr03$w) %>% arrange(weight)

im.weights03

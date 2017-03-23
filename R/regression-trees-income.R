# Regression Trees 03
# Federal Firearms License data
# Income Features

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(broom)
library(rpart)
library(rpart.plot)
library(tree)
library(ggplot2)
library(ggdendro)

# load themes and functions
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

income.perCapita <- read.csv("data/per-capita-clean/per-capita-income.csv")
rownames(income.perCapita) <- income.perCapita$X
income.perCapita$X <- NULL

# Income features -------------------------------------------------------------

# rename columns for ordering
colnames(income.perCapita) <- c("a.LessThan5000", "b.5000to9999", "c.10000to14999",
                                "d.15000to19999", "e.20000to24999", "f.250000to34999",
                                "g.35000to49999", "h.50000to74999", "i.75000to99999",
                                "j.100000to149000", "k.150000.or.more", "perCapitaFFL")

str(income.perCapita)

# `tree` model
income.tree.b <- tree(perCapitaFFL ~ ., data = income.perCapita)
plot(income.tree.b)
text(income.tree.b, pretty = 0)

# `rpart` tree
income.tree.a <- rpart(perCapitaFFL ~ ., data = income.perCapita)
rpart.plot(income.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# within a range, more income leads to more FFLs
# after getting to super-rich (5289 per 100k households), this trend reverses 
# and the mean FFL count goes down. 5289 households however, is above 
# the 3rd quantile for that particular distribution.

# Examing Split Variable Distributions ----------------------------------------

# upper class income
summary(income.perCapita$k.150000.or.more)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1699    2806    3203    3796    4721    6972

# distibutions of all income brackets
income.perCapita %>%
  mutate(NAME = rownames(income.perCapita)) %>%
  gather("category", "pop.per.100k", 1:11) %>%
  ggplot(aes(x = pop.per.100k, y = ..density..)) +
  geom_histogram(binwidth = 250, color = "antiquewhite2", fill = "white") +
  geom_density(stat = "density", fill = "red4", 
               color = "firebrick3", alpha = 0.05) +
  facet_wrap(~ category, ncol = 3) +
  pd.scatter +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black")) +
  labs(x = "population per 100k",
       title = "Distribution of Population by Annual Income")

# examine top split
summary(income.perCapita$h.50000to74999)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 5424    6392    6915    6853    7299    8150

quantile(income.perCapita$h.50000to74999)
#       0%      25%      50%      75%     100% 
# 5423.926 6392.141 6914.593 7298.875 8149.873 

# upper middle class
ggplot(income.perCapita, aes(h.50000to74999)) +
  geom_histogram(binwidth = 375, color = "black", fill = "white") +
  geom_vline(xintercept = 7291, color = "firebrick3", linetype = "dashed") +
  pd.scatter +
  labs(x = "population per 100k",
       title = "Distribution of Population by Annual Income: $50,000 to $74,999")

# distribution of tree-split variables
income.perCapita %>%
  select(h.50000to74999, k.150000.or.more, a.LessThan5000) %>%
  mutate(NAME = rownames(income.perCapita)) %>%
  gather("category", "pop.per.100k", 1:3) %>%
  ggplot(aes(x = pop.per.100k, y = ..density..)) +
  geom_histogram(binwidth = 250, color = "antiquewhite2", fill = "white") +
  geom_density(stat = "density", fill = "red4", 
               color = "firebrick3", alpha = 0.05) +
  facet_wrap(~ category, ncol = 1) +
  pd.facet +
  theme(panel.grid = element_line(color = "gray98", size = 0.01)) +
  labs(x = "population per 100k",
       title = "Distribution of Population by Annual Income: Tree Split Variables")

# Income: Decision Tree Splits ------------------------------------------------

# state names for labels
income.perCapita$.rownames <- rownames(income.perCapita)

# scatterplot with labels and decision-tree splits
income.p1 <- ggplot(income.perCapita, 
                    aes(h.50000to74999, k.150000.or.more, 
                        label = .rownames, 
                        size = perCapitaFFL)) +
  geom_segment(x = 7291, xend = 7291, y = 0, yend = 10000,
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 0, xend = 7291, y = 5289, yend = 5289, 
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 0, xend = 7291, y = 2732, yend = 2732, 
               linetype = "dotted", color = "red3", size = 0.1) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  scale_size(name = "per capita FFLs", range = c(1, 14)) +
  geom_text(aes(h.50000to74999, k.150000.or.more, label = .rownames),
            size = 3, hjust = -0.01, vjust = -0.55, 
            check_overlap = T, family = "GillSans", data = income.perCapita) +
  pd.facet +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        panel.grid = element_blank()) +
  labs(x = "per capita population: annual household income $50,000-74,999", 
       y = "per capita population: annual household income $150,000 or more",
       color = "per capita FFLs")

income.p1

# high resolution version
ggplot(income.perCapita, 
       aes(h.50000to74999, k.150000.or.more, 
           label = .rownames, 
           size = perCapitaFFL)) +
  geom_segment(x = 7291, xend = 7291, y = 0, yend = 10000,
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 0, xend = 7291, y = 5289, yend = 5289, 
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 0, xend = 7291, y = 2732, yend = 2732, 
               linetype = "dotted", color = "red3", size = 0.1) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  scale_size(name = "per capita FFLs", range = c(1, 44)) +
  geom_text(aes(h.50000to74999, k.150000.or.more, label = .rownames),
            size = 12, hjust = -0.01, vjust = -0.55, 
            check_overlap = T, family = "GillSans", data = income.perCapita) +
  pd.hires +
  theme(legend.position = "right",
        legend.title = element_text(size = 28),
        panel.grid = element_blank()) +
  labs(x = "per capita population: annual household income $50,000-74,999", 
       y = "per capita population: annual household income $150,000 or more",
       color = "per capita FFLs")


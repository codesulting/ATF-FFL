# Regression Trees 03
# Federal Firearms License data
# Industry Features

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(broom)
library(rpart)
library(rpart.plot)
library(tree)
library(ggplot2)
library(ggendro)

# load themes and functions
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# per capita Industry data
industry.perCapita <- read.csv("data/per-capita-clean/per-capita-industry.csv")
rownames(industry.perCapita) <- industry.perCapita$X
industry.perCapita$X <- NULL
str(industry.perCapita)

# Industry Features -----------------------------------------------------------

# `rpart` model
industry.tree.a <- rpart(perCapitaFFL ~ ., data = industry.perCapita)

rpart.plot(industry.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(industry.tree.a)


# Examing Split Variable Distributions ----------------------------------------

# distribtion of Agriculture
summary(industry.perCapita$agriculture)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   162.3   517.4  1020.0  1299.0  1655.0  6497.0

# distribtion of tree-split varibles
industry.perCapita %>%
  select(agriculture, manufacturing, perCapitaFFL) %>%
  mutate(NAME = rownames(industry.perCapita)) %>%
  gather("industry", "pop.per.100k", 1:2) %>%
  ggplot(aes(x = pop.per.100k)) +
  geom_histogram(binwidth = 250, color = "gray50", 
                 fill = "deepskyblue4", alpha = 0.25) +
  facet_wrap(~ industry, ncol = 1) +
  pd.facet +
  theme(panel.grid = element_line(color = "gray98", size = 0.01)) +
  labs(x = "population per 100k",
       title = "Distribution of Population by Industry: Tree Split Variables")

# boxplot of tree-split varibles
industry.perCapita %>%
  select(agriculture, manufacturing, perCapitaFFL) %>%
  mutate(NAME = rownames(industry.perCapita)) %>%
  gather("industry", "pop.per.100k", 1:2) %>%
  ggplot(aes(x = industry, y = pop.per.100k)) +
  geom_boxplot() + pd.facet +
  labs(y = "population per 100k", x = "industry",
       title = "Distribution of Population by Industry: Tree Split Variables")

# Industry: Decision Tree Splits ----------------------------------------------

# scatterplot of tree-split varibles
industry.perCapita$.rownames <- rownames(industry.perCapita)

ggplot(industry.perCapita, aes(agriculture, manufacturing, label = .rownames)) +
  geom_segment(x = 2287, xend = 2287, y = 0, yend = 10000,
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 1269, xend = 1269, y = 0, yend = 10000, 
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = -1000, xend = 1269, y = 5250, yend = 5250, 
               linetype = "dashed", color = "red3", size = 0.10) +
  geom_point(aes(size = perCapitaFFL*8, color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  scale_size(name = "per capita FFLs", range = c(1, 18)) +
  geom_text(aes(agriculture, manufacturing, label = .rownames),
            size = 3, hjust = 0.9, vjust = -0.55, 
            check_overlap = T, family = "GillSans", data = industry.perCapita) +
  expand_limits(x = c(-50, 7000)) +
  pd.facet +
  theme(legend.position = "none",
        legend.title = element_text(size = 10),
        panel.grid = element_blank()) +
  labs(x = "Agriculture, Hunting & Fishing, Mining workforce per 100k", 
       y = "Manufacturing workforce per 100k",
       color = "per capita FFLs")

# high resolution version
ggplot(industry.perCapita, aes(agriculture, manufacturing, label = .rownames)) +
  geom_segment(x = 2287, xend = 2287, y = 0, yend = 10000,
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = 1269, xend = 1269, y = 0, yend = 10000, 
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = -1000, xend = 1269, y = 5250, yend = 5250, 
               linetype = "dashed", color = "red3", size = 1) +
  geom_point(aes(size = perCapitaFFL*8, color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  scale_size(name = "per capita FFLs", range = c(1, 42)) +
  geom_text(aes(agriculture, manufacturing, label = .rownames),
            size = 9.5, hjust = 0.9, vjust = -0.55, 
            check_overlap = T, family = "GillSans", data = industry.perCapita) +
  expand_limits(x = c(-50, 7000)) +
  pd.hires +
  theme(legend.position = "none",
        legend.title = element_text(size = 10),
        panel.grid = element_blank()) +
  labs(x = "Agriculture, Hunting & Fishing, Mining workforce per 100k", 
       y = "Manufacturing workforce per 100k",
       color = "per capita FFLs")




# Regression Trees 03
# Federal Firearms License data
# Education Features

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(broom)
library(rpart)
library(rpart.plot)
library(tree)
library(ggplot2)

# load themes and functions
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# per capita Education data
edu.perCapita <- read.csv("data/per-capita-clean/per-capita-education.csv")
rownames(edu.perCapita) <- edu.perCapita$X
edu.perCapita$X <- NULL
str(edu.perCapita)

# Education features ----------------------------------------------------------

# subset for only HS and BA data
edu.pc <- edu.perCapita %>%
  select(perCapitaFFL, contains("HS"), contains("BA"))

# rpart - education tree a
edu.tree.a <- rpart(perCapitaFFL ~ ., data = edu.pc)

par(mfrow = c(1, 1), family = "GillSans")
rpart.plot(edu.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(edu.tree.a)

# the more BAs, the less FFLs

# tree - education tree b
edu.tree.b <- tree(perCapitaFFL ~ ., data = edu.pc)
plot(edu.tree.b)
text(edu.tree.b, pretty = 0)

summary(edu.tree.b)

# Education: Decision Tree Splits ---------------------------------------------

# state names for labels
edu.perCapita$.rownames <- rownames(edu.perCapita)

# scatterplot with labels and decision-tree splits
ggplot(edu.perCapita, aes(pc.18to24.BA, pc.25to34.BA, 
                          label = .rownames, 
                          size = perCapitaFFL)) +
  geom_segment(x = 717, xend = 717, y = 0, yend = 10000,
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 717, xend = 10000, y = 4765, yend = 4765, 
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 966, xend = 966, y = 0, yend = 4765, 
               linetype = "dotted", color = "red3", size = 0.1) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", 
                        midpoint = 52, guide = F) +
  scale_size(name = "per capita FFLs", range = c(1, 14), guide = F) +
  geom_text(aes(pc.18to24.BA, pc.25to34.BA, label = .rownames),
            size = 3, hjust = -0.01, vjust = -0.55, 
            check_overlap = T, family = "GillSans", 
            data = edu.perCapita) +
  expand_limits(x = c(400, 2000)) +
  pd.facet +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        panel.grid = element_blank()) +
  labs(x = "per capita population: 18 to 24 year olds with BA", 
       y = "per capita population: 25 to 34 year olds with BA",
       color = "per capita FFLs")

# scatterplot with labels and decision-tree splits:
# high-resolution version
ggplot(edu.perCapita, aes(pc.18to24.BA, pc.25to34.BA, 
                          label = .rownames, 
                          size = perCapitaFFL)) +
  geom_segment(x = 717, xend = 717, y = 0, yend = 10000,
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = 717, xend = 10000, y = 4765, yend = 4765, 
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = 966, xend = 966, y = 0, yend = 4765, 
               linetype = "dotted", color = "red3", size = 1) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", 
                        midpoint = 52, guide = F) +
  scale_size(name = "per capita FFLs", range = c(1, 42), guide = F) +
  geom_text(aes(pc.18to24.BA, pc.25to34.BA, label = .rownames),
            size = 10, hjust = -0.01, vjust = -0.55, 
            check_overlap = T, family = "GillSans", 
            data = edu.perCapita) +
  expand_limits(x = c(400, 2000)) +
  pd.hires +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        panel.grid = element_blank()) +
  labs(x = "per capita population: 18 to 24 year olds with BA", 
       y = "per capita population: 25 to 34 year olds with BA",
       color = "per capita FFLs")


# Education: high school only -------------------------------------------------

edu.hs <- edu.perCapita %>%
  select(perCapitaFFL, contains("HS"))

# rpart tree
edu.hs.a <- rpart(perCapitaFFL ~ ., data = edu.hs)
rpart.plot(edu.hs.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# `tree` model
edu.hs.b <- tree(perCapitaFFL ~ ., data = edu.hs)
plot(edu.hs.b)
text(edu.hs.b, pretty = 0)

# Federal Firearms License data
# Regression Tree models
# Legislative Features

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

# per capita Legislative/State Government data
legislative.perCapita <- read.csv("data/per-capita-clean/per-capita-legislative.csv")
rownames(legislative.perCapita) <- legislative.perCapita$X
legislative.perCapita$X <- NULL

# Legislative Features --------------------------------------------------------

# `rpart` tree model
legislative.tree.a <- rpart(perCapitaFFL ~ ., data = legislative.perCapita)

rpart.plot(legislative.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(legislative.tree.a)

# `tree` regression model
legislative.tree.b <- tree(perCapitaFFL ~ ., data = legislative.perCapita)
par(family = "GillSans")
plot(legislative.tree.b)
text(legislative.tree.b, pretty = 0)

# Examine Split Variable Distributions ----------------------------------------

# distribtion of tree-split varibles
legislative.perCapita %>%
  select(House.Dem, House.Rep, perCapitaFFL, .rownames) %>%
  gather("category", "number", 1:2) %>%
  ggplot(aes(number)) +
  geom_histogram(binwidth = 10, color = "black", fill = "white") + 
  facet_wrap(~ category, ncol = 1) +
  pd.facet + 
  labs(y = "count", x = "number",
       title = "")

# Legislation: Decision Tree Splits -------------------------------------------

# state names for labels
legislative.perCapita$.rownames <- rownames(legislative.perCapita)

# scatterplot with labels and decision-tree splits: rpart model
ggplot(legislative.perCapita, aes(House.Dem, House.Rep, label = .rownames)) +
  geom_point(aes(size = perCapitaFFL*8, color = perCapitaFFL), position = "jitter") +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  geom_segment(x = 23.5, xend = 23.5, y = -10, yend = 1000,
               linetype = "dotted", color = "red3", size = 0.05) +
  geom_segment(x = 23.5, xend = 1000, y = 44.5, yend = 44.5, 
               linetype = "dotted", color = "red3", size = 0.05) +
  geom_text(aes(House.Dem, House.Rep), alpha = 0.5,
            position = "jitter", size = 2.75, hjust = 1.05, vjust = 1.15,
            check_overlap = F, family = "GillSans") +
  pd.facet +
  theme(legend.position = "none",
        legend.title = element_text(size = 10),
        panel.grid = element_blank()) +
  labs(x = "number of Democrats in State House of Representatives", 
       y = "number of Republicans in State House of Representatives",
       color = "per capita FFLs")

# scatterplot with labels and decision-tree splits: tree model
ggplot(legislative.perCapita, aes(House.Dem, House.Rep,
                             label = .rownames, 
                             size = perCapitaFFL)) +
  geom_segment(x = 23.5, xend = 23.5, y = -20, yend = 1000,
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 23.5, xend = 1000, y = 44.5, yend = 44.5, 
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 40.5, xend = 40.5, y = -10, yend = 44.5, 
               linetype = "dotted", color = "red3", size = 0.25) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  scale_size(name = "per capita FFLs", range = c(1, 14)) +
  geom_text(aes(House.Dem, House.Rep), alpha = 1,
            position = "jitter", size = 2.75, 
            hjust = 1.05, vjust = 1.15,
            check_overlap = T, family = "GillSans") +
  expand_limits(x = c(-5, 200)) +
  pd.facet +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        panel.grid = element_blank()) +
  labs(x = "Democrats in State House of Representatives", 
       y = "Republicans in State House of Representatives",
       color = "per capita FFLs")

# high resolution version: tree model scatterplot
ggplot(legislative.perCapita, aes(House.Dem, House.Rep,
                                  label = .rownames, 
                                  size = perCapitaFFL)) +
  geom_segment(x = 23.5, xend = 23.5, y = -20, yend = 1000,
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = 23.5, xend = 1000, y = 44.5, yend = 44.5, 
               linetype = "dashed", color = "red3", size = 1) +
  geom_segment(x = 40.5, xend = 40.5, y = -10, yend = 44.5, 
               linetype = "dotted", color = "red3", size = 1) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  scale_size(name = "per capita FFLs", range = c(1, 44)) +
  geom_text(aes(House.Dem, House.Rep), alpha = 1,
            position = "jitter", size = 10, 
            hjust = 1.05, vjust = 1.05,
            check_overlap = T, family = "GillSans") +
  expand_limits(x = c(-5, 200)) +
  pd.hires +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        panel.grid = element_blank()) +
  labs(x = "Democrats in State House of Representatives", 
       y = "Republicans in State House of Representatives",
       color = "per capita FFLs")



legislative.perCapita %>%
  filter(State.Control == "Dem" | State.Control == "Rep")


ggplot(legislative.perCapita, aes(House.Dem, House.Rep, 
                                  label = .rownames, 
                                  size = perCapitaFFL)) +
  geom_segment(x = 23.5, xend = 23.5, y = -20, yend = 1000,
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 23.5, xend = 1000, y = 44.5, yend = 44.5, 
               linetype = "dashed", color = "red3", size = 0.25) +
  geom_segment(x = 40.5, xend = 40.5, y = -10, yend = 44.5, 
               linetype = "dotted", color = "red3", size = 0.25) +
  geom_point(aes(color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  scale_size(name = "per capita FFLs", range = c(1, 14)) +
  geom_text(aes(House.Dem, House.Rep, label = State.Control), alpha = 1,
            position = "jitter", size = 2.75,
            hjust = 0, vjust = 0,
            check_overlap = T, family = "GillSans") +
  expand_limits(x = c(-5, 200)) +
  pd.facet +
  theme(legend.position = "right",
        legend.title = element_text(size = 10),
        panel.grid = element_blank()) +
  labs(x = "Democrats in State House of Representatives", 
       y = "Republicans in State House of Representatives",
       color = "per capita FFLs")

# Heatmap - House and Senate totals vs FFL
legislative.perCapita %>%
  select(House.Dem, House.Rep, perCapitaFFL, .rownames) %>%
  filter(.rownames != "New Hampshire") %>%
  gather("field", "value", 1:3) %>%
  ggplot(aes(field, .rownames, fill = value)) +
  geom_tile(color = "white", size = 0.5) + 
  pd.theme +
  scale_fill_gradient2(low = "firebrick4",
                       mid = "antiquewhite2",
                       high = "deepskyblue4", midpoint = 62.5)

# Heatmap - Controlling Parties vs FFL
legislative.perCapita %>%
  select(Legislative.Control, Governing.Party, State.Control, perCapitaFFL, .rownames) %>%
  gather("body", "party", 1:3) %>%
  ggplot(aes(body, 
             reorder(.rownames, perCapitaFFL),
             fill = party)) +
  geom_tile(color = "white", size = 0.5) +
  scale_fill_manual(values = c("deepskyblue4", "gray50", 
                               "black", "firebrick4", 
                               "antiquewhite2")) +
  scale_x_discrete(position = "top") +
  pd.theme + 
  labs(x = "",
       y = "", fill = "control")



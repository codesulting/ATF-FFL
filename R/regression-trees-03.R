# Regression Trees 03
# Federal Firearms License data


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

edu.perCapita <- read.csv("data/per-capita-clean/per-capita-education.csv")
income.perCapita <- read.csv("data/per-capita-clean/per-capita-income.csv")
industry.perCapita <- read.csv("data/per-capita-clean/per-capita-industry.csv")
legislative.perCapita <- read.csv("data/per-capita-clean/per-capita-legislative.csv")
population.perCapita <- read.csv("data/per-capita-clean/per-capita-rural-urban.csv")

all.features <- read.csv("data/per-capita-clean/per-capita-all.csv")

# 2015 population data --------------------------------------------------------
pop.2015 <- read.csv("~/GitHub/ATF-FFL/data/per-capita-clean/pop-2015.csv", stringsAsFactors = F)
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)

ffl <- ffl %>%
  select(NAME, perCapitaFFL) %>%
  left_join(pop.2015, by = "NAME")

all.features.2015 <- all.features %>%
  left_join(ffl)

all.features.2015 <- all.features.2015[, -c(53:61)]

rownames(all.features.2015) <- all.features.2015$NAME
all.features.2015$NAME <- NULL

# clean up rownames -----------------------------------------------------------

rownames(edu.perCapita) <- edu.perCapita$X
edu.perCapita$X <- NULL

rownames(income.perCapita) <- income.perCapita$X
income.perCapita$X <- NULL

rownames(industry.perCapita) <- industry.perCapita$X
industry.perCapita$X <- NULL

rownames(legislative.perCapita) <- legislative.perCapita$X
legislative.perCapita$X <- NULL

rownames(population.perCapita) <- population.perCapita$X
population.perCapita$X <- NULL

rownames(all.features) <- all.features$X
all.features$X <- NULL

# Regression Trees ------------------------------------------------------------


# all features 01 -------------------------------------------------------------

# rpart - tree 01a
all.tree.a <- rpart(perCapitaFFL ~ ., data = all.features)
rpart.plot(all.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# tree- tree 01b
all.tree.b <- tree(perCapitaFFL ~ ., data = all.features)
par(family = "GillSans", cex = 0.85)
plot(all.tree.b)
text(all.tree.b, pretty = 0)

# all features 02 -------------------------------------------------------------

# rpart - tree 02a
all.tree.2a <- rpart(perCapitaFFL ~ ., data = all.features.2015)
rpart.plot(all.tree.2a, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# tree - tree 02b
all.tree.2b <- tree(perCapitaFFL ~ ., data = all.features.2015)
plot(all.tree.2b)
text(all.tree.2b, pretty = 0)

# Education features ----------------------------------------------------------

# subset for only HS and BA data
edu.pc <- edu.perCapita %>%
  select(perCapitaFFL, contains("HS"), contains("BA"))

# rpart - education tree a
edu.tree.a <- rpart(perCapitaFFL ~ ., data = edu.pc)
rpart.plot(edu.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(edu.tree.a)

# vis with ggdendro
edu.tree.a.vis <- dendro_data(edu.tree.a)

ggplot(segment(edu.tree.a.vis)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = edu.tree.a.vis$labels, 
            aes(x = x, y = y - 0.015, label = label), vjust = -0.5, size = 3) +
  geom_text(data = edu.tree.a.vis$leaf_labels, 
            aes(x = x, y = y, label = label), vjust = 0.5, size = 3) +
  pd.theme


# the more BAs, the less FFLs

# tree - education tree b
edu.tree.b <- tree(perCapitaFFL ~ ., data = edu.pc)
plot(edu.tree.b)
text(edu.tree.b, pretty = 0)

summary(edu.tree.b)


# vis with ggdendro
edu.tree.b.vis <- dendro_data(edu.tree.b)

ggplot(segment(edu.tree.b.vis)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(data = edu.tree.b.vis$labels, 
            aes(x = x, y = y, label = label), vjust = -0.5, size = 3) +
  geom_text(data = leaf_label(edu.tree.b.vis), 
            aes(x = x, y = y, label = label), 
            vjust = 0.5, size = 2) +
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0))

ggplot(segment(edu.tree.b.vis)) +
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_text(data = edu.tree.b.vis$labels, 
            aes(x = x, y = y, label = label), vjust = -0.5, size = 3) +
  geom_text(data = edu.tree.b.vis$leaf_labels, 
            aes(x = x, y = y, label = label), vjust = 0.5, size = 3) +
  pd.theme


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

edu.hs.a.vis <- dendro_data(edu.hs.a)
ggplot(segment(edu.hs.a.vis)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(data = edu.hs.a.vis$labels, 
            aes(x = x, y = y, label = label), 
            vjust = -0.5, size = 4) +
  geom_text(data = leaf_label(edu.hs.a.vis), 
            aes(x = x, y = y, label = label), 
            vjust = 0.5, size = 4) +
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0)) +
  pd.theme

# `tree` model
edu.hs.b <- tree(perCapitaFFL ~ ., data = edu.hs)
plot(edu.hs.b)
text(edu.hs.b, pretty = 0)

edu.hs.b.vis <- dendro_data(edu.hs.b)
ggplot(segment(edu.hs.b.vis)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  geom_text(data = edu.hs.b.vis$labels, 
            aes(x = x, y = y, label = label), 
            vjust = -0.5, size = 4) +
  geom_text(data = leaf_label(edu.hs.b.vis), 
            aes(x = x, y = y, label = label), 
            vjust = 0.5, size = 4) +
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0))


# Income features -------------------------------------------------------------

# rename columns for ordering
colnames(income.perCapita) <- c("a.LessThan5000", "b.5000to9999", "c.10000to14999",
                                "d.15000to19999", "e.20000to24999", "f.250000to34999",
                                "g.35000to49999", "h.50000to74999", "i.75000to99999",
                                "j.100000to149000", "k.150000.or.more", "perCapitaFFL")

str(income.perCapita)

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

summary(income.perCapita$k.150000.or.more)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1699    2806    3203    3796    4721    6972

# plot distibution of population by income bracket
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

# distribution of tree-split variables
income.perCapita %>%
  select(h.50000to74999, k.150000.or.more, a.LessThan5000) %>%
  mutate(NAME = rownames(income.perCapita)) %>%
  gather("category", "pop.per.100k", 1:3) %>%
  ggplot(aes(x = pop.per.100k)) +
  geom_histogram(binwidth = 250, color = "antiquewhite3", 
                 fill = "firebrick3", alpha = 0.25) +
  facet_wrap(~ category, ncol = 1) +
  pd.facet +
  theme(panel.grid = element_line(color = "gray98", size = 0.01)) +
  labs(x = "population per 100k",
       title = "Distribution of Population by Annual Income: Tree Split Variables")

# boxplot across tree-split variables
income.perCapita %>%
  select(h.50000to74999, k.150000.or.more, a.LessThan5000, perCapitaFFL) %>%
  mutate(NAME = rownames(income.perCapita)) %>%
  gather("category", "pop.per.100k", 1:3) %>%
  ggplot(aes(x = pop.per.100k, perCapitaFFL)) +
  geom_boxplot() +
  facet_wrap(~ category, ncol = 1) +
  pd.facet +
  theme(panel.grid = element_line(color = "gray98", size = 0.01),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  labs(x = "population per 100k",
       title = "Distribution of Population by Annual Income")

# `tree` model
income.tree.b <- tree(perCapitaFFL ~ ., data = income.perCapita)
plot(income.tree.b)
text(income.tree.b, pretty = 0)

# Industry Features -----------------------------------------------------------

# `rpart` model
industry.tree.a <- rpart(perCapitaFFL ~ ., data = industry.perCapita)
rpart.plot(industry.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(industry.tree.a)

# distribtion of Agriculture
summary(industry.perCapita$agriculturePC)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   162.3   517.4  1020.0  1299.0  1655.0  6497.0

# distribtion of tree-split varibles
industry.perCapita %>%
  select(agriculturePC, manufacturingPC, perCapitaFFL) %>%
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

# distribtion of tree-split varibles
industry.perCapita %>%
  select(agriculturePC, manufacturingPC, perCapitaFFL) %>%
  mutate(NAME = rownames(industry.perCapita)) %>%
  gather("industry", "pop.per.100k", 1:2) %>%
  ggplot(aes(x = industry, y = pop.per.100k)) +
  geom_boxplot() + pd.facet +
  labs(y = "population per 100k", x = "industry",
       title = "Distribution of Population by Industry: Tree Split Variables")

# scatter of tree-split varibles

industry.perCapita$.rownames <- rownames(industry.perCapita)

ggplot(industry.perCapita, aes(agriculturePC, manufacturingPC, label = .rownames)) +
  geom_point(aes(size = perCapitaFFL*8, color = perCapitaFFL)) +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  geom_segment(x = 2287, xend = 2287, y = 0, yend = 10000,
               linetype = "dotted", color = "red3", size = 0.05) +
  geom_segment(x = 1269, xend = 1269, y = 0, yend = 10000, 
               linetype = "dotted", color = "red3", size = 0.05) +
  geom_segment(x = 0, xend = 1269, y = 5290, yend = 5290, 
               linetype = "dotted", color = "red3", size = 0.05) +
  geom_text(aes(agriculturePC, manufacturingPC, label = .rownames),
            size = 3, hjust = -0.01, vjust = -0.55, 
            check_overlap = T, family = "GillSans", data = industry.perCapita) +
  pd.facet +
  theme(legend.position = "none",
        legend.title = element_text(size = 10),
        panel.grid = element_blank()) +
  labs(x = "Agriculture, Hunting & Fishing, Mining workforce per 100k", 
       y = "Manufacturing workforce per 100k",
       color = "per capita FFLs")

# Legislative Features --------------------------------------------------------

# `rpart` tree model
legislative.tree.a <- rpart(perCapitaFFL ~ ., data = legislative.perCapita)
rpart.plot(legislative.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(legislative.tree.a)


legislative.perCapita$.rownames <- rownames(legislative.perCapita)

# plot splits
ggplot(legislative.perCapita, aes(House.Dem, House.Rep, label = .rownames)) +
  geom_point(aes(size = perCapitaFFL*8, color = perCapitaFFL), position = "jitter") +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  geom_segment(x = 23.5, xend = 23.5, y = -10, yend = 1000,
               linetype = "dotted", color = "red3", size = 0.05) +
  geom_segment(x = -100, xend = 1000, y = 44.5, yend = 44.5, 
               linetype = "dotted", color = "red3", size = 0.05) +
  geom_segment(x = 0, xend = 1269, y = 5290, yend = 5290, 
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

legislative.perCapita.2 <- legislative.perCapita %>%
  select(-.rownames)

legislative.tree.b <- tree(perCapitaFFL ~ ., data = legislative.perCapita.2)
plot(legislative.tree.b)
text(legislative.tree.b, pretty = 0)

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



  
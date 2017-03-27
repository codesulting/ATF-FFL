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

# load themes and functions
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

all.features.2016 <- read.csv("data/per-capita-clean/per-capita-all.csv")
rownames(all.features.2016) <- all.features.2016$X
all.features.2016$X <- NULL

# 2015 population data --------------------------------------------------------
pop.2015 <- read.csv("~/GitHub/ATF-FFL/data/per-capita-clean/pop-2015.csv", stringsAsFactors = F)
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)

ffl <- ffl %>%
  select(NAME, perCapitaFFL) %>%
  left_join(pop.2015, by = "NAME")

all.features.2015 <- all.features.2016 %>%
  left_join(ffl)

all.features.2015 <- all.features.2015[, -c(53:61, 97:100)]
all.features.2015 <- all.features.2015[, -c(88:89)]

rownames(all.features.2015) <- all.features.2015$NAME
all.features.2015$NAME <- NULL

# Regression Trees ------------------------------------------------------------

# all features 01 -------------------------------------------------------------
# rpart - tree 01a
all.tree.a <- rpart(perCapitaFFL ~ ., data = all.features.2015)

rpart.plot(all.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# tree- tree 01b
all.tree.b <- tree(perCapitaFFL ~ ., data = all.features.2015)
par(family = "GillSans", cex = 0.85)
plot(all.tree.b)
text(all.tree.b, pretty = 0)

# all features 02: per capita  ------------------------------------------------

# per Capita function
perCapita <- function (x) {
  x <- (x / all.features.2015$POPESTIMATE2015) * 100000
  x
}

# compute per capita
all.features.pc <- all.features.2015 %>%
  mutate(NAME = rownames(all.features.2015)) %>%
  mutate_each(funs(perCapita), 79:86)

# merge immigration data
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

immigration <- immigration %>%
  select(NAME, y2014) %>%
  left_join(all.features.pc)

all.features.pc <- immigration

# rpart - tree 02
all.tree.02 <- rpart(perCapitaFFL ~ ., data = all.features.pc)

rpart.plot(all.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# tree- tree 02b
all.tree.b <- tree(perCapitaFFL ~ ., data = all.features.pc)
par(family = "GillSans", cex = 0.85)
plot(all.tree.b)
text(all.tree.b, pretty = 0)

# Federal Firearms Licenses
# Firearms Freedom Act data
# Legislative Characteristics of States

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

# FFA data --------------------------------------------------------------------
# Firearms Freedom Act
# http://gunwars.news21.com/interactives/nullification.html

ffa <- read.csv("data/ffa-2014.csv")

# merge with ffl
ffa <- ffa %>%
  left_join(ffl) %>%
  dplyr::select(-LicCount, -Pop2016, -LicCountMonthly, -perCapFFLyear, -ATF.Region)

rownames(ffa) <- ffa$NAME
ffa$NAME <- NULL
str(ffa)

# fit baseline linear model
ffa.model <- lm(perCapitaFFL ~ .-NAME, data = ffa)
summary(ffa.model)

# diagnotic plot of baseline lm model
par(mfrow = c(2, 2), family = "GillSans")
plot(ffa.model)

# look at data distributions
# create long dataframe and faceted histogram plot
ffa %>%
  mutate(NAME = rownames(ffa)) %>%
  dplyr::select(everything()) %>%
  gather(action, result, 1:5) %>%
  ggplot(aes(result, fill = result)) + 
  geom_histogram(stat = "count") +
  facet_wrap(~ action, ncol = 5) +
  scale_fill_manual(values = c("firebrick4",
                               "deepskyblue4")) +
  pd.facet

# generally: fewer states enacted the bill,
# but it's much closer when looking at introducing the bill

# plot introduction and enactment by state
ffa %>%
  mutate(NAME = rownames(ffa)) %>%
  dplyr::select(NAME, ffa.introduced, ffa.enacted) %>%
  gather(action, result, 2:3) %>%
  group_by(result, NAME) %>%
  ggplot(aes(NAME, result, fill = result)) +
  geom_tile() +
  facet_wrap(~ action) +
  scale_fill_manual(values = c("firebrick4",
                               "deepskyblue4")) +
  pd.facet + coord_flip()

# remove leading whitespace
ffa$ffa.introduced <- gsub(" F", "F", ffa$ffa.introduced)
ffa$ffa.introduced <- gsub(" T", "T", ffa$ffa.introduced)
ffa$ffa.introduced <- factor(ffa$ffa.introduced)
levels(ffa$ffa.introduced)

ffa$ffa.enacted <- gsub(" F", "F", ffa$ffa.enacted)
ffa$ffa.enacted <- gsub(" T", "T", ffa$ffa.enacted)
ffa$ffa.enacted <- factor(ffa$ffa.enacted)
levels(ffa$ffa.enacted)

# Which introduced the bill BUT ultimately DID NOT enact it? ------------------

# intro no enact
introduced.not.enacted <- ffa %>% 
  mutate(NAME = rownames(ffa)) %>%
  dplyr::select(NAME, perCapitaFFL, ffa.introduced, ffa.enacted) %>%
  filter(ffa.introduced == "T" & ffa.enacted == "F")

summary(introduced.not.enacted$perCapitaFFL)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   9.811  23.360  31.620  31.290  38.380  49.690

# plot states that introduced but did not  enact FFA bill,
# map per capita FFL to color
ggplot(introduced.not.enacted,
       aes(reorder(NAME, perCapitaFFL), 
           perCapitaFFL, 
           fill = perCapitaFFL)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite2",
                       high = "firebrick4",
                       midpoint = 52) +
  coord_flip() + pd.theme +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18)) +
  labs(title = "Introduced - but not Enacted", x = "",
       y = "per capita FFLs", fill = "")

# Which introduced the bill AND ultimately DID enact it? ----------------------

# intro + enact
introduced.and.enacted <- ffa %>% 
  mutate(NAME = rownames(ffa)) %>%
  dplyr::select(NAME, perCapitaFFL, ffa.introduced, ffa.enacted) %>%
  filter(ffa.introduced == "T" & ffa.enacted == "T")

summary(introduced.and.enacted$perCapitaFFL)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   23.09   30.33   56.75   58.31   81.82  104.70

# plot states that introduced and enacted FFA bill,
# map per capita FFL to color
ggplot(introduced.and.enacted, 
       aes(reorder(NAME, perCapitaFFL), 
           perCapitaFFL, 
           fill = perCapitaFFL)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite2",
                       high = "firebrick4",
                       midpoint = 52) +
  coord_flip() + pd.theme +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18)) +
  labs(title = "Introduced - and Enacted", x = "",
       y = "per capita FFLs", fill = "")

# Which states NEVER introduced the FFA bill? ---------------------------------
never.introduced <- ffa %>% 
  mutate(NAME = rownames(ffa)) %>%
  dplyr::select(NAME, perCapitaFFL, ffa.introduced, ffa.enacted) %>%
  filter(ffa.introduced == "F" & ffa.enacted == "F")

summary(never.introduced$perCapitaFFL)
#    Min. 1st Qu.  Median    Mean  3rd Qu.   Max. 
#   3.687  13.810  21.260  22.060  29.180  54.440

ggplot(never.introduced, aes(reorder(NAME, perCapitaFFL), 
                             perCapitaFFL, 
                             fill = perCapitaFFL)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite2",
                       high = "firebrick4",
                       midpoint = 52) +
  coord_flip() + pd.theme + 
  theme(legend.position = "bottom",
        plot.title = element_text(size = 18)) +
  labs(title = "Never Introduced", x = "",
       y = "per capita FFLs", fill = "")

# North Dakota is the outlier here in never having introduced the FFA bill. 

# Compare FFL distributions of these 3 legislative conditions -----------------

# create long dataframe with condition as T/F
# e.g Introduced and Enacted == T
ffa.3 <- ffa %>%
  dplyr::select(everything()) %>%
  mutate(NAME = rownames(ffa),
         FFA.Introduced.and.Enacted = 
           ifelse(ffa.introduced == "T" & ffa.enacted == "T", "T", "F"),
         FFA.Introduced.not.Enacted = 
           ifelse(ffa.introduced == "T" & ffa.enacted == "F", "T", "F"),
         FFA.Never.Introduced = 
           ifelse(ffa.introduced == "F" & ffa.enacted == "F", "T", "T")) %>%
  gather(process, result, 8:10)

# boxplot of FFLs by condition
ggplot(ffa.3, aes(result, perCapitaFFL)) +
  geom_boxplot(outlier.color = "firebrick4", 
               outlier.shape = 19, 
               outlier.size = 3,
               outlier.alpha = 0.75,
               varwidth = F, notch = F) +
  facet_wrap(~ process, ncol = 1) +
  coord_flip() +
  pd.facet

# ANOVA model -----------------------------------------------------------------
ffa.anova <- aov(perCapitaFFL ~ ., data = ffa)
summary(ffa.anova)
plot(ffa.anova)

# Linear model with Interactions ----------------------------------------------
ffa.model.02 <- lm(perCapitaFFL ~ ffa.introduced * ffa.enacted, data = ffa)
summary(ffa.model.02)

bind_rows(tidy(ffa.model), tidy(ffa.model.02)) %>%
  arrange(p.value)

# Regression Trees  -----------------------------------------------------------

library(rpart)
library(rpart.plot)
library(tree)

ffa.tree.a <- rpart(perCapitaFFL ~ ., data = ffa)

par(mfrow = c(1, 1), family = "GillSans")
rpart.plot(ffa.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# Introduce State Government data ---------------------------------------------
legislative <- read.csv("data/per-capita-clean/per-capita-legislative.csv",
                        stringsAsFactors = F)

rownames(legislative) <- legislative$X
colnames(legislative)[1] <- "NAME"
 
# merge with FFA data
# create NAME variable to merge along with perCapitaFFL
ffa$NAME <- rownames(ffa)

state.gov <- legislative %>%
  left_join(ffa) %>%
  dplyr::select(-Total.Seats, -Total.Senate, -Total.House, -Senate.other)

rownames(state.gov) <- state.gov$NAME

# State Government: Linear model ----------------------------------------------
state.model.01 <- lm(perCapitaFFL ~ .-NAME, data = state.gov)
summary(state.model.01)

par(mfrow = c(2, 2))
plot(state.model.01)

# State Government: Regression Trees ------------------------------------------
state.gov$NAME <- NULL
state.tree.a <- rpart(perCapitaFFL ~ ., data = state.gov)
summary(state.tree.a)

par(mfrow = c(1, 1), family = "GillSans")
rpart.plot(state.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)



# ATF - Federal Firearms Licenses
# Feature Selection + Engineering
# Census Electorate Characteristics & American Community Survey data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# custom plot themes and capwords function
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
source("~/GitHub/ATF-FFL/R/capwords.R")

# racial makeup by state, per 100k
race <- read.csv("~/GitHub/ATF-FFL/data/per-capita-clean/per-capita-race.csv", stringsAsFactors = F)

# ffl data, per 100k
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)

# prepare race data to merge
colnames(race)[1] <- "NAME"
race$NAME <- tolower(race$NAME)
race$NAME <- capwords(race$NAME)

# select state and per capita FFLs
ffl <- ffl %>%
  select(NAME, perCapitaFFL)

# merge race and ffl data
race <- race %>%
  left_join(ffl)

# create long dataframe for facet plots
race.ffl <- race %>%
  select(-Total) %>%
  gather(race, perCapitaPop, 2:11)

levels(as.factor(race.ffl$race))

# EDA: Race -------------------------------------------------------------------

race.ffl %>%
  filter(race != "Male" & race != "Female") %>%
  ggplot(aes(perCapitaPop, perCapitaFFL, label = NAME, fill = perCapitaFFL)) +
  geom_point(shape = 21) +
  geom_text(hjust = 0, vjust = 1, size = 3, check_overlap = T) +
  facet_wrap(~ race, ncol = 4)

# Asian & White
race.ffl %>%
  filter(race == "Asian" | race == "White") %>%
  ggplot(aes(perCapitaPop, perCapitaFFL, label = NAME, fill = perCapitaFFL)) +
  geom_point(shape = 21) +
  geom_text(hjust = 0, vjust = 1, size = 3, check_overlap = T) +
  facet_wrap(~ race, ncol = 1)

# Black & White
race.ffl %>%
  filter(race == "Black" | race == "White") %>%
  ggplot(aes(perCapitaPop, perCapitaFFL, label = NAME, fill = perCapitaFFL)) +
  geom_point(shape = 21) +
  geom_text(hjust = 0, vjust = 1, size = 3, check_overlap = T) +
  facet_wrap(~ race, ncol = 1)
 

race.ffl %>%
  filter(race != "Male" & race != "Female") %>%
  ggplot(aes(reorder(NAME, perCapitaFFL), perCapitaPop, fill = perCapitaFFL)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ race, ncol = 4) +
  scale_fill_gradient2(low = "deepskyblue4",
                       mid = "antiquewhite2",
                       high = "firebrick4",
                       midpoint = 52, guide = F) +
  coord_flip() + pd.facet +
  theme(axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1)) +
  labs(x = "population per capita (100k)", y = "")

# America is mostly White, White.Combination, and White.NonHispanic

# Baseline Linear Model  ------------------------------------------------------

rownames(race) <- race$NAME

race.model <- race %>%
  select(-Total, -Male, -Female)

race.mod01 <- lm(perCapitaFFL ~ .-NAME, data = race.model)
summary(race.mod01)
tidy(race.mod01) %>% arrange(p.value)
glance(race.mod01)

hist(race.mod01$residuals)
par(mfrow = c(2, 2))
plot(race.mod01)

race.coef01 <- augment(race.mod01)

# plot fitted vs observed
race.coef01 %>%
  ggplot(aes(Black, perCapitaFFL, label = .rownames)) +
  geom_point(size = 0.75, alpha = 0.75) +
  geom_point(aes(Black, .fitted),
             color = "firebrick3", alpha = 0.85,
             shape = 23, size = 2.5,
             data = race.coef01) +
  geom_text(size = 2.5, alpha = 0.7,
            position = "jitter",
            check_overlap = T,
            hjust = 0, 
            vjust = 1) +
  geom_errorbar(aes(x = Black, 
                    ymin = .fitted, 
                    ymax = perCapitaFFL), 
                linetype = "dotted") +
  geom_smooth(method = "lm", se = F, size = 0.2, 
              color = "deepskyblue4", 
              linetype = "longdash") +
  pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black")) +
  labs(x = "per capita black population",
       y = "per capita Federal Firearms Licenses")

# Linear Model 02 -------------------------------------------------------------

race.model02 <- race.model %>%
  mutate(total.black = Black + Black.combination,
         total.asian = Asian + Asian.combination,
         total.white = White + White.combination + White.NonHispanic,
         total.hispanic = Hispanic) %>%
  dplyr::select(NAME, perCapitaFFL, total.black, total.asian, total.white, total.hispanic)

race.mod02 <- lm(perCapitaFFL ~ .-NAME, data = race.model02)
summary(race.mod02)
tidy(race.mod02)


# Linear Model 03 -------------------------------------------------------------

race.mod03 <- lm(perCapitaFFL ~ Black + Black.combination + Asian + Asian.combination +
                   Hispanic + White, data = race.model)

summary(race.mod03)
tidy(race.mod03) %>% arrange(p.value)

# are the 'white' variables correlated? 

library(corrplot)
race.model$NAME <- NULL
race.cor <- cor(race.model)
corrplot(race.cor, method = "shade", shade.col = NA, 
         tl.col = "gray23", tl.srt = 45, tl.cex = 1, 
         addCoef.col = "black", number.cex = 1, order = "AOE",
         mar = c(2, 2, 2, 2))

# Linear Model 04 -------------------------------------------------------------

race.mod04 <- lm(perCapitaFFL ~ Black + Asian + Hispanic + White, data = race.model)
summary(race.mod04)

tidy(race.mod04) %>% arrange(p.value)


# Compare all models ----------------------------------------------------------

# compare variable p.values
m01 <- tidy(race.mod01) %>% mutate(model = "01")
m02 <- tidy(race.mod02) %>% mutate(model = "02")
m03 <- tidy(race.mod03) %>% mutate(model = "03")
m04 <- tidy(race.mod04) %>% mutate(model = "04")

linear.comparison <- bind_rows(m01, m02, m03, m04) %>%
  filter(term != "(Intercept)") %>%
  arrange(p.value)

linear.comparison

# compare r-squared
g01 <- glance(race.mod01) %>% mutate(model = "01")
g02 <- glance(race.mod02) %>% mutate(model = "02")
g03 <- glance(race.mod03) %>% mutate(model = "03")
g04 <- glance(race.mod04) %>% mutate(model = "04")

rsq.comparison <- bind_rows(g01, g02, g03, g04) %>%
  arrange(desc(adj.r.squared)) %>%
  dplyr::select(r.squared, adj.r.squared, statistic, p.value, 
                AIC, deviance, model)

rsq.comparison

# Robust Regression Model -----------------------------------------------------

library(MASS)

race.rr <- rlm(perCapitaFFL ~ .-NAME, data = race.model)
summary(race.rr)

rr.weights <- data.frame(.rownames = race$NAME, 
                         .resid = race.rr$resid,
                         weight = race.rr$w) %>% arrange(weight)

rr.weights
#         .rownames       .resid    weight
# 1         Wyoming  54.46646312 0.2119064
# 2         Montana  44.29795370 0.2605492
# 3           Idaho  18.32960966 0.6296819
# 4       Tennessee -13.50771801 0.8544631
# 5         Indiana -11.58653636 0.9961384

rr.weights <- rr.weights %>%
  left_join(augment(race.rr))

rr.weights %>%
  ggplot(aes(Black, perCapitaFFL, label = .rownames)) +
  geom_point(size = 0.75, alpha = 0.75) +
  geom_point(aes(Black, .fitted),
             color = "firebrick3", alpha = 0.8,
             shape = 23, size = 2.5,
             data = rr.weights) +
  geom_text(size = 2.5, alpha = 0.7,
            position = "jitter",
            check_overlap = T,
            hjust = 0, 
            vjust = 1) +
  geom_errorbar(aes(x = Black, 
                    ymin = .fitted, 
                    ymax = perCapitaFFL), 
                linetype = "dotted") +
  geom_smooth(method = "rlm", se = F, size = 0.2, 
              color = "deepskyblue4", 
              linetype = "longdash") +
  pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black")) +
  labs(x = "per capita black population",
       y = "per capita Federal Firearms Licenses")

# Regression Trees ------------------------------------------------------------

library(tree)
library(rpart)
library(rpart.plot)

# `rpart` model
race.tree.a <- rpart(perCapitaFFL ~ ., data = race.model)

par(mfrow = c(1, 1))
rpart.plot(race.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# `tree` model`
race.tree.b <- tree(perCapitaFFL ~ ., data = race.model)
plot(race.tree.b)
text(race.tree.b, pretty = 0)

# model with total variables - rpart
race.tree.02a <- rpart(perCapitaFFL ~ .-NAME, data = race.model02)
rpart.plot(race.tree.02a, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

# model with total variables - tree
race.tree.02b <- tree(perCapitaFFL ~ .-NAME, data = race.model02)
plot(race.tree.02b)
text(race.tree.02b, pretty = 0, cex = 0.8)




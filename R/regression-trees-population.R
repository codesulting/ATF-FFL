# Federal Firearms License data
# Regression Tree models
# Rural-Urban Population Features

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(broom)
library(ggplot2)

# load themes and functions
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# per capita Legislative/State Government data
population.perCapita <- read.csv("data/per-capita-clean/per-capita-rural-urban.csv")
rownames(population.perCapita) <- population.perCapita$X
population.perCapita$X <- NULL

# bind 2015 population data
population.perCapita$NAME <- rownames(population.perCapita)

pop.perCapita <- population.perCapita %>%
  left_join(pop2015) %>%
  select(-contains("2016"))

rownames(pop.perCapita) <- pop.perCapita$NAME

pop.perCapita <- pop.perCapita %>%
  select(-NAME, -atf.Region, -REGION, -DIVISION)

# rural-urban proportions only
population.perCapita <- population.perCapita %>%
 select(-contains("2016"), -NAME, -atf.Region)

# Rural-Urban Proportions Features --------------------------------------------

library(rpart)
library(rpart.plot)
library(tree)

ru.tree.a <- rpart(perCapitaFFL ~ ., data = population.perCapita)
rpart.plot(ru.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

ru.tree.b <- tree(perCapitaFFL ~ ., data = population.perCapita)
par(family = "GillSans", mfrow = c(1, 1))
plot(ru.tree.b)
text(ru.tree.b, pretty = 0)

# Population Features ---------------------------------------------------------

ffl <- read.csv("data/ffl-2016-perCapita-compact.csv")
pop2015 <- read.csv("data/per-capita-clean/pop-2015.csv")

pop2015 <- ffl %>%
  left_join(pop2015) %>%
  select(-ATF.Region, -Pop2016, -LicCount, -LicCountMonthly, -perCapFFLyear, -REGION, -DIVISION)

pop.tree.a <- rpart(perCapitaFFL ~ .-NAME, data = pop2015)
rpart.plot(pop.tree.a, type = 1, extra = 1,
           digits = 4, cex = 0.85, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

pop.tree.b <- tree(perCapitaFFL ~ .-NAME, data = pop2015)
plot(pop.tree.b)
text(pop.tree.b, pretty = 0)

# International Migration again is the strongest indicator. 
# Firearms as a distinctly American endeavor? 

summary(pop2015$INTERNATIONALMIG2015)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     594    3042    7694   20650   22930  148100

# Population: examine distribution of split variables -------------------------

# plot International Migration by State
ggplot(pop2015, aes(reorder(NAME, INTERNATIONALMIG2015), INTERNATIONALMIG2015,
                    fill = INTERNATIONALMIG2015)) +
  geom_bar(stat = "identity") + coord_flip()

# plot International Migration by per Capita FFL
ggplot(pop2015, aes(intl.migration.perCapita, perCapitaFFL, label = NAME)) +
  geom_point() +
  geom_text(hjust = -0.1, vjust = 1, size = 3, 
            check_overlap = T, family = "GillSans") +
  expand_limits(x = c(0, 700)) +
  pd.scatter +
  labs(x = "per capita international migration",
       y = "per capita Federal Firearms Licenses")

# per capita International Migration
pop2015 <- pop2015 %>%
  mutate(intl.migration.perCapita = (INTERNATIONALMIG2015/POPESTIMATE2015) * 100000)

# plot per capita International Migration
ggplot(pop2015, aes(reorder(NAME, intl.migration.perCapita), intl.migration.perCapita,
                    fill = intl.migration.perCapita)) +
  geom_bar(stat = "identity") + coord_flip()

# fit linear model
rownames(pop2015) <- pop2015$NAME
migration.model <- lm(perCapitaFFL ~ intl.migration.perCapita, data = pop2015)
summary(migration.model)

glance(migration.model)
#   r.squared adj.r.squared    sigma statistic      p.value df    logLik      AIC      BIC deviance df.residual
# 1 0.3159448     0.3016937 18.05177  22.16978 2.159102e-05  2 -214.5886 435.1771 440.9132 15641.58          48

par(mfrow = c(2, 2), family = "GillSans")
plot(migration.model)

migration.coef <- augment(migration.model)

# plot fitted vs residuals
ggplot(migration.coef, aes(intl.migration.perCapita, perCapitaFFL, label = .rownames)) +
  geom_point() +
  geom_point(aes(intl.migration.perCapita, .fitted), color = "red3", shape = 2)

# Robust Migration Model ------------------------------------------------------

library(MASS)

migration.rr <- rlm(perCapitaFFL ~ intl.migration.perCapita, data = pop2015)
summary(migration.rr)

# check assigned weights
migration.weights <- data.frame(.rownames = pop2015$NAME, 
                          .resid = migration.rr$resid,
                          weight = migration.rr$w) %>% arrange(weight)

migration.weights
#         .rownames      .resid    weight
# 1         Wyoming  66.9675930 0.2087030
# 2         Montana  63.5027119 0.2200914
# 3          Alaska  59.9722049 0.2330433
# 4    South Dakota  28.4806458 0.4907402
# 5    North Dakota  25.1272929 0.5562322
# 6           Idaho  19.6866046 0.7099838
# 7         Vermont  15.6565914 0.8927414
# 8      California -12.8964124 1.0000000

# merge fitted values with weights
migration.rr.coef <- migration.weights %>%
  left_join(augment(migration.rr)) %>%
  mutate(weighted.fit = weight * .fitted)

# plot fitted vs observed
ggplot(migration.rr.coef, aes(intl.migration.perCapita, weighted.fit, label = .rownames)) +
  geom_point() +
  geom_point(aes(intl.migration.perCapita, perCapitaFFL), 
             color = "red3", shape = 17, size = 2, data = migration.rr.coef) +
  geom_text(hjust = 0, vjust = 1, check_overlap = T)







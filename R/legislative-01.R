# ATF- FFL - NCSL State Legislator Data
# Feature Selection + Engineering

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(MASS)

# plot themes
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
source("~/GitHub/ATF-FFL/R/usa-map-prep.R")

# dataset containing data by state for:
# - state legislature - control
# - state legislature - compensation      
pop <- read.csv("~/GitHub/ATF-FFL/data/population-compact.csv", stringsAsFactors = F)
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)

leg.14 <- read.csv("~/GitHub/ATF-FFL/data/02-state-legislatures/control/2014.csv")
leg.15 <- read.csv("~/GitHub/ATF-FFL/data/02-state-legislatures/control/2015.csv")
leg.16 <- read.csv("~/GitHub/ATF-FFL/data/02-state-legislatures/control/2016.csv")

# add year variable and merge dataframes
leg.14$Year <- "2014"
leg.15$Year <- "2015"
leg.16$Year <- "2016"

legislature <- rbind(leg.14, leg.15, leg.16)

# cleanse data ----------------------------------------------------------------

# replace "Dem*" with "Dem"
levels(legislature$Legis.Control)
levels(legislature$State.Control)
legislature$Legis.Control <- gsub("Dem\\*", "Dem", legislature$Legis.Control)
legislature$State.Control <- gsub("Dem\\*", "Dem", legislature$State.Control)
legislature$Legis.Control <- factor(legislature$Legis.Control)
legislature$State.Control <- factor(legislature$State.Control)

str(legislature)

# Which Party controls more States, 2014-2016?
ggplot(legislature, aes(State.Control)) +
  geom_bar() + pd.theme

# Which Party controls more States, in each year? 
ggplot(legislature, aes(State.Control, fill = State.Control)) +
  geom_bar() +
  facet_wrap(~ Year) + pd.scatter + coord_flip() +
  scale_fill_manual(values = c("deepskyblue4", "antiquewhite2", 
                               "gray23", "firebrick4"), guide = F) +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = "seats", x = "party", 
       title = "2014-2016: State Control by Party")

# Which Party controls more Legislature, in each year? 
ggplot(legislature, aes(Legis.Control, fill = Legis.Control)) +
  geom_bar() +
  facet_wrap(~ Year) + pd.scatter + coord_flip() +
  scale_fill_manual(values = c("deepskyblue4", 
                               "gray23", 
                               "firebrick4", 
                               "antiquewhite2"), guide = F) +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = "seats", x = "party",
       title = "2014-2016: Legislative Control by Party")


# Which is the Governing Party, in each year? 
ggplot(legislature, aes(Gov.Party, fill = Gov.Party)) +
  geom_bar() +
  facet_wrap(~ Year) + pd.scatter + coord_flip() +
  scale_fill_manual(values = c("deepskyblue4", 
                               "firebrick4", 
                               "antiquewhite2"), guide = F) +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.border = element_rect(fill = NA, color = "black")) +
  labs(y = "seats", x = "party",
       title = "2014-2016: Governing Party")

# Republicans dominate both years. Democrats make a very small portion,
# with there being nearly as many "Divided" as there are "Republican". 

# Which States are divided? 
legislature %>%
  filter(State.Control == "Divided") %>%
  ggplot(aes(STATE, State.Control, fill = Legis.Control)) +
  geom_tile() + facet_wrap(~ Year) + pd.scatter + coord_flip() +
  scale_fill_manual(values = c("deepskyblue4",
                               "firebrick4",
                               "antiquewhite2")) +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.border = element_rect(fill = NA, color = "black"))

# Facet Party Totals ----------------------------------------------------------

# clean up variable names
colnames(legislature)[11:13] <- c("Legislative.Control", 
                                  "Governing.Party", 
                                  "State.Control")

legislature.stack <- legislature %>%
  dplyr::select(STATE, Legislative.Control, Governing.Party, State.Control, Year) %>%
  gather("Area", "Party", 2:4)


ggplot(legislature.stack, aes(Party, fill = Party)) +
  geom_bar(stat = "count", position = "dodge") +
  facet_wrap(~ Year) + 
  pd.scatter + 
  scale_fill_manual(values = c("deepskyblue4",
                               "antiquewhite2",
                               "cadetblue4",
                               "gray23",
                               "firebrick4",
                               "antiquewhite4"), guide = F) +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text.x = element_text(angle = 45, size = 11,
                                   hjust = 1, vjust = 1),
        axis.title = element_text(size = 12)) +
  labs(x = "", y = "number of seats")

ggplot(legislature.stack, aes(Year, fill = Party)) +
  geom_bar(stat = "count") +
  facet_wrap(~ Area) + 
  pd.scatter + 
  scale_fill_manual(values = c("deepskyblue4",
                               "antiquewhite2",
                               "cadetblue4",
                               "gray23",
                               "firebrick4",
                               "antiquewhite4")) +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 11),
        axis.title = element_text(size = 12),
        legend.position = "bottom") + 
  labs(x = "", y = "number of states")


# Map Parties by State --------------------------------------------------------

# merge spatial data
colnames(legislature)[1] <- "NAME"
leg.map <- legislature %>%
  left_join(fifty_states)

# map divided states
leg.map %>%
  filter(State.Control == "Divided" & Year == "2014") %>%
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = Legislative.Control)) + 
  scale_fill_manual(values = c("deepskyblue4",
                               "firebrick4",
                               "antiquewhite2")) +
  coord_map("polyconic") +
  pd.theme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 12)) +
  labs(title = "2014: Divided States, by Party in Legislative Control",
       fill = "legislative\ncontrol")

# map all states by Legislative Control 
leg.map %>%
  filter(Year == "2014") %>%
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = Legislative.Control), 
               color = "white", size = 0.025) + 
  scale_fill_manual(values = c("deepskyblue4",
                               "gray23",
                               "firebrick4",
                               "antiquewhite2")) +
  coord_map("polyconic") +
  pd.theme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 12)) +
  labs(title = "2014: by Party in Legislative Control",
       fill = "legislative\ncontrol")

# map all states by Governing Party
leg.map %>%
  filter(Year == "2014") %>%
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = Governing.Party), 
               color = "white", size = 0.025) + 
  scale_fill_manual(values = c("deepskyblue4",
                               "firebrick4")) +
  coord_map("polyconic") +
  pd.theme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 12)) +
  labs(title = "2014: by Governing Party",
       fill = "")

# map all states by State Control
leg.map %>%
  filter(Year == "2014") %>%
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = State.Control)) + 
  scale_fill_manual(values = c("deepskyblue4",
                               "antiquewhite2",
                               "gray23",
                               "firebrick4")) +
  coord_map("polyconic") +
  pd.theme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 12)) +
  labs(title = "2014: by Party in State Control",
       fill = "governing\nparty")

# Facetted Maps ---------------------------------------------------------------
# facet map - state control
leg.map %>% 
  group_by(Year) %>%
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = State.Control)) + 
  scale_fill_manual(values = c("deepskyblue4",
                               "antiquewhite2",
                               "gray23",
                               "firebrick4")) +
  coord_map("polyconic") +
  facet_wrap(~ Year, ncol = 1) +
  pd.theme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 12),
        legend.position = "bottom") +
  labs(title = "State Control by Year",
       fill = "")

# facet map - legislative control
leg.map %>% 
  group_by(Year) %>%
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = Legislative.Control)) + 
  scale_fill_manual(values = c("deepskyblue4",
                               "gray23",
                               "firebrick4",
                               "antiquewhite2")) +
  coord_map("polyconic") +
  facet_wrap(~ Year, ncol = 1) +
  pd.theme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 12),
        legend.position = "bottom") +
  labs(title = "Legislative Control by Year",
       fill = "")

# facet map - governing control
leg.map %>% 
  group_by(Year) %>%
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = Governing.Party)) + 
  scale_fill_manual(values = c("deepskyblue4",
                               "firebrick4",
                               "antiquewhite2")) +
  coord_map("polyconic") +
  facet_wrap(~ Year, ncol = 1) +
  pd.theme +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        legend.title = element_text(size = 12),
        legend.position = "bottom") +
  labs(title = "Governing Party by Year",
       fill = "")

# Model -----------------------------------------------------------------------

# merge ffl data
colnames(leg.14)[1] <- "NAME"

leg.ffl <- leg.14 %>%
  left_join(ffl)

# clean Nebraska
str(leg.ffl)
leg.ffl[27, c(4, 5, 7, 8, 9, 10)] <- "0"

leg.ffl$Total.House <- as.numeric(levels(leg.ffl$Total.House))[leg.ffl$Total.House]
rownames(leg.ffl) <- leg.ffl$NAME
leg.ffl$Total.House <- as.integer(leg.ffl$Total.House)
leg.ffl$Total.House

leg.model <- leg.ffl %>%
  dplyr::select(1:9, 11, 12, 13, 20) %>%
  filter(NAME != "Nebraska")

rownames(leg.model) <- leg.model$NAME

# Model 01 --------------------------------------------------------------------
mod01 <- lm(perCapitaFFL ~ Total.Seats + Total.Senate + Senate.Dem + Senate.Rep +
              Total.House + House.Dem + House.Rep, data = leg.model)
summary(mod01)

par(mfrow = c(2, 2), family = "GillSans")
plot(mod01)

# Model 02 --------------------------------------------------------------------
mod02 <- lm(perCapitaFFL ~ Senate.Dem + Senate.Rep + House.Dem + House.Rep, data = leg.model)
summary(mod02)

par(mfrow = c(2, 2), family = "GillSans")
plot(mod01)

# Model 03 --------------------------------------------------------------------
# proportions

leg.model$Senate.Dem <- as.integer(leg.model$Senate.Dem)
leg.model$Senate.Rep <- as.integer(leg.model$Senate.Rep)
leg.model$House.Dem <- as.integer(leg.model$House.Dem)
leg.model$House.Rep <- as.integer(leg.model$House.Rep)

leg.model <- leg.model %>%
  mutate(ratio.senate.Dems = Senate.Dem/Total.Senate,
         ratio.senate.Reps = Senate.Rep/Total.Senate,
         ratio.house.Dems = House.Dem/Total.House,
         ratio.house.Reps = House.Rep/Total.House,
         ratio.Dems = (Senate.Dem + House.Dem)/Total.Seats,
         ratio.Reps = (Senate.Rep + House.Rep)/Total.Seats)

leg.model.ratio <- leg.model %>%
  dplyr::select(NAME, perCapitaFFL, contains("ratio"))

rownames(leg.model.ratio) <- leg.model.ratio$NAME

mod03 <- lm(perCapitaFFL ~ .-NAME, data = leg.model.ratio)
summary(mod03)

# Model 04 --------------------------------------------------------------------

legislature.ffl <- legislature %>%
  left_join(ffl) %>%
  dplyr::select(1:14, perCapitaFFL)

legislature2$House.Dem <- as.integer(legislature2$House.Dem)
legislature2$House.Rep <- as.integer(legislature2$House.Rep)
rownames(legislature2) <- legislature2$NAME

mod04<- lm(perCapitaFFL ~ .-NAME, data = legislature2)
summary(mod04)


# Model 05 --------------------------------------------------------------------

mod05 <- lm(perCapitaFFL ~ Legislative.Control + Governing.Party + State.Control, 
           data = legislature.ffl)
summary(mod05)
anova(mod05)

# Model 06 --------------------------------------------------------------------

mod06 <- lm(perCapitaFFL ~ Legislative.Control + Governing.Party, data = legislature.ffl)
summary(mod06)
anova(mod06)

# Model 07 --------------------------------------------------------------------

mod07 <- lm(perCapitaFFL ~ Legislative.Control, data = legislature.ffl)
summary(mod07)
anova(mod07)
plot(mod07)

# Robust Regression 01 --------------------------------------------------------

rr.leg.01 <- rlm(perCapitaFFL ~ Legislative.Control, data = legislature.ffl)
summary(rr.leg.01)

weights.rr01 <- data.frame(.rownames = legislature.ffl$NAME, 
                           .resid = rr.leg.01$resid,
                           weight = rr.leg.01$w,
                           year = legislature.ffl$Year) %>% arrange(weight)

weights.rr01 %>% filter(year == "2014")
#         .rownames        .resid    weight year
# 1         Wyoming  7.255818e+01 0.2597453 2014
# 2         Montana  7.164401e+01 0.2630597 2014
# 3          Alaska  4.965067e+01 0.3795889 2014
# 4         Vermont  3.311714e+01 0.5690920 2014
# 5   West Virginia  3.229100e+01 0.5836522 2014
# 6    South Dakota  2.747420e+01 0.6860015 2014
# 7           Idaho  2.458215e+01 0.7667139 2014
# 8           Maine  2.229809e+01 0.8452263 2014
# 9    North Dakota  2.227802e+01 0.8460180 2014
# 10        Alabama -6.873761e+00 1.0000000 2014

# join with fitted and observed data
rr.huber01 <- augment(rr.leg.01) %>%
  left_join(weights.rr01) %>%
  arrange(weight) %>%
  mutate(weighted.resid = .resid * weight,
         weighted.fit = .fitted + weighted.resid)

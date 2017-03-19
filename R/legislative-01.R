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

# Map Controls by State -------------------------------------------------------

# merge spatial data
colnames(legislature)[1] <- "NAME"
leg.map <- legislature %>%
  left_join(fifty_states)

# map divided states
leg.map %>%
  filter(State.Control == "Divided" & Year == "2014") %>%
  ggplot(aes(lon, lat, group = group)) +
  geom_polygon(aes(fill = Legis.Control)) + 
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
  geom_polygon(aes(fill = Legis.Control), 
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
  geom_polygon(aes(fill = Gov.Party), 
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
       fill = "governing\nparty")

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

# Model -----------------------------------------------------------------------

# merge ffl data
colnames(leg.14)[1] <- "NAME"

leg.ffl <- leg.14 %>%
  left_join(ffl)

# clean Nebraska
str(leg.ffl)
leg.ffl$Total.House <- as.numeric(leg.ffl$Total.House)
leg.ffl[27, c(4, 5, 8, 9, 10)] <- 0

rownames(leg.ffl) <- leg.ffl$NAME

leg.model <- leg.ffl %>%
  dplyr::select(1:9, 11, 12, 13, 20)

mod01 <- lm(perCapitaFFL ~ Total.Seats + Total.Senate + Senate.Dem + Senate.Rep +
              Total.House + House.Dem + House.Rep, data = leg.ffl)
summary(mod01)

par(mfrow = c(2, 2), family = "GillSans")
plot(mod01)

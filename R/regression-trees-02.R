# ATF- FFL
# Regression Trees on Federal Firearms License Data
# Census Electorate Characteristics & American Community Survey data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(broom)
library(rpart)
library(rpart.plot)
library(ggplot2)
library(ggtree)
library(ggdendro)

# load themes and functions
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
source("~/GitHub/ATF-FFL/R/capwords.R")

# dataset containing data by state for:
# - population by sex, race/origin
# - industry
# - working class
# - educational attainment
# - financial characteristics

# all ACS data, per capita FFLs by State, Population data, Rural-Urban Proportions data
# acs <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-all-data.csv", stringsAsFactors = F)
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)
pop <- read.csv("~/GitHub/ATF-FFL/data/population-compact.csv", stringsAsFactors = F)

# Industry Data preparation ---------------------------------------------------

# cleanse industry data
industry <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-industry.csv", stringsAsFactors = F)
colnames(industry)[2] <- "NAME"
industry <- industry[-c(9, 52), ]

# Per Capita function
perCap2015 <- function (x) {
  x <- (x / pop$Pop2015) * 100000
  x
}

# create per capita industry variables
industryPerCapita <- industry %>%
  left_join(pop, by = "NAME") %>%
  mutate(workforcePC = perCap2015(ind.01.Civilian.16), 
         agriculturePC = perCap2015(ind.02.Agriculture.Forestry.Fish.Hunt.Mining),
         constructionPC = perCap2015(ind.03.Construction),
         manufacturingPC = perCap2015(ind.04.Manufacturing),
         wholesalePC = perCap2015(ind.05.Wholesale.Trade),
         retailPC = perCap2015(ind.06.Retail.Trade),
         transportationPC = perCap2015(ind.07.Transportation.Warehousing.Util),
         informationPC = perCap2015(ind.08.Information),
         financePC = perCap2015(ind.09.Finance.Insurance.RealEstate),
         pro.scientificPC = perCap2015(ind.10.Professional.Scientific.Mgmt.Admin.Waste),
         educationPC = perCap2015(ind.11.Education.HealthCare.Social),
         artsPC = perCap2015(ind.12.Arts.Entertain.Accomodation.FoodService),
         otherPC = perCap2015(ind.13.OtherServices),
         publicAdminPC = perCap2015(ind.14.PublicAdministration))

# merge data 
ffl.pop <- ffl %>%
  left_join(pop, by = "NAME")

ffl.pop$Pop2016.x <- NULL
colnames(ffl.pop)[13] <- "Pop2016"

industry <- industryPerCapita %>%
  left_join(ffl.pop, by = "NAME")

rownames(industry) <- industry$NAME

# write.csv(industry.perCapita, file = "~/GitHub/federal-firearms-licenses/data/per-capita-industry.csv", 
#           row.names = T)

# Regression Trees: Industry Data ---------------------------------------------

industry.perCapita <- industry %>%
  dplyr::select(perCapitaFFL, 24:37)

str(industry.perCapita)

# grow regression tree
industry.tree <- rpart(perCapitaFFL ~ ., data = industry.perCapita,
                       control = rpart.control(cp = 0.000001))

# plot tree
rpart.plot(industry.tree, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(industry.tree)
printcp(industry.tree)

ggplot(industry.perCapita, aes(agriculturePC, perCapitaFFL)) +
  geom_point() +
  geom_vline(xintercept = 2287) +
  geom_vline(xintercept = 1269)


# Income Data preparation -----------------------------------------------------
income <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-financePerCapita-all.csv", 
                   stringsAsFactors = F)
colnames(income)[3] <- "NAME"
income$X <- NULL
income$total.GEO.id2 <- NULL

# subset variables for modeling
income.perCapita <- income %>%
  dplyr::select(15:25, 38)

rownames(income.perCapita) <- income$NAME
summary(income.perCapita)

# write.csv(income.perCapita, file = "~/GitHub/federal-firearms-licenses/data/per-capita-income.csv",
#           row.names = T)

# Regression Trees: Income data -----------------------------------------------
str(income.perCapita)

income.tree <- rpart(perCapitaFFL ~ ., data = income.perCapita)

rpart.plot(income.tree, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(income.tree)
summary(income.perCapita$perCapita.50000to74999)

# Education Data preparation --------------------------------------------------

# ACS Financial Characteristics data, FFL data, Total Population data
education <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-education.csv", stringsAsFactors = F)

# cleanse and bind
education <- education[-c(9, 52), ]
education$edu.GEO.id2 <- NULL
colnames(education) <- gsub("edu.[0-9][0-9].", "", colnames(education))
colnames(education)[1] <- "NAME"
rownames(education) <- education$NAME

# per capita calculation
edu <- as.data.frame(apply(education[2:52], 2, perCap2015))
edu$NAME <- rownames(edu)

# merge FFL and population data
ffl.pop <- ffl %>%
  left_join(pop, by = "NAME")

ffl.pop$Pop2016.x <- NULL
colnames(ffl.pop)[13] <- "Pop2016"

edu.pc <- ffl.pop %>%
  left_join(edu)

# remove and rename columns: Per Capita
edu.pc[c(2:5, 7:11)] <- NULL
colnames(edu.pc) <- gsub("Total", "pc", colnames(edu.pc))

# subset education categories for 
# population/age bracket, high school, and bachelor's degree
edu.pc2 <- edu.pc %>%
  dplyr::select(NAME, perCapitaFFL, Pop2015, Pop2016,
                pc.18to24, pc.18to24.HS, pc.18to24.BA,
                pc.25to34, pc.25to34.HS, pc.25to34.BA,
                pc.35to44, pc.35to44.HS, pc.35to44.BA,
                pc.45to64, pc.45to64.HS, pc.45to64.BA,
                pc.65plus, pc.65plus.HS, pc.65plus.BA)

rownames(edu.pc2) <- edu.pc2$NAME

edu.perCapita <- edu.pc2 %>%
  select(-c(NAME, Pop2015, Pop2016))

str(edu.perCapita)

# write.csv(edu.perCapita, file = "~/GitHub/federal-firearms-licenses/data/per-capita-education.csv",
#           row.names = T)

# Regression Trees: Education data --------------------------------------------

str(edu.perCapita)

edu.tree <- rpart(perCapitaFFL ~ ., data = edu.perCapita)

rpart.plot(edu.tree, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85, 
           fallen.leaves = T)

print(edu.tree)

# Rural-Urban data preparation ------------------------------------------------

# cleansed dataframe with FFL & Rural-Urban data
rural.urban <- read.csv("~/GitHub/ATF-FFL/data/rural-urban-compact.csv")
str(rural.urban)

rownames(rural.urban) <- rural.urban$NAME

rural.urban.perCapita <- rural.urban %>%
  select(perCapitaFFL, 7:38)

# write.csv(rural.urban.perCapita,
#           file = "~/GitHub/federal-firearms-licenses/data/per-capita-rural-urban.csv",
#           row.names = T)

# Regression Trees: Rural-Urban data  -----------------------------------------

str(rural.urban.perCapita)

rural.urban.tree <- rpart(perCapitaFFL ~ ., data = rural.urban.perCapita)

rpart.plot(rural.urban.tree, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85,
           fallen.leaves = T)

print(rural.urban.tree)

# Legislature data preparation ------------------------------------------------

# load 2014 data
leg.14 <- read.csv("~/GitHub/ATF-FFL/data/02-state-legislatures/control/2014.csv")

# add year variable 
leg.14$Year <- "2014"

# rename for merging
colnames(leg.14)[1] <- "NAME"

# merge ffl data
leg.ffl <- leg.14 %>%
  left_join(ffl)

# clean Nebraska
str(leg.ffl)

leg.ffl$Total.House <- as.numeric(levels(leg.ffl$Total.House))[leg.ffl$Total.House]
leg.ffl[27, c(4, 5, 7, 8, 9, 10)] <- 0
leg.ffl$Total.House <- as.integer(leg.ffl$Total.House)
leg.ffl$Total.House

legislative.perCapita <- leg.ffl %>%
  dplyr::select(1:9, 11, 12, 13, 20)

rownames(legislative.perCapita) <- legislative.perCapita$NAME

legislative.perCapita <- legislative.perCapita %>%
  select(-NAME)

legislative.perCapita$House.Dem <- as.integer(legislative.perCapita$House.Dem)
legislative.perCapita$House.Rep <- as.integer(legislative.perCapita$House.Rep)
legislative.perCapita$Senate.Dem <- as.integer(legislative.perCapita$Senate.Dem)
legislative.perCapita$Senate.Rep <- as.integer(legislative.perCapita$Senate.Rep)

legislative.perCapita$Legis.Control <- gsub("Dem\\*", "Dem", legislative.perCapita$Legis.Control)
legislative.perCapita$State.Control <- gsub("Dem\\*", "Dem", legislative.perCapita$State.Control)
legislative.perCapita$Legis.Control <- factor(legislative.perCapita$Legis.Control)
legislative.perCapita$State.Control <- factor(legislative.perCapita$State.Control)

colnames(legislative.perCapita)[9:11] <- c("Legislative.Control",
                               "Governing.Party",
                               "State.Control")

# write.csv(legislative.perCapita, 
#           file = "~/GitHub/federal-firearms-licenses/data/per-capita-legislative.csv",
#           row.names = T)

# Regression Trees: Legislature data  -----------------------------------------

str(legislative.perCapita)

legislative.tree <- rpart(perCapitaFFL ~ ., data = legislative.perCapita)

rpart.plot(legislative.tree, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85,
           fallen.leaves = T)

print(legislative.tree)

# All data preparation --------------------------------------------------------

all.features <- legislative.perCapita %>%
  left_join(edu.perCapita) %>%
  left_join(income.perCapita) %>%
  left_join(industry.perCapita) %>%
  left_join(rural.urban.perCapita)

# write.csv(all.features, 
#           file = "~/GitHub/federal-firearms-licenses/data/per-capita-all.csv",
#           row.names = T)

# Regression Tree: All Features -----------------------------------------------

str(all.features)

all.tree <- rpart(perCapitaFFL ~ ., data = all.features)

rpart.plot(all.tree, type = 1, extra = 1,
           digits = 4, cex = 0.75, 
           split.family = "GillSans", split.cex = 1.1,
           nn.family = "GillSans", nn.cex = 0.85,
           fallen.leaves = T)

# Visualize Trees and Splits  -------------------------------------------------

# Industry Trees and Splits ---------------------------------------------------
industry.tree.df <- dendro_data(industry.tree, type = "triangle")

industry.perCapita$.rownames <- rownames(industry.perCapita)
summary(industry.perCapita$manufacturingPC)

# Industry splits
ggplot(industry.perCapita, aes(manufacturingPC, agriculturePC, 
                               color = perCapitaFFL, label = .rownames)) +
  geom_point() +
  scale_color_gradient2(low = "deepskyblue",
                       mid = "antiquewhite3",
                       high = "firebrick4", midpoint = 52) +
  geom_segment(x = 5298, xend = 5298, y = 0, yend = 1269,
             linetype = "dashed", color = "firebrick", size = 0.01) +
  geom_segment(x = 0, xend = 9463, y = 2287, yend = 2287, 
               linetype = "dashed", color = "firebrick", size = 0.025) +
  geom_segment(x = 0, xend = 9463, y = 1269, yend = 1269,
             linetype = "dashed", color = "firebrick", size = 0.025) +
  geom_text(aes(manufacturingPC, agriculturePC),
            size = 3, hjust = 1.1, vjust = -0.55, 
            check_overlap = T, family = "GillSans") +
  pd.scatter +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10)) +
  labs(x = "manufacturing per capita", y = "agriculture per capita",
       color = "per capita FFLs")

# Industry Tree
ggplot() + 
  geom_segment(data = industry.tree.df$segments, 
               aes(x = x, y = y, xend = xend, yend = yend),
               linetype = "dotted") + 
  geom_text(data = industry.tree.df$labels, 
            aes(x = x, y = y - 0.015, label = label),
            family = "GillSans") +
  geom_text(data = industry.tree.df$leaf_labels, 
            aes(x = x, y = y - 0.01, label = label,
                family = "GillSans")) +
  pd.theme + 
  theme(axis.text = element_blank(),
        panel.grid = element_blank()) +
  labs(x = "", y = "")

# Income Trees and Splits -----------------------------------------------------

# extract tree data
income.tree.df <- dendro_data(income.tree, type = "triangle")

# Industry Tree
ggplot() + 
  geom_segment(data = income.tree.df$segments, 
               aes(x = x, y = y, xend = xend, yend = yend),
               linetype = "dotted") + 
  geom_text(data = income.tree.df$labels, 
            aes(x = x, y = y - 0.005, label = label),
            family = "GillSans") +
  geom_text(data = income.tree.df$leaf_labels, 
            aes(x = x, y = y - 0.005, label = label,
                family = "GillSans")) +
  pd.theme + 
  theme(axis.text = element_blank(),
        panel.grid = element_blank()) +
  labs(x = "", y = "")

# Income splits
income.perCapita$.rownames <- rownames(income.perCapita)
summary(income.perCapita)

# Income splits plot
ggplot(income.perCapita, aes(perCapita.50000to74999, perCapita.150000.or.more, 
                               color = perCapitaFFL, label = .rownames)) +
  geom_point() +
  scale_color_gradient2(low = "deepskyblue4",
                        mid = "antiquewhite2",
                        high = "firebrick4", midpoint = 52) +
  geom_segment(x = 7291, xend = 7291, y = 0, yend = 7000,
               linetype = "dotted", color = "red3", size = 0.15) +
  geom_segment(x = 0, xend = 8150, y = 5289, yend = 5289, 
               linetype = "dotted", color = "red3", size = 0.15) +
  geom_segment(x = 0, xend = 7291, y = 2732, yend = 2732, 
               linetype = "dotted", color = "red3", size = 0.15) +
  geom_text(aes(perCapita.50000to74999, perCapita.150000.or.more),
            size = 3, hjust = -0.01, vjust = -0.55, 
            check_overlap = T, family = "GillSans") +
  pd.classic +
  theme(legend.position = "bottom",
        legend.title = element_text(size = 10),
        panel.background= element_rect(fill = NA, color = "black")) +
  labs(x = "per capita income: $50k to $74.9k", y = "per capita income: over $150k",
       color = "per capita FFLs")

summary(all.features$House.Dem)

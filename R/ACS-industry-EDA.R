# ATF- FFL
# Feature Selection + Engineering
# Census Electorate Characteristics & American Community Survey data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)

# dataset containing data by state for:
# - population by sex, race/origin
# - industry
# - working class
# - educational attainment
# - financial characteristics

acs <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-all-data.csv", stringsAsFactors = F)

# individual datasets to explore
education <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-education.csv", stringsAsFactors = F)
finance <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-finance.csv", stringsAsFactors = F)
industry <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-industry.csv", stringsAsFactors = F)
working.class <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-working-class.csv", stringsAsFactors = F)

# custom plot themes
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# FFL data --------------------------------------------------------------------
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita.csv")

# create a compact 2016 FFL count
ffl <- ffl %>%
  select(NAME, atf.Region, POPESTIMATE2016, LicCount, LicCountMonthly, perCapFFLyear, perCapitaFFL)

colnames(ffl)[1:3] <- c("NAME", "ATF.Region", "Pop2016")
write.csv(ffl, file = "~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", row.names = F)

# Population data -------------------------------------------------------------
pop <- read.csv("~/GitHub/ATF-FFL/data/nst-est2016-alldata.csv")

# create compact population dataframe
pop <- pop %>%
  select(NAME, 
         POPESTIMATE2010, POPESTIMATE2011, POPESTIMATE2012, 
         POPESTIMATE2013, POPESTIMATE2014, POPESTIMATE2015, POPESTIMATE2016)

# remove first 4 rows (states only)
pop <- pop[-c(1:5, 14, 57), ]
pop$NAME <- factor(pop$NAME)
colnames(pop) <- c("NAME", 
                   "Pop2010", "Pop2011", "Pop2012",
                   "Pop2013", "Pop2014", "Pop2015", "Pop2016")

rownames(pop) <- NULL
write.csv(pop, file = "~/GitHub/ATF-FFL/data/population-compact.csv", row.names = F)

# EDA: Industry ---------------------------------------------------------------

industry <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-industry.csv", stringsAsFactors = F)
str(industry)
summary(industry)

# remove DC and Puerto Rico
industry <- industry[-c(9, 52), ]

summary(industry$ind.01.Civilian.16)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   295300   778100  1983000  3003000  3491000 18050000 

18050000/2
# 9025000

# who has the largest workforce? 
ggplot(industry, 
       aes(reorder(ind.NAME, ind.01.Civilian.16), 
           ind.01.Civilian.16, 
           fill = ind.01.Civilian.16)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient2(low = "firebrick4",
                       mid = "antiquewhite2",
                       high = "cadetblue4", midpoint = 9025000) +
  labs(title = "Total Workforce Population ~ State", 
       x = "", 
       y = "workforce population aged 16+") +
  pd.theme + coord_flip()

# These figures need to be per capita.
# Bind population data, and compute per capita

# Per Capita function
perCap2015 <- function (x) {
  x <- (x / pop$Pop2015) * 100000
  x
}

colnames(industry)[2] <- 'NAME'

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

# Bind FFL data
industryPerCapita <- left_join(industryPerCapita, ffl, by = "NAME")

# Individual Plots: FFLs ~ Industry Category ----------------------------------

# plot workforce per 100k population 
summary(industryPerCapita$workforcePC)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   40110   45200   47400   47430   50200   53630

53630/2
# 26815

ggplot(industryPerCapita, aes(reorder(NAME, workforcePC), workforcePC, 
                     fill = workforcePC)) +
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "antiquewhite1", high = "cadetblue4") +
  scale_y_discrete(limits = c(0, 10000, 20000, 30000, 40000, 50000)) + 
  labs(title = "Total Workforce Population ~ State, per 100k population", x = "", 
       y = "workforce population aged 16+", fill = "") +
  pd.theme + coord_flip()

# agriculture Per Capita vs FFL per capita
ggplot(industryPerCapita, 
       aes(agriculturePC, 
           perCapitaFFL, 
           label = NAME)) +
  geom_point(size = 0.75, shape = 1) + 
  geom_text(size = 3, position = "jitter", 
            alpha = 0.85, hjust = -0.1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  expand_limits(x = c(0, 7000)) +
  labs(title = "FFLs ~ Industry Type: Agricultural, Forestry, Hunting, Mining - per 100k", 
       y = "per capita Federal Firearms Licenses", 
       x = "per capita workforce population", 
       fill = "") +
  pd.classic

# This appears to be significant - 
# an upward trend, positive correlation.

# Construction Per Capita vs FFL per capita
ggplot(industryPerCapita, 
       aes(constructionPC, 
           perCapitaFFL, 
           label = NAME)) +
  geom_point(size = 0.75, shape = 1) + 
  geom_text(size = 3.1, position = "jitter", 
            alpha = 0.85, hjust = -0.1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  labs(title = "FFLs ~ Industry Type: Construction per 100k", 
       y = "per capita FFLs", 
       x = "per capita workforce population", fill = "") +
  pd.classic

# FFLs ~ Public Administration
ggplot(industryPerCapita, aes(publicAdminPC, perCapitaFFL, label = NAME)) +
  geom_point(size = 0.75, shape = 1) + 
  geom_text(size = 3.1, position = "jitter", 
            alpha = 0.75, hjust = -0.1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  labs(title = "FFLs ~ Industry Type: Public Administration per 100k", 
       y = "per capita FFLs", 
       x = "per capita workforce population", fill = "") +
  pd.classic

# FFLs ~ Manufacturing
ggplot(industryPerCapita, aes(manufacturingPC, perCapitaFFL, label = NAME)) +
  geom_point(size = 0.75, shape = 1) + 
  geom_text(size = 3.1, position = "jitter", 
            alpha = 0.75, hjust = -0.1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  labs(title = "FFLs ~ Industry Type: Manufacturing per 100k", 
       y = "per capita FFLs", 
       x = "per capita workforce population", fill = "") +
  pd.classic

# FFLs ~ Finance
ggplot(industryPerCapita, aes(financePC, perCapitaFFL, label = NAME)) +
  geom_point(size = 0.75, shape = 1) + 
  geom_text(size = 3.1, position = "jitter", 
            alpha = 0.85, hjust = -0.1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  labs(title = "FFLs ~ Industry Type: Finance per 100k", 
       y = "per capita FFLs", 
       x = "per capita workforce population", fill = "") +
  pd.classic

# FFLs ~ Scientific
ggplot(industryPerCapita, aes(pro.scientificPC, perCapitaFFL, label = NAME)) +
  geom_point(size = 0.75, shape = 1) + 
  geom_text(size = 3.1, position = "jitter", 
            alpha = 0.85, hjust = -0.1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  labs(title = "FFLs ~ Industry Type: Sciences per 100k", 
       y = "per capita FFLs", 
       x = "per capita workforce population", fill = "") +
  pd.classic

# FACET plot for all variables ------------------------------------------------

# select only per capita observations
industryPerCapita <- industryPerCapita %>%
  select(ind.GEO.id2, NAME, Pop2015, 
         workforcePC, agriculturePC, constructionPC, manufacturingPC,
         wholesalePC, retailPC, transportationPC, informationPC,
         financePC, pro.scientificPC, educationPC, artsPC, otherPC, 
         publicAdminPC, perCapitaFFL)

# create a long dataframe
indPC <- industryPerCapita %>%
  gather(key = Industry, value = Pop2015, 4:17)

colnames(indPC)[6] <- "PerCapIndustry"
indPC$Industry <- gsub("PC", "", indPC$Industry)
indPC$Industry <- gsub("publicAdmin", "Public Administration", indPC$Industry)

# capwords function
source("~/GitHub/ATF-FFL/R/capwords.R")

# clean Industry levels for plotting
levels(as.factor(indPC$Industry))
indPC$Industry <- capwords(indPC$Industry)
indPC$Industry <- gsub("Pro.scientific", "Sciences", indPC$Industry)
indPC$Industry <- gsub("Arts", "Arts, Entertainment, and Accomodation", indPC$Industry)
indPC$Industry <- factor(indPC$Industry)

write.csv(indPC, file = "~/GitHub/ATF-FFL/data/2015-ACS-industryPerCapita.csv", row.names = F)
write.csv(industryPerCapita, file = "~/GitHub/ATF-FFL/data/2015-ACS-industryPerCapita-full.csv",
          row.names = F)

# Facetted Plot for all variables ---------------------------------------------
indPC %>% group_by(Industry) %>%
  filter(Industry != "Workforce") %>%
  ggplot(aes(PerCapIndustry, perCapitaFFL, label = NAME)) +
  geom_point(size = 1, alpha = 0.65) +
  geom_text(size = 2.25, position = "jitter", 
            alpha = 0.75, hjust = 1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  facet_wrap(~ Industry, scales = "free_x", ncol = 3) + pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 7.5),
        axis.title = element_text(size = 12)) +
  labs(title = "Federal Firearms Licenses ~ Workforce Industry Population, per 100k",
       y = "FFLs per capita", x = "workforce population")

library(extrafont)
font_import()
fonttable()
fonts()
loadfonts(device="postscript", quiet = T)

# Facetted Plot for Variables of Interest - Edit 01
indPC %>% group_by(Industry) %>%
  filter(Industry == "Agriculture" | Industry == "Construction" | 
           Industry == "Finance" | Industry == "Manufacturing" |
           Industry == "Sciences" | Industry == "Public Administration" |
           Industry == "Wholesale" | Industry == "Retail" | 
           Industry == "Arts, Entertainment, and Accomodation") %>%
  ggplot(aes(PerCapIndustry, perCapitaFFL, label = NAME)) +
  geom_point(size = 1, alpha = 0.85) +
  geom_text(size = 2.25, position = "jitter", 
            alpha = 0.85, hjust = 1.075, vjust = 1,
            check_overlap = T, family = "Open Sans") +
  facet_wrap(~ Industry, scales = "free_x", nrow = 3) + pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10)) +
  labs(title = "FFLs ~ Workforce Industry Population, per 100k",
       y = "FFLs per capita", x = "workforce population")

# Facetted Plot for Variables of Interest - Edit 02
indPC %>% group_by(Industry) %>%
  filter(Industry == "Agriculture" | Industry == "Construction" | 
           Industry == "Finance" | Industry == "Manufacturing" |
           Industry == "Sciences" | Industry == "Public Administration") %>%
  ggplot(aes(PerCapIndustry, perCapitaFFL, label = NAME)) +
  geom_point(size = 1, alpha = 0.65) +
  geom_text(size = 2.25, position = "jitter", 
            alpha = 0.85, hjust = 1.075, vjust = 1,
            check_overlap = T, family = "Gill Sans") +
  facet_wrap(~ Industry, scales = "free_x", nrow = 3) + pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10)) +
  labs(title = "FFLs ~ Workforce Industry Population, per 100k",
       y = "FFLs per capita", x = "workforce population")

# Facetted Plot for Variables of Interest - Edit 03
indPC %>% group_by(Industry) %>%
  filter(Industry == "Agriculture" | 
           Industry == "Construction" | 
           Industry == "Finance" |
           Industry == "Sciences" | 
           Industry == "Public Administration" |
           Industry == "Wholesale") %>%
  ggplot(aes(PerCapIndustry, perCapitaFFL, label = NAME)) +
  geom_point(size = 1, alpha = 0.65) +
  geom_text(size = 2.25, position = "jitter", 
            alpha = 0.85, hjust = 1.075, vjust = 1,
            check_overlap = T, family = "Gill Sans") +
  facet_wrap(~ Industry, scales = "free_x", nrow = 5) + pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 8),
        axis.title = element_text(size = 10)) +
  labs(title = "FFLs ~ Workforce Industry Population, per 100k",
       y = "FFLs per capita", x = "workforce population")

# Facetted Plot for Variables of Interest - Edit 04
indPC %>% group_by(Industry) %>%
  filter(Industry == "Agriculture" | Industry == "Construction" | 
           Industry == "Finance" | Industry == "Manufacturing" |
           Industry == "Sciences" | Industry == "Workforce") %>%
  ggplot(aes(PerCapIndustry, perCapitaFFL, label = NAME)) +
  geom_point(size = 1, alpha = 0.65) +
  geom_text(size = 2.5, position = "jitter", 
            alpha = 0.85, hjust = 1.075, vjust = 1,
            check_overlap = T, family = "Gill Sans") +
  facet_wrap(~ Industry, scales = "free_x", nrow = 3) + pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10)) +
  labs(title = "FFLs ~ Workforce Industry Population, per 100k",
       y = "FFLs per capita", x = "workforce population")

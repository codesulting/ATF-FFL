# ATF- FFL - ACS Financial Characteristics Data
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
# acs <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-all-data.csv", stringsAsFactors = F)

# ACS Financial Characteristics data, FFL data, Total Population data
finance <- read.csv("~/GitHub/ATF-FFL/data/2015-ACS-finance.csv", stringsAsFactors = F)
ffl <- read.csv("~/GitHub/ATF-FFL/data/ffl-2016-perCapita-compact.csv", stringsAsFactors = F)
pop <- read.csv("~/GitHub/ATF-FFL/data/population-compact.csv", stringsAsFactors = F)

# Per Capita function
perCap2015 <- function (x) {
  x <- (x / pop$Pop2015) * 100000
  x
}

# custom plot themes
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")

# Cleanse and Bind Finance and FFL data ---------------------------------------

str(ffl)
str(pop)
str(finance)

# remove DC and PR
finance <- finance[-c(9, 52), ]

# remove percentages variables
# There are percentage-of-state-population variables, 
# that have been converted to counts instead.
finance <- finance %>%
  select(-c(4:14))

# rename columns for binding
colnames(finance) <- paste0("total", gsub("fin", "", colnames(finance)))
colnames(finance)[2] <- "NAME"

# new dataframe with per capita observations
financePerCapita <- perCap2015(finance[, c(5:15)])
colnames(financePerCapita) <- gsub("total", "perCapita", colnames(financePerCapita))
financePerCapita <- cbind(finance, financePerCapita)

financePerCapita <- left_join(financePerCapita, pop, by = "NAME")
financePerCapita <- left_join(financePerCapita, ffl, by = "NAME")

# write.csv(financePerCapita, file = "~/GitHub/ATF-FFL/data/2015-ACS-financePerCapita-all.csv")

# create long dataframe for facet plots ---------------------------------------

finance.stack <- financePerCapita %>%
  select(-c(1, 5:15, 27:31)) %>% 
  gather(key = Category, value = Pop2015, 2:14)

colnames(finance.stack)[11] <- "PerCapCategory"
finance.stack$Pop2016.y <- NULL

finance.stack$Category <- gsub("perCapita.", "", finance.stack$Category)
finance.stack$Category <- gsub("total.01.OccupiedHousingUnits", "Occupied Housing Units", finance.stack$Category)
finance.stack$Category <- gsub("total.14.MedianHouseholdIncome", "Median Household Income", finance.stack$Category)
finance.stack$Category <- gsub("LessThan5000", "Less than 5000", finance.stack$Category)
finance.stack$Category <- gsub("to", " to ", finance.stack$Category)
finance.stack$Category <- gsub(".or\\.", " or ", finance.stack$Category)

levels(as.factor(finance.stack$Category))
finance.stack$Category <- factor(finance.stack$Category)

# write.csv(finance.stack, file = "~/GitHub/ATF-FFL/data/2015-ACS-financePerCapita-stack.csv")

# Barplot of States ~ Median Household Income ---------------------------------
summary(finance$total.14.MedianHouseholdIncome)
#       Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#     40590   49450   54440   56020   62500   75850

# determine midpoint for fill
# max - min, divided by 2, added to to min
range(finance$total.14.MedianHouseholdIncome)
max(finance$total.14.MedianHouseholdIncome) - min(finance$total.14.MedianHouseholdIncome)
35254/2
17627 + min(finance$total.14.MedianHouseholdIncome)

# or use Median of Median Household Income
ggplot(finance, 
       aes(reorder(NAME, total.14.MedianHouseholdIncome),
           total.14.MedianHouseholdIncome, fill = 
             total.14.MedianHouseholdIncome)) +
  geom_bar(stat = "identity", alpha = 0.85) +
  scale_fill_gradient2(low = "firebrick4",
                       mid = "antiquewhite1",
                       high = "cadetblue", 
                       midpoint = 54440) +
  scale_y_discrete(limits = seq(0, 75000, 25000)) +
  pd.theme + theme(axis.text.x = element_text(angle = 45, 
                                              hjust = 1, 
                                              vjust = 1)) +
  labs(title = "Median Household Income ~ State", 
       y = "Median Household Income", x = "", fill = "")

# Facet Plot all Income Brackets ~ FFLs ---------------------------------------
finance.stack %>% group_by(Category) %>%
  filter(Category != "Occupied Housing Units" & 
           Category != "Median Household Income") %>%
  ggplot(aes(PerCapCategory, perCapitaFFL, label = NAME)) +
  geom_point(size = 1, alpha = 0.65) +
  geom_text(size = 2.25, position = "jitter", 
            alpha = 0.75, hjust = 1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  facet_wrap(~ Category, scales = "free_x") + pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10)) +
  labs(title = "FFLs ~ Financial Category, per 100k",
       x = "income bracket per capita", y = "FFLs per capita")

# Plot Select Income Brackets ~ FFLs ------------------------------------------

levels(finance.stack$Category)

finance.stack %>% group_by(Category) %>%
  filter(Category == "Less than 5000" | 
           Category == "150000 or more" |
           Category == "") %>%
  ggplot(aes(PerCapCategory, perCapitaFFL, label = NAME)) +
  geom_point(size = 1, alpha = 0.65) +
  geom_text(size = 2.25, position = "jitter", 
            alpha = 0.75, hjust = 1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  facet_wrap(~ Category, scales = "free_x") + pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10)) +
  labs(title = "FFLs ~ Financial Category, per 100k",
       x = "population by income bracket (per 100k households)", y = "FFLs per capita")










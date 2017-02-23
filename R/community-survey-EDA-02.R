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
financePerCapita <- perCap2015(finance[, c(3:15)])
colnames(financePerCapita) <- gsub("total", "perCapita", colnames(financePerCapita))
financePerCapita <- cbind(finance, financePerCapita)

financePerCapita <- left_join(financePerCapita, pop, by = "NAME")
financePerCapita <- left_join(financePerCapita, ffl, by = "NAME")

financePerCapita <- financePerCapita %>%
  select(-c(1, 3:15, 29:33))

write.csv(financePerCapita, file = "~/GitHub/ATF-FFL/data/2015-ACS-financePerCapita.csv")

# create long dataframe for facet plots ---------------------------------------

finance.long <- financePerCapita %>%
  gather(key = Category, value = Pop2015, 2:14)

colnames(finance.long)[11] <- "PerCapCategory"

finance.long$Category <- gsub("perCapita.", "", finance.long$Category)
finance.long$Category <- gsub("01.OccupiedHousingUnits", "Occupied Housing Units", finance.long$Category)
finance.long$Category <- gsub("14.MedianHouseholdIncome", "Median Household Income", finance.long$Category)
finance.long$Category <- gsub("LessThan5000", "Less than 5000", finance.long$Category)
finance.long$Category <- gsub("to", " to ", finance.long$Category)
finance.long$Category <- gsub(".or\\.", " or ", finance.long$Category)

levels(as.factor(finance.long$Category))
finance.long$Category <- factor(finance.long$Category)

# Barplot of States ~ Median Household Income
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
  scale_y_discrete(limits = seq(0, 80000, 20000)) +
  pd.theme + coord_flip() +
  labs(title = "Median Household Income ~ State", 
       y = "Median Household Income", x = "", fill = "")

# Plot all Income Brackets ~ FFLs
finance.long %>% group_by(Category) %>%
  filter(Category != "Occupied Housing Units") %>%
  ggplot(aes(PerCapCategory, perCapitaFFL, label = NAME)) +
  geom_point(size = 1, alpha = 0.65) +
  geom_text(size = 2.25, position = "jitter", 
            alpha = 0.75, hjust = 1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  facet_wrap(~ Category, scales = "free_x", nrow = 3) + pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10)) +
  labs(title = "FFLs ~ Financial Category, per 100k",
       x = "income bracket per capita", y = "FFLs per capita")










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

# custom plot themes and maps
source("~/GitHub/ATF-FFL/R/00-pd-themes.R")
source("~/GitHub/ATF-FFL/R/usa-map-prep.R")

# Cleanse and Bind Finance and FFL data ---------------------------------------

str(ffl)
str(pop)
str(finance)

# remove DC and PR
finance <- finance[-c(9, 52), ]

# remove percentages variables
# There are percentage-of-state-population variables, 
# that have been converted to counts instead.
# finance <- finance %>%
#   select(-c(4:14))

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
  geom_bar(stat = "identity", alpha = 0.9) +
  scale_fill_gradient2(low = "darkgoldenrod",
                       mid = "antiquewhite2",
                       high = "cadetblue4", 
                       midpoint = 54440) +
  scale_y_discrete(limits = seq(0, 75000, 25000)) +
  pd.theme + theme(axis.text.x = element_text(angle = 45, size = 8,
                                              hjust = 1, vjust = 1)) +
  labs(title = "Median Household Income ~ State", 
       y = "Median Household Income", x = "", fill = "")

# Map of States ~ Median Household Income -------------------------------------

library(maps)
library(mapproj)

# load map data for US
usa <- map_data("state")
colnames(usa) <- c("lon", "lat", "group", "order", "NAME", "subregion")

# capitalize state.name (function from tolower() documentation)
source("~/GitHub/ATF-FFL/R/capwords.R")
usa$NAME <- capwords(usa$NAME)

# bind geo data to finance data
finance.map <- financePerCapita %>%
  left_join(usa, by = "NAME") %>%
  arrange(group, order)

summary(finance.map$total.14.MedianHouseholdIncome)

# ACS-Finance-Map Median Household Income
ggplot(finance.map, aes(lon, lat, group = group, 
                        fill = total.14.MedianHouseholdIncome)) +
  geom_polygon(color = "gray92", size = 0.075) +
  scale_fill_gradient2(low = "darkgoldenrod",
                       mid = "antiquewhite2",
                       high = "cadetblue4", 
                       midpoint = 54440) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "Median Household Income ~ State", 
       x = "", y = "", fill = "USD")

summary(finance.map$perCapita.150000.or.more)

# ACS-Finance-Map 150k Income bracket
ggplot(finance.map, aes(lon, lat, group = group, 
                        fill = perCapita.150000.or.more)) +
  geom_polygon(color = "gray92", size = 0.075) +
  scale_fill_gradient2(low = "darkgoldenrod",
                       mid = "antiquewhite2",
                       high = "cadetblue4", 
                       midpoint = 3177) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "Household Incomes > $150,000", 
       x = "", y = "", fill = "number of\nhouseholds\nper 100k")

# ACS-Finance-Map LessThan5k Income bracket
quantile(finance.map$total.LessThan5000)
summary(finance.map$perCapita.LessThan5000)

ggplot(finance.map, aes(lon, lat, group = group, 
                        fill = perCapita.LessThan5000)) +
  geom_polygon(color = "gray92", size = 0.075) +
  scale_fill_gradient2(low = "darkgoldenrod",
                       mid = "antiquewhite2",
                       high = "cadetblue4", 
                       midpoint = 1286.0) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "Household Incomes < $5,000", 
       x = "", y = "", fill = "number of\nhouseholds\nper 100k")

# ACS-Finance-Map 35-50k
summary(finance.map$perCapita.35000to49999)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   3411    4637    5283    5105    5602    6224

ggplot(finance.map, aes(lon, lat, group = group, 
                        fill = perCapita.35000to49999)) +
  geom_polygon(color = "gray92", size = 0.075) +
  scale_fill_gradient2(low = "darkgoldenrod",
                       mid = "antiquewhite2",
                       high = "cadetblue4", 
                       midpoint = 5283) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "Household Incomes $35,000-49,999", 
       x = "", y = "", fill = "number of\nhouseholds\nper 100k")

# ACS-Finance-Map upper.median
summary(finance.map$perCapita.50000to74999)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   5424    6212    6820    6798    7275    8150

ggplot(finance.map, aes(lon, lat, group = group, 
                        fill = perCapita.50000to74999)) +
  geom_polygon(color = "gray92", size = 0.075) +
  scale_fill_gradient2(low = "darkgoldenrod",
                       mid = "antiquewhite2",
                       high = "cadetblue4", 
                       midpoint = 6820) +
  coord_map("polyconic") + pd.theme +
  theme(legend.position = "right",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        legend.text = element_text(size = 10, hjust = 1, vjust = 1)) +
  labs(title = "Household Incomes $50,000-74,999", 
       x = "", y = "", fill = "number of\nhouseholds\nper 100k")


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

# It almost appears as if there's a weak, but positive linear trend 
# as the household income approaches the median. 

# Plot Select Income Brackets ~ FFLs ------------------------------------------

levels(finance.stack$Category)

# Single-Column Test
finance.stack %>% group_by(Category) %>%
  filter(Category == "Less than 5000" | 
           Category == "150000 or more" |
           Category == "50000 to 74999") %>%
  ggplot(aes(PerCapCategory, perCapitaFFL, label = NAME)) +
  geom_point(size = 1, alpha = 0.65) +
  geom_text(size = 2.25, position = "jitter", 
            alpha = 0.75, hjust = 1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  facet_wrap(~ Category, scales = "free_x", ncol = 1) + pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 9),
        axis.title = element_text(size = 10)) +
  labs(title = "FFLs ~ Financial Category, per 100k",
       x = "population by income bracket (per 100k households)", y = "FFLs per capita")

# ACS - Finance - Facet - Edit 01
finance.stack %>% group_by(Category) %>%
  filter(Category == "Less than 5000" | 
           Category == "150000 or more" |
           Category == "50000 to 74999" |
           Category == "35000 to 49999") %>%
  ggplot(aes(PerCapCategory, perCapitaFFL, label = NAME)) +
  geom_point(size = 1, alpha = 0.65) +
  geom_text(size = 2.25, position = "jitter", 
            alpha = 0.75, hjust = 1, vjust = 1,
            check_overlap = T, family = "GillSans") +
  facet_wrap(~ Category, scales = "free_x", ncol = 2) + pd.theme +
  theme(strip.background = element_rect(fill = NA, color = "black"),
        panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 9.25),
        axis.title = element_text(size = 11)) +
  labs(title = "FFLs ~ Financial Category, per 100k: Median and Extremes",
       x = "population by income bracket (per 100k households)", y = "FFLs per capita")

# ACS - Finance - Facet - Edit 02
finance.stack %>% group_by(Category) %>%
  filter(Category == "35000 to 49999") %>%
  ggplot(aes(PerCapCategory, perCapitaFFL, label = NAME)) +
  geom_point(size = 1.25, alpha = 0.75) +
  geom_text(size = 3.25, position = "jitter", 
            alpha = 0.75, hjust = 1.05, vjust = 1.1,
            check_overlap = T, family = "GillSans") + 
  pd.theme +
  theme(axis.line = element_line(color = "black"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 11)) +
  labs(title = "FFLs ~ Financial Category, per 100k: $35,000 to $49,000",
       x = "income $35,000 to $49,000 (per 100k households)", y = "FFLs per capita")


finance.stack %>% group_by(Category) %>%
  filter(Category == "35000 to 49999") %>%
  ggplot(aes(PerCapCategory, perCapitaFFL, label = NAME, 
             size = PerCapCategory, fill = perCapitaFFL)) +
  geom_point(position = "jitter") +
  geom_text(size = 2.5, position = "jitter", 
            alpha = 0.75, hjust = -0.1, vjust = -0.1,
            check_overlap = T, family = "GillSans",
            aes(size = PerCapCategory/100), data = finance.stack) +
  scale_size_area(max_size = 40, guide = F) + pd.theme +
  scale_fill_gradient2(low = "deepskyblue4", 
                       mid = "antiquewhite2",
                       high = "firebrick4",
                       midpoint = 1000, guide = F) +
  theme(panel.background = element_rect(fill = NA, color = "black"),
        axis.text = element_text(size = 9.25),
        axis.title = element_text(size = 11)) +
  labs(title = "FFLs ~ Financial Category, per 100k: $35,000 to $49,000",
       x = "population by income bracket (per 100k households)", y = "FFLs per capita")


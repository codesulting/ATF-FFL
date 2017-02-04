# ATF - Federal Firearms Licenses
# Exploratory Data Analysis
# Firearms Commerce Data

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# data: ATF Firearms Commerce History -----------------------------------------

# 2016 ATF Firearms Commerce Report, Exhibit 10
# in the ATF Commerce report, there are numerous tables relating to:
# manufacturing, exports, imports, firearms by ATF form type, 
# registered weapons by state, taxpayers by state, total FFLs, FFL actions, and FFL 
# This data is from 1976-2015.

commerce.FFL.total <- read.csv("~/Documents/ATF-FFL/data/commerce/10-FFL-total.csv",
                               stringsAsFactors = T)
str(commerce.FFL.total)
summary(commerce.FFL.total)

# consider making this a 'long' dataframe.
# or consider making it a wide one:
ffl.history <- as.data.frame(t(commerce.FFL.total))
colnames(ffl.history) <- 1975:2015
summary(ffl.history)

# long dataframe
FFLs <- commerce.FFL.total %>%
  gather(key = LicenseType, value = n, 2:11)

summary(FFLs)
# 'n' variable has commas and is reading as character string

# function to remove commas from numeric variables
commas <- function(x) {
  x <- gsub(",", "", x)
  x <- as.integer(x)
  x
}

FFLs$n <- commas(FFLs$n)
colnames(FFLs)[3] <- "FFL.Rate"

mins <- group_by(FFLs, LicenseType) %>% slice(which.min(FFL.Rate))
maxs <- group_by(FFLs, LicenseType) %>% slice(which.max(FFL.Rate))
ends <- group_by(FFLs, LicenseType) %>% filter(Fiscal.Year == max(Fiscal.Year))
quarts <- FFLs %>% group_by(LicenseType) %>%
  summarize(quart1 = quantile(FFL.Rate, 0.25),
            quart2 = quantile(FFL.Rate, 0.75)) %>%
  right_join(FFLs)

# plot
ggplot(FFLs, aes(Fiscal.Year, FFL.Rate, group = LicenseType)) +
  facet_grid(LicenseType ~ .) +
  geom_ribbon(data = quarts, aes(ymin = quart1, max = quart2), fill = 'grey90') +
  geom_line(size = 0.25) +
  geom_point(data = mins, col = 'red') +
  geom_point(data = maxs, col = 'blue') +
  geom_text(data = mins, aes(label = FFL.Rate), vjust = -1, size = 2.75) +
  geom_text(data = maxs, aes(label = FFL.Rate), vjust = -1, size = 2.75) +
  geom_text(data = ends, aes(label = FFL.Rate), hjust = 0, nudge_x = 1) +
  geom_text(data = ends, aes(label = LicenseType), hjust = 0, nudge_x = 5) +
  expand_limits(x = max(FFLs$Fiscal.Year) + (0.25 * (max(FFLs$Fiscal.Year) - min(FFLs$Fiscal.Year)))) +
  scale_x_continuous(breaks = seq(1975, 2015, 10)) +
  scale_y_continuous(expand = c(0.1, 0)) +
  theme_tufte(base_size = 10, base_family = "GillSans") +
  theme(axis.title = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), strip.text = element_blank(),
        axis.text.x = element_text(angle = 45, size = 11,
                                   hjust = 1, vjust = 1),
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))





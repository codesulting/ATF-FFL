# ATF-FFL
# Population data 2015

library(dplyr)
library(tidyr)

# load data -------------------------------------------------------------------

pop.all <- read.csv("~/GitHub/ATF-FFL/data/01-census/nst-est2016-alldata.csv")
str(pop.all)


# subset 2015 population data -------------------------------------------------
pop.2015 <- pop.all %>%
  select(REGION, DIVISION, NAME, contains("2015"))

# remove US & regional totals; DC and Puerto Rico
pop.2015 <- pop.2015[-c(1:5, 14, 57), ]

write.csv(pop.2015, file = "~/GitHub/ATF-FFL/data/per-capita-clean/pop-2015.csv",
          row.names = F)
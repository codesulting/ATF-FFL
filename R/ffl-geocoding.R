# ATF - Federal Firearms Licenses
# Exploratory Data Analysis
# Firearms Commerce Data

# load data -------------------------------------------------------------------

library(data.table)

# data: Federal Firearms Licenses 2016 ----------------------------------------
f16 <- fread("~/Documents/ATF-FFL/data/ffl-2016-V3.csv", stringsAsFactors = F)
f16 <- as.data.frame(f16)
str(f16)

length(unique(f16$LicenseName))
# 75038

# subset unique license names for geocoding
f16 <- f16[!duplicated(f16$LicenseName), ]
write.csv(f16, file = "~/Documents/ATF-FFL/data/f16-geo.csv", row.names = F)

# using the Google Maps API limits to 2500 addresses per day, 
# geocodio charges less than Google for each address over 2500.

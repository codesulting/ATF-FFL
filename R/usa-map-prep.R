# ATF-FFL - United States Map Prep

library(maps)
library(mapproj)
library(maptools)
library(sp)
library(rgdal)
library(tigris)
library(fiftystater)
library(rgeos)
library(dplyr)
library(ggplot2)

# Function: Capwords ----------------------------------------------------------

# from tolower() documentation
capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# USA Map preparation ---------------------------------------------------------

# load map data for US
usa <- map_data("state")

# match variable names in FFL data to merge with US map
colnames(usa) <- c("lon", "lat", "group", "order", "NAME", "subregion")

# capitalize state names for binding
usa$NAME <- capwords(usa$NAME)

# test map out
ggplot(usa, aes(lon, lat, group = group)) +
  geom_path() + coord_map("polyconic") 

# Alaska and Hawaii -----------------------------------------------------------

# load fifty states data
data("fifty_states")
fifty_states

# rename columns for binding later
colnames(fifty_states) <- c("lon", "lat", "order", "hole", "piece", "NAME", "group")
fifty_states$NAME <- capwords(fifty_states$NAME)

# test map out
ggplot(fifty_states, aes(lon, lat, group = group)) +
  geom_path() + coord_map("polyconic")

# USA by county  --------------------------------------------------------------

us.county <- map_data("county")

colnames(us.county)[5:6] <- c("NAME", "County")
us.county$NAME <- capwords(us.county$NAME)
us.county$County <- capwords(us.county$County)

ggplot(us.county, aes(long, lat, group = group)) +
  geom_path() + coord_map("polyconic")

# USA by county shp -----------------------------------------------------------

# Download county shape file.
us.map <- tigris::counties(cb = TRUE, year = 2015)

# Remove Alaska(2), Hawaii(15), Puerto Rico (72), Guam (66), Virgin Islands (78), American Samoa (60)
#  Mariana Islands (69), Micronesia (64), Marshall Islands (68), Palau (70), Minor Islands (74)
us.map <- us.map[!us.map$STATEFP %in% c("02", "15", "72", "66", "78", "60", "69",
                                        "64", "68", "70", "74"),]
# Make sure other outling islands are removed.
us.map <- us.map[!us.map$STATEFP %in% c("81", "84", "86", "87", "89", "71", "76",
                                        "95", "79"),]
# Produce map
county_map <- fortify(us.map, region = "NAME")
colnames(county_map)[6] <- "COUNTYNAME"

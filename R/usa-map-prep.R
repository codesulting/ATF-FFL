# ATF-FFL - United States Map Prep

library(maps)
library(mapproj)
library(fiftystater)
library(rgeos)
library(rgdal)
library(maptools)
library(dplyr)

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







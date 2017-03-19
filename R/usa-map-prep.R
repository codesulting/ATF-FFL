# ATF-FFL - United States Map Prep

library(maps)
library(mapproj)
library(maptools)
library(sp)
library(fiftystater)
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

# Alaska and Hawaii Maps-----------------------------------------------------------

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

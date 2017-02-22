# ATF - Federal Firearms Licenses
# Exploratory Data Analysis
# Single State Mapping

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(data.table)

library(maps)
library(mapproj)
library(maptools)
library(sp)
library(rgdal)

# address geocoded 2016 FFL data
# from 2016 FFL dataframe, subsetting to unique LicenseName values
geo <- fread("~/Documents/ATF-FFL/data/f16-geocoded.csv")
geo <- as.data.frame(geo)
str(geo)

# rename duplicate column name
colnames(geo)[4] <- "County.FFL"

# To satisfy a basic curiousity, will be mapping a few individual states 
# That appear as outliers because of population or FFLs per 100k

# wrangle: Wyoming ------------------------------------------------------------

# filter for Wyoming
wy.geo <- geo %>%
  filter(PremiseState == "WY") %>%
  select(PremiseState, Latitude, Longitude, Number, Street, City, State, Zip, 
         County, EstPop2016, LicCount, Type)

wy.geo$County <- factor(wy.geo$County)
summary(wy.geo)

# Wyoming shapefile
wy.shp <- readShapeLines("~/Documents/ATF-FFL/data-spatial/WY/wyoming_administrative.shp")
wy.admin <- fortify(wy.shp)
summary(wy.shp)
summary(wy.admin)

ggplot(wy.admin, aes(long, lat, group = group)) + geom_path() +
  theme_minimal(base_size = 11, base_family = "GillSans") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank())

# assign EPSG 3857 to shapefiles
wy.3857 <- wy.shp
proj4string(wy.3857) <- CRS("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

wy.natural <- readShapePoly("~/Documents/ATF-FFL/data-spatial/WY/wyoming_natural.shp")
wy.nat <- fortify(wy.natural)
summary(wy.natural)

wy.water <- readShapePoly("~/Documents/ATF-FFL/data-spatial/WY/wyoming_water.shp")
summary(wy.water)

# Map: Wyoming ----------------------------------------------------------------

# map of Wyoming with FFL locations - shapefile
ggplot(wy.admin, aes(long, lat, group = group)) + 
  geom_path(size = 0.35, linetype = "dotted", alpha = 0.5) +
  
  geom_polygon(aes(long, lat, group = group), data = wy.natural,
            size = 0.5, color = "bisque4") +
  
  geom_polygon(aes(long, lat, group = group), data = wy.water,
            size = 1, color = "cadetblue2", alpha = 0.25) +
  
  geom_point(aes(Longitude, Latitude), size = 4, alpha = 0.35, 
             color = "firebrick3", data = wy.geo, group = NA) +

  theme_void(base_size = 11, base_family = "GillSans") + 
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "Wyoming - 2016 Federal Firearms Locations") +
  coord_map("polyconic")

# map of Wyoming with FFL locations - ggmap chloropleth tutorial
# http://rforpublichealth.blogspot.com/2015/10/mapping-with-ggplot-create-nice.html
  
# Wrangle: Texas --------------------------------------------------------------

tx.geo <- geo %>%
  filter(PremiseState == "TX") %>%
  select(PremiseState, Latitude, Longitude, Number, Street, City, State, Zip, 
         County, EstPop2016, LicCount, Type)

tx.geo$County <- factor(tx.geo$County)

# read in shapefiles; convert to dataframe
tx.shp <- readShapeLines("~/Documents/ATF-FFL/data-spatial/TX/texas_administrative.shp")
tx.coast <- readShapeLines("~/Documents/ATF-FFL/data-spatial/TX/texas_coastline.shp")
tx.natural <- readShapeLines("~/Documents/ATF-FFL/data-spatial/TX/texas_natural.shp")
tx.water <- readShapeLines("~/Documents/ATF-FFL/data-spatial/TX/texas_water.shp")

tx.admin <- fortify(tx.shp)
tx.coastal <- fortify(tx.coast)
tx.nat <- fortify(tx.nat)
tx.h20 <- fortify(tx.water)

# check projection
summary(tx.shp)
summary(tx.admin)

# test plot
ggplot(tx.admin, aes(long, lat, group = group)) + geom_path() +
  theme_minimal(base_size = 11, base_family = "GillSans") +
  theme(panel.background = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank()) +
  coord_map("polyconic")

# remove blank observations
tx.geo[3840, ]
tx.geo <- tx.geo[-3840, ]

# Map: Texas ------------------------------------------------------------------

# geocoded FFL addresses; manually choropleth
# consider shape on points
# consider geom_hex() for geocoded coordinates
ggplot(tx.coast, aes(long, lat, group = group)) + 
  geom_path(size = 0.75, linetype = "dashed", color = "cadetblue3") +
  geom_polygon(aes(long, lat, group = group), data = tx.natural,
               size = 0.75, color = "bisque3", fill = "bisque2") +
  geom_polygon(aes(long, lat, group = group), data = tx.water,
               size = 0.75, color = "cadetblue3", fill = "cadetblue2") +
  geom_point(aes(Longitude, Latitude, group = NA), data = tx.geo,
             size = 2, color = "mistyrose2", alpha = 0.3, shape = 19) +
  geom_point(aes(Longitude, Latitude, group = NA), data = tx.geo,
             size = 2.2, color = "mistyrose4", alpha = 0.4, shape = 3) +
  theme_void(base_size = 12, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  coord_map("polyconic")



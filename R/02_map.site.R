# 02_siteMap.R ####
## create site map for intertidal points

library(tidyverse)
library(rgdal)  #  Geospatial Data Abstraction Library functions
library(geojsonio)  # deal with json file
library(sp)  # deal with spatial data
library(leaflet)
library(mapview)

cbPalette <- c( ### colourblind-friendly chart colour palette
  # comments reflect Red/Green/Blue equivalents
  "#0072B2", #000, 114, 178
  "#e79f00", #231, 159, 0
  "#009E73", #000, 158, 115
  "#9ad0f3", #154, 208, 243
  "#000000", #0, 0, 0
  "#D55E00", #213, 94, 0
  "#CC79A7", #204, 121, 167
  "#F0E442"  #240, 228, 66
)

# load data and convert to lat/long ####
df0 <- as_tibble(openxlsx::read.xlsx("data/in/2022IntertidalPoints.xlsx",sheet = "mean"))

## convert Eastings & Northings to LatLong ####
df <- df0 %>%
  dplyr::transmute(  # create new columns and drop all the others
    Eastings = as.numeric(as.character(Eastings)), # make this text column numeric
    Northings = as.numeric(as.character(Northings))
  ) %>% 
  dplyr::rename(longitude = Eastings, latitude = Northings)  # rename

### append names
tmp <- df0[,c(1,4)]

### merge together into geospatial object ####
spdf <- sp::SpatialPointsDataFrame(  # create a SPDF
  coords = df,  # site co-ordinates
  data = tmp,  # the site names
  proj4string = CRS("+init=epsg:27700")  # BNG projection system
) %>% 
  sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system

dplyr::glimpse(spdf)

#rm(df,tmp) ### tidy up

# create map ####
## Underlying map ####
# (map <- leaflet::leaflet() %>%
#   leaflet::addProviderTiles(providers$OpenStreetMap))

spdf

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  # addMarkers(lng=coordinates(spdf)[,1],
  #            lat=coordinates(spdf)[,2],
  #            popup=tmp$Transect)
  addCircleMarkers(lng=coordinates(spdf)[,1],
                   lat=coordinates(spdf)[,2],
                   color = spdf$zone
                   )
m  # Print the map

# alternative map using mapview ####
mapview(spdf, label=spdf$Transect)

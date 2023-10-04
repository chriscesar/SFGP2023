# 02_siteMap.R ####
## create site map for intertidal points

# library(tidyverse)
# library(rgdal)  #  Geospatial Data Abstraction Library functions
# library(geojsonio)  # deal with json file
# library(sp)  # deal with spatial data
# library(leaflet)
# library(mapview)
# library(maps)

### ggplot section ####
ld_pkgs <- c("tidyverse","ggplot2","sf","rgdal","maps")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

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

### GIS folder
gisfol <- "//prodds.ntnl/Shared/AN/KFH/Groups/N_Marine/02 Projects_Tasks/05 Nat Ops_FCRM/NEAS/10NEAS Lincs shore/GIS/"

# load data and convert to lat/long ####
df0 <- as_tibble(openxlsx::read.xlsx("data/in/2022IntertidalPoints.xlsx",sheet = "mean"))

# ## convert Eastings & Northings to LatLong ####
# df <- df0 %>%
#   dplyr::transmute(  # create new columns and drop all the others
#     Eastings = as.numeric(as.character(Eastings)), # make this text column numeric
#     Northings = as.numeric(as.character(Northings))
#   ) %>% 
#   dplyr::rename(longitude = Eastings, latitude = Northings)  # rename
# 
# ### append names
# tmp <- df0[,c(1,4)]
# 
# ### merge together into geospatial object ####
# spdf <- sp::SpatialPointsDataFrame(  # create a SPDF
#   coords = df,  # site co-ordinates
#   data = tmp,  # the site names
#   proj4string = CRS("+init=epsg:27700")  # BNG projection system
# ) %>% 
#   sp::spTransform(CRS("+init=epsg:4326")) # reproject to latlong system
# 
# dplyr::glimpse(spdf)
# 
# #rm(df,tmp) ### tidy up
# 
# # create map ####
# ## Underlying map ####
# # (map <- leaflet::leaflet() %>%
# #   leaflet::addProviderTiles(providers$OpenStreetMap))
# 
# spdf
# 
# m <- leaflet() %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   # addMarkers(lng=coordinates(spdf)[,1],
#   #            lat=coordinates(spdf)[,2],
#   #            popup=tmp$Transect)
#   addCircleMarkers(lng=coordinates(spdf)[,1],
#                    lat=coordinates(spdf)[,2],
#                    # color = ~spdf$zone,
#                    popup=tmp$Transect
#                    )
# m  # Print the map
# 
# # alternative map using mapview ####
# mapview(spdf, label=spdf$Transect)

# GGPLOT STATIC MAPS ######
#### attempt at ggplot2 version
# worldmap <- map_data("world")
# ggplot()+
#   geom_polygon(data=worldmap,
#                aes(x = long,
#                    y = lat,
#                    group = group
#                    ),
#                fill = 'gray90',
#                color = 'black') +
#   coord_fixed(xlim = c(-10,3), 
#               ylim = c(50.3, 59))+
#   geom_point()

## based on:
# https://joshuamccrain.com/tutorials/ggplot_maps/maps_tutorial.html
base_0 <- st_read(paste0(gisfol,"shapes/Coast_polygon.shp"))
# base_0 <- readOGR(paste0(gisfol,"shapes/Coast_polygon.shp")) ### UK polygon
basedf <- fortify(base_0)
ggplot()+
  geom_polygon(data=basedf, aes(x=long, y=lat, group=group),
                      fill="white", colour="black")+
  # coord_cartesian(ylim(355000,389072))+
  # coord_cartesian(xlim(538691,570636),  ylim(349811,389072))+
  xlim(538691,570636)+  ylim(355000,389072)+
  geom_point(data=df0, aes(x=Eastings, y=Northings, fill=zone),
             colour="#000000",pch=21,size=5,
             inherit.aes = FALSE)+
  theme_void()#+


subset_df <- basedf[basedf$lat >= 355000 & basedf$lat <= 389072
                    & basedf$long >= 538691 & basedf$long <= 570636, ]

ggplot(data = subset_df, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill = "white", colour = "black") +
  # coord_cartesian(ylim = c(355000, 389072))+
  geom_point(data=df0, aes(x=Eastings, y=Northings, fill=zone),
             colour="#000000",pch=21,size=5,
             inherit.aes = FALSE)
  
  
  ggplot()+geom_polygon(data=basedf, aes(x=long, y=lat, group=group),
                        fill="white", colour="black")+
    coord_cartesian(ylim(355000,389072))
  
  

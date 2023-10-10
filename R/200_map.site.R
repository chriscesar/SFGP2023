# 02_siteMap.R ####
## create site map for intertidal points

### load packages
ld_pkgs <- c("tidyverse","ggplot2","sf","rgdal","maps",
             "ggpubr", "ggspatial","ggrepel")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

cbPalette <- c( ### colourblind-friendly chart colour palette
  # comments reflect Red/Green/Blue equivalents
  "#0072B2", #000, 114, 178
  "#e79f00", #231, 159, 0
  "#009E73", #000, 158, 115
  "#9ad0f3", #154, 208, 243
  "#CC79A7", #204, 121, 167
  "#000000", #0, 0, 0
  "#D55E00", #213, 94, 0
  "#F0E442"  #240, 228, 66
)

### GIS folder
gisfol <- "//prodds.ntnl/Shared/AN/KFH/Groups/N_Marine/02 Projects_Tasks/05 Nat Ops_FCRM/NEAS/10NEAS Lincs shore/GIS/"

# load data and convert to lat/long ####
df0 <- as_tibble(openxlsx::read.xlsx("data/in/2022IntertidalPoints.xlsx",
                                     sheet = "mean"))
df0$zone <- factor(df0$zone,
                   levels=c("Above","Inside","Inside2","Below","Wash"))

# GGPLOT STATIC MAPS ######
#### attempt at ggplot2 version
base_0 <- st_read(paste0(gisfol,"shapes/Coast_polygon.shp"))
basedf <- fortify(base_0)
towns_pt_0 <- st_read(paste0(gisfol,"shapes/","Urban_pt_TwnCty.shp"))
towns_pt_df <- as_tibble(fortify(towns_pt_0))
towns_pt_df$East <- st_coordinates(towns_pt_0)[,"X"]
towns_pt_df$North <- st_coordinates(towns_pt_0)[,"Y"]
towns_area <- st_read(paste0(gisfol,"shapes/","Urban_areas_250k.shp"))

ggplot()+
  geom_sf(data = base_0, fill = "darkolivegreen3") +
  geom_sf(data = towns_area[towns_area$DESCRIPTIO == "Large Urban Area polygon",],
          fill="darkgrey")+
  geom_point(data = df0,
             aes(x = Eastings,
                 y = Northings,
                 fill = zone),
             colour = 1, pch = 21, size = 4, 
             inherit.aes = FALSE,
             show.legend = FALSE)+
  # geom_text(data = df0,
  #           hjust=-0.275,
  #           vjust=0.2,
  #           aes(x=Eastings,
  #               y=Northings,
  #               label = Transect),
  #           fontface="bold",
  #           inherit.aes = FALSE)+
  geom_text_repel(data = df0,
                 segment.colour="grey",
                  nudge_x = 0.1,
                  point.padding = 0.5,
                 aes(x = Eastings,
                     y = Northings,
                     label = Transect), 
                 fontface = "bold",
                 force = 0.5,
                 inherit.aes = FALSE,
                 seed = pi,
                 direction = "x") +
  geom_text(data = towns_pt_df,
            aes(x = East, y = North, label = NAME), 
            inherit.aes = FALSE,
            hjust = 1,
            vjust = 1, 
            fontface = "bold")+
  coord_sf(xlim = c(534750, 563000),
           ylim = c(344500, 389070))+
  scale_fill_manual(values = cbPalette)+
  labs(title = "Location of intertidal transects surveyed as part of the Saltfleet to Gibraltar Point Strategy 2023")+
  ggthemes::theme_few()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())+
  ggspatial::annotation_scale(location="br", width_hint=0.5)+
  ggspatial::annotation_north_arrow(which_north = "grid", location = "bl",
                                    pad_x = unit(0.0, "cm"),
                                    pad_y = unit(0.5, "cm"),
                                    height = unit(2, "cm"),
                                    width = unit(2, "cm"),
                                    style = north_arrow_fancy_orienteering
                                    )

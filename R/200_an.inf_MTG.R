# 200_an.inf_MTG.R ####
### Multivariate analysis of 2022 Major Taxonomic Group data

source("R/100_imp.inf_MTG.R")
dfw$zone1 <- factor(dfw$zone1,levels=c("Above","Inside","Inside2","Below","Wash"))
### load packages
ld_pkgs <- c("tidyverse","ggplot2")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### set metadata
source("R/00_meta_setMeta.R")

cbPalette3 <- c(
  "#0072B2",
           "#e79f00",
           "#009E73",
           "#9ad0f3",
           "#A0A0A0" ,
           "#D55E00" ,
           "#CC79A7" ,
           "#004738",
           "#F0E442",
           
           "#003582",
           "#744500",
           "#DEAAC6",
           "#FFFFFF",
           "#556572",
           "#A0A0A0" ,
           "#683000" ,
           "#664080" ,
           "#701063",
           "#807821"
)

# rejig data ####
### keep only 1 mm data. Remove Wash
df.cur <- dfw[dfw$year==cur.yr,]

dfw %>% 
  filter(.,mesh=="1.0mm") %>% 
  filter(.,zone1 != "Wash") %>% 
  pivot_longer(cols =`AFAUNAL`:`Mollusc_Chiton`,
               names_to = "MTG",values_to = "abundance") %>% 
  filter(.,abundance !=0) -> dfl

# dfl$MTG2 <- ifelse(dfl$MTG == "Arthropod_Barnacle", "Arthropod_Other",
#                    ifelse(dfl$MTG == "Arthropod_Copepod", "Arthropod_Other",
#                           ifelse(dfl$MTG == "Arthropod_IsopodMysid", "Arthropod_Other",
#                                  ifelse(dfl$MTG == "Arthropod_Pycnogonid", "Arthropod_Other",
#                                         ifelse(dfl$MTG == "Arthropod_Ostracod", "Arthropod_Other",
#                                         ifelse(dfl$MTG == "Ascidian", "Sessile_Colonial",
#                                                ifelse(dfl$MTG == "Bryozoan", "Sessile_Colonial",
#                                                       ifelse(dfl$MTG == "Hydrozoan", "Sessile_Colonial",
#                                                              ifelse(dfl$MTG == "Porifera", "Sessile_Colonial",dfl$MTG
#                                                              )))))))))

dfl$MTG2 <- ifelse(dfl$MTG == "Arthropod_Barnacle", "Arthropod_Sessile",
                   ifelse(dfl$MTG == "Arthropod_Copepod", "Arthropod_Pelagic",
                          ifelse(dfl$MTG == "Arthropod_IsopodMysid", "Arthropod_Epifauna",
                                 ifelse(dfl$MTG == "Arthropod_Pycnogonid", "Arthropod_Epifauna",
                                        ifelse(dfl$MTG == "Arthropod_Ostracod", "Arthropod_Pelagic",
                                               ifelse(dfl$MTG == "Ascidian", "Sessile_Colonial",
                                                      ifelse(dfl$MTG == "Bryozoan", "Sessile_Colonial",
                                                             ifelse(dfl$MTG == "Hydrozoan", "Sessile_Colonial",
                                                                    ifelse(dfl$MTG == "Porifera", "Sessile_Colonial",dfl$MTG
                                                                    )))))))))


## reorder groups for plotting
# dfl$MTG2 <- factor(dfl$MTG2, levels=c(
#   "Annelid_Oligochaete",
#   "Annelid_Polychaete",
#   "Arthropod_Amphipod",
#   "Arthropod_Shrimp_Crab",
#   "Mollusc_Bivalve",
#   "Mollusc_Gastropod",
#   "Arthropod_Other",
#   "Sessile_Colonial",
#   "Nematode",
#   "Nemertean",
#   "Flatworm",
#   "AFAUNAL"
#   ))

dfl$MTG2 <- factor(dfl$MTG2, levels=c(
  "Annelid_Oligochaete",
  "Annelid_Polychaete",
  "Arthropod_Amphipod",
  "Arthropod_Epifauna",
  "Arthropod_Shrimp_Crab",
  "Arthropod_Pelagic",
  "Arthropod_Sessile",
  "Mollusc_Bivalve",
  "Mollusc_Gastropod",
  "Sessile_Colonial",
  "Nematode",
  "Nemertean",
  "Flatworm",
  "AFAUNAL"
))

png(
  file = "output/figs/inf.MTG.ts.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
)

dfl %>% 
  dplyr::select(.,-c(MTG)) %>% 
  group_by(across(c(!abundance))) %>% 
  summarise(abundance=sum(abundance),.groups = "drop") %>% 
  filter(.,MTG2 != "Arthropod_Pelagic") %>% 
  filter(.,MTG2 != "AFAUNAL") %>% 
  ggplot(.,aes(x=as.integer(year),abundance, fill=MTG2)) + 
  geom_bar(colour=1,position = "fill",stat = "identity")+
  facet_wrap(.~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of major taxonomic groups over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       Pelagic-associated taxa (including ostracods and copepods) removed prior to plotting.  Afaunal samples excluded.
       'Sessile_Colonial' includes sponges, hydroids and bryozoans.
       No survey was conducted in 2010.")+
  ylab("Relative prevalence")+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

# tidy up ####
rm(list = ls(pattern = "^cb"))
rm(list = ls(pattern = "^df"))
rm(cur.yr, fol, gisfol, perm, ppi)

detach(package:ggplot2, unload=TRUE)
detach(package:tidyverse, unload=TRUE)

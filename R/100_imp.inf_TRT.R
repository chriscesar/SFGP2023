# 100_imp.inf_TRT.R ####
## Import trait data for consideration of current distribution
## and longer-term trends

# Set up ####
### load packages ####
ld_pkgs <- c("tidyverse","readxl")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### load metadata ####
source("R/00_meta_setMeta.R")

##amend cols
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

# copy & load data ####

file_name <- "inf_ts_longRAW_USE.xlsx"

# Set the source and destination file paths
source_file <- paste0(fol,file_name)
destination_file <- paste0("data/in/",file_name)

# Check if the file exists in the data folder
if (!file.exists(destination_file)) {
  # If not, copy the file from the source folder to the data folder
  file.copy(source_file, destination_file)
  cat("File copied successfully.\n")
} else {
  # If the file already exists in the data folder, do nothing
  cat("File already exists in the data folder.\n")
}

# import data ####
df0 <- as_tibble(read_xlsx(destination_file,
                           sheet = "traitsOutUSE"))

# drop nuicance/non-marine taxa ####
df <- df0 %>%
  filter(., units != "flag_fragments") %>% # remove taxa flagged as fragments
  filter(., units != "flag_plants") %>% # remove taxa flagged as plants
  filter(., is.na(Order) | Order != "Diptera") %>% ###drop flies
  filter(., is.na(Order) | Order != "Hemiptera") %>% ###drop bugsLepidoptera
  filter(., is.na(Order) | Order != "Lepidoptera") %>% ###drop butterflies/moths
  filter(., is.na(Order) | Order != "Hymenoptera") %>% ###drop ants/bees/wasps
  filter(., taxonUSE != "Animalia") %>%  ###drop taxa flagged only as Animalia
  dplyr::select(.,-c(zone2.1:zone2.2,yr.trn:taxonReported, taxonUSE:TaxUSETrt)) %>% 
  mutate(abundance = 1)

dflall <- df %>% ## sum prevalence of traits across all taxa in all samples
  pivot_longer(.,cols = sr_Less_than_10:b_None,
               names_to = "trait_cat",
               values_to = "affiliation"
               ) %>% 
  mutate(.,affiliation = abundance*affiliation) %>% 
  dplyr::select(.,!c(abundance)) %>% 
  group_by(across(c(!affiliation))) %>%
  summarise(.,affiliation=sum(affiliation,na.rm=TRUE),.groups="drop")

## add trait and category column names
# Splitting the strings based on "_"
split_values <- strsplit(dflall$trait_cat, "_")

# Extracting the values before and after the first "_"
dflall$trait <- sapply(split_values, function(x) x[1])
dflall$category <- sapply(split_values, function(x) paste(x[-1], collapse = "_"))

# rename traits ####
dflall$trait <- 
  ifelse(dflall$trait == "b","Bioturbation",
                    ifelse(dflall$trait == "ed","EggDevelopment",
                           ifelse(dflall$trait == "f","FeedingMode",
                                  ifelse(dflall$trait == "l","Lifespan_years",
                                         ifelse(dflall$trait == "ld","LarvalDevelopment",
                                                ifelse(dflall$trait == "lh","LivingHabit",
                                                       ifelse(dflall$trait == "m","Morphology",
                                                              ifelse(dflall$trait == "mob","Mobility",
                                                                     ifelse(dflall$trait == "sp","SedimentPosition",
                                                                            ifelse(dflall$trait == "sr","MaxSize",
                                                                                   NA
                                                                            ))))))))))
## assign and order factors
#zone1
dflall$zone1 <- factor(dflall$zone1, levels = c("Above","Inside","Inside2","Below","Wash"))
#shore
dflall$shore <- factor(dflall$shore, levels=c("Mid","Low"))
##traits
dflall$category <- factor(dflall$category,
                          levels=c("None","Surface_deposition","Diffusive_mixing",
                                   "Downward_conveyer","Upward_conveyor","Asexual",
                                   "Sexual_brooded","Sexual_benthic",
                                   "Sexual_pelagic","Scavenger","Predator",
                                   "Surface_deposit","Subsurface_deposit",
                                   "Suspension","Parasite","Less_than_1","1_to_3",
                                   "3_to_10",
                                   "More_than_10","Pelagic_lecithotrophic",
                                   "Pelagic_planktotrophic","Benthic_direct",
                                   "Free_living","Burrow_dwelling",
                                   "Tube_dwelling","Crevice_hole_under_stones",
                                   "Epi_endo_biotic","Attached_to_substratum",
                                   "Soft","Cushion","Tunic","Stalked","Crustose",
                                   "Exoskeleton","Swim","Burrower",
                                   "Crawl_creep_climb","Sessile","Surface",
                                   "Shallow_infauna_0_to_5cm",
                                   "Mid_depth_infauna_5_to_10cm",
                                   "Deep_infauna_more_than_10cm",
                                   "Less_than_10","11_to_20","21_to_100",
                                   "101_to_200","201_to_500","More_than_500"))

##mean trait prevalence by across replicates
dfl_reps <- dflall %>% 
  dplyr::select(.,!c(rep,#remove replicates
                     units)) %>% #remove flags
  group_by(across(c(!affiliation))) %>%
  summarise(.,affiliation=mean(affiliation, #calc mean prevalence across reps
                               na.rm = TRUE),
            .groups = "drop")

##mean trait prevalence by across zones*shores
dfl_shzone <- dflall %>% 
  dplyr::select(.,!c(rep,#remove replicates
                     units,#remove flags
                     transect #remove transects
                     )) %>% 
  group_by(across(c(!affiliation))) %>%
  summarise(.,affiliation=mean(affiliation, #calc mean prevalence across reps
                               na.rm = TRUE),
            .groups = "drop")

# plot traits over time by zone ####
## Bioturbation ####
png(
  file = "output/figs/inf.Trt.ts.Bioturb.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
)
dfl_shzone %>% 
  filter(., mesh != "0.5mm") %>%
  filter(., zone1 != "Wash") %>% 
  filter(.,trait=="Bioturbation") %>% 
  rename(.,Bioturbation=category) %>% 
  droplevels(.) %>% 
  ggplot(., aes(x = as.integer(year), y=affiliation, fill=Bioturbation))+
  geom_bar( colour=1,position="fill",stat="identity")+
  facet_grid(shore~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of Bioturbation traits over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones and shore levels
       Prevalence values indicate the number of taxa recorded in each Shore-Zone which have an affinity for a given trait.
       These are based on presence-only occurences and do not incorporate measures of taxon abundance.
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       No survey was conducted in 2010.")+
  ylab("Proportion of infaunal taxa")+
  theme(axis.title.x = element_blank(),
        # legend.title = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

## EggDevelopment ####
png(
  file = "output/figs/inf.Trt.ts.EggDevt.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
  )
dfl_shzone %>% 
  filter(., mesh != "0.5mm") %>%
  filter(., zone1 != "Wash") %>% 
  filter(.,trait=="EggDevelopment") %>% 
  rename(.,Egg_Development=category) %>% 
  droplevels(.) %>% 
  ggplot(., aes(x = as.integer(year), y=affiliation, fill=Egg_Development))+
  geom_bar( colour=1,position="fill",stat="identity")+
  facet_grid(shore~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of Egg Development traits over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones and shore levels
       Prevalence values indicate the number of taxa recorded in each Shore-Zone which have an affinity for a given trait.
       These are based on presence-only occurences and do not incorporate measures of taxon abundance.
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       No survey was conducted in 2010.")+
  ylab("Proportion of infaunal taxa")+
  theme(axis.title.x = element_blank(),
        # legend.title = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

## FeedingMode ####
png(
  file = "output/figs/inf.Trt.ts.FeedMode.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
  )
dfl_shzone %>% 
  filter(., mesh != "0.5mm") %>%
  filter(., zone1 != "Wash") %>% 
  rename(.,Feeding_Mode=category) %>% 
  filter(.,trait=="FeedingMode") %>% 
  droplevels(.) %>% 
  ggplot(., aes(x = as.integer(year), y=affiliation, fill=Feeding_Mode))+
  geom_bar( colour=1,position="fill",stat="identity")+
  facet_grid(shore~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of Feeding Mode traits over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones and shore levels
       Prevalence values indicate the number of taxa recorded in each Shore-Zone which have an affinity for a given trait.
       These are based on presence-only occurences and do not incorporate measures of taxon abundance.
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       No survey was conducted in 2010.")+
  ylab("Proportion of infaunal taxa")+
  theme(axis.title.x = element_blank(),
        # legend.title = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

## Lifespan_years ####
png(
  file = "output/figs/inf.Trt.ts.Lifespan.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
  )
dfl_shzone %>% 
  filter(., mesh != "0.5mm") %>%
  filter(., zone1 != "Wash") %>% 
  filter(.,trait=="Lifespan_years") %>% 
  rename(.,Lifespan_years=category) %>% 
  droplevels(.) %>% 
  ggplot(., aes(x = as.integer(year), y=affiliation, fill=Lifespan_years))+
  geom_bar( colour=1,position="fill",stat="identity")+
  facet_grid(shore~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of lifespans displayed by taxa over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones and shore levels
       Prevalence values indicate the number of taxa recorded in each Shore-Zone which have an affinity for a given trait.
       These are based on presence-only occurences and do not incorporate measures of taxon abundance.
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       No survey was conducted in 2010.")+
  ylab("Proportion of infaunal taxa")+
  theme(axis.title.x = element_blank(),
        # legend.title = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

## LarvalDevelopment ####
png(
  file = "output/figs/inf.Trt.ts.LarvDevt.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
)
dfl_shzone %>% 
  filter(., mesh != "0.5mm") %>%
  filter(., zone1 != "Wash") %>% 
  filter(.,trait=="LarvalDevelopment") %>% 
  rename(.,Larval_Development=category) %>% 
  droplevels(.) %>% 
  ggplot(., aes(x = as.integer(year), y=affiliation, fill=Larval_Development))+
  geom_bar( colour=1,position="fill",stat="identity")+
  facet_grid(shore~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of Larval Development traits over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones and shore levels
       Prevalence values indicate the number of taxa recorded in each Shore-Zone which have an affinity for a given trait.
       These are based on presence-only occurences and do not incorporate measures of taxon abundance.
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       No survey was conducted in 2010.")+
  ylab("Proportion of infaunal taxa")+
  theme(axis.title.x = element_blank(),
        # legend.title = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

## LivingHabit ####
png(
  file = "output/figs/inf.Trt.ts.LivHab.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
  )
dfl_shzone %>% 
  filter(., mesh != "0.5mm") %>%
  filter(., zone1 != "Wash") %>% 
  filter(.,trait=="LivingHabit") %>% 
  rename(.,Living_Habit=category) %>% 
  droplevels(.) %>% 
  ggplot(., aes(x = as.integer(year), y=affiliation, fill=Living_Habit))+
  geom_bar( colour=1,position="fill",stat="identity")+
  facet_grid(shore~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of Living Habit traits over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones and shore levels
       Prevalence values indicate the number of taxa recorded in each Shore-Zone which have an affinity for a given trait.
       These are based on presence-only occurences and do not incorporate measures of taxon abundance.
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       No survey was conducted in 2010.")+
  ylab("Proportion of infaunal taxa")+
  theme(axis.title.x = element_blank(),
        # legend.title = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

## Morphology ####
png(
  file = "output/figs/inf.Trt.ts.Morph.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
  )
dfl_shzone %>% 
  filter(., mesh != "0.5mm") %>%
  filter(., zone1 != "Wash") %>% 
  filter(.,trait=="Morphology") %>% 
  rename(.,Morphology=category) %>% 
  droplevels(.) %>% 
  ggplot(., aes(x = as.integer(year), y=affiliation, fill=Morphology))+
  geom_bar( colour=1,position="fill",stat="identity")+
  facet_grid(shore~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of Morphology traits over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones and shore levels
       Prevalence values indicate the number of taxa recorded in each Shore-Zone which have an affinity for a given trait.
       These are based on presence-only occurences and do not incorporate measures of taxon abundance.
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       No survey was conducted in 2010.")+
  ylab("Proportion of infaunal taxa")+
  theme(axis.title.x = element_blank(),
        # legend.title = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

## Mobility ####
png(
  file = "output/figs/inf.Trt.ts.Mobil.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
  )
dfl_shzone %>% 
  filter(., mesh != "0.5mm") %>%
  filter(., zone1 != "Wash") %>% 
  filter(.,trait=="Mobility") %>% 
  rename(.,Mobility=category) %>% 
  droplevels(.) %>% 
  ggplot(., aes(x = as.integer(year), y=affiliation, fill=Mobility))+
  geom_bar( colour=1,position="fill",stat="identity")+
  facet_grid(shore~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of Mobility traits over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones and shore levels
       Prevalence values indicate the number of taxa recorded in each Shore-Zone which have an affinity for a given trait.
       These are based on presence-only occurences and do not incorporate measures of taxon abundance.
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       No survey was conducted in 2010.")+
  ylab("Proportion of infaunal taxa")+
  theme(axis.title.x = element_blank(),
        # legend.title = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

## SedimentPosition ####
png(
  file = "output/figs/inf.Trt.ts.SedPos.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
  )
dfl_shzone %>% 
  filter(., mesh != "0.5mm") %>%
  filter(., zone1 != "Wash") %>% 
  filter(.,trait=="SedimentPosition") %>% 
  rename(.,Sediment_Position=category) %>% 
  droplevels(.) %>% 
  ggplot(., aes(x = as.integer(year), y=affiliation, fill=Sediment_Position))+
  geom_bar( colour=1,position="fill",stat="identity")+
  facet_grid(shore~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of Sediment Position traits over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones and shore levels
       Prevalence values indicate the number of taxa recorded in each Shore-Zone which have an affinity for a given trait.
       These are based on presence-only occurences and do not incorporate measures of taxon abundance.
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       No survey was conducted in 2010.")+
  ylab("Proportion of infaunal taxa")+
  theme(axis.title.x = element_blank(),
        # legend.title = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

## MaxSize ####
png(
  file = "output/figs/inf.Trt.ts.MaxSize.png",
  width = 15 * ppi,
  height = 10 * ppi,
  res = ppi
  )
dfl_shzone %>% 
  filter(., mesh != "0.5mm") %>%
  filter(., zone1 != "Wash") %>% 
  filter(.,trait=="MaxSize") %>% 
  rename(.,Max_Size=category) %>% 
  droplevels(.) %>% 
  ggplot(., aes(x = as.integer(year), y=affiliation, fill=Max_Size))+
  geom_bar( colour=1,position="fill",stat="identity")+
  facet_grid(shore~zone1)+
  scale_fill_manual(values = rep(cbPalette3,2))+
  labs(title = "Prevalence of the maximum size (mm) of taxa over time within monitoring zones",
       subtitle = "Intertidal infaunal assemblages sampled as part of the Saltfleet to Gibraltar Point Strategy",
       caption="Mean relative prevalences of major taxonomic groups across monitoring zones and shore levels
       Prevalence values indicate the number of taxa recorded in each Shore-Zone which have an affinity for a given trait.
       These are based on presence-only occurences and do not incorporate measures of taxon abundance.
       Assemblages were recorded in intertidal infaunal cores extracted from mid and low shore intertidal sediments and sieved over a 1mm mesh.
       No survey was conducted in 2010.")+
  ylab("Proportion of infaunal taxa")+
  theme(axis.title.x = element_blank(),
        # legend.title = element_blank(),
        legend.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"))
dev.off()

# dfl_list <- split.data.frame(dfl,f = dfl$trait)
#######################################################
# TO DO ####
# finalise formatting for stacked bar chart
# need to sort factor levels


# # TIDY UP ####
# # rm(list = ls(pattern = "^df"))
# rm(list = ls(pattern = "^cb"))
# rm(cur.yr,destination_file,file_name,fol,gisfol,perm,ppi,source_file)
# 
# detach(package:readxl, unload=TRUE)
# detach(package:tidyverse, unload=TRUE)

## imp.inf0.R ##
## Import infaunal data from current year and combine with historic

# Set up ####
### load packages ####
ld_pkgs <- c("tidyverse")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### load metadata ####
source("R/00_meta_setMeta.R")

### load data ####
df0 <- as_tibble(read.csv(paste0(fol,"inf_ts_longRAW_USE.csv")))

#########################################################################
#OLD CODE (FOR REFERENCE)####
#### prep.inf.R ####
# ##### prep infauna ts data
# 

# # ### save raw data ####
# # ptm <- proc.time()
# # write.csv(df_abund,file="data/processed/csv/inf.abund.ts.RAW.csv",row.names=FALSE)
# # saveRDS(df_abund,file="data/processed/R/inf.abund.ts.RAW.Rdat")
# # write.csv(df_noZero,file="data/processed/csv/inf.abund.ts.RAW.noZero.csv",row.names=FALSE)
# # saveRDS(df_noZero,file="data/processed/R/inf.abund.ts.RAW.noZero.Rdat")
# # proc.time() - ptm
# 
# #### load previously imported data ####
# df_abund0 <- readRDS("data/processed/R/inf.abund.ts.RAW.Rdat")
# #df_noZero <- readRDS("data/processed/R/inf.abund.ts.RAW.noZero.Rdat")
# 
# df_abund <- df_abund0
# 
# ### replace "." with space in taxon names
# df_abund$taxonRAW <- df_abund$taxon
# df_abund$taxon <- gsub(".", " ", df_abund$taxon, fixed=TRUE)
# 
# ### sort out capitalisation
# source("R/functions/capitalise.R")
# df_abund$taxon <- capitalise(df_abund$taxon)
# rm(capitalise)
# 
# ### remove taxa flagged as fragments
# df_abund <- df_abund %>% 
#   filter(.,!str_detect(taxon,"fragment"))
# 
# # xx <- df_abund0 %>% 
# #   filter(.,str_detect(taxon,"ragment"))
# 
# ### remove text after '#' character and trim whitespace
# df_abund$taxon<-trimws(gsub("#.*","",df_abund$taxon))
# ###throw out NA's
# df_abund <- df_abund %>% 
#   filter(!is.na(abundance))
# 
# ### save data & export taxon names for matching
# # txnm <- unique(df_abund$taxon)
# # write.csv(txnm,file="data/processed/csv/inf.ts.tax.csv",row.names=FALSE)
# 
# ### import correct taxon names & append ####
# tx <- openxlsx::read.xlsx("data/processed/csv/inf.ts.tax.xlsx",sheet="tax.out")
# df_abund <- left_join(df_abund,tx,by="taxon")
# rm(tx)
# 
# ### drop 'inappropriate' variables ####
# ### trim
# df_abund <- df_abund %>% 
#   filter(.,!str_detect(taxon,"Anthropogenic")) %>% ###drop 'anthropogenics'
#   filter(.,!str_detect(taxon,"Plastic")) %>%  ### drop 'plastics'
#   filter(., is.na(Kingdom) | Kingdom == "Animalia") %>% ## keep only fauna & 'no biota'
#   filter(., taxon_USE != "del")
# 
# ### drop nuicance/non-marine taxa
# df_abund <- df_abund %>% 
#   filter(., is.na(Class) | Class != "Insecta") %>% ###drop Insects
#   filter(., taxon_USE != "Animalia") %>% ###drop taxa flagged only as Animalia
#   filter(., is.na(Class) | Class != "Arachnida") ### drop spiders 156680
# #write.csv(df_abund,file="data/processed/csv/inf.abund.ts.longForTidying.csv",row.names = FALSE)
# 
# ### sum duplicated taxa
# df_abund <- df_abund %>% 
#   filter(!is.na(abundance)) %>% 
#   group_by(.,tranShoreRepMeshYr,transect,shore,rep,core_area_m2,mesh,year,taxon,
#            taxonRAW,taxon_USE,AphiaID,Match.type,ScientificName,AphiaID_accepted,
#            ScientificName_accepted,Kingdom,Phylum,Class,Order,Family,Genus,Species,
#            isMarine,isBrackish,isFresh,isTerrestrial) %>% 
#   summarise(.,abundance=sum(abundance)) %>% ungroup()
# 
# ### save files
# # write.csv(df_abund,file="data/processed/csv/inf.abund.ts.long.tidy.rep.csv",row.names=FALSE)
# # df_abund <- read.csv("data/processed/csv/inf.abund.ts.long.tidy.rep.csv")
# 
# ### calculate means across replicates & widen
# #drp <- c("rep","tranShoreRepMeshYr")### cols to drop when summarising
# 
# #### remove unneccessary columns, widen, fill in with zeroes,
# ###re-lengthen remove Rep, calculate means, rewiden
# df_abund_w <- df_abund %>% 
#   select(., -AphiaID,-taxonRAW,-taxon,-Match.type,-ScientificName,
#          -AphiaID_accepted,-ScientificName_accepted,-Kingdom,-Phylum,-Class,-Order,
#          -Family,-Genus,-Species,-isMarine,-isBrackish,-isFresh,-isTerrestrial) %>% ##drop cols
#   group_by(.,tranShoreRepMeshYr,transect,shore,rep,core_area_m2,mesh,year,taxon_USE) %>% 
#   summarise(.,abundance=sum(abundance)) %>% ungroup() %>% ### sum to remove duplicate taxon names
#   ### widen and fill gaps with 0:
#   pivot_wider(.,names_from=taxon_USE,values_from=abundance,values_fill=list(abundance = 0)) %>% 
#   ### re-lengthen for summarising:
#   pivot_longer(.,8:ncol(.),names_to="taxon",values_to="abundance") %>% 
#   select(.,-rep,-tranShoreRepMeshYr) %>% ###drop 'rep' and code variables
#   ### calculate mean across replicates:
#   group_by(.,transect,shore,core_area_m2, mesh,year,taxon) %>%
#   summarise(.,abundance=mean(abundance)) %>% ungroup() %>%###calc mean across replicates
#   ##re-widen:
#   pivot_wider(.,names_from=taxon,values_from=abundance) %>% ungroup() %>% 
#   relocate(.,"No biota", .after=year)
# 
# 
# df_abund_w$yr.trn.sh.meth <- paste0(df_abund_w$year,".",df_abund_w$transect,".",df_abund_w$shore,
#                                     ".",df_abund_w$mesh)
# df_abund_w <- df_abund_w %>% 
#   relocate(.,yr.trn.sh.meth)### move to start
# 
# ### save & tidy ####
# write.csv(df_abund_w,file="data/processed/csv/inf.abund.ts.csv",row.names = FALSE)
# saveRDS(df_abund_w,file="data/processed/R/inf.abund.ts.Rdat")
# 
# ####
# rm(list = ls(pattern = "^df"))
# rm(libfolder, ptm,txnm)
# detach(package:tidyverse, unload=TRUE)

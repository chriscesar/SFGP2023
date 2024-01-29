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

dfl <- dflall %>% 
  dplyr::select(.,!c(rep)) %>% #remove replicates
  group_by(across(c(!affiliation))) %>%
  summarise(.,affiliation=mean(affiliation, #calc mean prevalence across reps
                               na.rm = TRUE),
            .groups = "drop")


## add trait and category column names
# Splitting the strings based on "_"
split_values <- strsplit(dfl$trait_cat, "_")

# Extracting the values before and after the first "_"
dfl$trait <- sapply(split_values, function(x) x[1])
dfl$category <- sapply(split_values, function(x) paste(x[-1], collapse = "_"))


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

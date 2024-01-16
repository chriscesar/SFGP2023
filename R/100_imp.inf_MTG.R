# 100_imp.inf_MTG.R ####
## Import major taxonomic group data for consideration of current distribution
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

df0 <- as_tibble(read_xlsx(destination_file,
                           sheet = "MTG"))

# # drop nuicance/non-marine taxa ####
# df <- df0 %>%
#   filter(., units != "flag_fragments") %>% # remove taxa flagged as fragments
#   filter(., units != "flag_plants") %>% # remove taxa flagged as plants
#   filter(., is.na(Order) | Order != "Diptera") %>% ###drop flies
#   filter(., is.na(Order) | Order != "Hemiptera") %>% ###drop bugsLepidoptera
#   filter(., is.na(Order) | Order != "Lepidoptera") %>% ###drop butterflies/moths
#   filter(., is.na(Order) | Order != "Hymenoptera") %>% ###drop ants/bees/wasps
#   filter(., taxonUSE != "Animalia") ###drop taxa flagged only as Animalia

#convert to presence-only
df <- df0

# calculate means across replicates and widen data for further analysis ####
dfw <- df %>%
  ### convert abundance to binary
  mutate(abundance = 1) %>%
  ## remove superfluous cols
  dplyr::select(
    .,
    -taxonReported, -units,
    -Kingdom,-Phylum,-Class,
    -Order,-Family,-Genus,
    -Species,-Comment,-taxonUSE
  ) %>%
  ### sum MTG values within samples
  group_by(across(c(!abundance))) %>%
  summarise(., abundance = sum(abundance, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  ### calculate means across reps
  dplyr::select(., -rep, -yr.trn.sh.meth.rep) %>%
  group_by(across(c(!abundance, MTG))) %>%
  summarise(., abundance = mean(abundance, na.rm = TRUE), .groups = "drop") %>%
  ungroup() %>%
  ## rewiden
  pivot_wider(
    .,
    names_from = MTG,
    values_from = abundance,
    values_fill = list(abundance = 0)
  ) %>%
  ungroup()

# TIDY UP ####
# rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^cb"))
rm(cur.yr,destination_file,file_name,fol,gisfol,perm,ppi,source_file)

detach(package:readxl, unload=TRUE)
detach(package:tidyverse, unload=TRUE)

## imp.hiResPSA0.R ##
## Prep 'high resolution' for import into Gradistat workbook

### load packages ####
ld_pkgs <- c("tidyverse")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### set metadata ####
source("R/00_meta_setMeta.R")

df0 <- read.csv(paste0(fol,"sed.psa.hi.ts.csv"))

df0 %>% 
  filter(.,year=="2023") -> dfcur

## widen for export
dfcur %>% 
  dplyr::select(.,code,microns,perc) %>%
  pivot_wider(.,names_from =  code,
              values_from = perc) -> dfw

dfw <- dfw[order(-dfw$microns),]

write.csv(dfw,file="data/forGradistat.csv",row.names = FALSE)

# TIDY UP ####
rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^cb"))
rm(cur.yr,fol,gisfol,perm,ppi)

detach(package:tidyverse, unload=TRUE)
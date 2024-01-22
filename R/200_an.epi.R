## 200_an.epi.R ##
## Analysis epifaunal & Crangon data from current year and combine with historic

### load data & format
source("R/100_imp.epi0.R")

dfw$zone1 <- factor(dfw$zone1,levels=c("Above","Inside","Inside2","Below","Wash"))

### load packages
ld_pkgs <- c("tidyverse","ggplot2", "vegan")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### set metadata
source("R/00_meta_setMeta.R")

### calculate indices
## faunal density

tmp <- dfw[,-c(1:5)]
rowSums(tmp[])
tmp <- apply(tmp, 2, function(x) ifelse(x == "-9999", 0, as.numeric(x)))
# Replace negative values with 0
tmp <- pmax(tmp, 0)
# Calculate row sums, ignoring values less than zero
dfw$N <- rowSums(tmp)
rm(tmp)

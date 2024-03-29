### 00_meta_setMeta.R ###
## set metadata/themes/etc for project

cbPalette <- c( ### colourblind-friendly chart colour palette
  # comments reflect Red/Green/Blue equivalents
  "#0072B2", #000, 114, 178
  "#e79f00", #231, 159, 0
  "#009E73", #000, 158, 115
  "#9ad0f3", #154, 208, 243
  "#000000", #0, 0, 0
  "#D55E00", #213, 94, 0
  "#CC79A7", #204, 121, 167
  "#DEAAC6", #222, 170, 198 - The Wash
  "#F0E442"  #240, 228, 66
  )
cbPaletteTxt <- c(
  "#0072B2", #000, 114, 178
  "#e79f00", #231, 159, 0
  "#009E73", #000, 158, 115
  "#78b0d1", #120, 176, 209
  "#000000", #0, 0, 0
  "#D55E00", #213, 94, 0
  "#CC79A7", #204, 121, 167
  "#F0E442") #240, 228, 66

cbPaletteshr <- c("#F15854","#B276B2","#B2912F","#4D4D4D")

ppi <- 300 #figure resolution
cur.yr <- 2023 #current year

ggplot2::theme_set(ggthemes::theme_few())###set theme for all ggplot objects
perm <- 9999 #number of permutations for analyses

source("R/00_datfol.R")
source("R/00_helper_functions.R")

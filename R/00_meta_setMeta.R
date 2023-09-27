### 00_meta_setMeta.R ###
## set metadata/themes/etc for project

cbPalette <- c( ### colourblind-friendly chart colour palette
  # comments reflect Red/Green/Blue equivalents
  "#0072B2", #000/114/178
  "#e79f00",
  "#009E73",#000/158/115
  "#9ad0f3",
  "#000000", 
  "#D55E00",
  "#CC79A7",
  "#F0E442"
  )
cbPaletteTxt <- c(
  "#0072B2",
  "#e79f00",
  "#009E73",
  "#78b0d1",
  "#000000",
  "#D55E00",
  "#CC79A7",
  "#F0E442")

ppi <- 300 #figure resolution
cur.yr <- 2023 #current year

theme_set(ggthemes::theme_few())###set theme for all ggplot objects
perm <- 9999 #number of permutations for analyses
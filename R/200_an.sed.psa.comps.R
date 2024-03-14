### an.sed.psa.comps.R ####

### comparing 5cm and 15cm sediment data

# Set up ####
## set local library ####
libfolder <- "M:/R/Shared Library"
.libPaths(libfolder)

### set metadata
cur.yr <- 2022 # current year
cbPalette <- c("#0072B2","#e79f00","#009E73", "#9ad0f3", "#000000", 
               "#D55E00", "#CC79A7", "#F0E442")
ppi <- 300
theme_set(ggthemes::theme_few())###set theme for all ggplot objects

## load data 
df0 <- openxlsx::read.xlsx("data/historic/sed.psa.bulkWIP_use.xlsx",sheet="sed.bulk.ts.out")

### keep only current year's cores
df <- df0 %>% 
  filter(., year == cur.yr) %>% droplevels(.)

### sort factors ####
df$shore <- factor(df$shore,levels=c("Upper","Mid","Low","Surf"))
df$zone1 <- factor(df$zone1,levels=c("Above","Inside","Inside2","Below"))

# sort data 
df <- as_tibble(df[order(df$site_code, df$method),])

# paired t tests ####
dia5cm <- df[df$method=="5cm",]
dia15cm <- df[df$method=="15cm",]

# mean sediment diameter
t.test(dia5cm$MEAN_folkWard_phi,dia15cm$MEAN_folkWard_phi,paired=TRUE,alternative="two.sided")

# sorting
t.test(dia5cm$SORTING_folkWard_phi,dia15cm$SORTING_folkWard_phi,paired=TRUE,alternative="two.sided")

# skewness
t.test(dia5cm$SKEWNESS_folkWard_phi,dia15cm$SKEWNESS_folkWard_phi,paired=TRUE,alternative="two.sided")

#kurtosis
t.test(dia5cm$KURTOSIS_folkWard_um,dia15cm$KURTOSIS_folkWard_um,paired=TRUE,alternative="two.sided")

# gravel
t.test(dia5cm$GRAVEL_perc,dia15cm$GRAVEL_perc,paired=TRUE,alternative="two.sided")

# sand
t.test(dia5cm$SAND_perc,dia15cm$SAND_perc,paired=TRUE,alternative="two.sided")

# mud
t.test(dia5cm$MUD_perc,dia15cm$MUD_perc,paired=TRUE,alternative="two.sided")

## tidy up ####
rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^dia"))
rm(cbPalette,cur.yr,libfolder,ppi)

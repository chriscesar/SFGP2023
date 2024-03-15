### 200_an.sed.cur.models.R ####
### ANOVAs across sediment parameters for current year's data.
### includes grain modalities and chemistry

### load packages ####
ld_pkgs <- c("tidyverse","lmerTest","effects")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### set universals ####
source("R/00_meta_setMeta.R")

## MODALITIES DATA ####
#### load packages & data ####
df0 <- openxlsx::read.xlsx(paste0(fol,"sed.psa.bulkWIP_use.xlsx"),sheet="sed.bulk.ts.out")
#### format factors####
df0$transect <- factor(df0$transect,levels=c("T1N","T1","T1S",
                                             "T4","T11","T7","T8","T12","T13",
                                             "T15", "T17", "T20", "T21", "T22",
                                             "T23",
                                             "T24","T25","T26",
                                             "WA1"))

df0$shore <- factor(df0$shore,levels=c("Upper","Mid","Low","Surf"))
df0$zone1 <- factor(df0$zone1,levels=c("Above","Inside","Inside2","Below","Wash"))

### keep only 5cm cores & current year
df0 %>% 
  filter(.,year==cur.yr) %>% 
  filter(.,method=="5cm") %>% 
  droplevels(.) -> df

### define function for standard error
se <- function(x) sqrt(var(x)/length(x))

#### Mean phi ####
# phi.mod <- aov(mean.phi ~ zone1*shore, data = df0);
phi.mean <- aggregate(df$MEAN_folkWard_phi~df$zone1, FUN=mean)
phi.se <- aggregate(df$MEAN_folkWard_phi~df$zone1, FUN=se)
phi.mean.s <- aggregate(df$MEAN_folkWard_phi~df$shore, FUN=mean)
phi.se.s <- aggregate(df$MEAN_folkWard_phi~df$shore, FUN=se)
phi.mean.sz <- aggregate(df$MEAN_folkWard_phi~df$shore*df$zone1, FUN=mean)
phi.se.sz <- aggregate(df$MEAN_folkWard_phi~df$shore*df$zone1, FUN=se)

range(df$MEAN_folkWard_phi) # as phi
range(df$MEAN_folkWard_um/1000) # as mm
mnmx <- df[c(which.min(df$MEAN_folkWard_phi),
             which.max(df$MEAN_folkWard_phi)),]
mnmx[,c(1:7,31)]

phi.mean;phi.se
phi.mean.s;phi.se.s
tm <- phi.mean.sz[,c(1:3)]
tm <- cbind(tm,phi.se.sz$`df$MEAN_folkWard_phi`) ## breaks?!

### zones
anova(mod2 <- lmer(MEAN_folkWard_phi ~ zone1 + (1|shore) , data = df,REML=TRUE))
summary(mod2)
# multiFit <- allFit(mod2)
# summary(multiFit)$fixef

d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
visreg::visreg(mod2)
rm(mod2,d)

### shore
anova(mod2 <- lmer(MEAN_folkWard_phi ~ shore + (1|zone1) , data = df,REML=TRUE))
summary(mod2)

d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
rm(mod2,d)

#### D10 ####
### zones
anova(mod2 <- lmer(D10_phi ~ zone1 + (1|shore) , data = df,REML=TRUE))
summary(mod2)

d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
rm(mod2,d)

#### D50 ####
### zones
anova(mod2 <- lmer(D50_phi ~ zone1 + (1|shore) , data = df,REML=TRUE))
summary(mod2)

d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
rm(mod2,d)

#### D90 ####
### zones
anova(mod2 <- lmer(D90_phi ~ zone1 + (1|shore) , data = df,REML=TRUE))
summary(mod2)

d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
rm(mod2,d)

#### Sorting ####
### zones
anova(mod2 <- lmer(SORTING_folkWard_phi ~ zone1 + (1|shore) , data = df,REML=TRUE))
summary(mod2)

d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
rm(mod2,d)

#### Skew ####
### zones
anova(mod2 <- lmer(SKEWNESS_folkWard_phi ~ zone1 + (1|shore) , data = df,REML=TRUE))
summary(mod2)

(mnmx <- df[c(which.min(df$SKEWNESS_folkWard_phi),
              which.max(df$SKEWNESS_folkWard_phi)),c(1:7,33)])


d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
rm(mod2,d)

#### Kurtosis ####
### zones
anova(mod2 <- lmer(KURTOSIS_folkWard_phi ~ zone1 + (1|shore) , data = df,REML=TRUE))
summary(mod2)

(mnmx <- df[c(which.min(df$SKEWNESS_folkWard_phi),
              which.max(df$SKEWNESS_folkWard_phi)),c(1:7,33)])


d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
rm(mod2,d)

#### partial tidy up ####
# rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^phi"))
rm(tm,mnmx,df0)

### CHEMICAL DATA ####
#### load packages & data ####
df0 <- as_tibble(openxlsx::read.xlsx(paste0(fol,"sed.psa.bulkWIP_use.xlsx"),sheet="chemLong.ts.out"))

#### format factors####
df0$transect <- factor(df0$transect,levels=c("T1N", "T1", "T1S",
                                             "T4", "T11", "T7", "T8", "T12",
                                             "T13",
                                             "T15", "T21", "T22", "T23",
                                             "T24", "T25", "T26",
                                             "WA1"))

df0$shore <- factor(df0$shore,levels=c("Upper","Mid","Low","Surf"))
df0$zone1 <- factor(df0$zone1,levels=c("Above","Inside","Inside2","Below","Wash"))

#### format data ####
### keep only 5cm cores & current year
df0 %>% 
  filter(.,year==cur.yr) %>% 
  filter(.,method!="15cm") -> dfchl

### define function for standard error
se <- function(x) sqrt(var(x)/length(x))

#### widen data
dfchl %>% 
dplyr::select(.,-c(qual,resultRaw,zone2.1,zone2.2)) %>% 
# group_by(transect,shore,method,year,zone1,det) %>% count() -> tmp
pivot_wider(.,names_from = det, values_from = result) -> dfw

#### Calcium carbonate ####
### zones
anova(mod2 <- lmer(CaCO3_dw_perc ~ zone1 + (1|shore) , data = dfw,REML=TRUE))
summary(mod2)

d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
rm(mod2,d)

#### Organic carbon ####
### zones
anova(mod2 <- lmer(C_org_dw_perc ~ zone1 + (1|shore) , data = dfw,REML=TRUE))
summary(mod2)

d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
rm(mod2,d)

#### Silicates ####
### zones
anova(mod2 <- lmerTest::lmer(Silicates_perc ~ zone1 + (1|shore),
                             data = dfw, REML=TRUE))
summary(mod2)

d <- as.data.frame(lmerTest::ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
rm(mod2,d)

### generate summary table for current year
df %>% 
  dplyr::select(.,zone1,shore,
                MEAN_folkWard_phi,
                D10_phi,
                D50_phi,
                D90_phi,
                SORTING_folkWard_phi,
                SKEWNESS_folkWard_phi,
                KURTOSIS_folkWard_phi
                ) %>% 
  group_by(zone1,shore) %>% 
  summarise(mean_phi=round(mean(MEAN_folkWard_phi),2),
            mean_phi_sd=round(sd(MEAN_folkWard_phi),2),
            mean_D10_phi=round(mean(D10_phi),2),
            mean_D10_phi_sd=round(sd(D10_phi),2),
            mean_D50_phi=round(mean(D50_phi),2),
            mean_D50_phi_sd=round(sd(D50_phi),2),
            mean_D90_phi=round(mean(D90_phi),2),
            mean_D90_phi_sd=round(sd(D90_phi),2),
            mean_Sorting_phi=round(mean(SORTING_folkWard_phi),2),
            mean_Sorting_phi_sd=round(sd(SORTING_folkWard_phi),2),
            mean_Skew_phi=round(mean(SKEWNESS_folkWard_phi),2),
            mean_Skew_phi_sd=round(sd(SKEWNESS_folkWard_phi),2),
            mean_Kurtosis_phi=round(mean(KURTOSIS_folkWard_phi),2),
            mean_Kurtosis_phi_sd=round(sd(KURTOSIS_folkWard_phi),2),
            .groups = "drop") -> summaryTbl

write.csv(summaryTbl, file="output/sed.bulk.summary.csv",row.names = FALSE)

### tidy up ####
rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^phi"))
rm(list = ls(pattern = "^cb"))
rm(cur.yr,ppi,se,fol,gisfol,perm, summaryTbl)

detach("package:effects", unload=TRUE)
detach("package:lmerTest", unload=TRUE)
detach("package:lme4", unload=TRUE)
detach("package:tidyverse", unload=TRUE)

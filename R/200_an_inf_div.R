# 200_an_inf_div.R ####
### calculate & analyse diversity indices ####

## load abundance & biomass time series data
source("R/100_imp.inf0.R")
### set metadata
source("R/00_meta_setMeta.R")
df_biom <- as_tibble(read.csv(paste0(fol,"inf.biom.ts.USE.csv"))) #load biomass TS
df_pre <- as_tibble(read.csv(paste0(fol,"inf_ts_div_pre2007.csv")))

### load packages
ld_pkgs <- c("tidyverse", "ggplot2", "vegan", "lmerTest", "patchwork")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

#############
# calculate indices ####
## taxon richness (S) ####
dfw$S <- vegan::specnumber(dfw[,-c(1:11)])

## Total individuals (N) ### as density/m2 ####
tmp <- dfw###temp data
tmp[tmp<0] <- 0
tmp$N <- rowSums(tmp[,c(12:(ncol(tmp)-1))])

dfw$N <- tmp$N
rm(tmp)

## Shannon index (H) ####
tmp <- dfw;tmp[tmp<0] <- 0###temp data
tmp$H <- vegan::diversity(tmp[,c(12:(ncol(tmp)-2))], index = "shannon")
dfw$H <- tmp$H;rm(tmp)

## Simpson’s index (lamda) ####
tmp <- dfw;tmp[tmp<0] <- 0###temp data
tmp$simp <- vegan::diversity(tmp[,c(12:(ncol(tmp)-3))], index = "simpson")
dfw$simp <- tmp$simp
rm(tmp)

## Pielou’s evenness (J) ####
# This is the Shannon index divided by the log of the species richness
dfw$J <- dfw$H/log(dfw$S)

## Margalef’s index (d) ####
# This is the number of taxa (S) minus 1 divided by the natural log of the total number of individuals (N)
dfw$d <- (dfw$S-1)/(dfw$N)
## Hill’s N1 ####
# This is the exponent of the Shannon index (H)
dfw$N1 <- exp(dfw$H)

## TO DO: calculate total biomass by sample
dfb <- df_biom %>% 
  dplyr::select(.,-biomass_raw_g, -units, -code, -Comment) %>%
  ungroup() %>% 
  # sum biomass by year/trasect/mesh/shore/rep
  dplyr::select(.,-taxon) %>% ungroup() %>% 
  group_by(across(c(!biomass_g_per_m2))) %>% 
  summarise(biom=sum(biomass_g_per_m2),
            .groups = "drop") %>% 
  #calculate mean biomass across replicates
  dplyr::select(.,-rep) %>% 
  group_by(across(c(!biom))) %>% 
  summarise(biom=mean(biom),
            .groups = "drop")
  
dfb$yr.trn.sh.meth <- paste0(dfb$year,".",dfb$transect,
                             ".",dfb$shore,".",dfb$mesh)

dfw <- left_join(dfw,dfb,by="yr.trn.sh.meth")

# data formatting ####
# rename columns
dfdiv <- dfw %>%
  rename(transect = transect.x,
         year = year.x,
         shore = shore.x,
         mesh = mesh.x) %>% 
  dplyr::select(.,-c(transect.y,year.y,shore.y,mesh.y)) %>% #remove superfluous repeated cols
  dplyr::select(.,-c("AFAUNAL":"Vesicularia spinosa")) %>% 
  dplyr::select(.,-c(zone2.1,zone2.2,yr.trn:yr.trn.sh.meth))#remove unneccessary cols

### append pre-2006 data
dfdiv <- bind_rows(df_pre,dfdiv)

# assign factors
## transect
dfdiv$transect <- factor(dfdiv$transect,levels=c("T1N","T1", "T1S",
                                             "T4",
                                             "T5","T6",
                                             "T7","T8",
                                             "T9",
                                             "T10",
                                             "T11","T12","T13",
                                             "T14",
                                             "T15","T16","T17",
                                             "T18","T19",
                                             "T20","T21",
                                             "T22","T23",
                                             "T24","T25","T26",
                                             "WA1"
                                             ))
## shore
dfdiv$shore <- factor(dfdiv$shore,levels = c("Mid","Low"))

###zone
ab <- c("T1N","T1", "T1S")
ins <- c("T4", "T5", "T6", "T7", "T8", "T9", "T10",
         "T11", "T12")
ins2 <- "T13"
bel <- c("T14",
         "T15","T16","T17",
         "T18","T19",
         "T20","T21",
         "T22","T23",
         "T24","T25","T26")
was <- "WA1"

dfdiv$zone1 <- ifelse(dfdiv$transect %in% ab, "Above",
                           ifelse(dfdiv$transect %in% ins, "Inside",
                                  ifelse(dfdiv$transect %in% ins2, "Inside2",
                                         ifelse(dfdiv$transect %in% bel, "Below",
                                                ifelse(dfdiv$transect %in% was, "Wash",NA
                                                )))))
dfdiv$zone1 <- factor(dfdiv$zone1,levels=c("Above","Inside",
                                       "Inside2","Below",
                                       "Wash"))
rm(ab,ins,ins2,bel,was)
# descriptive: current year & 1.0mm ####
df.cur <- dfdiv %>% 
  filter(., year == cur.yr) %>% 
  filter(., mesh == "1.0mm")

### summarise across all zones ####

mean(df.cur$S,na.rm = TRUE);sd(df.cur$S,na.rm = TRUE)

### summarise means and SD by zone ####
tmz <- droplevels(dfdiv[dfdiv$transect != "WA1" & dfdiv$mesh=="1.0mm",]) %>%
  group_by(zone1) %>%
  summarise(mn.S = mean(S,na.rm = TRUE), sd.S = sd(S, na.rm = TRUE),
            mn.N = mean(N,na.rm = TRUE), sd.N = sd(N, na.rm = TRUE),
            mn.simp = mean(simp,na.rm = TRUE), sd.simp = sd(simp, na.rm = TRUE),
            mn.d = mean(d,na.rm = TRUE), sd.d = sd(d, na.rm = TRUE),
            mn.H = mean(H,na.rm = TRUE), sd.H = sd(H, na.rm = TRUE),
            mn.J = mean(J,na.rm = TRUE), sd.J = sd(J, na.rm = TRUE),
            mn.N1 = mean(N1,na.rm = TRUE), sd.N1 = sd(N1, na.rm = TRUE),
            mn.biom = mean(biom,na.rm = TRUE), sd.biom = sd(biom, na.rm = TRUE))

dfdivcur <- dfdiv %>% 
  filter(., mesh == "1.0mm") %>% 
  filter(., year == cur.yr) %>% 
  droplevels(.)

## species richness ####
anova(mod2 <- lmer(S ~ zone1 + (1|shore),
                   data=dfdivcur))
summary(mod2)

d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
visreg::visreg(mod2)
rm(mod2,d)

## abundance ####
mean(dfdivcur$N); sd(dfdivcur$N)
# dfdivcur[which.max(dfdivcur$N),c(2:7,(ncol(dfdivcur)-7):(ncol(dfdivcur)))]##max abund
# dfdivcur[which.min(dfdivcur$N),c(2:7,(ncol(dfdivcur)-7):(ncol(dfdivcur)))]##min abund]

anova(mod2 <- lmer(N ~ zone1 + (1|shore), data=dfdivcur))
# summary(mod2)
# d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
# d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
visreg::visreg(mod2)
rm(mod2,d)

## biomass ####
mean(dfdivcur$biom); sd(dfdivcur$biom)
# dfdivcur[which.max(dfdivcur$N),c(2:7,(ncol(dfdivcur)-7):(ncol(dfdivcur)))]##max abund
# dfdivcur[which.min(dfdivcur$N),c(2:7,(ncol(dfdivcur)-7):(ncol(dfdivcur)))]##min abund]

anova(mod2 <- lmer(biom ~ zone1 + (1|shore), data=dfdivcur))
# summary(mod2)
# d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
# d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
visreg::visreg(mod2)
rm(mod2,d)

## Margalef ####
anova(mod2 <- lmer(d ~ zone1 + (1|shore), data=dfdivcur))
rm(mod2)

## Shannon ####
anova(mod2 <- lmer(H ~ zone1 + (1|shore), data=dfdivcur))
rm(mod2)

## Pielou ####
anova(mod2 <- lmer(J ~ zone1 + (1|shore), data=dfdivcur))
rm(mod2)

# PLOTS ####
## Current year ####
### Spp Rich ####
S <- ggplot(data = dfdivcur,
aes(x = transect, y = S,
fill = zone1, color = "black"))+
geom_bar(stat = "identity", colour = "black")+
facet_wrap(~ shore, ncol = 1)+
theme(legend.position="none",strip.text.x = element_text(size = 12))+
scale_x_discrete(breaks = NULL)+
ylab("Taxon richness") + xlab("")+
scale_fill_manual(values=cbPalette)+
labs(fill = "")+
labs(subtitle = "Taxon richness")

### Faunal density ####
N <- ggplot(data = dfdivcur,
aes(x = transect, y = N,
fill = zone1, color = "black"))+
geom_bar(stat = "identity", colour = "black")+
facet_wrap(~ shore, ncol = 1)+
theme(legend.position="none",strip.text.x = element_text(size = 12))+
scale_x_discrete(breaks=NULL)+
ylab("Faunal density") + xlab("")+
scale_fill_manual(values=cbPalette)+
labs(fill="")+
labs(subtitle = "Taxon density")

### Logged faunal density ####
lN <- ggplot(data = dfdivcur,
aes(x = transect, y = log(N),
fill = zone1, color = "black"))+
geom_bar(stat = "identity", colour = "black")+
facet_wrap(~ shore, ncol = 1)+
theme(legend.position="none",strip.text.x = element_text(size = 12))+
scale_x_discrete(breaks=NULL)+
ylab("Log(faunal density)") + xlab("")+
scale_fill_manual(values=cbPalette)+
labs(fill="")+
labs(subtitle = "Taxon density (log)")

### Faunal Biomass ####
B <- ggplot(data = dfdivcur,
aes(x = transect, y = biom,
fill = zone1, color = "black"))+
geom_bar(stat = "identity", colour = "black")+
facet_wrap(~ shore, ncol = 1)+
theme(legend.position="none",strip.text.x = element_text(size = 12))+
scale_x_discrete(breaks=NULL)+
ylab("Faunal biomass") + xlab("")+
scale_fill_manual(values=cbPalette)+
labs(fill="")+
labs(subtitle = "Faunal biomass")

### Logged faunal Biomass ####
lB <- ggplot(data = dfdivcur,
aes(x = transect, y = log1p(biom),
fill = zone1, color = "black"))+
geom_bar(stat = "identity", colour = "black")+
facet_wrap(~ shore, ncol = 1)+
theme(legend.position="none",strip.text.x = element_text(size = 12))+
scale_x_discrete(breaks=NULL)+
ylab("Log faunal biomass") + xlab("")+
scale_fill_manual(values=cbPalette)+
labs(fill="")+
labs(subtitle = "Faunal biomass (log)")

### shannon ####
H <- ggplot(data = dfdivcur,
aes(x = transect, y = H,
fill = zone1, color = "black"))+
geom_bar(stat = "identity", colour = "black")+
facet_wrap(~ shore, ncol = 1)+
theme(legend.position="none",strip.text.x = element_text(size = 12))+
scale_x_discrete(breaks=NULL)+
ylab("Shannon's entropy") + xlab("")+
scale_fill_manual(values=cbPalette)+
labs(fill="")+
labs(subtitle = expression("Shannon's entropy ("~italic("H'")~")"))

### Pielou ####
J <- ggplot(data = dfdivcur,
aes(x = transect, y = J,
fill = zone1, color = "black"))+
geom_bar(stat = "identity", colour = "black")+
facet_wrap(~ shore, ncol = 1)+
theme(legend.position="none",strip.text.x = element_text(size = 12),
axis.text.x  = element_text(angle=90, vjust=0.5, size = 12))+
scale_y_continuous(breaks=c(0,0.5, 1))+
ylab("Pielou's evenness") + xlab("")+
scale_fill_manual(values=cbPalette)+
# labs(subtitle = "")
labs(subtitle = expression("Pielou's evenness ("~italic("J'")~")"))

### Margalef ####
d <- ggplot(data = dfdivcur,
aes(x = transect, y = d,
fill = zone1, color = "black"))+
geom_bar(stat = "identity", colour = "black")+
facet_wrap(~ shore, ncol = 1)+
theme(legend.title=element_blank(),legend.direction="horizontal",
legend.position = c(.75, 1.3),
strip.text.x = element_text(size = 12),
axis.text.x  = element_text(angle=90,
vjust=0.5,
size = 12)
)+
#scale_y_continuous(breaks=c(0,0.5, 1))+
ylab("Margalef's d") + xlab("")+
scale_fill_manual(values=cbPalette)+
labs(fill="")+
labs(subtitle = expression("Margalef's index ("~italic("d")~")"))
guides(fill=guide_legend(nrow=2, byrow=TRUE))

### COMBINE plots into single chart
x <- (lN|lB)/
(S|H)/
(J|d)

png(file = "output/figs/inf.2023.div.png",
width = 12 * ppi, height = 10 * ppi, res = ppi)
x+plot_annotation(tag_levels = "A")
dev.off();
rm(J,d,lB,B,S,H,lN,N)

## Time series ####
dfts <- dfdiv %>% 
  filter(.,is.na(mesh) | mesh=="1.0mm") %>% 
  filter(.,transect != "WA1")

### LOG density ####
N <- ggplot(data = dfts, aes(y = log(N+1), x = year, fill = zone1)) +
geom_hline(yintercept = mean(log(dfts$N+1),na.rm = TRUE),colour="grey",linetype="dashed")+
geom_hline(yintercept = min(log(dfts$N+1),na.rm = TRUE),colour="grey",linetype="dotted")+
geom_hline(yintercept = max(log(dfts$N+1),na.rm = TRUE),colour="grey",linetype="dotted")+
geom_boxplot(aes(group=year))+
# geom_smooth(method = "loess", colour = "red", span = .9)+
geom_smooth(method = "gam", colour = "red", span = .9)+
facet_grid(shore~zone1)+
scale_colour_manual(name = "", values=cbPalette)+
scale_fill_manual(name = "", values=cbPalette)+
xlab("Year") + ylab(bquote("Log faunal density"))+
scale_x_continuous(breaks = seq(1996, 2022, by = 2))+
coord_cartesian(ylim=c(0,NA))+
theme(legend.position="none",
strip.text.x = element_text(size = 12),
strip.text.y = element_text(size = 12),
axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))

png(file = "output/figs/inf.ts.logN.1996_gam.png",
width=12*ppi, height=6*ppi, res=ppi)
# png(file = "output/figs/inf.ts.logN.1996_loess.png",
#     width=12*ppi, height=6*ppi, res=ppi)
print(N);
dev.off();
rm(N)

### species ####
S <- ggplot(data = dfts, aes(y = S, x = year, fill = zone1))+
geom_hline(yintercept = mean(dfts$S,na.rm = TRUE),colour="grey",linetype="dashed")+
geom_hline(yintercept = min(dfts$S,na.rm = TRUE),colour="grey",linetype="dotted")+
geom_hline(yintercept = max(dfts$S,na.rm = TRUE),colour="grey",linetype="dotted")+
geom_boxplot(aes(group=year))+
geom_smooth(method = "loess", colour = "red", span = .9)+
# geom_smooth(method = "gam", colour = "red", span = .9)+
facet_grid(shore~zone1)+
scale_colour_manual(name = "", values=cbPalette)+
scale_fill_manual(name = "", values=cbPalette)+
scale_x_continuous(breaks = seq(1996, 2023, by = 2))+
xlab("Year") + ylab(bquote("Taxon richness"))+
theme(legend.position="none",
strip.text.x = element_text(size = 12),
strip.text.y = element_text(size = 12),
axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
coord_cartesian(ylim=c(0,NA))

png(file = "output/figs/inf.ts.S.1996_loess.png",
width=12*ppi, height=6*ppi, res=ppi)
# png(file = "output/figs/inf.ts.S.1996_gam.png",
#     width=12*ppi, height=6*ppi, res=ppi)
print(S);
dev.off();
rm(S)

### shannon ####
H <- ggplot(data = dfts, aes(y = H, x = year, fill = zone1))+
geom_hline(yintercept = mean(dfts$H,na.rm = TRUE),colour="grey",linetype="dashed")+
geom_hline(yintercept = min(dfts$H,na.rm = TRUE),colour="grey",linetype="dotted")+
geom_hline(yintercept = max(dfts$H,na.rm = TRUE),colour="grey",linetype="dotted")+
geom_boxplot(aes(group=year))+
# geom_smooth(method = "loess", colour = "red", span = .9)+
geom_smooth(method = "gam", colour = "red", span = .9)+
facet_grid(shore~zone1)+
scale_colour_manual(name = "", values=cbPalette)+
scale_fill_manual(name = "", values=cbPalette)+
xlab("Year") + ylab(bquote("Shannon diversity"))+
theme(legend.position="none",
strip.text.x = element_text(size = 12),
strip.text.y = element_text(size = 12),
axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
scale_x_continuous(breaks = seq(2007, 2023, by = 2))+
coord_cartesian(ylim=c(0,NA))

# png(file = "output/figs/inf.ts.H.1996_loess.png",
# width=12*ppi, height=6*ppi, res=ppi)
png(file = "output/figs/inf.ts.H.1996_gam.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(H);
dev.off();
rm(H)

### Pielou ####
J <- ggplot(data = dfts, aes(y = J, x = year, fill = zone1))+
geom_hline(yintercept = mean(dfts$J,na.rm = TRUE),colour="grey",linetype="dashed")+
geom_hline(yintercept = min(dfts$J,na.rm = TRUE),colour="grey",linetype="dotted")+
geom_hline(yintercept = max(dfts$J,na.rm = TRUE),colour="grey",linetype="dotted")+
geom_boxplot(aes(group=year))+
geom_smooth(method = "gam", colour = "red", span = .9)+
# geom_smooth(method = "loess", colour = "red", span = .9)+
facet_grid(shore~zone1)+
scale_colour_manual(name = "", values=cbPalette)+
scale_fill_manual(name = "", values=cbPalette)+
xlab("Year") + ylab(bquote("Pielou's evenness"))+
theme(legend.position="none",
strip.text.x = element_text(size = 12),
strip.text.y = element_text(size = 12),
axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
scale_x_continuous(breaks = seq(2007, 2023, by = 2))+
coord_cartesian(ylim=c(0,NA))

# png(file = "output/figs/inf.ts.J.1996_loess.png",
# width=12*ppi, height=6*ppi, res=ppi)
png(file = "output/figs/inf.ts.J.1996_gam.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(J);
dev.off();
rm(J)

### Margalef ####
# d <- ggplot(data = dfts, aes(y = d, x = year, fill = zone1))+
# geom_hline(yintercept = mean(dfts$d,na.rm = TRUE),colour="grey",linetype="dashed")+
# geom_hline(yintercept = min(dfts$d,na.rm = TRUE),colour="grey",linetype="dotted")+
# geom_hline(yintercept = max(dfts$d,na.rm = TRUE),colour="grey",linetype="dotted")+
# geom_boxplot(aes(group=year))+
# # geom_smooth(method = "loess", colour = "red", span = .9)+
#   geom_smooth(method = "gam", colour = "red", span = .9)+
# facet_grid(shore~zone1)+
# scale_colour_manual(name = "", values=cbPalette)+
# scale_fill_manual(name = "", values=cbPalette)+
# xlab("Year") + ylab(bquote("Margalef's richness"))+
#   ylim(0,2)+
# theme(legend.position="none",
# strip.text.x = element_text(size = 12),
# strip.text.y = element_text(size = 12),
# axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
# scale_x_continuous(breaks = seq(2007, 2023, by = 2))+
# coord_cartesian(ylim=c(0,NA))
# 
# png(file = "output/figs/inf.ts.d.1996_gam.png",
# width=12*ppi, height=6*ppi, res=ppi)
# # png(file = "output/figs/inf.ts.d.1996_loess.png",
# #     width=12*ppi, height=6*ppi, res=ppi)
# print(d);
# dev.off();
# rm(d)

### log biomass_m2 ####
d <- ggplot(data = dfts, aes(y = log(biom+1), x = year, fill = zone1))+
geom_hline(yintercept = mean(log(dfts$biom+1),na.rm = TRUE),colour="grey",linetype="dashed")+
geom_hline(yintercept = min(log(dfts$biom+1),na.rm = TRUE),colour="grey",linetype="dotted")+
geom_hline(yintercept = max(log(dfts$biom+1),na.rm = TRUE),colour="grey",linetype="dotted")+
geom_boxplot(aes(group=year))+
geom_smooth(method = "loess", colour = "red", span = .9)+
  # geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
scale_colour_manual(name = "", values=cbPalette)+
scale_fill_manual(name = "", values=cbPalette)+
xlab("Year") + ylab(bquote("log(Biomass)"))+
scale_x_continuous(breaks = seq(2007, 2023, by = 2))+
theme(legend.position="none",
strip.text.x = element_text(size = 12),
strip.text.y = element_text(size = 12),
axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))

# png(file = "output/figs/inf.ts.logbiom.1996_gam.png",
# width=12*ppi, height=6*ppi, res=ppi)
png(file = "output/figs/inf.ts.logbiom.1996_loess.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(d);
dev.off();
rm(d)

### export biomass for report appendices
## TO DO: calculate total biomass by sample
df_biom %>% 
  filter(year==cur.yr) %>% 
  filter(mesh=="1.0mm") %>% 
  dplyr::select(.,-biomass_raw_g, -units, -code, -Comment) %>%
  ungroup() %>% 
  ### widen, fill with zeroes, re-lengthen
  pivot_wider(names_from = taxon, values_from = biomass_g_per_m2,
              values_fill=list(biomass_g_per_m2 = 0)) %>% 
  pivot_longer(cols = AFAUNAL:"Pontocrates arenarius",
               names_to = "taxon",
               values_to = "biomass_g_per_m2") %>% 
  dplyr::select(.,-rep) %>%
  group_by(across(c(!biomass_g_per_m2))) %>% 
  summarise(mb=mean(biomass_g_per_m2),
            .groups = "drop")->df_biom_w


df_biom_w$yr.trn.sh.meth <- paste0(df_biom_w$year,".",df_biom_w$mesh,".",
                                   df_biom_w$transect,".",df_biom_w$shore)

df_biom_w <- df_biom_w %>%
  dplyr::select(.,-c(year,mesh,transect,shore)) %>% 
  pivot_wider(.,names_from = yr.trn.sh.meth,values_from = mb)

write.csv(df_biom_w,file="output/dfw_biomass.csv")

## Tidy up ####
rm(list = ls(pattern = "^df"))
rm(cbPalette,cbPaletteTxt, ppi,tmz,perm,cur.yr,x,
   fol,gisfol)

detach("package:tidyverse", unload=TRUE)
detach("package:lmerTest", unload=TRUE)
detach("package:vegan", unload=TRUE)
detach("package:patchwork", unload=TRUE)

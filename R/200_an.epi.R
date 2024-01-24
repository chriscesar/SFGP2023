## 200_an.epi.R ####
## Analysis epifaunal & Crangon data from current year and combine with historic

## load data & format ####
source("R/100_imp.epi0.R")

dfw$zone1 <- factor(dfw$zone1,levels=c("Above","Inside","Inside2","Below","Wash"))

## load packages
ld_pkgs <- c("tidyverse","ggplot2", "vegan", "lmerTest")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### set metadata
source("R/00_meta_setMeta.R")

## calculate indices ####
## faunal density (N)
tmp <- dfw[,-c(1:5)]
tmp <- apply(tmp, 2, function(x) ifelse(x == "-9999", 0, as.numeric(x)))
# Replace negative values with 0
tmp <- pmax(tmp, 0)
# Calculate row sums, ignoring values less than zero
dfw$N <- rowSums(tmp)
rm(tmp)

## species richness (S)
tmp <- dfw[,c(6:(ncol(dfw)-1))]
tmp <- (tmp <0 | tmp >0)
S <- rowSums(tmp)
dfw$S <- S
rm(tmp, S)

### explore data for current year ####
# tax rich
dfw %>% 
  filter(., year == cur.yr) %>% 
  dplyr::select(.,c(1:5,N:S)) %>% 
  View(.)

# RUN MODELS #####
### amend structure to focus models on Inside
dfw$zone1mod <- factor(dfw$zone1, levels=c("Inside","Above","Inside2","Below"))
dfw$zone1 <- factor(dfw$zone1, levels=c("Above","Inside","Inside2","Below"))

#### taxon density #####
dfw %>% 
  filter(.,year == cur.yr)-> dfcur

N_pl <- ggplot(dfcur,aes(x=zone1,y=N,fill=zone1))+
  geom_boxplot(outlier.colour = NA,show.legend = FALSE)+
  geom_jitter(height=0,width=0.35,alpha=0.3,show.legend = FALSE)+
  scale_fill_manual(values=cbPalette);N_pl

summary(mod1 <- lmer(N ~ zone1mod + (1|depth) + (1|mon),
                     data=dfcur))
anova(mod1)
d <- as.data.frame(ls_means(mod1, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod1,show.values=TRUE, show.p=TRUE)
# visreg::visreg(mod1)
rm(mod1,d,N_pl)

#### taxon richness ####
S_pl <- ggplot(dfcur,aes(x=zone1,y=S,fill=zone1))+
  geom_boxplot(outlier.colour = NA,show.legend = FALSE)+
  geom_jitter(height=0,width=0.35,alpha=0.3,show.legend = FALSE)+
  scale_fill_manual(values=cbPalette);S_pl

summary(mod1 <- lmer(S ~ zone1mod + (1|depth) + (1|mon),
                     data=dfcur))

anova(mod1)
d <- as.data.frame(ls_means(mod1, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
# sjPlot::plot_model(mod1,show.values=TRUE, show.p=TRUE)
# visreg::visreg(mod1)
rm(mod1,d,S_pl)

##########
### to do:####
## generate time series charts

### species ####
S <- ggplot(data = dfw, aes(y = S, x = year, fill = zone1))+
  geom_hline(yintercept = mean(dfw$S,na.rm = TRUE),colour="grey",linetype="dashed")+
  geom_hline(yintercept = min(dfw$S,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_hline(yintercept = max(dfw$S,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  # geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(depth~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  scale_x_continuous(breaks = seq(2011, 2023, by = 2))+
  xlab("Year") + ylab(bquote("Taxon richness"))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
  coord_cartesian(ylim=c(0,NA));S

png(file = "output/figs/epi.ts.S_loess.png",
    width=12*ppi, height=6*ppi, res=ppi)
# png(file = "output/figs/epi.ts.S_gam.png",
#     width=12*ppi, height=6*ppi, res=ppi)
print(S);
dev.off();
rm(S)

### TaxDensity ####
N <- ggplot(data = dfw, aes(y = log(N+1), x = year, fill = zone1))+
  geom_hline(yintercept = mean(dfw$S,na.rm = TRUE),colour="grey",linetype="dashed")+
  geom_hline(yintercept = min(dfw$S,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_hline(yintercept = max(dfw$S,na.rm = TRUE),colour="grey",linetype="dotted")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  # geom_smooth(method = "gam", colour = "red", span = .9)+
  facet_grid(depth~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  scale_x_continuous(breaks = seq(2011, 2023, by = 2))+
  xlab("Year") + ylab(bquote("Faunal density"))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
  coord_cartesian(ylim=c(0,NA));N

png(file = "output/figs/epi.ts.S_loess.png",
    width=12*ppi, height=6*ppi, res=ppi)
# png(file = "output/figs/epi.ts.S_gam.png",
#     width=12*ppi, height=6*ppi, res=ppi)
print(N);
dev.off();
rm(N)

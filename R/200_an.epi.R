## 200_an.epi.R ####
## Analysis epifaunal & Crangon data from current year and combine with historic

## load data & format ####
source("R/100_imp.epi0.R")

dfw$zone1 <- factor(dfw$zone1,levels=c("Above","Inside","Inside2","Below","Wash"))

## load packages
ld_pkgs <- c("tidyverse","ggplot2", "vegan", "lmerTest","ggpp","ggtext",
             "mvabund", "patchwork")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### set metadata
source("R/00_meta_setMeta.R")

cbPalette2 <- c( ### colourblind-friendly chart colour palette
  # comments reflect Red/Green/Blue equivalents
  "#0072B2", #000, 114, 178
           "#003959",
           "#e79f00", #231, 159, 0
           "#745000",
           "#009E73", #000, 158, 115
           "#004F3A",
           "#9ad0f3", #154, 208, 243
           "#4D6879",
           "#000000", #0, 0, 0
           "#D55E00", #213, 94, 0
           "#CC79A7", #204, 121, 167
           "#DEAAC6", #222, 170, 198 - The Wash
           "#F0E442"  #240, 228, 66
)

cbPalette2 <- c( ### colourblind-friendly chart colour palette
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
# dfw %>% 
#   filter(., year == cur.yr) %>% 
#   dplyr::select(.,c(1:5,N:S)) %>% 
#   View(.)

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
  # geom_jitter(alpha=0.6)+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  # geom_smooth(method = "loess", span = .9, aes(group=mon, col=mon))+
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

png(file = "output/figs/epi.ts.N_loess.png",
    width=12*ppi, height=6*ppi, res=ppi)
# png(file = "output/figs/epi.ts.S_gam.png",
#     width=12*ppi, height=6*ppi, res=ppi)
print(N);
dev.off();
rm(N)

# MULTIVARIATE ####
#=#=#=#=#=#=#=#=#==
### Ordination ####
#=#=#=#=#=#=#=#=#==
met <- dfcur %>% 
  dplyr::select(.,year:mon,zone1mod)
dat <- dfcur %>% 
  dplyr::select(.,-c(year:mon,zone1mod,S,N)) %>% 
  mutate_all(~ifelse(. < 0, 1, .)) %>% 
  dplyr::select(.,(names(.)[colSums(.) >= 1])) #%>% 

colnames(dat) <- make.cepnames(colnames(dat)) #shorten names for display
  
### RUN ORDINATION ####
set.seed(pi)
ord <- metaMDS(
  dat,
  distance = "bray",
  k = 2,
  trymax = 500,
  maxit = 1500
)

plot(ord)


### MDS plot prep ####
abv <- c("T1")
ins <- c("T4","T8")
ins2 <- "T13"
bel <- c("T20")

data.scores <- as.data.frame(scores(ord)[1])
names(data.scores) <- c("NMDS1","NMDS2")
data.scores$transect <- met$transect
data.scores$depth <- met$depth
data.scores$mon <- met$mon
data.scores$zone1 <- ifelse(data.scores$transect %in% abv, "Above",
                            ifelse(data.scores$transect %in% ins, "Inside",
                                   ifelse(data.scores$transect %in% ins2, "Inside2",
                                          ifelse(data.scores$transect %in% bel, "Below","ERROR")
                                   )))

species.scores <- as.data.frame(scores(ord, "species"))
species.scores$species <- rownames(species.scores)
data.scores$zone1 <- factor(data.scores$zone1,
                            levels =
                              c("Above", "Inside", "Inside2",
                                "Below"))

data.scores$zoneMon <- paste0(data.scores$zone1,data.scores$mon)
rm(abv,bel,ins,ins2)

### MDS classic version ####
#### version 1 ####
# Need to tweak model to generate output for each month
png(
  # file = "output/figs/epi.MDS.23.Sep.png",
  file = "output/figs/epi.MDS.23.Oct.png",
  width = 12 * ppi,
  height = 8 * ppi,
  res = ppi
)

ggplot() +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_vline(xintercept = 0, colour = "grey") +
  geom_text(data = species.scores,
            aes(x = NMDS1, y = NMDS2,
                label = species),
            alpha = 0.5) + # add the species labels
  geom_point(data = data.scores[data.scores$mon=="Oct",],
             aes(
               x = NMDS1,
               y = NMDS2,
               shape = depth,
               colour = zone1),
             size = 8,
             show.legend = TRUE
             ) +# add the point markers
  # scale_fill_manual(values = cbPalette) +
  scale_color_manual("Zone", values = cbPalette) +
  scale_shape_discrete(name = "Depth") +
  coord_fixed()+
  # geom_text_npc(aes(npcx = .99, npcy = .99, label=paste("September")))+
  geom_text_npc(aes(npcx = .99, npcy = .99, label=paste("October\nStress = ",
                                                        round(ord$stress, 3))))+
  # facet_wrap(.~mon)
  labs(
  # title = paste0("Nonmetric Multidimensional Scaling plot of epifauna recorded in ",cur.yr),
  #      # caption=expression(atop(Based~on~0.1~m^2~sediment~cores~sieved~over~a~1~mm~mesh,
  #      #                         paste("Midshore sample of transect T8 excluded"))))+
       caption="Based on epibenthic beam trawls.<br>'Presence-only' taxon scores replaced with values of '1'")+
  theme(plot.caption = element_markdown(lineheight = 1.2))

dev.off()

#### version 2 ####
# Alternative version
png(
  file = "output/figs/epi.MDS.23.SepOct.png",
  width = 12 * ppi,
  height = 8 * ppi,
  res = ppi
  )
ggplot() +
  geom_hline(yintercept = 0, colour = "grey") +
  geom_vline(xintercept = 0, colour = "grey") +
  geom_text(data = species.scores,
            aes(x = NMDS1, y = NMDS2,
                label = species),
            alpha = 0.5) + # add the species labels
  geom_point(data = data.scores,
             aes(
               x = NMDS1,
               y = NMDS2,
               shape = depth,
               fill = zone1,
               colour=mon),
             size = 7,
             show.legend = FALSE,
             stroke=1.5 #control border thickness of points
  ) +
  scale_colour_manual("Month", values = c("red","black")) +
  scale_shape_manual("Depth",values = rep(c(21,24,22),2))+
  scale_fill_manual("Zone", values = cbPalette) +
  coord_fixed()+
  # geom_text_npc(aes(npcx = .99, npcy = .99, label=paste("September")))+
  geom_text_npc(aes(npcx = .99, npcy = .99, label=paste("Stress = ",
                                                        round(ord$stress, 3))))+
  guides(colour = "none")+
  # facet_wrap(.~mon)
  labs(
    title = paste0("Nonmetric Multidimensional Scaling plot of epifauna recorded in ",cur.yr),
    caption="Based on epibenthic beam trawls.<br>'Presence-only' taxon scores replaced with values of '1'")+
  theme(plot.caption = element_markdown(lineheight = 1.2))
dev.off()

# ADONIS  (Permanova) ####
# ano_epiinf <- adonis2(dat ~ met$zone1,
#                       permutations = perm)
# saveRDS(ano_epiinf,file="output/models/epiadonis2023.Rdat")
(ano_epiinf <- readRDS(file="output/models/epiadonis2023.Rdat"))
### is P <0.05?
ano_epiinf[["Pr(>F)"]][1] <0.05 ###Is P<0.05?

###output model summaries
###full model
write.csv(ano_epiinf,
          file="output/models/ano_epiinf.csv",
          row.names=TRUE)
rm(ano_epiinf)

# ADONIS  (Permanova) ####
# ano_epiinf2 <- adonis2(dat ~ met$zone1*met$depth*met$mon,
#                       permutations = perm)
# saveRDS(ano_epiinf2, file="output/models/epiadonis2023_full.Rdat")
(ano_epiinf2 <- readRDS(file="output/models/epiadonis2023_full.Rdat"))
### is P <0.05?
ano_epiinf2[["Pr(>F)"]][1] <0.05 ###Is P<0.05?

###output model summaries
###full model
write.csv(ano_epiinf2,
          file="output/models/ano_epiinf.csv",
          row.names=TRUE)


# MVABUND version ####
cur_spp <- mvabund(dat)

# mean-variance plot
png(file = "output/figs/epiMeanVar.png",
    width=12*ppi, height=6*ppi, res=ppi)
mvpl <- mvabund::meanvar.plot(cur_spp, add.trendline=TRUE,
                              xlab="Mean",
                              ylab="Variance",
                              table=TRUE
                              )

# Step 1: Find the minimum and maximum values
min_value <- min(mvpl[,2])
max_value <- max(mvpl[,2])

min_order <- floor(log10(min_value))
max_order <- floor(log10(max_value))
orders_of_magnitude_covered <- max_order - min_order

ttl <- "Very strong mean-variance relationship in epifaunal abundance data"
sbtt <- paste0("Variance within the dataset covers ",orders_of_magnitude_covered," orders of magnitude*.\nMany multivariate analyses (e.g. ANOSIM, PERMANOVA) assume *no mean-variance relationship*\nThis makes interpretation of such analyses potentially erroneous. Model-based approaches offer an alternative, allowing the mean-variance relationship to be incorporated into the model predictions")

mtext(side=3, line = 3, at =-0.07, adj=0, cex = 1, ttl, font=2)
mtext(side=3, line = 0.75, at =-0.07, adj=0, cex = 0.7, sbtt)
dev.off()

rm(min_value,max_value,min_order,max_order,orders_of_magnitude_covered,ttl,sbtt)

# mod1_pois <- manyglm(cur_spp ~ met$zone1, family="poisson")
# plot(mod1_pois)
mod1_nb <- manyglm(cur_spp ~ met$zone1, family="negative_binomial")
plot(mod1_nb)
# AIC(mod1_pois,mod1_nb)

# mod2 <- manyglm(cur_spp ~ met$zone1*met$depth, family="negative_binomial")
# plot(mod2)
# 
# summary(mod2)

# anova_mod2 <- mvabund::anova.manyglm(mod2,p.uni = "adjusted")
# saveRDS(anova_mod2,file="output/models/epi.2023.mvabund.Rdat")
(res.binary <- readRDS("output/models/epi.2023.mvabund.Rdat"))

mod3 <- manyglm(cur_spp ~ met$zone1*met$depth*met$mon, family="negative_binomial")
plot(mod3)
summary(mod3)

# anova_mod3 <- mvabund::anova.manyglm(mod3,p.uni = "adjusted")
# saveRDS(anova_mod3,file="output/models/epi.2023.mvabund_mod3.Rdat")
(res.binary <- readRDS("output/models/epi.2023.mvabund_mod3.Rdat"))

### export current year's taxa for appendices
remove_zero_sum_numeric_cols <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  sum_zero <- sapply(data[, numeric_cols], sum) == 0
  data %>%
    select(-which(numeric_cols)[sum_zero])
}

# Apply the function to your dataframe
result <- remove_zero_sum_numeric_cols(dfcur)
write.csv(result, file = "output/dfw_epi.csv",row.names = FALSE)

# plot taxon trends ####
df0 %>% 
  dplyr::select(.,-c(taxonRecorded,Kingdom:Species,Comments,DataSource)) %>% 
  group_by(year,transect,zone1,depth,mon,taxonUse) %>% 
  summarise(abund=sum(abund),.groups = "drop") %>% 
  # distinct() %>% group_by(year,transect,zone1,depth,mon,taxonUse) %>% count() %>% filter(n>1) %>% View(.)
  pivot_wider(names_from = taxonUse, values_from = abund,values_fill = 0) %>%
  pivot_longer(cols = Ammodytes:"Electra monostachys",names_to = "taxon", values_to = "abund") %>% 
  ## drop month and take mean by station
  dplyr::select(.,-c(mon)) %>% 
  group_by(year,transect,zone1,depth, taxon) %>% 
  summarise(abund=mean(abund),.groups="drop") %>% 
  filter(.,abund>-1) %>% 
  ggplot(data=.,aes(x=year, y=log(abund+1)))+
  geom_line(aes(group=taxon))+geom_point(aes(group=taxon))+
  facet_grid(depth~zone1, scales = "free_y")+
  geom_smooth(se=FALSE, alpha=0.8,aes(group=taxon))

# Tidy up ####
# unload packages
detach("package:mvabund", unload=TRUE)
detach("package:patchwork", unload=TRUE)
detach("package:ggtext", unload=TRUE)
detach("package:ggpp", unload=TRUE)
detach("package:lmerTest", unload=TRUE)
detach("package:vegan", unload=TRUE)
detach("package:ggplot2", unload=TRUE)
detach("package:tidyverse", unload=TRUE)

# remove data
rm(list = ls(pattern = "^mod"))
rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^cbPal"))
rm(dat,fol, ppi, cur.yr,gisfol,perm,ord,res.binary,ano_epiinf2,cur_spp,
   data.scores,met,mvpl,species.scores,result,remove_zero_sum_numeric_cols)

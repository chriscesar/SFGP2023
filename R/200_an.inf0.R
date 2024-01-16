# 200_an.inf0.R ####
### Multivariate analysis of 2022 intertidal invertebrate data

## load data from long term
source("R/100_imp.inf0.R")

### load packages
ld_pkgs <- c("tidyverse","ggplot2","vegan","ggdendro",#data vis
             "dendextend",#data vis
             "ggtext",#data vis
             "ggpp",
             "mvabund",
             "Hmsc",
             "ggpubr"
             )
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### set metadata
source("R/00_meta_setMeta.R")

# rejig data ####
### keep only 1 mm data from current year
df.cur <- dfw[dfw$year==cur.yr,]
df.cur <- df.cur[df.cur$mesh=="1.0mm",]

df.cur$mesh <- NULL

### lengthen, remove zeroes and rewiden to retain only recorded taxa
df.cur %>% 
  pivot_longer(cols =`Abra alba`:`Vesicularia spinosa`,
               names_to = "taxon",values_to = "abundance") %>% 
  dplyr::select(.,-AFAUNAL) %>% 
  filter(.,abundance != 0) %>% # remove zero values
  mutate(abundance = ifelse(abundance < 0, #replace <0 values with 1
                            1,
                            abundance)) %>% #rewiden for ordination
  pivot_wider(.,names_from = taxon,
              values_from = abundance,
              values_fill=list(abundance = 0)) %>% 
  ungroup() -> df.cur.w.trm
  

#=#=#=#=#=#=#=#=#==
### Ordination ####
#=#=#=#=#=#=#=#=#==
temp.ord <- as.data.frame(df.cur.w.trm)
##set row names
row.names(temp.ord) <- paste0(df.cur.w.trm$transect,".",df.cur.w.trm$shore)

tt <- rowSums(temp.ord[, -c(1:10)])###remove non-taxon metadata
use <- tt > 0
temp.ord <- temp.ord[use, ] # removed empty rows
rm(use, tt)
temp.ord.run <- temp.ord[,-c(1:9)]
colnames(temp.ord.run) <-
  make.cepnames(colnames(temp.ord.run)) #shorten names for display

### RUN ORDINATION ####
set.seed(pi)
ord <- metaMDS(
  temp.ord.run,#[,colSums(temp.ord.run)>100],
  distance = "bray",
  k = 2,
  trymax = 500,
  maxit = 1500
)

plot(ord)

abv <- c("T1","T1N","T1S")
ins <- c("T4","T7","T8","T11","T12")
ins2 <- "T13"
bel <- c("T15","T21","T22","T23","T24","T25","T26")

### MDS plot prep ####
data.scores <- as.data.frame(scores(ord)[1])
names(data.scores) <- c("NMDS1","NMDS2")
data.scores$transect <- temp.ord$transect
data.scores$shore <- temp.ord$shore
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
rm(abv,bel,ins,ins2)

### MDS classic version ####
p <- ggplot() +
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
               shape = shore,
               colour = zone1),
             size = 8) +# add the point markers
  scale_fill_manual(values = cbPalette) +
  scale_color_manual("Zone", values = cbPalette) +
  scale_shape_discrete(name = "Shore") +
  coord_fixed()+
  geom_text_npc(aes(npcx = .99, npcy = .99, label=paste("Stress = ",
                                                        round(ord$stress, 3))))+
  labs(title = paste0("Nonmetric Multidimensional Scaling plot of intertidal infauna recorded in ",cur.yr),
       # caption=expression(atop(Based~on~0.1~m^2~sediment~cores~sieved~over~a~1~mm~mesh,
       #                         paste("Midshore sample of transect T8 excluded"))))+
       caption="Based on 0.1m^2 sediment cores sieved over a 1 mm mesh.<br>
       'Presence-only' taxon scores replaced with values of '1'")+
  theme(plot.caption = element_markdown(lineheight = 1.2));p

### export mds plot ####
png(
  file = "output/figs/inf.MDS.23.png",
  width = 12 * ppi,
  height = 8 * ppi,
  res = ppi
)
print(p)
dev.off()

rm(p,ord)

### no longer able to run SIMPROF using clustsig package.
# check here for alternative?
# https://www.davidzeleny.net/anadat-r/doku.php/en:hier-agglom_examples

# ANOSIM ####
anosim_intinf <- anosim(temp.ord.run,
                        grouping = df.cur$zone1,permutations=perm)
saveRDS(anosim_intinf,file="output/models/intinfAnosim2023.Rdat")
(anosim_intinf <- readRDS("output/models/intinfAnosim2023.Rdat"))
summary(anosim_intinf)
plot(anosim_intinf)

# ADONIS  (Permanova) ####
ano_intinf <- adonis2(temp.ord.run ~ df.cur$zone1,
                            permutations = perm)
saveRDS(ano_intinf,file="output/models/intinfadonis2023.Rdat")
(ano_intinf <- readRDS(file="output/models/intinfadonis2023.Rdat"))
### is P <0.05?
ano_intinf[["Pr(>F)"]][1] <0.05 ###Is P<0.05?

###output model summaries
###full model
write.csv(ano_intinf,
          file="output/models/ano_intinf.csv",
          row.names=TRUE)

# MVABUND version ####
cur_spp <- mvabund(df.cur.w.trm[,-c(1:9)])

# mean-variance plot
ttl <- "Very strong mean-variance relationship in zooplankton abundance data"
sbtt <- "Variance within the dataset covers *5 orders of magnitude*.\nMany multivariate analyses (e.g. ANOSIM, PERMANOVA) assume *no mean-variance relationship*\nThis makes interpretation of such analyses potentially erroneous. Model-based approaches offer an alternative, allowing the mean-variance relationship to be incorporated into the model predictions"

png(file = "output/figs/infMeanVar.png",
    width=12*ppi, height=6*ppi, res=ppi)
mvabund::meanvar.plot(cur_spp, add.trendline=TRUE,
                      xlab="Mean",
                      ylab="Variance",
                      )
mtext(side=3, line = 3, at =-0.07, adj=0, cex = 1, ttl, font=2)
mtext(side=3, line = 0.75, at =-0.07, adj=0, cex = 0.7, sbtt)
dev.off()

mod1 <- manyglm(cur_spp ~ df.cur$zone1, family="poisson")
plot(mod1)

mod2 <- manyglm(cur_spp ~ df.cur$zone1*df.cur$shore, family="negative_binomial")
plot(mod2)
summary(mod2)

# anova_mod2 <- mvabund::anova.manyglm(mod2,p.uni = "adjusted")
# saveRDS(anova_mod2,file="output/models/inf.2023.mvabund.Rdat")
(res.binary <- readRDS("output/models/inf.2023.mvabund.Rdat"))

# ##############
# # HMSC version ####
# # To Revisit ####
# ## see here:
# #https://www.youtube.com/watch?v=u07eFE3Uqtg&t=730s
# #and here:
# #https://www.r-bloggers.com/2020/06/guide-to-using-the-hmsc-package-for-the-production-of-joint-species-distribtuion-models/
# 
# ### create 'boxes' of data for analysis ###
# ### Y: Species occurrence ####
# Y <- as.matrix(df.cur.w.trm[, -c(1:9)])
# Y[Y>0] <- 1 #convert to presence-absence
# 
# #### study design ####
# #Variable selection
# studyDesign <- df.cur.w.trm[, c(2,3)]
# studyDesign <- data.frame(studyDesign)
# studyDesign$transect <- as.factor(studyDesign$transect)
# studyDesign$shore <- as.factor(studyDesign$shore)
# ns <- ncol(df.cur.w.trm %>% dplyr::select(.,-c(year:yr.trn.sh.meth)))
# 
# #Variable structuring
# transect <- HmscRandomLevel(units = studyDesign$transect)
# shore <- HmscRandomLevel(units = studyDesign$shore)
# (ranlevels <- list(transect=transect,shore=shore))
# 
# ### X: Environmental ####
# X <- as.data.frame(df.cur$zone1)
# X$zone1 <- X$`df.cur$zone1`;X$`df.cur$zone1` <- NULL
# X$zone1 <- as.factor(X$zone1)
# XFormula <- ~zone1
# 
# ## create & run model ####
# # create model
# simul <- Hmsc(Y=Y, XData = X,
#               XFormula=XFormula,
#               studyDesign = studyDesign,
#               ranLevels = ranlevels,
#               distr = "probit")
# 
# # Run model
# thin <- 10
# samples <- 1000
# nChains <- 3
# transient <- ceiling(thin*samples*.5)
# 
# # ptm <- proc.time()
# # mod_HMSC <- sampleMcmc(simul,
# #                        samples = samples,
# #                        thin = thin,
# #                        transient = transient,
# #                        nChains = nChains#,
# #                        # nParallel = nChains
# #                        )
# # saveRDS(mod_HMSC, file = paste0("output/models/mod_HMSC","_smp",samples,
# #                                 "_thn",thin,"_trns",transient,"_chn",nChains,
# #                                 ".Rdata"))
# # proc.time() - ptm
# mod_HMSC <- readRDS("output/models/mod_HMSC_smp1000_thn10_trns5000_chn3.Rdata")
# # mod_HMSC <- readRDS("output/models/mod_HMSC_smp1000_thn50_trns25000_chn3.Rdata")
# 
# ## investigate model outputs ####
# mpost <- convertToCodaObject(mod_HMSC) # model diagnostics/convergence
# preds <- computePredictedValues(mod_HMSC) # model performance
# MF <- evaluateModelFit(hM=mod_HMSC, predY = preds) # r2, etc
# VP <- computeVariancePartitioning(mod_HMSC) # variance partitioning
# ### to check ####
# #VP warning:
# #In cor(lbeta[[i]][k, ], lmu[[i]][k, ]) : the standard deviation is zero
# 
# ess.beta <- effectiveSize(mpost$Beta) %>%
#   as_tibble() %>% dplyr::rename(ess_beta=value)
# psrf.beta <- gelman.diag(mpost$Beta, multivariate = FALSE)$psrf %>%
#   as_tibble() %>% dplyr::rename(psrf_beta = "Point est.")
# 
# diag_all <- ggarrange(ggplot(ess.beta,aes(x=ess_beta))+
#                         geom_histogram()+
#                         xlab("Effective sample size"),
#                       ggplot(psrf.beta,aes(x=psrf_beta))+
#                         geom_histogram()+
#                         geom_vline(xintercept = 1.001, col=2)+
#                         xlab("Gelman diagnostic"), align = "v")+
#   ggtitle("All plots");diag_all
# 
# hist(ess.gamma <- effectiveSize(mpost$Gamma))
# hist(psrf.gamma <- gelman.diag(mpost$Gamma, multivariate=FALSE)$psrf)
# 
# sppairs = matrix(sample(x = 1:ns^2, size = 100))
# tmp = mpost$Omega[[1]]
# for (chain in 1:length(tmp)){
#   tmp[[chain]] = tmp[[chain]][,sppairs]
# }
# ess.omega = effectiveSize(tmp)
# psrf.omega = gelman.diag(tmp, multivariate=FALSE)$psrf
# hist(ess.omega)
# hist(psrf.omega)
# 
# preds = computePredictedValues(simul)
# MF = evaluateModelFit(hM=m, predY=preds)
# hist(MF$R2, xlim = c(0,1), main=paste0("Mean = ", round(mean(MF$R2),2)))
# 
# MF$TjurR2 %>% mean(na.rm=TRUE)*100
# 
# # species niches
# postBeta <- getPostEstimate(mod_HMSC, parName = "Beta")
# 
# plotVariancePartitioning(mod_HMSC, VP=VP, las=2, horiz=TRUE)
# plotBeta(mod_HMSC,post=postBeta, param = "Support", #supportLevel=0.95,
#          split = .4, spNamesNumbers = c(T,F))

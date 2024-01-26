# 200_an.inf_gllvm.R ####
### Multivariate analysis of 2023 intertidal invertebrate data
### using gllvm

## load data from long term
source("R/100_imp.inf0.R")

### load packages
ld_pkgs <- c("tidyverse","ggplot2","vegan","ggdendro",#data vis
             "dendextend",#data vis
             "ggtext",#data vis
             "ggpp",
             "gllvm",
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

####

### prep models ####
### fit models ####
### unconstrained model ####
#### Tweedie distribution ####
ptm <- Sys.time()
sDsn <- data.frame(shore = df.cur.w.trm$shore,
                   transect=df.cur.w.trm$transect)
m_lvm_0 <- gllvm(df.cur.w.trm[,-c(1:9)],
                 family="tweedie",
                 studyDesign = sDsn,
                 row.eff = ~(1|shore)
                 )
saveRDS(m_lvm_0, file="output/models/gllvm_uncon_tweed.Rdat")
Sys.time() - ptm;rm(ptm)
m_lvm_0 <- readRDS("output/models/gllvm_uncon_tweed.Rdat")

### constrained model ####
#### Tweedie distribution ####
ptm <- Sys.time()
sDsn <- data.frame(shore = df.cur.w.trm$shore,
                   transect=df.cur.w.trm$transect)
m_lvm_1 <- gllvm(y=df.cur.w.trm[,-c(1:9)], # model with environmental parameters
                 X=df.cur.w.trm[,c(1:9)],
                 formula = ~ zone1,
                 family="tweedie",
                 studyDesign = sDsn, row.eff = ~(1|shore)
)
saveRDS(m_lvm_1, file="output/models/gllvm_con_tweed.Rdat")
Sys.time() - ptm;rm(ptm)
m_lvm_1 <- readRDS("output/models/gllvm_con_tweed.Rdat")

## compare models####
AIC(m_lvm_0,m_lvm_1)
anova(m_lvm_0,m_lvm_1)

pdf(file = "output/figs/m_lvm_3_tx_all.pdf",width=16,height=8)
coefplot(m_lvm_1,cex.ylab = 0.3,
         order=FALSE)
dev.off()

####################
# TO DO! ####
####################

## extract 'significant' model/species terms from model
# ci_mod_all <- as.data.frame(confint(m_lvm_1))
# ci_mod_var <- ci_mod_all[grep("^X", rownames(ci_mod_all)), ]
# rownames(ci_mod_var) <- substring(rownames(ci_mod_var), 7)
# ci_mod_var$varTrt <- rownames(ci_mod_var)
# 
# sigterms_all <- summary(m_lvm_1)
# sigterms_all <- as.data.frame(sigterms_all$Coef.tableX)
# sigterms_all$variable <- sub(":.*","",row.names(sigterms_all))
# sigterms_all$variable <- substr(sigterms_all$variable, 6, nchar(sigterms_all$variable))
# sigterms_all$trt <- sub(".*:","",row.names(sigterms_all))
# sigterms_all$varTrt <- rownames(sigterms_all)
# sigterms_all$varTrt <- substr(sigterms_all$varTrt, 6, nchar(sigterms_all$varTrt))
# sigterms_all <- left_join(sigterms_all, ci_mod_var, by = "varTrt")
# sigterms_all$sig <- sigterms_all$`2.5 %`*sigterms_all$`97.5 %`>0
# 
# sigterms_sig <- sigterms_all[sigterms_all$`Pr(>|z|)`>0.05,]
# 
# ### plot! ####
# ## recreate coefplot
# ggplot(sigterms_all[sigterms_all$variable=="nh4",],
#        aes(x=Estimate, y=trt,
#            xmin=`2.5 %`,
#            xmax=`97.5 %`,
#            colour=sig))+
#   geom_vline(xintercept = 0)+
#   geom_errorbar()+
#   geom_point()+
#   scale_y_discrete(limits = rev(levels(as.factor(sigterms_all$trt))))+
#   scale_colour_manual(values = c("grey","black"))+
#   guides(colour="none")

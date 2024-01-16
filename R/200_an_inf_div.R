# 200_an_inf_div.R ####
### calculate & analyse diversity indices ####

## load abundance & biomass time series data
source("R/100_imp.inf0.R")
### set metadata
source("R/00_meta_setMeta.R")
df_biom <- as_tibble(read.csv(paste0(fol,"inf.biom.ts.USE.csv")))

### load packages
ld_pkgs <- c("tidyverse", "ggplot2", "vegan")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

#############
# calculate indices ####
# taxon richness (S)
dfw$S <- vegan::specnumber(dfw[,-c(1:11)])

#Total individuals (N) ### as density/m2
tmp <- dfw###temp data
tmp[tmp<0] <- 0
tmp$N <- rowSums(tmp[,c(12:(ncol(tmp)-1))])

dfw$N <- tmp$N
rm(tmp)

#shannon index (H)
tmp <- dfw;tmp[tmp<0] <- 0###temp data
tmp$H <- vegan::diversity(tmp[,c(12:(ncol(tmp)-2))], index = "shannon")
dfw$H <- tmp$H;rm(tmp)

#Simpson’s index (lamda)
tmp <- dfw;tmp[tmp<0] <- 0###temp data
tmp$simp <- vegan::diversity(tmp[,c(12:(ncol(tmp)-3))], index = "simpson")
dfw$simp <- tmp$simp
rm(tmp)

#Pielou’s evenness (J) – this is the Shannon index divided by the log of the species richness
dfw$J <- dfw$H/log(dfw$S)

# Margalef’s index (d) – this is the number of taxa (S) minus 1 divided by the natural log of the total number of individuals (N)
dfw$d <- (dfw$S-1)/log(dfw$N)
# Hill’s N1 – this is the exponent of the Shannon index (H)
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


dfdiv <- dfw[,c(1:4,7,205:211,216)]
dfdiv[is.na(dfdiv$biom),]

# TO DO: CHECK FOR 'MISSING' BIOMASS IN DATA IMPORT ####

###### OLD CODE FROM 2022##########
#                         
 # ### load data ####
 # df_abund_w <- readRDS("data/processed/R/inf.abund.ts.Rdat") ###created in prep.inf.v2.R
 # df_biom_w <- readRDS("data/processed/R/biomass.ts.Rdat")### created in prep.inf.biom.R
 # df_div_old <- read.csv("data/historic/inf_ts_div_pre2007.csv")
 # 
 # df_biom_w$yr.trn.sh.meth <- df_biom_w$df_biom_w.yr.trn.sh.meth
 # df_biom_w$df_biom_w.yr.trn.sh.meth <- NULL
 # df_biom_w$biom <- df_biom_w$df_biom_w.biomsum;df_biom_w$df_biom_w.biomsum <- NULL
 # 
 # #############
 # # calculate indices ####
 # # taxon richness (S)
 # df_abund_w$S <- vegan::specnumber(df_abund_w[,-c(1:7)])
 # 
 # #Total individuals (N) ### as density/m2
 # tmp <- df_abund_w###temp data
 # tmp[tmp<0] <- 0
 # tmp$N <- rowSums(tmp[,c(8:(ncol(tmp)-1))])/tmp$core_area_m2
 # 
 # df_abund_w$N <- tmp$N
 # rm(tmp)
 # 
 # #shannon index (H)
 # tmp <- df_abund_w;tmp[tmp<0] <- 0###temp data
 # tmp$H <- vegan::diversity(tmp[,c(8:(ncol(tmp)-2))], index = "shannon")
 # df_abund_w$H <- tmp$H;rm(tmp)
 # 
 # #Simpson’s index (lamda)
 # tmp <- df_abund_w;tmp[tmp<0] <- 0###temp data
 # tmp$simp <- vegan::diversity(tmp[,c(8:(ncol(tmp)-3))], index = "simpson")
 # df_abund_w$simp <- tmp$simp
 # rm(tmp)
 # 
 # #Pielou’s evenness (J) – this is the Shannon index divided by the log of the species richness
 # df_abund_w$J <- df_abund_w$H/log(df_abund_w$S)
 # 
 # # Margalef’s index (d) – this is the number of taxa (S) minus 1 divided by the natural log of the total number of individuals (N)
 # df_abund_w$d <- (df_abund_w$S-1)/log(df_abund_w$N)
 # # Hill’s N1 – this is the exponent of the Shannon index (H)
 # df_abund_w$N1 <- exp(df_abund_w$H)
 # 
 # # ### append biomass data
 # # tmp <- df_biom_w %>% 
 # #   select(yr.trn.sh.meth,biom)
 # 
 # ### append biomass data
 # df_biom_w$yr.trn.sh.meth <- paste0(df_biom_w$df_biom_w.year,".",
 #                                    df_biom_w$df_biom_w.transect,".",
 #                                    df_biom_w$df_biom_w.shore,".",
 #                                    df_biom_w$df_biom_w.mesh)
 # xdf_biom_w <- df_biom_w %>% 
 #   select(yr.trn.sh.meth,biom) %>% 
 #   group_by(yr.trn.sh.meth) %>% 
 #   summarise(biom=mean(biom,na.rm=TRUE))
 # 
 # df_abund_w <- left_join(df_abund_w,xdf_biom_w,by="yr.trn.sh.meth")
 # rm(xdf_biom_w)
 # 
 # df_abund_w$transect <- factor(df_abund_w$transect,levels=c("T1N","T1", "T1S",
 #                                                            "T4","T7","T8",
 #                                                            "T11","T12","T13",
 #                                                            "T15","T17","T20","T21",
 #                                                            "T22","T23",
 #                                                            "T24","T25","T26",
 #                                                            "WA1"))
 # ### shore
 # df_abund_w$shore <- factor(df_abund_w$shore,levels = c("Mid","Low"))
 # ###zone
 # ab <- c("T1N","T1", "T1S")
 # ins <- c("T4","T7","T8","T11","T12")
 # ins2 <- "T13"
 # bel <- c("T15","T17","T20","T21","T22","T23","T24","T25","T26","WA1")
 # df_abund_w$zone1 <- ifelse(df_abund_w$transect %in% ab, "Above",
 #                            ifelse(df_abund_w$transect %in% ins, "Inside",
 #                                   ifelse(df_abund_w$transect %in% ins2, "Inside2",
 #                                          ifelse(df_abund_w$transect %in% bel, "Below",NA))))
 # 
 # df_abund_w$zone1 <- factor(df_abund_w$zone1,levels=c("Above","Inside",
 #                                                      "Inside2","Below"))
 # 
 # # append pre 2007 diversity indices ####
 # dfold <- df_div_old %>% 
 #   select(.,-"ï..code",-method)
 # 
 # df_abund_w <- bind_rows(dfold,df_abund_w)
 # 
 # ###arrange cols
 # df_abund_w <- df_abund_w %>% 
 #   relocate(yr.trn.sh.meth) %>% 
 #   relocate(S,.after=last_col()) %>% 
 #   relocate(N,.after=last_col()) %>% 
 #   relocate(H,.after=last_col()) %>% 
 #   relocate(simp,.after=last_col()) %>% 
 #   relocate(J,.after=last_col()) %>% 
 #   relocate(d,.after=last_col()) %>% 
 #   relocate(N1,.after=last_col()) %>% 
 #   relocate(biom,.after=last_col())
 # 
 # rm(ab,bel,ins,ins2,dfold,df_div_old)
 # df_abund <- df_abund_w %>% 
 #   filter(., year == cur.yr)
 # 
 # ##################
 # ### 2022 data ####
 # ##################
 # 
 # ####################################
 # ######################################################
 # ######################################################
 # ####################################
 # 
 # ## reorder to compare Inside with rest
 # df_abund$zone1 <- factor(df_abund$zone1,levels=c("Inside","Above","Inside2","Below"))
 # 
 # ### summarise means and SD by zone ####
 # tmz <- droplevels(df_abund[df_abund$transect != "WA1" & df_abund$mesh=="1.0mm",]) %>% 
 #   group_by(zone1) %>%
 #   summarise(mn.S = mean(S,na.rm = TRUE), sd.S = sd(S, na.rm = TRUE),
 #             mn.N = mean(N,na.rm = TRUE), sd.N = sd(N, na.rm = TRUE),
 #             mn.simp = mean(simp,na.rm = TRUE), sd.simp = sd(simp, na.rm = TRUE),
 #             mn.d = mean(d,na.rm = TRUE), sd.d = sd(d, na.rm = TRUE),
 #             mn.H = mean(H,na.rm = TRUE), sd.H = sd(H, na.rm = TRUE),
 #             mn.J = mean(J,na.rm = TRUE), sd.J = sd(J, na.rm = TRUE),
 #             mn.N1 = mean(N1,na.rm = TRUE), sd.N1 = sd(N1, na.rm = TRUE),
 #             mn.biom = mean(biom,na.rm = TRUE), sd.biom = sd(biom, na.rm = TRUE))
 # 
 # ## species richness ####
 # mean(df_abund$S[df_abund$mesh=="1.0mm"]); sd(df_abund$S[df_abund$mesh=="1.0mm"])
 # df_abund[which.max(df_abund$S),c(2:7,(ncol(df_abund)-7):(ncol(df_abund)))]##max taxa
 # df_abund[which.min(df_abund$S),c(2:7,(ncol(df_abund)-7):(ncol(df_abund)))]##min taxa
 # df_abund[df_abund$S == 0,c(2:7,(ncol(df_abund)-7):(ncol(df_abund)))] ## zeroes
 # 
 # anova(mod2 <- lmer(S ~ zone1 + (1|shore),
 #                    data=df_abund[df_abund$transect !="WA1" & df_abund$mesh=="1.0mm",]))
 # summary(mod2)
 # 
 # d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
 # d[d$`Pr(>|t|)`<0.051,]
 # sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
 # rm(mod2,d)
 # 
 # ## abundance ####
 # mean(df_abund$N); sd(df_abund$N)
 # df_abund[which.max(df_abund$N),c(2:7,(ncol(df_abund)-7):(ncol(df_abund)))]##max abund
 # df_abund[which.min(df_abund$N),c(2:7,(ncol(df_abund)-7):(ncol(df_abund)))]##min abund]
 # 
 # anova(mod2 <- lmer(N ~ zone1 + (1|shore),
 #                    data=df_abund[df_abund$transect !="WA1" & df_abund$mesh=="1.0mm",]))
 # summary(mod2)
 # d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
 # d[d$`Pr(>|t|)`<0.051,]
 # sjPlot::plot_model(mod2,show.values=TRUE, show.p=TRUE)
 # rm(mod2,d)
 # 
 # ## biomass ####
 # mean(df_abund$biom, na.rm=TRUE); sd(df_abund$biom,na.rm=TRUE)
 # df_abund[which.max(df_abund$biom),]
 # df_abund[which.min(df_abund$biom),]
 # 
 # anova(mod2 <- lmer(biom ~ zone1 + (1|shore),
 #                    data=df_abund[df_abund$transect !="WA1" & df_abund$mesh=="1.0mm",]))
 # summary(mod2)
 # anova(mod2)
 # d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
 # d[d$`Pr(>|t|)`<0.051,]
 # rm(mod2,d)
 # 
 # # Margalef's d ####
 # mean(dfcur$d, na.rm=TRUE); sd(dfcur$d,na.rm=TRUE)
 # dfcur[which.max(dfcur$N),]
 # dfcur[which.min(dfcur$N),]
 # 
 # ### working!
 # anova(mod2 <- lmer(d ~ zone1 + (1|shore),
 #                    data=df_abund[df_abund$transect !="WA1" & df_abund$mesh=="1.0mm",]))
 # summary(mod2)
 # 
 # d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
 # d[d$`Pr(>|t|)`<0.051,]
 # rm(mod2,d)
 # 
 # # Pielou's J ####
 # mean(dfcur$J,na.rm=TRUE); sd(dfcur$J,na.rm=TRUE)
 # dfcur[which.max(dfcur$J),]
 # dfcur[which.min(dfcur$J),]
 # 
 # anova(mod2 <- lmer(J ~ zone1 + (1|shore),
 #                    data=df_abund[df_abund$transect !="WA1" & df_abund$mesh=="1.0mm",]))
 # summary(mod2)
 # 
 # d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
 # d[d$`Pr(>|t|)`<0.051,]
 # rm(mod2,d)
 # 
 # # Shannon's H ####
 # mean(dfcur$H); sd(dfcur$H)
 # dfcur[which.max(dfcur$H),]
 # dfcur[which.min(dfcur$H),]
 # 
 # anova(mod2 <- lmer(H ~ zone1 + (1|shore),
 #                    data=df_abund[df_abund$transect !="WA1" & df_abund$mesh=="1.0mm",]))
 # summary(mod2)
 # 
 # d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
 # d[d$`Pr(>|t|)`<0.051,]
 # rm(mod2,d)
 # 
 # # Hill's N1 ####
 # mean(df_abund$N1); sd(df_abund$N1)
 # dfcur[which.max(dfcur$H),]
 # dfcur[which.min(dfcur$H),]
 # 
 # mod2 <- lmer(N1 ~ zone1 + (1|shore),
 #              data=df_abund[df_abund$transect !="WA1" & df_abund$mesh=="1.0mm",])
 # summary(mod2)
 # anova(mod2)
 # d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
 # d[d$`Pr(>|t|)`<0.051,]
 # rm(mod2,d)
 
 # TS PLOTS ######
 
#                         df <- df_abund_w[,-c(8:(ncol(df_abund_w)-8))] %>% 
#                           filter(.,is.na(mesh) | mesh=="1.0mm") %>% 
#                           filter(.,transect != "WA1")
#                         
#                         ###reorder factors for plotting
#                         df$zone1 <- factor(df$zone1,levels=c("Above","Inside","Inside2","Below"))
#                         df$shore <- factor(df$shore,levels=c("Mid","Low"))
#                         
#                         #### Plot
#                         ## LOG density WITH COPEPODS, NEMATODES & OSTRACODS
#                         N <- ggplot(data = df, aes(y = log(N+1), x = year, fill = zone1)) +
#                           geom_hline(yintercept = mean(log(df$N+1),na.rm = TRUE),colour="grey",linetype="dashed")+
#                           geom_hline(yintercept = min(log(df$N+1),na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_hline(yintercept = max(log(df$N+1),na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_boxplot(aes(group=year))+
#                           geom_smooth(method = "loess", colour = "red", span = .9)+
#                           facet_grid(shore~zone1)+
#                           # xlim(2007,NA)+
#                           #ylim(0,max(log(df$N+1)))+
#                           scale_colour_manual(name = "", values=cbPalette)+
#                           scale_fill_manual(name = "", values=cbPalette)+
#                           xlab("Year") + ylab(bquote("Log faunal density"))+
#                           #ylim(0,max(inf.dat$abund)) +
#                           scale_x_continuous(breaks = seq(1996, 2022, by = 2))+
#                           coord_cartesian(ylim=c(0,NA))+
#                           theme(legend.position="none",
#                                 strip.text.x = element_text(size = 12),
#                                 strip.text.y = element_text(size = 12),
#                                 axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))
#                         
#                         png(file = "output/figs/intinv/inf.ts.logN.1996.png",
#                             width=12*ppi, height=6*ppi, res=ppi)
#                         print(N);
#                         dev.off();
#                         rm(N)
#                         
#                         ## species
#                         S <- ggplot(data = df, aes(y = S, x = year, fill = zone1))+
#                           geom_hline(yintercept = mean(df$S,na.rm = TRUE),colour="grey",linetype="dashed")+
#                           geom_hline(yintercept = min(df$S,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_hline(yintercept = max(df$S,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_boxplot(aes(group=year))+
#                           geom_smooth(method = "loess", colour = "red", span = .9)+
#                           facet_grid(shore~zone1)+
#                           scale_colour_manual(name = "", values=cbPalette)+
#                           scale_fill_manual(name = "", values=cbPalette)+
#                           scale_x_continuous(breaks = seq(1996, 2022, by = 2))+
#                           xlab("Year") + ylab(bquote("Taxon richness"))+
#                           theme(legend.position="none",
#                                 strip.text.x = element_text(size = 12),
#                                 strip.text.y = element_text(size = 12),
#                                 axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
#                           coord_cartesian(ylim=c(0,NA))
#                         
#                         png(file = "output/figs/intinv/inf.ts.S.1996.png",
#                             width=12*ppi, height=6*ppi, res=ppi)
#                         print(S);
#                         dev.off();
#                         rm(S)
#                         
#                         ## shannon Incl Nematodes, Copepods, Ostracods
#                         H <- ggplot(data = df, aes(y = H, x = year, fill = zone1))+
#                           geom_hline(yintercept = mean(df$H,na.rm = TRUE),colour="grey",linetype="dashed")+
#                           geom_hline(yintercept = min(df$H,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_hline(yintercept = max(df$H,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_boxplot(aes(group=year))+
#                           geom_smooth(method = "loess", colour = "red", span = .9)+
#                           facet_grid(shore~zone1)+
#                           scale_colour_manual(name = "", values=cbPalette)+
#                           scale_fill_manual(name = "", values=cbPalette)+
#                           xlab("Year") + ylab(bquote("Shannon diversity"))+
#                           theme(legend.position="none",
#                                 strip.text.x = element_text(size = 12),
#                                 strip.text.y = element_text(size = 12),
#                                 axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
#                           scale_x_continuous(breaks = seq(2007, 2021, by = 2))+
#                           coord_cartesian(ylim=c(0,NA))
#                         
#                         png(file = "output/figs/intinv/inf.ts.H.png",
#                             width=12*ppi, height=6*ppi, res=ppi)
#                         print(H);
#                         dev.off();
#                         rm(H)
#                         
#                         ## Simpson Incl Nematodes, Copepods, Ostracods
#                         L <- ggplot(data = df, aes(y = simp, x = year, fill = zone1))+
#                           geom_hline(yintercept = mean(df$simp,na.rm = TRUE),colour="grey",linetype="dashed")+
#                           geom_hline(yintercept = min(df$simp,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_hline(yintercept = max(df$simp,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_boxplot(aes(group=year))+
#                           geom_smooth(method = "loess", colour = "red", span = .9)+
#                           facet_grid(shore~zone1)+
#                           scale_colour_manual(name = "", values=cbPalette)+
#                           scale_fill_manual(name = "", values=cbPalette)+
#                           xlab("Year") + ylab(bquote("Simpson's index"))+
#                           theme(legend.position="none",
#                                 strip.text.x = element_text(size = 12),
#                                 strip.text.y = element_text(size = 12),
#                                 axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
#                           scale_x_continuous(breaks = seq(2007, 2021, by = 2))+
#                           coord_cartesian(ylim=c(0,NA))
#                         
#                         png(file = "output/figs/intinv/inf.ts.Lam.png",
#                             width=12*ppi, height=6*ppi, res=ppi)
#                         print(L);
#                         dev.off();
#                         rm(L)
#                         
#                         ## Pielou Incl Nematodes, Copepods, Ostracods
#                         J <- ggplot(data = df, aes(y = J, x = year, fill = zone1))+
#                           geom_hline(yintercept = mean(df$J,na.rm = TRUE),colour="grey",linetype="dashed")+
#                           geom_hline(yintercept = min(df$J,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_hline(yintercept = max(df$J,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_boxplot(aes(group=year))+
#                           geom_smooth(method = "loess", colour = "red", span = .9)+
#                           facet_grid(shore~zone1)+
#                           scale_colour_manual(name = "", values=cbPalette)+
#                           scale_fill_manual(name = "", values=cbPalette)+
#                           xlab("Year") + ylab(bquote("Pielou's evenness"))+
#                           theme(legend.position="none",
#                                 strip.text.x = element_text(size = 12),
#                                 strip.text.y = element_text(size = 12),
#                                 axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
#                           scale_x_continuous(breaks = seq(2007, 2021, by = 2))+
#                           coord_cartesian(ylim=c(0,NA))
#                         
#                         png(file = "output/figs/intinv/inf.ts.J.png",
#                             width=12*ppi, height=6*ppi, res=ppi)
#                         print(J);
#                         dev.off();
#                         rm(J)
#                         
#                         ## Margalef's Incl Nematodes, Copepods, Ostracods
#                         d <- ggplot(data = df, aes(y = d, x = year, fill = zone1))+
#                           geom_hline(yintercept = mean(df$d,na.rm = TRUE),colour="grey",linetype="dashed")+
#                           geom_hline(yintercept = min(df$d,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_hline(yintercept = max(df$d,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_boxplot(aes(group=year))+
#                           geom_smooth(method = "loess", colour = "red", span = .9)+
#                           facet_grid(shore~zone1)+
#                           scale_colour_manual(name = "", values=cbPalette)+
#                           scale_fill_manual(name = "", values=cbPalette)+
#                           xlab("Year") + ylab(bquote("Margalef's richness"))+
#                           theme(legend.position="none",
#                                 strip.text.x = element_text(size = 12),
#                                 strip.text.y = element_text(size = 12),
#                                 axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
#                           scale_x_continuous(breaks = seq(2007, 2021, by = 2))+
#                           coord_cartesian(ylim=c(0,NA))
#                         
#                         png(file = "output/figs/intinv/inf.ts.d.png",
#                             width=12*ppi, height=6*ppi, res=ppi)
#                         print(d);
#                         dev.off();
#                         rm(d)
#                         
#                         ## Biomass Incl Nematodes, Copepods, Ostracods
#                         df$biom_m2 <- df$biom/df$core_area_m2
#                         
#                         d <- ggplot(data = df, aes(y = biom, x = year, fill = zone1))+
#                           geom_hline(yintercept = mean(df$biom,na.rm = TRUE),colour="grey",linetype="dashed")+
#                           geom_hline(yintercept = min(df$biom,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_hline(yintercept = max(df$biom,na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_boxplot(aes(group=year))+
#                           geom_smooth(method = "loess", colour = "red", span = .9)+
#                           facet_grid(shore~zone1)+
#                           scale_colour_manual(name = "", values=cbPalette)+
#                           scale_fill_manual(name = "", values=cbPalette)+
#                           xlab("Year") + ylab(bquote("Biomass (g/m2)"))+
#                           theme(legend.position="none",
#                                 strip.text.x = element_text(size = 12),
#                                 strip.text.y = element_text(size = 12),
#                                 axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))+
#                           scale_x_continuous(breaks = seq(2007, 2021, by = 2))+
#                           coord_cartesian(ylim=c(0,NA))
#                         
#                         png(file = "output/figs/intinv/inf.ts.biomass.png",
#                             width=12*ppi, height=6*ppi, res=ppi)
#                         print(d);
#                         dev.off();
#                         rm(d)
#                         
#                         ##log biomass_m2
#                         d <- ggplot(data = df, aes(y = log(biom_m2+1), x = year, fill = zone1))+
#                           geom_hline(yintercept = mean(log(df$biom_m2+1),na.rm = TRUE),colour="grey",linetype="dashed")+
#                           geom_hline(yintercept = min(log(df$biom_m2+1),na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_hline(yintercept = max(log(df$biom_m2+1),na.rm = TRUE),colour="grey",linetype="dotted")+
#                           geom_boxplot(aes(group=year))+
#                           geom_smooth(method = "loess", colour = "red", span = .9)+
#                           facet_grid(shore~zone1)+
#                           scale_colour_manual(name = "", values=cbPalette)+
#                           scale_fill_manual(name = "", values=cbPalette)+
#                           xlab("Year") + ylab(bquote("log(Biomass)"))+
#                           scale_x_continuous(breaks = seq(2007, 2021, by = 2))+
#                           theme(legend.position="none",
#                                 strip.text.x = element_text(size = 12),
#                                 strip.text.y = element_text(size = 12),
#                                 axis.text.x = element_text(angle = 270,hjust=1,vjust=0.5))
#                         
#                         png(file = "output/figs/intinv/inf.ts.logbiomass_m2.png",
#                             width=12*ppi, height=6*ppi, res=ppi)
#                         print(d);
#                         dev.off();
#                         rm(d)
#                         
#                         # current year plots ####
#                         table(cur.yr == df_abund$year)###sense check we have correct data
#                         dfcur <- df_abund %>% ### rename and remove excess names for plotting
#                           select(.,1:8,(ncol(.)-7):ncol(.)) %>% 
#                           filter(.,mesh=="1.0mm") %>% 
#                           filter(.,transect != "WA1")
#                         
#                         dfcur$transect <- factor(dfcur$transect,levels=c("T1N","T1","T1S",
#                                                                          "T4","T7","T8",
#                                                                          "T11","T12","T13",
#                                                                          "T15","T17","T20",
#                                                                          "T21","T22","T23",
#                                                                          "T24","T25","T26"))
#                         dfcur$shore <- factor(dfcur$shore, levels=c("Mid","Low"))
#                         dfcur$zone1 <- factor(dfcur$zone1,levels=c("Above","Inside","Inside2","Below"))
#                         
#                         #taxon richness
#                         S <- ggplot(data = dfcur,
#                                     aes(x = transect, y = S,
#                                         fill = zone1, color = "black"))+
#                           geom_bar(stat = "identity", colour = "black")+
#                           facet_wrap(~ shore, ncol = 1)+
#                           theme(legend.position="none",strip.text.x = element_text(size = 12))+
#                           scale_x_discrete(breaks = NULL)+
#                           ylab("Taxon richness") + xlab("")+
#                           scale_fill_manual(values=cbPalette)+
#                           labs(fill = "")+
#                           labs(subtitle = "Taxon richness")
#                         
#                         #Faunal density
#                         N <- ggplot(data = dfcur,
#                                     aes(x = transect, y = N,
#                                         fill = zone1, color = "black"))+
#                           geom_bar(stat = "identity", colour = "black")+
#                           facet_wrap(~ shore, ncol = 1)+
#                           theme(legend.position="none",strip.text.x = element_text(size = 12))+
#                           scale_x_discrete(breaks=NULL)+
#                           ylab("Faunal density") + xlab("")+
#                           scale_fill_manual(values=cbPalette)+
#                           labs(fill="")+
#                           labs(subtitle = "Taxon density")
#                         
#                         # Logged faunal density
#                         lN <- ggplot(data = dfcur,
#                                      aes(x = transect, y = log(N),
#                                          fill = zone1, color = "black"))+
#                           geom_bar(stat = "identity", colour = "black")+
#                           facet_wrap(~ shore, ncol = 1)+
#                           theme(legend.position="none",strip.text.x = element_text(size = 12))+
#                           scale_x_discrete(breaks=NULL)+
#                           ylab("Log(faunal density)") + xlab("")+
#                           scale_fill_manual(values=cbPalette)+
#                           labs(fill="")+
#                           labs(subtitle = "Taxon density (log)")
#                         
#                         #Faunal Biomass
#                         B <- ggplot(data = dfcur,
#                                     aes(x = transect, y = biom,
#                                         fill = zone1, color = "black"))+
#                           geom_bar(stat = "identity", colour = "black")+
#                           facet_wrap(~ shore, ncol = 1)+
#                           theme(legend.position="none",strip.text.x = element_text(size = 12))+
#                           scale_x_discrete(breaks=NULL)+
#                           ylab("Faunal biomass") + xlab("")+
#                           scale_fill_manual(values=cbPalette)+
#                           labs(fill="")+
#                           labs(subtitle = "Faunal biomass")
#                         
#                         #Logged faunal Biomass
#                         lB <- ggplot(data = dfcur,
#                                      aes(x = transect, y = log1p(biom),
#                                          fill = zone1, color = "black"))+
#                           geom_bar(stat = "identity", colour = "black")+
#                           facet_wrap(~ shore, ncol = 1)+
#                           theme(legend.position="none",strip.text.x = element_text(size = 12))+
#                           scale_x_discrete(breaks=NULL)+
#                           ylab("Log faunal biomass") + xlab("")+
#                           scale_fill_manual(values=cbPalette)+
#                           labs(fill="")+
#                           labs(subtitle = "Faunal biomass (log)")
#                         
#                         #shannon
#                         H <- ggplot(data = dfcur,
#                                     aes(x = transect, y = H,
#                                         fill = zone1, color = "black"))+
#                           geom_bar(stat = "identity", colour = "black")+
#                           facet_wrap(~ shore, ncol = 1)+
#                           theme(legend.position="none",strip.text.x = element_text(size = 12))+
#                           scale_x_discrete(breaks=NULL)+
#                           ylab("Shannon diversity") + xlab("")+
#                           scale_fill_manual(values=cbPalette)+
#                           labs(fill="")+
#                           labs(subtitle = "Shannon's H")
#                         
#                         #Pielou
#                         J <- ggplot(data = dfcur,
#                                     aes(x = transect, y = J,
#                                         fill = zone1, color = "black"))+
#                           geom_bar(stat = "identity", colour = "black")+
#                           facet_wrap(~ shore, ncol = 1)+
#                           theme(legend.position="none",strip.text.x = element_text(size = 12),
#                                 axis.text.x  = element_text(angle=90, vjust=0.5, size = 12))+
#                           scale_y_continuous(breaks=c(0,0.5, 1))+
#                           ylab("Pielou's evenness") + xlab("")+
#                           scale_fill_manual(values=cbPalette)+
#                           labs(subtitle = "Pielou's J")
#                         
#                         #Margalef
#                         d <- ggplot(data = dfcur,
#                                     aes(x = transect, y = d,
#                                         fill = zone1, color = "black"))+
#                           geom_bar(stat = "identity", colour = "black")+
#                           facet_wrap(~ shore, ncol = 1)+
#                           theme(legend.title=element_blank(),legend.direction="horizontal",
#                                 legend.position = c(.75, 1.3),
#                                 strip.text.x = element_text(size = 12),
#                                 axis.text.x  = element_text(angle=90,
#                                                             vjust=0.5,
#                                                             size = 12)
#                           )+
#                           #scale_y_continuous(breaks=c(0,0.5, 1))+
#                           ylab("Margalef's d") + xlab("")+
#                           scale_fill_manual(values=cbPalette)+
#                           labs(fill="")+
#                           labs(subtitle = "Margalef's d")+
#                           guides(fill=guide_legend(nrow=2, byrow=TRUE))
#                         
#                         ## COMBINE plots into single chart
#                         x <- (lN|lB)/
#                           (S|H)/
#                           (J|d)
#                         
#                         png(file = "output/figs/intinv/inf.2022.div.png",
#                             width = 12 * ppi, height = 10 * ppi, res = ppi)
#                         x+plot_annotation(tag_levels = "A")
#                         dev.off();
#                         rm(J,lB,d,lB,B,S,H,lN,N)
#                         
#                         # Tidy up ####
#                         rm(list = ls(pattern = "^df"))
#                         rm(cbPalette, ppi,tmz,perm,cur.yr,x,libfolder)
#                         
#                         detach("package:tidyverse", unload=TRUE)
#                         detach("package:lmerTest", unload=TRUE)
#                         detach("package:vegan", unload=TRUE)
#                         detach("package:patchwork", unload=TRUE)
#                         
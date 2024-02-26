### 200_an.epi.cra.R ####
### import and analyse Crangon length data

# Set up ####
## set local library ####

## load data & format ####
source("R/100_imp.epi0.R")

#### load packages ####
ld_pkgs <- c("tidyverse","lmerTest", "ggridges")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

#### set metadata & universals ####
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

###import & format data ####
df <- df_cra_ts[df_cra_ts$year == cur.yr,] ### current year only
df <- df[!is.na(as.numeric(as.character(df$len.mm))),]### remove non-numeric
df$len.mm <- as.numeric(df$len.mm)

### factorise zone1: comparing all with Inside
df$zone1 <- factor(df$zone1, levels = c("Inside","Above","Inside2",
                                        "Below"))

### descriptives ####
### min max Crangon abundance ####
dfw$Crangon_all <- dfw$Crangon+dfw$`Crangon crangon`
dfwcur <- dfw[dfw$year==cur.yr,]
#reorder factors for comparison
dfwcur$zone1 <- factor(dfwcur$zone1, levels = c("Inside","Above","Inside2","Below"))

dfwcur %>% 
  dplyr::select(.,c(1:5,Crangon_all)) %>% 
  filter(.,mon=="Oct") %>% 
  View(.)

# View(dfwcur[,c(1:5, ncol(dfwcur))])
plot(performance::check_distribution(dfwcur$Crangon_all)) # NegBin
# car::Anova(mod01<-lmer(Crangon_all ~ zone1 +
#                          (1|depth) +
#                          (1|mon)
#                        ,
#                 data=dfwcur))
# performance::check_model(mod01)

car::Anova(mod02<-glmer.nb(Crangon_all ~ zone1 +
                   # (1|depth) +
                   (1|mon)
                 ,
                 data=dfwcur))
summary(mod02)
visreg::visreg(mod02)
performance::check_model(mod02)

### summary data: mean length
mean(df$len.mm);sd(df$len.mm)

### adults vs juves
table(df$ad.ju)
print(paste0(round(length(df$ad.ju[df$ad.ju=="Adult"])/length(df$ad.ju)*100,2),"% Adults"))#;ad.per;rm(ad.per)
print(paste0(round(length(df$ad.ju[df$ad.ju=="Juv"])/length(df$ad.ju)*100,2),"% Juveniles"))#;ju.per;rm(ju.per)

### COMPARE LENGTHS BETWEEN SURVEYS ####
## reorganise factors
dd <- df
dd$zone1 <- factor(dd$zone1,levels=c("Inside","Above","Inside2","Below"))
dd$mon <- factor(dd$mon,levels=c("Sep","Oct"))

summary(m1 <- aov(len.mm ~ mon,data=dd))
visreg::visreg(m1)

### difference looks small.  What are the means?
df %>% 
  group_by(.,mon) %>% 
  summarise(mn.ln=mean(len.mm),sd.ln=sd(len.mm))
effsize::cohen.d(df$len.mm,df$mon)## Cohen's d indicates 'large' effect of survey month
rm(m1)

### COMPARE LENGTHS BETWEEN ZONES ####
## reorganise factors
summary(m2 <- lmerTest::lmer(len.mm ~ zone1+(1|mon)+(1|site),data=dd))
visreg::visreg(m2)
anova(m2)

d <- as.data.frame(ls_means(m2, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]

rm(m2)

# plot abundances for current year
dfcra <- dfw[dfw$year==cur.yr,]

dfcra$zone1 <- factor(dfcra$zone1, levels=c("Above","Inside","Inside2","Below"))
dfcra$transect <- factor(dfcra$transect,levels=c("T1","T4","T8","T13","T20"))
dfcra$trmon <- paste0(dfcra$transect,".",dfcra$mon)
dfcra$trmon <- factor(dfcra$trmon, levels=c(
  "T1.Sep","T1.Oct","T4.Sep","T4.Oct","T8.Sep","T8.Oct","T13.Sep","T13.Oct",
  "T20.Sep","T20.Oct"))

png(file = "output/figs/epi.cra.abnd.cur.png",
    width=12*ppi, height=6*ppi, res=ppi)
dfcra %>% 
  filter(.,year==cur.yr) %>% 
  ggplot(.,aes(x=trmon,y=log(Crangon_all+1), fill=zone1))+
  geom_bar(stat = "identity",
           colour=1)+
  facet_wrap(.~depth)+
  scale_fill_manual(values=cbPalette)+
  ylab("Log(Crangon abundance+1)")+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(face="bold"),
        axis.title.y = element_text(face="bold"),
        strip.text.x = element_text(size = 12),
        axis.text.x = element_text(
          angle = 90,
          hjust=1,
          vjust=0.5))
dev.off()
rm(dfcra)

### Crangon time series ####
## fix factors
dfw$zone1 <- factor(dfw$zone1, levels = c("Above","Inside","Inside2","Below"))
##standardise months
dfw$month <- ifelse(dfw$mon == "Oct.1", "Oct",
  ifelse(dfw$mon == "Oct.2", "Oct",
         ifelse(dfw$mon == "Oct", "Oct",
         ifelse(dfw$mon == "October", "Oct",
                ifelse(dfw$mon == "Dec", "Dec",
                       ifelse(dfw$mon == "Sep", "Sep",
                              ifelse(dfw$mon == "Nov", "Nov",
                ifelse(dfw$mon == "September", "Sep","Oct"))))))))
## this replaces unrecorded sampling months with the value "Oct"

#create date variable
dfw$day <- ifelse(dfw$mon=="Oct.2",15,1)
dfw$monNum <- match(dfw$month,month.abb)

dfw$month <- factor(dfw$month,
                    levels=c("Sep","Oct","Nov","Dec","NA"))
dfw$date <- as.Date(paste(dfw$day,match(dfw$month,month.abb),dfw$year,
                          sep = "/"),
                    format = "%d/%m/%Y")

png(file = "output/figs/epi.cra.abnd.ts.png",
    width=12*ppi, height=6*ppi, res=ppi)
dfw %>% 
  filter(.,year<cur.yr) %>%
  ggplot(.,aes(x=date,
               y=log(Crangon_all+1)))+
  geom_point()+
  facet_grid(depth~zone1)+
  geom_smooth(
    # method="gam",
    method="loess", span=.99,
    aes(colour=zone1),
    show.legend = FALSE)+
  scale_colour_manual(values=cbPalette)+
  ylab("Log(Crangon abundance+1)")+
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(face="bold",size = 12),
        axis.title.y = element_text(face="bold"))
dev.off()

###############################################################################
###############################################################################
###############################################################################
###############################################################################
###############################################################################

### Compare length vs zone ####

df$zone1 <- factor(df$zone1,levels=c("Above","Inside","Inside2","Below"))

### plot carapaces - current ####
png(file = "output/figs/epi.shr.len.2023.png",
    width=12*ppi, height=6*ppi, res=ppi)
i <- ggplot(data=df, aes(x = zone1, y=len.mm, fill = zone1))+
  # geom_violin(show.legend=FALSE)+
  geom_boxplot(outlier.colour = NA,show.legend = FALSE,varwidth = TRUE)+
  geom_jitter(height = 0, width = 0.25, alpha = 0.25,show.legend = FALSE)+
  theme(legend.title=element_blank())+
  scale_fill_manual(values=cbPalette)+
  theme(legend.title=element_blank())+
  facet_wrap(~site)+
  xlab("")+ylab("Carapace length (mm)")+ylim(0,NA) +
  theme(legend.title = element_blank(),
        axis.title.x = element_blank(),
        strip.text = element_text(face="bold",size = 12),
        axis.title.y = element_text(face="bold"))
# coord_flip()
print(i)
dev.off()

pdf("output/figs/epi.shr.len.2023.pdf", width=14,height = 8)
print(i)
dev.off();rm(i)


### plot carapaces - TS ####
# ggplot(data=df_cra_ts,aes(x=len.mm,y=as.factor(year)))+
#   facet_grid(zone1~site)+
#   geom_density_ridges(scale=4, alpha=0.7)+
#   scale_y_discrete(limits=rev)+
#   scale_fill_manual(values = cbPalette)
# 
# png(file = "output/figs/epi/epi.shr.len.ts.png",
#     width=12*ppi, height=6*ppi, res=ppi)
# i <- ggplot(data=df_cra_ts, aes(x = year, y=len.mm, fill = zone1))+
#   # geom_violin(show.legend=FALSE)+
#   
#   
#   
#   geom_boxplot(outlier.colour = NA,show.legend = FALSE,varwidth = TRUE)+
#   geom_jitter(height = 0, width = 0.25, alpha = 0.25,show.legend = FALSE)+
#   theme(legend.title=element_blank())+
#   scale_fill_manual(values=cbPalette)+
#   theme(legend.title=element_blank())+
#   facet_wrap(~site)+
#   xlab("")+ylab("Carapace length (mm)")+ylim(0,NA)#+
# # coord_flip()
# print(i)
# dev.off()
# 
# pdf("output/figs/epi/epi.shr.len.2022.pdf", width=14,height = 8)
# print(i)
# dev.off();rm(i)

############################
#### continue from here ####
############################
#### COMPARE BIOMASS BETWEEN SURVEYS ####
### WEIGHTS ####
mean(df$wt.g)
se(df$wt.g)
sd(df$wt.g)

wt_mod <- lmer(wt.g ~ zone1 + (1|site) + (1|mon), data=dd)
anova(wt_mod)
summary(wt_mod)
d <- as.data.frame(ls_means(wt_mod, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
sjPlot::plot_model(wt_mod,show.values=TRUE, show.p=TRUE)
rm(wt_mod,d)

# m1 <- aov(wt.g ~ mon*zone1*site, data=df);summary(m1)
# cra.tuk <- TukeyHSD(aov(m1))
# a0 <- cra.tuk$mon
# a <- cra.tuk$`zone1:site`
# a <- as.data.frame(a)
# a <- a[!is.na(a$`p adj`),]
# b <- a[order(a$`p adj`),]
# b$p.adj <- b$"p adj";b$"p adj" <- NULL
# b <- subset(b, p.adj<0.05);rm(a,cra.tuk)
# b
# # rm(m1,a0,b)
# summary(m1 <- aov(wt.g ~ mon, df))
# rm(m1)
# 
# ### working! ####
# mod2 <- lmer(wt.g ~ zone1 + (1|site) + (1|mon),
#              data=df)
# summary(mod2)
# anova(mod2)
# d <- as.data.frame(ls_means(mod2, test.effs = "Group",pairwise = TRUE))
# d[d$`Pr(>|t|)`<0.051,]
# rm(mod2,d)
# 
# #### Append 2020 data to historic ####
# df.20 <- df; rm(dd)
# df.20$year <- 2020# add year variable
# dfold <- readRDS("data/base/historic/epi.cra.ts_19.rds")###load historic data
# df.20$spp <- df.20$Species;df.20$Species <- NULL
# df.20$survey <- ifelse(df.20$mon == "Sep",1,
#                        ifelse(df.20$mon == "Oct",2,NA))
# ### reorder columns
# # match: year transect site zone1 survey mon spp len.mm wt.g ad.ju
# df.nu <- df.20[,c(9,1,2,7,11,5,10,3,4,8)]
# 
# ### join data & save
# df.cra <- bind_rows(dfold,df.nu)
# 
# saveRDS(df.cra, file = "data/processed/R/epi.cra.ts_20.rds")### rds-local version
# saveRDS(df.cra, file = "output/data/data_exports/epi.cra.ts_20.rds")### rds-for export
# write.csv(df.cra, file = "data/processed/csv/epi.cra.ts_20.csv",row.names=FALSE)### csv-local version
# write.csv(df.cra, file = "output/data/data_exports/epi.cra.ts_20.csv",row.names=FALSE)### csv-for export

# Pandalus ####
dfw %>% 
  select(.,1:5,starts_with("Pandalus")) %>% 
  filter(.,year==cur.yr) -> df.pan

mean(df.pan$`Pandalus montagui`);sd(df.pan$`Pandalus montagui`)
pan_mod <- lmer(`Pandalus montagui` ~ zone1 + (1|depth) + (1|mon), data=df.pan)
pan_mod <- lm(`Pandalus montagui` ~ zone1*depth*mon, data=df.pan)
anova(pan_mod)
summary(pan_mod)
d <- as.data.frame(ls_means(pan_mod, test.effs = "Group",pairwise = TRUE))
d[d$`Pr(>|t|)`<0.051,]
sjPlot::plot_model(wt_mod,show.values=TRUE, show.p=TRUE)
rm(wt_mod,d)



#### tidy up ####
detach("package:tidyverse", unload=TRUE)
detach("package:lmerTest", unload=TRUE)
detach("package:lme4", unload=TRUE)
detach("package:ggthemes", unload=TRUE)
rm(df,dd,df_cra_ts,i,cbPalette,ppi,se,cur_yr)

# an.sed.phys.R ####
### import & analyse cone penetrometer and beach angle data

# Set up ####
### load packages ####
ld_pkgs <- c("tidyverse","ggthemes","lmerTest","effects",
             "sjPlot","visreg")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### load metadata ####
source("R/00_meta_setMeta.R")

## import & format data ####
df <- as_tibble(read.csv(paste0(fol,"sed.phys.ts.csv"),
                         stringsAsFactors = FALSE))

##order factors
df$transect <- factor(df$transect,
                      levels =
                        c("T1N","T1","T1S","T4", "T7", "T8",
                          "T11", "T12", "T13", "T15", "T17",
                          "T24","T25","T26",
                          "T20", "T21","T22", "T23","WA1"))
df$zone1 <- factor(df$zone1,
                   levels = c("Above", "Inside","Inside2","Below"))

df$shore <- factor(df$shore,
                   levels=c("Upper","Mid","Low","Surf"))

cur_dat <- subset(df, year == cur.yr)

##=============###
# Wave type   ####
##=============###
subset(cur_dat, type == "WaveClass")## pull out angle data

##=============###
# Beach Angle  ####
##=============###
ang_cu <- subset(cur_dat, type == "angle")## pull out angle data
ang_cu$value <- as.numeric(ang_cu$value)

png(file = paste0("figs/sed.",cur.yr,".sed.mor.angle.png"),
    width=12*ppi, height=6*ppi, res=ppi)
set.seed(pi); ggplot(data = ang_cu, aes(y = value, x = zone1,
                                    fill=zone1))+
  geom_boxplot(outlier.colour=NA)+
  geom_jitter(aes(shape = zone1),
              width = 0.3, height = 0.05,size = 3,stroke = 1,
              show.legend = FALSE)+
  scale_shape_manual(values = c(21:(length(unique(ang_cu$zone1))+21)))+
  labs(title = paste0("Beach slopes recorded in the ",cur.yr," monitoring programme"))+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",#c(.5,1.02),
        legend.direction = "horizontal",
        legend.key.size = unit(1,"cm"),#change legend size!
        legend.title=element_blank(),
        legend.text=element_text(size = 12,
                                 face = "bold"),
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.margin=margin(0,0,0,0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(face="bold")
        )+
  xlab("") + ylab("Beach angle (°)")+
  scale_fill_manual(values=cbPalette)+
  scale_x_discrete(breaks=NULL)+
  facet_wrap(~shore)+
  guides(fill=guide_legend(nrow=1))
dev.off()

## Get summary ####
## mean by zone & shore: current year
(mean_23 <- ang_cu %>%
    group_by(zone1, shore) %>%
    summarise("mean.angle"=mean(value), sd(value)))

## mean by zone: current year
(mean_23_zone <- ang_cu %>%
    group_by(zone1) %>%
    summarise("sdangle"=mean(value), sd(value)))

## mean by zone & shore: all years
(mean_all <- subset(df, type == "angle") %>%
    group_by(zone1, shore) %>%
    summarise("mean.angle"=mean(as.numeric(value), na.rm=TRUE),
              "sd.angle"=sd(as.numeric(value),na.rm=TRUE)))

### mean by station: current year
mn_st_cur <- subset(df, type == "angle") %>%
  filter(.,year == cur.yr) %>% 
  group_by(transect, shore) %>% 
  summarise("mean.angle"=mean(as.numeric(value),na.rm=TRUE),
            "sd.angle"=sd(as.numeric(value),na.rm=TRUE),
            .groups = "drop")
mn_st_cur[which.min(mn_st_cur$mean.angle),]
mn_st_cur[which.max(mn_st_cur$mean.angle),]

print(mean_23); print(mean_all)

df_tm <- df
df_tm$shore <- factor(df_tm$shore, levels=c("Upper","Mid","Low"))
png(file = "figs/sed.ts.mor.angle.png",
    width=12*ppi, height=6*ppi, res=ppi)
ggplot(data = df_tm[df_tm$type=="angle",],
                    aes(y = as.numeric(value), x = year, fill = zone1))+
  geom_boxplot(aes(group=year),outlier.shape = NA)+
  geom_jitter(width = 0.1, height = 0,alpha=0.3)+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  scale_x_continuous(breaks = seq(2008, 2022, by = 2))+
  xlab("Year") + ylab("Angle")+
  theme(plot.title = element_text(face = "bold"),
        legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        strip.text = element_text(face="bold"),
        axis.title = element_text(face="bold")
        )+
  labs(title = paste0("Beach slopes recorded since 2008 as part of the SFGPBM programme"))
dev.off()

## Statistical comparison ####
##re-level to compare all with Inside
ang_cu$zone1 <- factor(ang_cu$zone1,levels=c("Inside","Above","Inside2","Below"))

ang_cur_mod1 <- lmerTest::lmer(value ~ zone1 + (1|shore) , data = ang_cu,REML=FALSE)
ang_cur_mod2 <- lmerTest::lmer(value ~ zone1 + (transect|shore) , data = ang_cu,REML=FALSE)
anova(ang_cur_mod3 <- lmerTest::lmer(value ~ zone1 + (1|transect)+(1|shore) , data = ang_cu,REML=FALSE))
anova(ang_cur_mod3.1 <- lmerTest::lmer(value ~ zone1 + (shore|transect) , data = ang_cu,REML=TRUE))
anova(ang_cur_mod4 <- lmerTest::lmer(value ~ zone1*shore + (1|transect) , data = ang_cu,REML=FALSE))

# model checking: whis is best? Lowest AIC = 'better' fit
anova(ang_cur_mod1, ang_cur_mod2, ang_cur_mod3,ang_cur_mod3.1, ang_cur_mod4)###compare model fits
AIC(ang_cur_mod1,ang_cur_mod2,ang_cur_mod3,ang_cur_mod3.1,ang_cur_mod4)
# ang_cur_mod3.1 has lowest AIC
plot(performance::compare_performance(ang_cur_mod1,
                                 ang_cur_mod2,
                                 ang_cur_mod3,
                                 ang_cur_mod3.1,
                                 ang_cur_mod4,
                                 rank = TRUE))
visreg::visreg(ang_cur_mod3.1)

# go with model ang_cur_mod3.1
anova(ang_cur_mod3.1)
lmerTest::ls_means(ang_cur_mod3.1, test.effs = "Group",pairwise = TRUE)
sjPlot::plot_model(ang_cur_mod3.1,show.values=TRUE, show.p=TRUE)
visreg::visreg(ang_cur_mod3.1)
performance::check_posterior_predictions(ang_cur_mod3.1)

## time series ####
png(file = "output/figs/sed.ang.ts.png",
    width=12*ppi, height=6*ppi, res=ppi)
df %>% 
  filter(.,type=="angle") %>% 
  mutate(value = as.numeric(value)) %>% 
  dplyr::select(.,c(year,shore,type,value,zone1)) %>% 
  ggplot(data=., aes(x=year, y=value, fill=zone1))+
  geom_boxplot(outlier.shape = NA,
               aes(group=as.factor(year)), show.legend = FALSE)+
  geom_jitter(show.legend = FALSE, alpha=0.25)+
  facet_grid(shore~zone1)+
  scale_fill_manual(values=cbPalette)+
  geom_smooth(span=0.9, show.legend = FALSE, col="red",
              # method = "gam"
              method = "loess"
              )+
  ylab("Beach angle (°)")+
  theme(axis.title.x = element_blank(),
  axis.text.y = element_text(size = 12),
  axis.title.y = element_text(size = 14),
  strip.text.x = element_text(size = 12),
  strip.text.y = element_text(size = 12),
  strip.text = element_text(face="bold"))
dev.off()

###=============###
# Compaction ####
###=============###
com_cu <- subset(cur_dat, type == "cone")## pull out angle data
com_cu$value <- as.numeric(com_cu$value)

(com_cu_sum <- com_cu %>% 
    group_by(.,zone1) %>% 
    summarise(mean_cu=mean(value,na.rm=TRUE),sd.cu=sd(value,na.rm=TRUE)) %>% 
    ungroup())
com_cu_st <- com_cu %>% 
  filter(.,year==cur.yr) %>% 
  group_by(transect, shore) %>% 
  summarise("mean.cone"=mean(as.numeric(value),na.rm=TRUE),
            "sd.cone"=sd(as.numeric(value),na.rm=TRUE),
            .groups = "drop")
com_cu_st[which.min(com_cu_st$mean.cone),]
com_cu_st[which.max(com_cu_st$mean.cone),]

### boxplot ###
png(file = paste0("figs/sed.",cur.yr,".sed.mor.pen.png"),
    width=12*ppi, height=6*ppi, res=ppi)
set.seed(pi); ggplot(data = com_cu, aes(y = value, x = zone1,
                                 fill=zone1))+
  geom_boxplot(outlier.colour=NA)+
  geom_jitter(aes(shape = zone1),
              width = 0.3, height = 0.05,size = 3,stroke = 1,
              show.legend = FALSE)+
  theme(plot.title = element_text(face = "bold"),
        legend.position = "bottom",#c(.5,1.02),
        legend.direction = "horizontal",
        # legend.box = "horizontal",
        legend.key.size = unit(1,"cm"),#change legend size!
        legend.title=element_blank(),
        legend.text=element_text(size = 12,
                                 face = "bold"),
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.margin=margin(0,0,0,0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text = element_text(face="bold")
  )+
  xlab("") + ylab("Cone index")+
  scale_fill_manual(values=cbPalette)+
  scale_x_discrete(breaks=NULL)+
  scale_shape_manual(values = c(21:(length(unique(com_cu$zone1))+21)))+
  facet_wrap(~shore)+
  guides(fill=guide_legend(nrow=1))+
  labs(title = paste0("Sediment compaction recorded in the ",cur.yr," monitoring programme"))
dev.off()

## Get summary ####
#require(dplyr)
(mean_cur <- com_cu %>%
    group_by(zone1, shore) %>%
    summarise(mean(value,na.rm=TRUE), sd(value,na.rm=TRUE)))

(mean_cur_zone <- com_cu %>%
    group_by(zone1) %>%
    summarise(mean(value,na.rm=TRUE), sd(value,na.rm=TRUE)))

(mean_all <- subset(df, type == "cone") %>%
    group_by(zone1, shore) %>%
    summarise(mean(as.numeric(value),na.rm=TRUE),
              sd(as.numeric(value),na.rm=TRUE)))

(mean_cur_tran <- com_cu %>%
    group_by(transect,shore) %>%
    summarise(mean(value,na.rm=TRUE), sd(value,na.rm=TRUE)))

### mean by station
mn_st_cur <- subset(df, type == "cone") %>%
  group_by(transect, shore) %>% 
  summarise(mean(as.numeric(value),na.rm=TRUE),
            sd(as.numeric(value),na.rm=TRUE),
            .groups = "drop")

## Time series ####
df_tm <- df
df_tm$shore <- factor(df_tm$shore, levels=c("Upper","Mid","Low"))

png(file = "figs/sed.ts.mor.pen.png",
    width=12*ppi, height=6*ppi, res=ppi)
ggplot(data = df_tm[df_tm$type=="cone",],
                 aes(y = as.numeric(value), x = year, fill = zone1))+
  geom_boxplot(aes(group=year),outlier.shape = NA)+
  geom_jitter(width = 0.1, height = 0,alpha=0.3)+
  geom_smooth(method = "loess", colour = "red", span = .9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  scale_x_continuous(breaks = seq(2008, 2022, by = 2))+
  xlab("Year") + ylab("Cone index")+
  theme(plot.title = element_text(face = "bold"),
        legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        strip.text = element_text(face="bold"),
        axis.title = element_text(face="bold"))+
  labs(title = paste0("Sediment compaction recorded since 2008 as part of the SFGPBM programme"))
dev.off()

## Statistical comparison ####
##re-level to compare all with Inside
com_cu$zone1 <- factor(com_cu$zone1,levels=c("Inside","Above","Inside2","Below"))

com_cur_mod1 <- lmerTest::lmer(value ~ zone1 + (1|shore) , data = com_cu,REML=FALSE)
com_cur_mod2 <- lmerTest::lmer(value ~ zone1 + (transect|shore) , data = com_cu,REML=FALSE)
anova(com_cur_mod3 <- lmerTest::lmer(value ~ zone1 + (1|transect) + (1|shore) , data = com_cu,REML=FALSE))
anova(com_cur_mod3.1 <- lmerTest::lmer(value ~ zone1 + (shore|transect) , data = com_cu,REML=TRUE))
anova(com_cur_mod3.2 <- lmerTest::lmer(value ~ zone1*shore + (shore|transect) , data = com_cu,REML=TRUE))
anova(com_cur_mod4 <- lmerTest::lmer(value ~ zone1*shore + (1|transect), data = com_cu,REML=FALSE))

anova(com_cur_mod1,com_cur_mod2,com_cur_mod3,com_cur_mod3.1,com_cur_mod4)
AIC(com_cur_mod1,com_cur_mod2,com_cur_mod3,com_cur_mod3.1,com_cur_mod4)
### Lowest AIC = com_cur_mod3.1

summary(com_cur_mod3.1)
print(com_cur_mod3.1,cor=FALSE)
anova(com_cur_mod3.1)
lmerTest::ls_means(com_cur_mod3.1, test.effs = "Group",pairwise = TRUE)
sjPlot::plot_model(com_cur_mod3.1,show.values=TRUE, show.p=TRUE)
visreg::visreg(com_cur_mod3.1)
performance::check_posterior_predictions(com_cur_mod3.1)
ggplot(data=com_cu, aes(x=zone1,y=value,colour=transect))+
  # geom_point()+facet_wrap(.~shore)+
  geom_jitter(width = 0.2)+
  facet_wrap(.~shore)

## time series ####
png(file = "output/figs/sed.cone.ts.png",
    width=12*ppi, height=6*ppi, res=ppi)
df %>% 
  filter(.,type=="cone") %>% 
  mutate(value = as.numeric(value)) %>% 
  dplyr::select(.,c(year,shore,type,value,zone1)) %>% 
  ggplot(data=., aes(x=year, y=value, fill=zone1))+
  geom_boxplot(outlier.shape = NA,
               aes(group=as.factor(year)), show.legend = FALSE)+
  geom_jitter(show.legend = FALSE, alpha=0.25)+
  facet_grid(shore~zone1)+
  scale_fill_manual(values=cbPalette)+
  geom_smooth(span=0.9, show.legend = FALSE, col="red",
              # method = "gam"
              method = "loess"
  )+
  ylab("Cone index")+
  theme(axis.title.x = element_blank(),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(size = 14),
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12),
        strip.text = element_text(face="bold"))
dev.off()

# Wave Class #### not interesting!
# df %>% filter(.,type == "WaveClass") -> df_wave
# plot(df_wave$value)

# create summary table for appendices ####

glimpse(cur_dat)

# smry$value <- as.numeric(smry$value)

# smry <- smry %>% 
#   select(.,transect,shore,type,value,zone1) %>%
#   group_by(transect,shore,type,zone1) %>% 
#   summarise(mean=mean(value,na.rm=TRUE),sd=sd(value,na.rm=TRUE)) %>% ungroup()
# 
# smry$ID <- paste0(smry$transect,".",smry$shore)
# 
# smry.cone <- smry %>% 
#   filter(.,type=="cone") %>% ungroup() %>% 
#   rename(., "Mean cone" = mean, "SD cone"=sd)
# smry.ang <- smry %>% 
#   filter(.,type=="angle") %>% 
#   select(.,-(transect:zone1)) %>% ungroup() %>% 
#   rename(., "Mean angle" = mean, "SD angle"=sd)
# 
# oo <- left_join(smry.cone,smry.ang,by="ID")
# write.csv(oo,file="output/forDoc/2021.sed.bulk.csv",row.names = FALSE)

#### tidy up ####
### Unload data
rm(list = ls(pattern = "^ang"))
rm(list = ls(pattern = "^com"))
rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^mean_"))
rm(list = ls(pattern = "^cur_"))
rm(list = ls(pattern = "^pl_"))
rm(cbPalette, ppi,mn_st_cur, cbPaletteTxt, cur.yr,histdatfol, perm)

### Unload packages
detach("package:tidyverse", unload=TRUE)
detach("package:effects", unload=TRUE)
# detach("package:flexplot", unload=TRUE)
detach("package:ggthemes", unload=TRUE)
detach("package:lmerTest", unload=TRUE)
detach("package:sjPlot", unload=TRUE)
detach("package:visreg", unload=TRUE)

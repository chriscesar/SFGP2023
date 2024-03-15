### an.sed.bulk.cur.R ####
## produce plots of bulk parameters for current year

### load packages ####
ld_pkgs <- c("tidyverse","ggplot2","patchwork")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### set meta ####
source("R/00_meta_setMeta.R")

### load & format data ####
df0 <- as_tibble(openxlsx::read.xlsx(paste0(fol,"sed.psa.bulkWIP_use.xlsx"),
                                     sheet="sed.bulk.ts.out"))
### format factors####
df0$transect <- factor(df0$transect,levels=c("T1N","T1","T1S",
                                              "T4","T11","T7","T8","T12","T13",
                                              "T15", "T17", "T20", "T21", "T22",
                                              "T23", "T24","T25","T26"))
                                              # "WA1"))

df0$shore <- factor(df0$shore,levels=c("Upper","Mid","Low","Surf"))
df0$zone1 <- factor(df0$zone1,levels=c("Above","Inside","Inside2","Below"))

df0$MEAN_folkWard_desc <- factor(df0$MEAN_folkWard_desc,
                                 levels=c("Fine Gravel",
                                          "Very Fine Gravel",
                                          "Very Coarse Sand",
                                          "Coarse Sand",
                                          "Medium Sand",
                                          "Fine Sand",
                                          "Very Fine Sand",
                                          "Very Coarse Silt",
                                          "Coarse Silt",
                                          "Medium Silt"))
df0$SORTING_folkWard_desc <- factor(df0$SORTING_folkWard_desc,
                                    levels=rev(c("Well Sorted",
                                                 "Moderately Well Sorted",
                                                 "Moderately Sorted",
                                                 "Poorly Sorted",
                                                 "Very Poorly Sorted")))
df0$SKEWNESS_folkWard_desc <- factor(df0$SKEWNESS_folkWard_desc,
                                     levels=rev(c("Very Coarse Skewed",
                                                  "Coarse Skewed",
                                                  "Symmetrical",
                                                  "Fine Skewed",
                                                  "Very Fine Skewed")))
df0$KURTOSIS_folkWard_desc <- factor(df0$KURTOSIS_folkWard_desc,
                                     levels=rev(c("Very Platykurtic",
                                                  "Platykurtic",
                                                  "Mesokurtic",
                                                  "Leptokurtic",
                                                  "Very Leptokurtic",
                                                  "Extremely Leptokurtic")))

df0$SEDIMENT.NAME <- factor(df0$SEDIMENT.NAME,levels=
                              rev(c("Sandy Very Coarse Gravel",
                                    "Sandy Coarse Gravel",
                                    "Sandy Medium Gravel",
                                    "Fine Gravel",
                                    "Sandy Fine Gravel",
                                    "Fine Silty Sandy Fine Gravel",
                                    "Very Fine Gravel",
                                    "Medium Silty Sandy Very Fine Gravel",
                                    "Sandy Very Fine Gravel",
                                    "Slightly Very Fine Gravelly Very Coarse Sand",
                                    "Very Fine Gravelly Very Coarse Sand",
                                    "Slightly Medium Gravelly Coarse Sand",
                                    "Slightly Very Fine Gravelly Coarse Sand",
                                    "Very Fine Gravelly Coarse Sand",
                                    "Very Coarse Gravelly Medium Sand",
                                    "Slightly Coarse Gravelly Medium Sand",
                                    "Coarse Gravelly Medium Sand",
                                    "Medium Gravelly Medium Sand",
                                    "Slightly Medium Gravelly Medium Sand",
                                    "Slightly Fine Gravelly Medium Sand",
                                    "Fine Gravelly Medium Sand",
                                    "Slightly Very Fine Gravelly Medium Sand",
                                    "Slightly Very Fine Gravelly Medium Silty Fine Sand",
                                    "Very Fine Gravelly Medium Sand",
                                    "Well Sorted Medium Sand",
                                    "Moderately Well Sorted Medium Sand",
                                    "Slightly Very Fine Gravelly Fine Silty Medium Sand",
                                    "Very Coarse Silty Fine Sand",
                                    "Coarse Gravelly Fine Sand",
                                    "Slightly Coarse Gravelly Fine Sand",
                                    "Slightly Coarse Gravelly Fine Silty Fine Sand",
                                    "Medium Gravelly Fine Sand",
                                    "Medium Gravelly Fine Silty Fine Sand",
                                    "Slightly Medium Gravelly Very Coarse Silty Fine Sand",
                                    "Slightly Medium Gravelly Medium Silty Fine Sand",
                                    "Slightly Medium Gravelly Fine Sand",
                                    "Slightly Medium Gravelly Fine Silty Fine Sand",
                                    "Slightly Fine Gravelly Fine Sand",
                                    "Slightly Fine Gravelly Fine Silty Fine Sand",
                                    "Slightly Very Fine Gravelly Coarse Silty Fine Sand",
                                    "Slightly Very Fine Gravelly Very Coarse Silty Fine Sand",
                                    "Slightly Very Fine Gravelly Fine Sand",
                                    "Slightly Very Fine Gravelly Fine Silty Fine Sand",
                                    "Fine Gravelly Fine Sand",
                                    "Fine Gravelly Fine Silty Fine Sand",
                                    "Slightly Fine Gravelly Very Coarse Silty Fine Sand",
                                    "Slightly Fine Gravelly Medium Silty Fine Sand",
                                    "Very Fine Gravelly Fine Sand",
                                    "Very Fine Gravelly Very Coarse Silty Fine Sand",
                                    "Very Fine Gravelly Fine Silty Fine Sand",
                                    "Moderately Well Sorted Fine Sand",
                                    "Well Sorted Fine Sand",
                                    "Slightly Coarse Gravelly Very Coarse Silty Very Fine Sand",
                                    "Slightly Medium Gravelly Very Coarse Silty Very Fine Sand",
                                    "Slightly Fine Gravelly Medium Silty Very Fine Sand",
                                    "Slightly Very Fine Gravelly Very Coarse Silty Very Fine Sand",
                                    "Fine Sandy Very Coarse Silt",
                                    "Slightly Very Fine Gravelly Fine Sandy Very Coarse Silt",
                                    "Slightly Very Fine Gravelly Very Fine Sandy Very Coarse Silt",
                                    "Slightly Very Fine Gravelly Coarse Silt",
                                    "Slightly Very Fine Gravelly Fine Sandy Medium Silt",
                                    "Slightly Very Fine Gravelly Very Fine Sandy Medium Silt",
                                    "Slightly Very Fine Gravelly Fine Sandy Fine Silt",
                                    "Fine Silt")))

### keep only 5cm cores & current year
df <- df0
df <- df[df$year==cur.yr,]
df <- droplevels(subset(df, method !="15cm"))

#### descriptive plots ####
### Sorting ####
set.seed(451.2)
h <- ggplot(df, aes(zone1, `SORTING_folkWard_desc`))+
  geom_jitter(aes(colour = shore,shape = shore),
              position = position_jitter(width = 0.4,
                                         height = 0.4),
              size = 7, alpha = 0.7)+
  scale_shape_manual(values=c(15:18))+
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10))+
  scale_fill_manual(values=cbPaletteshr) +
  scale_color_manual(values=cbPaletteshr)+
  xlab("") + ylab("")+
  geom_hline(yintercept = c(0.5,1.5,2.5, 3.5,4.5),
             col = "grey", linetype = "dashed")+
  geom_vline(xintercept = c(0.5,1.5,2.5,3.5,4.5),
             col = "grey", linetype = "dashed")+
  ggtitle(paste0("Sediment sorting ",cur.yr))

ppi <- 200
png(file = paste0("output/figs/sed.",cur.yr,".sort.png"),
    width=12*ppi, height=6*ppi, res=ppi)
print(h)
dev.off(); rm(h)

### Skew ####
set.seed(451.2)
h <- ggplot(df, aes(zone1, SKEWNESS_folkWard_desc))+
  geom_jitter(aes(colour = shore, shape = shore),
              position = position_jitter(width = 0.4,
                                         height = 0.4),
              size = 7, alpha = 0.7)+
  scale_shape_manual(values=c(15:18))+
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10))+
  scale_fill_manual(values=cbPaletteshr) +
  scale_color_manual(values=cbPaletteshr)+
  xlab("") + ylab("")+
  geom_hline(yintercept = c(0.5,1.5,2.5, 3.5,4.5),
             col = "grey", linetype = "dashed")+
  geom_vline(xintercept = c(0.5,1.5,2.5,3.5,4.5),
             col = "grey", linetype = "dashed")+
  ggtitle(paste0("Sediment skew ",cur.yr))

ppi <- 200
png(file = paste0("output/figs/sed.",cur.yr,".skew.png"),
    width=12*ppi, height=6*ppi, res=ppi)
print(h)
dev.off(); rm(h)

### Kurtosis ####
set.seed(2144)
h <- ggplot(df, aes(zone1, KURTOSIS_folkWard_desc))+
  geom_jitter(aes(colour = shore, shape=shore),
              position = position_jitter(width = 0.4,
                                         height = 0.4),
              size = 7, alpha = 0.7)+
  scale_shape_manual(values=c(15:18))+
  theme(legend.title = element_blank(),
        axis.text = element_text(size = 12),
        legend.text = element_text(size = 10))+
  scale_fill_manual(values=cbPaletteshr) +
  scale_color_manual(values=cbPaletteshr)+
  xlab("") + ylab("")+
  geom_hline(yintercept = c(0.5,1.5,2.5, 3.5,4.5),
             col = "grey", linetype = "dashed")+
  geom_vline(xintercept = c(0.5,1.5,2.5,3.5,4.5),
             col = "grey", linetype = "dashed")+
  ggtitle(paste0("Sediment kurtosis ",cur.yr))

png(file = paste0("output/figs/sed.",cur.yr,".kurt.png"),
    width=12*ppi, height=6*ppi, res=ppi)
print(h)
dev.off(); rm(h)

### Sediment TYPE ####
set.seed(222)
h <- ggplot(df, aes(zone1, SEDIMENT.NAME))+
 geom_jitter(aes(colour = shore,shape=shore),
             position = position_jitter(width = 0.25,
                                        height = 0.25),
             size = 7, alpha = 0.7)+
 scale_shape_manual(values=c(15:18))+
 theme(legend.title = element_blank(),
       axis.text = element_text(size = 12),
       legend.text = element_text(size = 10))+
 scale_fill_manual(values=cbPaletteshr) +
 scale_color_manual(values=cbPaletteshr)+
 xlab("") + ylab("")+
 geom_hline(yintercept = c(seq(from = 0.5, to = 22.5, by = 1)),
            col = "grey", linetype = "dashed")+
 geom_vline(xintercept = c(0.5,1.5,2.5,3.5,4.5),
            col = "grey", linetype = "dashed")+
 ggtitle(paste0("Sediment type ",cur.yr))

ppi <- 200
png(file = paste0("output/figs/sed.",cur.yr,".sedname.png"),
   width=12*ppi, height=6*ppi, res=ppi)
print(h)
dev.off(); rm(h)
 
###############################################################
#####                    Continue from here              ######
###############################################################
### Percentage compositions & quantitative #####
### Proportions #####
#### MUD ####
mud <- ggplot(data = df,
              aes(x = transect, y = MUD_perc,
                  fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol=2)+
  theme(legend.title=element_blank(),
        legend.direction="horizontal",
        legend.position = c(0.00,1.2),
        strip.text.x = element_text(size = 12),
        axis.text.x  = element_text(angle=90, vjust=0.5))+
  ylab("Mud %") + xlab("")+
  scale_fill_manual(values=cbPalette)+
  ylim(0,1)+
  labs(fill="")

#### Sand ####
snd <- ggplot(data = df,
              aes(x = transect, y = SAND_perc,
                  fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol=2)+
  theme(legend.position="none",strip.text.x = element_text(size = 12),
        axis.text.x  = element_text(angle=90, vjust=0.5))+
  ylab("Sand %") + xlab("")+
  scale_fill_manual(values=cbPalette)+
  ylim(0,1)+
  labs(fill="")

#### Gravel ####
grv <- ggplot(data = df,
              aes(x = transect, y = GRAVEL_perc,
                  fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol=2)+
  theme(legend.position="none",strip.text.x = element_text(size = 12))+
  ylab("Gravel %") + xlab("")+
  scale_fill_manual(values=cbPalette)+
  labs(fill="")+
  ylim(0,1)+
  scale_x_discrete(breaks=NULL)

#### phi ####
phi <- ggplot(data = df,
              aes(x = transect, y = MEAN_folkWard_phi,
                  fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol=2)+
  theme(legend.position="none",strip.text.x = element_text(size = 12))+
  ylab("Mean phi") + xlab("")+
  scale_fill_manual(values=cbPalette)+
  labs(fill="")+
  scale_x_discrete(breaks=NULL)

source("R/00_multiplot.R")
png(file = paste0("output/figs/sed.",cur.yr,".sed.bulk.png"),
    width=12*ppi, height=6*ppi, res=ppi)
multiplot(phi, snd, grv,mud,cols = 2)
dev.off()
rm(phi,mud,grv,snd)
####

#### stacked version of sediment plot ####
# x=transect = prop
dflx <- df[,c(2,4,5,6,7,59:61)]

dfl <- dflx %>% 
  pivot_longer(.,GRAVEL_perc:MUD_perc,names_to="sediment",values_to="result.prop") %>% 
  mutate(result.perc=result.prop*100) %>% ungroup()

dfl$labl <- ifelse(dfl$sediment=="GRAVEL_perc","Gravel",
                   ifelse(dfl$sediment=="SAND_perc","Sand",
                          ifelse(dfl$sediment=="MUD_perc","Mud","ERROR")))
dfl$sediment <- factor(dfl$sediment, levels=c("GRAVEL_perc","SAND_perc","MUD_perc"))
dfl$labl <- factor(dfl$labl, levels=c("Gravel","Sand","Mud"))

stk <- ggplot(data=dfl, aes(x=transect,y=result.perc))+
  geom_col(aes(fill = labl),width=0.7,colour="black")+
  facet_wrap(~shore)+
  scale_fill_manual(values=c("lightgrey","gold1","brown"))+
  ylab("Percentage contribution")+
  theme(legend.title=element_blank(),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size = 12))

#### phi version2 ####
phi2 <- ggplot(data = df,
               aes(x = transect, y = MEAN_folkWard_phi,
                   fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol=2)+
  theme(legend.position="none",strip.text.x = element_text(size = 12))+
  ylab("Mean phi") + xlab("")+
  scale_fill_manual(values=cbPalette)+
  labs(fill="")+
  scale_x_discrete(breaks=NULL)

##plot
x <- phi2+stk+ plot_layout(ncol=1) + plot_annotation(tag_levels = 'A')
png(file = paste0("output/figs/sed.",cur.yr,".sed.phiBult2.png"),
    width=12*ppi, height=12*ppi, res=ppi)
print(x)
dev.off()
rm(x,phi2,stk,dfl)

### Sorting ####
srt <- ggplot(data = df,
              aes(x = transect, y = SORTING_folkWard_phi,
                  fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol=2)+
  theme(legend.position="none",strip.text.x = element_text(size = 12))+
  ylab("Sorting coefficient") + xlab("")+
  scale_fill_manual(values=cbPalette)+
  labs(fill="")+
  scale_x_discrete(breaks=NULL)

### Skew ####
skw <- ggplot(data = df,
              aes(x = transect, y = SKEWNESS_folkWard_phi,
                  fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol=2)+
  theme(legend.position="none",strip.text.x = element_text(size = 12),
        axis.text.x  = element_text(angle=90, vjust=0.5))+
  ylab("Skew") + xlab("")+
  scale_fill_manual(values=cbPalette)+
  labs(fill="")

### Kurtosis ####
krt <- ggplot(data = df,
              aes(x = transect, y = KURTOSIS_folkWard_phi,
                  fill = zone1, color = "black"))+
  geom_bar(stat = "identity", colour = "black")+
  facet_wrap(~ shore, ncol=2)+
  theme(legend.title=element_blank(),
        legend.direction="horizontal",
        legend.position = c(0.4,-0.6),
        strip.text.x = element_text(size = 12),
        axis.text.x  = element_text(angle=90, vjust=0.5))+
  ylab("Kurtosis") + xlab("")+
  scale_fill_manual(values=cbPalette)+
  labs(fill="")

png(file = paste0("output/figs/sed.",cur.yr,".sed.bulks.png"),
    width=12*ppi, height=6*ppi, res=ppi)
multiplot(srt, skw, krt, cols = 2)
dev.off()
rm(srt,krt,skw)

### quick calcs for doc ####
x <- prop.table(table(droplevels(df$SEDIMENT.NAME)))*100
x[order(-x)]

### mean phi values
View(df[order(-df$MEAN_folkWard_phi),c(1:10,31)])


# Tidy up ####
rm(list = ls(pattern = "df"))
rm(list = ls(pattern = "cbP"))
rm(cur.yr,ppi,multiplot,x,fol,gisfol,perm)
detach("package:patchwork", unload=TRUE)
detach("package:tidyverse", unload=TRUE)
detach("package:ggplot2", unload=TRUE)

### an.sed.psa.ts.bulk.plots.USE.R ####

### Production of time series charts for bulk sediment data

# Set up ####
ld_pkgs <- c("tidyverse","ggplot2","patchwork")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE);rm(ld_pkgs)

### set metadata
source("R/00_meta_setMeta.R")

## load data 
df0 <- openxlsx::read.xlsx(paste0(fol,"sed.psa.bulkWIP_use.xlsx"),sheet="sed.bulk.ts.out")

### keep only 5cm cores
df <- df0 %>% 
  filter(., method !="15cm") %>% 
  filter(.,transect !="WA1") %>% droplevels(.)

### sort factors ####
df$shore <- factor(df$shore,levels=c("Upper","Mid","Low","Surf"))
df$zone1 <- factor(df$zone1,levels=c("Above","Inside","Inside2","Below"))

### Mean phi ####
###Box
h <- ggplot(data = df, aes(y = MEAN_folkWard_phi, x = year, fill = zone1))+
  geom_hline(data=df,aes(yintercept=mean(MEAN_folkWard_phi)),lty=2, col="grey")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = 0.9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  #scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("Year") + ylab(expression(paste("Mean ",phi)))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
png(file = "output/figs/sed.ts.meanphi.box.png",
    width=12*ppi, height=6*ppi, res=ppi)
print(h);
dev.off(); #rm(h)

### Line
k <- ggplot(data = df, aes(x = year, y = MEAN_folkWard_phi, group = transect))+
  geom_point(data = df[,!names(df) %in% c("shore","zone")],
             colour = "grey80")+
  geom_line(data = df[,!names(df) %in% c("shore","zone")],
            colour = "grey80",
            aes(group = site_code))+
  geom_point(aes(colour = zone1))+
  geom_line(aes(group = transect, colour = zone1),
            linewidth = .75)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  # scale_x_continuous(breaks=c(2008,2010,2012,
  #                             2014,2016,2018))+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("")+
  ylab(expression(paste("Mean ",phi)))

# png(file = "output/figs/psa/sed.ts.meanphi.line.png",
#     width=12*ppi, height=6*ppi, res=ppi)
# print(k);
# dev.off(); rm(k)

ptc <- (k/h)+plot_annotation(tag_levels = 'A')

png(file = "output/figs/sed.ts.meanphi.png",
    width=12*ppi, height=12*ppi, res=ppi)
print(ptc);
dev.off(); rm(k,h,ptc)

### Sorting ####
###Box
h <- ggplot(data = df, aes(y = SORTING_folkWard_phi, x = year, fill = zone1))+
  geom_hline(data=df,aes(yintercept=mean(SORTING_folkWard_phi)),lty=2, col="grey")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = 0.9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("Year") + ylab("Sorting coefficient")+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
# png(file = "output/figs/psa/sed.ts.sort.box.png",
#     width=12*ppi, height=6*ppi, res=ppi)
# print(h);
# dev.off(); rm(h)

### Line
k <- ggplot(data = df, aes(x = year, y = SORTING_folkWard_phi, group = transect))+
  geom_point(data = df[,!names(df) %in% c("shore","zone")],
             colour = "grey80")+
  geom_line(data = df[,!names(df) %in% c("shore","zone")],
            colour = "grey80",
            aes(group = site_code))+
  geom_point(aes(colour = zone1))+
  geom_line(aes(group = transect, colour = zone1),
            size = .75)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("")+
  ylab("Sorting coefficient")

# # png(file = "output/figs/psa/sed.ts.sort.line.png",
# #     width=12*ppi, height=6*ppi, res=ppi)
# # print(k);
# # dev.off(); rm(k)
# # 

ptc <- (k/h)+plot_annotation(tag_levels = 'A')

png(file = "output/figs/sed.ts.sort.png",
    width=12*ppi, height=12*ppi, res=ppi)
print(ptc);
dev.off(); rm(k,h,ptc)

### Skew ####
###Box
h <- ggplot(data = df, aes(y = SKEWNESS_folkWard_phi, x = year, fill = zone1))+
  geom_hline(data=df,aes(yintercept=mean(SKEWNESS_folkWard_phi)),lty=2, col="grey")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = 0.9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("Year") + ylab("Skew")+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
# png(file = "output/figs/psa/sed.ts.skew.box.png",
#     width=12*ppi, height=6*ppi, res=ppi)
# print(h);
# dev.off(); rm(h)

### Line
k <- ggplot(data = df, aes(x = year, y = SKEWNESS_folkWard_phi, group = transect))+
  geom_point(data = df[,!names(df) %in% c("shore","zone")],
             colour = "grey80")+
  geom_line(data = df[,!names(df) %in% c("shore","zone")],
            colour = "grey80",
            aes(group = site_code))+
  geom_point(aes(colour = zone1))+
  geom_line(aes(group = transect, colour = zone1),
            size = .75)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("")+
  ylab("Skew")
# png(file = "output/figs/psa/sed.ts.skew.line.png",
#     width=12*ppi, height=6*ppi, res=ppi)
# print(k);
# dev.off(); rm(k)
ptc <- (k/h)+plot_annotation(tag_levels = 'A')

png(file = "output/figs/sed.ts.skew.png",
    width=12*ppi, height=12*ppi, res=ppi)
print(ptc);
dev.off(); rm(k,h,ptc)

### Kurtosis ####
###Box
h <- ggplot(data = df, aes(y = KURTOSIS_folkWard_phi, x = year, fill = zone1))+
  geom_hline(data=df,aes(yintercept=mean(KURTOSIS_folkWard_phi)),lty=2, col="grey")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = 0.9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("Year") + ylab("Kurtosis")+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
# png(file = "output/figs/psa/sed.ts.kurtosis.box.png",
#     width=12*ppi, height=6*ppi, res=ppi)
# print(h);
# dev.off(); rm(h)

### Line
k <- ggplot(data = df, aes(x = year, y = KURTOSIS_folkWard_phi, group = transect))+
  geom_point(data = df[,!names(df) %in% c("shore","zone")],
             colour = "grey80")+
  geom_line(data = df[,!names(df) %in% c("shore","zone")],
            colour = "grey80",
            aes(group = site_code))+
  geom_point(aes(colour = zone1))+
  geom_line(aes(group = transect, colour = zone1),
            size = .75)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("")+
  ylab("Kurtosis")

ptc <- (k/h)+plot_annotation(tag_levels = 'A')

png(file = "output/figs/sed.ts.kurt.png",
    width=12*ppi, height=12*ppi, res=ppi)
print(ptc);
dev.off(); rm(k,h,ptc)

### Prop grav####
### Box
h <- ggplot(data = df, aes(y = GRAVEL_perc, x = year, fill = zone1))+
  geom_hline(data=df,aes(yintercept=mean(GRAVEL_perc)),lty=2, col="grey")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = 0.9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  coord_cartesian(ylim=c(0,NA))+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("Year") + ylab("Proportion of gravel")+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))

### Line
k <- ggplot(data = df, aes(x = year, y = GRAVEL_perc, group = transect))+
  geom_point(data = df[,!names(df) %in% c("shore","zone")],
             colour = "grey80")+
  geom_line(data = df[,!names(df) %in% c("shore","zone")],
            colour = "grey80",
            aes(group = site_code))+
  geom_point(aes(colour = zone1))+
  geom_line(aes(group = transect, colour = zone1),
            size = .75)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  coord_cartesian(ylim=c(0,NA))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("")+
  ylab("Proportion of gravel")

ptc <- (k/h)+plot_annotation(tag_levels = 'A')

png(file = "output/figs/sed.ts.prop.grav.png",
    width=12*ppi, height=12*ppi, res=ppi)
print(ptc);
dev.off(); rm(k,h,ptc)

### Prop sand ####
### Box
h <- ggplot(data = df, aes(y = SAND_perc, x = year, fill = zone1))+
  geom_hline(data=df,aes(yintercept=mean(SAND_perc)),lty=2, col="grey")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = 0.9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("Year") + ylab("Proportion of sand")+
  coord_cartesian(ylim=c(0,NA))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))
### Line
k <- ggplot(data = df, aes(x = year, y = SAND_perc, group = transect))+
  geom_point(data = df[,!names(df) %in% c("shore","zone")],
             colour = "grey80")+
  geom_line(data = df[,!names(df) %in% c("shore","zone")],
            colour = "grey80",
            aes(group = site_code))+
  geom_point(aes(colour = zone1))+
  geom_line(aes(group = transect, colour = zone1),
            size = .75)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  coord_cartesian(ylim=c(0,NA))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("")+
  ylab("Proportion of sand")

ptc <- (k/h)+plot_annotation(tag_levels = 'A')

png(file = "output/figs/sed.ts.prop.sand.png",
    width=12*ppi, height=12*ppi, res=ppi)
print(ptc);
dev.off(); rm(k,h,ptc)

### Prop mud ####
### Box
h <- ggplot(data = df, aes(y = MUD_perc, x = year, fill = zone1))+
  geom_hline(data=df,aes(yintercept=mean(MUD_perc)),lty=2, col="grey")+
  geom_boxplot(aes(group=year))+
  geom_smooth(method = "loess", colour = "red", span = 0.9)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("Year") + ylab("Proportion of mud")+
  coord_cartesian(ylim=c(0,NA))+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))

### Line
k <- ggplot(data = df, aes(x = year, y = MUD_perc, group = transect))+
  geom_point(data = df[,!names(df) %in% c("shore","zone")],
             colour = "grey80")+
  geom_line(data = df[,!names(df) %in% c("shore","zone")],
            colour = "grey80",
            aes(group = site_code))+
  geom_point(aes(colour = zone1))+
  geom_line(aes(group = transect, colour = zone1),
            size = .75)+
  facet_grid(shore~zone1)+
  scale_colour_manual(name = "", values=cbPalette)+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  # scale_x_continuous(breaks = seq(2007, 2019, by = 3))+
  xlab("")+
  ylab("Proportion of mud")

ptc <- (k/h)+plot_annotation(tag_levels = 'A')

png(file = "output/figs/sed.ts.prop.mud.png",
    width=12*ppi, height=12*ppi, res=ppi)
print(ptc);
dev.off(); rm(k,h,ptc)

### tidy up ####
rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^cb"))
rm(cur.yr,ppi,fol,gisfol,perm)
detach("package:patchwork", unload=TRUE)
detach("package:ggplot2", unload=TRUE)

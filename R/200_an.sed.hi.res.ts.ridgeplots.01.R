### an.sed.hi.res.ts.ridgeplots.01.R #####
### analyse sediment data and produce ridgeplots

#### load packages ####
ld_pkgs <- c("tidyverse","ggplot2","ggridges")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### set metadata
source("R/00_meta_setMeta.R")

### load data & format ####
dfl0 <- as_tibble(read.csv(paste0(fol,"sed.psa.hi.ts.csv")))

##remove Wash samples
dfl0 %>% 
  filter(., transect != "WA1") -> dfl
## arrange factor levels
dfl$zone1 <- factor(dfl$zone1,levels=c("Above","Inside","Inside2","Below"))
dfl$shore <- factor(dfl$shore,levels=c("Upper","Mid","Low","Surf"))

cur.dat <- dfl[dfl$year == cur.yr,]
cur.dat <- cur.dat[cur.dat$method=="5cm",]

#### create mean across transects ####
dflmn <- dfl %>% 
  filter(method == "5cm") %>% 
  dplyr::select(-c(transect,code,microns,method,zone2.1:notes)) %>% 
  group_by(year,zone1,shore,phi) %>% 
  summarise("meanperc"=mean(perc),"sdperc"=sd(perc),.groups = "drop") %>% ungroup() %>% 
  mutate(label=paste0(shore," shore: ",zone1))

dflmn$label <- factor(dflmn$label,levels=c("Upper shore: Above","Upper shore: Inside","Upper shore: Inside2","Upper shore: Below",
                                           "Mid shore: Above","Mid shore: Inside","Mid shore: Inside2","Mid shore: Below",
                                           "Low shore: Above","Low shore: Inside","Low shore: Inside2","Low shore: Below",
                                           "Surf shore: Above","Surf shore: Inside","Surf shore: Inside2","Surf shore: Below"
                                           ))

### plot ####
# png(file = paste0("output/figs/psa/sed.",cur.yr,".PSAdistribn.png"),
#     width=12*ppi, height=6*ppi, res=ppi)

# all <- ggplot(data=dflmn,aes(x=phi,y=as.factor(year)))+
#   facet_wrap(.~zone1)+
#   geom_density_ridges(scale=4, alpha=0.7)+scale_y_discrete(limits=rev)

pl <- ggplot(data = dflmn[dflmn$year!="2009",],
             aes(x = phi, y=as.factor(year), height = meanperc))+#, group = year,colour=shore)) +
  geom_vline(xintercept = c(0,4), colour="darkgrey",lty=2)+
  # facet_wrap(.~zone1)+
  # facet_wrap(~label)+
  facet_grid(shore ~ zone1)+
  geom_density_ridges(stat="identity",scale=1.4,alpha=0.7,
                      aes(fill=zone1),
                      show.legend = FALSE)+
  scale_y_discrete(limits=rev)+
  scale_fill_manual(values=cbPalette)+
  xlim(-5,9)+
  labs(x="Phi",y="Year",
       title = "Distribution of sediment grain sizes since 2011 within different beach nourishment zones on the Lincolnshire coast",
       subtitle="Sandy sediments characterise most of the monitoring zone. Coarser sediments (i.e., smaller phi values) dominate Inside the beach nourishment zone.\nThis difference is largely confined to the upper and mid shore sites which receive the majority of nourishment material",
       caption = "Phi ranges: <0 = gravel; 0-4 = sand; >4 = silt")+
  theme(strip.text = element_text(size = 14, face="bold"));pl

# pdf("output/figs/sed.ts.psahires.pdf", width=14,height = 14)
png("output/figs/sed.ts.psahires.png", width=14,height = 14,units = "in",res=ppi)
print(pl)
dev.off()

# dftmp <- dflmn %>% 
#   filter(year != "2009",
#          shore %in% c("Mid")) %>% #,"Upper","Low")) %>% 
#   droplevels()
# 
# plmid <- ggplot(data=dftmp,
#                 aes(x= phi, y=as.factor(year), height = meanperc))+#, group = year,colour=shore)) +
#   geom_vline(xintercept = c(0,4), colour="darkgrey",lty=2)+
#   facet_wrap(.~zone1)+
#   geom_density_ridges(stat="identity",scale=1.2,alpha=0.6,
#                       aes(fill=shore))+
#   scale_y_discrete(limits=rev)+
#   scale_fill_manual(values=cbPalette)+
#   xlim(-5,9)+
#   labs(x="Phi",y="Year",
#        title = "Distribution of mid-shore sediment grain sizes since 2011 within different beach nourishment zones on the Lincolnshire coast",
#        subtitle="Sandy sediments characterise most of the monitoring zone. Coarser sediments dominate Inside the beach nourishment zone.",
#        caption = "Phi ranges: <0 = gravel; 0-4 = sand; >4 = silt")+
#   # guides(fill=guide_legend(title="Shore \nheight"))+
#   theme(legend.position="none")
# 
# pdf("output/figs/psa/sed.ts.psahires_midshore.pdf", width=14,height = 14)
# print(plmid)
# dev.off()

# ggplot(dflmn[dflmn$shore=="Mid"], aes(x= phi, y=year, height = meanperc, group = year)) +
#   facet_wrap(.~zone1)+
#   # geom_ridgeline(scale=0.1,
#   #                fill="lightblue",
#   #                alpha=0.7)+
#   geom_density_ridges(stat="identity",scale=1)+
#   scale_y_discrete(limits=rev)

pl <- ggplot(data=cur.dat[!is.na(cur.dat$code),], aes(x = phi, y = perc,group=transect))+
  geom_line(aes(group=transect,colour=zone1),size = 1.25)+
  # geom_point(aes(group=transect,colour=zone1),size=3)+
  # geom_point(aes(group=transect),shape=1,size=3)+
  ylab ("Percentage contribution")+
  geom_point(aes(group=transect,shape=zone1,fill=zone1),size=3)+
  scale_shape_manual(values = c(21:24)) +#tell R which symbols to use
  theme(legend.position=c(0.5,0.5),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        axis.text = element_text(size = 11),
        strip.text.x = element_text(size = 12),
        legend.background=element_rect(fill=alpha("white",0.1))
  )+
  facet_wrap(~ shore, ncol = 2)+
  scale_colour_manual(values=cbPalette)+
  scale_fill_manual(values=cbPalette)+
  xlim(NA,10)+
  labs(title=paste0("Sediment grain size distributions ",cur.yr))+
  theme(strip.text = element_text(face="bold"))

png("output/figs/sed.cur.psahires.png", width=12,height = 8,units = "in",res=ppi)
print(pl)
dev.off()

# TIDY UP ####
rm(list = ls(pattern = "^df"))
rm(list = ls(pattern = "^cb"))
rm(list = ls(pattern = "^cur"))
rm(pl,fol,gisfol,ppi,perm)
detach("package:tidyverse", unload=TRUE)
detach("package:ggridges", unload=TRUE)
detach("package:ggplot2", unload=TRUE)

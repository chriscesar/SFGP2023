## imp.ifca0.R ####
## Import IFCA cockle stock data from current year and combine with historic

# Set up ####
### load packages ####
ld_pkgs <- c("tidyverse")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### load metadata ####
source("R/00_meta_setMeta.R")

### load data ####
ifca <- as_tibble(read.csv(paste0(fol,"icfa_ts.csv")))

ifca$class <- as.factor(ifca$class)

## remove Longsand data
ifca$bed <- as.factor(ifca$bed)

## import Waddington weather station data ####
# https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/waddingtondata.txt
url <- "https://www.metoffice.gov.uk/pub/data/weather/uk/climate/stationdata/waddingtondata.txt"
ifca.weather <- as_tibble(read.table(url, skip = 7))
names(ifca.weather) <- c("yyyy","mm", "TMax_degC", "TMin_degC",
                         "afDays","rain_mm", "sun_h")

ifca.weather$afDays <- as.numeric(ifelse(ifca.weather$afDays == "---",
                                         NA,ifca.weather$afDays))

ifca.weather$sun_h <- as.numeric(gsub("[#^]","",ifca.weather$sun_h))### remove #s from end of $sun values

# analyse long term cockle data ####

### format data ####
## drop 'ext' data
keep <- c("Friskney","Wrangle")
ifca <- droplevels(ifca[ifca$bed %in% keep,]);rm(keep)

ifca.interp <- ifca

### interpolate values ####
### no stock assessments conducted in 2020
## Friskney: Adult = mean Friskney Adult 2019 & Friskney Adult 2021
ifca.interp$tonnes[ifca.interp$year=="2020" & ifca.interp$class == "adult" & ifca.interp$bed=="Friskney"] <- mean(c(ifca.interp$tonnes[ifca.interp$year=="2019" & ifca.interp$class == "adult" & ifca.interp$bed=="Friskney"],
                                                                                                                    ifca.interp$tonnes[ifca.interp$year=="2021" & ifca.interp$class == "adult" & ifca.interp$bed=="Friskney"]))
## Friskney: Juvenile = mean Friskney Juvenile 2019 & Friskney Juvenile 2021
ifca.interp$tonnes[ifca.interp$year == "2020" &
                     ifca.interp$class == "juv" &
                     ifca.interp$bed == "Friskney"] <-
  mean(c(ifca.interp$tonnes[ifca.interp$year == "2019" &
                              ifca.interp$class == "juv" & ifca.interp$bed == "Friskney"],
         ifca.interp$tonnes[ifca.interp$year ==
                              "2021" & ifca.interp$class == "juv" & ifca.interp$bed == "Friskney"]))

## Wrangle: Adult = mean Wrangle Adult 2019 & Wrangle Adult 2021
ifca.interp$tonnes[ifca.interp$year == "2020" &
                     ifca.interp$class == "adult" &
                     ifca.interp$bed == "Wrangle"] <-
  mean(c(ifca.interp$tonnes[ifca.interp$year == "2019" &
                              ifca.interp$class == "adult" & ifca.interp$bed == "Wrangle"],
         ifca.interp$tonnes[ifca.interp$year ==
                              "2021" &
                              ifca.interp$class == "adult" & ifca.interp$bed == "Wrangle"]))

## Wrangle: Juvenile = mean Wrangle Juvenile 2019 & Wrangle Juvenile 2021
ifca.interp$tonnes[ifca.interp$year == "2020" &
                     ifca.interp$class == "juv" &
                     ifca.interp$bed == "Wrangle"] <-
  mean(c(ifca.interp$tonnes[ifca.interp$year == "2019" &
                              ifca.interp$class == "juv" & ifca.interp$bed == "Wrangle"],
         ifca.interp$tonnes[ifca.interp$year ==
                              "2021" & ifca.interp$class == "juv" & ifca.interp$bed == "Wrangle"]))

### calculate total cockles
tot <-
  ifca %>% group_by(year, bed) %>%
  summarise(tot.cockle = sum(tonnes),
            .groups = "drop")
tot.interp <-
  ifca.interp %>% group_by(year, bed) %>%
  summarise(tot.cockle = sum(tonnes),
            .groups = "drop")

### plot Cockles
### by class
levels(ifca$class) <- c("Adult","Juvenile")
levels(ifca.interp$class) <- c("Adult","Juvenile")

png(file = "figs/ifca.class.ts.png",
    width=12*ppi, height=6*ppi, res=ppi)
ggplot(data = ifca, aes(x = year, y = tonnes, fill = bed))+
  geom_hline(yintercept = 0,colour="lightgrey",lty=2)+
  geom_smooth(method = "loess", colour = "red", span = 0.9)+
  geom_point(size=2)+
  facet_grid(class~bed)+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  scale_x_continuous(breaks = seq(2004, 2022, by = 2))+
  xlab("") + ylab("Cockle stock estimate (tonnes)")+
  labs(title="Estimated adult and juvenile cockle stock biomasses within 2 cockle beds in the north of The Wash",
       subtitle = "Estimates provided by the Eastern Inshore Fisheries and Conservation Authority",
       caption = "Red lines indicate loess smooth with span = 0.9. Ribbons indicate Standard Errors")+
  theme(strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14),
        strip.text = element_text(face="bold"))+
  coord_cartesian(ylim=c(-100, NA))
dev.off()

### combined: normal
png(file = "figs/ifca.tot.ts.png",
    width=12*ppi, height=6*ppi, res=ppi)
ggplot(data = tot, aes(x = year, y = tot.cockle, fill = bed))+
  geom_smooth(method = "loess", colour = "red", span = 0.9)+
  geom_point(size = 2)+
  facet_grid(bed~.)+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  scale_x_continuous(breaks = seq(2004, 2022, by = 2))+
  xlab("") + ylab("Cockle stock estimate (tonnes)")+
  labs(title="Estimated total cockle stock biomasses within 2 cockle beds in the north of The Wash",
       subtitle = "Estimates provided by the Eastern Inshore Fisheries and Conservation Authority",
       caption = "Red lines indicate loess smooth with span = 0.9. Ribbons indicate Standard Errors")+
  theme(strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14))+
  coord_cartesian(ylim=c(0, NA))
dev.off()

### combined: interpolated
png(file = "figs/ifca.tot.ts.interp.png",
    width=12*ppi, height=6*ppi, res=ppi)
ggplot(data = tot.interp, aes(x = year, y = tot.cockle, fill = bed))+
  geom_smooth(method = "loess", colour = "red", span = 0.9)+
  geom_point(size = 2)+
  facet_grid(bed~.)+
  theme(legend.position="none",
        strip.text.x = element_text(size = 12),
        strip.text.y = element_text(size = 12))+
  scale_colour_manual(name = "", values=cbPalette)+
  scale_fill_manual(name = "", values=cbPalette)+
  scale_x_continuous(breaks = seq(2004, 2022, by = 2))+
  scale_y_continuous(limits=c(NA,NA),expand=c(0,0))+
  xlab("") + ylab("Cockle stock estimate (tonnes)")+
  labs(title="Estimated total cockle stock biomasses within 2 cockle beds in the north of The Wash",
       subtitle = "Estimates provided by the Eastern Inshore Fisheries and Conservation Authority",
       caption = "Red lines indicate loess smooth with span = 0.9. Ribbons indicate Standard Errors")+
  theme(strip.text.x = element_text(size = 14),
        strip.text.y = element_text(size = 14))+
  coord_cartesian(ylim=c(0, NA))
dev.off()

###==================================================####
# Compare correlation of adult & juvenile cockles    ####
###==================================================####
### split to 2 dataframes
ad <- subset(ifca, class == "Adult")
ju <- subset(ifca, class == "Juvenile")
acf(ad$tonnes, na.action = na.pass) 
acf(ju$tonnes, na.action = na.pass) #no trend for 2023 dataset

ad.interp <- subset(ifca.interp, class == "Adult")
ju.interp <- subset(ifca.interp, class == "Juvenile")
acf(ad.interp$tonnes, na.action = na.pass)
acf(ju.interp$tonnes, na.action = na.pass) #no trend for 2023 dataset

###=====================##
### Cross-correlation ####
###=====================##
### ccf(x,y) explores the relationship between 2 time series.
### A NEGATIVE lag=k for ccf(x,y) means that x happens and k
### time steps later, y follows
### A POSTIVE lag=k for cch(x,y) means that x is influenced by
### what y was doing k steps previously

###===========##
### Both beds ####
###===========##
ccf(ad$tonnes,ju$tonnes, main = "Adult vs Juvenile",
    na.action = na.pass)
## for 2023 dataset:
## this shows significant lags at t = +2 and t = -2.
## >> This shows that adult stock at t=2 is associated
## with juvenile stocks at t=0
## >> The t = -2 shows that adult stocks at t=0 are correlated with
## juvenile stocks 2 years hence.
## This shows that a good stock of juveniles at t0 is a
## reasonable predictor of adults 2 years later (t2).

#### interpolated version ####
ccf(ad.interp$tonnes,ju.interp$tonnes, main = "Adult vs Juvenile",
    na.action = na.pass)

## Also, a healthy adult stock at t0 is linked to healthy juvenile recruitment
## at t2
## shown also by:
ccf(ju$tonnes,ad$tonnes, main = "Juvenile vs Adult",
    na.action = na.pass)
ccf(ju.interp$tonnes,ad.interp$tonnes, main = "Juvenile vs Adult",
    na.action = na.pass)

png(file = "figs/ifca.juv&adult.ccf.bothbeds.png",
    width=12*ppi, height=6*ppi, res=ppi)
ccf(ju$tonnes,ad$tonnes, main = "Juvenile vs Adult", na.action = na.pass)
dev.off()

png(file = "figs/ifca.juv&adult.ccf.bothbeds.interp.png",
    width=12*ppi, height=6*ppi, res=ppi)
ccf(ju.interp$tonnes,ad.interp$tonnes, main = "Juvenile vs Adult", na.action = na.pass)
dev.off()

###===================##
### Individual beds ####
###===================##
### normal
ad.wr <- subset(ad, bed == "Wrangle")
ju.wr <- subset(ju, bed == "Wrangle")
png(file = "figs/ifca.juv&adult.Wrangle.ccf.png",
    width=12*ppi, height=6*ppi, res=ppi)
par(mar = c(4,4,4,0.2))
ccf(ju.wr$tonnes,ad.wr$tonnes,
    main = "Juvenile vs Adult\nWrangle Flats",
    na.action = na.pass)
dev.off()

ad.fr <- subset(ad, bed == "Friskney")
ju.fr <- subset(ju, bed == "Friskney")
png(file = "figs/ifca.juv&adult.Friskney.ccf.png",
    width=12*ppi, height=6*ppi, res=ppi)
par(mar = c(4,4,4,0.2))
ccf(ju.fr$tonnes,ad.fr$tonnes,
    main = "Juvenile vs Adult\nFriskney Flats", na.action = na.pass)
dev.off()

### Interpolated
ad.wr.interp <- subset(ad.interp, bed == "Wrangle")
ju.wr.interp <- subset(ju.interp, bed == "Wrangle")

png(file = "figs/ifca.juv&adult.Wrangle.ccf.interp.png",
    width=12*ppi, height=6*ppi, res=ppi)
par(mar = c(4,4,4,0.2))
ccf(ju.wr.interp$tonnes,ad.wr.interp$tonnes,
    main = "Juvenile vs Adult\nWrangle Flats", na.action = na.pass)
dev.off()
ad.fr.interp <- subset(ad.interp, bed == "Friskney")
ju.fr.interp <- subset(ju.interp, bed == "Friskney")
png(file = "figs/ifca.juv&adult.Friskney.ccf.interp.png",
    width=12*ppi, height=6*ppi, res=ppi)
par(mar = c(4,4,4,0.2))
ccf(ju.fr.interp$tonnes,ad.fr.interp$tonnes,
    main = "Juvenile vs Adult\nFriskney Flats", na.action = na.pass)
dev.off()

##both plots
par(mfrow = c(2, 1));par(mar = c(2,4,.5,0.2))
wr.ccf <- ccf(ju.wr$tonnes,ad.wr$tonnes, main = "", na.action = na.pass)
fr.ccf <- ccf(ju.fr$tonnes,ad.fr$tonnes, main = "", na.action = na.pass)
wr.ccf.interp <- ccf(ju.wr.interp$tonnes,ad.wr.interp$tonnes, main = "",
                     na.action = na.pass)
fr.ccf.interp <- ccf(ju.fr.interp$tonnes,ad.fr.interp$tonnes, main = "",
                     na.action = na.pass)

### normal ###
png(file = "figs/ifca.juv&adult.each.ccf.png",
    width=12*ppi, height=6*ppi, res=ppi)
par(mfrow=c(2,1))
par(mar = c(2,4,.5,0.2))
plot(fr.ccf,lwd=3,col="#d68e00")
abline(v=0,col="light grey",lty=4)
text(-8.25,0.8,"Friskney",cex=2)
plot(wr.ccf,lwd = 3,col="#008d62");
abline(v=0,col="light grey",lty=4)
text(-8.25,-0.37,"Wrangle",cex=2)
dev.off()
### significant lag at t= -1 for Friskney (a)
## significant lag at t= +1 for Wrangle (b)
## FRISKNEY: Adult stocks at t=-1 associated with juvenile stocks at t=0
## OR that a good stock of adult cockles are a
## reasonable predictor of juveniles the next year (t=-1)
### For Wrangle, adult stocks at t = +1 are associated with juveniles at t=0.
# i.e., a strong juvenile cohort is a reasonable predictor of adults the following year

### interpolated###
png(file = "figs/ifca.juv&adult.each.ccf.interp.png",
    width=12*ppi, height=6*ppi, res=ppi)
par(mfrow=c(2,1))
par(mar = c(2,4,.5,0.2))
plot(fr.ccf.interp,lwd=3,col="#d68e00")
abline(v=0,col="light grey",lty=4)
text(-8.25,0.75,"Friskney",cex=2)
plot(wr.ccf.interp,lwd = 3,col="#008d62");
abline(v=0,col="light grey",lty=4)
text(-8.25,-0.37,"Wrangle",cex=2)
dev.off()

###=============##
### ACF PLOTS ####
###=============##
png(file = "output/figs/ifca.acf.frisk.interp.png",
    width=12*ppi, height=6*ppi, res=ppi)
par(mfrow = c(2,1))
par(mar = c(4, 4, 0.5, 0.2))
acf(ad.fr.interp$tonnes,main = "", na.action = na.pass)
text(x = 9.25, y = -0.37,"Friskney Adults", adj=0)
acf(ju.fr.interp$tonnes,main = "", na.action = na.pass)
text(x = 9.25, y = -0.37,"Friskney Juveniles", adj=0)
dev.off()

png(file = "output/figs/ifca.acf.wrang.interp.png",
    width = 12 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(2,1))
par(mar = c(4, 4, 0.5, 0.2))
acf(ad.wr.interp$tonnes,main="", na.action = na.pass)
text(x = 9.25, y = -0.37,"Wrangle Adults", adj=0)
acf(ju.wr.interp$tonnes,main="", na.action = na.pass)
text(x = 9.25, y = -0.37,"Wrangle Juveniles", adj=0)
dev.off()

######################################################
# FROM HERE ####################
# Awaiting update on annual nourishment volumes from FCRM/NEAS ###
######################################################

### import sediment nourishment data ####
vols <- read.csv(file="data/2022_nourish_vol&landings.csv", header = T)
vols <- droplevels(vols[vols$measure == "sand nourishment",])
vols$sand_m3 <- vols$value

### subset data
vols <- subset(vols, year<2023 & year>2003)
tot.fr <- subset(tot, bed == "Friskney")
tot.wr <- subset(tot, bed == "Wrangle")
tot.fr.interp <- subset(tot.interp, bed == "Friskney")
tot.wr.interp <- subset(tot.interp, bed == "Wrangle")
#

# sed.fr <- ccf(tot.fr$tot.cockle,vols$sand_m3)
sed.fr <- ccf(tot.fr$tot.cockle,vols$sand_m3,na.action = na.contiguous)
sed.wr <- ccf(tot.wr$tot.cockle,vols$sand_m3,na.action = na.contiguous)

sed.fr.interp <- ccf(tot.fr.interp$tot.cockle,vols$sand_m3,na.action = na.contiguous)
sed.wr.interp <- ccf(tot.wr.interp$tot.cockle,vols$sand_m3,na.action = na.contiguous)

### +ve correlation at lag +1 for Wrangle:
### Suggests that Sediment nourishment at time t
### is associated with elevated cockle hauls at t+1

png(file = "output/figs/ifca/ifca.ccf.totcocklVSed.png",
    width = 12 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(2, 1))
par(mar = c(4, 4, 0.5, 0.2))
plot(sed.fr, lwd = 3, col = "#d68e00")
abline(v = 0, col = "light grey", lty = 4)
text(-8.75, -0.35, "Friskney", cex = 1.5) #a) = FRISKNEY
plot(sed.wr, lwd = 3, col = "#008d62")
abline(v = 0, col = "light grey", lty = 4)
text(-8.75, -0.35, "Wrangle", cex = 1.5) #b) = WRANGLE
dev.off()

png(file = "output/figs/ifca/ifca.ccf.totcocklVSed.interp.png",
    width = 12 * ppi, height = 6 * ppi, res = ppi)
par(mfrow = c(2, 1))
par(mar = c(4, 4, 0.5, 0.2))
plot(sed.fr.interp, lwd = 3, col = "#d68e00")
abline(v = 0, col = "light grey", lty = 4)
text(-8.75, -0.35, "Friskney", cex = 1.5) #a) = FRISKNEY
plot(sed.wr.interp, lwd = 3, col = "#008d62")
abline(v = 0, col = "light grey", lty = 4)
text(-8.75, -0.35, "Wrangle", cex = 1.5) #b) = WRANGLE
dev.off()

### Tidy up ####
rm(list = ls(pattern = "^ad"))
rm(list = ls(pattern = "^ju"))
rm(list = ls(pattern = "^fr"))
rm(list = ls(pattern = "^wr"))
rm(list = ls(pattern = "^ifca"))
rm(list = ls(pattern = "^tot"))
rm(list = ls(pattern = "^sed"))
rm(vols,cbPalette,ppi,cur_yr,libfolder)

detach("package:tidyverse", unload = TRUE)

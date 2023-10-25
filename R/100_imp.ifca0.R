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
ifca <- as_tibble(read.csv(paste0(histdatfol,"icfa_ts.csv")))

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
       caption = "Red lines indicate loess smooth with span = 9. Ribbons indicate Standard Errors")+
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
       caption = "Red lines indicate loess smooth with span = 9. Ribbons indicate Standard Errors")+
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
       caption = "Red lines indicate loess smooth with span = 9. Ribbons indicate Standard Errors")+
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

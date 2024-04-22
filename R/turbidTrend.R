## turbidTrend.R
## quick exploration of turbidity trends
# Investitgating whether summertime nourishment has potential to impact
# intertidal seagrasses at Wells-next-the-Sea.

### set metadata
source("R/00_meta_setMeta.R")

### load packages
ld_pkgs <- c("tidyverse", "ggplot2","lubridate")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

### load turbidity data

df0 <- readxl::read_excel("data/in/AngsuspSeds.xlsx", sheet=1)

### nearest data-rich monitoring site: WA562344

df0 %>% 
  filter(SAMP_SMPT_USER_REFERENCE == "WA562344") %>% 
  mutate(day=yday(SAMP_SAMPLE_DATE)) %>%
  # mutate(mindat=min(SAMP_SAMPLE_DATE),
  #        maxdat=max(SAMP_SAMPLE_DATE)) %>% 
  ggplot(., aes(x=day, y = MEAS_RESULT)) +
  geom_point()+
  geom_smooth()+
  xlab("Day")+
  ylab("Turbidity")+
  labs(caption=paste0("Data from sampling station WA562344 in the vicinity of the mouth of The Wash\n",
                      "Turbidity values measured in situ as part of Environment Agency's monitoring activity between 15/11/2010 & 27/03/2021"))
ggsave("output/figs/WA562344_TurbidityTrend.png")

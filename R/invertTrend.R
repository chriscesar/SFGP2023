## invertTrend.R

### set metadata
source("R/00_meta_setMeta.R")

### load packages
ld_pkgs <- c("tidyverse", "ggplot2","lubridate","vegan")
vapply(ld_pkgs, library, logical(1L),
       character.only = TRUE, logical.return = TRUE)
rm(ld_pkgs)

df0 <- as_tibble(read.csv("data/in/BENTH_OPEN_DATA_TAXA_2024-04-22.csv"))

df <- df0 %>% 
  # dplyr::summarise(n = dplyr::n(), .by = c(SAMPLE_ID, SAMPLE_DATE, PREFERRED_TAXON_NAME)) |>
  # dplyr::filter(n > 1L)
  group_by(SAMPLE_ID, PREFERRED_TAXON_NAME, SAMPLE_DATE) %>% 
  summarise(NUMBER_FOUND=sum(NUMBER_FOUND)) %>% 
  pivot_wider(names_from = PREFERRED_TAXON_NAME, values_from = NUMBER_FOUND, values_fill = 0)

df$taxa <- vegan::specnumber(df[,-c(1,2)])

df %>% 
  ggplot(., aes(x=SAMPLE_DATE, y=taxa))+
  geom_point()

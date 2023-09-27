# 00_meta_installpackages.R

# check if required packages are installed and install if not present

#### install required packages ####
ptm <- proc.time()
req_packages <- c(
                  ## data wrangling and plotting
                  "tidyverse", #data manipulation
                  "ggthemes", #prettify plots
                  "openxlsx", #import from MS Excel
                  "janitor", #work with times & dates
                  "patchwork", #combine multiple plots for document
                  "scales", #Graphical scales mapping data to aesthetics
                  "ggridges", #producing ridgeplots for time series data
                  "corrplot",#fancy plotting
                  "ggpubr",#fancy plots
                  "ggtext", #plotting with text
                  "ggpmisc",#data vis
                  "sjPlot", #plotting comparisons for model outputs
                  "visreg", #plotting comparisons for model outputs
                  ## stats
                  "effects", #post hoc model digging for mixed effects models
                  "lme4", #mixed effects models
                  "lmerTest", #helper for lme4
                  "vegan", #multivariate analysis of ecological data
                  "ggdendro", #production of dendrograms
                  "dendextend", #functions for extending 'dendrogram' objects in R
                  "mgcv", #running GAMs
                  "gratia", #plotting GAM outputs
                  "Hmsc",#(experimental) multivariate data analysis
                  "snow",#allow parallel processing
                  "BayesLogit",#required for Hmsc
                  "mvabund",#(experimental) multivariate data analysis
                  "dfoptim","optimx"#variety of optimizers for lme4 mixed mods
                  )

new_packages <- req_packages[!(req_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages,library=libfolder,type="binary")
proc.time() - ptm;rm(ptm,req_packages,new_packages)


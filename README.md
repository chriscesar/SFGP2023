# SFGP2023
Code for conducting analyses of data as part of the **Saltfleet to Gibraltar Point** Environmental Monitoring for 2023

Annual beach nourishment is conducted along the Lincolnshire coastline and annual reporting is conducted to assess the impacts of annual beach nourishment activity on intertidal and subtidal biological and abiotic receptor variables.

The monitoring design was established in the mid/late 1990s and the current monitoring appraoch is based on this
initial design.

## Analyses conducted
The scripts within this repo perform the following tasks:
* Import of new data from annual monitoring work
* Formatting new data and merging with existant data sets
* Statistical analysis of data to infer impacts
* Produce graphical and statistical outputs to visualise analysis outputs

## Variables considered
The analyses conducted cover a range of variables, encompassing intertidal and subtidal receptors.
Novel analyses for this year's report includes analysis of biological traits for intertidal time series data.

### General
* Summary of annual nourishment activity/volume of sediment introduced

### Intertidal
Intertidal core surveys, aimed at gathering the following:
* Infaunal diversity and multivariate analyses
* Particle size analyses and sediment destributional statistics
* Abiotic parameters (chemical)
* Beach descriptors (beach slopes, sediment compaction, sediment descriptor, wave type)
* Temporal trends of *Cerastoderma* fishery stocks (based on data provided by the local IFCA)

### Subtidal
Benthic beam trawl surveys, aimed at gathering the following:
* Epifaunal taxon counts and diversity analyses
* Shrimp (*Crangon* spp.) demographics

## Naming convention
Scripts (as stored in the *R* folder) are named using the following convention:
* Beginning with *00_* = metadata and package/function loading
* Beginning with *1xx_* = data import
* Beginning with *2xx_* = data processing and visualisation

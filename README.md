# WoodyEncroachmentHub

## Description
This repo contains data and code for the manuscript "Woody plant encroachment intensifies under climate change across tundra and savanna biomes".

## Authors
Mariana García Criado, Isla H. Myers-Smith, Anne D. Bjorkman, Caroline E.R. Lehmann and Nicola Stevens

Contact: Mariana García Criado, mariana.garcia.criado@gmail.com

## Data use guidelines
tbc, license

## Data availability & access
The data and code for this manuscript will be mantained at this GitHub repository (https://github.com/marianagarciacriado/WoodyEncroachmentHub)

## Citation
tbc

## Acknowledgments from the manuscript
M.G.C. was supported by the Principal Career’s Development PhD Scholarship from The University of Edinburgh. I.M.S. was supported by the NERC Shrub Tundra grant (NE/M016323/1). We thank all tundra and savanna data collectors, including members of the International Tundra Experiment Network (ITEX), for their efforts in data collection and for making their data accessible. We are grateful to Jakob Assmann for his assistance in extracting and manipulating the CHELSA climatic data. We thank local and indigenous peoples of the tundra and savanna biomes for the opportunity to work with data collected on their lands.

## Data
All data for our analyses can be found in the mastersheets folder. More details?

## Scripts
All the analyses undertaken for this manuscript are split between multiple R scripts. 
They can be followed in a sequential order (i.e., 01 to 12), but I recommend not to run scripts #5 and most of script #6 (until which line?) as they deal with the extraction and manipulation of raster data from CHELSA (http://chelsa-climate.org/). Both the climatologies and time series are large files and are thus stored in a hard drive so it is not possible to run the scripts without access to the climatic raster files.

## Figures
The figures generated in R are stored in the figures folder.

## Model outputs
Full model outputs for statistical analyses are stored in the models folder.

## Software requirements
R version 3.4.3 or greater.

R packages: MCMCglmm, ggplot2, plyr, gridExtra, dplyr, tidyr, stargazer, cowplot, broom, tidyverse, ggbpubr, maps, mapdata, mapproj, proj4, scales, ggalt, raster, rgdal, rasterVis, sp, readr, grid, MCMCvis

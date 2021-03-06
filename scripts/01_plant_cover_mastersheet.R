#### Woody encroachment across biomes 
#### Script 01. Plant cover mastersheet creation
#### Mariana García Criado 
#### March 2019


## LIBRARIES ----
library(tidyverse)


## MASTERSHEET CREATION ----

## Loading biome-specific mastersheets with the compiled data
tun.ms <- read.csv("mastersheets/tundra_mastersheet.csv")
itex.ms <- read.csv("mastersheets/itex_mastersheet.csv")
sav.ms <- read.csv("mastersheets/savanna_mastersheet.csv")


## Merging tundra mastersheets (tundra data from the literature + ITEX data)
tun.ms <- dplyr::select(tun.ms, Biome_type, Country, Site, Latitude, Longitude, Citation, Species, Species_Group, Time_period, 
                 Start_year, End_year, Start_cover, End_cover, Total_cover_change, Annual.rate, Method)
itex.ms <- dplyr::select(itex.ms, Biome_type, Country, Site, Latitude, Longitude, Citation, Species, Species_Group, Time_period, 
                 Start_year, End_year, Start_cover, End_cover, Total_cover_change, Annual.rate, Method)
tun.merg <- rbind(tun.ms, itex.ms)

# Add an unique Plot.ID identifier to each record
tun.merg$Plot.ID <- paste0("t", seq(1:length(tun.merg$Biome_type)))

# Add a 'Continent' column
tun.merg$Continent[tun.merg$Country %in% c("Canada", "Greenland", "USA")] <- "North America"
tun.merg$Continent[tun.merg$Country %in% c("Svalbard", "Sweden", "Switzerland", "Faroe Islands", "Italy", "Iceland", "Norway")] <- "Europe"
tun.merg$Continent[tun.merg$Country %in% c("Russia", "Japan", "Tibet")] <- "Asia"

# Add an unique Plot.ID identifier to each savanna record and select relevant columns
sav.ms$Plot.ID <- paste0("s", seq(1:length(sav.ms$Biome_type)))

sav.ms <- dplyr::select(sav.ms, Biome_type, Country, Site, Latitude, Longitude, Citation, Species, Species_Group, Time_period, 
                 Start_year, End_year, Start_cover, End_cover, Total_cover_change, Annual.rate, Method, Plot.ID, Continent)


## Merge into a single mastersheet
cover.ms <- rbind(tun.merg, sav.ms)

# Add overall 'trend' column
cover.ms$Trend <- ifelse(cover.ms$Annual.rate < -0.01, 'Decrease',
                         ifelse(cover.ms$Annual.rate >= -0.01 & cover.ms$Annual.rate <= 0.01, 'Stable',
                                ifelse(cover.ms$Annual.rate > 0.01, 'Increase', 'other')))

# Add 'Biome trend' column
cover.ms$Biome_trend[cover.ms$Biome_type == "Savanna" & cover.ms$Trend == "Decrease"] <- "Savanna_decrease"
cover.ms$Biome_trend[cover.ms$Biome_type == "Savanna" & cover.ms$Trend == "Stable"] <- "Savanna_stable"
cover.ms$Biome_trend[cover.ms$Biome_type == "Savanna" & cover.ms$Trend == "Increase"] <- "Savanna_increase"

cover.ms$Biome_trend[cover.ms$Biome_type == "Tundra" & cover.ms$Trend == "Decrease"] <- "Tundra_decrease"
cover.ms$Biome_trend[cover.ms$Biome_type == "Tundra" & cover.ms$Trend == "Increase"] <- "Tundra_increase"
cover.ms$Biome_trend[cover.ms$Biome_type == "Tundra" & cover.ms$Trend == "Stable"] <- "Tundra_stable"

# Save mastersheet with NAs
write.csv(cover.ms, "mastersheets/cover_ms_with_na.csv")


## Create mastersheet without NAs
cover.ms.no.na <- cover.ms %>% drop_na(Annual.rate)

# Create new Plot.ID identifiers
cover.ms.no.na <- subset(cover.ms.no.na, select = -Plot.ID)

cover.ms.no.na$Plot.ID[cover.ms.no.na$Biome_type == "Tundra"] <- paste0("t", seq(1:length(cover.ms.no.na$Biome_type == "Tundra")))
cover.ms.no.na$Plot.ID[cover.ms.no.na$Biome_type == "Savanna"] <- paste0("s", seq(1:length(cover.ms.no.na$Biome_type == "Savanna")))

# Save mastersheet without NAs
write.csv(cover.ms.no.na, "mastersheets/cover_ms_clean.csv")


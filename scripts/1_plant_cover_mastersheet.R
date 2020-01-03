#### Woody encroachment across biomes 
#### Script 1. Plant cover mastersheet creation
#### Mariana Garcia Criado 
#### March 2019


## Libraries
#.libPaths("C:/R_library")
library(tidyverse)

## Loading biome-specific mastersheets
tun.ms <- read.csv("scripts/users/mgarciacriado/encroachment_paper/mastersheets/Supershinynewtundramastersheet.csv")
itex.ms <- read.csv("scripts/users/mgarciacriado/encroachment_paper/itex/itex_shrubs_v2_with_references.csv")
sav.ms <- read.csv("scripts/users/mgarciacriado/encroachment_paper/mastersheets/Supershinynewsavannamastersheet.csv")

## Merging tundra mastersheets
tun.ms <- dplyr::select(tun.ms, Biome_type, Country, Site, Latitude, Longitude, Citation, Species, Species_Group, Time_period, 
                 Start_year, End_year, Start_cover, End_cover, Total_cover_change, Annual.rate, Method)
itex.ms <- dplyr::select(itex.ms, Biome_type, Country, Site, Latitude, Longitude, Citation, Species, Species_Group, Time_period, 
                 Start_year, End_year, Start_cover, End_cover, Total_cover_change, Annual.rate, Method)
tun.merg <- rbind(tun.ms, itex.ms)

# Add an unique Plot.ID identifier to each record
tun.merg$Plot.ID <- paste0("t", seq(1:length(tun.merg$Biome_type)))

# Add continent as a column
tun.merg$Continent[tun.merg$Country %in% c("Canada", "Greenland", "USA")] <- "North America"
tun.merg$Continent[tun.merg$Country %in% c("Svalbard", "Sweden", "Switzerland", "Faroe Islands", "Italy", "Iceland", "Norway")] <- "Europe"
tun.merg$Continent[tun.merg$Country %in% c("Russia", "Japan", "Tibet")] <- "Asia"

# Add an unique Plot.ID identifier to each savanna record
sav.ms$Plot.ID <- paste0("s", seq(1:length(sav.ms$Biome_type)))

sav.ms <- dplyr::select(sav.ms, Biome_type, Country, Site, Latitude, Longitude, Citation, Species, Species_Group, Time_period, 
                 Start_year, End_year, Start_cover, End_cover, Total_cover_change, Annual.rate, Method, Plot.ID, Continent)


## Merge into a single mastersheet
cover.ms <- rbind(tun.merg, sav.ms)

# Add 'trend' column
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

## Mastersheet with NAs
write.csv(cover.ms, "scripts/users/mgarciacriado/encroachment_paper/final_scripts/mastersheets/cover_ms_with_na.csv")

## Mastersheet without NAs
cover.ms.no.na <- cover.ms %>% drop_na(Annual.rate)

# Create new Plot.ID identifiers
cover.ms.no.na <- subset(cover.ms.no.na, select = -Plot.ID)

cover.ms.no.na$Plot.ID[cover.ms.no.na$Biome_type == "Tundra"] <- paste0("t", seq(1:length(cover.ms.no.na$Biome_type == "Tundra")))
cover.ms.no.na$Plot.ID[cover.ms.no.na$Biome_type == "Savanna"] <- paste0("s", seq(1:length(cover.ms.no.na$Biome_type == "Savanna")))

# Save mastersheet without NAs
write.csv(cover.ms.no.na, "scripts/users/mgarciacriado/encroachment_paper/final_scripts/mastersheets/cover_ms_clean.csv")



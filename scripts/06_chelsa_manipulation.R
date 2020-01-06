#### Woody encroachment across biomes
#### Script 06. CHELSA climatic data preparation
#### Mariana García Criado
#### November 2018

## LIBRARIES ----
library(raster)
library(rgdal)
library(rasterVis)
library(sp)
library(tidyr) # load tidyr before dplyr
library(readr)
library(dplyr)
library(broom)

## NB. This script can be skipped (until line 333) when running all scripts sequentially, since it requires access to the downloaded
## climatologies and time series raster files from CHELSA which are not stored online due to their large sizes.
## The original climate data can be downloaded directly from the CHELSA website (http://chelsa-climate.org/)


## MAT DATA EXTRACTION ----

# defining the filepath
folderpath_mat <- "D:/chelsa_mat"
filenames_mat <- list.files(folderpath_mat, pattern = "*.tif")
filepath_mat = paste0(folderpath_mat, "/", filenames_mat)

# loading the mastersheet that contains the coordinates
mastersheet <- read.csv("mastersheets/cover_ms_clim_coord.csv")

# extract the coordinates as a spatial points object 
latlong <- mastersheet %>% dplyr::select(Longitude, Latitude) %>% drop_na() %>% distinct() %>% SpatialPoints()

# create raster stack
mat_stack <- stack(filepath_mat)

# extract MAT values for each pair of coordinates
master <- mat_stack %>% raster::extract(., latlong, df = TRUE)

# convert the SpatialPoints object into a dataframe 
latlong2 <- as.data.frame(latlong)

# reassign the ID to the latlong and assign it to the mastersheet too
latlong2$ID <- row.names(latlong2)
#mastersheet$ID <- row.names(mastersheet)

# merge the two dataframes
master_mat <- merge(master, latlong2, by = c("ID"))
#master_mat <- merge(mastersheet, master_mat, by = c("ID", "lat", "lon"))

# reshape from wide to long format 
master_mat <- gather(data = master_mat, key = "var_year", value = "value", select = 2:36)
master_mat <- master_mat %>% separate(var_year, c("CHELSA", "variable", "year"))
master_mat <- dplyr::select(master_mat, -CHELSA)

# convert to celsius from kelvin
master_mat$value <- master_mat$value/10 - 273.15

# save dataframe as a file
write.csv(master_mat, "data/master_mat.csv")


## MAP DATA EXTRACTION ----

# define file paths for MAP
folderpath_map <- "D:/chelsa_precip/precip_annual"
filenames_map <- list.files(folderpath_map, pattern = "*.tif")
filepath_map = paste0(folderpath_map, "/", filenames_map)

# create raster stack
map_stack <- stack(filepath_map)

# extract MAP values for each pair of coordinates
master2 <- map_stack %>% raster::extract(., latlong, df = TRUE)

# merge the two dataframes
master_map <- merge(master2, latlong2, by = c("ID"))

# reshape from wide to long format 
master_map <- gather(data = master_map, key = "var_year", value = "value", select = 2:36)
master_map <- master_map %>% separate(var_year, c("CHELSA", "variable", "year"))
master_map <- dplyr::select(master_map, -CHELSA)

# save dataframe as a file
write.csv(master_map, "data/master_map.csv")


## JAN&FEB MAT DATA EXTRACTION ----

# define file paths
folderpath_mat_janfeb <- "D:/chelsa_mat_janfeb"
filenames_mat_janfeb <- list.files(folderpath_mat_janfeb, pattern = "*.tif")
filepath_mat_janfeb = paste0(folderpath_mat_janfeb, "/", filenames_mat_janfeb)

# create raster stack
mat_janfeb_stack <- stack(filepath_mat_janfeb)

# extract MAP values for each pair of coordinates
master3 <- mat_janfeb_stack %>% raster::extract(., latlong, df = TRUE)

# merge the two dataframes
master_mat_janfeb <- merge(master3, latlong2, by = c("ID"))

# reshape from wide to long format 
master_mat_janfeb <- gather(data = master_mat_janfeb, key = "var_year", value = "value", select = 2:36)
master_mat_janfeb <- master_mat_janfeb %>% separate(var_year, c("CHELSA", "variable", "months", "year"))
master_mat_janfeb$variable <- paste0(master_mat_janfeb$variable, "_", master_mat_janfeb$months)
master_mat_janfeb <- dplyr::select(master_mat_janfeb, -CHELSA, -months)

# convert to celsius from kelvin
master_mat_janfeb$value <- master_mat_janfeb$value/10 - 273.15

# save dataframe as a file
write.csv(master_mat_janfeb, "data/master_mat_janfeb.csv")


## JUNE&JULY MAT EXTRACTION ----

# define file paths
folderpath_mat_junjul <- "D:/chelsa_mat_junjul"
filenames_mat_junjul <- list.files(folderpath_mat_junjul, pattern = "*.tif")
filepath_mat_junjul = paste0(folderpath_mat_junjul, "/", filenames_mat_junjul)

# create raster stack
mat_junjul_stack <- stack(filepath_mat_junjul)

# extract MAP values for each pair of coordinates
master4 <- mat_junjul_stack %>% raster::extract(., latlong, df = TRUE)

# merge the two dataframes
master_mat_junjul <- merge(master4, latlong2, by = c("ID"))

# reshape from wide to long format 
master_mat_junjul <- gather(data = master_mat_junjul, key = "var_year", value = "value", select = 2:36)
master_mat_junjul <- master_mat_junjul %>% separate(var_year, c("CHELSA", "variable", "months", "year"))
master_mat_junjul$variable <- paste0(master_mat_junjul$variable, "_", master_mat_junjul$months)
master_mat_junjul <- dplyr::select(master_mat_junjul, -CHELSA, -months)

# convert to celsius from kelvin
master_mat_junjul$value <- master_mat_junjul$value/10 - 273.15

# save dataframe as a file
write.csv(master_mat_junjul, "data/master_mat_junjul.csv")


## JAN&FEB MAP DATA EXTRACTION ----

# define file paths
folderpath_map_janfeb <- "D:/chelsa_map_janfeb"
filenames_map_janfeb <- list.files(folderpath_map_janfeb, pattern = "*.tif")
filepath_map_janfeb = paste0(folderpath_map_janfeb, "/", filenames_map_janfeb)

# create raster stack
map_janfeb_stack <- stack(filepath_map_janfeb)

# extract MAP values for each pair of coordinates
master5 <- map_janfeb_stack %>% raster::extract(., latlong, df = TRUE)

# merge the two dataframes
master_map_janfeb <- merge(master5, latlong2, by = c("ID"))

# reshape from wide to long format 
master_map_janfeb <- gather(data = master_map_janfeb, key = "var_year", value = "value", select = 2:36)
master_map_janfeb <- master_map_janfeb %>% separate(var_year, c("CHELSA", "variable", "months", "year"))
master_map_janfeb$variable <- paste0(master_map_janfeb$variable, "_", master_map_janfeb$months)
master_map_janfeb <- dplyr::select(master_map_janfeb, -CHELSA, -months)

# save dataframe as a file
write.csv(master_map_janfeb, "data/master_map_janfeb.csv")


## JUNE&JULY MAP DATA EXTRACTION ----

# define file paths
folderpath_map_junjul <- "D:/chelsa_map_junjul"
filenames_map_junjul <- list.files(folderpath_map_junjul, pattern = "*.tif")
filepath_map_junjul = paste0(folderpath_map_junjul, "/", filenames_map_junjul)

# create raster stack
map_junjul_stack <- stack(filepath_map_junjul)

# extract MAP values for each pair of coordinates
master6 <- map_junjul_stack %>% raster::extract(., latlong, df = TRUE)

# merge the two dataframes
master_map_junjul <- merge(master6, latlong2, by = c("ID"))

# reshape from wide to long format 
master_map_junjul <- gather(data = master_map_junjul, key = "var_year", value = "value", select = 2:36)
master_map_junjul <- master_map_junjul %>% separate(var_year, c("CHELSA", "variable", "months", "year"))
master_map_junjul$variable <- paste0(master_map_junjul$variable, "_", master_map_junjul$months)
master_map_junjul <- dplyr::select(master_map_junjul, -CHELSA, -months)

# save dataframe as a file
write.csv(master_map_junjul, "data/master_map_junjul.csv")



## MINIMUM TEMP DATA EXTRACTION ----

# defining the filepath
folderpath_mat_minmax <- "D:/chelsa_tmean"
filenames_mat_minmax <- list.files(folderpath_mat_minmax, pattern = "*.tif")
filepath_mat_minmax = paste0(folderpath_mat_minmax, "/", filenames_mat_minmax)

# create raster stack
mat_minmax_stack <- stack(filepath_mat_minmax)

# extract MAT values for each pair of coordinates
master7 <- mat_minmax_stack %>% raster::extract(., latlong, df = TRUE)

# merge the two dataframes
master_mat_minmax <- merge(master7, latlong2, by = c("ID"))

# reshape from wide to long format 
master_mat_minmax <- master_mat_minmax %>% gather(key = "var_year", value = "value", select = 2:421) %>% 
  separate(var_year, c("CHELSA", "variable", "year", "months")) %>% dplyr::select(-CHELSA)

# convert to celsius from kelvin
master_mat_minmax$value <- master_mat_minmax$value/10 - 273.15

write.csv(master_mat_minmax, "data/master_mat_minmax.csv")

# identify the month with the lowest temperature for each site and year
master_mat_minmax <- read.csv("data/master_mat_minmax.csv")

master_mat_min <- master_mat_minmax %>% group_by(ID, Longitude, Latitude, year) %>% summarise(mintemp = min(value)) %>% ungroup()
#master_mat_min_merge <- semi_join(master_mat_min, master_mat_minmax)

# change the format to match the rest of dataframes
master_mat_min$variable <- "mat_min"
colnames(master_mat_min)[colnames(master_mat_min)=="mintemp"] <- "value"
master_mat_min <- master_mat_min[c("ID", "Longitude", "Latitude", "variable", "year", "value")]

# save dataframe as a file
write.csv(master_mat_min, "data/master_mat_min.csv")


## MAXIMUM TEMP DATA EXTRACTION ----
master_mat_minmax

# identify the month with the highest temperature for each site and year
master_mat_max <- master_mat_minmax %>% group_by(ID, Longitude, Latitude, year) %>% summarise(maxtemp = max(value)) %>% ungroup()
#master_mat_max_merge <- semi_join(master_mat_max, master_mat_minmax)

# change the format to match the rest of dataframes
master_mat_max$variable <- "mat_max"
colnames(master_mat_max)[colnames(master_mat_max)=="maxtemp"] <- "value"
master_mat_max <- master_mat_max[c("ID", "Longitude", "Latitude", "variable", "year", "value")]

# save dataframe as a file
write.csv(master_mat_max, "data/master_mat_max.csv")


## MAXIMUM PRECIP DATA EXTRACTION ----

# defining the filepath
folderpath_map_minmax <- "D:/chelsa_precip/precip_timeseries"
filenames_map_minmax <- list.files(folderpath_map_minmax, pattern = "*.tif")
filepath_map_minmax = paste0(folderpath_map_minmax, "/", filenames_map_minmax)

# create raster stack
map_minmax_stack <- stack(filepath_map_minmax)

# extract MAT values for each pair of coordinates
master8 <- map_minmax_stack %>% raster::extract(., latlong, df = TRUE)

# merge the two dataframes
master_map_minmax <- merge(master8, latlong2, by = c("ID"))

# reshape from wide to long format 
master_map_minmax <- gather(data = master_map_minmax, key = "var_year", value = "value", select = 2:421)
master_map_minmax <- master_map_minmax %>% separate(var_year, c("CHELSA", "variable", "year", "months"))
master_map_minmax <- dplyr::select(master_map_minmax, -CHELSA)

# identify the month with the highest precipitation for each site and year
master_map_max <- master_map_minmax %>% group_by(ID, Longitude, Latitude, year) %>% summarise(maxprec = max(value)) %>% ungroup()
#master_map_max_merge <- semi_join(master_map_max, master_map_minmax)

# change the format to match the rest of dataframes
master_map_max$variable <- "map_max"
colnames(master_map_max)[colnames(master_map_max)=="maxprec"] <- "value"
master_map_max <- master_map_max[c("ID", "Longitude", "Latitude", "variable", "year", "value")]

# save dataframe as a file
write.csv(master_map_max, "data/master_map_max.csv")


## MINIMUM PRECIP DATA EXTRACTION ----

# identify the month with the lowest precipitation for each site and year
master_map_min <- master_map_minmax %>% group_by(ID, Longitude, Latitude, year) %>% summarise(minprec = min(value)) %>% ungroup()
#master_map_min_merge <- semi_join(master_map_min, master_map_minmax)

# change the format to match the rest of dataframes
master_map_min$variable <- "map_min"
colnames(master_map_min)[colnames(master_map_min)=="minprec"] <- "value"
master_map_min <- master_map_min[c("ID", "Longitude", "Latitude", "variable", "year", "value")]

# save dataframe as a file
write.csv(master_map_min, "data/master_map_min.csv")



## DATAFRAME COMBINATION ----
master_map <- read.csv("data/master_map.csv")
master_mat <- read.csv("data/master_mat.csv")
master_mat_janfeb <- read.csv("data/master_mat_janfeb.csv")
master_mat_junjul <- read.csv("data/master_mat_junjul.csv")
master_map_janfeb <- read.csv("data/master_map_janfeb.csv")
master_map_junjul <- read.csv("data/master_map_junjul.csv")
master_mat_min <- read.csv("data/master_mat_min.csv")
master_mat_max <- read.csv("data/master_mat_max.csv")
master_map_max <- read.csv("data/master_map_max.csv")
master_map_min <- read.csv("data/master_map_min.csv")

master_clima <- rbind(master_map, master_mat, master_mat_janfeb, master_mat_junjul, master_map_janfeb, 
                      master_map_junjul, master_mat_min, master_mat_max, master_map_max, master_map_min)


## Site-Specific Timeseries (SST) dataframe
# adding 4 years prior the start date to account for time lags
mastersheet.seq <- mastersheet %>% group_by(Plot.ID) %>% 
  mutate(year.b4 = min(Start_year) - 4) %>% mutate(year = list(seq(year.b4, End_year))) %>% unnest()

write.csv(mastersheet.seq, "mastersheets/mastersheet_seq.csv")

# merge mastersheet.seq with master_clima
clima.fit <- merge(mastersheet.seq, master_clima, by = c("Longitude", "Latitude", "year"))
write.csv(clima.fit, "mastersheets/clima_fit.csv")


## Extracting slopes (annual change in climatic variables) for each record 
clima.fit <- read.csv("mastersheets/clima_fit.csv")

sst.fit.slopes <- clima.fit %>%
  group_by(., variable, Plot.ID) %>%
  do(mod = lm(value ~ year, data = .)) %>%
  tidy(mod) %>%
  filter(term == "year") %>%
  dplyr::select(variable, Plot.ID, estimate)

write.csv(sst.fit.slopes, "mastersheets/sst_fit_slopes.csv")

# Merge the slopes with the vegetation/trend data in one final object
clima.fit.sst <- clima.fit %>% dplyr::select(Biome_type, Plot.ID, Latitude, Longitude, tempc, precip, 
                                             variable, Annual.rate, Trend, Biome_trend, geo.coords)
clima.fit.sst.full <- inner_join(clima.fit.sst, sst.fit.slopes, by = c("Plot.ID" = "Plot.ID", "variable" = "variable"))
clima.fit.sst.final <- distinct(clima.fit.sst.full)
write.csv(clima.fit.sst.final, "mastersheets/clima_fit_sst.csv")

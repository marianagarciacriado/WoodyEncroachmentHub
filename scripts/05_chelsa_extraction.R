#### Woody encroachment across biomes
#### Script 05. CHELSA raster data extraction 
#### Mariana García Criado with help from Jakob Assmann
#### October 2018

## LIBRARIES ----
library(raster)
library(rgdal)
library(rasterVis)
library(sp)

## NB. This script can be skipped when running all scripts sequentially, since it requires access to the downloaded
## climatologies and time series raster files from CHELSA which are not stored online due to their large sizes.
## Thse data can be downloaded directly from the CHELSA website (http://chelsa-climate.org/)
## For the actual climatic analysis, see script named '07_climatic_timeseries'.

## Mean Annual Temperature (MAT) ----

# Define path to timeseries
path_to_timeseries <- "D:/Mariana/temperature/"
test <- raster(paste0(path_to_timeseries, "CHELSA_temp_1_1979_V1.2.1.tif"))

# Define the function for the whole process in a linear way
calculate_mat <- function(year){ #for each year, do the following 
  if(!file.exists(paste0(path_to_timeseries, "CHELSA_mat_", year, ".tif"))){
    months <- 1:12 #for each month
    filenames <- paste0(path_to_timeseries, "CHELSA_temp_", months, "_", year, "_V1.2.1.tif") #create a list of names for each year and month
    print(filenames)
    month_stack <- stack(filenames) #stack all monthly raster files for each year
    crs(month_stack) <- CRS("+proj=longlat +datum=WGS84 +no_defs") # Set CRS (as chelsa files have not set it by default, grabbed CRS from chelsa website)
    year_mean <- mean(month_stack) #calculate the mean temperature for each year
    writeRaster(year_mean, paste0(path_to_timeseries, "CHELSA_mat_", year, ".tif")) #create a raster file for each year with MAT value
    print(paste("Saved", year)) #returns a message to report on progress on console
    return(paste("Done with", year)) #return value of function to be stored on list
  }
  else {
    print(paste("File exsits for", year, "moving on..."))
    return(paste("Done with", year))
  }
}
for(i in 1996:2013){
  calculate_mat(i)
}

# .tif files are created and stored in the hard drive (D:/chelsa_mat). We can work with them now!


## Mean Annual Precipitation (MAP) ----
path_to_map_timeseries <- "D:/chelsa_precip/precip_timeseries/"
test2 <- raster(paste0(path_to_map_timeseries, "CHELSA_prec_1979_01_V1.2.1.tif"))

# Define the function for the whole process and run it linearly:
calculate_map <- function(year){ #for each year, do the following 
  if(!file.exists(paste0(path_to_map_timeseries, "CHELSA_map_", year, ".tif"))){
    months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12") #for each month
    filenames <- paste0(path_to_map_timeseries, "CHELSA_prec_", year, "_", months, "_V1.2.1.tif") #create a list of names for each year and month
    print(filenames)
    month_stack <- stack(filenames) #stack all monthly raster files for each year
    crs(month_stack) <- CRS("+proj=longlat +datum=WGS84 +no_defs") # Set CRS (as chelsa files have not set it by default, grabbed CRS from chelsa website)
    year_mean <- mean(month_stack) #calculate the mean temperature for each year
    writeRaster(year_mean, paste0(path_to_map_timeseries, "CHELSA_map_", year, ".tif")) #create a raster file for each year with MAT value
    print(paste("Saved", year)) #returns a message to report on progress on console
    return(paste("Done with", year)) #return value of function to be stored on list
  }
  else {
    print(paste("File exists for", year, "moving on..."))
    return(paste("Done with", year))
  }
}
for(i in 1979:2013){
  calculate_map(i)
}

# MAP files are stored in the hard drive (D:/chelsa_precip/precip_annual)



## Jan&Feb Mean Temperature ----
path_to_mat_janfeb <- "D:/chelsa_tmean/"
test3 <- raster(paste0(path_to_mat_janfeb, "CHELSA_tmean_1979_01_V1.2.1.tif"))

# Define the function for the whole process and run it linearly:
calculate_mat_janfeb <- function(year){ #for each year, do the following 
  if(!file.exists(paste0(path_to_mat_janfeb, "CHELSA_mat_janfeb_", year, ".tif"))){
    months <- c("01", "02") #for each month
    filenames <- paste0(path_to_mat_janfeb, "CHELSA_tmean_", year, "_", months, "_V1.2.1.tif") #create a list of names for each year and month
    print(filenames)
    month_stack <- stack(filenames) #stack all monthly raster files for each year
    crs(month_stack) <- CRS("+proj=longlat +datum=WGS84 +no_defs") # Set CRS (as chelsa files have not set it by default, grabbed CRS from chelsa website)
    year_mean <- mean(month_stack) #calculate the mean temperature for each year
    writeRaster(year_mean, paste0(path_to_mat_janfeb, "CHELSA_mat_janfeb_", year, ".tif")) #create a raster file for each year with MAT value
    print(paste("Saved", year)) #returns a message to report on progress on console
    return(paste("Done with", year)) #return value of function to be stored on list
  }
  else {
    print(paste("File exists for", year, "moving on..."))
    return(paste("Done with", year))
  }
}
for(i in 1979:2013){
  calculate_mat_janfeb(i)
}

# Jan-Feb temp files are stored in the hard drive (D:/chelsa_mat_janfeb)


## June&July Mean Temperature ----
path_to_mat_junjul <- "D:/chelsa_tmean/"
test4 <- raster(paste0(path_to_mat_junjul, "CHELSA_tmean_1979_06_V1.2.1.tif"))

# Define the function for the whole process and run it linearly:
calculate_mat_junjul <- function(year){ #for each year, do the following 
  if(!file.exists(paste0(path_to_mat_junjul, "CHELSA_mat_junjul_", year, ".tif"))){
    months <- c("06", "07") #for each month
    filenames <- paste0(path_to_mat_junjul, "CHELSA_tmean_", year, "_", months, "_V1.2.1.tif") #create a list of names for each year and month
    print(filenames)
    month_stack <- stack(filenames) #stack all monthly raster files for each year
    crs(month_stack) <- CRS("+proj=longlat +datum=WGS84 +no_defs") # Set CRS (as chelsa files have not set it by default, grabbed CRS from chelsa website)
    year_mean <- mean(month_stack) #calculate the mean temperature for each year
    writeRaster(year_mean, paste0(path_to_mat_junjul, "CHELSA_mat_junjul_", year, ".tif")) #create a raster file for each year with MAT value
    print(paste("Saved", year)) #returns a message to report on progress on console
    return(paste("Done with", year)) #return value of function to be stored on list
  }
  else {
    print(paste("File exists for", year, "moving on..."))
    return(paste("Done with", year))
  }
}
for(i in 1979:2013){
  calculate_mat_junjul(i)
}

# Jun-Jul temp files are stored in the hard drive (D:/chelsa_mat_junjul)



## Jan&Feb Mean Precipitation ----
path_to_map_janfeb <- "D:/chelsa_precip/precip_timeseries/"
test4 <- raster(paste0(path_to_map_janfeb, "CHELSA_prec_1979_01_V1.2.1.tif"))

# Define the function for the whole process and run it linearly:
calculate_map_janfeb <- function(year){ #for each year, do the following 
  if(!file.exists(paste0(path_to_map_janfeb, "CHELSA_map_janfeb_", year, ".tif"))){
    months <- c("01", "02") #for each month
    filenames <- paste0(path_to_map_janfeb, "CHELSA_prec_", year, "_", months, "_V1.2.1.tif") #create a list of names for each year and month
    print(filenames)
    month_stack <- stack(filenames) #stack all monthly raster files for each year
    crs(month_stack) <- CRS("+proj=longlat +datum=WGS84 +no_defs") # Set CRS (as chelsa files have not set it by default, grabbed CRS from chelsa website)
    year_mean <- mean(month_stack) #calculate the mean temperature for each year
    writeRaster(year_mean, paste0(path_to_map_janfeb, "CHELSA_map_janfeb_", year, ".tif")) #create a raster file for each year with MAT value
    print(paste("Saved", year)) #returns a message to report on progress on console
    return(paste("Done with", year)) #return value of function to be stored on list
  }
  else {
    print(paste("File exists for", year, "moving on..."))
    return(paste("Done with", year))
  }
}
for(i in 1979:2013){
  calculate_map_janfeb(i)
}

# Jan-Feb precip files are stored in the hard drive (D:/chelsa_map_janfeb)


## June&July Mean Precipitation ----
path_to_map_junjul <- "D:/chelsa_precip/precip_timeseries/"
test5 <- raster(paste0(path_to_map_junjul, "CHELSA_prec_1979_06_V1.2.1.tif"))

# Define the function for the whole process and run it linearly:
calculate_map_junjul <- function(year){ #for each year, do the following 
  if(!file.exists(paste0(path_to_map_junjul, "CHELSA_map_junjul_", year, ".tif"))){
    months <- c("06", "07") #for each month
    filenames <- paste0(path_to_map_junjul, "CHELSA_prec_", year, "_", months, "_V1.2.1.tif") #create a list of names for each year and month
    print(filenames)
    month_stack <- stack(filenames) #stack all monthly raster files for each year
    crs(month_stack) <- CRS("+proj=longlat +datum=WGS84 +no_defs") # Set CRS (as chelsa files have not set it by default, grabbed CRS from chelsa website)
    year_mean <- mean(month_stack) #calculate the mean temperature for each year
    writeRaster(year_mean, paste0(path_to_map_junjul, "CHELSA_map_junjul_", year, ".tif")) #create a raster file for each year with MAT value
    print(paste("Saved", year)) #returns a message to report on progress on console
    return(paste("Done with", year)) #return value of function to be stored on list
  }
  else {
    print(paste("File exists for", year, "moving on..."))
    return(paste("Done with", year))
  }
}
for(i in 1979:2013){
  calculate_map_junjul(i)
}

# Jun-July precip files are stored in the hard drive (D:/chelsa_map_junjul)

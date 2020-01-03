#### Woody encroachment across biomes 
#### Script 12. Summary table - model outputs
#### Mariana Garcia
#### April 2019

## Libraries ----
.libPaths("C:/R_library")
library(tidyverse)
library(broom)
library(MCMCglmm)
library(stargazer)

## NB. All models below have been created in scripts 1-12. However not all models are pushed on the GitHub repo because of their large
## size. These models are the following: map_sav_dw, mat_tun_dw, sens_map_sav, sens_mat_sav. In any case it is not necessary for the 
## creation of the table to have them in the repo since they can be loaded as objects from the environment if they have been previously
## created within the same R session.

## NB2. Something's up with the magnitude vs magnitude models, which don't appear correctly in this model table when I updated them.
## I have fitted them separately in a table in script 11.

## Defining functions ----

# Definining function to extract model outputs with random effects
clean.MCMC <- function(x) {
  sols <- summary(x)$solutions  ## pull out relevant info from model summary
  Gcovs <- summary(x)$Gcovariances
  Rcovs <- summary(x)$Rcovariances
  
  fixed <- data.frame(row.names(sols), sols, row.names = NULL)  ## convert to dataframes with the row.names as the first col
  random <- data.frame(row.names(Gcovs), Gcovs, row.names = NULL)
  residual <- data.frame(row.names(Rcovs), Rcovs, row.names = NULL)
  
  names(fixed)[names(fixed) == "row.names.sols."] <- "variable"  ## change the columns names to variable, so they all match
  names(random)[names(random) == "row.names.Gcovs."] <- "variable"
  names(residual)[names(residual) == "row.names.Rcovs."] <- "variable"
  
  fixed$effect <- "fixed"  ## add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  residual$effect <- "residual"
  
  modelTerms <- as.data.frame(bind_rows(fixed, random, residual))  # merge it all together
}

getName.MCMC <- function(x) deparse(substitute(x))


# Function for when models have no random effects
clean.MCMC.2 <- function(x) {
  sols <- summary(x)$solutions  # pull out relevant info from model summary
  Gcovs <- summary(x)$Gcovariances
  Rcovs <- summary(x)$Rcovariances
  
  fixed <- data.frame(row.names(sols), sols, row.names = NULL)  # convert to dataframes with the row.names as the first col
  residual <- data.frame(row.names(Rcovs), Rcovs, row.names = NULL)
  
  names(fixed)[names(fixed) == "row.names.sols."] <- "variable"  # change the columns names to variable, so they all match
  names(residual)[names(residual) == "row.names.Rcovs."] <- "variable"
  
  fixed$effect <- "fixed"  # add ID column for type of effect (fixed, random, residual)
  residual$effect <- "residual"
  
  modelTerms <- as.data.frame(bind_rows(fixed, residual))  # merge it all together
}



## Models with random effects ----
# Create list of input models with random effects
dataList <- list(tun.clim.mod, sav.clim.mod, mat.tun.mod, mat.sav.mod, 
                 mat.junjul.tun.mod, mat.junjul.sav.mod, mat.janfeb.tun.mod, mat.janfeb.sav.mod, 
                 map.tun.mod, map.sav.mod, map.janfeb.tun.mod, map.janfeb.sav.mod, 
                 map.junjul.tun.mod, map.junjul.sav.mod, mat.min.tun.mod, mat.min.sav.mod, 
                 mat.max.tun.mod, mat.max.sav.mod, map.min.tun.mod, map.min.sav.mod, 
                 map.max.tun.mod, map.max.sav.mod, 
                 tun.slop.mod, sav.slop.mod,
                 mat.tun.mag.mod, mat.sav.mag.mod, 
                 map.tun.mag.mod, map.sav.mag.mod, 
                 mat.tun.grad.mod, mat.sav.grad.mod, map.tun.grad.mod, map.sav.grad.mod, 
                 sav.rain, tun.rain, sav.mat.temp.mod, tun.mat.temp.mod)

# Create lists of input model names
dataListNames <- list("Climatology interaction Tundra", "Climatology interaction Savanna", "MAT Tundra rate vs rate", "MAT Savanna rate vs rate",
                      "MAT Jun-Jul Tundra rate vs rate", "MAT Jun-Jul Savanna rate vs rate", "MAT Jan-Feb Tundra rate vs rate", "MAT Jan-Feb Savanna rate vs rate", 
                      "MAP Tundra rate vs rate", "MAP Savanna rate vs rate", "MAP Jan-Feb Tundra rate vs rate", "MAP Jan-Feb Savanna rate vs rate",
                      "MAP Jun-Jul Tundra rate vs rate", "MAP Jun-Jul Savanna rate vs rate", "MAT Min Tundra rate vs rate", "MAT Min Savanna rate vs rate",
                      "MAT Max Tundra rate vs rate", "MAT Max Savanna rate vs rate", "MAP Min Tundra rate vs rate", "MAP Min Savanna rate vs rate", 
                      "MAP Max Tundra rate vs rate", "MAP Max Savanna rate vs rate", 
                      "MAT & MAP change interaction Tundra", "MAT & MAP change interaction Savanna",
                      "Magnitude vs magnitude MAT Tundra", "Magnitude vs magnitude MAT Savanna",
                      "Magnitude vs magnitude MAP Tundra", "Magnitude vs magnitude MAP Savanna",
                      "Precip x MAT change Tundra", "Precip x MAT change Savanna", "Temperature x MAP change Tundra", "Temperature x MAP change Savanna", 
                      "Precip x MAP change Savanna", "Precip x MAP change Tundra", "Temperature x MAT change Savanna", "Temperature x MAT change Tundra")


# Get model outputs and add model names
readyList <- mapply(cbind, lapply(dataList, clean.MCMC), "modelName" = dataListNames, SIMPLIFY = F)

# Turn list of dataframes into a dataframe
mcmc.outputs.random <- as.data.frame(do.call(rbind, readyList), stringsAsFactors = FALSE)



## Models without random effects ----

# Create list of input models with no random effects
dataList2 <- list(inc.means.mod, stab.means.mod, dec.means.mod, overall.mod, 
                  duration.biomes, 
                  dens.mat.model, dens.mat.junjul.mod, dens.mat.janfeb.mod, dens.map.model, 
                  dens.map.janfeb.mod, dens.map.junjul.mod, dens.mat.min, dens.mat.max, 
                  dens.map.min, dens.map.max,
                  length.mod.tun, length.mod.sav, 
                  group.tun.mod, group.sav.mod, sav.rainfall)

# Create lists of input model names
dataListNames2 <- list("Increasing cover means", "Stable cover means", "Decreasing cover means", "Overall cover change means",
                       "Study duration means",  "MAT change", "Jun-Jul MAT change", "Jan-Feb MAT change", "MAP change", 
                       "Jan-Feb MAP change", "Jun-Jul MAP change", "Min MAT change", "Max MAT change", 
                       "Min MAP change", "Max MAP change", 
                       "Study length and tundra cover", "Study length and savanna cover", 
                       "Species vs group Tundra", "Species vs group Savanna", "High vs low rainfall savannas")

# Get model outputs and add model names
readyList2 <- mapply(cbind, lapply(dataList2, clean.MCMC.2), "modelName" = dataListNames2, SIMPLIFY = F)

# Turn list of dataframes into a dataframe
mcmc.outputs.simple <- as.data.frame(do.call(rbind, readyList2), stringsAsFactors = FALSE)

# Combine outputs from models with and without random effects
mcmc.outputs.all <- rbind(mcmc.outputs.random, mcmc.outputs.simple)

# Add model number
mcmc.outputs.all.final <- transform(mcmc.outputs.all, ModelNumber = as.numeric(interaction(modelName)))

# Reorder columns
mcmc.outputs.all.final.real <- mcmc.outputs.all.final[, c(9, 8, 1, 7, 2, 3, 4, 5, 6)]

# Write csv
write.csv(mcmc.outputs.all.final.real, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/Table_S2.csv")

# Create nice table in html
stargazer(mcmc.outputs.all.final.real, title = "Summary of all models", type = "html", summary = FALSE, 
          out = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/figures/Table_S2.htm")

# Make it usable in Word 
# Copy the html code that appears on the console, click on File/New/R HTML file
# Delete everything in the newly generated file except the <html> and </html> tags and include the copied html code there
# Click Knit, save the html file
# The file can now be opened with Word for final touches

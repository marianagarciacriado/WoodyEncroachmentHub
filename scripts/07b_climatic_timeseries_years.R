#### Woody encroachment across biomes
#### Script 7b. Climatic timeseries models & figures - excluding sites from before 1979 and after 2013
#### Mariana Garcia
#### September 2019

## Libraries ----
.libPaths("C:/R_library")

library(tidyverse)
library(ggplot2)
library(broom)
library(MCMCglmm)
library(MCMCvis)
library(cowplot)
library(ggpubr)
library(grid)
library(gridExtra)

## CREATE OBJECTS ----

## Load database (from script #6)
clima.fit <- read.csv("scripts/users/mgarciacriado/encroachment_paper/mastersheets/clima_fit.csv")


# Create a new object including start and end years
clima.year <- clima.fit %>% dplyr::select(Biome_type, Plot.ID, Latitude, Longitude, tempc, precip, 
                                             variable, Annual.rate, Trend, Biome_trend, geo.coords, year.b4, End_year)

clima.years <- inner_join(clima.year, sst.fit.slopes, by = c("Plot.ID" = "Plot.ID", "variable" = "variable"))
clima.years.final <- distinct(clima.years)

write.csv(clima.years.final, "scripts/users/mgarciacriado/encroachment_paper/final_scripts/mastersheets/clima_years.csv")


## Remove studies which records started before 1979
clima.years.1979 <- subset(clima.years.final, year.b4 >= 1979)

## Remove studies which records started before 1979 and those that finished after 2013
clima.years.2013 <- subset(clima.years.1979, End_year <= 2013)


## Create individual climatic variable objects
mat_y <- filter(clima.years.2013, variable == "mat")
map_y <- filter(clima.years.2013, variable == "map")
mat_junjul_y <- filter(clima.years.2013, variable == "mat_junjul")
mat_janfeb_y <- filter(clima.years.2013, variable == 'mat_janfeb')
map_janfeb_y <- filter(clima.years.2013, variable == 'map_janfeb')
map_junjul_y <- filter(clima.years.2013, variable == 'map_junjul')
mat_min_y <- filter(clima.years.2013, variable == 'mat_min')
mat_max_y <- filter(clima.years.2013, variable == 'mat_max')
map_min_y <- filter(clima.years.2013, variable == 'map_min')
map_max_y <- filter(clima.years.2013, variable == 'map_max')






## FITTING MODELS EXCLUDING <1979 & >2013 ----

#### Mean Annual Temperature ----
mat.y.tundra <- filter(mat_y, Biome_type == "Tundra")
mat.y.sav <- filter(mat_y, Biome_type == "Savanna")


## Tundra: Fitting the model including location as a random factor and random intercepts
mat.y.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, prior = prior6,
                        data = mat.y.tundra, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)
summary(mat.y.tun.mod) #negative significant relationship, same as main model
plot(mat.y.tun.mod$Sol)
plot(mat.y.tun.mod$VCV)
autocorr.plot(mat.y.tun.mod$VCV)
hist(mcmc(mat.y.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.y.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat.y.tun.mod.RData")


## Savanna: Fitting the model with location as a random factorand random intercepts
mat.y.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, prior = prior6, 
                        data = mat.y.sav, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.y.sav.mod) #negative significant slope, contrary to main model which was non-significant
plot(mat.y.sav.mod$Sol)
plot(mat.y.sav.mod$VCV)
autocorr.plot(mat.y.sav.mod$VCV)
hist(mcmc(mat.y.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.y.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat.y.sav.mod.RData")






#### June-July Temperature ----
mat.junjul.y.tun <- filter(mat_junjul_y, Biome_type == "Tundra")
mat.junjul.y.sav <- filter(mat_junjul_y, Biome_type == "Savanna")

## Tundra: Fitting the model including biogeography as a random factor
mat.junjul.y.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = mat.junjul.y.tun, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.junjul.y.tun.mod) #negative non-significant (same as main)
plot(mat.junjul.y.tun.mod$Sol)
plot(mat.junjul.y.tun.mod$VCV)
autocorr.plot(mat.junjul.y.tun.mod$VCV)
hist(mcmc(mat.junjul.y.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.junjul.y.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat.junjul.y.tun.mod.RData")


## Savanna: Fitting the model with biogeography as a random factor
mat.junjul.y.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = mat.junjul.y.sav, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.junjul.y.sav.mod) #negative significant (contrary to main which was non-significant)
plot(mat.junjul.y.sav.mod$Sol)
plot(mat.junjul.y.sav.mod$VCV)
autocorr.plot(mat.junjul.y.sav.mod$VCV)
hist(mcmc(mat.junjul.y.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.junjul.y.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat.junjul.y.sav.mod.RData")





#### Jan-Feb Mean Temperature ----
mat.janfeb.y.savanna <- filter(mat_janfeb_y, Biome_type == "Savanna")
mat.janfeb.y.tundra <- filter(mat_janfeb_y, Biome_type == "Tundra")


## Tundra: Fitting the model including biogeography as a random factor
mat.janfeb.y.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = mat.janfeb.y.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.janfeb.y.tun.mod) #negative significant slope (same as main)
plot(mat.janfeb.y.tun.mod$Sol)
plot(mat.janfeb.y.tun.mod$VCV)
autocorr.plot(mat.janfeb.y.tun.mod$VCV)
hist(mcmc(mat.janfeb.y.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.janfeb.y.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat.janfeb.y.tun.mod.RData")


## Savanna: Fitting the model with biogeography as a random factor
mat.janfeb.y.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = mat.janfeb.y.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.janfeb.y.sav.mod) # negative non-significant slope (same as main)
plot(mat.janfeb.y.sav.mod$Sol)
plot(mat.janfeb.y.sav.mod$VCV)
autocorr.plot(mat.janfeb.y.sav.mod$VCV)
hist(mcmc(mat.janfeb.y.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.janfeb.y.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat.janfeb.y.sav.mod.RData")





#### Mean Annual Precipitation (MAP) ----
map.savanna.y <- filter(map_y, Biome_type == "Savanna")
map.tundra.y <- filter(map_y, Biome_type == "Tundra")

## Tundra: Fitting the model including biogeography as a random factor
map.tun.y.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                        data = map.tundra.y, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.tun.y.mod) #negative non-significant slope (contrary to main which was positive non-significant)
plot(map.tun.y.mod$Sol)
plot(map.tun.y.mod$VCV)
autocorr.plot(map.tun.y.mod$VCV)
hist(mcmc(map.tun.y.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.tun.y.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.tun.y.mod.RData")


## Savanna: Fitting the model with biogeography as a random factor
map.sav.y.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                        data = map.savanna.y, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.sav.y.mod) #positive non-significant slope (same as main)
plot(map.sav.y.mod$Sol)
plot(map.sav.y.mod$VCV)
autocorr.plot(map.sav.y.mod$VCV)
hist(mcmc(map.sav.y.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.sav.y.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.sav.y.mod.RData")





#### January-February Mean Precipitation ----
map.janfeb.y.sav <- filter(map_janfeb_y, Biome_type == "Savanna")
map.janfeb.y.tun <- filter(map_janfeb_y, Biome_type == "Tundra")

## Tundra: Fitting the model including biogeography as a random factor
map.janfeb.y.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = map.janfeb.y.tun, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.janfeb.y.tun.mod) #negative non-significant slope (same as main)
plot(map.janfeb.y.tun.mod$Sol)
plot(map.janfeb.y.tun.mod$VCV)
autocorr.plot(map.janfeb.y.tun.mod$VCV)
hist(mcmc(map.janfeb.y.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.janfeb.y.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.janfeb.y.tun.mod.RData")

## Savanna: Fitting the model with biogeography as a random factor
map.janfeb.y.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = map.janfeb.y.sav, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.janfeb.y.sav.mod) #negative non-significant slope (main was positive non-significant)
plot(map.janfeb.y.sav.mod$Sol, ask = F)
plot(map.janfeb.y.sav.mod$VCV)
autocorr.plot(map.janfeb.y.sav.mod$VCV)
hist(mcmc(map.janfeb.y.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.janfeb.y.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.janfeb.y.sav.mod.RData")






#### June-July Mean Precipitation ----
map.junjul.y.tundra <- filter(map_junjul_y, Biome_type == "Tundra")
map.junjul.y.savanna <- filter(map_junjul_y, Biome_type == "Savanna")

## Tundra: Fitting the model including biogeography as a random factor
map.junjul.y.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = map.junjul.y.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.junjul.y.tun.mod) #positive non-significant slope (same as main)
plot(map.junjul.y.tun.mod$Sol)
plot(map.junjul.y.tun.mod$VCV)
autocorr.plot(map.junjul.y.tun.mod$VCV)
hist(mcmc(map.junjul.y.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.junjul.y.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.junjul.y.tun.mod.RData")


## Savanna: Fitting the model with biogeography as a random factor
map.junjul.y.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = map.junjul.y.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.junjul.y.sav.mod) #negative non-significant slope (same as main)
plot(map.junjul.y.sav.mod$Sol)
plot(map.junjul.y.sav.mod$VCV)
autocorr.plot(map.junjul.y.sav.mod$VCV)
hist(mcmc(map.junjul.y.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.junjul.y.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.junjul.y.sav.mod.RData")






#### Minimum Temperature Analysis ----
mat.min.y.tundra <- filter(mat_min_y, Biome_type == "Tundra")
mat.min.y.savanna <- filter(mat_min_y, Biome_type == "Savanna")


## Tundra: Fitting the model including biogeography as a random factor
mat.min.y.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                            data = mat.min.y.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.min.y.tun.mod) #significant negative slope (same as main)
plot(mat.min.y.tun.mod$Sol)
plot(mat.min.y.tun.mod$VCV)
autocorr.plot(mat.min.y.tun.mod$VCV)
hist(mcmc(mat.min.y.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.min.y.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat.min.y.tun.mod.RData")


## Savanna: Fitting the model with biogeography as a random factor
mat.min.y.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                            data = mat.min.y.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.min.y.sav.mod) #negative non-signficant slope (same as main)
plot(mat.min.y.sav.mod$Sol)
plot(mat.min.y.sav.mod$VCV)
autocorr.plot(mat.min.y.sav.mod$VCV)
hist(mcmc(mat.min.y.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.min.y.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat.min.y.sav.mod.RData")





#### Maximum Temperature Analysis ----
mat.max.y.tundra <- filter(mat_max_y, Biome_type == "Tundra")
mat.max.y.savanna <- filter(mat_max_y, Biome_type == "Savanna")


## Tundra: Fitting the model including biogeography as a random factor
mat.max.y.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                            data = mat.max.y.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.max.y.tun.mod) #negative non-significant slope (same as main)
plot(mat.max.y.tun.mod$Sol)
plot(mat.max.y.tun.mod$VCV)
autocorr.plot(mat.max.y.tun.mod$VCV)
hist(mcmc(mat.max.y.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.max.y.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat.max.y.tun.mod.RData")


## Savanna: Fitting the model with biogeography as a random factor
mat.max.y.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                            data = mat.max.y.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.max.y.sav.mod) #positive non-significant slope (contrary to main which is negative non-signficant)
plot(mat.max.y.sav.mod$Sol)
plot(mat.max.y.sav.mod$VCV)
autocorr.plot(mat.max.y.sav.mod$VCV)
hist(mcmc(mat.max.y.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.max.y.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/mat.max.y.sav.mod.RData")





#### Minimum Precipitation month analysis ----
map.min.y.tundra <- filter(map_min_y, Biome_type == "Tundra")
map.min.y.savanna <- filter(map_min_y, Biome_type == "Savanna")


## Tundra: Fitting the model including biogeography as a random factor
map.min.y.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                            data = map.min.y.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.min.y.tun.mod) #negative non-significant slope (main was significant)
plot(map.min.y.tun.mod$Sol)
plot(map.min.y.tun.mod$VCV)
autocorr.plot(map.min.y.tun.mod$VCV)
hist(mcmc(map.min.y.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.min.y.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.min.y.tun.mod.RData")


## Savanna: Fitting the model with biogeography as a random factor, random intercepts
map.min.y.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                            data = map.min.y.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.min.y.sav.mod) #negative significant slope (same as main)
plot(map.min.y.sav.mod$Sol)
plot(map.min.y.sav.mod$VCV)
autocorr.plot(map.min.y.sav.mod$VCV)
hist(mcmc(map.min.y.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.min.y.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.min.y.sav.mod.RData")





#### Maximum Precipitation month analysis ----
map.max.y.tundra <- filter(map_max_y, Biome_type == "Tundra")
map.max.y.savanna <- filter(map_max_y, Biome_type == "Savanna")


## Tundra: Fitting the model including biogeography as a random factor
map.max.y.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                            data = map.max.y.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.max.y.tun.mod) #positive non-significant slope (same as main)
plot(map.max.y.tun.mod$Sol)
plot(map.max.y.tun.mod$VCV)
autocorr.plot(map.max.y.tun.mod$VCV)
hist(mcmc(map.max.y.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.max.y.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.max.y.tun.mod.RData")


## Savanna: Fitting the model with biogeography as a random factor
map.max.y.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                            data = map.max.y.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.max.y.sav.mod) #positive non-significant slope (main was significant)
plot(map.max.y.sav.mod$Sol)
plot(map.max.y.sav.mod$VCV)
autocorr.plot(map.max.y.sav.mod$VCV)
hist(mcmc(map.max.y.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.max.y.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.max.y.sav.mod.RData")




#### EFFECT SIZES ----

# Definining function to extract model outputs
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

# Create list of input models
dataListy <- list(map.janfeb.y.sav.mod, map.janfeb.y.tun.mod, map.junjul.y.sav.mod, map.junjul.y.tun.mod, map.max.y.sav.mod,
                 map.max.y.tun.mod, map.min.y.sav.mod, map.min.y.tun.mod, map.sav.y.mod, map.tun.y.mod,
                 mat.janfeb.y.sav.mod, mat.janfeb.y.tun.mod, mat.junjul.y.sav.mod, mat.junjul.y.tun.mod, mat.max.y.sav.mod, 
                 mat.max.y.tun.mod, mat.min.y.sav.mod, mat.min.y.tun.mod, mat.y.sav.mod, mat.y.tun.mod)

# Create lists of input model names
dataListNamesy <- list("MAP Jan-Feb Savanna", "MAP Jan-Feb Tundra", "MAP Jun-Jul Savanna", "MAP Jun-Jul Tundra", "MAP Max Savanna",
                      "MAP Max Tundra", "MAP Min Savanna", "MAP Min Tundra", "MAP Savanna", "MAP Tundra",
                      "MAT Jan-Feb Savanna", "MAT Jan-Feb Tundra", "MAT Jun-Jul Savanna", "MAT Jun-Jul Tundra", "MAT Max Savanna",
                      "MAT Max Tundra", "MAT Min Savanna", "MAT Min Tundra", "MAT Savanna", "MAT Tundra")

# Get model outputs and add model names
readyListy <- mapply(cbind, lapply(dataListy, clean.MCMC), "modelName" = dataListNamesy, SIMPLIFY = F)

# Turn list of dataframes into a dataframe
mcmcOutputsy <- as.data.frame(do.call(rbind, readyListy), stringsAsFactors = FALSE)

# Write csv
#write.csv(mcmcOutputs, file = "scripts/users/mgarciacriado/encroachment_paper/climatic_timeseries/data/mcmc_outputs_random")

# Create nice summary table in html format
#stargazer(mcmcOutputs, title = "Model results (random effects)", type = "html", summary = FALSE, 
#          out = "scripts/users/mgarciacriado/encroachment_paper/climatic_timeseries/models/Rate_models_random.htm")

# Prepare data for plotting
mcmcOutputs.estimate.y <- mcmcOutputsy %>% filter(variable == "estimate")
mcmcOutputs.estimate.y$Biome[mcmcOutputs.estimate.y$modelName %in% c("MAP Jan-Feb Savanna", "MAP Jun-Jul Savanna", "MAP Max Savanna", "MAP Min Savanna", "MAP Savanna", 
                                                                 "MAT Jan-Feb Savanna",  "MAT Jun-Jul Savanna",  "MAT Max Savanna",
                                                                 "MAT Min Savanna", "MAT Savanna")] <- "Savanna"

mcmcOutputs.estimate.y$Biome[mcmcOutputs.estimate.y$modelName %in% c("MAP Jan-Feb Tundra", "MAP Jun-Jul Tundra", "MAP Max Tundra","MAP Min Tundra", 
                                                                 "MAP Tundra", "MAT Jan-Feb Tundra","MAT Jun-Jul Tundra","MAT Max Tundra",
                                                                 "MAT Min Tundra", "MAT Tundra")] <- "Tundra"
# Create two dataframes for each type of climatic variable
mcmcOutputs.estimate.y.temp <- mcmcOutputs.estimate.y %>% filter(modelName %in% c("MAT Jan-Feb Savanna", "MAT Jan-Feb Tundra", "MAT Jun-Jul Savanna", 
                                                                              "MAT Jun-Jul Tundra", "MAT Max Savanna", "MAT Max Tundra", "MAT Min Savanna",
                                                                              "MAT Min Tundra", "MAT Savanna", "MAT Tundra"))
mcmcOutputs.estimate.y.temp$clim.var <- c("Jan-Feb", "Jan-Feb", "Jun-Jul", "Jun-Jul", "Max", "Max", "Min", "Min", "MAT", "MAT")

mcmcOutputs.estimate.y.prec <- mcmcOutputs.estimate.y %>% filter(modelName %in% c("MAP Jan-Feb Savanna", "MAP Jan-Feb Tundra","MAP Jun-Jul Savanna", 
                                                                              "MAP Jun-Jul Tundra", "MAP Max Savanna", "MAP Max Tundra", "MAP Min Savanna", 
                                                                              "MAP Min Tundra", "MAP Savanna", "MAP Tundra"))
mcmcOutputs.estimate.y.prec$clim.var <- c("Jan-Feb", "Jan-Feb", "Jun-Jul", "Jun-Jul", "Max", "Max", "Min", "Min", "MAP", "MAP")


## PLOTS ---

# plotting temperature effect sizes
(effect.sizes.temp.y <- ggplot(mcmcOutputs.estimate.y.temp, aes(x = factor(clim.var), y = post.mean, fill = factor(Biome))) + 
    geom_bar(stat = "identity", alpha = 0.6, position=position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin=l.95..CI, ymax=u.95..CI), position = position_dodge(0.7), width = 0.25) + 
    geom_hline(yintercept=0) + ylab("Annual cover change rate (%)/\nAnnual temperature change rate (°C)\n") + xlab("") +
    scale_x_discrete(labels=c("Jan-Feb", "Jun-Jul", "MAT", "Max", "Min")) + clima.theme +
    theme(axis.text.x  = element_text(vjust=0.5, size=26, colour = "black"), 
          axis.title.y = element_text(face="bold", size=26),
          axis.text.y  = element_text(vjust=0.5, size=26, colour = "black"), 
          legend.text = element_text(size=26), legend.spacing.x = unit(0.3, 'cm')))

# significant effects at pMCMC < 0.05
(effect.sizes.temp.sig.y <- effect.sizes.temp.y + 
    annotate("text", x = 1.18, y = 0.5, label = "*", size = 10) +
    annotate("text", x = 1.85, y = 0.5, label = "*", size = 10) +
    annotate("text", x = 2.85, y = 0.5, label = "*", size = 10) +
    annotate("text", x = 3.18, y = 0.5, label = "*", size = 10) + 
    annotate("text", x = 5.18, y = 0.5, label = "*", size = 10))


# plotting precipitation effect sizes
(effect.sizes.prec.y <- ggplot(mcmcOutputs.estimate.y.prec, aes(x = factor(clim.var), y = post.mean, fill = factor(Biome))) + 
    geom_bar(stat = "identity", alpha = 0.6, position=position_dodge(), width = 0.7) + 
    geom_errorbar(aes(ymin=l.95..CI, ymax=u.95..CI), position = position_dodge(0.7), width = 0.25) + 
    geom_hline(yintercept=0) + ylab("Annual cover change rate (%)/\nAnnual precipitation change rate (mm)\n") + xlab("") + 
    scale_x_discrete(labels=c("Jan-Feb", "Jun-Jul","MAP", "Max", "Min")) + clima.theme + 
    theme(axis.text.x  = element_text(vjust=0.5, size=26, colour = "black"), 
          axis.title.y = element_text(face="bold", size=26),
          axis.text.y  = element_text(vjust=0.5, size=26, colour = "black"), 
          legend.text = element_text(size=26), legend.spacing.x = unit(0.3, 'cm')))


# significant effects at pMCMC < 0.05
(effect.sizes.prec.sig.y <- effect.sizes.prec.y +  
    annotate("text", x = 4.84, y = 0.1, label = "*", size = 10))


## Combine effect sizes plots (excluding records before 1979 and after 2013 
(effect.sizes.panel.y <- ggarrange(effect.sizes.temp.sig.y, effect.sizes.prec.sig.y, labels = c("(a)", "(b)"), 
                               font.label = list(size = 26), common.legend = TRUE, legend = "right"))
ggsave(effect.sizes.panel.y, filename = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/figures/Figure_4_v2_minus79_13.png", 
       width = 60, height = 20, units = "cm")


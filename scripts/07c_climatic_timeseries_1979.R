#### Woody encroachment across biomes
#### Script 07c. Climatic timeseries models & figures
#### 'Rate vs rate' analysis excluding sites from before 1979 only
#### Mariana García Criado
#### September 2019

## LIBRARIES ----
library(tidyverse)
library(ggplot2)
library(broom)
library(MCMCglmm)
library(cowplot)
library(ggpubr)
library(stargazer)

## THEME ----
clima.theme <- theme(legend.title = element_blank(), 
                     legend.text = element_text(size=30), legend.key = element_blank(), 
                     legend.spacing.x = unit(0.3, 'cm'),
                     axis.title.x = element_text(face="bold", size=28),
                     axis.text.x  = element_text(vjust=0.5, size=28, colour = "black"), 
                     axis.title.y = element_text(face="bold", size=28),
                     axis.text.y  = element_text(vjust=0.5, size=28, colour = "black"),
                     panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                     panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                     plot.margin = unit(c(1,1,1,1), units = , "cm"))

## Remove studies which records started before 1979 - object "clima.years.1979" has been created in script 07b

## Create individual climatic variable objects
mat_y9 <- filter(clima.years.1979, variable == "mat")
map_y9 <- filter(clima.years.1979, variable == "map")
mat_junjul_y9 <- filter(clima.years.1979, variable == "mat_junjul")
mat_janfeb_y9 <- filter(clima.years.1979, variable == 'mat_janfeb')
map_janfeb_y9 <- filter(clima.years.1979, variable == 'map_janfeb')
map_junjul_y9 <- filter(clima.years.1979, variable == 'map_junjul')
mat_min_y9 <- filter(clima.years.1979, variable == 'mat_min')
mat_max_y9 <- filter(clima.years.1979, variable == 'mat_max')
map_min_y9 <- filter(clima.years.1979, variable == 'map_min')
map_max_y9 <- filter(clima.years.1979, variable == 'map_max')

# parameter-expanded prior with inverse Wishart distribution
a <- 1000
prior6 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*a)))


## MODELS WITHOUT RECORDS <1979 ----

#### Mean Annual Temperature ----
mat.y9.tundra <- filter(mat_y9, Biome_type == "Tundra")
mat.y9.sav <- filter(mat_y9, Biome_type == "Savanna")

# Tundra model
mat.y9.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, prior = prior6,
                          data = mat.y9.tundra, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)
summary(mat.y9.tun.mod) #negative significant relationship, same as main model and 2013
plot(mat.y9.tun.mod$Sol)
plot(mat.y9.tun.mod$VCV)
autocorr.plot(mat.y9.tun.mod$VCV)
hist(mcmc(mat.y9.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.y9.tun.mod, file = "models/random/mat.y9.tun.mod.RData")

# Savanna model
mat.y9.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, prior = prior6, 
                          data = mat.y9.sav, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.y9.sav.mod) #negative non-significant slope, contrary to 2013 but same as main model
plot(mat.y9.sav.mod$Sol)
plot(mat.y9.sav.mod$VCV)
autocorr.plot(mat.y9.sav.mod$VCV)
hist(mcmc(mat.y9.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.y9.sav.mod, file = "models/random/mat.y9.sav.mod.RData")



#### June-July Temperature ----
mat.junjul.y9.tun <- filter(mat_junjul_y9, Biome_type == "Tundra")
mat.junjul.y9.sav <- filter(mat_junjul_y9, Biome_type == "Savanna")

# Tundra model
mat.junjul.y9.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = mat.junjul.y9.tun, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.junjul.y9.tun.mod) #positive non-significant (contrary to main and 2013 which are negative non-significant)
plot(mat.junjul.y9.tun.mod$Sol)
plot(mat.junjul.y9.tun.mod$VCV)
autocorr.plot(mat.junjul.y9.tun.mod$VCV)
hist(mcmc(mat.junjul.y9.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.junjul.y9.tun.mod, file = "models/random/mat.junjul.y9.tun.mod.RData")

# Savanna model
mat.junjul.y9.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = mat.junjul.y9.sav, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.junjul.y9.sav.mod) #negative non-significant (same as main, but 2013 was significant)
plot(mat.junjul.y9.sav.mod$Sol)
plot(mat.junjul.y9.sav.mod$VCV)
autocorr.plot(mat.junjul.y9.sav.mod$VCV)
hist(mcmc(mat.junjul.y9.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.junjul.y9.sav.mod, file = "models/random/mat.junjul.y9.sav.mod.RData")



#### Jan-Feb Mean Temperature ----
mat.janfeb.y9.savanna <- filter(mat_janfeb_y9, Biome_type == "Savanna")
mat.janfeb.y9.tundra <- filter(mat_janfeb_y9, Biome_type == "Tundra")

# Tundra model
mat.janfeb.y9.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = mat.janfeb.y9.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.janfeb.y9.tun.mod) #negative significant slope (same as main and 2013)
plot(mat.janfeb.y9.tun.mod$Sol)
plot(mat.janfeb.y9.tun.mod$VCV)
autocorr.plot(mat.janfeb.y9.tun.mod$VCV)
hist(mcmc(mat.janfeb.y9.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.janfeb.y9.tun.mod, file = "models/random/mat.janfeb.y9.tun.mod.RData")

# Savanna model
mat.janfeb.y9.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = mat.janfeb.y9.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.janfeb.y9.sav.mod) # negative non-significant slope (same as main and 2013)
plot(mat.janfeb.y9.sav.mod$Sol)
plot(mat.janfeb.y9.sav.mod$VCV)
autocorr.plot(mat.janfeb.y9.sav.mod$VCV)
hist(mcmc(mat.janfeb.y9.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.janfeb.y9.sav.mod, file = "models/random/mat.janfeb.y9.sav.mod.RData")



#### Mean Annual Precipitation (MAP) ----
map.savanna.y9 <- filter(map_y9, Biome_type == "Savanna")
map.tundra.y9 <- filter(map_y9, Biome_type == "Tundra")

# Tundra model
map.tun.y9.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                          data = map.tundra.y9, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.tun.y9.mod) #positive non-significant (same as main, 2013 was negative non-significant)
plot(map.tun.y9.mod$Sol)
plot(map.tun.y9.mod$VCV)
autocorr.plot(map.tun.y9.mod$VCV)
hist(mcmc(map.tun.y9.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.tun.y9.mod, file = "models/random/map.tun.y9.mod.RData")

# Savanna model
map.sav.y9.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                          data = map.savanna.y9, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.sav.y9.mod) #positive significant (both main and 2013 were non-significant)
plot(map.sav.y9.mod$Sol)
plot(map.sav.y9.mod$VCV)
autocorr.plot(map.sav.y9.mod$VCV)
hist(mcmc(map.sav.y9.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.sav.y9.mod, file = "models/random/map.sav.y9.mod.RData")



#### January-February Mean Precipitation ----
map.janfeb.y9.sav <- filter(map_janfeb_y9, Biome_type == "Savanna")
map.janfeb.y9.tun <- filter(map_janfeb_y9, Biome_type == "Tundra")

# Tundra model
map.janfeb.y9.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = map.janfeb.y9.tun, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.janfeb.y9.tun.mod) #negative non-significant slope (same as main and 2013)
plot(map.janfeb.y9.tun.mod$Sol)
plot(map.janfeb.y9.tun.mod$VCV)
autocorr.plot(map.janfeb.y9.tun.mod$VCV)
hist(mcmc(map.janfeb.y9.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.janfeb.y9.tun.mod, file = "models/random/map.janfeb.y9.tun.mod.RData")

# Savanna model
map.janfeb.y9.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = map.janfeb.y9.sav, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.janfeb.y9.sav.mod) #positive significant (2013 was negative non-significant and main was positive non-significant)
plot(map.janfeb.y9.sav.mod$Sol)
plot(map.janfeb.y9.sav.mod$VCV)
autocorr.plot(map.janfeb.y9.sav.mod$VCV)
hist(mcmc(map.janfeb.y9.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.janfeb.y9.sav.mod, file = "models/random/map.janfeb.y9.sav.mod.RData")



#### June-July Mean Precipitation ----
map.junjul.y9.tundra <- filter(map_junjul_y9, Biome_type == "Tundra")
map.junjul.y9.savanna <- filter(map_junjul_y9, Biome_type == "Savanna")

# Tundra model
map.junjul.y9.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = map.junjul.y9.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.junjul.y9.tun.mod) #positive non-significant slope (same as main and 2013)
plot(map.junjul.y9.tun.mod$Sol)
plot(map.junjul.y9.tun.mod$VCV)
autocorr.plot(map.junjul.y9.tun.mod$VCV)
hist(mcmc(map.junjul.y9.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.junjul.y9.tun.mod, file = "models/random/map.junjul.y9.tun.mod.RData")

# Savanna model
map.junjul.y9.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = map.junjul.y9.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.junjul.y9.sav.mod) #negative non-significant slope (same as main and 2013)
plot(map.junjul.y9.sav.mod$Sol)
plot(map.junjul.y9.sav.mod$VCV)
autocorr.plot(map.junjul.y9.sav.mod$VCV)
hist(mcmc(map.junjul.y9.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.junjul.y9.sav.mod, file = "models/random/map.junjul.y9.sav.mod.RData")



#### Minimum Temperature Analysis ----
mat.min.y9.tundra <- filter(mat_min_y9, Biome_type == "Tundra")
mat.min.y9.savanna <- filter(mat_min_y9, Biome_type == "Savanna")

# Tundra model
mat.min.y9.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                              data = mat.min.y9.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.min.y9.tun.mod) #significant negative slope (same as main and 2013)
plot(mat.min.y9.tun.mod$Sol)
plot(mat.min.y9.tun.mod$VCV)
autocorr.plot(mat.min.y9.tun.mod$VCV)
hist(mcmc(mat.min.y9.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.min.y9.tun.mod, file = "models/random/mat.min.y9.tun.mod.RData")

# Savanna model
mat.min.y9.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                              data = mat.min.y9.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.min.y9.sav.mod) #positive non-significant (contrary to main and 2013 which were negative non-signficant)
plot(mat.min.y9.sav.mod$Sol)
plot(mat.min.y9.sav.mod$VCV)
autocorr.plot(mat.min.y9.sav.mod$VCV)
hist(mcmc(mat.min.y9.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.min.y9.sav.mod, file = "models/random/mat.min.y9.sav.mod.RData")



#### Maximum Temperature Analysis ----
mat.max.y9.tundra <- filter(mat_max_y9, Biome_type == "Tundra")
mat.max.y9.savanna <- filter(mat_max_y9, Biome_type == "Savanna")

# Tundra model
mat.max.y9.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                              data = mat.max.y9.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.max.y9.tun.mod) #negative non-significant slope (same as main and 2013)
plot(mat.max.y9.tun.mod$Sol)
plot(mat.max.y9.tun.mod$VCV)
autocorr.plot(mat.max.y9.tun.mod$VCV)
hist(mcmc(mat.max.y9.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.max.y9.tun.mod, file = "models/random/mat.max.y9.tun.mod.RData")

# Savanna model
mat.max.y9.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                              data = mat.max.y9.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.max.y9.sav.mod) # negative non-significant (similar to main but 2013 was positive non-significant)
plot(mat.max.y9.sav.mod$Sol)
plot(mat.max.y9.sav.mod$VCV)
autocorr.plot(mat.max.y9.sav.mod$VCV)
hist(mcmc(mat.max.y9.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.max.y9.sav.mod, file = "models/random/mat.max.y9.sav.mod.RData")



#### Minimum Precipitation month analysis ----
map.min.y9.tundra <- filter(map_min_y9, Biome_type == "Tundra")
map.min.y9.savanna <- filter(map_min_y9, Biome_type == "Savanna")

# Tundra model
map.min.y9.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                              data = map.min.y9.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.min.y9.tun.mod) #negative non-significant slope (same as 2013, main was significant)
plot(map.min.y9.tun.mod$Sol)
plot(map.min.y9.tun.mod$VCV)
autocorr.plot(map.min.y9.tun.mod$VCV)
hist(mcmc(map.min.y9.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.min.y9.tun.mod, file = "models/random/map.min.y9.tun.mod.RData")

# Savanna model
map.min.y9.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                              data = map.min.y9.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.min.y9.sav.mod) #negative significant slope (same as main and 2013)
plot(map.min.y9.sav.mod$Sol)
plot(map.min.y9.sav.mod$VCV)
autocorr.plot(map.min.y9.sav.mod$VCV)
hist(mcmc(map.min.y9.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.min.y9.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/map.min.y9.sav.mod.RData")



#### Maximum Precipitation month analysis ----
map.max.y9.tundra <- filter(map_max_y9, Biome_type == "Tundra")
map.max.y9.savanna <- filter(map_max_y9, Biome_type == "Savanna")

# Tundra model
map.max.y9.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                              data = map.max.y9.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.max.y9.tun.mod) #positive non-significant slope (same as main and 2013)
plot(map.max.y9.tun.mod$Sol)
plot(map.max.y9.tun.mod$VCV)
autocorr.plot(map.max.y9.tun.mod$VCV)
hist(mcmc(map.max.y9.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.max.y9.tun.mod, file = "models/random/map.max.y9.tun.mod.RData")

# Savanna model
map.max.y9.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                              data = map.max.y9.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.max.y9.sav.mod) #positive significant (same as main, 2013 non-significant)
plot(map.max.y9.sav.mod$Sol)
plot(map.max.y9.sav.mod$VCV)
autocorr.plot(map.max.y9.sav.mod$VCV)
hist(mcmc(map.max.y9.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.max.y9.sav.mod, file = "models/random/map.max.y9.sav.mod.RData")





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
dataListy9 <- list(map.janfeb.y9.sav.mod, map.janfeb.y9.tun.mod, map.junjul.y9.sav.mod, map.junjul.y9.tun.mod, map.max.y9.sav.mod,
                  map.max.y9.tun.mod, map.min.y9.sav.mod, map.min.y9.tun.mod, map.sav.y9.mod, map.tun.y9.mod,
                  mat.janfeb.y9.sav.mod, mat.janfeb.y9.tun.mod, mat.junjul.y9.sav.mod, mat.junjul.y9.tun.mod, mat.max.y9.sav.mod, 
                  mat.max.y9.tun.mod, mat.min.y9.sav.mod, mat.min.y9.tun.mod, mat.y9.sav.mod, mat.y9.tun.mod)

# Create lists of input model names
dataListNamesy9 <- list("MAP Jan-Feb Savanna", "MAP Jan-Feb Tundra", "MAP Jun-Jul Savanna", "MAP Jun-Jul Tundra", "MAP Max Savanna",
                       "MAP Max Tundra", "MAP Min Savanna", "MAP Min Tundra", "MAP Savanna", "MAP Tundra",
                       "MAT Jan-Feb Savanna", "MAT Jan-Feb Tundra", "MAT Jun-Jul Savanna", "MAT Jun-Jul Tundra", "MAT Max Savanna",
                       "MAT Max Tundra", "MAT Min Savanna", "MAT Min Tundra", "MAT Savanna", "MAT Tundra")

# Get model outputs and add model names
readyListy9 <- mapply(cbind, lapply(dataListy9, clean.MCMC), "modelName" = dataListNamesy9, SIMPLIFY = F)

# Turn list of dataframes into a dataframe
mcmcOutputsy9 <- as.data.frame(do.call(rbind, readyListy9), stringsAsFactors = FALSE)

# Prepare data for plotting
mcmcOutputs.estimate.y9 <- mcmcOutputsy9 %>% filter(variable == "estimate")
mcmcOutputs.estimate.y9$Biome[mcmcOutputs.estimate.y9$modelName %in% c("MAP Jan-Feb Savanna", "MAP Jun-Jul Savanna", "MAP Max Savanna", "MAP Min Savanna", "MAP Savanna", 
                                                                   "MAT Jan-Feb Savanna",  "MAT Jun-Jul Savanna",  "MAT Max Savanna",
                                                                   "MAT Min Savanna", "MAT Savanna")] <- "Savanna"

mcmcOutputs.estimate.y9$Biome[mcmcOutputs.estimate.y9$modelName %in% c("MAP Jan-Feb Tundra", "MAP Jun-Jul Tundra", "MAP Max Tundra","MAP Min Tundra", 
                                                                   "MAP Tundra", "MAT Jan-Feb Tundra","MAT Jun-Jul Tundra","MAT Max Tundra",
                                                                   "MAT Min Tundra", "MAT Tundra")] <- "Tundra"
# Create two dataframes for each type of climatic variable
mcmcOutputs.estimate.y9.temp <- mcmcOutputs.estimate.y9 %>% filter(modelName %in% c("MAT Jan-Feb Savanna", "MAT Jan-Feb Tundra", "MAT Jun-Jul Savanna", 
                                                                                  "MAT Jun-Jul Tundra", "MAT Max Savanna", "MAT Max Tundra", "MAT Min Savanna",
                                                                                  "MAT Min Tundra", "MAT Savanna", "MAT Tundra"))
mcmcOutputs.estimate.y9.temp$clim.var <- c("Jan-Feb", "Jan-Feb", "Jun-Jul", "Jun-Jul", "Max", "Max", "Min", "Min", "MAT", "MAT")

mcmcOutputs.estimate.y9.prec <- mcmcOutputs.estimate.y9 %>% filter(modelName %in% c("MAP Jan-Feb Savanna", "MAP Jan-Feb Tundra","MAP Jun-Jul Savanna", 
                                                                                  "MAP Jun-Jul Tundra", "MAP Max Savanna", "MAP Max Tundra", "MAP Min Savanna", 
                                                                                  "MAP Min Tundra", "MAP Savanna", "MAP Tundra"))
mcmcOutputs.estimate.y9.prec$clim.var <- c("Jan-Feb", "Jan-Feb", "Jun-Jul", "Jun-Jul", "Max", "Max", "Min", "Min", "MAP", "MAP")



## PLOTS ----

# plotting temperature effect sizes
(effect.sizes.temp.y9 <- ggplot(mcmcOutputs.estimate.y9.temp, aes(x = factor(clim.var), y = post.mean, fill = factor(Biome))) + 
    geom_bar(stat = "identity", alpha = 0.6, position=position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin=l.95..CI, ymax=u.95..CI), position = position_dodge(0.7), width = 0.25) + 
    geom_hline(yintercept=0) + ylab("Annual cover change rate (%)/\nAnnual temperature change rate (°C)\n") + xlab("") +
    scale_x_discrete(labels=c("Jan-Feb", "Jun-Jul", "MAT", "Max", "Min")) + clima.theme +
    theme(axis.text.x  = element_text(vjust=0.5, size=26, colour = "black"), 
          axis.title.y = element_text(face="bold", size=26),
          axis.text.y  = element_text(vjust=0.5, size=26, colour = "black"), 
          legend.text = element_text(size=26), legend.spacing.x = unit(0.3, 'cm')))

# significant effects at pMCMC < 0.05
(effect.sizes.temp.sig.y9 <- effect.sizes.temp.y9 + 
    annotate("text", x = 1.18, y = 0.5, label = "*", size = 10) +
    annotate("text", x = 3.18, y = 0.5, label = "*", size = 10) + 
    annotate("text", x = 5.18, y = 0.5, label = "*", size = 10))


# plotting precipitation effect sizes
(effect.sizes.prec.y9 <- ggplot(mcmcOutputs.estimate.y9.prec, aes(x = factor(clim.var), y = post.mean, fill = factor(Biome))) + 
    geom_bar(stat = "identity", alpha = 0.6, position=position_dodge(), width = 0.7) + 
    geom_errorbar(aes(ymin=l.95..CI, ymax=u.95..CI), position = position_dodge(0.7), width = 0.25) + 
    geom_hline(yintercept=0) + ylab("Annual cover change rate (%)/\nAnnual precipitation change rate (mm)\n") + xlab("") + 
    scale_x_discrete(labels=c("Jan-Feb", "Jun-Jul","MAP", "Max", "Min")) + clima.theme + 
    theme(axis.text.x  = element_text(vjust=0.5, size=26, colour = "black"), 
          axis.title.y = element_text(face="bold", size=26),
          axis.text.y  = element_text(vjust=0.5, size=26, colour = "black"), 
          legend.text = element_text(size=26), legend.spacing.x = unit(0.3, 'cm')))

# significant effects at pMCMC < 0.05
(effect.sizes.prec.sig.y9 <- effect.sizes.prec.y9 +  
    annotate("text", x = 4.84, y = 0.15, label = "*", size = 10) +
    annotate("text", x = 3.84, y = 0.15, label = "*", size = 10) +
    annotate("text", x = 2.84, y = 0.15, label = "*", size = 10))


## Figure S7
(all.ef.sizes <- ggpubr::ggarrange(effect.sizes.temp.sig, effect.sizes.prec.sig, effect.sizes.temp.sig.y9, 
                          effect.sizes.prec.sig.y9, effect.sizes.temp.sig.y, effect.sizes.prec.sig.y, 
                          nrow = 3, ncol = 2,
                          labels = c("(a) All records", "", "(b) Excluding records pre-1979", "", 
                                     "(c) Excluding records pre-1979 and post-2013", ""),
                          font.label = list(size = 26), hjust = c(-1, -1, -0.45, -0.5, -0.3, -0.5),
                          legend = "none"))

ggsave(all.ef.sizes, filename = "figures/Figure_S7.png", 
       width = 52, height = 80, units = "cm")



#### TABLE S3 - MODEL COMPARISONS ----

# Create list of input models
dataList.all <- list(map.janfeb.y9.sav.mod, map.janfeb.y9.tun.mod, map.junjul.y9.sav.mod, map.junjul.y9.tun.mod, map.max.y9.sav.mod,
                   map.max.y9.tun.mod, map.min.y9.sav.mod, map.min.y9.tun.mod, map.sav.y9.mod, map.tun.y9.mod,
                   mat.janfeb.y9.sav.mod, mat.janfeb.y9.tun.mod, mat.junjul.y9.sav.mod, mat.junjul.y9.tun.mod, mat.max.y9.sav.mod, 
                   mat.max.y9.tun.mod, mat.min.y9.sav.mod, mat.min.y9.tun.mod, mat.y9.sav.mod, mat.y9.tun.mod, 
                   
                   map.janfeb.y.sav.mod, map.janfeb.y.tun.mod, map.junjul.y.sav.mod, map.junjul.y.tun.mod, map.max.y.sav.mod,
                   map.max.y.tun.mod, map.min.y.sav.mod, map.min.y.tun.mod, map.sav.y.mod, map.tun.y.mod,
                   mat.janfeb.y.sav.mod, mat.janfeb.y.tun.mod, mat.junjul.y.sav.mod, mat.junjul.y.tun.mod, mat.max.y.sav.mod, 
                   mat.max.y.tun.mod, mat.min.y.sav.mod, mat.min.y.tun.mod, mat.y.sav.mod, mat.y.tun.mod, 
                   
                   map.janfeb.sav.mod, map.janfeb.tun.mod, map.junjul.sav.mod, map.junjul.tun.mod, map.max.sav.mod,
                   map.max.tun.mod, map.min.sav.mod, map.min.tun.mod, map.sav.mod, map.tun.mod,
                   mat.janfeb.sav.mod, mat.janfeb.tun.mod, mat.junjul.sav.mod, mat.junjul.tun.mod, mat.max.sav.mod, 
                   mat.max.tun.mod, mat.min.sav.mod, mat.min.tun.mod, mat.sav.mod, mat.tun.mod)

# Create lists of input model names
dataListNames.all <- list("MAP Jan-Feb Savanna (exc. pre-1979)", "MAP Jan-Feb Tundra (exc. pre-1979)", 
                          "MAP Jun-Jul Savanna (exc. pre-1979)", "MAP Jun-Jul Tundra (exc. pre-1979)", 
                          "MAP Max Savanna (exc. pre-1979)", "MAP Max Tundra (exc. pre-1979)", "MAP Min Savanna (exc. pre-1979)", 
                          "MAP Min Tundra (exc. pre-1979)", "MAP Savanna (exc. pre-1979)", "MAP Tundra (exc. pre-1979)",
                        "MAT Jan-Feb Savanna (exc. pre-1979)", "MAT Jan-Feb Tundra (exc. pre-1979)", "MAT Jun-Jul Savanna (exc. pre-1979)", 
                        "MAT Jun-Jul Tundra (exc. pre-1979)", "MAT Max Savanna (exc. pre-1979)",
                        "MAT Max Tundra (exc. pre-1979)", "MAT Min Savanna (exc. pre-1979)", "MAT Min Tundra (exc. pre-1979)", 
                        "MAT Savanna (exc. pre-1979)", "MAT Tundra (exc. pre-1979)",
                        
                        "MAP Jan-Feb Savanna (exc. post-2013)", "MAP Jan-Feb Tundra (exc. post-2013)", 
                        "MAP Jun-Jul Savanna (exc. post-2013)", "MAP Jun-Jul Tundra (exc. post-2013)",
                        "MAP Max Savanna (exc. post-2013)", "MAP Max Tundra (exc. post-2013)", 
                        "MAP Min Savanna (exc. post-2013)", "MAP Min Tundra (exc. post-2013)", 
                        "MAP Savanna (exc. post-2013)", "MAP Tundra (exc. post-2013)",
                        "MAT Jan-Feb Savanna (exc. post-2013)", "MAT Jan-Feb Tundra (exc. post-2013)", 
                        "MAT Jun-Jul Savanna (exc. post-2013)", "MAT Jun-Jul Tundra (exc. post-2013)", 
                        "MAT Max Savanna (exc. post-2013)", "MAT Max Tundra (exc. post-2013)", 
                        "MAT Min Savanna (exc. post-2013)", "MAT Min Tundra (exc. post-2013)", 
                        "MAT Savanna (exc. post-2013)", "MAT Tundra (exc. post-2013)", 
                        
                        "MAP Jan-Feb Savanna (all records)", "MAP Jan-Feb Tundra (all records)", "MAP Jun-Jul Savanna (all records)", 
                        "MAP Jun-Jul Tundra (all records)", "MAP Max Savanna (all records)", "MAP Max Tundra (all records)", 
                        "MAP Min Savanna (all records)", "MAP Min Tundra (all records)", "MAP Savanna (all records)", 
                        "MAP Tundra (all records)", "MAT Jan-Feb Savanna (all records)", "MAT Jan-Feb Tundra (all records)", 
                        "MAT Jun-Jul Savanna (all records)", "MAT Jun-Jul Tundra (all records)", "MAT Max Savanna (all records)",
                        "MAT Max Tundra (all records)", "MAT Min Savanna (all records)", "MAT Min Tundra (all records)", 
                        "MAT Savanna (all records)", "MAT Tundra (all records)")

# Get model outputs and add model names
readyList.all <- mapply(cbind, lapply(dataList.all, clean.MCMC), "modelName" = dataListNames.all, SIMPLIFY = F)

# Turn list of dataframes into a dataframe
mcmcOutputs.all <- as.data.frame(do.call(rbind, readyList.all), stringsAsFactors = FALSE)

# Add model number
mcmc.Outputs.all2 <- transform(mcmcOutputs.all, ModelNumber = as.numeric(interaction(modelName)))

# Reorder columns
mcmc.Outputs.all.clean <- mcmc.Outputs.all2[, c(9, 8, 1, 7, 2, 3, 4, 5, 6)]

# Write csv
write.csv(mcmc.Outputs.all.clean, file = "models/TableS3.csv")

# Create nice summary table in html format
stargazer(mcmc.Outputs.all.clean, title = "Woody cover change rates vs. climatic change rates", type = "html", summary = FALSE, 
          out = "models/Table_S3.htm")




#### Woody encroachment across biomes
#### Script 07. Climatic timeseries models & figures
#### Mariana García Criado
#### April - October 2018

## LIBRARIES ----
library(tidyverse)
library(ggplot2)
library(broom)
library(MCMCglmm)
library(stargazer)
library(cowplot)
library(ggpubr)


## THEME -----
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

## DATA PREP ----
clim.fit.ms <- read.csv("mastersheets/clima_fit_sst.csv")

# Extracting individiual climatic variables
mat <- filter(clim.fit.ms, variable == "mat")
map <- filter(clim.fit.ms, variable == "map")
mat_junjul <- filter(clim.fit.ms, variable == "mat_junjul")
mat_janfeb <- filter(clim.fit.ms, variable == 'mat_janfeb')
map_janfeb <- filter(clim.fit.ms, variable == 'map_janfeb')
map_junjul <- filter(clim.fit.ms, variable == 'map_junjul')
mat_min <- filter(clim.fit.ms, variable == 'mat_min')
mat_max <- filter(clim.fit.ms, variable == 'mat_max')
map_min <- filter(clim.fit.ms, variable == 'map_min')
map_max <- filter(clim.fit.ms, variable == 'map_max')


## MEAN ANNUAL TEMPERATURE ---- 

## MAT change model
dens.mat.model <- MCMCglmm(estimate ~ Biome_type + 0, data = mat, 
                  nitt = 100000, burnin = 5000, thin = 30)
summary(dens.mat.model) #significant increase in MAT both in tundra and savanna
plot(dens.mat.model$Sol)
plot(dens.mat.model$VCV)
save(dens.mat.model, file = "models/fixed/dens.mat.model.RData")

# Density plots with MAT change over time
(dens_mat <- ggplot(mat, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.6) + 
   ylab("Density\n") + xlab("\nChange in MAT (°C per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 0.012434, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.058117, linetype = "dashed", color = "turquoise4") + 
   clima.theme)


## Cover change rate as function of MAT annual change model

# Defining prior - parameter-expanded prior with inverse Wishart distribution
a <- 1000
prior6 <- list(R = list(V = diag(1), nu = 0.002),
               G = list(G1 = list(V = diag(1), nu = 1, alpha.mu = 0, alpha.V = diag(1)*a)))

## In the tundra
mat.tundra <- filter(mat, Biome_type == "Tundra")

# Fitting the model including location as a random factor and random intercepts
mat.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, prior = prior6,
                         data = mat.tundra, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)
summary(mat.tun.mod) # significant negative slope
plot(mat.tun.mod$Sol, auto.layout = F)
plot(mat.tun.mod$VCV)
autocorr.plot(mat.tun.mod$VCV)
hist(mcmc(mat.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.tun.mod, file = "models/random/mat.tun.mod.RData")

# Checking number of studies in each tundra grid cell
grid_cell_t <- mat.tundra %>% group_by(geo.coords) %>% summarise(num_studies = length(unique(Plot.ID)))

# Predicting values from the model
pred.mat.tundra <- predict.MCMCglmm(mat.tun.mod, interval = "confidence")
pred.mat.tundra <- data.frame(pred.mat.tundra)
pred.raw.mat.tundra <- cbind(mat.tundra, pred.mat.tundra)


## In the savanna
mat.savanna <- filter(mat, Biome_type == "Savanna")

## Fitting the model
mat.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                           data = mat.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.sav.mod) #negative non-significant slope
plot(mat.sav.mod$Sol, auto.layout = F)
plot(mat.sav.mod$VCV)
autocorr.plot(mat.sav.mod$VCV)
hist(mcmc(mat.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.sav.mod, file = "models/random/mat.sav.mod.RData")

# Checking number of studies in each savanna grid cell
grid_cell_s <- mat.savanna %>% group_by(geo.coords) %>% summarise(num_studies = length(unique(Plot.ID)))

# Predicting values from the model
pred.mat.savanna <- predict.MCMCglmm(mat.sav.mod, interval = "confidence")
pred.mat.savanna <- data.frame(pred.mat.savanna)
pred.raw.mat.savanna <- cbind(mat.savanna, pred.mat.savanna)

# saving the two biomes in a dataframe
pred.raw.mat <- rbind(pred.raw.mat.savanna, pred.raw.mat.tundra)


## Plotting the two biomes in a figure
(mat.rate <- ggplot(pred.raw.mat, aes(x = estimate, y = Annual.rate, colour = Biome_type)) + geom_point(size = 2) + 
    geom_line(aes(x = estimate, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) + 
    ylab("Cover change rate\n(% per year)\n") + xlab("\nChange in MAT\n(°C per year)") + clima.theme)


## JUNE-JULY MEAN TEMPERATURE ----

## June-July MAT change model
dens.mat.junjul.mod <- MCMCglmm(estimate ~ Biome_type + 0, data = mat_junjul, 
                                nitt = 100000, burnin = 5000, thin = 30)
summary(dens.mat.junjul.mod) # there have been significant increases in tundra and savanna Jun-Jul MAT 
plot(dens.mat.junjul.mod$Sol)
plot(dens.mat.junjul.mod$VCV)
save(dens.mat.junjul.mod, file = "models/fixed/dens.mat.junjul.RData")

# Density plots with changes in June-July Mean Temp 
(dens_mat_junjul <- ggplot(mat_junjul, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.5) + 
   ylab("Density\n") + xlab("\nChange in June-July Temperature\n(°C per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 0.01745, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.04583, linetype = "dashed", color = "turquoise4") + 
   clima.theme)

## Cover change rate as function of June-July mean temperature annual change model

## In the Tundra
mat.junjul.tundra <- filter(mat_junjul, Biome_type == "Tundra")

# Fitting the model
mat.junjul.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                           data = mat.junjul.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.junjul.tun.mod) #negative non-significant
plot(mat.junjul.tun.mod$Sol, ask = F)
plot(mat.junjul.tun.mod$VCV)
autocorr.plot(mat.junjul.tun.mod$VCV)
hist(mcmc(mat.junjul.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.junjul.tun.mod, file = "models/random/mat.junjul.tun.mod.RData")

# Predicting values from the model
pred.mat.junjul.tundra <- predict.MCMCglmm(mat.junjul.tun.mod, interval = "confidence")
pred.mat.junjul.tundra <- data.frame(pred.mat.junjul.tundra)
pred.raw.mat.junjul.tundra <- cbind(mat.junjul.tundra, pred.mat.junjul.tundra)


## In the savanna
mat.junjul.savanna <- filter(mat_junjul, Biome_type == "Savanna")

# Fitting the model
mat.junjul.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                             data = mat.junjul.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.junjul.sav.mod) # negative non-significant slope
plot(mat.junjul.sav.mod$Sol, ask = F)
plot(mat.junjul.sav.mod$VCV)
autocorr.plot(mat.junjul.sav.mod$VCV)
hist(mcmc(mat.junjul.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.junjul.sav.mod, file = "models/random/mat.junjul.sav.mod.RData")

# Predicting values from the model
pred.mat.junjul.savanna <- predict.MCMCglmm(mat.junjul.sav.mod, interval = "confidence")
pred.mat.junjul.savanna <- data.frame(pred.mat.junjul.savanna)
pred.raw.mat.junjul.savanna <- cbind(mat.junjul.savanna, pred.mat.junjul.savanna)

# saving the two biomes in a dataframe
pred.raw.mat.junjul <- rbind(pred.raw.mat.junjul.savanna, pred.raw.mat.junjul.tundra)

## Plotting the two biomes in a figure
(mat.junjul.rate <- ggplot(pred.raw.mat.junjul, aes(x = estimate, y = Annual.rate, colour = Biome_type)) + geom_point(size = 2) + 
    geom_line(aes(x = estimate, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) +
    ylab("Cover change rate\n(% per year)\n") + xlab("\nChange in June-July Temperature\n(°C per year)") + clima.theme)


## JAN-FEB MEAN TEMPERATURE ----

## January-February Mean Temp over time model
dens.mat.janfeb.mod <- MCMCglmm(estimate ~ Biome_type + 0, data = mat_janfeb, 
                                nitt = 100000, burnin = 5000, thin = 30)
summary(dens.mat.janfeb.mod) #there have been significant increases in tundra and savanna Jan-Feb mean temp 
plot(dens.mat.janfeb.mod$Sol)
plot(dens.mat.janfeb.mod$VCV)
save(dens.mat.janfeb.mod, file = "models/fixed/dens.mat.janfeb.RData")

# Density plots with changes in Jan-Feb mean temp
(dens_mat_janfeb <- ggplot(mat_janfeb, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.5) + 
   ylab("Density\n") + xlab("\nChange in Jan-Feb Temperature\n(°C per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 0.02506, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.06821, linetype = "dashed", color = "turquoise4") + 
   clima.theme)

## Cover change rate as function of Jan-Feb mean temperature annual change model

## In the Tundra
mat.janfeb.tundra <- filter(mat_janfeb, Biome_type == "Tundra")

# Fitting the model 
mat.janfeb.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                  data = mat.janfeb.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.janfeb.tun.mod) #negative significant slope
plot(mat.janfeb.tun.mod$Sol, ask = F)
plot(mat.janfeb.tun.mod$VCV)
autocorr.plot(mat.janfeb.tun.mod$VCV)
hist(mcmc(mat.janfeb.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.janfeb.tun.mod, file = "models/random/mat.janfeb.tun.mod.RData")

# Predicting values from the model
pred.mat.janfeb.tundra <- predict.MCMCglmm(mat.janfeb.tun.mod, interval = "confidence")
pred.mat.janfeb.tundra <- data.frame(pred.mat.janfeb.tundra)
pred.raw.mat.janfeb.tundra <- cbind(mat.janfeb.tundra, pred.mat.janfeb.tundra)


## In the savanna
mat.janfeb.savanna <- filter(mat_janfeb, Biome_type == "Savanna")

# Fitting the model
mat.janfeb.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                    data = mat.janfeb.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.janfeb.sav.mod) # negative non-significant slope in the savanna
plot(mat.janfeb.sav.mod$Sol, ask = F)
plot(mat.janfeb.sav.mod$VCV)
autocorr.plot(mat.janfeb.sav.mod$VCV)
hist(mcmc(mat.janfeb.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.janfeb.sav.mod, file = "models/random/mat.janfeb.sav.mod.RData")

# Predicting values from the model
pred.mat.janfeb.savanna <- predict.MCMCglmm(mat.janfeb.sav.mod, interval = "confidence")
pred.mat.janfeb.savanna <- data.frame(pred.mat.janfeb.savanna)
pred.raw.mat.janfeb.savanna <- cbind(mat.janfeb.savanna, pred.mat.janfeb.savanna)

# saving the two biomes in a dataframe
pred.raw.mat.janfeb <- rbind(pred.raw.mat.janfeb.savanna, pred.raw.mat.janfeb.tundra)


## Plotting the two biomes in a figure
(mat.janfeb.rate <- ggplot(pred.raw.mat.janfeb, aes(x = estimate, y = Annual.rate, colour = Biome_type)) + geom_point(size = 2) + 
    geom_line(aes(x = estimate, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) +
    ylab("Cover change rate\n(% per year)\n") + xlab("\nChange in Jan-Feb Temperature\n(°C per year)") + clima.theme)


## MEAN ANNUAL PRECIPITATION (MAP) ----

## Changes in MAP over time model
dens.map.model <- MCMCglmm(estimate ~ Biome_type + 0, data = map, 
                           nitt = 100000, burnin = 5000, thin = 30)
summary(dens.map.model) #there have been significant increases in tundra and savanna MAP
plot(dens.map.model$Sol)
plot(dens.map.model$VCV)
save(dens.map.model, file = "models/fixed/dens.map.model.RData")

# Density plots with changes in MAP
(dens_map <- ggplot(map, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.5) + 
   ylab("Density\n") + xlab("\nChange in MAP (mm per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 0.43650, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.14759, linetype = "dashed", color = "turquoise4") + 
   clima.theme)

## Cover change rate as function of MAP annual change model

## In the Tundra
map.tundra <- filter(map, Biome_type == "Tundra")

# Fitting the model 
map.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                           data = map.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.tun.mod) #positive non-significant slope
plot(map.tun.mod$Sol, ask = F)
plot(map.tun.mod$VCV)
autocorr.plot(map.tun.mod$VCV)
hist(mcmc(map.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.tun.mod, file = "models/random/map.tun.mod.RData")

# Predicting values from the model
pred.map.tundra <- predict.MCMCglmm(map.tun.mod, interval = "confidence")
pred.map.tundra <- data.frame(pred.map.tundra)
pred.raw.map.tundra <- cbind(map.tundra, pred.map.tundra)


## In the savanna
map.savanna <- filter(map, Biome_type == "Savanna")

# Fitting the model 
map.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                             data = map.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.sav.mod) #positive almost significant slope
plot(map.sav.mod$Sol, ask = F)
plot(map.sav.mod$VCV)
autocorr.plot(map.sav.mod$VCV)
hist(mcmc(map.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.sav.mod, file = "models/random/map.sav.mod.RData")

# Predicting values from the model
pred.map.savanna <- predict.MCMCglmm(map.sav.mod, interval = "confidence")
pred.map.savanna <- data.frame(pred.map.savanna)
pred.raw.map.savanna <- cbind(map.savanna, pred.map.savanna)

# saving the two biomes in a dataframe
pred.raw.map <- rbind(pred.raw.map.savanna, pred.raw.map.tundra)


## Plotting the two biomes in a figure
(map.rate <- ggplot(pred.raw.map, aes(x = estimate, y = Annual.rate, colour = Biome_type)) + geom_point(size = 2) + 
    geom_line(aes(x = estimate, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) +
    ylab("Cover change rate\n(% per year)\n") + xlab("\nChange in MAP\n(mm per year)") + clima.theme)     


## JAN-FEB MEAN PRECIPITATION ----

## Jan-Feb mean precip changes over time model
dens.map.janfeb.mod <- MCMCglmm(estimate ~ Biome_type + 0, data = map_janfeb, 
                                nitt = 100000, burnin = 5000, thin = 30)
summary(dens.map.janfeb.mod) #precipitation has increased in the savanna significantly
plot(dens.map.janfeb.mod$Sol)
plot(dens.map.janfeb.mod$VCV)
save(dens.map.janfeb.mod, file = "models/fixed/dens.map.janfeb.RData")

# Density plots with changes in January-February Mean Prec
(dens_map_janfeb <- ggplot(map_janfeb, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.5) + 
   ylab("Density\n") + xlab("\nChange in Jan-Feb Precipitation\n(mm per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 0.65082, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.17830, linetype = "dashed", color = "turquoise4") + 
   clima.theme)

## Cover change rate as function of Jan-Feb mean precip annual change model

## In the Tundra
map.janfeb.tundra <- filter(map_janfeb, Biome_type == "Tundra")

# Fitting the model 
map.janfeb.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                  data = map.janfeb.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.janfeb.tun.mod) #negative non-significant slope
plot(map.janfeb.tun.mod$Sol, ask = F)
plot(map.janfeb.tun.mod$VCV)
autocorr.plot(map.janfeb.tun.mod$VCV)
hist(mcmc(map.janfeb.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.janfeb.tun.mod, file = "models/random/map.janfeb.tun.mod.RData")

# Predicting values from the model
pred.map.janfeb.tundra <- predict.MCMCglmm(map.janfeb.tun.mod, interval = "confidence")
pred.map.janfeb.tundra <- data.frame(pred.map.janfeb.tundra)
pred.raw.map.janfeb.tundra <- cbind(map.janfeb.tundra, pred.map.janfeb.tundra)


## In the savanna
map.janfeb.savanna <- filter(map_janfeb, Biome_type == "Savanna")

# Fitting the model
map.janfeb.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                    data = map.janfeb.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.janfeb.sav.mod) #positive non-significant slope
plot(map.janfeb.sav.mod$Sol, ask = F)
plot(map.janfeb.sav.mod$VCV)
autocorr.plot(map.janfeb.sav.mod$VCV)
hist(mcmc(map.janfeb.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.janfeb.sav.mod, file = "models/random/map.janfeb.sav.mod.RData")

# Predicting values from the model
pred.map.janfeb.savanna <- predict.MCMCglmm(map.janfeb.sav.mod, interval = "confidence")
pred.map.janfeb.savanna <- data.frame(pred.map.janfeb.savanna)
pred.raw.map.janfeb.savanna <- cbind(map.janfeb.savanna, pred.map.janfeb.savanna)

# saving the two biomes in a dataframe
pred.raw.map.janfeb <- rbind(pred.raw.map.janfeb.savanna, pred.raw.map.janfeb.tundra)


## Plotting the two biomes in a figure
(map.janfeb.rate <- ggplot(pred.raw.map.janfeb, aes(x = estimate, y = Annual.rate, colour = Biome_type)) + geom_point(size = 2) + 
    geom_line(aes(x = estimate, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) +
    ylab("Cover change rate\n(% per year)\n") + xlab("\nChange in Jan-Feb Precipitation\n(mm per year)") + clima.theme)


## JUNE-JULY MEAN PRECIPITATION ----

## Changes in June-July mean precip over time
dens.map.junjul.mod <- MCMCglmm(estimate ~ Biome_type + 0, data = map_junjul, 
                                nitt = 100000, burnin = 5000, thin = 30)
summary(dens.map.junjul.mod)
plot(dens.map.junjul.mod$Sol)
plot(dens.map.junjul.mod$VCV)
# there has been a significant increase in the savanna, not for tundra.
save(dens.map.junjul.mod, file = "models/fixed/dens.map.junjul.RData")

# Density plots with changes in June-July mean precip
(dens_map_junjul <- ggplot(map_junjul, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.5) + 
   ylab("Density\n") + xlab("\nChange in June-July Precipitation\n(mm per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 0.59459, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = -0.02393, linetype = "dashed", color = "turquoise4") + 
   clima.theme)


## Cover change rate as function of June-July mean precip annual change model

## In the Tundra
map.junjul.tundra <- filter(map_junjul, Biome_type == "Tundra")

# Fitting the model
map.junjul.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                  data = map.junjul.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.junjul.tun.mod) #positive non-significant slope
plot(map.junjul.tun.mod$Sol, ask = F)
plot(map.junjul.tun.mod$VCV)
autocorr.plot(map.junjul.tun.mod$VCV)
hist(mcmc(map.junjul.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.junjul.tun.mod, file = "models/random/map.junjul.tun.mod.RData")

# Predicting values from the model
pred.map.junjul.tundra <- predict.MCMCglmm(map.junjul.tun.mod, interval = "confidence")
pred.map.junjul.tundra <- data.frame(pred.map.junjul.tundra)
pred.raw.map.junjul.tundra <- cbind(map.junjul.tundra, pred.map.junjul.tundra)


## In the savanna
map.junjul.savanna <- filter(map_junjul, Biome_type == "Savanna")

# Fitting the model 
map.junjul.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                    data = map.junjul.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.junjul.sav.mod) #negative non-significant slope
plot(map.junjul.sav.mod$Sol, ask = F)
plot(map.junjul.sav.mod$VCV)
autocorr.plot(map.junjul.sav.mod$VCV)
hist(mcmc(map.junjul.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.junjul.sav.mod, file = "models/random/map.junjul.sav.mod.RData")

# Predicting values from the model
pred.map.junjul.savanna <- predict.MCMCglmm(map.junjul.sav.mod, interval = "confidence")
pred.map.junjul.savanna <- data.frame(pred.map.junjul.savanna)
pred.raw.map.junjul.savanna <- cbind(map.junjul.savanna, pred.map.junjul.savanna)

# saving the two biomes in a dataframe
pred.raw.map.junjul <- rbind(pred.raw.map.junjul.savanna, pred.raw.map.junjul.tundra)


## Plotting the two biomes in a figure
(map.junjul.rate <- ggplot(pred.raw.map.junjul, aes(x = estimate, y = Annual.rate, colour = Biome_type)) + geom_point(size = 2) + 
    geom_line(aes(x = estimate, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) + 
    ylab("Cover change rate\n(% per year)\n") + xlab("\nChange in June-July Precipitation\n(mm per year)") + clima.theme)


## MINIMUM TEMPERATURE ----

## Changes in Minimum Temp over time
dens.mat.min <- MCMCglmm(estimate ~ Biome_type + 0, data = mat_min, 
                         nitt = 100000, burnin = 5000, thin = 30)
summary(dens.mat.min) #there have been significant increases in tundra min MAT, but not in the savanna
plot(dens.mat.min$Sol)
plot(dens.mat.min$VCV)
save(dens.mat.min, file = "models/fixed/dens.mat.min.RData")

# Density plots with changes in Min Temp 
(dens_mat_min <- ggplot(mat_min, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.5) + 
   ylab("Density\n") + xlab("\nChange in Minimum Temperature\n(°C per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 0.000613, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.057310, linetype = "dashed", color = "turquoise4") + 
   clima.theme)

## Cover change rate as function of minimum temperature annual change model

## In the Tundra
mat.min.tundra <- filter(mat_min, Biome_type == "Tundra")

# Fitting the model 
mat.min.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                           data = mat.min.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.min.tun.mod) #significant negative slope
plot(mat.min.tun.mod$Sol, ask = F)
plot(mat.min.tun.mod$VCV)
autocorr.plot(mat.min.tun.mod$VCV)
hist(mcmc(mat.min.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.min.tun.mod, file = "models/random/mat.min.tun.mod.RData")

# Predicting values from the model
pred.mat.min.tundra <- predict.MCMCglmm(mat.min.tun.mod, interval = "confidence")
pred.mat.min.tundra <- data.frame(pred.mat.min.tundra)
pred.raw.mat.min.tundra <- cbind(mat.min.tundra, pred.mat.min.tundra)


## In the savanna
mat.min.savanna <- filter(mat_min, Biome_type == "Savanna")

# Fitting the model
mat.min.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                             data = mat.min.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.min.sav.mod) #negative non-signficant slope
plot(mat.min.sav.mod$Sol, ask = F)
plot(mat.min.sav.mod$VCV)
autocorr.plot(mat.min.sav.mod$VCV)
hist(mcmc(mat.min.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.min.sav.mod, file = "models/random/mat.min.sav.mod.RData")

# Predicting values from the model
pred.mat.min.savanna <- predict.MCMCglmm(mat.min.sav.mod, interval = "confidence")
pred.mat.min.savanna <- data.frame(pred.mat.min.savanna)
pred.raw.mat.min.savanna <- cbind(mat.min.savanna, pred.mat.min.savanna)

# saving the two biomes in a dataframe
pred.raw.mat.min <- rbind(pred.raw.mat.min.savanna, pred.raw.mat.min.tundra)


## Plotting the two biomes in a figure
(mat.min.rate <- ggplot(pred.raw.mat.min, aes(x = estimate, y = Annual.rate, colour = Biome_type)) + geom_point(size = 2) + 
    geom_line(aes(x = estimate, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) + 
    ylab("Cover change rate\n(% per year)\n") + xlab("\nChange in Minimum Temperature\n(°C per year)") + clima.theme)


## MAXIMUM TEMPERATURE ----

## Changes in maximum temperature over time model
dens.mat.max <- MCMCglmm(estimate ~ Biome_type + 0, data = mat_max, 
                         nitt = 100000, burnin = 5000, thin = 30)
summary(dens.mat.max) # Max Temp has increased in both biomes significantly
plot(dens.mat.max$Sol)
plot(dens.mat.max$VCV)
save(dens.mat.max, file = "models/fixed/dens.mat.max.RData")

# Density plots with changes in max temp
(dens_mat_max <- ggplot(mat_max, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.5) + 
   ylab("Density\n") + xlab("\nChange in Maximum Temperature\n(°C per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 0.01780, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.04449, linetype = "dashed", color = "turquoise4") + 
   clima.theme)


## Cover change rate as function of maximum temp annual change model

## In the Tundra
mat.max.tundra <- filter(mat_max, Biome_type == "Tundra")

# Fitting the model 
mat.max.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = mat.max.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.max.tun.mod) #negative non-significant slope
plot(mat.max.tun.mod$Sol, ask = F)
plot(mat.max.tun.mod$VCV)
autocorr.plot(mat.max.tun.mod$VCV)
hist(mcmc(mat.max.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.max.tun.mod, file = "models/random/mat.max.tun.mod.RData")

# Predicting values from the model
pred.mat.max.tundra <- predict.MCMCglmm(mat.max.tun.mod, interval = "confidence")
pred.mat.max.tundra <- data.frame(pred.mat.max.tundra)
pred.raw.mat.max.tundra <- cbind(mat.max.tundra, pred.mat.max.tundra)


## In the savanna
mat.max.savanna <- filter(mat_max, Biome_type == "Savanna")

# Fitting the model
mat.max.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = mat.max.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(mat.max.sav.mod) #negative non-significant slope
plot(mat.max.sav.mod$Sol, ask = F)
plot(mat.max.sav.mod$VCV)
autocorr.plot(mat.max.sav.mod$VCV)
hist(mcmc(mat.max.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(mat.max.sav.mod, file = "models/random/mat.max.sav.mod.RData")

# Predicting values from the model
pred.mat.max.savanna <- predict.MCMCglmm(mat.max.sav.mod, interval = "confidence")
pred.mat.max.savanna <- data.frame(pred.mat.max.savanna)
pred.raw.mat.max.savanna <- cbind(mat.max.savanna, pred.mat.max.savanna)

# saving the two biomes in a dataframe
pred.raw.mat.max <- rbind(pred.raw.mat.max.savanna, pred.raw.mat.max.tundra)


## Plotting the two biomes in a figure
(mat.max.rate <- ggplot(pred.raw.mat.max, aes(x = estimate, y = Annual.rate, colour = Biome_type)) + geom_point(size = 2) + 
    geom_line(aes(x = estimate, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) +
    ylab("Cover change rate\n(% per year)\n") + xlab("\nChange in Maximum Temperature\n(°C per year)") + clima.theme)


## MINIMUM PRECIPITATION ----

## Minimum precip changes over time model
dens.map.min <- MCMCglmm(estimate ~ Biome_type + 0, data = map_min, 
                         nitt = 100000, burnin = 5000, thin = 30)
summary(dens.map.min) # min MAP has increased significantly in the tundra, but not in the savanna
plot(dens.map.min$Sol)
plot(dens.map.min$VCV)
save(dens.map.min, file = "models/fixed/dens.map.min.RData")

# Density plots with changes in Min Precip
(dens_map_min <- ggplot(map_min, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.5) + 
   ylab("Density\n") + xlab("\nChange in Minimum Precipitation\n(mm per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = -0.02534, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.17287, linetype = "dashed", color = "turquoise4") + 
   clima.theme)

## Cover change rate as function of minimum precip annual change model

## In the Tundra
map.min.tundra <- filter(map_min, Biome_type == "Tundra")

# Fitting the model 
map.min.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = map.min.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.min.tun.mod) #negative significant slope
plot(map.min.tun.mod$Sol, ask = F)
plot(map.min.tun.mod$VCV)
autocorr.plot(map.min.tun.mod$VCV)
hist(mcmc(map.min.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.min.tun.mod, file = "models/random/map.min.tun.mod.RData")

# Predicting values from the model
pred.map.min.tundra <- predict.MCMCglmm(map.min.tun.mod, interval = "confidence")
pred.map.min.tundra <- data.frame(pred.map.min.tundra)
pred.raw.map.min.tundra <- cbind(map.min.tundra, pred.map.min.tundra)


## In the savanna
map.min.savanna <- filter(map_min, Biome_type == "Savanna")

# Fitting the model 
map.min.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = map.min.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.min.sav.mod) #negative significant slope
plot(map.min.sav.mod$Sol, ask = F)
plot(map.min.sav.mod$VCV)
autocorr.plot(map.min.sav.mod$VCV)
hist(mcmc(map.min.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.min.sav.mod, file = "models/random/map.min.sav.mod.RData")

# Predicting values from the model
pred.map.min.savanna <- predict.MCMCglmm(map.min.sav.mod, interval = "confidence")
pred.map.min.savanna <- data.frame(pred.map.min.savanna)
pred.raw.map.min.savanna <- cbind(map.min.savanna, pred.map.min.savanna)

# saving the two biomes in a dataframe
pred.raw.map.min <- rbind(pred.raw.map.min.savanna, pred.raw.map.min.tundra)


## Plotting the two biomes in a figure
(map.min.rate <- ggplot(pred.raw.map.min, aes(x = estimate, y = Annual.rate, colour = Biome_type)) + geom_point(size = 2) + 
    geom_line(aes(x = estimate, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) + 
    ylab("Cover change rate\n(% per year)\n") + xlab("\nChange in Minimum Precipitation\n(mm per year)") + clima.theme)


## MAXIMUM PRECIPITATION ----

## Maximum prep change over time model
dens.map.max <- MCMCglmm(estimate ~ Biome_type + 0, data = map_max, 
                         nitt = 100000, burnin = 5000, thin = 30)
summary(dens.map.max) # Max Prec has increased significantly in the savanna but not in the tundra
plot(dens.map.max$Sol)
plot(dens.map.max$VCV)
save(dens.map.max, file = "models/fixed/dens.map.max.RData")

# Density plots with changes in Max Prec 
(dens_map_max <- ggplot(map_max, aes(x = estimate, fill = Biome_type)) + geom_density(alpha = 0.5) + 
   ylab("Density\n") + xlab("\nChange in Maximum Precipitation\n(mm per year)") +
   geom_vline(xintercept = 0) + 
   geom_vline(xintercept = 2.8325, linetype = "dashed", color = "salmon") + 
   geom_vline(xintercept = 0.1971, linetype = "dashed", color = "turquoise4") + 
   clima.theme)


## Cover change rate as function of max precip annual change model

## In the Tundra
map.max.tundra <- filter(map_max, Biome_type == "Tundra")

# Fitting the model
map.max.tun.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                               data = map.max.tundra, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.max.tun.mod) #positive non-significant slope
plot(map.max.tun.mod$Sol, ask = F)
plot(map.max.tun.mod$VCV)
autocorr.plot(map.max.tun.mod$VCV)
hist(mcmc(map.max.tun.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.max.tun.mod, file = "models/random/map.max.tun.mod.RData")

# Predicting values from the model
pred.map.max.tundra <- predict.MCMCglmm(map.max.tun.mod, interval = "confidence")
pred.map.max.tundra <- data.frame(pred.map.max.tundra)
pred.raw.map.max.tundra <- cbind(map.max.tundra, pred.map.max.tundra)


## In the savanna
map.max.savanna <- filter(map_max, Biome_type == "Savanna")

# Fitting the model
map.max.sav.mod <- MCMCglmm(Annual.rate ~ estimate, random = ~us(1):geo.coords, 
                                 data = map.max.savanna, prior = prior6, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(map.max.sav.mod) #positive significant slope
plot(map.max.sav.mod$Sol, ask = F)
plot(map.max.sav.mod$VCV)
autocorr.plot(map.max.sav.mod$VCV)
hist(mcmc(map.max.sav.mod$VCV)[,"(Intercept):(Intercept).geo.coords"])
save(map.max.sav.mod, file = "models/random/map.max.sav.mod.RData")

# Predicting values from the model
pred.map.max.savanna <- predict.MCMCglmm(map.max.sav.mod, interval = "confidence")
pred.map.max.savanna <- data.frame(pred.map.max.savanna)
pred.raw.map.max.savanna <- cbind(map.max.savanna, pred.map.max.savanna)

# saving the two biomes in a dataframe
pred.raw.map.max <- rbind(pred.raw.map.max.savanna, pred.raw.map.max.tundra)


## Plotting the two biomes in a figure
(map.max.rate <- ggplot(pred.raw.map.max, aes(x = estimate, y = Annual.rate, colour = Biome_type)) + geom_point(size = 2) + 
    geom_line(aes(x = estimate, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) +
    ylab("Cover change rate\n(% per year)\n") + xlab("\nChange in Maximum Precipitation\n(mm per year)") + clima.theme)


## EFFECT SIZES (RATE VS RATE MODELS) ----

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
dataList <- list(map.janfeb.sav.mod, map.janfeb.tun.mod, map.junjul.sav.mod, map.junjul.tun.mod, map.max.sav.mod,
                 map.max.tun.mod, map.min.sav.mod, map.min.tun.mod, map.sav.mod, map.tun.mod,
                 mat.janfeb.sav.mod, mat.janfeb.tun.mod, mat.junjul.sav.mod, mat.junjul.tun.mod, mat.max.sav.mod, 
                 mat.max.tun.mod, mat.min.sav.mod, mat.min.tun.mod, mat.sav.mod, mat.tun.mod)

# Create lists of input model names
dataListNames <- list("MAP Jan-Feb Savanna", "MAP Jan-Feb Tundra", "MAP Jun-Jul Savanna", "MAP Jun-Jul Tundra", "MAP Max Savanna",
                      "MAP Max Tundra", "MAP Min Savanna", "MAP Min Tundra", "MAP Savanna", "MAP Tundra",
                      "MAT Jan-Feb Savanna", "MAT Jan-Feb Tundra", "MAT Jun-Jul Savanna", "MAT Jun-Jul Tundra", "MAT Max Savanna",
                      "MAT Max Tundra", "MAT Min Savanna", "MAT Min Tundra", "MAT Savanna", "MAT Tundra")

# Get model outputs and add model names
readyList <- mapply(cbind, lapply(dataList, clean.MCMC), "modelName" = dataListNames, SIMPLIFY = F)

# Turn list of dataframes into a dataframe
mcmcOutputs <- as.data.frame(do.call(rbind, readyList), stringsAsFactors = FALSE)

# Prepare data for plotting
mcmcOutputs.estimate <- mcmcOutputs %>% filter(variable == "estimate")
mcmcOutputs.estimate$Biome[mcmcOutputs.estimate$modelName %in% c("MAP Jan-Feb Savanna", "MAP Jun-Jul Savanna", "MAP Max Savanna", "MAP Min Savanna", "MAP Savanna", 
                                                       "MAT Jan-Feb Savanna",  "MAT Jun-Jul Savanna",  "MAT Max Savanna",
                                                        "MAT Min Savanna", "MAT Savanna")] <- "Savanna"

mcmcOutputs.estimate$Biome[mcmcOutputs.estimate$modelName %in% c("MAP Jan-Feb Tundra", "MAP Jun-Jul Tundra", "MAP Max Tundra","MAP Min Tundra", 
                                                                 "MAP Tundra", "MAT Jan-Feb Tundra","MAT Jun-Jul Tundra","MAT Max Tundra",
                                                                 "MAT Min Tundra", "MAT Tundra")] <- "Tundra"
# Create two dataframes for each type of climatic variable
mcmcOutputs.estimate.temp <- mcmcOutputs.estimate %>% filter(modelName %in% c("MAT Jan-Feb Savanna", "MAT Jan-Feb Tundra", "MAT Jun-Jul Savanna", 
                                                             "MAT Jun-Jul Tundra", "MAT Max Savanna", "MAT Max Tundra", "MAT Min Savanna",
                                                             "MAT Min Tundra", "MAT Savanna", "MAT Tundra"))
mcmcOutputs.estimate.temp$clim.var <- c("Jan-Feb", "Jan-Feb", "Jun-Jul", "Jun-Jul", "Max", "Max", "Min", "Min", "MAT", "MAT")

mcmcOutputs.estimate.prec <- mcmcOutputs.estimate %>% filter(modelName %in% c("MAP Jan-Feb Savanna", "MAP Jan-Feb Tundra","MAP Jun-Jul Savanna", 
                                                             "MAP Jun-Jul Tundra", "MAP Max Savanna", "MAP Max Tundra", "MAP Min Savanna", 
                                                             "MAP Min Tundra", "MAP Savanna", "MAP Tundra"))
mcmcOutputs.estimate.prec$clim.var <- c("Jan-Feb", "Jan-Feb", "Jun-Jul", "Jun-Jul", "Max", "Max", "Min", "Min", "MAP", "MAP")


# plotting temperature effect sizes
(effect.sizes.temp <- ggplot(mcmcOutputs.estimate.temp, aes(x = factor(clim.var), y = post.mean, fill = factor(Biome))) + 
    geom_bar(stat = "identity", alpha = 0.6, position=position_dodge(), width = 0.7) +
    geom_errorbar(aes(ymin=l.95..CI, ymax=u.95..CI), position = position_dodge(0.7), width = 0.25) + 
    geom_hline(yintercept=0) + ylab("Cover change rate (% per year)/\nTemperature change rate (°C per year)\n") + xlab("") +
    scale_x_discrete(labels=c("Jan-Feb", "Jun-Jul", "MAT", "Max", "Min")) + clima.theme +
    theme(axis.text.x  = element_text(vjust=0.5, size=26, colour = "black"), 
          axis.title.y = element_text(face="bold", size=26),
          axis.text.y  = element_text(vjust=0.5, size=26, colour = "black"), 
          legend.text = element_text(size=26), legend.spacing.x = unit(0.3, 'cm')))

# significant effects at pMCMC < 0.05
(effect.sizes.temp.sig <- effect.sizes.temp + 
    annotate("text", x = 3.18, y = 0.5, label = "*", size = 10) + 
    annotate("text", x = 1.18, y = 0.5, label = "*", size = 10) + 
    annotate("text", x = 5.18, y = 0.5, label = "*", size = 10))

# plotting precipitation effect sizes
(effect.sizes.prec <- ggplot(mcmcOutputs.estimate.prec, aes(x = factor(clim.var), y = post.mean, fill = factor(Biome))) + 
    geom_bar(stat = "identity", alpha = 0.6, position=position_dodge(), width = 0.7) + 
    geom_errorbar(aes(ymin=l.95..CI, ymax=u.95..CI), position = position_dodge(0.7), width = 0.25) + 
    geom_hline(yintercept=0) + ylab("Cover change rate (% per year)/\nPrecipitation change rate (mm per year)\n") + xlab("") + 
    scale_x_discrete(labels=c("Jan-Feb", "Jun-Jul","MAP", "Max", "Min")) + clima.theme + 
    theme(axis.text.x  = element_text(vjust=0.5, size=26, colour = "black"), 
          axis.title.y = element_text(face="bold", size=26),
          axis.text.y  = element_text(vjust=0.5, size=26, colour = "black"), 
          legend.text = element_text(size=26), legend.spacing.x = unit(0.3, 'cm')))
# these plots indicate the relative strength of the relationships between climatic change rates and annual cover change rates 

# significant effects at pMCMC < 0.05
(effect.sizes.prec.sig <- effect.sizes.prec + 
    annotate("text", x = 3.82, y = 0.1, label = "*", size = 10) + 
    annotate("text", x = 4.82, y = 0.1, label = "*", size = 10) + 
    annotate("text", x = 5.19, y = 0.1, label = "*", size = 10))



## FIGURE PANELS ----

## Density plots - climatic change (Figure 3)
(density.panels.c <- ggpubr::ggarrange(dens_mat, dens_map, dens_mat_janfeb, dens_map_janfeb, dens_mat_junjul, dens_map_junjul,
                                       dens_mat_min, dens_map_min, dens_mat_max, dens_map_max, 
                                       labels = c("(a)", "(f)", "(b)", "(g)", "(c)", "(h)", "(d)", "(i)", "(e)", "(j)"),
                             font.label = list(size = 32), ncol = 2, nrow = 5, common.legend = TRUE, legend = "top"))
ggsave(density.panels.c, filename = "figures/Figure_3.png", 
       width = 41, height = 70, units = "cm")

## Cover change rates vs. annual climatic change rates stacked in panel (Figure S6)
(rates.panel <- ggpubr::ggarrange(mat.rate, map.rate, mat.janfeb.rate, map.janfeb.rate, mat.junjul.rate, map.junjul.rate, 
                                  mat.min.rate, map.min.rate, mat.max.rate, map.max.rate, 
                                  labels = c("(a)", "(f)", "(b)", "(g)", "(c)", "(h)", "(d)", "(i)", "(e)", "(j)"),
                          font.label = list(size = 26), ncol =2, nrow = 5, common.legend = TRUE, legend = "top"))
ggsave(rates.panel, filename = "figures/Figure_S6.png", 
       width = 35, height = 70, units = "cm")

# If you get an error saying 'scales not found' when arranging them in a panel, make sure to run all the
# code for all the plots again, and then it should work



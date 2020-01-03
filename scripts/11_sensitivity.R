#### Woody encroachment across biomes 
#### Script 11. Sensitivity analysis
#### Mariana Garcia
#### April 2019


## Libraries ----
#.libPaths("C:/R_library")
library(dplyr)
library(broom)
library(tidyverse)
library(MCMCglmm)
library(ggpubr)

# Sensitivy analysis of woody cover as a function of MAT/MAP
# Sensitivity means that where temperatures are warmer/rainfall is higher, plant cover is higher
# Start cover as a function of temp/rainfall start, end cover as a function of temp/rainfall end

# Two points for each site: initial cover and temp, and final cover and temp
# Slope is the temperature sensitivity of that species

## Setting the theme -----
clima.theme <- theme(legend.position = "right", legend.title = element_blank(), 
                     legend.text = element_text(size=26), legend.key = element_blank(), 
                     legend.spacing.x = unit(0.3, 'cm'),
                     axis.title.x = element_text(face="bold", size=21),
                     axis.text.x  = element_text(vjust=0.5, size=26, colour = "black"), 
                     axis.title.y = element_text(face="bold", size=21),
                     axis.text.y  = element_text(vjust=0.5, size=26, colour = "black"),
                     panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), 
                     panel.grid.minor.y=element_blank(), panel.grid.major.y=element_blank(), 
                     panel.background = element_blank(), axis.line = element_line(colour = "black"), 
                     plot.margin = unit(c(1,1,1,1), units = , "cm"))


## Load database (from script #6) ----
clima.fit <- read.csv("scripts/users/mgarciacriado/encroachment_paper/mastersheets/clima_fit.csv")

# Add Site.ID to the mastersheet (this function is amazing)
clima.fit.id <- transform(clima.fit, Site.ID = as.numeric(interaction(Latitude, Longitude, drop = TRUE)))

# Filter to keep only start and end climatic values
start.clim <- clima.fit.id %>% filter(year == Start_year) %>% mutate(cover = Start_cover) %>% mutate(clim_start = value)
final.clim <- clima.fit.id %>% filter(year == End_year) %>% mutate(cover = End_cover) %>% mutate(clim_end = value)
sensitivity.clim <- full_join(start.clim, final.clim, by = c("Plot.ID", "variable"), copy = FALSE)

#### MEAN ANNUAL TEMPERATURE ----

# we remove the records for which there are no start or cover values 
# or that don't have climatic data available for start or end date (i.e. before 1979 or after 2013)
sensit.mat <- filter(sensitivity.clim, variable == "mat")
sensit.mat <- sensit.mat %>% drop_na(Start_cover.x, End_cover.x, clim_start, clim_end) 
sensit.mat <- subset(sensit.mat, Start_cover.x !="not reported")
sensit.mat <- subset(sensit.mat, Start_cover.x !="not recorded")
sensit.mat <- subset(sensit.mat, Start_cover.x !="")
sensit.mat$Start_cover.x <- as.character(sensit.mat$Start_cover.x)
sensit.mat$Start_cover.x <- as.numeric(sensit.mat$Start_cover.x)

sensit.mat <- subset(sensit.mat, End_cover.x !="not reported")
sensit.mat <- subset(sensit.mat, End_cover.x !="not recorded")
sensit.mat <- subset(sensit.mat, End_cover.x !="")
sensit.mat$End_cover.x <- as.character(sensit.mat$End_cover.x)
sensit.mat$End_cover.x <- as.numeric(sensit.mat$End_cover.x)


# calculate the difference in climatic conditions per record
sensitivity.mat.dif <- sensit.mat %>% mutate(climbaseline = 0) %>% mutate(climdif = clim_end - clim_start)
sensitivity.mat.dif <- dplyr::select(sensitivity.mat.dif, -5, -6, -7)

# filter per biome
sensit.mat.tun <- filter(sensitivity.mat.dif, Biome_type.x == "Tundra")
sensit.mat.sav <- filter(sensitivity.mat.dif, Biome_type.x == "Savanna")


# For models with one random effect
prior <- list(R = list(V = 1, nu = 0.002),
              G = list(G1 = list(V = diag(2), nu = 2, alpha.mu = rep(0, 2), alpha.V = diag(10000, 2, 2))))


#### Tundra temperature increases only ----
sensit.mat.inc.tun <- filter(sensit.mat.tun, climdif > 0)
sensit.mat.inc.tun <- subset(sensit.mat.inc.tun, climdif !="NA")

## Tundra Temperature MCMCglmm model
sens.mat.inc.tun.mod <- MCMCglmm(Total_cover_change.x ~ climdif, random = ~us(1 + climdif):Site.ID.x, 
                                 data = sensit.mat.inc.tun, prior = prior, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(sens.mat.inc.tun.mod)
# positive slope non-significant
plot(sens.mat.inc.tun.mod$Sol, auto.layout = F)
plot(sens.mat.inc.tun.mod$VCV)
autocorr.plot(sens.mat.inc.tun.mod$VCV)
save(sens.mat.inc.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/sens_mat_inc_tun.RData")

## Tundra Temperature MCMC predictions
pred.matdif.tun <- predict.MCMCglmm(sens.mat.inc.tun.mod, interval = "confidence")
pred.matdif.tun <- data.frame(pred.matdif.tun)
pred.raw.matdif.tun <- cbind(sensit.mat.inc.tun, pred.matdif.tun)




#### Savanna Temperature increases model ----
sensit.mat.inc.sav <- filter(sensit.mat.sav, climdif > 0)
sensit.mat.inc.sav <- subset(sensit.mat.inc.sav, climdif !="NA")

sens.mat.inc.sav.mod <- MCMCglmm(Total_cover_change.x ~ climdif, random = ~us(1 + climdif):Site.ID.x, 
                                 data = sensit.mat.inc.sav, prior = prior, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(sens.mat.inc.sav.mod)
#positive non-significant slope
plot(sens.mat.inc.sav.mod$Sol, auto.layout = F)
plot(sens.mat.inc.sav.mod$VCV)
autocorr.plot(sens.mat.inc.sav.mod$VCV)
save(sens.mat.inc.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/sens_mat_sav.RData")

## Savanna temperature MCMC predictions
pred.matdif.sav <- predict.MCMCglmm(sens.mat.inc.sav.mod, interval = "confidence")
pred.matdif.sav <- data.frame(pred.matdif.sav)
pred.raw.matdif.sav <- cbind(sensit.mat.inc.sav, pred.matdif.sav)


## Plot temp and cover differences  ----
# without hyperparameters (more biologically meaningful)
(sens.mat.dif <- ggplot(sensitivity.mat.dif, x = climdif, y = End_cover.x) + 
   geom_segment(mapping = aes(x = climbaseline, y = Start_cover.x, xend = climdif, yend = End_cover.x, group = factor(Plot.ID), 
                              color=Biome_trend.x), size = 1.5, alpha = 0.6, data = sensitivity.mat.dif) + geom_vline(xintercept = 0) +
   scale_color_manual(values = c("indianred2", "steelblue1", "gray56", "darkred", "darkblue", "gray28"), 
                      guide = guide_legend(title = "Cover trends"), 
                      labels = c("Savanna decrease", "Savanna increase", "Savanna stable", 
                                 "Tundra decrease", "Tundra increase", "Tundra stable")) + ylim(0, 100) +
   ylab("Woody cover (%)\n") + xlab("\nDifference in Mean Annual Temperature(°C)") + 
  theme(legend.key = element_blank()) + clima.theme)



#### MEAN ANNUAL PRECIPITATION ----

# we remove the records for which there are no start or cover values 
# or that don't have climatic data available for start or end date (i.e. before 1979 or after 2013)
sensit.map <- filter(sensitivity.clim, variable == "map")
sensit.map <- sensit.map %>% drop_na(Start_cover.x, End_cover.x, clim_start, clim_end)
sensit.map <- subset(sensit.map, Start_cover.x !="not reported")
sensit.map <- subset(sensit.map, Start_cover.x !="not recorded")
sensit.map <- subset(sensit.map, Start_cover.x !="")
sensit.map$Start_cover.x <- as.character(sensit.map$Start_cover.x)
sensit.map$Start_cover.x <- as.numeric(sensit.map$Start_cover.x)

sensit.map <- subset(sensit.map, End_cover.x !="not reported")
sensit.map <- subset(sensit.map, End_cover.x !="not recorded")
sensit.map <- subset(sensit.map, End_cover.x !="")
sensit.map$End_cover.x <- as.character(sensit.map$End_cover.x)
sensit.map$End_cover.x <- as.numeric(sensit.map$End_cover.x)


# calculate the difference per record
sensitivity.map.dif <- sensit.map %>% mutate(climbaseline = 0) %>% mutate(climdif = clim_end - clim_start)
sensitivity.map.dif <- dplyr::select(sensitivity.map.dif, -5, -6, -7)
sensitivity.map.dif <- subset(sensitivity.map.dif, Biome_trend.y != "NA")

# filter per biome
sensit.map.tun <- filter(sensitivity.map.dif, Biome_type.y == "Tundra")
sensit.map.sav <- filter(sensitivity.map.dif, Biome_type.y == "Savanna")


## Savanna precip increases only ----
sensit.map.inc.sav <- filter(sensit.map.sav, climdif > 0)
sensit.map.inc.sav <- subset(sensit.map.inc.sav, climdif !="NA")

sens.map.inc.sav.mod <- MCMCglmm(Total_cover_change.x ~ climdif, random = ~us(1 + climdif):Site.ID.x, 
                                 data = sensit.map.inc.sav, prior = prior, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(sens.map.inc.sav.mod)
#positive significant slope
plot(sens.map.inc.sav.mod$Sol, auto.layout = F)
plot(sens.map.inc.sav.mod$VCV)
autocorr.plot(sens.map.inc.sav.mod$VCV) #quite high auto-correlation
save(sens.map.inc.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/sens_map_sav.RData")

## Savanna temperature MCMC predictions
pred.mapdif.sav <- predict.MCMCglmm(sens.map.inc.sav.mod, interval = "confidence")
pred.mapdif.sav <- data.frame(pred.mapdif.sav)
pred.raw.mapdif.sav <- cbind(sensit.map.inc.sav, pred.mapdif.sav)



## Tundra precip increases only ----
sensit.map.inc.tun <- filter(sensit.map.tun, climdif > 0)
sensit.map.inc.tun <- subset(sensit.map.inc.tun, climdif !="NA")

## Tundra precip increases model
sens.map.inc.tun.mod <- MCMCglmm(Total_cover_change.x ~ climdif, random = ~us(1 + climdif):Site.ID.x, 
                                 data = sensit.map.inc.tun, prior = prior, nitt = 200000, burnin = 30000, thin = 50, pr=TRUE)

summary(sens.map.inc.tun.mod)
# positive slope non-significant
plot(sens.map.inc.tun.mod$Sol, auto.layout = F)
plot(sens.map.inc.tun.mod$VCV)
autocorr.plot(sens.map.inc.tun.mod$VCV)
save(sens.map.inc.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/random/sens_map_tun.RData")

## Tundra precip increases predictions
pred.mapdif.tun <- predict.MCMCglmm(sens.map.inc.tun.mod, interval = "confidence")
pred.mapdif.tun <- data.frame(pred.mapdif.tun)
pred.raw.mapdif.tun <- cbind(sensit.map.inc.tun, pred.mapdif.tun)


## Plotting sensitivity to MAP
(sens.map.dif <- ggplot(sensitivity.map.dif, x = climdif, y = End_cover.x) + 
    geom_segment(mapping = aes(x = climbaseline, y = Start_cover.x, xend = climdif, yend = End_cover.x, group = factor(Plot.ID), 
                               color=Biome_trend.y), size = 1.5, alpha = 0.6, data = sensitivity.map.dif) +
    scale_color_manual(values = c("indianred2", "steelblue1", "gray56", "darkred", "darkblue", "gray28"), 
                       guide = guide_legend(title = "Cover trends"), 
                       labels = c("Savanna decrease", "Savanna increase", "Savanna stable", 
                                  "Tundra decrease", "Tundra increase", "Tundra stable")) + ylim(0, 100) +
    geom_vline(xintercept = 0) +
    ylab("Woody cover (%)\n") + xlab("\nDifference in Mean Annual Precipitation (mm)") + 
    theme(legend.key = element_blank()) + clima.theme)


## PANEL STACK (Figure S4) ----
(sens.panel <- ggarrange(sens.mat.dif, sens.map.dif, labels = c("(a)", "(b)"), ncol = 1, nrow = 2,
                                 font.label = list(size = 26), common.legend = TRUE, legend = "right"))
ggsave(sens.panel, filename = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/figures/Figure_S4.png", 
       width = 40, height = 70, units = "cm")


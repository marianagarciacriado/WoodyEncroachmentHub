#### Woody encroachment across biomes
#### Script 08. Study duration and group/species analyses
#### Mariana García Criado
#### March 2019

## LIBRARIES ----
library(tidyverse)
library(ggplot2)
library(MCMCglmm)

## THEME -----
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

## STUDY DURATION ANALYSIS ----
cover.dur <- read.csv("mastersheets/cover_ms_clean.csv")
length.tundra <- filter(cover.dur, Biome_type == "Tundra")
length.savanna <- filter(cover.dur, Biome_type == "Savanna")

## Are longer studies picking larger rates of woody cover change?

# Tundra model
length.mod.tun <- MCMCglmm(Annual.rate ~ Time_period, data = length.tundra, 
                            nitt = 100000, burnin = 5000, thin = 30)
summary(length.mod.tun)
plot(length.mod.tun$Sol)
plot(length.mod.tun$VCV)
autocorr.plot(length.mod.tun$VCV)
save(length.mod.tun, file = "models/fixed/length.mod.tun.RData")

# Predicting values from the model
pred.length.tun <- predict.MCMCglmm(length.mod.tun, interval = "confidence")
pred.length.tun <- data.frame(pred.length.tun)
pred.raw.length.tun <- cbind(length.tundra, pred.length.tun)


# Savanna model
length.mod.sav <- MCMCglmm(Annual.rate ~ Time_period, data = length.savanna, 
                            nitt = 100000, burnin = 5000, thin = 30)
summary(length.mod.sav)
save(length.mod.sav, file = "models/fixed/length.mod.sav.RData")

# Predicting values from the model
pred.length.sav <- predict.MCMCglmm(length.mod.sav, interval = "confidence")
pred.length.sav <- data.frame(pred.length.sav)
pred.raw.length.sav <- cbind(length.savanna, pred.length.sav)

# saving the two biomes in a dataframe
pred.raw.length.total <- rbind(pred.raw.length.tun, pred.raw.length.sav)


## Plotting cover change vs study duration
(length.plot <- ggplot(pred.raw.length.total, aes(x = Time_period, y = Annual.rate, colour = Biome_type)) + 
    geom_point(size = 5, alpha = 0.6) + geom_line(aes(x = Time_period, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) + 
    ylab("Annual cover change rate (%)\n") + xlab("\nDuration of studies (years)") + clima.theme)



## SPECIES VS GROUP ANALYSIS ----

## Are there larger rates of cover change when a record is reported at the species or functional group level?

# Tundra model
group.tun.mod <- MCMCglmm(Annual.rate ~ Species_Group, data = length.tundra, 
                          nitt = 100000, burnin = 5000, thin = 30)
summary(group.tun.mod)
# there is no significant difference in the annual rate estimates between species or groups in the tundra
save(group.tun.mod, file = "models/fixed/group.tun.mod.RData")

# Savanna model
group.sav.mod <- MCMCglmm(Annual.rate ~ Species_Group, data = length.savanna, 
                               nitt = 100000, burnin = 5000, thin = 30)
summary(group.sav.mod)
# there is no significant difference in the annual rate estimates between species or groups in the savanna
save(group.sav.mod, file = "models/fixed/group.sav.mod.RData")

#### Woody encroachment across biomes
#### Script 8. Study duration and group/species analysis
#### Mariana Garcia
#### March 2019

## Libraries ----
#.libPaths("C:/R_library")
library(tidyverse)
library(ggplot2)
library(MCMCglmm)

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

## Loading cover mastersheet ----
cover.dur <- read.csv("scripts/users/mgarciacriado/encroachment_paper/final_scripts/mastersheets/cover_ms_clean.csv")
length.tundra <- filter(cover.dur, Biome_type == "Tundra")
length.savanna <- filter(cover.dur, Biome_type == "Savanna")

## Are longer studies picking larger rates of encroachment?
## Fitting the model for the tundra ----
length.mod.tun <- MCMCglmm(Annual.rate ~ Time_period, data = length.tundra, 
                            nitt = 100000, burnin = 5000, thin = 30)
summary(length.mod.tun)
plot(length.mod.tun$Sol)
plot(length.mod.tun$VCV)
autocorr.plot(length.mod.tun$VCV)
save(length.mod.tun, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/fixed/length.mod.tun.RData")

# Predicting values from the model
pred.length.tun <- predict.MCMCglmm(length.mod.tun, interval = "confidence")
pred.length.tun <- data.frame(pred.length.tun)
pred.raw.length.tun <- cbind(length.tundra, pred.length.tun)



## Fitting the model for the savanna ----
length.mod.sav <- MCMCglmm(Annual.rate ~ Time_period, data = length.savanna, 
                            nitt = 100000, burnin = 5000, thin = 30)
summary(length.mod.sav)
save(length.mod.sav, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/fixed/length.mod.sav.RData")

# Predicting values from the model
pred.length.sav <- predict.MCMCglmm(length.mod.sav, interval = "confidence")
pred.length.sav <- data.frame(pred.length.sav)
pred.raw.length.sav <- cbind(length.savanna, pred.length.sav)

# saving the two biomes in a dataframe
pred.raw.length.total <- rbind(pred.raw.length.tun, pred.raw.length.sav)


## Plotting study duration vs. cover change ----
(length.plot <- ggplot(pred.raw.length.total, aes(x = Time_period, y = Annual.rate, colour = Biome_type)) + 
    geom_point(size = 5, alpha = 0.6) + geom_line(aes(x = Time_period, y = fit, colour = Biome_type), size = 1) + 
    geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Biome_type), alpha = 0.32, linetype = 0) + 
    ylab("Annual cover change rate (%)\n") + xlab("\nDuration of studies (years)") + clima.theme)
ggsave(length.plot, filename = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/figures/Figure_S2.png", 
       width = 30, height = 20, units = "cm")


## Are larger rates of encroachment displayed when a record is reported at the species or functional group level?
## Model of species vs. group ----
group.tun.mod <- MCMCglmm(Annual.rate ~ Species_Group, data = length.tundra, 
                          nitt = 100000, burnin = 5000, thin = 30)
summary(group.tun.mod)
# there is no significant difference in the annual rate estimates between species or groups in the tundra
save(group.tun.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/fixed/group.tun.mod.RData")

group.sav.mod <- MCMCglmm(Annual.rate ~ Species_Group, data = length.savanna, 
                               nitt = 100000, burnin = 5000, thin = 30)
summary(group.sav.mod)
# there is no significant difference in the annual rate estimates between species or groups in the savanna
save(group.sav.mod, file = "scripts/users/mgarciacriado/encroachment_paper/final_scripts/models/fixed/group.sav.mod.RData")
